# ==============================================================================
# R/01_ingest_cbs.R
# CBS Sports 138: a weekly ranking of every FBS team (no API, so it is scraped).
#
# The live page shows only the current ranking, so each sync archives it as
# data/raw/cbs138/<updated date>.html. Earlier weeks (2022 on) come from Wayback Machine
# captures, via a one-off cbs_backfill_wayback(). Captures are sparse: some weeks are missing.
# Every archived file is parsed into the `cbs_rankings` table; read_season() then aligns each
# ranking to the snapshot it follows, the same way AP polls are aligned.
# ==============================================================================

CBS_URL <- "https://www.cbssports.com/college-football/rankings/cbs-sports-rankings/"
CBS_POLL <- "CBS Sports 138"

cbs_request <- function(url) {
  httr2::request(url) |>
    httr2::req_user_agent("Mozilla/5.0 (personal CFB rankings project)") |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_timeout(60)
}

#' Parse one saved page: tibble(updated, rank, cbs_name, cbs_slug); NULL if it has no table
cbs_parse_html <- function(html) {
  upd <- regmatches(html, regexpr("Updated(?:\\s|<[^>]+>)+[A-Z][a-z]{2}\\s+\\d{1,2},\\s+\\d{4}", html, perl = TRUE))
  rows <- regmatches(html, gregexpr("(?s)<tr class=\"TableBase-bodyTr\">.*?</tr>", html, perl = TRUE))[[1]]
  if (length(upd) == 0 || length(rows) < 100) return(NULL)
  pick <- function(pattern) sub(pattern, "\\1", rows, perl = TRUE)
  out <- tibble::tibble(
    updated = as.Date(sub("(?s)^.*?([A-Z][a-z]{2}\\s+\\d{1,2},\\s+\\d{4})$", "\\1", upd, perl = TRUE), format = "%b %d, %Y"),
    rank = suppressWarnings(as.integer(pick("(?s)^.*?<td[^>]*>\\s*(\\d+)\\s*</td>.*$"))),
    cbs_name = trimws(pick("(?s)^.*?class=\"TeamName\"><a[^>]*>([^<]+)</a>.*$")),
    cbs_slug = pick("(?s)^.*?/college-football/teams/[A-Z0-9]+/([a-z0-9-]+)/.*$")
  )
  out[!is.na(out$rank), ]
}

#' Parse a weekly CBS Sports 138 article (top 25 only; the article links to the full list):
#' tibble(updated = publication date, rank, cbs_name, cbs_slug = NA unless linked)
cbs_parse_article <- function(html) {
  pub <- regmatches(html, regexpr("\"datePublished\":\"\\d{4}-\\d{2}-\\d{2}", html))
  tab <- regmatches(html, regexpr("(?s)<table class=\"TableBuilder\">.*?</table>", html, perl = TRUE))
  if (length(pub) == 0 || length(tab) == 0) return(NULL)
  rows <- regmatches(tab, gregexpr("(?s)<tr><td>.*?</tr>", tab, perl = TRUE))[[1]]
  cells <- lapply(rows, \(r) regmatches(r, gregexpr("(?s)<td>(.*?)</td>", r, perl = TRUE))[[1]])
  strip <- function(x) trimws(gsub("&amp;", "&", gsub("<[^>]+>", "", x)))
  tibble::tibble(
    updated = as.Date(sub(".*\"(\\d{4}-\\d{2}-\\d{2})$", "\\1", pub)),
    rank = suppressWarnings(as.integer(strip(vapply(cells, `[`, "", 1)))),
    cbs_name = strip(vapply(cells, `[`, "", 2)),
    cbs_slug = ifelse(grepl("/teams/", vapply(cells, `[`, "", 2)),
                      sub("(?s)^.*?/college-football/teams/[A-Z0-9]+/([a-z0-9-]+)/.*$", "\\1", vapply(cells, `[`, "", 2), perl = TRUE),
                      NA_character_)
  ) |> dplyr::filter(!is.na(rank))
}

#' Archive weekly articles (top 25 only), for weeks with no full-page capture
cbs_fetch_articles <- function(urls, raw_dir = "data/raw") {
  dir <- file.path(raw_dir, "cbs138", "articles")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  for (u in urls) {
    html <- httr2::resp_body_string(httr2::req_perform(cbs_request(u)))
    p <- cbs_parse_article(html)
    if (is.null(p)) { warning("[CBS] no ranking table in ", u); next }
    writeLines(html, file.path(dir, paste0(format(p$updated[1]), ".html")), useBytes = TRUE)
    Sys.sleep(1)
  }
}

cbs_save <- function(html, dir) {
  p <- cbs_parse_html(html)
  if (is.null(p)) return(NA)
  file <- file.path(dir, paste0(format(p$updated[1]), ".html"))
  if (file.exists(file)) return(FALSE)
  writeLines(html, file, useBytes = TRUE)
  TRUE
}

#' Archive the live ranking (a no-op when that week's ranking is already saved)
cbs_fetch_live <- function(raw_dir = "data/raw") {
  dir <- file.path(raw_dir, "cbs138")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  html <- httr2::resp_body_string(httr2::req_perform(cbs_request(CBS_URL)))
  saved <- cbs_save(html, dir)
  if (is.na(saved)) warning("[CBS] live page had no ranking table (layout changed?)")
  invisible(saved)
}

#' One-off: archive every distinct Wayback Machine capture of the ranking page
cbs_backfill_wayback <- function(raw_dir = "data/raw", from = 2022) {
  dir <- file.path(raw_dir, "cbs138")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  cdx <- httr2::request("https://web.archive.org/cdx/search/cdx") |>
    httr2::req_url_query(url = sub("^https://www\\.", "", CBS_URL), fl = "timestamp",
                         filter = "statuscode:200", collapse = "timestamp:8", from = from) |>
    httr2::req_retry(max_tries = 5, is_transient = \(r) TRUE) |>
    httr2::req_perform() |> httr2::resp_body_string()
  stamps <- strsplit(trimws(cdx), "\n")[[1]]
  n_new <- 0L
  for (ts in stamps) {
    html <- tryCatch(
      httr2::resp_body_string(httr2::req_perform(cbs_request(sprintf("https://web.archive.org/web/%sid_/%s", ts, CBS_URL)))),
      error = function(e) NA_character_)
    if (!is.na(html) && isTRUE(cbs_save(html, dir))) n_new <- n_new + 1L
    Sys.sleep(1)
  }
  message(sprintf("[CBS] %d captures checked, %d new rankings archived", length(stamps), n_new))
  invisible(n_new)
}

slugify <- function(x) {
  # stringi, not iconv: iconv's transliteration is platform-dependent ("San José" becomes
  # "San Jos'e" on macOS and can become "San Jos?" on Linux)
  x <- tolower(stringi::stri_trans_general(gsub("&", "", x), "Latin-ASCII"))
  gsub("^-|-$", "", gsub("-+", "-", gsub("[^a-z0-9]+", "-", x)))
}

# CBS page slugs that don't follow "<CFBD school>-<mascot>"
CBS_SLUG_OVERRIDES <- c(
  "miami-fla-hurricanes" = "Miami", "appalachian-state-mountaineers" = "App State",
  "connecticut-huskies" = "UConn", "army-west-point-black-knights" = "Army",
  "hawaii-rainbow-warriors" = "Hawai'i", "fiu-panthers" = "Florida International",
  "delaware-fightin-blue-hens" = "Delaware", "middle-tenn-blue-raiders" = "Middle Tennessee",
  "louisianamonroe-warhawks" = "UL Monroe", "ul-monroe-warhawks" = "UL Monroe"
)
# CBS display names that don't match the CFBD school name
CBS_NAME_OVERRIDES <- c(
  "Miami (Fla.)" = "Miami", "Miami (Ohio)" = "Miami (OH)", "App. St." = "App State",
  "Middle Tenn." = "Middle Tennessee", "UL-Monroe" = "UL Monroe", "FIU" = "Florida International",
  "Hawaii" = "Hawai'i", "Army" = "Army", "UConn" = "UConn", "UMass" = "Massachusetts"
)

#' Map CBS slugs to CFBD team ids: "<school>-<mascot>" slug, then overrides, then the
#' longest FBS school slug that prefixes the CBS slug
cbs_match_teams <- function(cbs, teams) {
  fbs <- teams[teams$classification %in% "fbs", ]
  fbs$slug <- slugify(paste(fbs$school, fbs$mascot))
  fbs$school_slug <- slugify(fbs$school)
  id <- fbs$team_id[match(cbs$cbs_slug, fbs$slug)]
  ov <- CBS_SLUG_OVERRIDES[cbs$cbs_slug]
  id[is.na(id)] <- fbs$team_id[match(ov[is.na(id)], fbs$school)]
  for (i in which(is.na(id) & !is.na(cbs$cbs_slug))) {
    hit <- fbs[startsWith(cbs$cbs_slug[i], paste0(fbs$school_slug, "-")), ]
    if (nrow(hit)) id[i] <- hit$team_id[which.max(nchar(hit$school_slug))]
  }
  # Name-only rows (articles): CBS name -> CFBD school
  nm <- ifelse(cbs$cbs_name %in% names(CBS_NAME_OVERRIDES), CBS_NAME_OVERRIDES[cbs$cbs_name], cbs$cbs_name)
  id[is.na(id)] <- fbs$team_id[match(slugify(nm[is.na(id)]), fbs$school_slug)]
  cbs$team_id <- as.integer(id)
  miss <- unique(dplyr::coalesce(cbs$cbs_slug, cbs$cbs_name)[is.na(id)])
  if (length(miss)) warning("[CBS] unmatched teams: ", paste(miss, collapse = ", "))
  cbs
}

#' Parse every archived ranking into `cbs_rankings`; also archives the live page when asked
sync_cbs138 <- function(con, raw_dir = "data/raw", live = TRUE) {
  if (live) tryCatch(cbs_fetch_live(raw_dir), error = function(e) warning("[CBS] live fetch failed: ", conditionMessage(e)))
  read <- \(f) paste(readLines(f, warn = FALSE), collapse = "\n")
  files <- list.files(file.path(raw_dir, "cbs138"), pattern = "\\.html$", full.names = TRUE)
  arts <- list.files(file.path(raw_dir, "cbs138", "articles"), pattern = "\\.html$", full.names = TRUE)
  if (length(c(files, arts)) == 0 || !DBI::dbExistsTable(con, "teams")) return(invisible(0L))
  full <- dplyr::bind_rows(lapply(files, \(f) cbs_parse_html(read(f)))) |> dplyr::mutate(source = "page")
  top25 <- dplyr::bind_rows(lapply(arts, \(f) cbs_parse_article(read(f)))) |> dplyr::mutate(source = "article")
  # an article only fills a week with no full-page capture (captures are dated by their update)
  if (nrow(top25) && nrow(full)) top25 <- top25[!top25$updated %in% full$updated, ]
  cbs <- dplyr::bind_rows(full, top25)
  cbs <- cbs_match_teams(cbs, DBI::dbReadTable(con, "teams"))
  m <- as.integer(format(cbs$updated, "%m"))
  cbs$season <- as.integer(format(cbs$updated, "%Y")) - (m == 1L)   # January = last season's final
  DBI::dbWriteTable(con, "cbs_rankings", cbs, overwrite = TRUE)
  invisible(nrow(cbs))
}

#' Turn CBS rankings into poll rows aligned like AP polls (poll week w+1 follows snapshot w;
#' postseason = the final ranking). A ranking follows the latest week whose games were
#' (90%) played on or before its update date; the latest ranking per week wins.
cbs_as_polls <- function(cbs, games) {
  if (is.null(cbs) || nrow(cbs) == 0) return(NULL)
  d <- as.Date(substr(games$start_date, 1, 10))
  week_end <- tapply(d, games$week_seq, \(x) stats::quantile(as.numeric(x), 0.9, type = 1))
  weeks <- as.integer(names(week_end))
  last_reg <- max(games$week_seq[games$season_type == "regular"])
  cbs$after_week <- vapply(as.numeric(cbs$updated), \(u) max(c(0L, weeks[week_end <= u])), integer(1))
  cbs |>
    dplyr::filter(!is.na(team_id)) |>
    dplyr::group_by(after_week) |>
    dplyr::filter(updated == max(updated)) |>
    dplyr::ungroup() |>
    dplyr::transmute(
      season, season_type = ifelse(after_week > last_reg, "postseason", "regular"),
      week = ifelse(after_week > last_reg, 1L, after_week + 1L),
      poll = CBS_POLL, rank, team_id, school = cbs_name, points = NA_real_
    )
}
