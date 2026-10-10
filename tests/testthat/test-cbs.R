test_that("CBS page and article parsers read rank, name and date", {
  row <- function(r, slug, name) sprintf('<tr class="TableBase-bodyTr"><td class="TableBase-bodyTd">\n %d </td><td><a href="/college-football/teams/X/%s/">x</a><span class="TeamName"><a href="/college-football/teams/X/%s/" class="">%s</a></span></td></tr>', r, slug, slug, name)
  page <- paste0('<span class="LeaguePoll-tablePromo">Updated</span>\n    Sep 21, 2026\n </span>',
                 paste(vapply(1:138, \(i) row(i, paste0("team-", i), paste("Team", i)), ""), collapse = ""))
  p <- cbs_parse_html(page)
  expect_equal(nrow(p), 138)
  expect_equal(p$updated[1], as.Date("2026-09-21"))
  expect_equal(p$cbs_slug[2], "team-2")

  art <- paste0('"datePublished":"2026-09-14T14:12:50+00:00"',
                '<table class="TableBuilder"><tbody><tr><td>1</td><td>Texas</td><td>2-0</td><td>2</td></tr>',
                '<tr><td>2</td><td><a class="Annotation-link" href="https://www.cbssports.com/college-football/teams/LSU/lsu-tigers/">LSU</a></td><td>2-0</td><td>6</td></tr></tbody></table>')
  a <- cbs_parse_article(art)
  expect_equal(a$cbs_name, c("Texas", "LSU"))
  expect_equal(a$cbs_slug, c(NA, "lsu-tigers"))
  expect_equal(a$updated[1], as.Date("2026-09-14"))
})

test_that("CBS rankings align to the snapshot they follow", {
  games <- tibble::tibble(
    week_seq = c(1L, 1L, 2L, 2L), season_type = "regular",
    start_date = c("2026-09-05T16:00", "2026-09-06T01:00", "2026-09-12T16:00", "2026-09-13T01:00"))
  cbs <- tibble::tibble(season = 2026L, updated = as.Date(c("2026-08-12", "2026-09-08", "2026-09-14")),
                        rank = 1L, cbs_name = c("A", "B", "C"), team_id = 1:3)
  p <- cbs_as_polls(cbs, games)
  expect_equal(p$week, c(1L, 2L, 3L))          # preseason -> week 0 snapshot, then weeks 1, 2
  expect_true(all(p$season_type == "regular"))
})

test_that("slugify transliterates accents the same on every platform", {
  expect_equal(slugify(c("San José State Spartans", "Texas A&M Aggies")),
               c("san-jose-state-spartans", "texas-am-aggies"))
})
