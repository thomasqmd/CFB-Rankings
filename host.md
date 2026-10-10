# Hosting the dashboard and reports

Notes on viewing the rendered outputs from other devices (phone, laptop, other people) once
`targets::tar_make()` has run on the build machine (the Pop!_OS box). Nothing here is set up yet.

## What there is to host

| Output | Path | Self-contained? |
|---|---|---|
| Dashboard | `docs/dashboard.html` | Yes (`embed-resources: true`), but uses OJS |
| Season report | `docs/reports/season_retro.html` + `season_retro_files/` | No |
| Diagnostics report | `docs/reports/model_diagnostics.html` + `model_diagnostics_files/` | No |
| Paper | `paper/cfb-paper.pdf` | Yes |

Everything is static: no Shiny, no R server. Any static file host works. Serve over HTTP rather
than opening the file directly (`file://`), because OJS and some browsers, mobile ones
especially, behave inconsistently on local files. The rendered HTML/PDF is tracked in git (see
`.gitignore`), so any machine that pulls the repo has the latest outputs.

Setting `embed-resources: true` on the two reports would make every output a single file, which
simplifies all the options below (no `_files/` folders to copy or commit).

## Option 1: home network only

The build machine serves the folder and devices on the same Wi-Fi open `http://<build-machine-ip>:8000`.

```bash
cd ~/CFB-Rankings && python3 -m http.server 8000 --bind 0.0.0.0
```

- Zero setup and free. Works only at home.
- To keep it running, use a systemd user service or a small Caddy server
  (`caddy file-server --root ~/CFB-Rankings --listen :8000`) instead of a terminal session.
- Pop!_OS has no firewall enabled by default; if `ufw` is on, `sudo ufw allow 8000/tcp`.
- Give the box a DHCP reservation on the router so the IP doesn't change.

## Option 2: anywhere, private (recommended)

**Tailscale.** Install it on the build machine and on each device (phone, laptop). Everything
on your tailnet can reach the box from anywhere, encrypted, with no ports opened on the router.

```bash
# on the build machine, after `tailscale up`
tailscale serve --bg ~/CFB-Rankings/docs
```

- That gives an HTTPS URL like `https://<machine-name>.<tailnet>.ts.net/`, reachable only by
  your own devices (and anyone you share the machine with).
- Free personal plan covers this comfortably.
- The box has to be on. Output updates the moment `tar_make()` re-renders; there's no publish step.
- Best fit if the audience is just you, or a few people you're willing to invite.

**Cloudflare Tunnel + Cloudflare Access** is the alternative: a public URL on a domain you own,
protected by an email login. More setup (needs a domain on Cloudflare), but viewers need
nothing installed.

## Option 3: anywhere, public

Push the rendered files to a static host. Works with the build machine off, and anyone with the
link can see it.

| Host | How | Notes |
|---|---|---|
| **GitHub Pages** | `quarto publish gh-pages --no-render` or a Pages workflow on `docs/` | This repo is **private**: Pages on a private repo needs GitHub Pro, and the site itself is still public. A separate public repo holding only the HTML avoids that. |
| **Quarto Pub** | `quarto publish quarto-pub docs/dashboard.qmd --no-render` | Free, public only, one command. Per-document, so each report is published separately. |
| **Netlify / Cloudflare Pages** | `quarto publish netlify --no-render`, or connect the repo | Free tiers; Cloudflare Pages can sit behind Cloudflare Access to make it private. |
| **Posit Connect Cloud** | Deploy from GitHub | Free tier for public content; it is built for R/Quarto. |

Use `--no-render` because `tar_make()` already renders: `quarto publish` would otherwise try
to re-run the R chunks outside the pipeline.

Things to consider before going public:
- The dashboard is fine to share. The paper is a draft, so maybe keep `paper/` off the public site.
- Nothing rendered includes the CFBD API key, but check before the first publish.

## Keeping it up to date

Weekly in season: sync, fit the new week, re-render, publish. On the build machine:

1. A systemd timer (or cron) runs `Rscript -e 'targets::tar_make()' && Rscript check_build.R`
   once a week, e.g. Sunday morning after the Saturday games.
2. If `check_build.R` passes, publish:
   - Option 2: nothing to do; Tailscale serves the folder that was just re-rendered.
   - Option 3: `git commit` the outputs and push, or run the `quarto publish ... --no-render` command.

Gating the publish on `check_build.R` means a snapshot that fails its convergence gates never
goes live.

## Suggested path

1. Now: Option 1 to check the dashboard on a phone at home.
2. Next: Tailscale (Option 2) for private access from anywhere, plus the weekly timer.
3. Later, if you want to share it: a public static host (Option 3), publishing only the dashboard
   and reports, gated on `check_build.R`.
