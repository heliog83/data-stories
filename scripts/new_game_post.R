#!/usr/bin/env Rscript
# ═══════════════════════════════════════════════════════════════════════════
# new_game_post.R
#
# Drafts a postgame post for one game. The report, every number in the insight
# strip, the menu of quotable facts and the provenance block all come from
# nfl_analytics R/47. The only things a person writes are the headline and the
# paragraphs that make the argument.
#
#   Rscript scripts/new_game_post.R <game_id> [options]
#
#   --strip=k1,k2,k3,k4   fact keys for the insight strip (see facts.json)
#   --allow-unverified    draft a game that has no independent expectations
#   --out=<dir>           posts directory (default: <blog>/posts)
#
# Safe to re-run: the report, facts.json and the blocks between
# <!-- name:start --> / <!-- name:end --> markers are regenerated; everything
# else in index.qmd (your headline and prose) is left alone.
#
# Then write the post and run:  Rscript scripts/check_game_post.R <slug>
#
# Source of truth : $NFL_ANALYTICS_ROOT (default ~/Documents/nfl_analytics),
#                   which must be on a clean checkout so provenance is honest
# Delivery payload: posts/<season>-wk-<week>-<away>-<home>/
#                   index.qmd, game-charts.html, facts.json
# ═══════════════════════════════════════════════════════════════════════════

args <- commandArgs(trailingOnly = TRUE)
opt  <- function(name, default = NULL) {
  hit <- grep(sprintf("^--%s(=|$)", name), args, value = TRUE)
  if (!length(hit)) return(default)
  if (!grepl("=", hit[1])) return(TRUE)
  sub("^[^=]*=", "", hit[1])
}
GAME_ID <- args[!startsWith(args, "--")][1]
if (is.na(GAME_ID) || !grepl("^[0-9]{4}_[0-9]{2}_[A-Z]+_[A-Z]+$", GAME_ID)) {
  stop("usage: Rscript scripts/new_game_post.R <game_id> [--strip=a,b,c,d] [--allow-unverified] [--out=dir]\n",
       "  e.g. 2026_02_BUF_MIA", call. = FALSE)
}

BLOG_DIR <- normalizePath(file.path(dirname(sub("^--file=", "", grep("^--file=",
              commandArgs(FALSE), value = TRUE)[1])), ".."), mustWork = FALSE)
if (is.na(BLOG_DIR) || !dir.exists(BLOG_DIR)) BLOG_DIR <- getwd()
NFL_DIR  <- normalizePath(Sys.getenv("NFL_ANALYTICS_ROOT", "~/Documents/nfl_analytics"), mustWork = TRUE)
POSTS    <- opt("out", file.path(BLOG_DIR, "posts"))
STRIP    <- strsplit(opt("strip", "turnovers,explosive,success,swing_value"), ",", fixed = TRUE)[[1]]

# ── 1. provenance precondition: a clean nfl_analytics checkout ──────────────
git <- function(...) suppressWarnings(system2("git", c("-C", shQuote(NFL_DIR), ...), stdout = TRUE, stderr = FALSE))
dirty <- git("status", "--porcelain")
if (length(dirty) && any(nzchar(dirty))) {
  stop("nfl_analytics has uncommitted changes, so the report could not name the code that built it:\n  ",
       paste(dirty, collapse = "\n  "), "\nCommit or stash them, then re-run.", call. = FALSE)
}
branch <- git("rev-parse", "--abbrev-ref", "HEAD")
if (!identical(branch, "main")) warning("nfl_analytics is on branch '", branch, "', not main.", call. = FALSE)

# ── 2. build and check the report ────────────────────────────────────────────
e <- new.env()
old <- setwd(NFL_DIR)
frames <- tryCatch({
  sys.source("R/47_game_charts.R", envir = e)
  e$build_game_charts(GAME_ID, quiet = TRUE)
}, finally = setwd(old))

checks <- e$smoke_test_game_charts(frames)
if (!is.null(checks) && !all(checks$pass)) {
  print(checks[!checks$pass, ])
  stop("R/47 smoke test FAILED for ", GAME_ID, ". Fix the report before drafting a post.", call. = FALSE)
}
verified <- !is.null(checks)
if (!verified && !isTRUE(opt("allow-unverified", FALSE))) {
  stop(GAME_ID, " has no independently measured expectations in R/47 (GAME_CHARTS_EXPECTED), ",
       "so its numbers have never been checked against a second source.\n",
       "Add expectations first, or pass --allow-unverified to draft anyway.", call. = FALSE)
}

meta  <- frames$meta
away  <- meta$away; home <- meta$home; teams <- meta$teams
sc    <- frames$final_score
f     <- frames$game_story$factors
F     <- function(t, col) f[[col]][f$team == t]
N     <- function(t, start = FALSE) e$.gc_team_name(t, start)
poss  <- function(t, start = FALSE) { n <- N(t, start); if (grepl("s$", n)) paste0(n, "'") else paste0(n, "'s") }
was   <- function(t) if (grepl("^the ", N(t))) "were" else "was"
pct   <- function(x) sprintf("%.1f%%", 100 * x)
of    <- function(k, n) sprintf("%d of %d", as.integer(k), as.integer(n))
plural <- function(n, w) sprintf("%d %s%s", as.integer(n), w, if (n == 1) "" else "s")

slug     <- sprintf("%d-wk-%d-%s-%s", meta$season, meta$week, tolower(away), tolower(home))
post_dir <- file.path(POSTS, slug)
dir.create(post_dir, showWarnings = FALSE, recursive = TRUE)

public_src <- file.path(NFL_DIR, meta$public_html_path)
if (!file.exists(public_src)) public_src <- meta$public_html_path
invisible(file.copy(public_src, file.path(post_dir, "game-charts.html"), overwrite = TRUE))
report_md5 <- unname(tools::md5sum(file.path(post_dir, "game-charts.html")))

# ── 3. facts: every number a post may quote ─────────────────────────────────
facts <- list()
fact  <- function(key, value, label, sentence) facts[[key]] <<- list(value = value, label = label, sentence = sentence)

winner <- if (sc[[away]] > sc[[home]]) away else if (sc[[home]] > sc[[away]]) home else NA
loser  <- if (is.na(winner)) NA else setdiff(teams, winner)
if (!is.na(winner)) {
  fact("score", sprintf("%d–%d", sc[[winner]], sc[[loser]]), sprintf("Final, %s over %s", N(winner), N(loser)),
       sprintf("%s beat %s %d–%d%s.", N(winner, TRUE), N(loser), sc[[winner]], sc[[loser]], if (identical(winner, away)) " on the road" else ""))
}
fact("success", sprintf("%s vs %s", pct(F(away, "success_rate")), pct(F(home, "success_rate"))),
     sprintf("Success rate, %s vs %s", N(away), N(home)),
     sprintf("Success rate (plays that left the offense better off): %s %s, %s %s.", N(away), pct(F(away, "success_rate")), N(home), pct(F(home, "success_rate"))))
fact("explosive", sprintf("%d to %d", F(away, "explosive_plays"), F(home, "explosive_plays")),
     sprintf("Explosive plays, %s to %s", N(away), N(home)),
     sprintf("Explosive plays (a pass of 15+ yards or a run of 10+): %s %d (%d pass, %d run), %s %d (%d pass, %d run).",
             N(away), F(away, "explosive_plays"), F(away, "explosive_pass"), F(away, "explosive_run"),
             N(home), F(home, "explosive_plays"), F(home, "explosive_pass"), F(home, "explosive_run")))
fact("turnovers", sprintf("%d to %d", F(away, "turnovers"), F(home, "turnovers")),
     sprintf("Turnovers, %s to %s", N(away), N(home)),
     sprintf("Turnovers: %s %d (%s, %s), %s %d (%s, %s).",
             N(away), F(away, "turnovers"), plural(F(away, "interceptions"), "interception"), plural(F(away, "fumbles_lost"), "lost fumble"),
             N(home), F(home, "turnovers"), plural(F(home, "interceptions"), "interception"), plural(F(home, "fumbles_lost"), "lost fumble")))
fact("td_outside_rz", sprintf("%d to %d", F(away, "td_drives_outside_rz"), F(home, "td_drives_outside_rz")),
     sprintf("Touchdown drives that never reached the 20, %s to %s", N(away), N(home)),
     sprintf("Touchdown drives that never ran a play inside the 20: %s %d, %s %d.", N(away), F(away, "td_drives_outside_rz"), N(home), F(home, "td_drives_outside_rz")))
for (t in teams) {
  k <- tolower(t)
  fact(paste0("red_zone_", k), of(F(t, "rz_tds"), F(t, "rz_trips")), sprintf("%s red-zone trips ending in a touchdown", N(t, TRUE)),
       sprintf("%s scored touchdowns on %s red-zone trips.", N(t, TRUE), of(F(t, "rz_tds"), F(t, "rz_trips"))))
  fact(paste0("third_down_", k), of(F(t, "third_conv"), F(t, "third_att")), sprintf("%s third-down conversions", N(t, TRUE)),
       sprintf("%s converted %s third downs.", N(t, TRUE), of(F(t, "third_conv"), F(t, "third_att"))))
  fact(paste0("fourth_down_", k), of(F(t, "fourth_conv"), F(t, "fourth_att")), sprintf("%s fourth-down conversions", N(t, TRUE)),
       sprintf("%s converted %s fourth downs.", N(t, TRUE), of(F(t, "fourth_conv"), F(t, "fourth_att"))))
  fact(paste0("sacks_by_", k), as.character(F(t, "sacks_made")), sprintf("Sacks by %s defense", poss(t)),
       sprintf("%s defense had %s and %s.", poss(t, TRUE), plural(F(t, "sacks_made"), "sack"), plural(F(t, "qb_hits_made"), "QB hit")))
  fact(paste0("penalties_", k), sprintf("%d for %d", F(t, "penalties"), F(t, "penalty_yards")), sprintf("%s penalties, for yards", N(t, TRUE)),
       sprintf("%s %s flagged %s for %d yards.", N(t, TRUE), was(t), plural(F(t, "penalties"), "time"), F(t, "penalty_yards")))
}
lv  <- frames$game_story$leverage
top <- lv[which.max(abs(lv$wpa_offense)), , drop = FALSE]
fact("swing_value", sprintf("%+.0f", 100 * top$wpa_offense), sprintf("Win-prob. points on the biggest swing (%s)", top$clock),
     sprintf("The biggest swing: %s, %s, %s, %+.1f win-probability points for %s.",
             top$clock, N(top$posteam), trimws(paste(top$player, top$action)), 100 * top$wpa_offense, N(top$posteam)))
late_t <- teams[which.max(c(F(away, "late_def_wp"), F(home, "late_def_wp")))]
if (F(late_t, "late_def_plays") > 0) fact("late_defense", sprintf("%+.0f", 100 * F(late_t, "late_def_wp")), sprintf("Win-prob. points for %s defense in the final 2:00", poss(late_t)),
     sprintf("In the final two minutes %s defense made %s worth %+.0f win-probability points.",
             poss(late_t), if (F(late_t, "late_def_plays") == 1) "1 sack or takeaway" else sprintf("%d sacks or takeaways", as.integer(F(late_t, "late_def_plays"))), 100 * F(late_t, "late_def_wp")))

bad <- setdiff(STRIP, names(facts))
if (length(bad)) stop("unknown --strip key(s): ", paste(bad, collapse = ", "), "\nknown: ", paste(names(facts), collapse = ", "), call. = FALSE)

record <- list(
  game_id = GAME_ID, season = meta$season, week = meta$week, away = away, home = home,
  final_score = as.list(sc), slug = slug, built = format(Sys.time(), "%Y-%m-%d %H:%M %Z"),
  verified = verified, checks = if (verified) nrow(checks) else 0L,
  report_md5 = report_md5, script_md5 = meta$script_md5, git_sha = meta$git_sha,
  source_md5 = meta$source_md5, ftn_attribution = if (is.null(frames$ftn)) NULL else frames$ftn$attribution,
  facts = facts
)
jsonlite::write_json(record, file.path(post_dir, "facts.json"), auto_unbox = TRUE, pretty = TRUE)

# ── 4. generated blocks ──────────────────────────────────────────────────────
esc <- function(x) gsub("<", "&lt;", gsub("&", "&amp;", x, fixed = TRUE), fixed = TRUE)
block <- function(name, body) c(sprintf("<!-- %s:start -->", name), body, sprintf("<!-- %s:end -->", name))

strip_block <- block("strip", c(
  "```{=html}", '<div class="insight-strip">',
  unlist(lapply(STRIP, function(k) c('  <div class="insight-item">',
    sprintf('    <div class="insight-value">%s</div>', esc(facts[[k]]$value)),
    sprintf('    <div class="insight-label">%s</div>', esc(facts[[k]]$label)), "  </div>"))),
  "</div>", "```"))

facts_block <- block("facts", c(
  "<!--",
  "Facts you can quote. check_game_post.R rejects a number that is not in",
  "facts.json or the embedded report. Strip keys: --strip=key1,key2,key3,key4",
  "",
  sprintf("  %-18s %s", names(facts), vapply(facts, function(x) x$sentence, character(1))),
  "-->"))

report_block <- block("report", c(
  "```{=html}",
  '<div class="report-wrap" style="margin: 1.5rem 0 0.5rem;">',
  '  <p style="font-family: var(--font-display); font-size: 0.85rem; color: var(--color-text-muted); margin-bottom: 0.5rem;">',
  '    <a href="game-charts.html" target="_blank" rel="noopener">Open the full report in its own tab</a>',
  "  </p>",
  sprintf('  <iframe id="game-report" src="game-charts.html" title="Full interactive game report: %s %d, %s %d, Week %d %d"',
          N(away, TRUE), sc[[away]], N(home, TRUE), sc[[home]], meta$week, meta$season),
  '          scrolling="no"',
  '          style="width: 100%; height: 2200px; border: 1px solid var(--color-border); border-radius: 8px; background: #ffffff; display: block; overflow: hidden;"></iframe>',
  "</div>",
  "<script>",
  "(function () {",
  "  var f = document.getElementById('game-report');",
  "  function fit() {",
  "    try {",
  "      var d = f.contentDocument;",
  "      if (!d || !d.body) return;",
  "      var cs = d.defaultView.getComputedStyle(d.body);",
  "      f.style.height = Math.ceil(d.body.getBoundingClientRect().height + parseFloat(cs.marginTop) + parseFloat(cs.marginBottom)) + 'px';",
  "    } catch (e) { /* cross-origin: keep the fixed height */ }",
  "  }",
  "  var ready = null;",
  "  function setup() {",
  "    try {",
  "      var d = f.contentDocument;",
  "      if (!d || !d.body || d.location.href === 'about:blank' || ready === d) return;",
  "      ready = d;",
  "      if (window.ResizeObserver) new ResizeObserver(fit).observe(d.body);",
  "      d.addEventListener('toggle', fit, true);",
  "    } catch (e) {}",
  "    fit();",
  "  }",
  "  f.addEventListener('load', setup);",
  "  try { if (f.contentDocument && f.contentDocument.readyState === 'complete') setup(); } catch (e) {}",
  "  window.addEventListener('resize', fit);",
  "})();",
  "</script>",
  "```"))

not_block <- block("not", c(
  "## What this is not", "",
  "- **One game.** A single afternoon is too small to say anything about team quality. Nothing here is a forecast.",
  "- **Win probability** is the nflverse model, with no market input.",
  "- **No betting or market prices.** This piece describes one game, nothing more."))

prov_block <- block("provenance", c(
  "## Provenance", "",
  sprintf("- **Game:** %s at %s, Week %d, %d (`%s`). Final: %s %d, %s %d.",
          N(away, TRUE), N(home), meta$week, meta$season, GAME_ID, N(away, TRUE), sc[[away]], N(home, TRUE), sc[[home]]),
  sprintf("- **Data:** nflverse play-by-play for Week %d, %d, through the final whistle (source file md5 `%s`).", meta$week, meta$season, meta$source_md5),
  sprintf("- **Report:** the public edition of `R/47_game_charts.R`, unedited (script md5 `%s`, commit `%s`, clean working tree).", meta$script_md5, substr(meta$git_sha, 1, 7)),
  if (verified) sprintf("- **Checked:** the report's numbers match %d values measured independently of the chart code.", nrow(checks))
  else "- **Checked:** no independent measurements are recorded for this game yet.",
  if (!is.null(frames$ftn)) "- **Charting data:** © FTN Data via nflverse, CC-BY-SA 4.0." else NULL))

# ── 5. index.qmd: create once, then refresh only the generated blocks ────────
qmd <- file.path(post_dir, "index.qmd")
if (!file.exists(qmd)) {
  writeLines(c(
    "---",
    'title: "TODO: the one claim this post makes"',
    'subtitle: "TODO"',
    'description: "TODO: one or two sentences for the homepage card and link previews"',
    sprintf("date: %s", format(Sys.Date())),
    'author: "Helio"',
    "categories: [nfl, football-analytics]",
    "format:",
    "  html:",
    "    css: ../../styles.css",
    "    code-tools: false",
    "---",
    "",
    "```{=html}",
    '<div class="title-bar">',
    "  <h2>TODO: same as the title</h2>",
    sprintf("  <p>Postgame analysis · Week %d, %d · %s %d, %s %d</p>",
            meta$week, meta$season, N(away, TRUE), sc[[away]], N(home, TRUE), sc[[home]]),
    "</div>",
    "```",
    "",
    "TODO: the claim, in two or three sentences. Quote only numbers from the facts list below.",
    "",
    "One afternoon, measured through the final whistle. Nothing here says anything about either team's season.",
    "",
    strip_block,
    "",
    "TODO: the supporting paragraph. The full picture is the report below.",
    "",
    report_block,
    "",
    facts_block,
    "",
    not_block,
    "",
    prov_block
  ), qmd)
  message("Drafted ", qmd)
} else {
  txt <- readLines(qmd, warn = FALSE)
  swap <- function(txt, name, new) {
    s <- grep(sprintf("^<!-- %s:start -->$", name), txt); e2 <- grep(sprintf("^<!-- %s:end -->$", name), txt)
    if (length(s) != 1 || length(e2) != 1 || e2 < s) { warning("no ", name, " block markers in index.qmd; left as is", call. = FALSE); return(txt) }
    c(txt[seq_len(s - 1)], new, txt[-seq_len(e2)])
  }
  for (b in list(list("strip", strip_block), list("report", report_block), list("facts", facts_block), list("not", not_block), list("provenance", prov_block))) {
    txt <- swap(txt, b[[1]], b[[2]])
  }
  writeLines(txt, qmd)
  message("Refreshed generated blocks in ", qmd, " (your prose was not touched)")
}
message(sprintf("Report %s | %s | %s", slug, if (verified) sprintf("%d checks PASS", nrow(checks)) else "UNVERIFIED", report_md5))
message("Next: write the TODOs, then  Rscript scripts/check_game_post.R ", slug)
