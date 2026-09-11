#!/usr/bin/env Rscript
# ═══════════════════════════════════════════════════════════════════════════
# generate_team_pages.R
#
# Regenerates every teams/*.qmd dossier and teams/index.qmd straight from the
# nfl_analytics pipeline outputs. The blog holds NO hand-typed numbers: re-run
# the pipeline, re-run this, and the site is current.
#
#   Rscript scripts/generate_team_pages.R [season]
#
# Source of truth : ~/Documents/nfl_analytics/data/processed/*.rds
# Delivery payload: blog/teams/<team-slug>.qmd  +  blog/teams/index.qmd
# Unique key      : posteam x season
# ═══════════════════════════════════════════════════════════════════════════

suppressPackageStartupMessages({
  library(dplyr)
})

args      <- commandArgs(trailingOnly = TRUE)
SEASON    <- if (length(args) >= 1) as.integer(args[1]) else 2025
PROJ_YEAR <- SEASON + 1

BLOG_DIR <- normalizePath(file.path(dirname(sub("^--file=", "", grep("^--file=",
              commandArgs(FALSE), value = TRUE)[1])), ".."), mustWork = FALSE)
if (is.na(BLOG_DIR) || !dir.exists(BLOG_DIR)) BLOG_DIR <- getwd()

NFL_DIR  <- path.expand("~/Documents/nfl_analytics/data/processed")
TEAMS_DIR <- file.path(BLOG_DIR, "teams")
dir.create(TEAMS_DIR, showWarnings = FALSE, recursive = TRUE)

# writeLines() re-encodes to the native locale and mangles em-dashes into
# <U+2014> literals. Write raw UTF-8 bytes instead.
write_utf8 <- function(txt, path) {
  con <- file(path, open = "wb")
  on.exit(close(con), add = TRUE)
  writeBin(charToRaw(enc2utf8(paste0(txt, "\n"))), con)
}

read_or_stop <- function(f) {
  p <- file.path(NFL_DIR, f)
  if (!file.exists(p)) stop("Missing pipeline output: ", p, call. = FALSE)
  suppressWarnings(readRDS(p))
}

# ── Load ──────────────────────────────────────────────────────────────────
cp    <- read_or_stop("complete_picture_all.rds")
qbr   <- read_or_stop("qb_rolling.rds")
rb    <- read_or_stop("rb_kpis.rds")
# Projections are named for the season they project. Fall back gracefully so a
# back-season rebuild does not silently staple 2026 numbers onto a 2024 page.
proj_file <- sprintf("projections_%d.rds", PROJ_YEAR)
if (file.exists(file.path(NFL_DIR, proj_file))) {
  proj <- suppressWarnings(readRDS(file.path(NFL_DIR, proj_file)))
} else {
  warning("No ", proj_file, " - projection blocks will be omitted.", call. = FALSE)
  proj <- data.frame(team = character(0), projected_wins = numeric(0),
                     ci_lower = numeric(0), ci_upper = numeric(0),
                     fragility_flag = character(0), stringsAsFactors = FALSE)
}

cp <- cp |> filter(season == SEASON)
if (nrow(cp) == 0) stop("No rows in complete_picture_all for season ", SEASON)

TEAM_NAMES <- c(
  ARI="Arizona Cardinals",   ATL="Atlanta Falcons",     BAL="Baltimore Ravens",
  BUF="Buffalo Bills",       CAR="Carolina Panthers",   CHI="Chicago Bears",
  CIN="Cincinnati Bengals",  CLE="Cleveland Browns",    DAL="Dallas Cowboys",
  DEN="Denver Broncos",      DET="Detroit Lions",       GB ="Green Bay Packers",
  HOU="Houston Texans",      IND="Indianapolis Colts",  JAX="Jacksonville Jaguars",
  KC ="Kansas City Chiefs",  LA ="Los Angeles Rams",    LAC="Los Angeles Chargers",
  LAR="Los Angeles Rams",    LV ="Las Vegas Raiders",   MIA="Miami Dolphins",
  MIN="Minnesota Vikings",   NE ="New England Patriots",NO ="New Orleans Saints",
  NYG="New York Giants",     NYJ="New York Jets",       PHI="Philadelphia Eagles",
  PIT="Pittsburgh Steelers", SEA="Seattle Seahawks",    SF ="San Francisco 49ers",
  TB ="Tampa Bay Buccaneers",TEN="Tennessee Titans",    WAS="Washington Commanders"
)

slugify <- function(x) gsub("[^a-z0-9]+", "-", tolower(x))

badge_class <- function(a) {
  switch(a,
    "Contender"            = "arch-contender",
    "Offensive Juggernaut" = "arch-juggernaut",
    "Defense-Led"          = "arch-defense",
    "Defensive Fortress"   = "arch-fortress",
    "Balanced"             = "arch-balanced",
    "Pretender"            = "arch-pretender",
    "Rebuilder"            = "arch-rebuilder",
    "arch-balanced")
}

# Plain-language gloss so a reader never meets a bare label.
ARCH_GLOSS <- c(
  "Contender"            = "Top-quartile passing offence with a defence that at least holds up. The profile that actually wins in January.",
  "Offensive Juggernaut" = "Elite passing offence dragging a bottom-quartile defence. Fun, volatile, and usually one shootout from elimination.",
  "Defense-Led"          = "Middling offence, top-quartile defence. Wins are real but the ceiling is set by the quarterback.",
  "Defensive Fortress"   = "Bottom-quartile offence carried entirely by an elite defence. Historically the least stable archetype year to year.",
  "Balanced"             = "Competent in both phases, elite in neither. The league's default state.",
  "Pretender"            = "Competent offence, poor defence. Records here tend to overstate the underlying team.",
  "Rebuilder"            = "Bottom-quartile passing offence. Nothing else matters until that changes."
)

fmt <- function(x, d = 1) formatC(round(x, d), format = "f", digits = d, big.mark = "")

# ── Diverging bar block ───────────────────────────────────────────────────
# `value` scaled against `lim` (a symmetric league-wide max) so every team's
# bars are directly comparable across pages.
epa_bar <- function(label, value, lim) {
  pct <- max(0, min(50, abs(value) / lim * 50))
  cls <- if (value >= 0) "epa-fill epa-pos" else "epa-fill epa-neg"
  txt <- if (value > 0) paste0("+", fmt(value, 3)) else fmt(value, 3)
  sprintf(
'  <div class="epa-row">
    <div class="epa-label">%s</div>
    <div class="epa-track"><div class="%s" style="width:%.2f%%"></div></div>
    <div class="epa-value">%s</div>
  </div>', label, cls, pct, txt)
}

# Everything is put on ONE per-play scale so the four bars on a page are
# genuinely comparable to each other, not just across teams. Season totals are
# NOT usable here: pass offence totals run to +/-200 while defence is per-play.
cp$rush_plays  <- cp$total_plays * cp$run_rate
cp$rush_epa_pp <- cp$rush_epa / cp$rush_plays

EPA_LIM <- max(abs(c(cp$pass_epa_pp, cp$rush_epa_pp,
                     cp$def_pass_epa_allowed, cp$def_rush_epa_allowed)),
               na.rm = TRUE)

# ── Per-team page ─────────────────────────────────────────────────────────
build_page <- function(r) {
  code <- r$posteam
  name <- if (!is.na(TEAM_NAMES[code])) TEAM_NAMES[[code]] else code
  slug <- slugify(name)

  losses <- r$games - r$wins
  arch   <- r$archetype
  gloss  <- if (!is.na(ARCH_GLOSS[arch])) ARCH_GLOSS[[arch]] else ""

  # QB: last rolling-5 row of the season with enough dropbacks
  q <- qbr |>
    filter(posteam == code, season == SEASON, !is.na(epa_per_dropback_rolling5)) |>
    arrange(desc(week))
  qb_line <- if (nrow(q) > 0) {
    q1 <- q[1, ]
    sprintf("**%s** closed the season at **%s EPA per dropback** and **%s%% CPOE** over his rolling five-game window (through week %d).",
            q1$passer_name, fmt(q1$epa_per_dropback_rolling5, 3),
            fmt(q1$cpoe_rolling5, 1), q1$week)
  } else "No qualifying rolling-five-game passer window this season."

  # Lead rusher by carries
  b <- rb |>
    filter(posteam == code, season == SEASON) |>
    arrange(desc(carries))
  rb_line <- if (nrow(b) > 0 && b$carries[1] >= 20) {
    b1 <- b[1, ]
    sprintf("**%s** led the backfield with **%d carries** at **%s EPA per rush** (%s YPC, %s%% success). Blocking versus creation: **%s** yards before contact, **%s** after. Style: *%s*.",
            b1$rusher_player_name, b1$carries, fmt(b1$epa_per_rush, 3),
            fmt(b1$yards_per_carry, 1), fmt(b1$success_rate * 100, 1),
            fmt(b1$ybc_per_carry, 1), fmt(b1$yac_per_carry, 1),
            if (is.na(b1$rb_style)) "unclassified" else b1$rb_style)
  } else "No back reached a 20-carry workload this season."

  # 2026 projection
  p <- proj |> filter(team == code)
  proj_block <- if (nrow(p) > 0) {
    sprintf("The multi-year prior model projects **%s wins** for %d, with a 90%% interval of **%s to %s**. Fragility flag: **%s**.",
            fmt(p$projected_wins[1]), PROJ_YEAR,
            fmt(p$ci_lower[1]), fmt(p$ci_upper[1]), p$fragility_flag[1])
  } else sprintf("No %d projection available for this team.", PROJ_YEAR)

  bars <- paste(
    epa_bar("Pass offence",  r$pass_epa_pp,           EPA_LIM),
    epa_bar("Rush offence",  r$rush_epa_pp,           EPA_LIM),
    epa_bar("Pass defence", -r$def_pass_epa_allowed,  EPA_LIM),
    epa_bar("Rush defence", -r$def_rush_epa_allowed,  EPA_LIM),
    sep = "\n")

  sprintf(
'---
title: "%s"
subtitle: "%d Analytical Dossier and %d Outlook"
description: "%s: EPA efficiency profile, offensive identity, quarterback form, and %d win projection."
date: "%s"
author: "Helio"
categories: [teams, %s]
---

```{=html}
<div class="team-hero">
  <div class="th-name">%s</div>
  <div class="th-record">%d&ndash;%d</div>
  <span class="arch-badge %s">%s</span>
</div>

<div class="insight-strip">
  <div class="insight-item">
    <div class="insight-value">%s</div>
    <div class="insight-label">Pass EPA / play</div>
  </div>
  <div class="insight-item">
    <div class="insight-value">#%d</div>
    <div class="insight-label">Defence rank (overall)</div>
  </div>
  <div class="insight-item">
    <div class="insight-value">%s%%</div>
    <div class="insight-label">Run rate</div>
  </div>
  <div class="insight-item">
    <div class="insight-value">%s</div>
    <div class="insight-label">%d projected wins</div>
  </div>
</div>
```

::: {.lead}
%s
:::

## What the four phases look like

::: {.sec-note}
EPA **per play** in each of the four phases. Blue is creating points, red is costing them. Defence is sign-flipped so longer blue always means better. All four bars share one league-wide scale (max %s EPA/play), so they are comparable to each other *and* to every other team page. Season totals are deliberately not used here &mdash; mixing totals and per-play rates on one axis would make the offence bars look four times longer than they are.
:::

```{=html}
<div class="epa-bars">
%s
  <div class="epa-scale">&larr; costing points &nbsp;&nbsp;|&nbsp;&nbsp; creating points &rarr;</div>
</div>
```

## Identity

This team classifies as **%s** &mdash; *%s*

Underneath, the offence reads as **"%s"**: %s

| Dimension | Grade |
|---|---|
| Offence tier | **%s** |
| Defence tier | **%s** |
| Pass defence rank | **#%d** |
| Rush defence rank | **#%d** |
| Wins above / below model | **%s** |

## Personnel

**Quarterback.** %s

**Running back.** %s

## %d outlook

%s

```{=html}
<div class="provenance">
  Source: <code>nfl_analytics</code> pipeline &mdash; <code>complete_picture_all.rds</code>,
  <code>qb_rolling.rds</code>, <code>rb_kpis.rds</code>, <code>projections_%d.rds</code>,
  built from <code>nflreadr</code> play-by-play. Archetype and tier thresholds are the
  5-season empirical quartiles documented in the
  <a href="../posts/nfl-team-archetypes/index.qmd">methodology post</a>.
  This page is generated by <code>scripts/generate_team_pages.R</code> &mdash; no figure on it was typed by hand.
</div>
```

[&larr; All 32 teams](index.qmd) &middot; [How the archetypes are built](../posts/nfl-team-archetypes/index.qmd)
',
    name, SEASON, PROJ_YEAR, name, PROJ_YEAR,
    format(Sys.Date()), tolower(code),
    name, r$wins, losses, badge_class(arch), arch,
    fmt(r$pass_epa_pp, 3), r$def_overall_rank, fmt(r$run_rate * 100, 1),
    if (nrow(p) > 0) fmt(p$projected_wins[1]) else "&mdash;", PROJ_YEAR,
    sprintf("%s finished %d at %d&ndash;%d. The model grades them as a %s: %s",
            name, SEASON, r$wins, losses, tolower(arch), tolower(substr(gloss, 1, 1))) |>
      paste0(substr(gloss, 2, nchar(gloss))),
    fmt(EPA_LIM, 3),
    bars,
    arch, gloss,
    r$identity,
    "the label describes how the offence distributes its production between the run and the pass, not how good it is.",
    r$off_tier, r$def_tier, r$def_pass_rank, r$def_rush_rank,
    if (!is.na(r$wins_residual)) sprintf("%s%s", if (r$wins_residual > 0) "+" else "", fmt(r$wins_residual)) else "&mdash;",
    qb_line, rb_line,
    PROJ_YEAR, proj_block,
    PROJ_YEAR
  ) |> list(slug = slug, name = name) |> (\(x) x)()
}

# Build every page
pages <- lapply(seq_len(nrow(cp)), function(i) {
  r <- cp[i, ]
  code <- r$posteam
  name <- if (!is.na(TEAM_NAMES[code])) TEAM_NAMES[[code]] else code
  slug <- slugify(name)
  txt  <- build_page(r)[[1]]
  write_utf8(txt, file.path(TEAMS_DIR, paste0(slug, ".qmd")))
  data.frame(code = code, name = name, slug = slug,
             wins = r$wins, losses = r$games - r$wins,
             archetype = r$archetype, identity = r$identity,
             off_tier = r$off_tier, def_tier = r$def_tier,
             pass_epa_pp = r$pass_epa_pp,
             stringsAsFactors = FALSE)
})
idx <- bind_rows(pages)

idx <- idx |>
  left_join(proj |> select(code = team, projected_wins), by = "code") |>
  arrange(desc(wins), desc(pass_epa_pp))

# ── Index page: cards grouped by archetype ────────────────────────────────
ARCH_ORDER <- c("Contender", "Offensive Juggernaut", "Defense-Led",
                "Defensive Fortress", "Balanced", "Pretender", "Rebuilder")

card <- function(row) sprintf(
'  <a class="team-card" href="%s.qmd">
    <div class="tc-top">
      <span class="tc-name">%s</span>
      <span class="tc-record">%d&ndash;%d</span>
    </div>
    <span class="arch-badge %s">%s</span>
    <div class="tc-meta">%s<br>Offence <strong>%s</strong> &middot; Defence <strong>%s</strong> &middot; %d proj <strong>%sW</strong></div>
  </a>',
  row$slug, row$name, row$wins, row$losses,
  badge_class(row$archetype), row$archetype, row$identity,
  row$off_tier, row$def_tier, PROJ_YEAR,
  if (is.na(row$projected_wins)) "&mdash;" else fmt(row$projected_wins))

sections <- character(0)
for (a in ARCH_ORDER) {
  sub <- idx |> filter(archetype == a)
  if (nrow(sub) == 0) next
  cards <- paste(vapply(seq_len(nrow(sub)), \(i) card(sub[i, ]), character(1)),
                 collapse = "\n")
  sections <- c(sections, sprintf(
'## %s <span style="font-weight:400;color:var(--color-text-muted);font-size:.8em">&nbsp;%d team%s</span>

::: {.sec-note}
%s
:::

```{=html}
<div class="team-grid">
%s
</div>
```
', a, nrow(sub), if (nrow(sub) == 1) "" else "s",
   if (!is.na(ARCH_GLOSS[a])) ARCH_GLOSS[[a]] else "", cards))
}

index_txt <- sprintf(
'---
title: "All 32 Teams, Sorted by What They Actually Are"
subtitle: "%d season, grouped by archetype rather than by conference"
description: "Every NFL franchise\'s %d analytical dossier &mdash; EPA efficiency, offensive identity, and %d win projection &mdash; grouped by archetype."
page-layout: full
toc: false
---

::: {.lead}
Standings sort teams by results. This page sorts them by *profile* &mdash; the combination of passing efficiency and defensive suppression that the win model says explains roughly three quarters of win variation. Teams sitting in the same group played fundamentally similar football in %d, whatever their records say.
:::

%s

```{=html}
<div class="provenance">
  %d team-seasons, %d regular season. Generated from the <code>nfl_analytics</code>
  pipeline by <code>scripts/generate_team_pages.R</code>. Tier cutoffs are 5-season
  empirical quartiles &mdash; see the
  <a href="../posts/nfl-team-archetypes/index.qmd">methodology post</a> for how the
  groups are drawn and where the rules are weakest.
</div>
```
', SEASON, SEASON, PROJ_YEAR, SEASON, paste(sections, collapse = "\n"),
   nrow(idx), SEASON)

write_utf8(index_txt, file.path(TEAMS_DIR, "index.qmd"))

message(sprintf("Generated %d team pages + index for season %d -> %s",
                nrow(idx), SEASON, TEAMS_DIR))
