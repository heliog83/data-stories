# Helio's Data Stories — blog

Quarto static site. Published output is `_site/` (git-ignored — rebuild, don't commit).

## The one rule

**No numbers are typed by hand into `teams/*.qmd`.** Those 33 files (32 teams +
index) are *generated*. Editing them directly gets your work overwritten on the
next run.

To change what a team page says, edit `scripts/generate_team_pages.R`.

## Workflow

```bash
# 1. refresh the analysis (in the nfl_analytics repo)
cd ~/Documents/nfl_analytics && Rscript run_pipeline.R

# 2. regenerate every team dossier from the pipeline outputs
cd ~/"Documents/personal agent/projects/publishing/blog"
Rscript scripts/generate_team_pages.R 2025

# 3. build
quarto render
```

`generate_team_pages.R` takes the season as its only argument. Pass `2024` and
you get a full 2024 site instead — the season is not hardcoded anywhere.

## Preview without waiting 23 seconds

`quarto preview` rebuilds all 36 pages before it serves anything. When working on
one page, preview only that page:

```bash
quarto preview posts/nfl-team-archetypes/index.qmd
```

For the whole site, skip the initial render and let it rebuild on save:

```bash
quarto preview --no-render
```

## Data flow

| Source (nfl_analytics) | Feeds |
|---|---|
| `complete_picture_all.rds` | archetype, tiers, EPA bars, identity, record |
| `qb_rolling.rds` | quarterback rolling-5 EPA / CPOE |
| `rb_kpis.rds` | lead back carries, YBC/YAC, style |
| `projections_2026.rds` | projected wins, CI, fragility flag |

## Conventions

- Charts on team pages are **pure CSS** (`.epa-bars`), not images — zero page
  weight, no JS, and they theme with light/dark automatically.
- All four EPA bars are **EPA per play** on one shared league-wide scale. Never
  mix season totals and per-play rates on the same axis.
- Design tokens live at the top of `styles.css`. Quarto's content wrapper is
  `#quarto-document-content` (an **id**, not a class) — scoping to
  `.quarto-document-content` silently does nothing.

## Market-efficiency post

`posts/market-efficiency/` reads seven generated fragments from `_generated/`.
Those are produced by `scripts/generate_market_efficiency.R`, which reads ten
saved parquet artifacts from the **private** `nfl_analytics` repo read-only and
never rebuilds ratings or models:

```bash
Rscript scripts/generate_market_efficiency.R [path-to-nfl_analytics]
# defaults to $NFL_ANALYTICS_ROOT, else ~/Documents/nfl_analytics
```

It writes `power-index.png`, `power-index.qmd`, `efficiency.png`,
`efficiency.qmd`, `roi.png`, `strategies.qmd` and `incremental.qmd`, plus
`provenance.html` (rendered from
`_includes/provenance.html`) and the `manifest.qmd` / `source-manifest.qmd`
fingerprint tables. `_generated/` is committed so the site builds from a fresh
clone without the private data.

`power-index.png` is the index itself — all 32 teams for the most recent
**completed** week of 2025, read from `37_bootleg_power_rankings.parquet` and
grouped into its five tier bands. The week is taken from the artifact (the
latest week in which every ranked team has a row), never from a calendar. The
tier names are the taxonomy `R/37_bootleg_power_index.R` *imposes* on the
composite score — never describe them on this site as observed or validated
Bootleg Football ratings, which is a validation that has never run.

The script carries an editorial gate: if the saved evidence stops matching the
published finding (six losing strategies, four non-significant efficiency
tests, two CIs crossing zero), it stops rather than printing new numbers under
old prose.

Needs: `arrow`, `ggplot2`, `knitr`, `digest`.

## Clustering validation

`scripts/cluster_archetypes.R` answers "are the archetypes real groups?" with four
tests that can each return *no*: mclust BIC, the gap statistic, a multivariate-normal
null comparison, and `fpc::clusterboot` stability. It writes every quoted number to
`outputs/cluster_results.json` and both light/dark variants of each figure.

```bash
Rscript scripts/cluster_archetypes.R
```

**The answer is no** — 160 NFL team-seasons form one continuous cloud. That result
is the subject of `posts/no-clusters/`. Do not re-describe the archetypes as
"clustered" anywhere on this site.

Needs: `mclust`, `fpc`, `cluster`, `rpart`, `MASS`, `jsonlite`, `ggplot2`.

## Figures in both themes

PNGs can't restyle themselves, so every figure ships twice — `*_light.png` and
`*_dark.png` — swapped by CSS on `[data-bs-theme="dark"]`. Use the `.figpair`
markup (see `posts/no-clusters/index.qmd`) and always write real `alt` text.
