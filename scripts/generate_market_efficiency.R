#!/usr/bin/env Rscript
# ═══════════════════════════════════════════════════════════════════════════
# generate_market_efficiency.R
#
# Regenerates every artifact the market-efficiency post reads, straight from
# the saved backtest/model parquet files in the private nfl_analytics repo.
# The blog holds NO hand-typed numbers from that study: re-run this, and the
# post is current.
#
#   Rscript scripts/generate_market_efficiency.R [path-to-nfl_analytics]
#
# The source repo is READ-ONLY here. This script never sources the pipeline,
# never rebuilds ratings or models, and never writes outside blog/_generated/.
#
# Source of truth : <nfl_analytics>/data/03_features/37_bootleg_power_rankings.parquet
#                   <nfl_analytics>/data/04_models/*.parquet
#                   <nfl_analytics>/data/05_backtests/*.parquet   (10 files)
# Delivery payload: blog/_generated/{power-index.png, power-index.qmd,
#                   efficiency.png, efficiency.qmd, roi.png, strategies.qmd,
#                   incremental.qmd} plus provenance.html and manifest.qmd
#                   (data range, UTC stamp, Git SHA, SHA-256s)
#
# Ported from nfl_analytics/site/scripts/render-artifacts.R. The validation
# gate, the interval arithmetic and the chart specs are deliberately identical.
# Needs: arrow, ggplot2, knitr, digest.
# ═══════════════════════════════════════════════════════════════════════════

artifact_paths <- c(
  "37_bootleg_power_rankings" = "data/03_features/37_bootleg_power_rankings.parquet",
  "36_model_metrics" = "data/04_models/36_model_metrics.parquet",
  "36_predictions" = "data/04_models/36_predictions.parquet",
  "36_feature_importance" = "data/04_models/36_feature_importance.parquet",
  "36_calibration" = "data/04_models/36_calibration.parquet",
  "36_incremental_value" = "data/04_models/36_incremental_value.parquet",
  "38_market_efficiency" = "data/05_backtests/38_market_efficiency.parquet",
  "38_strategy_summary" = "data/05_backtests/38_strategy_summary.parquet",
  "38_strategy_by_season" = "data/05_backtests/38_strategy_by_season.parquet",
  "38_bets" = "data/05_backtests/38_bets.parquet"
)

read_public_artifacts <- function(root) {
  paths <- file.path(root, artifact_paths)
  if (any(!file.exists(paths))) {
    stop("Missing saved artifacts: ", paste(artifact_paths[!file.exists(paths)], collapse = ", "),
         ". Restore reviewed artifacts separately; this render never rebuilds them.")
  }
  x <- setNames(lapply(paths, function(p) as.data.frame(arrow::read_parquet(p))),
                names(artifact_paths))
  validate_public_artifacts(x)
  x
}

validate_public_artifacts <- function(x) {
  required <- list(
    "37_bootleg_power_rankings" = c("season", "week", "team", "games_played",
                                   "bootleg_power_score", "power_rank", "tier",
                                   "tier_label", "is_flawed_contender"),
    "36_model_metrics" = c("model", "experiment", "n", "brier"),
    "36_predictions" = c("game_id", "season", "experiment", "pred_calibrated"),
    "36_feature_importance" = c("experiment", "feature", "importance"),
    "36_calibration" = c("experiment", "n", "mean_predicted", "observed_rate"),
    "36_incremental_value" = c("experiment", "n", "brier_delta", "ci_low", "ci_high"),
    "38_market_efficiency" = c("result_type", "test", "term", "n", "estimate",
                              "std_error", "null_value", "p_vs_null"),
    "38_strategy_summary" = c("strategy", "n_bets", "roi"),
    "38_strategy_by_season" = c("strategy", "season", "n_bets", "roi"),
    "38_bets" = c("game_id", "season", "strategy", "stake", "pnl", "threshold", "signal_source")
  )
  for (name in names(required)) {
    if (!is.data.frame(x[[name]]) || !nrow(x[[name]]) ||
        !all(required[[name]] %in% names(x[[name]]))) stop("Artifact schema mismatch: ", name)
  }
  s <- x[["38_strategy_summary"]]
  b <- x[["38_bets"]]
  inc <- x[["36_incremental_value"]]
  e <- x[["38_market_efficiency"]]
  e <- e[e$result_type == "efficiency_test", ]
  # Editorial gate, NOT a pipeline/model contract: a changed finding requires
  # reviewing this article, not silently publishing old prose over new evidence.
  stopifnot("Evidence changed: review public article before rendering" =
              nrow(s) == 6L && all(is.finite(s$roi)) && all(s$roi < 0) &&
              nrow(e) == 4L && all(e$p_vs_null > 0.05) &&
              nrow(inc) == 2L && all(inc$brier_delta < 0) &&
              all(inc$ci_low < 0 & inc$ci_high > 0),
            "Reviewed coverage changed" =
              identical(sort(unique(b$season)), 2022:2025) &&
              identical(sort(unique(x[["36_predictions"]]$season)), 2024:2025),
            all(b$signal_source == "walk_forward"),
            !anyDuplicated(s$strategy), all(is.finite(e$std_error)),
            all(e$std_error > 0), all(e$n > 2))
  for (strategy in s$strategy) {
    rows <- b[b$strategy == strategy, ]
    saved <- s[s$strategy == strategy, ]
    stopifnot(nrow(rows) == saved$n_bets, all(is.finite(rows$pnl)),
              all(is.finite(rows$stake) & rows$stake > 0),
              abs(sum(rows$pnl) / sum(rows$stake) - saved$roi) < 0.000006)
  }
  invisible(TRUE)
}

efficiency_intervals <- function(headline) {
  e <- headline[headline$result_type == "efficiency_test", ]
  keys <- c("rating_diff -> market error (closing)|rating_diff",
            "closing spread unbiasedness|market_spread",
            "closing spread unbiasedness|(Intercept)",
            "rating_diff -> ATS cover (logistic)|rating_diff")
  observed <- paste(e$test, e$term, sep = "|")
  if (anyDuplicated(observed) || !setequal(keys, observed)) {
    stop("Efficiency tests changed: review chart labels and nulls.")
  }
  e <- e[match(keys, observed), ]
  stopifnot(identical(as.numeric(e$null_value), c(0, 1, 0, 0)))
  # Approximate 95% intervals using the producer's t reference (df = n - 2),
  # including its logistic row. Ordinary model SEs, NOT cluster-robust intervals.
  radius <- stats::qt(0.975, df = e$n - 2) * e$std_error
  e$lower <- e$estimate - radius
  e$upper <- e$estimate + radius
  e
}

# ── Power index ────────────────────────────────────────────────────────────
# The descriptive composite the article is about. It is read, never rebuilt:
# the tiers below are the quintile taxonomy R/37 IMPOSES on the composite, not
# observed or validated external ratings (see that script's Honesty Note 1).
POWER_INDEX_SEASON <- 2025L

#' Latest week of `season` in which every ranked team has a row: the most
#' recent COMPLETED week, taken from the artifact rather than from a calendar.
power_index_latest_week <- function(pr, season = POWER_INDEX_SEASON) {
  d <- pr[pr$season == season, ]
  if (!nrow(d)) stop("Power index has no rows for season ", season)
  per_week <- table(d$week)
  full <- as.integer(names(per_week)[per_week == max(per_week)])
  max(full)
}

#' The 32-team snapshot for that week, ordered best to worst, with structural
#' checks that would catch a changed grain, tier scheme or ranking convention.
power_index_snapshot <- function(pr, season = POWER_INDEX_SEASON) {
  week <- power_index_latest_week(pr, season)
  d <- pr[pr$season == season & pr$week == week, ]
  d <- d[order(d$power_rank), ]
  stopifnot("Power index snapshot changed: review chart and copy" =
              nrow(d) == 32L && !anyDuplicated(d$team) &&
              identical(d$power_rank, 1:32) &&
              all(is.finite(d$bootleg_power_score)) &&
              !is.unsorted(rev(d$bootleg_power_score)) &&
              setequal(unique(d$tier), c("S", "A", "B", "C", "D")) &&
              all(tapply(d$tier_label, d$tier, function(v) length(unique(v))) == 1L))
  d$tier <- factor(d$tier, levels = c("S", "A", "B", "C", "D"))
  d <- d[order(d$tier, d$power_rank), ]
  d$week <- week
  d
}

#' Wrap the artifact's own tier label so it fits a facet strip unedited.
wrap_tier_label <- function(x, width = 26) {
  vapply(x, function(s) paste(strwrap(s, width = width), collapse = "\n"),
         character(1), USE.NAMES = FALSE)
}

render_market_efficiency <- function(source_root, blog_dir) {
  x <- read_public_artifacts(source_root)
  out <- file.path(blog_dir, "_generated")
  dir.create(out, showWarnings = FALSE, recursive = TRUE)
  stamp <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  # Read-only interrogation of the source checkout: the SHA that identifies the
  # data repository state these numbers came out of.
  sha <- system2("git", c("-C", shQuote(source_root), "rev-parse", "HEAD"), stdout = TRUE)
  if (length(sha) != 1L || !grepl("^[0-9a-f]{40}$", sha)) stop("Git SHA unavailable")
  dirty <- length(system2("git", c("-C", shQuote(source_root), "status", "--porcelain"), stdout = TRUE)) > 0
  scope <- "Research only - no picks, no staking advice, no established market edge."
  coverage <- paste("NFL regular seasons 2021-2025 (efficiency universe / model development);",
                    "strategy evaluation 2022-2025; saved model tests 2024-2025.")
  # The efficiency summary has no season column. Its range is the reviewed
  # producer scope, not inferred from the shorter bet ledger.
  s <- x[["38_strategy_summary"]]
  b <- x[["38_bets"]]
  e <- efficiency_intervals(x[["38_market_efficiency"]])
  inc <- x[["36_incremental_value"]]
  for (i in seq_len(nrow(s))) {
    rows <- b[b$strategy == s$strategy[i], ]
    th <- unique(rows$threshold)
    stopifnot(length(th) == 1L)
    s$label[i] <- if (startsWith(s$strategy[i], "flat_spread")) {
      sprintf("Spread | edge >= %g pts", th)
    } else sprintf("Moneyline | edge >= %g pp", th * 100)
  }
  caption <- function(paths, range) paste(
    range, paste("Generated:", stamp), paste("Source repo Git SHA:", sha),
    paste("Sources:", paste(paths, collapse = "\n")), scope, sep = "\n")
  theme <- ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"),
                   plot.caption = ggplot2::element_text(size = 8, hjust = 0),
                   panel.grid.minor = ggplot2::element_blank(),
                   plot.margin = ggplot2::margin(16, 20, 16, 16))

  # ── Power index: the ranking itself, before any statistical test ──────────
  pi <- power_index_snapshot(x[["37_bootleg_power_rankings"]])
  pi_week <- pi$week[1]
  pi_range <- sprintf("Data range: %d regular season, through week %d (composite is a rolling window inclusive of that week)",
                      POWER_INDEX_SEASON, pi_week)
  pi$strip <- factor(wrap_tier_label(pi$tier_label),
                     levels = wrap_tier_label(unique(pi$tier_label)))
  pi$row_label <- sprintf("%2d. %s", pi$power_rank, pi$team)
  pi$row_label <- factor(pi$row_label, levels = rev(pi$row_label))
  tier_fill <- c("S" = "#175c72", "A" = "#3f8ea5", "B" = "#7d8892",
                 "C" = "#c07a4e", "D" = "#a73c36")
  pi_pad <- diff(range(pi$bootleg_power_score)) * 0.18
  power <- ggplot2::ggplot(pi, ggplot2::aes(x = bootleg_power_score, y = row_label,
                                            fill = tier)) +
    ggplot2::geom_vline(xintercept = 0, colour = "#455667") +
    ggplot2::geom_col(width = 0.68) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%+.2f", bootleg_power_score),
                                    hjust = ifelse(bootleg_power_score >= 0, -0.15, 1.15)),
                       size = 2.9, colour = "#22303c") +
    ggplot2::scale_fill_manual(values = tier_fill, guide = "none") +
    ggplot2::scale_x_continuous(
      limits = c(min(pi$bootleg_power_score) - pi_pad, max(pi$bootleg_power_score) + pi_pad)) +
    ggplot2::facet_grid(rows = ggplot2::vars(strip), scales = "free_y", space = "free_y",
                        switch = "y") +
    ggplot2::labs(
      title = sprintf("The power index itself: all 32 teams, %d through week %d",
                      POWER_INDEX_SEASON, pi_week),
      subtitle = paste("Score is standard deviations above or below that week's league average.",
                       "Tier bands are the five equal slices of the league this project imposes on the score -",
                       "its own labels, not observed or validated external ratings.", sep = "\n"),
      x = "Power index score (standard deviations from the league average)", y = NULL,
      caption = caption(artifact_paths["37_bootleg_power_rankings"], pi_range)) +
    theme +
    ggplot2::theme(strip.placement = "outside",
                   strip.text.y.left = ggplot2::element_text(angle = 0, hjust = 0, size = 8.5,
                                                             face = "bold", colour = "#22303c"),
                   panel.grid.major.y = ggplot2::element_blank(),
                   panel.spacing.y = ggplot2::unit(6, "pt"),
                   axis.text.y = ggplot2::element_text(size = 8.5, family = "mono"))
  ggplot2::ggsave(file.path(out, "power-index.png"), power, width = 11, height = 10,
                  dpi = 150, bg = "white")

  # Layer-1 reading aid: one row per tier band, every figure taken from the
  # snapshot above so the prose never hand-types a rank, a count or a score.
  pi_tiers <- do.call(rbind, lapply(levels(pi$tier), function(t) {
    g <- pi[pi$tier == t, ]
    data.frame(
      `Tier band` = as.character(g$tier_label[1]),
      `Teams` = nrow(g),
      `Ranks` = sprintf("%d-%d", min(g$power_rank), max(g$power_rank)),
      `Score range` = sprintf("%+.2f to %+.2f", max(g$bootleg_power_score),
                              min(g$bootleg_power_score)),
      `Who is in it` = paste(g$team, collapse = ", "),
      check.names = FALSE)
  }))
  writeLines(c(
    sprintf(paste("The table below reads the same snapshot as the chart: the %d regular season",
                  "through week %d, %d teams, split into five equal bands by score."),
            POWER_INDEX_SEASON, pi_week, nrow(pi)),
    "",
    as.character(knitr::kable(pi_tiers, format = "pipe", row.names = FALSE))),
    file.path(out, "power-index.qmd"))

  roi <- ggplot2::ggplot(s, ggplot2::aes(x = roi * 100,
                                       y = factor(label, levels = rev(label)))) +
    ggplot2::geom_vline(xintercept = 0, colour = "#455667") +
    ggplot2::geom_col(fill = "#a73c36", width = 0.6) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f%%  |  n=%d", roi * 100, n_bets)),
                       hjust = -0.05, size = 3.4) +
    ggplot2::scale_x_continuous(limits = c(min(s$roi * 100) - 1, 3)) +
    ggplot2::labs(title = "Every tested strategy lost in aggregate",
      subtitle = "Different threshold units: spread points vs moneyline probability percentage points",
      x = "Net ROI (% of stake wagered)", y = NULL,
      caption = caption(artifact_paths[c("38_strategy_summary", "38_bets")],
                        "Data range: strategy evaluation 2022-2025; regular season")) + theme
  ggplot2::ggsave(file.path(out, "roi.png"), roi, width = 11, height = 7, dpi = 150, bg = "white")

  e$label <- c("Rating -> closing-line error (points / rating unit)",
               "Closing-spread slope (points / point)",
               "Closing-spread intercept (points)",
               "Rating -> ATS cover (log-odds / rating unit)")
  e$label <- factor(e$label, levels = e$label)
  eff <- ggplot2::ggplot(e, ggplot2::aes(x = estimate, y = 1)) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = null_value), linetype = 2, colour = "#a73c36") +
    ggplot2::geom_errorbar(ggplot2::aes(xmin = lower, xmax = upper), orientation = "y", width = 0.15) +
    ggplot2::geom_point(size = 3, colour = "#175c72") +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("n=%d | p=%.3f", n, p_vs_null)), y = 1.4, size = 3.3) +
    ggplot2::facet_wrap(~label, scales = "free_x", ncol = 2) +
    ggplot2::scale_y_continuous(limits = c(0.6, 1.6), breaks = NULL) +
    ggplot2::labs(title = "No significant residual signal detected",
      subtitle = "Approximate 95% t intervals from saved SEs; dashed lines mark each test's null",
      x = "Coefficient estimate (panel-specific units and scales)", y = NULL,
      caption = caption(artifact_paths["38_market_efficiency"],
                        "Data range: efficiency universe 2021-2025; regular season (documented producer scope)")) + theme
  ggplot2::ggsave(file.path(out, "efficiency.png"), eff, width = 11, height = 7, dpi = 150, bg = "white")

  writeLines(knitr::kable(data.frame(Strategy = s$label, Bets = s$n_bets,
    `Net ROI` = sprintf("%.2f%%", s$roi * 100), check.names = FALSE), format = "pipe"),
    file.path(out, "strategies.qmd"))
  writeLines(knitr::kable(data.frame(Test = as.character(e$label), n = e$n,
    Estimate = sprintf("%.5f", e$estimate), Null = e$null_value,
    `Approx. 95% interval` = sprintf("[%.5f, %.5f]", e$lower, e$upper),
    `p vs null` = sprintf("%.5f", e$p_vs_null), check.names = FALSE), format = "pipe"),
    file.path(out, "efficiency.qmd"))
  writeLines(knitr::kable(data.frame(Split = inc$experiment, Games = inc$n,
    `Brier delta: full minus baseline` = sprintf("%+.5f", inc$brier_delta),
    `Paired-bootstrap 95% CI` = sprintf("[%+.5f, %+.5f]", inc$ci_low, inc$ci_high),
    check.names = FALSE), format = "pipe"), file.path(out, "incremental.qmd"))

  template <- readLines(file.path(blog_dir, "_includes", "provenance.html"), warn = FALSE)
  sources <- paste0("<li><code>", artifact_paths, "</code></li>", collapse = "\n")
  fields <- c("{{DATA_RANGE}}" = coverage, "{{TIMESTAMP}}" = stamp, "{{GIT_SHA}}" = sha,
              "{{WORKTREE}}" = if (dirty) "dirty / includes uncommitted files" else "clean",
              "{{SOURCES}}" = sources, "{{DISCLAIMER}}" = scope)
  for (key in names(fields)) template <- gsub(key, fields[[key]], template, fixed = TRUE)
  writeLines(template, file.path(out, "provenance.html"))

  manifest <- data.frame(`Source artifact path` = unname(artifact_paths),
    Rows = unname(vapply(x, nrow, integer(1))),
    SHA256 = unname(vapply(file.path(source_root, artifact_paths), function(p) digest::digest(file = p, algo = "sha256"), character(1))),
    check.names = FALSE, row.names = NULL)
  writeLines(c("### Artifact fingerprints", "", paste("Generated:", stamp), "",
    paste("Source repo Git SHA:", sha), "", paste("Data range:", coverage), "", scope, "",
    as.character(knitr::kable(manifest, format = "pipe", row.names = FALSE))), file.path(out, "manifest.qmd"))
  # Hash the publication sources as well: HEAD alone cannot identify dirty files.
  public_sources <- sort(c(file.path(blog_dir, "posts", "market-efficiency", "index.qmd"),
                           file.path(blog_dir, "scripts", "generate_market_efficiency.R"),
                           file.path(blog_dir, "_includes", "provenance.html")))
  public_sources <- public_sources[file.exists(public_sources)]
  source_manifest <- data.frame(Path = substring(public_sources, nchar(blog_dir) + 2),
    SHA256 = unname(vapply(public_sources, function(p) digest::digest(file = p, algo = "sha256"), character(1))),
    row.names = NULL)
  writeLines(c("### Publication-source fingerprints", "",
    "These identify the current publication sources, including uncommitted edits.", "",
    as.character(knitr::kable(source_manifest, format = "pipe", row.names = FALSE))),
    file.path(out, "source-manifest.qmd"))
  message("Market-efficiency artifacts: read 10 parquet files from ", source_root,
          "; wrote 3 charts, 4 tables, provenance and manifests under ", out, ".")
  message("Generated: ", stamp, " | Source repo Git SHA: ", sha,
          " | source worktree ", if (dirty) "dirty" else "clean")
  invisible(out)
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
  blog_dir <- normalizePath(file.path(dirname(script), ".."), mustWork = TRUE)
  source_root <- if (length(args) >= 1) args[1] else
    Sys.getenv("NFL_ANALYTICS_ROOT", path.expand("~/Documents/nfl_analytics"))
  if (!dir.exists(source_root)) {
    stop("nfl_analytics checkout not found at: ", source_root,
         "\nPass it as an argument or set NFL_ANALYTICS_ROOT.", call. = FALSE)
  }
  render_market_efficiency(normalizePath(source_root, mustWork = TRUE), blog_dir)
}
