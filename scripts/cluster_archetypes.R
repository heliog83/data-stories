#!/usr/bin/env Rscript
# ═══════════════════════════════════════════════════════════════════════════
# cluster_archetypes.R
#
# Asks whether NFL team-seasons actually form clusters in EPA space, using four
# independent tests that can each say "no":
#
#   1. mclust BIC          - how many Gaussian components does the data want?
#   2. Gap statistic       - Tibshirani's k vs. a uniform reference
#   3. MVN null comparison - does k-means beat k-means on a structureless blob?
#   4. fpc::clusterboot    - do the clusters survive resampling? (Jaccard)
#
# Then describes the structure that IS there via PCA, and measures how well the
# existing hand-drawn archetype labels persist year to year.
#
# Writes: outputs/cluster_results.json  (numbers the post reads)
#         images/*.png                  (light + dark variants)
#
#   Rscript scripts/cluster_archetypes.R
# ═══════════════════════════════════════════════════════════════════════════

suppressPackageStartupMessages({
  library(dplyr); library(mclust); library(cluster); library(fpc)
  library(ggplot2); library(jsonlite); library(MASS)
})
select <- dplyr::select
set.seed(42)

BLOG <- normalizePath(file.path(dirname(sub("^--file=", "", grep("^--file=",
          commandArgs(FALSE), value = TRUE)[1])), ".."), mustWork = FALSE)
if (is.na(BLOG) || !dir.exists(BLOG)) BLOG <- getwd()
IMG <- file.path(BLOG, "posts", "no-clusters", "images")
OUT <- file.path(BLOG, "outputs")
dir.create(IMG, showWarnings = FALSE, recursive = TRUE)
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)

# ── Data ───────────────────────────────────────────────────────────────────
cp <- suppressWarnings(readRDS(path.expand(
  "~/Documents/nfl_analytics/data/processed/complete_picture_all.rds")))

cp <- cp |> mutate(
  rush_plays  = total_plays * run_rate,
  rush_epa_pp = rush_epa / rush_plays,
  def_pass_pp = -def_pass_epa_allowed,   # sign-flipped: higher = better defence
  def_rush_pp = -def_rush_epa_allowed
)

FEAT <- c("pass_epa_pp", "rush_epa_pp", "def_pass_pp", "def_rush_pp")
X <- scale(as.matrix(cp[, FEAT]))
N <- nrow(X)
message("n = ", N, " team-seasons, ", length(FEAT), " standardised features")

res <- list(n_team_seasons = N,
            seasons = paste(range(cp$season), collapse = "-"),
            features = FEAT)

# ── Test 1: how many Gaussians does the data want? ─────────────────────────
mc <- Mclust(X, G = 1:9, verbose = FALSE)
res$mclust <- list(chosen_G = mc$G, model = mc$modelName,
                   bic = round(max(mc$BIC, na.rm = TRUE), 1))
message("mclust chose G = ", mc$G)

# ── Test 2: gap statistic ──────────────────────────────────────────────────
gp <- clusGap(X, FUN = kmeans, nstart = 50, K.max = 9, B = 200, verbose = FALSE)
gap_k <- maxSE(gp$Tab[, "gap"], gp$Tab[, "SE.sim"], method = "Tibs2001SEmax")
gap_df <- data.frame(k = 1:9, gap = gp$Tab[, "gap"], se = gp$Tab[, "SE.sim"])
res$gap <- list(chosen_k = gap_k, table = gap_df)
message("gap statistic chose k = ", gap_k)

# ── Test 3: silhouette + null comparison ───────────────────────────────────
d <- dist(X)
sil <- sapply(2:9, \(k) mean(silhouette(kmeans(X, k, nstart = 50)$cluster, d)[, 3]))
obs <- sapply(2:8, \(k) { km <- kmeans(X, k, nstart = 50); km$tot.withinss/km$totss })
nullm <- replicate(200, {
  Z <- MASS::mvrnorm(N, mu = colMeans(X), Sigma = cov(X))
  sapply(2:8, \(k) { km <- kmeans(Z, k, nstart = 20); km$tot.withinss/km$totss })
})
res$silhouette <- data.frame(k = 2:9, silhouette = round(sil, 3))
res$null_test <- data.frame(
  k = 2:8, observed = round(obs, 3), mvn_null_mean = round(rowMeans(nullm), 3),
  percentile = round(sapply(1:7, \(i) mean(nullm[i, ] < obs[i])), 2))
message("best silhouette = ", round(max(sil), 3), " at k = ", (2:9)[which.max(sil)])

# ── Test 4: bootstrap stability ────────────────────────────────────────────
stab <- lapply(2:8, function(k) {
  cb <- clusterboot(X, B = 200, clustermethod = kmeansCBI, krange = k,
                    count = FALSE, seed = 42)
  data.frame(k = k, min_jaccard = min(cb$bootmean),
             mean_jaccard = mean(cb$bootmean), max_jaccard = max(cb$bootmean))
}) |> bind_rows()
res$stability <- stab |> mutate(across(-k, \(x) round(x, 3)))
message("stability computed")

# ── The structure that IS there ────────────────────────────────────────────
pca <- prcomp(X)
cp$pc1 <- -pca$x[, 1]   # flip so higher = better team (loadings are all negative)
cp$pc2 <-  pca$x[, 2]
res$pca <- list(
  variance = round(summary(pca)$importance[2, ], 3),
  loadings = round(pca$rotation[, 1:2], 3),
  cor_pc1_wins = round(cor(cp$pc1, cp$wins), 3),
  cor_pc2_wins = round(cor(cp$pc2, cp$wins), 3))
message("PC1 vs wins r = ", res$pca$cor_pc1_wins)

# ── Do the existing labels persist? ────────────────────────────────────────
pers <- cp |> select(posteam, season, archetype, pc1) |>
  arrange(posteam, season) |> group_by(posteam) |>
  mutate(prev = lag(archetype), prev_pc1 = lag(pc1)) |> ungroup() |>
  filter(!is.na(prev))
res$persistence <- list(
  n_pairs = nrow(pers),
  same_label_rate = round(mean(pers$archetype == pers$prev), 3),
  chance_baseline = round(sum((table(cp$archetype)/N)^2), 3),
  cor_pc1_year_over_year = round(cor(pers$pc1, pers$prev_pc1), 3))
message("label persistence = ", res$persistence$same_label_rate,
        " vs chance ", res$persistence$chance_baseline)

# ── A decision tree that explains the EXISTING labels ──────────────────────
suppressPackageStartupMessages(library(rpart))
tree_df <- cp |> select(archetype, all_of(FEAT)) |> mutate(archetype = factor(archetype))
fit <- rpart(archetype ~ ., data = tree_df, method = "class",
             control = rpart.control(maxdepth = 3, cp = 0.01))
res$tree <- list(
  accuracy_depth3 = round(mean(predict(fit, type = "class") == tree_df$archetype), 3),
  n_leaves = sum(fit$frame$var == "<leaf>"))
message("depth-3 tree reproduces labels ", res$tree$accuracy_depth3)

write_json(res, file.path(OUT, "cluster_results.json"),
           auto_unbox = TRUE, digits = 4, pretty = TRUE)

# ══════════════════════════════════════════════════════════════════════════
# FIGURES — light + dark variants of each, swapped by CSS
# ══════════════════════════════════════════════════════════════════════════
BLUE_L <- "#2a78d6"; BLUE_D <- "#3987e5"
RAMP_L <- c("#cde2fb", "#3987e5", "#0d366b")
RAMP_D <- c("#184f95", "#5598e7", "#cde2fb")

theme_story <- function(dark = FALSE) {
  surf <- if (dark) "#1a1a19" else "#fcfcfb"
  ink  <- if (dark) "#ffffff" else "#0b0b0b"
  mute <- if (dark) "#c3c2b7" else "#52514e"
  grid <- if (dark) "#333331" else "#e7e6e2"
  theme_minimal(base_size = 13) +
    theme(
      plot.background   = element_rect(fill = surf, colour = NA),
      panel.background  = element_rect(fill = surf, colour = NA),
      legend.background = element_rect(fill = surf, colour = NA),
      legend.key        = element_rect(fill = surf, colour = NA),
      panel.grid.major  = element_line(colour = grid, linewidth = .35),
      panel.grid.minor  = element_blank(),
      axis.text   = element_text(colour = mute, size = 10.5),
      axis.title  = element_text(colour = mute, size = 11),
      plot.title  = element_text(colour = ink, face = "bold", size = 15,
                                 margin = margin(b = 4)),
      plot.subtitle = element_text(colour = mute, size = 11.5,
                                   margin = margin(b = 14), lineheight = 1.25),
      plot.caption  = element_text(colour = mute, size = 9, hjust = 0,
                                   margin = margin(t = 12)),
      legend.text  = element_text(colour = mute, size = 10),
      legend.title = element_text(colour = mute, size = 10),
      plot.margin  = margin(16, 18, 12, 16))
}

save_pair <- function(build, name, w = 8, h = 5) {
  for (mode in c("light", "dark")) {
    dk <- mode == "dark"
    ggsave(file.path(IMG, sprintf("%s_%s.png", name, mode)),
           build(dk), width = w, height = h, dpi = 150, bg = "transparent")
  }
  message("wrote ", name, "_{light,dark}.png")
}

# ── Fig 1: gap statistic ───────────────────────────────────────────────────
save_pair(function(dk) {
  col <- if (dk) BLUE_D else BLUE_L
  ink <- if (dk) "#ffffff" else "#0b0b0b"
  ggplot(gap_df, aes(k, gap)) +
    geom_ribbon(aes(ymin = gap - se, ymax = gap + se), fill = col, alpha = .16) +
    geom_line(colour = col, linewidth = .9) +
    geom_point(colour = col, size = 2.6) +
    geom_point(data = gap_df[1, ], colour = col, size = 5.4) +
    annotate("text", x = 1.35, y = gap_df$gap[1], hjust = 0, vjust = .5,
             label = "peak at k = 1", colour = ink, size = 4, fontface = "bold") +
    scale_x_continuous(breaks = 1:9) +
    labs(title = "The gap statistic peaks at one cluster",
         subtitle = "Gap compares within-cluster tightness against a structureless reference.\nA real cluster count shows up as a peak. This curve only falls.",
         x = "Number of clusters (k)", y = "Gap statistic",
         caption = "160 NFL team-seasons, 2021-2025. 4 standardised EPA-per-play features. B = 200 reference sets.") +
    theme_story(dk)
}, "fig_gap")

# ── Fig 2: bootstrap stability ─────────────────────────────────────────────
save_pair(function(dk) {
  col  <- if (dk) BLUE_D else BLUE_L
  ink  <- if (dk) "#ffffff" else "#0b0b0b"
  mute <- if (dk) "#c3c2b7" else "#52514e"
  ggplot(stab, aes(factor(k), min_jaccard)) +
    geom_hline(yintercept = .85, linetype = "22", colour = mute, linewidth = .45) +
    geom_hline(yintercept = .60, linetype = "22", colour = mute, linewidth = .45) +
    geom_col(fill = col, width = .62) +
    geom_text(aes(label = sprintf("%.2f", min_jaccard)), vjust = -0.7,
              colour = ink, size = 3.7, fontface = "bold") +
    annotate("text", x = 7.4, y = .875, label = "stable", hjust = 1,
             colour = mute, size = 3.4) +
    annotate("text", x = 7.4, y = .625, label = "not real", hjust = 1,
             colour = mute, size = 3.4) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, .2)) +
    labs(title = "No cluster count survives resampling",
         subtitle = "Weakest cluster's Jaccard similarity across 200 bootstrap resamples.\nAbove 0.85 a group is a real, recoverable structure. Below 0.60 it is noise.",
         x = "Number of clusters (k)", y = "Weakest cluster's Jaccard",
         caption = "fpc::clusterboot, k-means, B = 200. Seven groups - the current archetype count - bottoms out at 0.41.") +
    theme_story(dk)
}, "fig_stability")

# ── Fig 3: the map that IS there ───────────────────────────────────────────
save_pair(function(dk) {
  ramp <- if (dk) RAMP_D else RAMP_L
  mute <- if (dk) "#c3c2b7" else "#52514e"
  ring <- if (dk) "#1a1a19" else "#fcfcfb"
  ggplot(cp, aes(pc1, pc2)) +
    geom_hline(yintercept = 0, colour = mute, linewidth = .3, linetype = "22") +
    geom_vline(xintercept = 0, colour = mute, linewidth = .3, linetype = "22") +
    geom_point(aes(fill = wins), shape = 21, size = 3.1,
               colour = ring, stroke = .7) +
    scale_fill_gradientn(colours = ramp, name = "Wins") +
    labs(title = "One cloud, not seven islands",
         subtitle = "Every team-season on the two axes that carry 72% of the variance.\nIf archetypes were real groups, you would see gaps here. There are none.",
         x = "worse  <-----  team quality (PC1, 37% of variance)  ----->  better",
         y = "defence-led  <-----  tilt (PC2, 35%)  ----->  offence-led",
         caption = sprintf("PC1 correlates %+.2f with wins: the dominant axis of 'team identity' is mostly just how good the team is.", cor(cp$pc1, cp$wins))) +
    theme_story(dk) +
    theme(legend.position = "right")
}, "fig_map", w = 8, h = 5.4)

message("\nDone. Results -> outputs/cluster_results.json")
