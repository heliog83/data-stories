#!/usr/bin/env Rscript
# ═══════════════════════════════════════════════════════════════════════════
# check_game_post.R
#
# The gate before publishing a postgame post drafted by new_game_post.R.
#
#   Rscript scripts/check_game_post.R <slug> [--posts=<dir>]
#
# FAILS (exit 1) when:
#   - a TODO is left anywhere in index.qmd
#   - the banner heading does not match the title
#   - the embedded report is not the file facts.json recorded (hand-edited)
#   - the prose quotes a number that appears neither in facts.json nor in the
#     embedded report (decimals, percentages, clock times and "N of M" /
#     "N to M" pairs are matched exactly; spelled-out numbers are converted)
#   - the prose uses betting language (the storytelling track never does)
# WARNS when:
#   - a number is hedged ("nearly", "about", "almost"...): the week-1 post said
#     "nearly twelve minutes" for 12:34, which was wrong in the direction
#   - the game has no independent expectations in R/47 (unverified)
# ═══════════════════════════════════════════════════════════════════════════

args <- commandArgs(trailingOnly = TRUE)
SLUG <- args[!startsWith(args, "--")][1]
BLOG_DIR <- normalizePath(file.path(dirname(sub("^--file=", "", grep("^--file=",
              commandArgs(FALSE), value = TRUE)[1])), ".."), mustWork = FALSE)
if (is.na(BLOG_DIR) || !dir.exists(BLOG_DIR)) BLOG_DIR <- getwd()
posts_opt <- grep("^--posts=", args, value = TRUE)
POSTS <- if (length(posts_opt)) sub("^--posts=", "", posts_opt[1]) else file.path(BLOG_DIR, "posts")
if (is.na(SLUG)) stop("usage: Rscript scripts/check_game_post.R <slug>   e.g. 2026-wk-2-buf-mia", call. = FALSE)

dir    <- file.path(POSTS, SLUG)
qmd    <- readLines(file.path(dir, "index.qmd"), warn = FALSE)
facts  <- jsonlite::read_json(file.path(dir, "facts.json"))
report <- paste(readLines(file.path(dir, "game-charts.html"), warn = FALSE), collapse = "\n")

fails <- character(0); warns <- character(0)
fail <- function(...) fails <<- c(fails, paste0(...))
warn <- function(...) warns <<- c(warns, paste0(...))

# ── structure ───────────────────────────────────────────────────────────────
todo <- grep("TODO", qmd)
if (length(todo)) fail("TODO left on line(s) ", paste(todo, collapse = ", "))

title  <- sub('^title: *"(.*)" *$', "\\1", grep("^title:", qmd, value = TRUE)[1])
banner <- sub("^ *<h2>(.*)</h2> *$", "\\1", grep("<h2>", qmd, value = TRUE)[1])
if (!identical(trimws(title), trimws(banner))) fail("banner heading \"", banner, "\" does not match title \"", title, "\"")

md5 <- unname(tools::md5sum(file.path(dir, "game-charts.html")))
if (!identical(md5, facts$report_md5)) fail("game-charts.html md5 ", md5, " is not the report facts.json recorded (", facts$report_md5, "); re-run new_game_post.R instead of editing it")
if (!isTRUE(facts$verified)) warn("no independent expectations for ", facts$game_id, ": its numbers are unverified")

# ── prose: everything a reader sees that a person wrote ─────────────────────
drop_block <- function(txt, name) {
  s <- grep(sprintf("^<!-- %s:start -->$", name), txt); e <- grep(sprintf("^<!-- %s:end -->$", name), txt)
  if (length(s) == 1 && length(e) == 1) txt[-(s:e)] else txt
}
prose <- qmd
for (b in c("strip", "report", "facts", "not", "provenance")) prose <- drop_block(prose, b)
prose <- prose[!grepl("^(date|author|categories|format|  html|    css|    code-tools):|^---$", prose)]
prose <- paste(prose, collapse = "\n")
prose <- gsub("(?s)```\\{=html\\}.*?```", "", prose, perl = TRUE)
prose <- paste(prose, title)  # the banner block was dropped; its heading is the title

# spelled-out numbers become digits so "four of five" is checked like "4 of 5"
words <- c(zero = 0, one = 1, two = 2, three = 3, four = 4, five = 5, six = 6, seven = 7, eight = 8, nine = 9,
           ten = 10, eleven = 11, twelve = 12, thirteen = 13, fourteen = 14, fifteen = 15, sixteen = 16,
           seventeen = 17, eighteen = 18, nineteen = 19, twenty = 20, thirty = 30, forty = 40, fifty = 50)
num_prose <- prose
for (w in names(words)) num_prose <- gsub(sprintf("(?i)\\b%s\\b", w), words[[w]], num_prose, perl = TRUE)
num_prose <- gsub("[–—]", "-", num_prose)

# allowed numbers: facts.json and the report's visible text and tooltips
flat <- function(x) if (is.list(x)) unlist(lapply(x, flat)) else as.character(x)
vis <- gsub("(?s)<(script|style)[^>]*>.*?</\\1>", " ", report, perl = TRUE)
tips <- paste(regmatches(vis, gregexpr('data-tip="[^"]*"', vis))[[1]], collapse = " ")
vis <- paste(gsub("<[^>]+>", " ", vis), gsub("<[^>]+>", " ", gsub("&lt;", "<", gsub("&gt;", ">", tips))))
allowed <- gsub("[–—]", "-", paste(c(flat(facts$facts), flat(facts$final_score), facts$season, facts$week, vis), collapse = " "))
allowed <- gsub("&minus;|−", "-", allowed)

tok_re <- "\\b\\d+ (?:of|to) \\d+\\b|\\b\\d+-\\d+\\b|\\b\\d{1,2}:\\d{2}\\b|[+-]?\\d+\\.\\d+%?|\\b\\d+%|\\b\\d+\\b"
toks <- unique(regmatches(num_prose, gregexpr(tok_re, num_prose, perl = TRUE))[[1]])
has <- function(tok) {
  if (grepl(" (of|to) ", tok)) return(grepl(tok, allowed, fixed = TRUE))
  grepl(sprintf("(?<![0-9.])%s(?![0-9])", gsub("([.+%])", "\\\\\\1", tok)), allowed, perl = TRUE)
}
for (tok in toks) if (!has(tok)) fail("number \"", tok, "\" is not in facts.json or the report")

hedge <- regmatches(num_prose, gregexpr("(?i)\\b(nearly|almost|about|roughly|around|more than|less than|over|under|close to)\\s+\\d+(?:[.:]\\d+)?", num_prose, perl = TRUE))[[1]]
for (h in unique(hedge)) warn("hedged number \"", h, "\": check it against the exact value")

betting <- regmatches(prose, gregexpr("(?i)\\b(odds|spread|moneyline|over/under|point total|bet|bets|betting|bettor|wager|sportsbook|picks?(?!-six)|parlay|CLV|closing line)\\b", prose, perl = TRUE))[[1]]
for (b in unique(betting)) fail("betting language \"", b, "\" in the prose")

# ── report ──────────────────────────────────────────────────────────────────
cat(sprintf("%s  (%s, %s)\n", SLUG, facts$game_id, if (isTRUE(facts$verified)) sprintf("%d checks", facts$checks) else "unverified"))
cat(sprintf("  numbers checked: %d\n", length(toks)))
if (length(warns)) cat(paste0("  WARN  ", warns, "\n"), sep = "")
if (length(fails)) {
  cat(paste0("  FAIL  ", fails, "\n"), sep = "")
  quit(status = 1)
}
cat("  PASS\n")
