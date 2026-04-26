# ============================================================
# West Bengal 2026 MrP — Pure Sequential Binomial (v2)
#
# Key changes from v1:
#   1. p_will_vote (probability) replaces will_vote (boolean)
#      Hard turnout label drawn via Bernoulli(p_will_vote)
#   2. Hard party vote label drawn from (p_AITC, p_NDA, p_OTHER)
#      for every respondent regardless of turnout draw —
#      stage models subset correctly downstream
#   3. n_individuals in survey is IGNORED (artefact of data gen)
#      Population weights come solely from stratification frame
#   4. Everything else follows professor spec:
#      no offset, 2021 as fixed-effect covariate only,
#      sequential binomials, RE shrinkage for unsurveyed seats
# ============================================================

rm(list = ls())
options(scipen = 9999)

library(data.table)
library(lme4)
library(MASS)

n_draws <- 500
choices <- c("AITC", "NDA", "OTHER")

dir.create("fits",        showWarnings = FALSE, recursive = TRUE)
dir.create("projections", showWarnings = FALSE, recursive = TRUE)

# ============================================================
# 1. Load survey
# ============================================================
survey <- fread("merged_sample_v3.csv")

# Rename party probability columns
setnames(survey,
         old = c("p_aitc", "p_nda", "p_lf", "p_inc", "p_nota"),
         new = c("p_AITC", "p_NDA", "p_LF",  "p_INC", "p_NOTA"))

survey[age_band == "60", age_band := "60+"]
survey[, constituency_name := trimws(constituency_name)]

# Drop columns we do not use:
#   will_vote     — superseded by p_will_vote
#   n_individuals — population counts from survey are NOT used;
#                   stratification frame is the sole pop source
survey[, will_vote     := NULL]
survey[, n_individuals := NULL]

# Require p_will_vote and party probs to be non-missing
survey <- survey[!is.na(p_will_vote)]
survey <- survey[!is.na(p_AITC) & !is.na(p_NDA)]

# ── Draw 1: Hard turnout label ────────────────────────────────
# Bernoulli draw from p_will_vote for every respondent
set.seed(2026)
survey[, voted_draw := rbinom(.N, 1L, p_will_vote)]

cat("Survey rows:", nrow(survey), "\n")
cat("Drawn voters:", sum(survey$voted_draw),
    sprintf("(%.1f%% of sample)\n", mean(survey$voted_draw) * 100))

# ── Draw 2: Hard party vote label ────────────────────────────
# Collapse minor parties into OTHER
survey[, p_OTHER := rowSums(cbind(p_LF, p_INC, p_NOTA), na.rm = TRUE)]

# Normalise to sum to 1 per row
prob_cols <- c("p_AITC", "p_NDA", "p_OTHER")
rs <- rowSums(survey[, ..prob_cols], na.rm = TRUE)
rs[rs == 0] <- 1
for (col in prob_cols)
  survey[, (col) := get(col) / rs]

# Draw one party label per respondent from the 3-way distribution
survey[, vote_draw := {
  result <- rep(NA_character_, .N)
  for (i in seq_len(.N)) {
    p <- c(p_AITC[i], p_NDA[i], p_OTHER[i])
    p[is.na(p)] <- 0
    p <- pmax(p, 0)
    if (sum(p) > 0)
      result[i] <- sample(choices, 1, prob = p / sum(p))
  }
  result
}]

cat("Vote draw distribution:\n")
print(table(survey$vote_draw, useNA = "ifany"))

# ── Binary outcome columns for sequential modelling ──────────
# Stage 0: voted or not (all rows)
survey[, voted := voted_draw]

# Stage 1: voted AITC | voted (voters only)
# NA for non-voters — correctly excluded when we subset
survey[, voted_AITC := fifelse(voted_draw == 1L & vote_draw == "AITC", 1L,
                               fifelse(voted_draw == 1L & !is.na(vote_draw),   0L,
                                       NA_integer_))]

# Stage 2: voted NDA | voted & not AITC (non-AITC voters only)
# NA for non-voters and AITC voters — correctly excluded when we subset
survey[, voted_NDA_cond := fifelse(voted_draw == 1L & vote_draw == "NDA",
                                   1L,
                                   fifelse(voted_draw == 1L & vote_draw == "OTHER",
                                           0L,
                                           NA_integer_))]

# ============================================================
# 2. Stratification frame — sole source of population counts
# ============================================================
sf <- fread("stratification_frame_cleaned.csv")
sf[age_band == "60", age_band := "60+"]
sf[, N_star := n_individuals]          # population per stratum cell
sf[, constituency_name := trimws(constituency_name)]
sf <- sf[!is.na(area_type) & area_type != ""]

cat("\nSF:", nrow(sf), "rows |",
    sf[, uniqueN(constituency_name)], "constituencies\n")
cat("Total population (N_star):", sf[, sum(N_star)], "\n")

# ============================================================
# 3. 2021 results — fixed-effect covariates only, no offset
# ============================================================
r21 <- fread("wb_2021_results.csv")
if ("constituency" %in% names(r21) && !"constituency_name" %in% names(r21))
  setnames(r21, "constituency", "constituency_name")
r21[, constituency_name := trimws(constituency_name)]

r21[, AITC_2021  := aitc_pct  / 100]
r21[, NDA_2021   := nda_pct   / 100]
r21[, OTHER_2021 := other_pct / 100]

# Turnout proxy (normalised to [0,1])
r21[, total_votes  := aitc_votes + nda_votes + nota_votes + other_votes]
r21[, turnout_2021 := total_votes / max(total_votes, na.rm = TRUE)]

# Conditional NDA share among non-AITC (for Stage 2)
r21[, NDA_cond_2021 := NDA_2021 / (NDA_2021 + OTHER_2021)]
r21[is.nan(NDA_cond_2021), NDA_cond_2021 := 0.5]

# Z-score all covariates
r21[, AITC_2021_z     := as.numeric(scale(AITC_2021))]
r21[, NDA_cond_2021_z := as.numeric(scale(NDA_cond_2021))]
r21[, turnout_2021_z  := as.numeric(scale(turnout_2021))]

r21_cov <- r21[, .(constituency_name,
                   AITC_2021, NDA_2021, OTHER_2021,
                   NDA_cond_2021, turnout_2021,
                   AITC_2021_z, NDA_cond_2021_z, turnout_2021_z)]

# ============================================================
# 4. Constituency diagnostics
# ============================================================
cat("\n=== CONSTITUENCY MATCHING ===\n")
surveyed_const   <- unique(survey$constituency_name)
sf_const         <- unique(sf$constituency_name)
unsurveyed_const <- setdiff(sf_const, surveyed_const)

cat("Surveyed:", length(surveyed_const), "\n")
cat("SF total:", length(sf_const), "\n")
cat("Unsurveyed (RE shrinkage to state mean):",
    length(unsurveyed_const), "\n")

# ============================================================
# 5. Merge 2021 covariates
# ============================================================
survey <- merge(survey, r21_cov, by = "constituency_name", all.x = TRUE)
sf     <- merge(sf,     r21_cov, by = "constituency_name", all.x = TRUE)

# Impute any missing with state median
for (col in names(r21_cov)[-1]) {
  med <- median(r21_cov[[col]], na.rm = TRUE)
  survey[is.na(get(col)), (col) := med]
  sf[is.na(get(col)),     (col) := med]
}

# Drop rows with missing demographic keys
char_cols <- c("constituency_name", "gender", "age_band",
               "area_type", "R", "G", "SG")

survey <- survey[complete.cases(survey[, ..char_cols])]
sf     <- sf[complete.cases(sf[, c(char_cols, "N_star"), with = FALSE])]

cat("After merge — survey:", nrow(survey),
    "| SF:", nrow(sf),
    "| SF seats:", sf[, uniqueN(constituency_name)], "\n")

# ============================================================
# 6. Factor levels — SF authoritative
# ============================================================
for (col in char_cols) {
  lvls <- sort(union(unique(as.character(sf[[col]])),
                     unique(as.character(survey[[col]]))))
  sf[,     (col) := factor(get(col), levels = lvls)]
  survey[, (col) := factor(get(col), levels = lvls)]
}

# ============================================================
# 7. Shared RE formula
# ============================================================
re_terms <- "(1|constituency_name) + (1|gender) + (1|age_band) +
             (1|area_type) + (1|R) + (1|G) + (1|SG)"

# ============================================================
# 8. Helper: fit glmer with fallbacks
# ============================================================
fit_glmer <- function(outcome_col, covariate_cols, data, label) {
  cat(sprintf("  Fitting %s...\n", label))
  
  cov_str   <- paste(covariate_cols, collapse = " + ")
  fml_full  <- as.formula(paste(outcome_col, "~", cov_str, "+", re_terms))
  fml_const <- as.formula(paste(outcome_col, "~", cov_str,
                                "+ (1|constituency_name)"))
  fml_glm   <- as.formula(paste(outcome_col, "~", cov_str))
  
  fit <- tryCatch(
    glmer(fml_full, data = data, family = binomial,
          control = glmerControl(optimizer = "bobyqa",
                                 optCtrl   = list(maxfun = 2e5))),
    error = function(e) {
      cat("  Full glmer failed -> constituency RE only\n")
      tryCatch(
        glmer(fml_const, data = data, family = binomial,
              control = glmerControl(optimizer = "bobyqa",
                                     optCtrl   = list(maxfun = 2e5))),
        error = function(e2) {
          cat("  Reduced glmer failed -> glm\n")
          glm(fml_glm, data = data, family = binomial)
        }
      )
    }
  )
  
  is_mer <- inherits(fit, "glmerMod")
  if (is_mer && isSingular(fit)) {
    cat("  Singular -> glm\n")
    fit    <- glm(fml_glm, data = data, family = binomial)
    is_mer <- FALSE
  }
  
  fe <- if (is_mer) fixef(fit) else coef(fit)
  cat("  Fixed effects:", paste(round(fe, 3), collapse = ", "), "\n")
  
  list(fit = fit, is_mer = is_mer)
}

# ============================================================
# 9. Helper: posterior draws -> predicted probability matrix
#    Returns n_draws x nrow(sf_pred) matrix
# ============================================================
draw_glmer <- function(fit, is_mer, sf_pred, covariate_cols, n_draws) {
  n_sf <- nrow(sf_pred)
  
  fe_mean <- if (is_mer) fixef(fit) else coef(fit)
  fe_vcov <- tryCatch(
    as.matrix(vcov(fit)) + diag(1e-8, length(fe_mean)),
    error = function(e) diag(length(fe_mean)) * 1e-4
  )
  fe_d <- MASS::mvrnorm(n_draws, mu = fe_mean, Sigma = fe_vcov)
  if (!is.matrix(fe_d)) fe_d <- matrix(fe_d, nrow = n_draws)
  
  # Fixed-effect design matrix
  X_list <- list(`(Intercept)` = rep(1, n_sf))
  for (cv in covariate_cols)
    X_list[[cv]] <- as.numeric(sf_pred[[cv]])
  X <- do.call(cbind, X_list)
  colnames(X) <- names(X_list)
  
  common <- intersect(colnames(fe_d), colnames(X))
  lp_fe  <- fe_d[, common, drop = FALSE] %*% t(X[, common, drop = FALSE])
  
  # RE residual noise (adds uncertainty for unsurveyed constituencies)
  lp_re <- matrix(0, nrow = n_draws, ncol = n_sf)
  if (is_mer) {
    vc     <- as.data.frame(VarCorr(fit))
    re_sds <- vc$sdcor[vc$grp != "Residual"]
    re_sds <- re_sds[!is.na(re_sds) & re_sds > 0]
    for (sd_k in re_sds)
      lp_re <- lp_re + matrix(rnorm(n_draws * n_sf, sd = sd_k),
                              nrow = n_draws)
  }
  
  plogis(lp_fe + lp_re)   # n_draws x n_sf
}

# ============================================================
# 10. STAGE 0 — Turnout
#     P(votes) — all survey rows
#     Outcome : voted_draw (Bernoulli draw from p_will_vote)
#     Covariate: turnout_2021_z
# ============================================================
cat("\n========== Stage 0: Turnout ==========\n")

result_s0 <- fit_glmer("voted", "turnout_2021_z", survey, "turnout")
saveRDS(result_s0$fit, "fits/stage0_turnout.rds")

sf_s0 <- copy(sf)
for (col in char_cols)
  sf_s0[[col]] <- factor(as.character(sf_s0[[col]]),
                         levels = levels(survey[[col]]))

p_votes_mat <- draw_glmer(result_s0$fit, result_s0$is_mer,
                          sf_s0, "turnout_2021_z", n_draws)

cat(sprintf("  Mean predicted turnout: %.1f%%\n",
            mean(p_votes_mat) * 100))

# ============================================================
# 11. STAGE 1 — AITC | votes
#     P(AITC | votes) — drawn voters only
#     Outcome : voted_AITC
#     Covariate: AITC_2021_z
# ============================================================
cat("\n========== Stage 1: AITC | votes ==========\n")

survey_voters <- survey[voted_draw == 1L & !is.na(voted_AITC)]
cat("  Stage 1 N:", nrow(survey_voters), "\n")

result_s1 <- fit_glmer("voted_AITC", "AITC_2021_z",
                       survey_voters, "AITC|votes")
saveRDS(result_s1$fit, "fits/stage1_aitc.rds")

sf_s1 <- copy(sf)
for (col in char_cols)
  sf_s1[[col]] <- factor(as.character(sf_s1[[col]]),
                         levels = levels(survey_voters[[col]]))

p_aitc_given_votes <- draw_glmer(result_s1$fit, result_s1$is_mer,
                                 sf_s1, "AITC_2021_z", n_draws)

cat(sprintf("  Mean P(AITC|votes): %.3f\n", mean(p_aitc_given_votes)))

# ============================================================
# 12. STAGE 2 — NDA | votes, not AITC
#     P(NDA | votes, not AITC) — non-AITC drawn voters only
#     Outcome : voted_NDA_cond
#     Covariate: NDA_cond_2021_z
# ============================================================
cat("\n========== Stage 2: NDA | votes, not AITC ==========\n")

survey_nonaitc <- survey[voted_draw == 1L & !is.na(voted_NDA_cond)]
cat("  Stage 2 N:", nrow(survey_nonaitc), "\n")

result_s2 <- fit_glmer("voted_NDA_cond", "NDA_cond_2021_z",
                       survey_nonaitc, "NDA|not AITC")
saveRDS(result_s2$fit, "fits/stage2_nda.rds")

sf_s2 <- copy(sf)
for (col in char_cols)
  sf_s2[[col]] <- factor(as.character(sf_s2[[col]]),
                         levels = levels(survey_nonaitc[[col]]))

p_nda_given_nonaitc <- draw_glmer(result_s2$fit, result_s2$is_mer,
                                  sf_s2, "NDA_cond_2021_z", n_draws)

cat(sprintf("  Mean P(NDA|not AITC): %.3f\n", mean(p_nda_given_nonaitc)))

# ============================================================
# 13. STAGE 3 — Reconstruct unconditional vote shares
#
#   share_AITC  = P(AITC | votes)
#   share_NDA   = (1 - P(AITC|votes)) * P(NDA|not AITC)
#   share_OTHER = (1 - P(AITC|votes)) * (1 - P(NDA|not AITC))
#
#   Multiply by P(votes) to get joint prob for post-stratification
# ============================================================
cat("\n========== Stage 3: Reconstruct ==========\n")

share_aitc  <- p_aitc_given_votes
share_nda   <- (1 - p_aitc_given_votes) * p_nda_given_nonaitc
share_other <- (1 - p_aitc_given_votes) * (1 - p_nda_given_nonaitc)

cat(sprintf("  Sum check: %.6f (should be 1.0)\n",
            mean(share_aitc + share_nda + share_other)))
cat("  Mean vote shares (conditional on voting):\n")
cat(sprintf("    AITC:  %.3f\n", mean(share_aitc)))
cat(sprintf("    NDA:   %.3f\n", mean(share_nda)))
cat(sprintf("    OTHER: %.3f\n", mean(share_other)))

# Weight by turnout probability for post-stratification
prob_ps <- list(
  AITC  = p_votes_mat * share_aitc,
  NDA   = p_votes_mat * share_nda,
  OTHER = p_votes_mat * share_other
)

# ============================================================
# 14. Post-stratification
#     Weights come entirely from sf$N_star (stratification frame)
#     Survey n_individuals is NOT used anywhere here
# ============================================================
cat("\n========== Post-stratifying ==========\n")

sf_pop <- sf[, .(pop = sum(N_star, na.rm = TRUE)), by = constituency_name]

post_strat <- function(prob_mat) {
  proj <- unique(sf[, .(constituency_name)])
  for (d in seq_len(n_draws)) {
    tmp <- data.table(
      constituency_name = sf$constituency_name,
      N_star            = sf$N_star,
      p                 = prob_mat[d, ]
    )
    ct <- tmp[, .(count = sum(N_star * p, na.rm = TRUE)),
              by = constituency_name]
    ct <- merge(ct, sf_pop, by = "constituency_name")
    ct[, share := count / pop]
    nm <- paste0("d", d)
    setnames(ct, "share", nm)
    proj <- merge(proj,
                  ct[, c("constituency_name", nm), with = FALSE],
                  by = "constituency_name", all.x = TRUE)
  }
  proj
}

proj_list <- list()
for (j in choices) {
  cat("Post-stratifying:", j, "\n")
  proj_list[[j]] <- post_strat(prob_ps[[j]])
  fwrite(proj_list[[j]], paste0("projections/", j, "_proj.csv"))
}

proj_turnout <- post_strat(p_votes_mat)

cat("Constituencies in output:",
    proj_list[[1]][, uniqueN(constituency_name)], "\n")

turnout_dcols <- grep("^d", names(proj_turnout), value = TRUE)
turnout_dm    <- as.matrix(proj_turnout[, ..turnout_dcols])
mean_turnout  <- mean(apply(turnout_dm, 1, median, na.rm = TRUE), na.rm = TRUE)
cat(sprintf("Mean predicted turnout: %.1f%%\n", mean_turnout * 100))

# ============================================================
# 15. Summarise vote shares
# ============================================================
cat("\n========== Summarising ==========\n")

summarise_proj <- function(proj, label) {
  dcols <- grep("^d", names(proj), value = TRUE)
  dm    <- as.matrix(proj[, ..dcols])
  for (r in seq_len(nrow(dm))) {
    na_i <- is.na(dm[r, ])
    if (any(na_i) && !all(na_i))
      dm[r, na_i] <- median(dm[r, ], na.rm = TRUE)
  }
  data.table(
    constituency_name = proj$constituency_name,
    choice  = label,
    median  = apply(dm, 1, median,          na.rm = TRUE) * 100,
    ci_low  = apply(dm, 1, quantile, 0.025, na.rm = TRUE) * 100,
    ci_high = apply(dm, 1, quantile, 0.975, na.rm = TRUE) * 100
  )
}

long <- rbindlist(lapply(choices, function(j)
  summarise_proj(proj_list[[j]], j)))

wide <- dcast(long, constituency_name ~ choice,
              value.var = c("median", "ci_low", "ci_high"))

sc <- sapply(choices, function(ch)
  grep(ch, grep("^median", names(wide), value = TRUE), value = TRUE)[1])
names(sc) <- choices

# Normalise medians to 100%
rs <- rowSums(wide[, ..sc], na.rm = TRUE)
for (col in sc)
  wide[, (col) := get(col) / rs * 100]

# Winner
wide[, winner := {
  vals <- as.numeric(unlist(.SD))
  choices[which.max(vals)]
}, by = seq_len(nrow(wide)), .SDcols = sc]

# Swing vs 2021
wide <- merge(wide,
              r21_cov[, .(constituency_name,
                          AITC_2021, NDA_2021, OTHER_2021)],
              by = "constituency_name", all.x = TRUE)
wide[, aitc_swing  := get(sc["AITC"])  - AITC_2021  * 100]
wide[, nda_swing   := get(sc["NDA"])   - NDA_2021   * 100]
wide[, other_swing := get(sc["OTHER"]) - OTHER_2021 * 100]
wide[, margin      := get(sc["AITC"])  - get(sc["NDA"])]
wide[, surveyed    := !(constituency_name %in% unsurveyed_const)]

fwrite(long, "projections/WB_long.csv")
fwrite(wide, "projections/WB_wide.csv")

# ============================================================
# 16. Results
# ============================================================
cat("\n========================================\n")
cat("SEAT COUNTS (", wide[, uniqueN(constituency_name)], "seats)\n")
cat("========================================\n")
print(wide[, .N, by = winner][order(-N)])

cat("\nMean vote shares vs 2021:\n")
for (j in choices)
  cat(sprintf("  %-6s %.1f%%   (2021: %.1f%%   swing: %+.1f pp)\n",
              j,
              mean(wide[[sc[j]]], na.rm = TRUE),
              mean(wide[[paste0(j, "_2021")]] * 100, na.rm = TRUE),
              mean(wide[[paste0(tolower(j), "_swing")]], na.rm = TRUE)))

cat("\nSD of predicted vs 2021:\n")
for (j in choices)
  cat(sprintf("  %-6s predicted SD=%.2f pp   2021 SD=%.2f pp\n",
              j,
              sd(wide[[sc[j]]], na.rm = TRUE),
              sd(wide[[paste0(j, "_2021")]] * 100, na.rm = TRUE)))

cat("\nTop 15 closest seats:\n")
print(wide[order(abs(margin))][
  1:15,
  .(constituency_name, winner, surveyed,
    AITC  = round(get(sc["AITC"]),  1),
    NDA   = round(get(sc["NDA"]),   1),
    OTHER = round(get(sc["OTHER"]), 1),
    margin     = round(margin,     1),
    aitc_swing = round(aitc_swing, 1),
    nda_swing  = round(nda_swing,  1))
])

cat("\nAll NDA seats:\n")
print(wide[winner == "NDA",
           .(constituency_name, surveyed,
             AITC   = round(get(sc["AITC"]), 1),
             NDA    = round(get(sc["NDA"]),  1),
             margin = round(margin, 1),
             aitc_swing = round(aitc_swing, 1),
             nda_swing  = round(nda_swing,  1))
][order(margin)])

cat("\nUnsurveyed seats (RE shrinkage to state mean):\n")
print(wide[surveyed == FALSE,
           .(constituency_name, winner,
             AITC   = round(get(sc["AITC"]),  1),
             NDA    = round(get(sc["NDA"]),   1),
             margin = round(margin, 1))
][order(margin)])

cat("\nDone.\n")
cat("  projections/WB_long.csv\n")
cat("  projections/WB_wide.csv\n")

# ============================================================
# 17. Scatter plots: 2026 predictions vs 2021 results
# ============================================================
library(ggplot2)
dir.create("plots", showWarnings = FALSE)

wide[, AITC_2021  := AITC_2021  * 100]
wide[, NDA_2021   := NDA_2021   * 100]
wide[, OTHER_2021 := OTHER_2021 * 100]

plot_scatter <- function(party, x_col, y_col, colour) {
  dat <- wide[!is.na(get(x_col)) & !is.na(get(y_col))]
  r2  <- cor(dat[[x_col]], dat[[y_col]], use = "complete.obs")^2
  
  ggplot(dat, aes(x = .data[[x_col]], y = .data[[y_col]])) +
    geom_abline(slope = 1, intercept = 0,
                linetype = "dashed", colour = "grey60", linewidth = 0.5) +
    geom_point(aes(colour = surveyed), size = 2.2, alpha = 0.75) +
    scale_colour_manual(values = c("TRUE"  = colour,
                                   "FALSE" = "grey50"),
                        labels = c("TRUE"  = "Surveyed",
                                   "FALSE" = "Unsurveyed (RE shrinkage)"),
                        name   = NULL) +
    geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
                colour = colour, fill = colour, alpha = 0.12,
                linewidth = 0.8) +
    annotate("text", x = Inf, y = -Inf,
             hjust = 1.1, vjust = -0.6,
             label = sprintf("R2 = %.3f", r2),
             size = 3.5, colour = "grey40") +
    labs(
      title    = sprintf("%s: 2026 Prediction vs 2021 Result", party),
      subtitle = "Each point = one constituency  |  dashed line = no change",
      x        = sprintf("%s 2021 vote share (%%)", party),
      y        = sprintf("%s 2026 predicted share (%%)", party)
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title       = element_text(face = "bold"),
      legend.position  = "bottom",
      panel.grid.minor = element_blank()
    )
}

parties <- list(
  list(party = "AITC",  x = "AITC_2021",  y = sc["AITC"],  colour = "#2166ac"),
  list(party = "NDA",   x = "NDA_2021",   y = sc["NDA"],   colour = "#d6604d"),
  list(party = "OTHER", x = "OTHER_2021", y = sc["OTHER"], colour = "#4dac26")
)

for (p in parties) {
  g <- plot_scatter(p$party, p$x, p$y, p$colour)
  ggsave(sprintf("plots/%s_scatter.png", p$party),
         g, width = 7, height = 6, dpi = 150)
  cat(sprintf("  plots/%s_scatter.png saved\n", p$party))
}

cat("Scatter plots saved.\n")
