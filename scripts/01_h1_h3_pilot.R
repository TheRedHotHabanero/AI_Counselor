# H1–H3 DESCRIPTIVE ANALYSIS AND SHARED MAIN MODEL (M1)
# Owner: Karina. Run from the project root after scripts/00_prepare_data.R.
# The analysis is reproducible but remains preliminary until the team agrees
# eligibility, missing-data treatment, controls and item directions.
# Base R and its recommended stats/graphics packages are sufficient; the
# template was tested with R 4.6.2. No package installation occurs here.
#
# Inspect unmatched barrier responses privately with the preparation maintainer.
# This script uses a clearly stated temporary sample rule, not a final decision.

# RESEARCH QUESTION AND CODING (state these before looking at results)
# H1: People use AI for emotional support more often when professional help is
# harder to afford. Current project label H1 differs from older numbering.
# H2: More fear of judgement is expected to be associated with more frequent AI
# use for emotional support. The two measured concepts are not identical:
# no_judgment is about AI advice; bar_stigma is about professional help.
# H3: Greater familiarity with AI is expected to be associated with more frequent
# use for emotional support. General AI frequency is our available proxy.
# Outcome ai_support_freq: Never = 0 ... Daily = 4 (ordinal frequency).
# afford_num: unable = 0, with difficulty = 1, comfortably = 2. A negative
# coefficient is expected because higher values mean MORE affordable help.
# bar_cost: 1 = cost selected as a barrier, 0 = not selected, NA = unknown;
# a positive coefficient is expected. These are related but distinct measures.
# M1 also includes no_judgment, bar_stigma and genai_freq_num for H2 and H3;
# the planned signs are positive. Verify question wording and score directions.
# M1 has five predictors; no unagreed demographic controls are added here.
# These are associations conditional on the other model terms, not causal effects.

input <- file.path("data", "processed", "analysis.csv")
if (!file.exists(input)) stop("Prepared CSV not found. Run Rscript scripts/00_prepare_data.R from the project root first.")
cat("PRELIMINARY ANALYSIS: eligibility and other rules require team review.\n")

# READ AND VERIFY THE SHARED TABLE
# Do not edit analysis.csv manually or independently recode shared variables.
# read.csv loads the full table but the script never prints raw text or IDs.
d <- read.csv(input, fileEncoding = "UTF-8", stringsAsFactors = FALSE,
              check.names = FALSE, na.strings = c("", "NA"))
required <- c("respondent_id", "eligibility_status", "ai_support_freq", "afford_num",
              "bar_cost", "no_judgment", "bar_stigma", "genai_freq_num",
              "barriers_unmatched", "barriers_contradictory", "use_frequency_inconsistent")
if (!all(required %in% names(d))) stop("Prepared table lacks required columns; rerun/check preparation.")
if (!nrow(d) || anyNA(d$respondent_id) || anyDuplicated(d$respondent_id)) stop("Missing or duplicate respondent IDs.")
if (anyNA(d$eligibility_status)) stop("Missing eligibility status; review preparation.")
model_vars <- c("ai_support_freq", "afford_num", "bar_cost", "no_judgment", "bar_stigma", "genai_freq_num")
allowed <- list(ai_support_freq = 0:4, afford_num = 0:2, bar_cost = 0:1,
                no_judgment = 1:5, bar_stigma = 0:1, genai_freq_num = 0:4)
for (name in model_vars) {
  if (!is.numeric(d[[name]]) || any(!is.na(d[[name]]) & !d[[name]] %in% allowed[[name]])) {
    stop("Unexpected type or code in ", name, "; review the prepared table.")
  }
}

# SAMPLE AND MISSINGNESS
# Preliminary complete-case analysis: all rows are considered, including those
# whose eligibility is undecided; a row enters M1 only if all six model fields
# are present. This is NOT an agreed eligibility or missing-data rule. Do not
# use these results as final until the team decides. The raw/prepared files are
# not modified, and no AI-user-only definition is imposed on this model.
cat("\nSample before model selection:", nrow(d), "\n")
cat("Eligibility statuses (not used to exclude anyone in this analysis):\n")
print(table(d$eligibility_status, useNA = "ifany"))
cat("Missing values by model field (counts can overlap):\n")
print(colSums(is.na(d[model_vars])))
cat("Review flag counts (TRUE / FALSE / missing):\n")
for (name in c("barriers_unmatched", "barriers_contradictory", "use_frequency_inconsistent")) {
  cat(name, ":\n"); print(table(d[[name]], useNA = "always"))
}
complete <- complete.cases(d[model_vars])
analysis <- d[complete, , drop = FALSE]
cat("Complete cases:", nrow(analysis), "; excluded for missing model fields:", sum(!complete), "\n")
if (nrow(analysis) <= length(model_vars)) stop("Not enough complete cases to fit the planned model.")

# DESCRIPTIVE EXPLORATION
# First examine all available data, then the exact complete-case model sample.
# Counts and percentages use the nonmissing denominator shown with each table.
# Category tables may contain small cells: follow the team's privacy rules when
# sharing outputs. They do not show respondent IDs or free-text answers.
describe <- function(z, label) {
  counts <- table(z, useNA = "ifany")
  cat("\n", label, "(nonmissing n =", sum(!is.na(z)), ")\n")
  print(counts)
  if (sum(!is.na(z)) > 0) print(round(100 * prop.table(table(z)), 1))
}
describe(d$ai_support_freq, "AI emotional-support frequency, all rows")
describe(d$afford_num, "Affordability, all rows")
describe(d$bar_cost, "Cost barrier, all rows")
describe(d$no_judgment, "No-judgment AI advice score, all rows")
describe(d$bar_stigma, "Judgment/stigma barrier, all rows")
describe(d$genai_freq_num, "General AI frequency, all rows")
describe(analysis$ai_support_freq, "AI frequency, M1 complete cases")
describe(analysis$afford_num, "Affordability, M1 complete cases")
describe(analysis$bar_cost, "Cost barrier, M1 complete cases")
describe(analysis$no_judgment, "No-judgment AI advice score, M1 complete cases")
describe(analysis$bar_stigma, "Judgment/stigma barrier, M1 complete cases")
describe(analysis$genai_freq_num, "General AI frequency, M1 complete cases")
cat("\nAI frequency by affordability (M1 sample; counts):\n")
afford_table <- table(affordability = factor(analysis$afford_num, levels = 0:2),
                      frequency = factor(analysis$ai_support_freq, levels = 0:4))
print(afford_table)
cat("Percentages within each affordability group:\n")
print(round(100 * prop.table(afford_table, margin = 1), 1))
cat("\nAI frequency by cost barrier (M1 sample; counts):\n")
cost_table <- table(cost_barrier = factor(analysis$bar_cost, levels = 0:1),
                    frequency = factor(analysis$ai_support_freq, levels = 0:4))
print(cost_table)
cat("Percentages within each cost-barrier group:\n")
print(round(100 * prop.table(cost_table, margin = 1), 1))

# H2 and H3: display outcome counts for every predictor group in the same
# complete-case sample as M1. Empty groups remain visible as rows of zeros.
judgment_table <- table(no_judgment = factor(analysis$no_judgment, levels = 1:5),
                        frequency = factor(analysis$ai_support_freq, levels = 0:4))
stigma_table <- table(bar_stigma = factor(analysis$bar_stigma, levels = 0:1),
                      frequency = factor(analysis$ai_support_freq, levels = 0:4))
general_ai_table <- table(general_ai = factor(analysis$genai_freq_num, levels = 0:4),
                          frequency = factor(analysis$ai_support_freq, levels = 0:4))
cat("\nAI frequency by no-judgment score (M1 sample; counts):\n")
print(judgment_table)
cat("\nAI frequency by stigma barrier (M1 sample; counts):\n")
print(stigma_table)
cat("\nAI frequency by general AI frequency (M1 sample; counts):\n")
print(general_ai_table)

# OPTIONAL EXPLORATION: SPEARMAN RANK CORRELATIONS
# As in Elisa's script, Spearman describes the unadjusted association between
# two ordered variables. These tests use the same complete cases as M1 so the
# samples match, but they do not adjust for the other predictors. They do not
# replace M1. exact = FALSE handles tied ranks in these short response scales.
spearman_judgment <- cor.test(analysis$no_judgment, analysis$ai_support_freq,
                             method = "spearman", exact = FALSE)
spearman_general_ai <- cor.test(analysis$genai_freq_num, analysis$ai_support_freq,
                                method = "spearman", exact = FALSE)
cat("\nExploratory Spearman: no-judgment score and AI support frequency\n")
print(spearman_judgment)
cat("\nExploratory Spearman: general AI and AI support frequency\n")
print(spearman_general_ai)

# FIT THE ONE SHARED M1 MODEL
# lm() approximates an ordinal 0–4 outcome using linear regression. Check its
# limitations and diagnostics; an ordinal model is an optional sensitivity
# analysis after the team agrees its specification. No separate H1-only model
# replaces the shared M1. The numerical affordability code assumes equal steps.
m1 <- lm(ai_support_freq ~ afford_num + bar_cost + no_judgment + bar_stigma + genai_freq_num,
         data = analysis, na.action = na.fail)
if (m1$rank < length(coef(m1))) stop("Model is rank deficient; review predictor variation before interpretation.")
cat("\nM1 model summary (PROVISIONAL):\n")
print(summary(m1))
cat("\nCoefficients and 95% confidence intervals (PROVISIONAL):\n")
print(cbind(estimate = coef(m1), confint(m1)))
cat("\nModel N:", nobs(m1), "; model R-squared:", summary(m1)$r.squared, "\n")
# Check how often a linear prediction falls outside the outcome's possible 0–4
# range. This can happen because lm() is not designed for bounded ordinal data.
cat("Predictions outside the observed 0–4 scale:", sum(fitted(m1) < 0 | fitted(m1) > 4), "\n")
cat("Largest Cook's distance:", max(cooks.distance(m1)), "\n")

# DIAGNOSTICS AND CHARTS
# Save plots locally under ignored outputs/. Existing files of the same name are
# overwritten. Review residual patterns, Q-Q deviations and influential cases;
# no plot or single test automatically proves the model assumptions are met.
# Diagnostic labels can identify row positions: treat outputs as sensitive.
out_dir <- file.path("outputs", "h1_h3_pilot")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
# Save model coefficients, confidence intervals and p-values for review.
# R-squared is a model statistic, so it is printed above, not attached to terms.
coefficient_table <- data.frame(term = names(coef(m1)),
                                estimate = unname(coef(m1)),
                                lower_95 = confint(m1)[, 1],
                                upper_95 = confint(m1)[, 2],
                                p_value = coef(summary(m1))[, 4],
                                row.names = NULL)
write.csv(coefficient_table, file.path(out_dir, "coefficients.csv"), row.names = FALSE)

# Save statistics for the whole model separately from predictor coefficients.
# R-squared and adjusted R-squared describe M1, not any one hypothesis.
model_summary <- summary(m1)
model_table <- data.frame(total_rows = nrow(d), model_n = nobs(m1),
                          missing_model_rows = sum(!complete),
                          r_squared = model_summary$r.squared,
                          adjusted_r_squared = model_summary$adj.r.squared,
                          residual_standard_error = model_summary$sigma,
                          predictions_outside_scale = sum(fitted(m1) < 0 | fitted(m1) > 4))
write.csv(model_table, file.path(out_dir, "model_summary.csv"), row.names = FALSE)

# Save the exploratory Spearman results separately from adjusted M1 estimates.
spearman_table <- data.frame(predictor = c("no_judgment", "genai_freq_num"),
                             n = nrow(analysis),
                             rho = c(unname(spearman_judgment$estimate),
                                     unname(spearman_general_ai$estimate)),
                             p_value = c(spearman_judgment$p.value, spearman_general_ai$p.value))
write.csv(spearman_table, file.path(out_dir, "spearman_exploratory.csv"), row.names = FALSE)

# Write a short, numerical results draft. These are associations adjusted for
# the other M1 predictors; they do not establish cause, and a nonsignificant
# result is not proof that no association exists. Check the wording of items
# and the diagnostics before using this draft in the project report.
report_lines <- c(
  "PRELIMINARY M1 RESULTS: H1–H3",
  "Linear regression approximates an ordinal 0–4 outcome; results are associations, not causes.",
  "All rows were considered; eligibility remains undecided. Complete cases were used.",
  sprintf("Input N = %d; model N = %d; excluded for missing model fields = %d.",
          nrow(d), nobs(m1), sum(!complete)),
  sprintf("Model R-squared = %.3f; adjusted R-squared = %.3f.",
          model_summary$r.squared, model_summary$adj.r.squared),
  "Expected signs: affordability negative; cost, no judgment, stigma and general AI frequency positive.",
  "Each coefficient describes a one-unit difference with the other M1 predictors held constant."
)
# Add the estimate, confidence interval and p-value for each hypothesis term.
for (i in seq_len(nrow(coefficient_table))) {
  if (coefficient_table$term[i] != "(Intercept)") {
    report_lines <- c(report_lines,
                      sprintf("%s: estimate = %.3f, 95%% CI [%.3f, %.3f], p = %.4g.",
                              coefficient_table$term[i], coefficient_table$estimate[i],
                              coefficient_table$lower_95[i], coefficient_table$upper_95[i],
                              coefficient_table$p_value[i]))
  }
}
report_lines <- c(report_lines,
                  "H1 uses affordability and cost; H2 uses AI no-judgment score and professional-help stigma; H3 uses general AI frequency as a proxy.",
                  "Review item direction, missing data, eligibility and diagnostics before drawing conclusions.")
writeLines(report_lines, file.path(out_dir, "results_summary.txt"), useBytes = TRUE)
# Stacked bar charts show the distribution of frequency within each group.
# These are descriptive associations, not model-adjusted effects. Counts in the
# console identify the denominators; small cells require care before sharing.
pdf(file.path(out_dir, "descriptive.pdf"))
barplot(table(factor(analysis$ai_support_freq, levels = 0:4)),
        xlab = "Emotional-support AI frequency (0-4)", ylab = "Respondents",
        main = "M1 complete-case sample (preliminary)")
colors <- c("#D9E6F2", "#AFCBE3", "#7EADD0", "#4C89B7", "#245A87")
afford_percent <- 100 * prop.table(afford_table, margin = 1)
barplot(t(afford_percent), col = colors, ylim = c(0, 100),
        names.arg = c("Cannot afford", "With difficulty", "Comfortably"),
        ylab = "Percent within affordability group",
        main = "AI frequency by affordability")
legend("topright", legend = c("0 Never", "1 Rarely", "2 Monthly", "3 Weekly", "4 Daily"),
       fill = colors, cex = 0.7)
cost_percent <- 100 * prop.table(cost_table, margin = 1)
barplot(t(cost_percent), col = colors, ylim = c(0, 100),
        names.arg = c("Not selected", "Cost selected"),
        ylab = "Percent within cost-barrier group",
        main = "AI frequency by cost barrier")
legend("topright", legend = c("0 Never", "1 Rarely", "2 Monthly", "3 Weekly", "4 Daily"),
       fill = colors, cex = 0.7)

# These count charts show H2 and H3 without suggesting that group means alone
# are the adjusted regression results. Each bar uses the M1 complete cases.
barplot(t(judgment_table), col = colors, beside = FALSE,
        xlab = "No-judgment AI advice score (1-5)", ylab = "Respondents",
        main = "AI support frequency by no-judgment score")
legend("topright", legend = c("0 Never", "1 Rarely", "2 Monthly", "3 Weekly", "4 Daily"),
       fill = colors, cex = 0.7)
barplot(t(stigma_table), col = colors, beside = FALSE,
        names.arg = c("Not selected", "Stigma selected"), ylab = "Respondents",
        main = "AI support frequency by stigma barrier")
legend("topright", legend = c("0 Never", "1 Rarely", "2 Monthly", "3 Weekly", "4 Daily"),
       fill = colors, cex = 0.7)
barplot(t(general_ai_table), col = colors, beside = FALSE,
        xlab = "General AI frequency (0 Never to 4 Daily)", ylab = "Respondents",
        main = "AI support frequency by general AI frequency")
legend("topright", legend = c("0 Never", "1 Rarely", "2 Monthly", "3 Weekly", "4 Daily"),
       fill = colors, cex = 0.7)
dev.off()

# A coefficient chart shows estimates and 95% confidence intervals together.
# Exclude the intercept: the five displayed terms correspond to H1–H3.
terms <- coefficient_table[coefficient_table$term != "(Intercept)", ]
pdf(file.path(out_dir, "model_coefficients.pdf"), width = 9, height = 6)
par(mar = c(5, 11, 4, 2))
# x limits include zero even when every interval is on one side of zero.
plot(terms$estimate, seq_len(nrow(terms)), xlim = range(c(0, terms$lower_95, terms$upper_95)),
     yaxt = "n", xlab = "Adjusted coefficient and 95% confidence interval",
     ylab = "", main = "M1: H1-H3 regression coefficients", pch = 19)
axis(2, at = seq_len(nrow(terms)), labels = terms$term, las = 1)
abline(v = 0, lty = 2, col = "gray50")
segments(terms$lower_95, seq_len(nrow(terms)), terms$upper_95, seq_len(nrow(terms)))
dev.off()

# Base R's standard plots help inspect residual patterns, normality, changes in
# spread and Cook's distance. They are aids, not automatic pass/fail tests.
pdf(file.path(out_dir, "model_diagnostics.pdf"))
plot(m1, which = 1:4, id.n = 0)
dev.off()
cat("\nSaved preliminary charts, tables and results draft in", out_dir, "\n")
