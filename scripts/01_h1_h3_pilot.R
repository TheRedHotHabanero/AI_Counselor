# H1 AFFORDABILITY ANALYSIS AND SHARED H1–H3 MODEL (M1)
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
describe(analysis$ai_support_freq, "AI frequency, M1 complete cases")
describe(analysis$afford_num, "Affordability, M1 complete cases")
describe(analysis$bar_cost, "Cost barrier, M1 complete cases")
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
dev.off()
pdf(file.path(out_dir, "model_diagnostics.pdf"))
plot(m1, which = 1:4, id.n = 0)
dev.off()
cat("\nSaved preliminary charts and coefficient table in", out_dir, "\n")
