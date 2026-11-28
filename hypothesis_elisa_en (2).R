# Hypothesis (Elisa)
# Respondents who less often have someone to talk to use AI
# more often for personal and emotional support.
#
# This is an extension of the main model: we check whether the
# availability of a human listener adds anything to emotional AI use.
# The handout only requires a Spearman correlation and a simple lm.
# Expected sign, stated in advance: negative.
# Less support from people -> higher AI frequency.
#
# How to run: set the working directory to the folder with this file and the CSV.
#   setwd("path/to/Rarrss")
#   source("hypothesis_elisa_en.R")

survey_path <- "survey_final_clean.csv"
if (!file.exists(survey_path)) {
  stop("survey_final_clean.csv was not found. Set the working directory to the project root.")
}

raw <- read.csv(survey_path, stringsAsFactors = FALSE, na.strings = c("", "NA"))

# --- Scales from the handout (text in the CSV; numbers are needed in R) ----

# Y: how often AI is used specifically for emotional support
ai_levels <- c(
  "Never" = 0,
  "Rarely" = 1,
  "A few times a month" = 2,
  "A few times a week" = 3,
  "Daily" = 4
)

# X: how often there is a person to talk to (without AI).
# This is the perceived availability of a listener. The survey
# does not measure loneliness.
support_levels <- c(
  "Never" = 0,
  "Rarely" = 1,
  "Most of the time" = 2,
  "Always" = 3
)

df <- data.frame(
  ai_support_freq = unname(ai_levels[raw$Frequency.AI.used.for.emotional.support]),
  support_num = unname(support_levels[raw$Support.talk.fraquency.noAI]),
  age = raw$Age,
  gender = raw$Gender,
  stringsAsFactors = FALSE
)

unknown_ai <- raw$Frequency.AI.used.for.emotional.support[is.na(df$ai_support_freq)]
unknown_support <- raw$Support.talk.fraquency.noAI[is.na(df$support_num)]
if (length(unknown_ai) || length(unknown_support)) {
  stop("A frequency value falls outside the handout scale. Check the coding.")
}

# Support network: 0/1 indicators built from Personal.support.network.
# These columns are not in the CSV; we create them here.
# network_size counts only people: friends, partner, and family.
# A mental health professional and AI itself are left out of network_size.
net <- raw$Personal.support.network
df$talk_friends <- as.integer(grepl("Friends", net, fixed = TRUE))
df$talk_partner <- as.integer(grepl("Partner", net, fixed = TRUE))
df$talk_family <- as.integer(grepl("Family", net, fixed = TRUE))
df$talk_friends[is.na(net)] <- NA
df$talk_partner[is.na(net)] <- NA
df$talk_family[is.na(net)] <- NA
df$network_size <- df$talk_friends + df$talk_partner + df$talk_family

cat("Observations:", nrow(df), "\n")
cat("Missing support_num:", sum(is.na(df$support_num)), "\n")
cat("Missing ai_support_freq:", sum(is.na(df$ai_support_freq)), "\n")
cat("Missing network_size:", sum(is.na(df$network_size)), "\n\n")

# --- Description: what the association looks like before the test ----------

support_labels <- c("0 Never", "1 Rarely", "2 Most of the time", "3 Always")

cat("AI frequency for emotional support by support_num group\n")
print(aggregate(
  ai_support_freq ~ support_num,
  data = df,
  FUN = function(x) c(n = length(x), mean = mean(x), sd = sd(x))
))

cat("\nContingency table (rows = support_num, columns = ai_support_freq)\n")
print(table(support = df$support_num, ai = df$ai_support_freq))

# Chart: mean AI frequency in each "someone to talk to" group.
means <- tapply(df$ai_support_freq, df$support_num, mean)
png("elisa_means.png", width = 800, height = 500)
barplot(
  means,
  names.arg = support_labels,
  ylim = c(0, 4),
  col = "gray70",
  ylab = "Mean frequency of AI use for emotional support (0-4)",
  xlab = "How often there is someone to talk to",
  main = "Elisa's hypothesis: the bars are expected to decline"
)
dev.off()
cat("\nMean chart saved to elisa_means.png\n")

# --- 1. Spearman ------------------------------------------------------------
# Both scales are ordinal (0-3 and 0-4), so Spearman is the primary test.
# alternative = "two.sided": in class the two-sided p-value is the one reported,
# and the direction is read from the sign of rho.
# H0: rho = 0. The hypothesis expects rho < 0.

spearman_support <- cor.test(
  df$support_num,
  df$ai_support_freq,
  method = "spearman",
  exact = FALSE
)
cat("\n=== Spearman: support_num and ai_support_freq ===\n")
print(spearman_support)

spearman_network <- cor.test(
  df$network_size,
  df$ai_support_freq,
  method = "spearman",
  exact = FALSE
)
cat("\n=== Spearman: network_size and ai_support_freq ===\n")
print(spearman_network)

# --- 2. Simple linear regression --------------------------------------------
# The same hypothesis in lm form: the coefficient on support_num.
# Report the coefficient, the p-value, and R^2. Expected sign: negative.

fit_simple <- lm(ai_support_freq ~ support_num, data = df)
cat("\n=== lm: ai_support_freq ~ support_num ===\n")
print(summary(fit_simple))

fit_network <- lm(ai_support_freq ~ network_size, data = df)
cat("\n=== lm: ai_support_freq ~ network_size ===\n")
print(summary(fit_network))

# Both predictors together. They describe different things:
# support_num — how often someone listens,
# network_size — how many types of close people were named (0-3).
fit_both <- lm(ai_support_freq ~ support_num + network_size, data = df)
cat("\n=== lm: ai_support_freq ~ support_num + network_size ===\n")
print(summary(fit_both))

# A short control for age and gender, separate from Rosy's main model.
# It checks whether the coefficient only reflects who is in the sample.
# Gender is Female / Male; the 3 missing values drop out of this model.
fit_controls <- lm(
  ai_support_freq ~ support_num + network_size + age + gender,
  data = df
)
cat("\n=== lm with age and gender ===\n")
print(summary(fit_controls))

# --- How to read the result -------------------------------------------------
# The hypothesis is supported only when the sign is negative and p is small
# (usually < 0.05). A matching sign with a large p is a lack of evidence.
# R^2 is the share of the spread in AI frequency that the model explains.
# Y has many zeros (the answer Never), the scale is short, and the Never
# group is small, so lm here is the working tool for the course.
#
# If MASS::polr is available, the same regression can be repeated as an
# ordinal logistic model, then the sign and the significance compared in one sentence.
if (requireNamespace("MASS", quietly = TRUE)) {
  fit_ord <- MASS::polr(
    ordered(ai_support_freq) ~ support_num,
    data = df,
    Hess = TRUE
  )
  cat("\n=== Ordinal logistic (check that the sign holds) ===\n")
  print(summary(fit_ord))
  cat("coef support_num:", coef(fit_ord)["support_num"], "\n")
}
