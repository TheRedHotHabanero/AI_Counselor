library(tidyverse)
library(broom)

setwd("C:\\Users\\xavie\\OneDrive\\Desktop")

df<-read.csv("survey_final_clean.csv")

## ==============================================================================
# SAMPLE DESCRIPTION (Age, Gender, Occupation, Nationality)
# ==============================================================================
library(tidyverse)

# --- 1. DESCRIPTIVE STATISTICS ---
# Age (continuous variable)
desc_age <- df |>
  summarise(
    variabile = "Age",
    n = sum(!is.na(Age)),
    media = round(mean(Age, na.rm = TRUE), 2),
    sd = round(sd(Age, na.rm = TRUE), 2),
    mediana = median(Age, na.rm = TRUE),
    iqr = IQR(Age, na.rm = TRUE),
    min = min(Age, na.rm = TRUE),
    max = max(Age, na.rm = TRUE)
  )
print(desc_age)

# Categorical variables: counts and percentages
tab_gender <- df |>
  count(Gender, name = "n") |>
  mutate(pct = round(100 * n / sum(n), 1))

tab_occupation <- df |>
  count(Main.occupation, name = "n") |>
  mutate(pct = round(100 * n / sum(n), 1))

tab_nationality <- df |>
  count(Nationality, sort = TRUE, name = "n") |>
  mutate(pct = round(100 * n / sum(n), 1))

print(tab_gender)
print(tab_occupation)
print(tab_nationality)

# --- 2. DESCRIPTIVE PLOTS (ggplot2) ---
# Plot 1: Age distribution
ggplot(df, aes(x = Age)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  labs(title = "Age distribution of the sample", x = "Age", y = "Count") +
  theme_minimal()

# Plot 2: Gender distribution
ggplot(df, aes(x = fct_infreq(as.character(Gender)))) +
  geom_bar(fill = "darkseagreen") +
  labs(title = "Distribution by gender", x = "Gender", y = "Count") +
  theme_minimal()

# Plot 3: Main occupation distribution
ggplot(df, aes(x = fct_infreq(as.character(Main.occupation)))) +
  geom_bar(fill = "coral") +
  labs(title = "Main occupation", x = "Occupation", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

# Plot 4: Nationality distribution
df |>
  count(Nationality, sort = TRUE) |>
  ggplot(aes(x = reorder(Nationality, n), y = n)) +
  geom_col(fill = "slateblue") +
  coord_flip() +
  labs(title = "Distribution by nationality", x = "Nationality", y = "Count") +
  theme_minimal()


# ==============================================================================
#  REMAINING DESCRIPTIVE ANALYSES + PLOTS (ggplot2)
# ==============================================================================
library(tidyverse)

# ------------------------------------------------------------------------------
# A. SOCIOECONOMIC CONTEXT AND SUPPORT NETWORK (NON-AI)
# ------------------------------------------------------------------------------
# 1. Summary tables
tab_parents <- df |>
  count(Parents.education, name = "n") |>
  mutate(pct = round(100 * n / sum(n), 1))

desc_ses <- df |>
  summarise(
    variable = "Personal.perception.of.social.status",
    n = sum(!is.na(Personal.perception.of.social.status)),
    mean = round(mean(Personal.perception.of.social.status, na.rm = TRUE), 2),
    sd = round(sd(Personal.perception.of.social.status, na.rm = TRUE), 2),
    median = median(Personal.perception.of.social.status, na.rm = TRUE),
    iqr = IQR(Personal.perception.of.social.status, na.rm = TRUE),
    min = min(Personal.perception.of.social.status, na.rm = TRUE),
    max = max(Personal.perception.of.social.status, na.rm = TRUE)
  )

print(desc_ses)

tab_talk_noai <- df |>
  count(Support.talk.fraquency.noAI, name = "n") |>
  mutate(pct = round(100 * n / sum(n), 1))

# Count respondents with valid (non-empty) multiple-choice support network entries
valid_net <- sum(!is.na(df$Personal.support.network) & str_squish(df$Personal.support.network) != "")
tab_network <- df |>
  filter(!is.na(Personal.support.network), str_squish(Personal.support.network) != "") |>
  separate_longer_delim(Personal.support.network, delim = ";") |>
  mutate(option = str_squish(Personal.support.network)) |>
  filter(option != "") |>
  count(option, sort = TRUE, name = "n") |>
  mutate(pct_valid_respondents = round(100 * n / valid_net, 1))

print(tab_parents)
print(desc_ses)
print(tab_talk_noai)
print(tab_network)

# 2. Plots
p_parents <- df |>
  filter(!is.na(Parents.education)) |>
  ggplot(aes(x = fct_infreq(Parents.education))) +
  geom_bar(fill = "steelblue") +
  coord_flip() +
  labs(title = "Parents' Education Level", x = NULL, y = "Count") +
  theme_minimal()

print(p_parents)

p_ses <- df |>
  filter(!is.na(Personal.perception.of.social.status)) |>
  ggplot(aes(x = Personal.perception.of.social.status)) +
  geom_bar(fill = "cadetblue") +
  scale_x_continuous(breaks = 1:10) +
  labs(title = "Perceived Socioeconomic Status (1-10)", x = "Ladder Score", y = "Count") +
  theme_minimal()

print(p_ses)

p_talk_noai <- df |>
  filter(!is.na(Support.talk.fraquency.noAI)) |>
  mutate(Support.talk.fraquency.noAI = factor(
    Support.talk.fraquency.noAI,
    levels = c("Never", "Rarely", "Most of the time", "Always")
  )) |>
  ggplot(aes(x = Support.talk.fraquency.noAI)) +
  geom_bar(fill = "darkseagreen") +
  labs(title = "Frequency of Talking to Someone (Non-AI)", x = NULL, y = "Count") +
  theme_minimal()

print(p_talk_noai)

p_network <- ggplot(tab_network, aes(x = reorder(option, n), y = pct_valid_respondents)) +
  geom_col(fill = "slateblue") +
  coord_flip() +
  labs(title = "Personal Support Network (Multiple Choice)", x = NULL, y = "% of Valid Respondents") +
  theme_minimal()

print(p_network)

# ------------------------------------------------------------------------------
# B. GENERAL AI USAGE VS. EMOTIONAL SUPPORT AI USAGE
# ------------------------------------------------------------------------------
# 1. Summary tables
freq_levels <- c("Never", "Rarely", "A few times a month", "A few times a week", "Daily")

tab_ai_gen <- df |>
  mutate(AI.usage.fraquency = factor(AI.usage.fraquency, levels = freq_levels)) |>
  count(AI.usage.fraquency, name = "n") |>
  mutate(pct = round(100 * n / sum(n), 1))

tab_ai_emo_yn <- df |>
  count(AI.used.for.emotional.support, name = "n") |>
  mutate(pct = round(100 * n / sum(n), 1))

tab_ai_emo_freq <- df |>
  mutate(Frequency.AI.used.for.emotional.support = factor(
    Frequency.AI.used.for.emotional.support, levels = freq_levels
  )) |>
  count(Frequency.AI.used.for.emotional.support, name = "n") |>
  mutate(pct = round(100 * n / sum(n), 1))

# Count respondents with valid multiple-choice AI use situations
valid_sit <- sum(!is.na(df$Which.situation.used.AI) & str_squish(df$Which.situation.used.AI) != "")
tab_situations <- df |>
  filter(!is.na(Which.situation.used.AI), str_squish(df$Which.situation.used.AI) != "") |>
  separate_longer_delim(Which.situation.used.AI, delim = ";") |>
  mutate(option = str_squish(Which.situation.used.AI)) |>
  filter(option != "") |>
  count(option, sort = TRUE, name = "n") |>
  mutate(pct_valid_respondents = round(100 * n / valid_sit, 1))

tab_decision <- df |>
  count(AI.decision.role, name = "n") |>
  mutate(pct = round(100 * n / sum(n), 1))

tab_post_ai <- df |>
  count(Post.AI.human.sharing, name = "n") |>
  mutate(pct = round(100 * n / sum(n), 1))

tab_dilemma <- df |>
  filter(!is.na(Personal.dilemma.reaction), str_squish(Personal.dilemma.reaction) != "") |>
  count(Personal.dilemma.reaction, sort = TRUE, name = "n") |>
  mutate(pct = round(100 * n / sum(n), 1))

print(tab_ai_gen)
print(tab_ai_emo_yn)
print(tab_ai_emo_freq)
print(tab_situations)
print(tab_decision)
print(tab_post_ai)
print(tab_dilemma)

# 2. Plots
# Side-by-side comparison: General AI usage frequency vs. emotional support AI usage frequency
p_ai_freq_comp <- df |>
  select(
    `General AI Usage` = AI.usage.fraquency,
    `Emotional Support AI Usage` = Frequency.AI.used.for.emotional.support
  ) |>
  pivot_longer(everything(), names_to = "usage_type", values_to = "frequency") |>
  filter(!is.na(frequency)) |>
  mutate(frequency = factor(frequency, levels = freq_levels)) |>
  ggplot(aes(x = frequency, fill = usage_type)) +
  geom_bar(position = "dodge") +
  labs(
    title = "AI Usage Frequency: General vs. Emotional Support",
    x = NULL, y = "Count", fill = NULL
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1), legend.position = "top")

print(p_ai_freq_comp)

p_situations <- ggplot(tab_situations, aes(x = reorder(option, n), y = pct_valid_respondents)) +
  geom_col(fill = "coral") +
  coord_flip() +
  labs(title = "Situations Where AI Was Used (Multiple Choice)", x = NULL, y = "% of Valid Respondents") +
  theme_minimal()

print(p_situations)

p_decision <- ggplot(tab_decision, aes(x = reorder(AI.decision.role, n), y = pct)) +
  geom_col(fill = "goldenrod3") +
  coord_flip() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 38)) +
  labs(title = "Role of AI in Personal Decisions", x = NULL, y = "% of Sample") +
  theme_minimal()

print(p_decision)

p_dilemma <- ggplot(tab_dilemma, aes(x = reorder(Personal.dilemma.reaction, n), y = pct)) +
  geom_col(fill = "mediumpurple3") +
  coord_flip() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 38)) +
  labs(title = "Typical Reaction to a Personal Dilemma", x = NULL, y = "% of Respondents") +
  theme_minimal()

print(p_dilemma)

# ------------------------------------------------------------------------------
# C. MENTAL HEALTH ACCESS AND BARRIERS
# ------------------------------------------------------------------------------
# 1. Summary tables
tab_mhp_sess <- df |>
  count(Session.mental.health.profess, name = "n") |>
  mutate(pct = round(100 * n / sum(n), 1))

tab_afford <- df |>
  count(MHP.budget.affordability, name = "n") |>
  mutate(pct = round(100 * n / sum(n), 1))

# Count respondents with valid multiple-choice mental health access barriers
valid_bar <- sum(!is.na(df$Access.barrier.to.MHP) & str_squish(df$Access.barrier.to.MHP) != "")
tab_barriers <- df |>
  filter(!is.na(Access.barrier.to.MHP), str_squish(df$Access.barrier.to.MHP) != "") |>
  separate_longer_delim(Access.barrier.to.MHP, delim = ";") |>
  mutate(option = str_squish(Access.barrier.to.MHP)) |>
  filter(option != "") |>
  count(option, sort = TRUE, name = "n") |>
  mutate(pct_valid_respondents = round(100 * n / valid_bar, 1))

print(tab_mhp_sess)
print(tab_afford)
print(tab_barriers)

# 2. Plots
p_mhp_sess <- df |>
  filter(!is.na(Session.mental.health.profess)) |>
  ggplot(aes(x = fct_infreq(Session.mental.health.profess))) +
  geom_bar(fill = "seagreen") +
  labs(title = "Sessions with Mental Health Professionals", x = NULL, y = "Count") +
  theme_minimal()

print(p_mhp_sess)

p_afford <- df |>
  filter(!is.na(MHP.budget.affordability)) |>
  ggplot(aes(x = fct_infreq(MHP.budget.affordability))) +
  geom_bar(fill = "darkorange2") +
  coord_flip() +
  labs(title = "Affordability of Private Therapy Sessions", x = NULL, y = "Count") +
  theme_minimal()

print(p_afford)

p_barriers <- ggplot(tab_barriers, aes(x = reorder(option, n), y = pct_valid_respondents)) +
  geom_col(fill = "indianred3") +
  coord_flip() +
  labs(title = "Barriers to Accessing Mental Health Professionals", x = NULL, y = "% of Valid Respondents") +
  theme_minimal()

print(p_barriers)

# ------------------------------------------------------------------------------
# D. PERCEPTIONS AND RISKS ON LIKERT SCALES (1-5): TOTAL VS. EMO-AI USERS
# ------------------------------------------------------------------------------
likert_vars <- c(
  "Perception.helpful.AI.support",
  "Prefer.AI.over.people",
  "Regular.AI.coping",
  "No.judgment.AI.advice",
  "AI.pleasing.bias",
  "AI.perceived.objectivity",
  "Concerns.AI.privacy",
  "Unsafe.AI.risks"
)

# 1. Comparative summary table
likert_summary <- bind_rows(
  df |>
    mutate(group = "Total Sample") |>
    pivot_longer(all_of(likert_vars), names_to = "variable", values_to = "value"),
  df |>
    filter(AI.used.for.emotional.support == "Yes") |>
    mutate(group = "Emotional AI Users (Yes)") |>
    pivot_longer(all_of(likert_vars), names_to = "variable", values_to = "value")
) |>
  filter(!is.na(value)) |>
  group_by(variable, group) |>
  summarise(
    n = n(),
    mean = round(mean(value), 2),
    sd = round(sd(value), 2),
    median = median(value),
    pct_agree_4_5 = round(100 * mean(value >= 4), 1),
    .groups = "drop"
  )

print(likert_summary)

# 2. Plot: % of agreement (score 4 or 5) across Likert items by group
p_likert_agree <- ggplot(likert_summary, aes(x = reorder(variable, pct_agree_4_5), y = pct_agree_4_5, fill = group)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(
    title = "Agreement (Score 4-5 out of 5) on AI Perceptions and Risks",
    x = NULL, y = "% of Respondents Scoring 4 or 5", fill = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "top")

print(p_likert_agree)



#==============================================================================
  # EXPLANATORY COMPONENT REGRESSIONS (social ladder -> affordability / cost
  # barrier -> AI frequency)
  # NOTE: these are separate regressions. No indirect effect (a x b) is computed,
  # so the results cannot be described as a confirmed mediation.


# 1. Analytic variables, coded in the SUBSTANTIVE order (never as.numeric(factor()))
df <- df |>
  mutate(
    # Predictor: perceived social status ladder (1-10)
    ses_ladder = as.numeric(Personal.perception.of.social.status),
    
    # Affordability: 0 = could not afford, 1 = with difficulty, 2 = comfortably.
    # "I'm not sure" and missing -> NA (same rule as the group's M1)
    afford_num = case_when(
      startsWith(MHP.budget.affordability, "No,")       ~ 0,
      startsWith(MHP.budget.affordability, "Yes, but")  ~ 1,
      startsWith(MHP.budget.affordability, "Yes, comf") ~ 2,
      TRUE ~ NA_real_
    ),
    
    # Cost barrier: 1 = "High cost / financial reasons" selected, 0 = not, NA = blank
    bar_cost = case_when(
      is.na(Access.barrier.to.MHP) ~ NA_real_,
      grepl("High cost / financial reasons", Access.barrier.to.MHP, fixed = TRUE) ~ 1,
      TRUE ~ 0
    ),
    
    # Outcome Y: frequency of AI use for emotional support, Never = 0 ... Daily = 4
    # (same coding as the group's M1; uses the frequency question directly)
    ai_support_freq_num = match(Frequency.AI.used.for.emotional.support,
                                freq_levels) - 1,
    
    # Flag: answered No/Yes to use but frequency says the opposite (14 cases)
    use_freq_inconsistent =
      (AI.used.for.emotional.support == "No"  & ai_support_freq_num > 0) |
      (AI.used.for.emotional.support == "Yes" & ai_support_freq_num == 0),
    
    # Secondary outcome: any session with a mental health professional (1 = yes)
    mhp_session_num = case_when(
      is.na(Session.mental.health.profess) ~ NA_real_,
      startsWith(Session.mental.health.profess, "Yes") ~ 1,
      TRUE ~ 0
    )
  )

# 2. CHECKS: every table must show a clean diagonal (one code per answer)
table(df$MHP.budget.affordability, df$afford_num, useNA = "ifany")
#   expected: No=0 (53), Yes but=1 (78), Yes comfortably=2 (44), NOT SURE -> NA (22+5 missing)
table(df$Frequency.AI.used.for.emotional.support, df$ai_support_freq_num, useNA = "ifany")
#   expected: Never=0 (87), Rarely=1 (66), month=2 (28), week=3 (14), Daily=4 (7)
table(cost_in_text = grepl("High cost / financial reasons", df$Access.barrier.to.MHP, fixed = TRUE),
      bar_cost = df$bar_cost, useNA = "ifany")
#   expected: TRUE -> 1 (91), FALSE -> 0 (86), FALSE -> NA (25 blank answers, incl. 1 cell with only a space)
table(df$Session.mental.health.profess, df$mhp_session_num, useNA = "ifany")
sum(df$use_freq_inconsistent, na.rm = TRUE)   # expected: 14 (13 "No" with freq > 0 + 1 "Yes" with Never)

# 3. Models (Age and Gender as controls, as in the previous version; the group's
#    M1 has no controls, so declare this difference when comparing results)
m_total  <- lm(ai_support_freq_num ~ ses_ladder + Age + Gender, data = df)
m_afford <- lm(afford_num ~ ses_ladder + Age + Gender, data = df)
m_cost   <- glm(bar_cost ~ ses_ladder + Age + Gender, data = df, family = binomial)
m_direct <- lm(ai_support_freq_num ~ ses_ladder + afford_num + bar_cost + Age + Gender,
               data = df)
m_secondary <- glm(mhp_session_num ~ ses_ladder + afford_num + bar_cost + Age + Gender,
                   data = df, family = binomial)

# 4. Tidy tables (broom)
results_tidy <- bind_rows(
  tidy(m_total,  conf.int = TRUE) |> mutate(model = "Total: AI_freq ~ ladder"),
  tidy(m_afford, conf.int = TRUE) |> mutate(model = "Path a1: afford_num ~ ladder"),
  tidy(m_cost,   conf.int = TRUE, exponentiate = TRUE) |>
    mutate(model = "Path a2: bar_cost (odds ratio) ~ ladder"),
  tidy(m_direct, conf.int = TRUE) |>
    mutate(model = "Direct: AI_freq ~ ladder + afford_num + bar_cost"),
  tidy(m_secondary, conf.int = TRUE, exponentiate = TRUE) |>
    mutate(model = "Secondary: MHP_session (odds ratio) ~ ladder + afford_num + bar_cost")
)

results_glance <- bind_rows(
  glance(m_total)     |> mutate(model = "Total"),
  glance(m_afford)    |> mutate(model = "Path a1"),
  glance(m_cost)      |> mutate(model = "Path a2"),
  glance(m_direct)    |> mutate(model = "Direct"),
  glance(m_secondary) |> mutate(model = "Secondary")
)

print(results_tidy, n = 50)
print(results_glance)
