# PURPOSE AND HOW TO RUN

# Run from the project root in a terminal: Rscript scripts/00_prepare_data.R
# Alternatively, from an R console whose working directory is the project root:
# source(file.path("scripts", "00_prepare_data.R"), encoding = "UTF-8")

# The script uses base R and the R-distributed tools package; no extra package
# installation is needed. It reads the original CSV and writes a separate CSV;
# it never edits the input!!! Running it again replaces the generated output.
#
# The output is one row per respondent. It contains source answers, derived
# variables and review flags. It can contain sensitive free text: do not commit
# it to Git or share it without the agreed safeguards.

# INPUT, OUTPUT AND FIXED SNAPSHOT
# file.path() builds portable relative paths on Windows, Linux and macOS.
# Paths are relative to the project root, not to the location of this script.
# PLEASE DO NOT USE FULL PATHS IN YOUR SCRIPTS, YOU WILL MAKE THEM NON-PORTABLE
input <- file.path("data", "raw", "AI_Counselor_pulito.csv")
output <- file.path("data", "processed", "analysis.csv")

# This is the MD5 checksum (a fingerprint of the file bytes) of the supplied
# snapshot. Initially it was computed in a Linux terminal from the project root with:
#   md5sum data/raw/AI_Counselor_pulito.csv
#
# IDs below depend on the original row order. Even a seemingly harmless edit,
# re-save or sort can change the file bytes; a changed order could assign an old
# ID to a different respondent. Stop instead of silently assigning misleading IDs!!!

# MD5 here detects accidental changes, not malicious tampering or authenticity.
expected_md5 <- "eee46e08bc81da06b882de7002f1ce5a"
if (!file.exists(input)) stop("Input CSV not found; run from the project root.")

# unname() removes the filename attached to the result by tools::md5sum().
if (unname(tools::md5sum(input)) != expected_md5) {
  stop("Input differs from the fixed snapshot; review ID assignment before proceeding.")
}

# Read all columns as text first, avoiding automatic guesses about their types.
# Empty fields and the literal text "NA" become R's missing value NA.
# check.names = FALSE keeps the source column names unchanged during import.
x <- read.csv(input, fileEncoding = "UTF-8", stringsAsFactors = FALSE,
              check.names = FALSE, na.strings = c("", "NA"),
              colClasses = "character")

# L marks an integer in R. The expected dimensions describe this snapshot, not
# an eligibility target: respondents are not dropped to reach a particular N.
if (nrow(x) != 204L || ncol(x) != 28L) stop("Unexpected input dimensions.")

# Assign readable names in the fixed source column order
names(x) <- c("submitted_at", "consent", "nationality", "occupation", "gender",
              "birth_year", "parents_education", "social_ladder", "general_ai_frequency",
              "listener_availability", "support_network", "therapy_history", "barriers",
              "affordability", "ai_support_use", "support_frequency", "situations",
              "helpfulness", "prefer_ai_over_people", "regular_ai_coping", "no_judgment",
              "pleasing_bias", "perceived_objectivity", "privacy_worry", "unsafe_risks",
              "decision_role", "post_ai_sharing", "dilemma_reaction")

# Retain all imported source columns under raw_* names, including free text.
# Imported missing values are NA, but answers are not otherwise recoded here.
names(x) <- paste0("raw_", names(x))

# seq_len() gives 1 through N; sprintf() formats them as r001, r002, etc.
# IDs are stable only for this verified snapshot in its original row order.
# d is the analysis data frame; x remains the imported source data frame.
d <- data.frame(respondent_id = sprintf("r%03d", seq_len(nrow(x))), x,
                check.names = FALSE, stringsAsFactors = FALSE)

# Helper: trim surrounding whitespace for derived values, then treat an empty
# answer as missing. It does not overwrite the corresponding raw_* column.
clean <- function(z) { z <- trimws(z); z[is.na(z) | z == ""] <- NA_character_; z }

# Helper: match each cleaned source answer to a named mapping and return the
# corresponding code. Missing or unmatched answers return NA. Later checks
# stop on unexpected categories for the mappings that must be complete.
recode <- function(z, mapping) {
  z <- clean(z)
  unname(mapping[match(z, names(mapping))])
}

# NUMERIC VARIABLES
# Both frequency questions use the same ordered 0–4 coding. The numeric codes
# are a proposed analysis convention; they do not make the outcome continuous.
frequency <- c("Never" = 0L, "Rarely" = 1L, "A few times a month" = 2L,
               "A few times a week" = 3L, "Daily" = 4L)
d$genai_freq_num <- recode(d$raw_general_ai_frequency, frequency)
d$ai_support_freq <- recode(d$raw_support_frequency, frequency)
d$support_num <- recode(d$raw_listener_availability,
                        c("Never" = 0L, "Rarely" = 1L, "Most of the time" = 2L, "Always" = 3L))

# The substantive affordability responses map to 0–2. "I'm not sure" and
# "Prefer not to say" map to NA, but remain distinguishable in raw_affordability.
# Treating these steps as numeric and equally spaced is a provisional decision.
d$afford_num <- recode(d$raw_affordability,
                      c("No, I couldn’t afford regular sessions" = 0L,
                        "Yes, but with some difficulty" = 1L, "Yes, comfortably" = 2L))

# These source fields are already numeric text. Convert them to integers without
# reverse-scoring; item direction must still be verified before interpretation.
numeric_fields <- c("birth_year", "social_ladder", "helpfulness", "prefer_ai_over_people",
                    "regular_ai_coping", "no_judgment", "pleasing_bias",
                    "perceived_objectivity", "privacy_worry", "unsafe_risks")
for (field in numeric_fields) {
  value <- clean(d[[paste0("raw_", field)]])
  # Suppress R's generic conversion warning because the explicit check below
  # stops with the field name if a nonmissing value cannot be represented exactly.
  d[[field]] <- suppressWarnings(as.integer(value))
  if (any(!is.na(value) & (is.na(d[[field]]) | as.character(d[[field]]) != value))) {
    stop("Invalid integer in ", field)
  }
}

# MULTIPLE-CHOICE QUESTIONS
# Each answer may contain selections separated by semicolons. choices is a
# named list: its names become output column names, and its values are the exact
# source labels recognized for each indicator. Unmatched tokens remain in raw_*.
checkbox <- function(raw, choices) {
  z <- clean(raw)
  # Split each answered field into trimmed selections; a blank has no tokens.
  tokens <- lapply(z, function(v) if (is.na(v)) character() else trimws(strsplit(v, ";", fixed = TRUE)[[1]]))
  known <- unlist(choices, use.names = FALSE)

  # Record whether each respondent has any selection outside the known labels.
  unmatched <- vapply(tokens, function(t) any(!t %in% known), logical(1))

  # For every indicator and respondent: missing question -> NA; selected -> 1;
  # unselected with an unmatched token -> NA; otherwise -> 0. Unknown text may
  # represent a choice we have not recognized, so do not silently assume zero.
  result <- lapply(choices, function(labels) vapply(seq_along(tokens), function(i) {
    if (is.na(z[i])) return(NA_integer_)
    if (any(tokens[[i]] %in% labels)) return(1L)
    if (unmatched[i]) return(NA_integer_)
    0L
  }, integer(1)))
  # Return the indicators and separate row-level review/missingness flags.
  list(values = result, unmatched = unmatched, missing = is.na(z))
}
# Labels below match the source exactly, including its spelling. Changing a
# label without checking the data can turn a known response into an unmatched one.
network <- checkbox(d$raw_support_network, list(
  talk_friends = "Friends", talk_family = "Family", talk_partner = "Partner",
  talk_professional = "Mental health professional (therapist, psychologist, counsellor)",
  talk_ai = "Artifical Intellligence"))
barriers <- checkbox(d$raw_barriers, list(
  bar_cost = "High cost / financial reasons", bar_self = "Preference to handle problems by myself",
  bar_time = "Lack of time or availability", bar_none = "None / I don’t need professional help",
  bar_find_professional = "Difficulty finding the right professional",
  bar_stigma = "Fear of judgement or social stigma"))
# Two observed non-use wordings are explicitly treated as the same indicator.
situations <- checkbox(d$raw_situations, list(
  situation_career = "Making career or academic choices",
  situation_nonuse = c("I haven’t used AI for personal matters over the past year", "I haven’t used AI for personal matters in the past 12 months"),
  situation_relationships = "Seeking advice on personal relationships or conflicts",
  situation_goals = "Everyday life choices and goal setting",
  situation_stress = "Vented about stress, anxiety, or a bad day",
  situation_no_judgment = "Exploring thoughts/emotions without feeling judged"))
# Copy all generated indicators into the one-row-per-person data frame.
for (group in list(network, barriers, situations)) {
  for (name in names(group$values)) d[[name]] <- group$values[[name]]
}
# Review flags identify rows, but the script does not print their free text.
d$network_unmatched <- network$unmatched
d$barriers_unmatched <- barriers$unmatched
d$situations_unmatched <- situations$unmatched
# This counts three kinds of human support, not the number of people. If any
# component is unknown, na.rm = FALSE keeps the total unknown instead of zero.
d$network_types <- rowSums(d[c("talk_friends", "talk_partner", "talk_family")], na.rm = FALSE)
# Flag an explicit "none" together with a known substantive barrier. This does
# not resolve the contradiction or search unmatched free text for other barriers.
d$barriers_contradictory <- !is.na(d$bar_none) & d$bar_none == 1L &
  rowSums(d[c("bar_cost", "bar_self", "bar_time", "bar_find_professional", "bar_stigma")] == 1L, na.rm = TRUE) > 0L
# Flag No paired with a non-Never frequency, or Yes paired with Never. The
# original answers are preserved; this is not a definition of an AI user.
d$use_frequency_inconsistent <- (clean(d$raw_ai_support_use) == "No" & d$ai_support_freq > 0L) |
  (clean(d$raw_ai_support_use) == "Yes" & d$ai_support_freq == 0L)
# No eligibility rule has been agreed. Keep every row and explicitly mark the
# unresolved status rather than interpreting birth year as exact age.
d$eligibility_status <- "undecided"
d$eligibility_reason <- "eligibility rule not agreed"

# VALIDATION: stop before writing if a required check fails.
# No joins are used: derived columns were added directly in original row order.
# Check that no rows were lost or added and that every generated ID is unique.
stopifnot(nrow(d) == nrow(x), !anyDuplicated(d$respondent_id))
# Helper: allow NA but reject any nonmissing value outside the supplied codes.
check_range <- function(name, allowed) {
  z <- d[[name]]
  if (any(!is.na(z) & !z %in% allowed)) stop("Out-of-range values in ", name)
}
for (name in c("genai_freq_num", "ai_support_freq")) check_range(name, 0:4)
check_range("support_num", 0:3); check_range("afford_num", 0:2)
check_range("social_ladder", 1:10)
for (name in setdiff(numeric_fields, c("birth_year", "social_ladder"))) check_range(name, 1:5)
indicator_names <- c(names(network$values), names(barriers$values), names(situations$values))
for (name in indicator_names) check_range(name, 0:1)
# Test selected, unselected, blank and unmatched cases using artificial values,
# not participants' responses. In particular, an unknown answer is not zero.
example <- checkbox(c("A", "B", NA_character_, "unknown", "A; unknown"),
                    list(a = "A", b = "B"))
stopifnot(identical(example$values$a, c(1L, 0L, NA_integer_, NA_integer_, 1L)),
          identical(example$values$b, c(0L, 1L, NA_integer_, NA_integer_, NA_integer_)))
# Also verify in the real data that blank questions have only NA indicators.
stopifnot(all(is.na(as.matrix(d[network$missing, names(network$values), drop = FALSE]))),
          all(is.na(as.matrix(d[barriers$missing, names(barriers$values), drop = FALSE]))),
          all(is.na(as.matrix(d[situations$missing, names(situations$values), drop = FALSE]))))
# For the required frequency and support mappings, every nonblank source value
# must have a numeric code. Unexpected categories stop the script for review.
for (pair in list(c("raw_general_ai_frequency", "genai_freq_num"),
                  c("raw_support_frequency", "ai_support_freq"),
                  c("raw_listener_availability", "support_num"))) {
  if (any(!is.na(clean(d[[pair[1]]])) & is.na(d[[pair[2]]]))) stop("Unmapped category in ", pair[1])
}
# Affordability has two recognized nonnumeric categories by design. Check
# against the full list instead of requiring those two to have numeric codes.
known_afford <- c("No, I couldn’t afford regular sessions", "Yes, but with some difficulty",
                  "Yes, comfortably", "I’m not sure", "Prefer not to say")
if (any(!is.na(clean(d$raw_affordability)) & !clean(d$raw_affordability) %in% known_afford)) stop("Unmapped affordability category")
# OUTPUT AND SUMMARY
# Create the output folder if needed and write UTF-8 CSV, using empty cells for
# NA. The output is generated and may be overwritten on the next run.
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
write.csv(d, output, row.names = FALSE, na = "", fileEncoding = "UTF-8")
# Read the saved file back to verify row count, ID order and column names. This
# checks structure, not a full cell-by-cell comparison of all output values.
check <- read.csv(output, fileEncoding = "UTF-8", stringsAsFactors = FALSE,
                  check.names = FALSE, colClasses = "character")
stopifnot(nrow(check) == nrow(d), identical(check$respondent_id, d$respondent_id),
          identical(names(check), names(d)))
# Print aggregate diagnostics only; never print participants' free-text answers.
cat("Prepared rows:", nrow(d), "; unique IDs:", length(unique(d$respondent_id)), "\n")
cat("Unmatched checkbox rows (network, barriers, situations):",
    sum(network$unmatched), sum(barriers$unmatched), sum(situations$unmatched), "\n")
cat("Missing checkbox questions (network, barriers, situations):",
    sum(network$missing), sum(barriers$missing), sum(situations$missing), "\n")
cat("Use/frequency inconsistencies:", sum(d$use_frequency_inconsistent, na.rm = TRUE), "\n")
cat("Missing values in prepared columns:\n")
print(colSums(is.na(d[setdiff(names(d), grep("^raw_", names(d), value = TRUE))])))
