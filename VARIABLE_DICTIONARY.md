# Preparation dictionary

The exact checkbox labels and mappings are specified in `scripts/00_prepare_data.R`. All 28 original columns are retained in input order as `raw_*` columns; raw answers, including refusals, inapplicability and unmatched text, are sensitive. Empty fields and the input's literal `NA` are imported as missing. Whitespace-only answers are retained in raw fields and treated as missing for derived values. No text is printed during mapping checks.

| Prepared fields | Meaning and codes |
| --- | --- |
| `respondent_id` | `r001`–`r204` in the original input order. The script requires the fixed file checksum before assigning IDs. Not derived from timestamps. |
| `genai_freq_num`, `ai_support_freq` | Never 0, Rarely 1, A few times a month 2, A few times a week 3, Daily 4. |
| `support_num` | Listener availability: Never 0, Rarely 1, Most of the time 2, Always 3. Not a measure of loneliness. |
| `afford_num` | Unable 0, with difficulty 1, comfortably 2. Unsure and refusal are NA; their different answers remain in `raw_affordability`. This numeric treatment is provisional. |
| `birth_year`, `social_ladder` | Integer source values; social ladder 1–10. Birth year does not establish exact age or eligibility. |
| `helpfulness`, `prefer_ai_over_people`, `regular_ai_coping`, `no_judgment`, `pleasing_bias`, `perceived_objectivity`, `privacy_worry`, `unsafe_risks` | Original integer scores 1–5, without reversal. Verify item wording and directions before interpreting. |
| `talk_*`, `bar_*`, `situation_*` | Named checkbox choices; exact source strings are listed in the script. 1 = selected, 0 = not selected in a fully recognized answered question, NA = blank or unselected with an unmatched token. Selected known choices stay 1 even when another token is unmatched. |
| `network_types` | Sum of friend, family and partner indicators, NA if any component is unknown. Counts categories, not people. |
| `network_unmatched`, `barriers_unmatched`, `situations_unmatched` | TRUE when at least one token does not match a known label. Inspect raw values privately and review mappings; these flags do not mean the answer is invalid. |
| `barriers_contradictory` | TRUE when explicit none and a known substantive barrier coexist. Raw values are not corrected. |
| `use_frequency_inconsistent` | TRUE when use is No with non-Never frequency, or Yes with Never frequency. Does not define who is an AI user. |
| `eligibility_status`, `eligibility_reason` | All records remain `undecided`; eligibility criteria are not agreed. |

The two documented situation non-use wordings map to one indicator. Other single-choice fields, including decision role and post-AI sharing, are retained as raw values rather than being silently normalized. Indicator NA values can therefore be higher than the count of blank questions; unknown free-text tokens make unselected indicators uncertain. A one-row-per-person output is generated directly without joins. The model variables and all mappings are proposals pending team review.
