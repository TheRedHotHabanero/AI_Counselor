# Input data audit

Verified 25 November 2026 against data/raw/AI_Counselor_pulito.csv. These findings describe this specific snapshot, not a later live Google Form export.

## Snapshot

- 204 respondent rows and 28 columns.
- SHA-256: 50487636cdef9d5dee373c57556657933e7daffc568259d6e5ea08080b024494
- No fully identical rows. There are 203 distinct timestamps, so a timestamp is not a unique respondent ID.
- All timestamps contain 2026.
- The teammate plan refers to survey_final_clean.csv and 202 respondents. That different file was not supplied. Do not infer which two rows to remove.

## Open decisions and data checks

| Issue | Observed | Required treatment before final analysis |
| --- | --- | --- |
| Eligibility | Only year of birth is present. 2026 minus birth year ranges from 18 to 32. | Year-only age is approximate. Agree an eligibility rule; flag rather than silently exclude. Some respondents born in 2008 could still be 17 at response date. |
| Barriers | 24 imported NA values plus one whitespace-only response. | Trim whitespace; retain 25 unknown responses as missing unless questionnaire routing establishes their meaning. |
| Affordability | 22 “I’m not sure” and 5 “Prefer not to say”. | Preserve raw categories. Proposed numeric mapping treats both as NA; this remains a coding decision. |
| AI-use consistency | 13 respondents report No but Rarely; one reports Yes but Never. | Check question wording and periods; preserve answers and flag. Do not silently select a user definition for H6–H7. |
| Other missing values | Social ladder 2; helpfulness 24; regular AI coping 5. | Evaluate applicability and model-specific missingness. |
| Occupation | One whitespace-only answer and one free-text category. | Preserve the original; agree categories rather than silently merging. |
| Wording variants | Some decision-role and post-AI-sharing non-use answers say “over the past year”; others say “in the past 12 months”. | Map clear equivalent standard labels transparently. Do not alter meaning. |
| Multiple selection | Support network, therapy barriers and situations contain delimited selections. | In this snapshot standard selections use semicolons. Inspect tokens and preserve unmatched text. |
| Contradictory options | “None” can coexist with substantive barrier choices. | Flag for review; do not overwrite selections automatically. |

Emotional-support frequency totals: Never 88, Rarely 67, a few times a month 28, a few times a week 14, Daily 7. The Yes/No use item has Yes 104 and No 100. These are different observed definitions, not interchangeable user counts.

Affordability substantive groups: unable to afford 54, with difficulty 78, comfortably 45. There are 177 respondents in these three groups before any eligibility filtering. Only 13 respondents place themselves on social-ladder rungs 1–4; broad claims about low-status groups require caution.

## Source and prepared names

The following table records source names and **historical proposed names**, not
the exact names in the generated table. For the implemented names and coding,
see `VARIABLE_DICTIONARY.md` and `scripts/00_prepare_data.R`. The script retains source
answers with a `raw_` prefix; it does not implement every historical proposal.

| Exact source column | Proposed name or derived field |
| --- | --- |
| Informazioni.cronologiche | submitted_at, retained for provenance; not a unique ID |
| Consent | consent_raw |
| Nationality | nationality |
| Main.occupation | occupation_raw, occupation |
| Gender | gender |
| Year.of.birth | birth_year; approximate_age only if clearly labelled |
| Parents.education | parents_education |
| Parsonal.percepition.of.social.status | social_ladder |
| AI.usage.fraquency | general_ai_frequency_raw; genai_freq_num |
| Support.talk.fraquency.noAI | listener_availability_raw; support_num |
| Parsonal.support.network | support_network_raw; talk_* indicators |
| Session.mental.health.profess. | therapy_history |
| Access.barrier.to.MHP | barriers_raw; bar_* indicators |
| MHP.budget.affordability | affordability_raw; afford_num |
| AI.used.for.emotional.support | ai_support_use_raw |
| Fraquency.AI.used.for.emotional.support | support_frequency_raw; ai_support_freq |
| Which.situation.used.AI | situations_raw; situation_* indicators |
| Percepition.helful.AI.support | helpfulness |
| Prefer.AI.over.people | prefer_ai_over_people |
| Regular.AI.coping | regular_ai_coping |
| NOjudgment.AI.advice | no_judgment |
| AI.pleasing.bias | pleasing_bias |
| AI.perceived.objectivity | perceived_objectivity |
| Privacy.worry.AI | privacy_worry |
| Unsafe.AI.risks | unsafe_risks |
| AI.decision.role | decision_role_raw, decision_role |
| Post.AI.human.sharing | post_ai_sharing_raw, post_ai_sharing |
| Personal.dilemma.reaction | dilemma_reaction_raw; agreed categories if recoded |

## Proposed codes from the supplied plan

- Both frequency items: Never 0, Rarely 1, A few times a month 2, A few times a week 3, Daily 4.
- afford_num: unable to afford 0, with difficulty 1, comfortably 2. Numeric treatment assumes equal steps; a factor version can check that assumption.
- support_num: Never 0, Rarely 1, Most of the time 2, Always 3.
- Likert scores remain 1–5 after verifying direction. Do not reverse-score or average unrelated questions without justification.
- Checkbox indicators: 1 selected, 0 not selected in a valid answered question, NA missing/unknown. Preserve explicit “none” and unmatched “Other” answers.
- network_types is the sum of known friend, partner and family indicators, ranging 0–3. It counts categories rather than people. Do not turn an unknown network into zero.

Keep original meaning alongside derived values where necessary. The proposed single wide analysis table can include status and review-flag columns. A separate raw copy retains the original delimited answers.
