# Team workflow

This is the main guide for the AI Counselor R project. Open the project folder
as the working directory; the same scripts work in RStudio, VS Code or a
terminal. See the shared analysis plan for the hypotheses and assignments.

## Project files

| File | Purpose |
| --- | --- |
| `scripts/00_prepare_data.R` | Creates the shared, flat analysis table from the fixed source CSV. |
| `scripts/01_h1_h3_pilot.R` | Karina's preliminary H1 affordability analysis and shared H1–H3 model; a commented example for the team. |
| `VARIABLE_DICTIONARY.md` | Explains the prepared variables, coding and review flags. |
| `DATA_AUDIT.md` | Describes the input snapshot, data issues and open decisions. |
| `AI_Counselor.Rproj` | Opens the project in RStudio; not required for VS Code or the terminal. |

## Run the preparation

Install R (tested with R 4.6.2). No extra R packages are required for preparation; base R and its bundled `tools` package are used. Package installation is separate from running scripts.

Open `AI_Counselor.Rproj` in RStudio, or open this folder in VS Code. In either case, make the project root your working directory. From a terminal in that directory run:

```sh
Rscript scripts/00_prepare_data.R
```

Or, in an R console whose working directory is the project root:

```r
source(file.path("scripts", "00_prepare_data.R"), encoding = "UTF-8")
```

The script reads `data/raw/final_data.csv` and creates `data/processed/analysis.csv`. Do not edit the generated table manually. Its fixed input checksum protects the respondent IDs: replacing or changing the input requires a reviewed ID and mapping strategy. See `VARIABLE_DICTIONARY.md`.


## Git basics

The team uses the public GitHub repository at https://github.com/TheRedHotHabanero/AI_Counselor, one `main` branch and separate script owners; one person maintains the shared preparation script. Survey data and generated results remain outside Git. Once collaborators have the required access:

1. **Pull** to get teammates' changes before starting: `git pull`.
2. Edit your files. Review `git status` and `git diff`; **stage** only intended files, for example `git add your_script.R`.
3. **Commit** the staged snapshot: `git commit -m "Describe the change"`.
4. **Push** it to the shared remote: `git push`.

Ask the coordinator for help with conflicts or a rejected push; never force push. Check staged files before committing (`git diff --cached` and `git status`), and never stage credentials, personal editor files or survey data without an explicit team decision. Git requires your own configured author name and email to commit; ask the coordinator if these are not set.

## Analysis next step

Before reporting final H1–H3 results, agree eligibility, treatment of missing and unmatched responses, controls, and coding directions. M1 is `ai_support_freq ~ afford_num + bar_cost + no_judgment + bar_stigma + genai_freq_num`. With the proposed codes, the expected signs are negative for affordability and positive for the other predictors. Describe N and missingness, report effect sizes and uncertainty, check model diagnostics, and note that linear regression approximates an ordinal outcome. Treat findings as associations, not causes. Do not make H6–H7 user-only selections until a user definition is agreed.

`scripts/01_h1_h3_pilot.R` is Karina's commented affordability (H1) analysis within M1. Run `Rscript scripts/01_h1_h3_pilot.R` from the project root after preparation. It uses preliminary complete cases and does not filter undecided eligibility; results are not final until the team agrees the analysis rules. It prints aggregate summaries and writes a coefficient table and plots under the ignored `outputs/` folder. Review outputs for disclosure risk before sharing. Other owners can adapt the structure to their own assigned method rather than reuse M1 for every hypothesis.
