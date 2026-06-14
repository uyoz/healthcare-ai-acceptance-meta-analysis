# Healthcare AI Acceptance — Beta/Path Meta-Analysis (R)

![R](https://img.shields.io/badge/R-%E2%89%A5%204.2-blue)
![License: MIT](https://img.shields.io/badge/License-MIT-green)
![Reproducible](https://img.shields.io/badge/analysis-deterministic-success)

R code for a **multilevel random-effects meta-analysis of standardised beta/path coefficients**, examining determinants of AI acceptance among healthcare professionals (UTAUT/TAM constructs). The pipeline pools eligible determinant-to-acceptance relationships, applies **cluster-robust (CR2) variance estimation** to handle dependent effect sizes, runs a sensitivity analysis, and then **auto-generates the manuscript-ready Methods/Results text directly from the numerical outputs** — so the reported figures and the prose can never drift apart.

This repository accompanies a systematic literature review submitted to the *International Journal of Medical Informatics*. It contains **code only**; no data are distributed (see [Input contract](#input-contract)).

---

## Why this design

- **Dependence handled properly.** Effect sizes are nested within independent samples and modelled with `metafor::rma.mv`; inference uses **CR2 cluster-robust** standard errors with Satterthwaite degrees of freedom (`clubSandwich`), the recommended approach for correlated/hierarchical effect sizes.
- **Sensitivity built in.** A one-effect-per-sample re-analysis tests whether conclusions depend on multiple effects drawn from the same sample.
- **Text generated from numbers, not by hand.** `02_*` reads the results workbook and writes the Methods, Results, and data-availability prose, plus a sentence bank — eliminating transcription error between tables and manuscript.
- **Integrity guard.** The scripts verify that the input reproduces the documented primary-pool counts (effects, studies, samples) and **halt** on mismatch.
- **No causal overclaiming.** Pooled estimates are reported as *beta/path associations*, never causal effects; an automated terminology check enforces this in the generated text.
- **Deterministic.** No random-number generation is involved, so results are bit-stable across runs given the same input and package versions.

---

## Repository structure

```
.
├── R/
│   ├── 01_run_beta_path_meta_analysis.R        # Fits pooled + sensitivity models, writes a results workbook
│   └── 02_generate_reproducible_writeup_pack.R # Generates Methods/Results text + sentence bank from results
├── .gitignore
├── CITATION.cff
├── LICENSE
└── README.md
```

---

## How to run

**Requirements:** R ≥ 4.2. Packages install automatically on first run: `readxl`, `dplyr`, `purrr`, `stringr`, `tibble`, `metafor`, `clubSandwich`, `writexl`, `openxlsx`.

```bash
# 1. Run the meta-analysis (writes a timestamped results workbook + session_info.txt to outputs/)
Rscript R/01_run_beta_path_meta_analysis.R <your_analysis_ready_input> outputs

# 2. Generate the reproducible write-up pack (auto-detects the latest results workbook in outputs/)
Rscript R/02_generate_reproducible_writeup_pack.R
```

`<your_analysis_ready_input>` is a table you supply that matches the contract below. Script 1 also runs with zero arguments if that table is placed at `data/analysis_ready_beta_path.xlsx`.

---

## Input contract

Script 1 expects an analysis-ready table — an Excel sheet named `R_metafor_input_beta_path`, or a CSV — with these columns:

| Column | Type | Description |
|--------|------|-------------|
| `effect_id_R` | character | Unique identifier for each extracted effect (one beta/path coefficient). |
| `Paper_ID` | character | Study identifier (author–year). |
| `sample_unit_id` | character | Identifier for the independent sample; the clustering unit for CR2 robust variance estimation. |
| `STEP5_coarse_pool` | character | Harmonised construct-pool label (e.g. `Performance expectancy / usefulness`). |
| `yi_beta` | numeric | Effect size: the standardised beta/path coefficient (`yi`). |
| `sei` | numeric | Standard error of the effect size. |
| `vi` | numeric | Sampling variance (`sei^2`). |
| `relationship_N` | numeric | Sample size for the modelled relationship. |
| `metafor_ready` | character | Inclusion flag (`yes`/`no`); only `yes` rows enter pooling. |

The four primary construct pools are: *effort expectancy / ease of use*, *performance expectancy / usefulness*, *social influence*, and *facilitating conditions / organisational support*.

> **Note on reuse.** The primary-pool count guard (`expected_counts` in script 1) is calibrated to the published analysis dataset and will halt on any other input. Adapting this code to a different dataset requires updating those expected counts.

---

## Method summary

Pooled estimates use REML multilevel random-effects models (`metafor::rma.mv`) with effects nested within sample units. Inference uses cluster-robust CR2 variance estimation with Satterthwaite degrees of freedom (`clubSandwich`), clustered on sample unit. A one-effect-per-sample model serves as a sensitivity check. Estimates are reported as pooled beta/path associations, not causal effects.

---

## Data availability

This repository shares **code only**. Because the underlying studies are published, copyrighted articles, original full texts and detailed extraction notes are not redistributed here.

## Citation

If you use this code, please cite it via [`CITATION.cff`](CITATION.cff).

## License

Released under the [MIT License](LICENSE).
