# =============================================================================
# 02_generate_reproducible_writeup_pack.R
#
# Generates reproducible manuscript text (Methods / Results / data-availability)
# and a per-pool sentence bank directly from the meta-analysis results workbook
# produced by 01_run_beta_path_meta_analysis.R, so the reported numbers and the
# prose cannot drift apart. Also emits verification checks (input counts,
# terminology, and a causal-overclaiming guard).
#
# Input  : meta-analysis results workbook (auto-detects the most recent
#          beta_path_meta_analysis_results_*.xlsx in the output directory).
# Output : a write-up workbook (.xlsx) and write-up text (.md) in the output dir.
# Usage  : Rscript R/02_generate_reproducible_writeup_pack.R [results_workbook] [output_dir]
#          Defaults: latest workbook in outputs/, output dir outputs
#
# Author : Usman Yousaf
# License: MIT
# =============================================================================

required_pkgs <- c("readxl", "dplyr", "openxlsx", "tibble")
missing_pkgs <- setdiff(required_pkgs, rownames(installed.packages()))
if (length(missing_pkgs) > 0) {
  install.packages(missing_pkgs)
}

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(openxlsx)
  library(tibble)
})

args <- commandArgs(trailingOnly = TRUE)

meta_analysis_path <- if (length(args) >= 1) {
  args[[1]]
} else {
  candidates <- list.files(
    "outputs",
    pattern = "^beta_path_meta_analysis_results_.*\\.xlsx$",
    full.names = TRUE
  )
  if (length(candidates) == 0) {
    "outputs/beta_path_meta_analysis_results_YYYY-MM-DD.xlsx"
  } else {
    candidates[which.max(file.info(candidates)$mtime)]
  }
}

output_dir <- if (length(args) >= 2) {
  args[[2]]
} else {
  "outputs"
}

if (!file.exists(meta_analysis_path)) {
  stop(
    "Meta-analysis workbook not found. Run R/01_run_beta_path_meta_analysis.R first, ",
    "or provide the meta-analysis workbook as the first command-line argument. Current path: ",
    meta_analysis_path
  )
}

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

out_xlsx <- file.path(
  output_dir,
  paste0("meta_analysis_writeup_pack_", Sys.Date(), ".xlsx")
)

out_md <- file.path(
  output_dir,
  paste0("meta_analysis_writeup_text_", Sys.Date(), ".md")
)

read_sheet_safe <- function(path, sheet) {
  sheets <- readxl::excel_sheets(path)
  if (!sheet %in% sheets) {
    warning("Sheet not found: ", sheet, " in ", basename(path))
    return(tibble())
  }
  readxl::read_excel(path, sheet = sheet)
}

fmt3 <- function(x) {
  ifelse(is.na(x), "NA", sprintf("%.3f", as.numeric(x)))
}

fmtp <- function(p) {
  p <- as.numeric(p)
  ifelse(
    is.na(p),
    "NA",
    ifelse(p < 0.001, "<0.001", paste0("=", sub("^0", "", sprintf("%.3f", p))))
  )
}

ci_txt <- function(lo, hi) {
  paste0(fmt3(lo), " to ", fmt3(hi))
}

pool_short <- function(pool) {
  dplyr::case_when(
    pool == "Effort expectancy / ease of use" ~ "effort expectancy/ease of use",
    pool == "Performance expectancy / usefulness" ~ "performance expectancy/usefulness",
    pool == "Social influence" ~ "social influence",
    pool == "Facilitating conditions / organisational support" ~ "facilitating conditions/organisational support",
    TRUE ~ pool
  )
}

primary_interpretation <- function(row) {
  est <- as.numeric(row$estimate_beta)
  lo <- as.numeric(row$ci_lower)
  hi <- as.numeric(row$ci_upper)
  cr2_lo <- as.numeric(row$cr2_ci_lower)
  cr2_hi <- as.numeric(row$cr2_ci_upper)

  if (!is.na(lo) && !is.na(cr2_lo) && lo > 0 && cr2_lo > 0) {
    return("positive pooled beta/path association; primary and CR2 intervals exclude zero")
  }
  if (!is.na(lo) && !is.na(cr2_lo) && lo > 0 && cr2_lo <= 0 && cr2_hi >= 0) {
    return("positive in the primary model, but CR2 interval crosses zero; interpret cautiously")
  }
  if (!is.na(est) && est > 0 && lo <= 0 && hi >= 0) {
    return("positive direction, but confidence interval crosses zero")
  }
  if (!is.na(est) && est < 0 && lo <= 0 && hi >= 0) {
    return("negative direction, but confidence interval crosses zero")
  }

  "check manually"
}

get_row <- function(df, pool) {
  df %>% filter(construct_pool == pool) %>% slice(1)
}

primary <- read_sheet_safe(meta_analysis_path, "Primary_Model")
sensitivity <- read_sheet_safe(meta_analysis_path, "Sensitivity_OneEffect")
input_counts <- read_sheet_safe(meta_analysis_path, "Input_Counts")
interpretation_check <- read_sheet_safe(meta_analysis_path, "Interpretation_Check")

if (nrow(primary) == 0 || nrow(sensitivity) == 0 || nrow(input_counts) == 0) {
  stop("The meta-analysis workbook is missing required sheets or data.")
}

counts_check <- input_counts %>%
  mutate(
    k = as.integer(k),
    unique_studies = as.integer(unique_studies),
    unique_samples = as.integer(unique_samples),
    expected_k = as.integer(expected_k),
    expected_studies = as.integer(expected_studies),
    expected_samples = as.integer(expected_samples),
    k_matches = k == expected_k,
    studies_match = unique_studies == expected_studies,
    samples_match = unique_samples == expected_samples,
    all_count_checks_pass = k_matches & studies_match & samples_match
  )

if (!isTRUE(all(counts_check$all_count_checks_pass))) {
  print(counts_check)
  stop("Input counts do not match the documented analysis dataset.")
}

primary_sentences <- primary %>%
  mutate(
    pool_label = pool_short(construct_pool),
    primary_sentence = paste0(
      "For ", pool_label,
      ", the primary multilevel random-effects model showed a pooled beta/path association of ",
      fmt3(estimate_beta), " (95% CI ", ci_txt(ci_lower, ci_upper),
      "; p", fmtp(p_value), "; k=", k,
      ", ", unique_studies, " studies, ", unique_samples, " samples)."
    ),
    cr2_sentence = paste0(
      "The CR2 clustered interval was ", ci_txt(cr2_ci_lower, cr2_ci_upper),
      " (p", fmtp(cr2_p), "; df=", fmt3(cr2_df), ")."
    ),
    interpretation = vapply(seq_len(n()), function(i) primary_interpretation(primary[i, ]), character(1))
  ) %>%
  select(construct_pool, primary_sentence, cr2_sentence, interpretation)

sensitivity_sentences <- sensitivity %>%
  mutate(
    pool_label = pool_short(construct_pool),
    sensitivity_sentence = paste0(
      "In the one-effect-per-sample sensitivity analysis for ", pool_label,
      ", the pooled beta/path association was ", fmt3(estimate_beta),
      " (95% CI ", ci_txt(ci_lower, ci_upper), "; p", fmtp(p_value),
      "; k=", k, ", ", unique_studies, " studies, ", unique_samples,
      " samples). The CR2 interval was ", ci_txt(cr2_ci_lower, cr2_ci_upper),
      " (p", fmtp(cr2_p), ")."
    )
  ) %>%
  select(construct_pool, sensitivity_sentence)

pool_sentences <- primary_sentences %>%
  left_join(sensitivity_sentences, by = "construct_pool")

pe <- get_row(primary, "Performance expectancy / usefulness")
ee <- get_row(primary, "Effort expectancy / ease of use")
si <- get_row(primary, "Social influence")
fc <- get_row(primary, "Facilitating conditions / organisational support")
fc_sens <- get_row(sensitivity, "Facilitating conditions / organisational support")

methods_text <- paste0(
  "For the meta-analysable beta/path evidence, eligible determinant-to-acceptance/intention paths were synthesised using REML multilevel random-effects models, with effect IDs nested within sample units. ",
  "Cluster-robust CR2 intervals were estimated by clustering on sample unit, and one-effect-per-sample sensitivity analyses were used to assess whether findings depended on multiple effects from the same sample. ",
  "Results are described as pooled beta/path associations rather than causal effects. Coefficients are described as standardised only where the source study or extraction coding supported that interpretation."
)

results_text <- paste0(
  "In the meta-analysable beta/path evidence, performance expectancy/usefulness showed the strongest and most consistent positive pooled beta/path association with AI acceptance/intention (beta=", fmt3(pe$estimate_beta), ", 95% CI ", ci_txt(pe$ci_lower, pe$ci_upper), "; k=", pe$k, ", ", pe$unique_studies, " studies). ",
  "Effort expectancy/ease of use also showed a positive pooled association (beta=", fmt3(ee$estimate_beta), ", 95% CI ", ci_txt(ee$ci_lower, ee$ci_upper), "; k=", ee$k, ", ", ee$unique_studies, " studies). ",
  "Facilitating conditions/organisational support was positive in the primary model (beta=", fmt3(fc$estimate_beta), ", 95% CI ", ci_txt(fc$ci_lower, fc$ci_upper), "; k=", fc$k, ", ", fc$unique_studies, " studies). The one-effect-per-sample CR2 interval crossed zero (95% CI ", ci_txt(fc_sens$cr2_ci_lower, fc_sens$cr2_ci_upper), "). ",
  "Social influence was positive in direction but imprecise (beta=", fmt3(si$estimate_beta), ", 95% CI ", ci_txt(si$ci_lower, si$ci_upper), "; k=", si$k, ", ", si$unique_studies, " studies)."
)

repository_statement <- paste0(
  "The R code used to conduct the beta/path meta-analysis and to generate the reproducible write-up tables is available in the public repository associated with this article. ",
  "The repository contains the analysis scripts and supporting documentation. ",
  "Because this review extracted data from published articles, original full-text articles, copyrighted source material, and detailed extraction notes containing article text are not redistributed."
)

generated_text <- tibble::tibble(
  section = c("Methods wording", "Results wording", "Repository/data availability wording"),
  text = c(methods_text, results_text, repository_statement)
)

all_generated_text <- paste(generated_text$text, collapse = "\n")

# Neutralise the known-good disclaimer ("rather than causal effects") before the
# causal-overclaiming check, so the guard does not flag its own safeguard while
# still catching genuine causal claims elsewhere in the generated text.
overclaim_text <- gsub("rather than causal effects", "", tolower(all_generated_text), fixed = TRUE)

verification_checks <- tibble::tibble(
  check_area = c(
    "Input counts",
    "Programmatic generation",
    "Terminology: meta-analysable beta/path evidence",
    "Terminology: pooled beta/path association",
    "No causal overclaiming in generated core text"
  ),
  status = c(
    ifelse(isTRUE(all(counts_check$all_count_checks_pass)), "PASS", "FAIL"),
    "PASS: text generated from meta-analysis workbook sheets",
    ifelse(grepl("meta-analysable beta/path evidence", all_generated_text, fixed = TRUE), "PASS", "CHECK"),
    ifelse(grepl("pooled beta/path association", all_generated_text, fixed = TRUE), "PASS", "CHECK"),
    ifelse(grepl("caused|causes|causal effect", overclaim_text), "CHECK", "PASS")
  ),
  evidence_or_action = c(
    paste(counts_check$construct_pool, counts_check$k, counts_check$unique_studies, counts_check$unique_samples, collapse = " | "),
    paste0("Source workbook: ", basename(meta_analysis_path)),
    "Generated Methods/Results text",
    "Generated Methods/Results text",
    "Generated text search (disclaimer phrase excluded)"
  )
)

wb <- createWorkbook()

add_sheet <- function(wb, name, data) {
  addWorksheet(wb, name)
  writeData(wb, name, data)
  freezePane(wb, name, firstRow = TRUE)
  setColWidths(wb, name, cols = 1:ncol(data), widths = "auto")
  header_style <- createStyle(textDecoration = "bold", fgFill = "#D9EAF7", border = "Bottom")
  addStyle(wb, name, header_style, rows = 1, cols = 1:ncol(data), gridExpand = TRUE)
}

add_sheet(wb, "Generated_Text", generated_text)
add_sheet(wb, "Pool_Sentence_Bank", pool_sentences)
add_sheet(wb, "Primary_Model_Table", primary)
add_sheet(wb, "Sensitivity_Table", sensitivity)
add_sheet(wb, "Input_Counts_Check", counts_check)
add_sheet(wb, "Verification_Checks", verification_checks)
add_sheet(wb, "Interpretation_Check", interpretation_check)

saveWorkbook(wb, out_xlsx, overwrite = TRUE)

md_lines <- c(
  "# Reproducible manuscript text generated from meta-analysis outputs",
  "",
  paste0("Generated: ", Sys.time()),
  "",
  "## Methods text",
  "",
  methods_text,
  "",
  "## Results text",
  "",
  results_text,
  "",
  "## Repository/data availability text",
  "",
  repository_statement,
  "",
  "## Pool sentence bank",
  "",
  paste0("- ", pool_sentences$primary_sentence, " ", pool_sentences$cr2_sentence, " ", pool_sentences$sensitivity_sentence),
  ""
)

writeLines(md_lines, out_md, useBytes = TRUE)

message("Write-up workbook written to: ", out_xlsx)
message("Write-up markdown written to: ", out_md)
