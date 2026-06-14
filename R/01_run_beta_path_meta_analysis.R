# =============================================================================
# 01_run_beta_path_meta_analysis.R
#
# Multilevel random-effects meta-analysis of standardised beta/path coefficients
# (UTAUT/TAM determinants of AI acceptance among healthcare professionals).
#
# Method : REML multilevel models (metafor::rma.mv) with effects nested within
#          sample units, plus cluster-robust CR2 inference (clubSandwich)
#          clustered on sample unit, and a one-effect-per-sample sensitivity
#          analysis. The pipeline uses no random-number generation, so results
#          are deterministic given the same input and package versions.
#
# Input  : an analysis-ready table (Excel sheet "R_metafor_input_beta_path",
#          or a CSV) with columns: effect_id_R, Paper_ID, sample_unit_id,
#          STEP5_coarse_pool, yi_beta, sei, vi, relationship_N, metafor_ready.
#          No data are distributed with this repository (see README).
# Output : a timestamped results workbook and session_info.txt in the output dir.
# Usage  : Rscript R/01_run_beta_path_meta_analysis.R [input_path] [output_dir]
#          Defaults: data/analysis_ready_beta_path.xlsx, outputs
#
# Note   : The script verifies that the input reproduces the documented primary-
#          pool counts (k / studies / samples) and halts on mismatch. This is an
#          integrity guard calibrated to the published analysis dataset; reusing
#          the code on other data requires updating `expected_counts` below.
#
# Author : Usman Yousaf
# License: MIT
# =============================================================================

required_pkgs <- c(
  "readxl", "dplyr", "purrr", "stringr", "tibble",
  "metafor", "clubSandwich", "writexl"
)

missing_pkgs <- setdiff(required_pkgs, rownames(installed.packages()))
if (length(missing_pkgs) > 0) {
  install.packages(missing_pkgs)
}

suppressPackageStartupMessages({
  invisible(lapply(required_pkgs, library, character.only = TRUE))
})

args <- commandArgs(trailingOnly = TRUE)

input_path <- if (length(args) >= 1) {
  args[[1]]
} else {
  file.path("data", "analysis_ready_beta_path.xlsx")
}

output_dir <- if (length(args) >= 2) {
  args[[2]]
} else {
  "outputs"
}

input_sheet <- "R_metafor_input_beta_path"

if (!file.exists(input_path)) {
  stop(
    "Input file not found. Provide the analysis-ready dataset as the first command-line argument, ",
    "or place it at data/analysis_ready_beta_path.xlsx. Current input_path: ",
    input_path
  )
}

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

output_file <- file.path(
  output_dir,
  paste0("beta_path_meta_analysis_results_", Sys.Date(), ".xlsx")
)

read_analysis_input <- function(path, sheet = input_sheet) {
  extension <- tolower(tools::file_ext(path))

  if (extension %in% c("xlsx", "xls")) {
    return(readxl::read_excel(path, sheet = sheet))
  }

  if (extension == "csv") {
    return(read.csv(path, stringsAsFactors = FALSE, check.names = FALSE))
  }

  stop("Unsupported input type. Use .xlsx, .xls, or .csv")
}

raw <- read_analysis_input(input_path)

required_source_cols <- c(
  "effect_id_R", "Paper_ID", "sample_unit_id", "STEP5_coarse_pool",
  "yi_beta", "sei", "vi", "relationship_N", "metafor_ready"
)

missing_cols <- setdiff(required_source_cols, names(raw))
if (length(missing_cols) > 0) {
  stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
}

analysis_data <- raw %>%
  transmute(
    effect_id = as.character(effect_id_R),
    study_id = as.character(Paper_ID),
    sample_id = as.character(sample_unit_id),
    construct_pool = as.character(STEP5_coarse_pool),
    yi_beta = as.numeric(yi_beta),
    sei = as.numeric(sei),
    vi = as.numeric(vi),
    relationship_n = as.numeric(relationship_N),
    include_in_meta_analysis = stringr::str_to_lower(as.character(metafor_ready)) == "yes"
  ) %>%
  filter(include_in_meta_analysis)

primary_pools <- c(
  "Effort expectancy / ease of use",
  "Performance expectancy / usefulness",
  "Social influence",
  "Facilitating conditions / organisational support"
)

expected_counts <- tibble::tibble(
  construct_pool = primary_pools,
  expected_k = c(11L, 11L, 10L, 8L),
  expected_studies = c(8L, 8L, 7L, 5L),
  expected_samples = c(8L, 8L, 7L, 5L)
)

input_counts <- analysis_data %>%
  filter(construct_pool %in% primary_pools) %>%
  group_by(construct_pool) %>%
  summarise(
    k = n(),
    unique_studies = n_distinct(study_id),
    unique_samples = n_distinct(sample_id),
    included_studies = paste(sort(unique(study_id)), collapse = "; "),
    .groups = "drop"
  ) %>%
  right_join(expected_counts, by = "construct_pool") %>%
  mutate(
    k_matches_expected = k == expected_k,
    studies_match_expected = unique_studies == expected_studies,
    samples_match_expected = unique_samples == expected_samples,
    all_count_checks_pass = k_matches_expected & studies_match_expected & samples_match_expected
  )

print(input_counts)

if (!isTRUE(all(input_counts$all_count_checks_pass))) {
  stop(
    "Primary pool counts do not match the documented analysis dataset. ",
    "Check the input data before continuing (or update `expected_counts` if reusing on other data)."
  )
}

extract_cr2 <- function(fit, dat) {
  tryCatch({
    coef_table <- as.data.frame(clubSandwich::coef_test(
      fit,
      vcov = "CR2",
      cluster = dat$sample_id,
      test = "Satterthwaite"
    ))

    se_col <- grep("^SE$", names(coef_table), value = TRUE)[1]
    p_col <- grep("^p", names(coef_table), value = TRUE)[1]
    df_col <- grep("df", names(coef_table), value = TRUE)[1]
    beta_col <- if ("beta" %in% names(coef_table)) {
      "beta"
    } else {
      grep("beta|Estimate|coef", names(coef_table), ignore.case = TRUE, value = TRUE)[1]
    }

    cr2_est <- as.numeric(coef_table[[beta_col]][1])
    cr2_se <- as.numeric(coef_table[[se_col]][1])
    cr2_df <- as.numeric(coef_table[[df_col]][1])
    cr2_p <- as.numeric(coef_table[[p_col]][1])

    cr2_ci <- tryCatch({
      ci_raw <- as.data.frame(clubSandwich::conf_int(
        fit,
        vcov = "CR2",
        cluster = dat$sample_id,
        test = "Satterthwaite"
      ))
      lo_col <- grep("CI_L|CI\\.L|Lower|lower", names(ci_raw), value = TRUE)[1]
      hi_col <- grep("CI_U|CI\\.U|Upper|upper", names(ci_raw), value = TRUE)[1]
      c(as.numeric(ci_raw[[lo_col]][1]), as.numeric(ci_raw[[hi_col]][1]))
    }, error = function(e) {
      crit <- qt(0.975, df = cr2_df)
      c(cr2_est - crit * cr2_se, cr2_est + crit * cr2_se)
    })

    tibble::tibble(
      cr2_estimate_beta = cr2_est,
      cr2_se = cr2_se,
      cr2_df = cr2_df,
      cr2_p = cr2_p,
      cr2_ci_lower = cr2_ci[1],
      cr2_ci_upper = cr2_ci[2],
      cr2_note = NA_character_
    )
  }, error = function(e) {
    tibble::tibble(
      cr2_estimate_beta = NA_real_,
      cr2_se = NA_real_,
      cr2_df = NA_real_,
      cr2_p = NA_real_,
      cr2_ci_lower = NA_real_,
      cr2_ci_upper = NA_real_,
      cr2_note = paste("CR2 failed:", conditionMessage(e))
    )
  })
}

fit_pool <- function(dat, model_label) {
  dat <- dat %>% arrange(study_id, sample_id, effect_id)

  if (nrow(dat) < 3 || n_distinct(dat$sample_id) < 3) {
    return(tibble::tibble(
      construct_pool = unique(dat$construct_pool)[1],
      model_label = model_label,
      k = nrow(dat),
      unique_studies = n_distinct(dat$study_id),
      unique_samples = n_distinct(dat$sample_id),
      estimate_beta = NA_real_,
      se = NA_real_,
      ci_lower = NA_real_,
      ci_upper = NA_real_,
      z_or_t = NA_real_,
      p_value = NA_real_,
      tau2_total = NA_real_,
      sigma2_level_1 = NA_real_,
      sigma2_level_2 = NA_real_,
      I2_total_approx = NA_real_,
      QE = NA_real_,
      QEp = NA_real_,
      cr2_estimate_beta = NA_real_,
      cr2_se = NA_real_,
      cr2_df = NA_real_,
      cr2_p = NA_real_,
      cr2_ci_lower = NA_real_,
      cr2_ci_upper = NA_real_,
      cr2_note = "Not fitted: fewer than three effects or three unique samples",
      model_type = "Not fitted"
    ))
  }

  fit <- metafor::rma.mv(
    yi = yi_beta,
    V = vi,
    random = ~ 1 | sample_id / effect_id,
    method = "REML",
    data = dat
  )

  pred <- predict(fit)
  cr2 <- extract_cr2(fit, dat)
  sigma <- fit$sigma2
  sigma_level_1 <- ifelse(length(sigma) >= 1, sigma[1], NA_real_)
  sigma_level_2 <- ifelse(length(sigma) >= 2, sigma[2], NA_real_)
  tau_total <- sum(sigma, na.rm = TRUE)
  i2_approx <- 100 * tau_total / (tau_total + mean(dat$vi, na.rm = TRUE))

  tibble::tibble(
    construct_pool = unique(dat$construct_pool)[1],
    model_label = model_label,
    k = nrow(dat),
    unique_studies = n_distinct(dat$study_id),
    unique_samples = n_distinct(dat$sample_id),
    estimate_beta = as.numeric(pred$pred),
    se = as.numeric(pred$se),
    ci_lower = as.numeric(pred$ci.lb),
    ci_upper = as.numeric(pred$ci.ub),
    z_or_t = as.numeric(fit$zval),
    p_value = as.numeric(fit$pval),
    tau2_total = tau_total,
    sigma2_level_1 = sigma_level_1,
    sigma2_level_2 = sigma_level_2,
    I2_total_approx = i2_approx,
    QE = as.numeric(fit$QE),
    QEp = as.numeric(fit$QEp),
    cr2,
    model_type = "rma.mv REML multilevel random-effects model; effect_id nested within sample_id; CR2 clustered by sample_id"
  )
}

primary_data <- analysis_data %>% filter(construct_pool %in% primary_pools)

primary_results <- primary_data %>%
  group_split(construct_pool) %>%
  purrr::map_dfr(~ fit_pool(.x, "Primary: all eligible effects"))

print(primary_results)

one_effect_data <- primary_data %>%
  group_by(construct_pool, sample_id) %>%
  arrange(vi, effect_id, .by_group = TRUE) %>%
  slice(1) %>%
  ungroup()

one_effect_results <- one_effect_data %>%
  group_split(construct_pool) %>%
  purrr::map_dfr(~ fit_pool(.x, "Sensitivity: one lowest-variance effect per sample"))

print(one_effect_results)

interpretation_check <- bind_rows(primary_results, one_effect_results) %>%
  mutate(
    ci_for_check_lower = if_else(!is.na(cr2_ci_lower), cr2_ci_lower, ci_lower),
    ci_for_check_upper = if_else(!is.na(cr2_ci_upper), cr2_ci_upper, ci_upper),
    direction_positive = estimate_beta > 0,
    ci_crosses_zero = ci_for_check_lower <= 0 & ci_for_check_upper >= 0,
    ci_excludes_zero_positive = ci_for_check_lower > 0,
    interpretation_note = case_when(
      direction_positive & ci_excludes_zero_positive ~ "Positive association; interval excludes zero",
      direction_positive & ci_crosses_zero ~ "Positive direction; interval crosses zero",
      !direction_positive & ci_crosses_zero ~ "Negative direction; interval crosses zero",
      TRUE ~ "Check manually"
    )
  ) %>%
  select(
    model_label, construct_pool, k, unique_studies, unique_samples,
    estimate_beta, ci_lower, ci_upper, p_value,
    cr2_se, cr2_df, cr2_p, cr2_ci_lower, cr2_ci_upper,
    direction_positive, ci_crosses_zero, ci_excludes_zero_positive,
    interpretation_note
  )

print(interpretation_check)

session_info <- tibble::tibble(
  item = c("input_path", "output_file", "run_date", "R_version"),
  value = c(input_path, output_file, as.character(Sys.Date()), paste(R.version$major, R.version$minor))
)

writexl::write_xlsx(
  list(
    Input_Counts = input_counts,
    Primary_Model = primary_results,
    Sensitivity_OneEffect = one_effect_results,
    Interpretation_Check = interpretation_check,
    Input_Used = primary_data,
    OneEffect_Input_Used = one_effect_data,
    Session_Info = session_info
  ),
  path = output_file
)

# Capture the full session (R and package versions) for reproducibility.
writeLines(
  capture.output(sessionInfo()),
  file.path(output_dir, "session_info.txt")
)

message("Meta-analysis workbook written to: ", output_file)
message("Session info written to: ", file.path(output_dir, "session_info.txt"))
