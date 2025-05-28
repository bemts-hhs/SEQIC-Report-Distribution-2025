### IOWA SEQIC REPORT PREP 2024 ----------------------------------------------

# This script prepares for the analyses using the `traumar` package v1.2.0
# For the shapefiles, it is assumed that the files are downloaded from
# https://www.census.gov/cgi-bin/geo/shapefiles/index.php using the year 2024
# and then utilizing the Counties (and equivalent), States (and equivalent), and
# Zip Code Tabulation Areas (ZCTAS) options in the dropdown dialogue to download
# the files manually and put them in the directory used here.

### PACKAGES -------------------------------------------------------------------

# CRAN versions ================================================================

# install these packages if not already
# install.packages(
#   c(
#     "tidyverse",
#     "traumar",
#     "devtools",
#     "remotes",
#     "naniar",
#     "devtools",
#     "renv",
#     "roxygen2",
#     "roxygen2md"
#   )
# )

# load packages ----
# library(tidyverse)
# library(traumar)
# library(devtools)
# library(remotes)
# library(traumar)
# library(naniar)
# library(devtools)
# library(renv)
# library(roxygen2)
# library(roxygen2md)

# Handy Functions --------------------------------------------------------------

### DATA MANIPULATION FACILITIES ===============================================

###_____________________________________________________________________________
# Standard output from traumar is in wide format, with separate columns for each
# SEQIC indicator's numerator, denominator, and calculated value (seqic_*).
#
# The following metadata and helper functions facilitate transforming that wide
# output into a long, tidy format that is easier to analyze, visualize, and
# interpret.
#
# Recommended workflow:
# 1. Compute indicators using traumar::seqic_indicator_*() functions.
# 2. Pass the resulting wide-format data to reshape_seqic_indicators()
#    to pivot it into long format with separate rows per indicator.
# 3. Use match_seqic_indicator() to append indicator names and goals
#    by matching on the indicator code.
# 4. Compute comparison service area and trauma facility verification level
# results
# 5. Use format_seqic_comparison() to clean up service area and comparable trauma
# facility verification results
# 6. Join the service area and comparable verified trauma center data to the
# agency-level data to provide a type of benchmark
###_____________________________________________________________________________

# seqic_indicator_names
# A reference data frame containing SEQIC indicator metadata used for mapping,
# labeling, and evaluation of performance metrics. It includes:
#
# Columns:
# - Indicator: Character code identifying each SEQIC indicator (e.g., "1a", "9b_all").
# - Indicator_Name: Descriptive name of the indicator, suitable for labeling or reporting.
# - Goal: Numeric target threshold (if applicable) representing the performance benchmark
#         for the indicator. Values are `NA_real_` where no explicit goal is defined.
#
# This dataset supports automated indicator classification and goal evaluation within
# trauma and EMS quality assurance workflows.
{
  seqic_indicator_names <- data.frame(
    Indicator = c(
      "1a",
      "1b",
      "1c",
      "1d",
      "1e",
      "1f",
      "2",
      "3",
      "4a",
      "4b",
      "5a",
      "5b",
      "5c",
      "5d",
      "6",
      "7",
      "8",
      "9a_all",
      "9b_all",
      "9c_all",
      "9d_all",
      "9e_all",
      "9f_all",
      "9a_activations",
      "9b_activations",
      "9c_activations",
      "9d_activations",
      "9e_activations",
      "9f_activations",
      "9a_risk",
      "9b_risk",
      "9c_risk",
      "9d_risk",
      "9e_risk",
      "9f_risk",
      "9a_activations_risk",
      "9b_activations_risk",
      "9c_activations_risk",
      "9d_activations_risk",
      "9e_activations_risk",
      "9f_activations_risk",
      "10a",
      "10b",
      "10c",
      "11",
      "12",
      "13"
    ),
    Indicator_Name = c(
      "Indicator 1a - Trauma Surgeon Responding Within 15 Minutes",
      "Indicator 1b - Trauma Surgeon Responding Within 30 Minutes",
      "Indicator 1c - Trauma Surgeon Response Time Unknown",
      "Indicator 1d - Physician Responding Within 5 Minutes",
      "Indicator 1e - Physician Responding Within 20 Minutes",
      "Indicator 1f - Physician Response Time Unknown",
      "Indicator 2 - Injury Time Blank",
      "Indicator 3 - Probability of Survival Calculated",
      "Indicator 4a - Deceased Trauma Patient Autopsied",
      "Indicator 4b - No Autopsy on Death with LOS > 72 Hours",
      "Indicator 5a - Blood ETOH Measured",
      "Indicator 5b - Blood ETOH Positive",
      "Indicator 5c – Drug Screen Completed",
      "Indicator 5d – Drug Screen Positive",
      "Indicator 6 - GCS Less Than 9 And Arrived to Definitive Care > 3 Hours from Injury",
      "Indicator 7 - Patients to Definitive Care > 3 Hours",
      "Indicator 8 - Survival Rate",
      "Indicator 9a - LOS (ED Or Hospital) > 2 Hours Among Transferred Patients",
      "Indicator 9b - LOS (ED Or Hospital) > 3 Hours Among Transferred Patients",
      "Indicator 9c - Time from Pt Arrival to Decision to D/C > 1 Hour Among Transferred Patients",
      "Indicator 9d - Time from Pt Arrival to Decision to D/C > 2 Hours Among Transferred Patients",
      "Indicator 9e - Time from Decision to D/C to Physical D/C > 1 Hour Among Transferred Patients",
      "Indicator 9f - Time from Decision to D/C to Physical D/C > 2 Hours Among Transferred Patients",
      "Indicator 9a By TTA - LOS (ED Or Hospital) > 2 Hours Among Transferred Patients",
      "Indicator 9b By TTA - LOS (ED Or Hospital) > 3 Hours Among Transferred Patients",
      "Indicator 9c By TTA - Time from Pt Arrival to Decision to D/C > 1 Hour Among Transferred Patients",
      "Indicator 9d By TTA - Time from Pt Arrival to Decision to D/C > 2 Hours Among Transferred Patients",
      "Indicator 9e By TTA - Time from Decision to D/C to Physical D/C > 1 Hour Among Transferred Patients",
      "Indicator 9f By TTA - Time from Decision to D/C to Physical D/C > 2 Hours Among Transferred Patients",
      "Indicator 9a By Risk Group - LOS (ED Or Hospital) > 2 Hours Among Transferred Patients",
      "Indicator 9b By Risk Group - LOS (ED Or Hospital) > 3 Hours Among Transferred Patients",
      "Indicator 9c By Risk Group - Time from Pt Arrival to Decision to D/C > 1 Hour Among Transferred Patients",
      "Indicator 9d By Risk Group - Time from Pt Arrival to Decision to D/C > 2 Hours Among Transferred Patients",
      "Indicator 9e By Risk Group - Time from Decision to D/C to Physical D/C > 1 Hour Among Transferred Patients",
      "Indicator 9f By Risk Group - Time from Decision to D/C to Physical D/C > 2 Hours Among Transferred Patients",
      "Indicator 9a By TTA and Risk Group - LOS (ED Or Hospital) > 2 Hours Among Transferred Patients",
      "Indicator 9b By TTA and Risk Group - LOS (ED Or Hospital) > 3 Hours Among Transferred Patients",
      "Indicator 9c By TTA and Risk Group - Time from Pt Arrival to Decision to D/C > 1 Hour Among Transferred Patients",
      "Indicator 9d By TTA and Risk Group - Time from Pt Arrival to Decision to D/C > 2 Hours Among Transferred Patients",
      "Indicator 9e By TTA and Risk Group - Time from Decision to D/C to Physical D/C > 1 Hour Among Transferred Patients",
      "Indicator 9f By TTA and Risk Group - Time from Decision to D/C to Physical D/C > 2 Hours Among Transferred Patients",
      "Indicator 10a - Major Trauma With Limited-to-No TTA at Definitive Care (Cribari)",
      "Indicator 10b - Minor Trauma With the Highest Level TTA at Definitive Care",
      "Indicator 10c - Major Trauma With Limited-to-No TTA at Definitive Care (MOdified Cribari)",
      "Indicator 11 - ISS < 9 With ED LOS < 24 Hours Among Patients Transferred to Definitive Care",
      "Indicator 12 - Incidents Submitted Within 60 Days of Patient Discharge",
      "Indicator 13 - Incidents with Validity Score > 84"
    ),
    Goal = c(
      0.80,
      0.80,
      0,
      0.80,
      1,
      0,
      0.25,
      0.90,
      NA_real_,
      NA_real_,
      NA_real_,
      NA_real_,
      NA_real_,
      NA_real_,
      1,
      0,
      NA_real_,
      NA_real_,
      NA_real_,
      NA_real_,
      NA_real_,
      NA_real_,
      NA_real_,
      NA_real_,
      NA_real_,
      NA_real_,
      NA_real_,
      NA_real_,
      NA_real_,
      NA_real_,
      NA_real_,
      NA_real_,
      NA_real_,
      NA_real_,
      NA_real_,
      NA_real_,
      NA_real_,
      NA_real_,
      NA_real_,
      NA_real_,
      NA_real_,
      0.05,
      0.35,
      NA_real_,
      NA_real_,
      0.80,
      0.90
    )
  )
}

# Reshape SEQIC indicator columns from wide to long format
#
# Expects input data with indicator-specific columns such as:
#   numerator_1a, denominator_1a, seqic_1a, ..., numerator_1f, etc.
#
# This function:
# - Uses `pivot_longer()` with a regex-based pattern to extract both the indicator
#   prefix (numerator, denominator, seqic) and the indicator code (e.g., "1a").
# - The `.value` syntax ensures each prefix becomes a column in the result.
# - The "Indicator" column identifies which indicator each row corresponds to.
# - Renames the `seqic` column to `performance` for clarity.
#
# Resulting output includes columns: Year, Service Area, Indicator,
#   numerator, denominator, and performance.
reshape_seqic_indicators <- function(data) {
  data |>
    tidyr::pivot_longer(
      cols = tidyselect::starts_with(c(
        "numerator_",
        "denominator_",
        "seqic_",
        "lower_ci",
        "upper_ci"
      )),
      names_to = c(".value", "indicator"),
      names_pattern = "(numerator|denominator|seqic|lower_ci|upper_ci)_(\\d+[a-z]?)"
    ) |>
    dplyr::rename(
      performance = seqic,
      `lower ci` = lower_ci,
      `upper ci` = upper_ci
    )
}

# match_seqic_indicator()
# This function searches for the first matching SEQIC indicator code
# (as defined in `seqic_indicator_names`) within a specified text column.
# It appends three new columns to the input dataframe:
#   - matched_indicator: the first matching indicator code (e.g., "9b_all")
#   - Indicator_Name: the corresponding descriptive name of the indicator
#   - Goal: the numeric target threshold associated with the indicator
#
# Usage:
#   match_seqic_indicator(data, col)
#   - data: a dataframe containing a column with text to search
#   - col: unquoted name of the column to search (uses tidy evaluation)
#
# Note: Only the first match per row is returned. If no match is found,
# all new columns will be NA for that row.
match_seqic_indicator <- function(
  data,
  col,
  indicator_df = seqic_indicator_names,
  performance_col
) {
  # Build regex pattern to match indicators exactly (as whole words)
  pattern <- stringr::str_c(indicator_df$Indicator, collapse = "|")

  data |>
    dplyr::mutate(
      name = dplyr::case_when(
        !is.na({{ col }}) ~
          indicator_df$Indicator_Name[
            match({{ col }}, indicator_df$Indicator)
          ],
        TRUE ~ NA_character_
      ),
      goal = dplyr::case_when(
        !is.na({{ col }}) ~
          indicator_df$Goal[
            match({{ col }}, indicator_df$Indicator)
          ],
        TRUE ~ NA_real_
      )
    ) |>
    dplyr::relocate(name, .after = {{ col }}) |>
    dplyr::relocate(goal, .after = {{ performance_col }})
}

# --- format_seqic_comparison ---
# Generalized function to format SEQIC indicator results for use as
# comparison benchmarks (region or trauma facility level).
#
# Arguments:
# - data: A tibble of wide-format SEQIC indicator results.
# - type: A string specifying the benchmark type, either "region" or "level".
#
# Output:
# A long-format tibble with indicator metadata and a single column renamed to
# reflect the comparison type:
# - "comparison region performance" or
# - "comparison trauma facility performance"
#
# Usage in workflow:
# 1. Calculate comparison data grouped by region or trauma level.
# 2. Use this function to prepare those data for joining with agency-level outputs.

format_seqic_comparison <- function(data, type = c("region", "level")) {
  type <- rlang::arg_match(type)

  # Determine appropriate name
  perf_col <- dplyr::case_when(
    type == "region" ~ "comparison region performance",
    type == "level" ~ "comparison trauma facility performance"
  )

  data |>
    reshape_seqic_indicators() |>
    match_seqic_indicator(col = indicator, performance_col = performance) |>
    dplyr::select(-c(name, numerator, denominator, goal)) |>
    dplyr::rename(!!perf_col := performance)
}

# join comparison data to SEQIC results
join_comparison_data <- function(data, data_level, data_region) {
  data |>
    dplyr::left_join(
      data_level,
      by = dplyr::join_by(Year, Level_I_II, indicator)
    ) |>
    dplyr::left_join(
      data_region,
      by = dplyr::join_by(Year, `Service Area`, indicator)
    ) |>
    dplyr::mutate(dplyr::across(
      c(
        performance,
        goal,
        `lower ci`,
        `upper ci`,
        `comparison trauma facility performance`,
        `comparison region performance`
      ),
      ~ ifelse(
        is.na(.),
        NA_real_,
        traumar::pretty_percent(variable = ., n_decimal = 2)
      )
    )) |>
    dplyr::rename_with(~ tolower(.)) |>
    dplyr::rename(level = level_i_ii)
}

### DATA EXPORT FACILITIES =====================================================

# ---- export_seqic_data ----
# Export SEQIC Indicator Results to CSV by Agency
#
# This function iterates over a vector of agency names and exports SEQIC
# indicator results (from a precomputed results tibble) to individual
# CSV files for each agency. Files are saved in dynamically generated
# subdirectories based on agency name.
#
# Parameters:
# - agency_names: A character vector of agency or facility names. Each name must
#   match values in the `Facility_Name` column of `seqic_results`.
#
# - seqic_results: A data frame or tibble of SEQIC indicator results that
#   includes a `Facility_Name` column used to filter results per agency.
#
# - indicator: A string indicating the indicator group (e.g., "1a_1f",
#   "4a_4b", "all_indicators") to name output files accordingly.
#
# Side Effects:
# - Creates output directories (if not already present) using:
#   `"C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/SEQIC Facility Reports/2024/output/"`
#
# - Writes agency-specific CSV files to the directory with naming convention:
#   `{agency}_{indicator}.csv`
#
# - Logs progress, warnings, and a summary using the `cli` package.
#
# Output:
# - Returns `invisible(NULL)`; primary function is side-effect file export.
#
# Workflow Context:
# - Use this function after calculating and formatting SEQIC results with
#   `traumar::seqic_indicator_*()` and any additional wrangling or annotation.
# - Designed for batch export of customized reports at the agency level.
#
# Example:
# export_seqic_data(
#   agency_names = unique(seqic_results$Facility_Name),
#   seqic_results = formatted_results,
#   indicator = "8_all"
# )

export_seqic_data <- function(
  agency_names,
  facility_name_col,
  seqic_results,
  indicator,
  output_dir_root = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/SEQIC Facility Reports/2024/output/"
) {
  # Validate input
  if (!tibble::is_tibble(seqic_results) && !is.data.frame(seqic_results)) {
    rlang::abort("`seqic_results` must be a data.frame or tibble.")
  }

  exported_count <- 0
  skipped_count <- 0
  agency_count <- length(agency_names)

  for (agency in agency_names) {
    temp <- seqic_results |>
      dplyr::filter({{ facility_name_col }} == agency)

    if (nrow(temp) == 0) {
      cli::cli_warn("Skipped {agency}: no matching rows in `seqic_results`.")
      skipped_count <- skipped_count + 1
      next
    }

    # Create dynamic output path
    output_path <- fs::path(output_dir_root, tolower(agency))
    fs::dir_create(output_path)

    # Build file path and write CSV
    file_path <- fs::path(
      output_path,
      glue::glue("{tolower(agency)}_{indicator}.csv")
    )
    readr::write_csv(x = temp, file = file_path)
    cli::cli_inform(c("v" = "Exported: {file_path}"))
    exported_count <- exported_count + 1
  }

  # Summary
  cli::cli_h1("SEQIC Indicator Export Summary")
  cli::cli_alert_success("Total agencies processed: {agency_count}")
  cli::cli_alert_success("Successfully exported: {exported_count}")
  cli::cli_alert_warning("Skipped (no matching data): {skipped_count}")

  invisible(NULL)
}

###_____________________________________________________________________________
# we will:
# - examine calendar years 2020-2024 of trauma data
# - ingest data and calculate results by agency and year using the `traumar`
# package
# - send 1 file per indicator to each agency
###_____________________________________________________________________________
