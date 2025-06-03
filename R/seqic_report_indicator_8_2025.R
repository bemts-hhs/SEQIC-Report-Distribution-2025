###_____________________________________________________________________________
### SEQIC Indicator Calculations 2025 - Indicator 8 Survival Rates
### To run this script, you must first have ran seqic_data_load.R and
### seqic_report_setup_2025.R.  Without running these, some functions and the
### data needed to use this script will not be available in your global
### environment
###_____________________________________________________________________________

### SEQIC indicator 8 ####

# Regions - overall
seqic_indicator_8_regions_overall <- trauma_2020_2024 |>
  traumar::seqic_indicator_8(
    level = Level,
    unique_incident_id = Unique_Incident_ID,
    mortality_indicator = Death,
    risk_group = Risk_Definition,
    groups = c("Year", "Service Area"),
    calculate_ci = "w"
  ) |>
  purrr::pluck(1) |> # get the first list element, which houses the overall results
  format_seqic_comparison(
    type = "region",
    rename_cols = TRUE,
    column_pattern = "(?:lower|upper)_ci",
    match_pattern = "_8$",
    replace_with = "_8_all"
  ) |>
  dplyr::select(-c(`lower ci`, `upper ci`)) |>
  dplyr::mutate(Risk_Definition = "All") |>
  dplyr::relocate(Risk_Definition, .after = `Service Area`)

# Regions - risk groups
seqic_indicator_8_regions_risk <- trauma_2020_2024 |>
  traumar::seqic_indicator_8(
    level = Level,
    unique_incident_id = Unique_Incident_ID,
    mortality_indicator = Death,
    risk_group = Risk_Definition,
    groups = c("Year", "Service Area"),
    calculate_ci = "w"
  ) |>
  purrr::pluck(2) |> # get the second list element, which houses the overall results
  format_seqic_comparison(
    type = "region",
    rename_cols = TRUE,
    column_pattern = "(?:lower|upper)_ci",
    match_pattern = "_8$",
    replace_with = "_8_risk"
  ) |>
  dplyr::select(-c(`lower ci`, `upper ci`))

# Union the region tibbles
seqic_indicator_8_regions <- seqic_indicator_8_regions_overall |>
  dplyr::bind_rows(seqic_indicator_8_regions_risk) |>
  dplyr::arrange(`Service Area`, Year, Risk_Definition)

# Level - overall
seqic_indicator_8_level_overall <- trauma_2020_2024 |>
  traumar::seqic_indicator_8(
    level = Level,
    unique_incident_id = Unique_Incident_ID,
    mortality_indicator = Death,
    risk_group = Risk_Definition,
    groups = c("Year", "Level_I_II"),
    calculate_ci = "w"
  ) |>
  purrr::pluck(1) |>
  format_seqic_comparison(
    type = "level",
    rename_cols = TRUE,
    column_pattern = "(?:lower|upper)_ci",
    match_pattern = "_8$",
    replace_with = "_8_all"
  ) |>
  dplyr::select(-c(`lower ci`, `upper ci`)) |>
  dplyr::mutate(Risk_Definition = "All") |>
  dplyr::relocate(Risk_Definition, .after = Level_I_II)

# Level - risk groups
seqic_indicator_8_level_risk <- trauma_2020_2024 |>
  traumar::seqic_indicator_8(
    level = Level,
    unique_incident_id = Unique_Incident_ID,
    mortality_indicator = Death,
    risk_group = Risk_Definition,
    groups = c("Year", "Level_I_II"),
    calculate_ci = "w"
  ) |>
  purrr::pluck(2) |>
  format_seqic_comparison(
    type = "level",
    rename_cols = TRUE,
    column_pattern = "(?:lower|upper)_ci",
    match_pattern = "_8$",
    replace_with = "_8_risk"
  ) |>
  dplyr::select(-c(`lower ci`, `upper ci`))

# Union the level tibbles
seqic_indicator_8_level <- seqic_indicator_8_level_overall |>
  dplyr::bind_rows(seqic_indicator_8_level_risk) |>
  dplyr::arrange(`Level_I_II`, Year, Risk_Definition)

# Agency-specific - overall
seqic_indicator_8_results_overall <- trauma_2020_2024 |>
  traumar::seqic_indicator_8(
    level = Level,
    unique_incident_id = Unique_Incident_ID,
    mortality_indicator = Death,
    risk_group = Risk_Definition,
    groups = c("Year", "Level_I_II", "Service Area", "Current Facility Name"),
    calculate_ci = "w"
  ) |>
  purrr::pluck(1) |>
  reshape_seqic_indicators(
    rename_cols = TRUE,
    column_pattern = "(?:lower|upper)_ci",
    match_pattern = "_8$",
    replace_with = "_8_all"
  ) |>
  match_seqic_indicator(col = indicator, performance_col = performance) |>
  dplyr::mutate(Risk_Definition = "All") |>
  dplyr::relocate(Risk_Definition, .after = `Service Area`) |>
  dplyr::left_join(
    seqic_indicator_8_level,
    by = dplyr::join_by(Year, Level_I_II, Risk_Definition, indicator)
  ) |>
  dplyr::left_join(
    seqic_indicator_8_regions,
    by = dplyr::join_by(Year, `Service Area`, Risk_Definition, indicator)
  )

# Agency-specific - risk groups
seqic_indicator_8_results_risk <- trauma_2020_2024 |>
  traumar::seqic_indicator_8(
    level = Level,
    unique_incident_id = Unique_Incident_ID,
    mortality_indicator = Death,
    risk_group = Risk_Definition,
    groups = c("Year", "Level_I_II", "Service Area", "Current Facility Name"),
    calculate_ci = "w"
  ) |>
  purrr::pluck(2) |>
  reshape_seqic_indicators(
    rename_cols = TRUE,
    column_pattern = "(?:lower|upper)_ci",
    match_pattern = "_8$",
    replace_with = "_8_risk"
  ) |>
  match_seqic_indicator(col = indicator, performance_col = performance) |>
  dplyr::left_join(
    seqic_indicator_8_level,
    by = dplyr::join_by(Year, Level_I_II, Risk_Definition, indicator)
  ) |>
  dplyr::left_join(
    seqic_indicator_8_regions,
    by = dplyr::join_by(Year, `Service Area`, Risk_Definition, indicator)
  ) |>
  dplyr::relocate(Risk_Definition, .after = `Service Area`)

# Union the results tibbles
seqic_indicator_8_results <- seqic_indicator_8_results_overall |>
  dplyr::bind_rows(seqic_indicator_8_results_risk) |>
  dplyr::mutate(
    Risk_Definition = ifelse(
      is.na(Risk_Definition),
      "Missing",
      Risk_Definition
    ),
    Risk_Definition = factor(
      Risk_Definition,
      levels = c("All", "Low", "Moderate", "High", "Missing")
    )
  ) |>
  dplyr::arrange(
    Year,
    `Service Area`,
    Level_I_II,
    `Current Facility Name`,
    Risk_Definition
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

### Export ####
export_seqic_data(
  agency_names = unique(trauma_2024$`Current Facility Name`),
  facility_name_col = `current facility name`,
  seqic_results = seqic_indicator_8_results,
  indicator = "indicator_8"
)
