###_____________________________________________________________________________
### SEQIC Indicator Calculations 2025 - Indicator 9 Transfer Delays
### To run this script, you must first have ran seqic_data_load.R and
### seqic_report_setup_2025.R.  Without running these, some functions and the
### data needed to use this script will not be available in your global
### environment
###_____________________________________________________________________________

### SEQIC indicator 9 ####

# Regions - overall
seqic_indicator_9_regions_overall <- trauma_2020_2024 |>
  traumar::seqic_indicator_9(
    level = Level,
    unique_incident_id = Unique_Incident_ID,
    transfer_out_indicator = Acute_Transfer_Out,
    transport_method = Transport_To_Your_Facility_By,
    trauma_team_activated = Trauma_Team_Activated,
    risk_group = Risk_Definition,
    ed_LOS = Length_of_Stay,
    ed_decision_LOS = Time_From_ED_Arrival_to_Decision_Minutes,
    ed_decision_discharge_LOS = Time_From_ED_Decision_to_Discharge,
    groups = c("Year", "Service Area"),
    calculate_ci = "w"
  ) |>
  purrr::pluck(1) |> # get the first list element, which houses the overall results
  format_seqic_comparison(type = "region") |>
  dplyr::select(-c(`lower ci`, `upper ci`)) |>
  dplyr::mutate(Trauma_Team_Activated = "All Records") |>
  dplyr::relocate(Trauma_Team_Activated, .after = `Service Area`)

# Regions - by trauma team activation status
seqic_indicator_9_regions_activation <- trauma_2020_2024 |>
  traumar::seqic_indicator_9(
    level = Level,
    unique_incident_id = Unique_Incident_ID,
    transfer_out_indicator = Acute_Transfer_Out,
    transport_method = Transport_To_Your_Facility_By,
    trauma_team_activated = Trauma_Team_Activated,
    risk_group = Risk_Definition,
    ed_LOS = Length_of_Stay,
    ed_decision_LOS = Time_From_ED_Arrival_to_Decision_Minutes,
    ed_decision_discharge_LOS = Time_From_ED_Decision_to_Discharge,
    groups = c("Year", "Service Area"),
    calculate_ci = "w"
  ) |>
  purrr::pluck(2) |> # get the second list element, which houses the overall results
  format_seqic_comparison(type = "region") |>
  dplyr::select(-c(`lower ci`, `upper ci`))

# Union the region tibbles
seqic_indicator_9_regions <- seqic_indicator_9_regions_overall |>
  dplyr::bind_rows(seqic_indicator_9_regions_activation) |>
  dplyr::arrange(`Service Area`, Year, Trauma_Team_Activated)

# Level - overall
seqic_indicator_9_level_overall <- trauma_2020_2024 |>
  traumar::seqic_indicator_9(
    level = Level,
    unique_incident_id = Unique_Incident_ID,
    transfer_out_indicator = Acute_Transfer_Out,
    transport_method = Transport_To_Your_Facility_By,
    trauma_team_activated = Trauma_Team_Activated,
    risk_group = Risk_Definition,
    ed_LOS = Length_of_Stay,
    ed_decision_LOS = Time_From_ED_Arrival_to_Decision_Minutes,
    ed_decision_discharge_LOS = Time_From_ED_Decision_to_Discharge,
    groups = c("Year", "Level_I_II"),
    calculate_ci = "w"
  ) |>
  purrr::pluck(1) |>
  format_seqic_comparison(type = "level") |>
  dplyr::select(-c(`lower ci`, `upper ci`)) |>
  dplyr::mutate(Trauma_Team_Activated = "All Records") |>
  dplyr::relocate(Trauma_Team_Activated, .after = Level_I_II)

# Level - by trauma team activation status
seqic_indicator_9_level_activation <- trauma_2020_2024 |>
  traumar::seqic_indicator_9(
    level = Level,
    unique_incident_id = Unique_Incident_ID,
    transfer_out_indicator = Acute_Transfer_Out,
    transport_method = Transport_To_Your_Facility_By,
    trauma_team_activated = Trauma_Team_Activated,
    risk_group = Risk_Definition,
    ed_LOS = Length_of_Stay,
    ed_decision_LOS = Time_From_ED_Arrival_to_Decision_Minutes,
    ed_decision_discharge_LOS = Time_From_ED_Decision_to_Discharge,
    groups = c("Year", "Level_I_II"),
    calculate_ci = "w"
  ) |>
  purrr::pluck(2) |>
  format_seqic_comparison(type = "level") |>
  dplyr::select(-c(`lower ci`, `upper ci`))

# Union the level tibbles
seqic_indicator_9_level <- seqic_indicator_9_level_overall |>
  dplyr::bind_rows(seqic_indicator_9_level_activation) |>
  dplyr::arrange(`Level_I_II`, Year, Trauma_Team_Activated)

# Agency-specific - overall
seqic_indicator_9_results_overall <- trauma_2020_2024 |>
  traumar::seqic_indicator_9(
    level = Level,
    unique_incident_id = Unique_Incident_ID,
    transfer_out_indicator = Acute_Transfer_Out,
    transport_method = Transport_To_Your_Facility_By,
    trauma_team_activated = Trauma_Team_Activated,
    risk_group = Risk_Definition,
    ed_LOS = Length_of_Stay,
    ed_decision_LOS = Time_From_ED_Arrival_to_Decision_Minutes,
    ed_decision_discharge_LOS = Time_From_ED_Decision_to_Discharge,
    groups = c("Year", "Level_I_II", "Service Area", "Current Facility Name"),
    calculate_ci = "w"
  ) |>
  purrr::pluck(1) |>
  reshape_seqic_indicators() |>
  match_seqic_indicator(col = indicator, performance_col = performance) |>
  dplyr::mutate(Trauma_Team_Activated = "All Records") |>
  dplyr::relocate(Trauma_Team_Activated, .after = `Service Area`) |>
  dplyr::left_join(
    seqic_indicator_9_level,
    by = dplyr::join_by(Year, Level_I_II, Trauma_Team_Activated, indicator)
  ) |>
  dplyr::left_join(
    seqic_indicator_9_regions,
    by = dplyr::join_by(Year, `Service Area`, Trauma_Team_Activated, indicator)
  )

# Agency-specific - by trauma team activation status
seqic_indicator_9_results_activation <- trauma_2020_2024 |>
  traumar::seqic_indicator_9(
    level = Level,
    unique_incident_id = Unique_Incident_ID,
    transfer_out_indicator = Acute_Transfer_Out,
    transport_method = Transport_To_Your_Facility_By,
    trauma_team_activated = Trauma_Team_Activated,
    risk_group = Risk_Definition,
    ed_LOS = Length_of_Stay,
    ed_decision_LOS = Time_From_ED_Arrival_to_Decision_Minutes,
    ed_decision_discharge_LOS = Time_From_ED_Decision_to_Discharge,
    groups = c("Year", "Level_I_II", "Service Area", "Current Facility Name"),
    calculate_ci = "w"
  ) |>
  purrr::pluck(2) |>
  reshape_seqic_indicators() |>
  match_seqic_indicator(col = indicator, performance_col = performance) |>
  dplyr::left_join(
    seqic_indicator_9_level,
    by = dplyr::join_by(Year, Level_I_II, Trauma_Team_Activated, indicator)
  ) |>
  dplyr::left_join(
    seqic_indicator_9_regions,
    by = dplyr::join_by(Year, `Service Area`, Trauma_Team_Activated, indicator)
  ) |>
  dplyr::relocate(Trauma_Team_Activated, .after = `Service Area`)

# Union the results tibbles
seqic_indicator_9_results <- seqic_indicator_9_results_overall |>
  dplyr::bind_rows(seqic_indicator_9_results_activation) |>
  dplyr::mutate(
    Trauma_Team_Activated = ifelse(
      is.na(Trauma_Team_Activated),
      "Missing",
      Trauma_Team_Activated
    ),
    Trauma_Team_Activated = factor(
      Trauma_Team_Activated,
      levels = c(
        "All Records",
        "Trauma Team Activated",
        "Trauma Team Not Activated",
        "Missing"
      )
    )
  ) |>
  dplyr::arrange(
    Year,
    `Service Area`,
    Level_I_II,
    `Current Facility Name`,
    Trauma_Team_Activated
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

###_____________________________________________________________________________
### More granular analyses
### These analyses will not have the same features as the previous
### They will just report the simple outputs from the functions and compare each
### facility to others in their region and among similarly verified trauma
### centers
###_____________________________________________________________________________

# Regions - risk group
seqic_indicator_9_regions_risk <- trauma_2020_2024 |>
  traumar::seqic_indicator_9(
    level = Level,
    unique_incident_id = Unique_Incident_ID,
    transfer_out_indicator = Acute_Transfer_Out,
    transport_method = Transport_To_Your_Facility_By,
    trauma_team_activated = Trauma_Team_Activated,
    risk_group = Risk_Definition,
    ed_LOS = Length_of_Stay,
    ed_decision_LOS = Time_From_ED_Arrival_to_Decision_Minutes,
    ed_decision_discharge_LOS = Time_From_ED_Decision_to_Discharge,
    groups = c("Year", "Service Area"),
    calculate_ci = "w"
  ) |>
  purrr::pluck(3) |> # get the third list element, which houses the risk group results
  format_seqic_comparison(type = "region") |>
  dplyr::select(-c(`lower ci`, `upper ci`))

# Regions - activations and risk group
seqic_indicator_9_regions_activations_risk <- trauma_2020_2024 |>
  traumar::seqic_indicator_9(
    level = Level,
    unique_incident_id = Unique_Incident_ID,
    transfer_out_indicator = Acute_Transfer_Out,
    transport_method = Transport_To_Your_Facility_By,
    trauma_team_activated = Trauma_Team_Activated,
    risk_group = Risk_Definition,
    ed_LOS = Length_of_Stay,
    ed_decision_LOS = Time_From_ED_Arrival_to_Decision_Minutes,
    ed_decision_discharge_LOS = Time_From_ED_Decision_to_Discharge,
    groups = c("Year", "Service Area"),
    calculate_ci = "w"
  ) |>
  purrr::pluck(4) |> # get the fourth list element, which houses the activation / risk group results
  format_seqic_comparison(type = "region") |>
  dplyr::select(-c(`lower ci`, `upper ci`))

# Level - risk group
seqic_indicator_9_level_risk <- trauma_2020_2024 |>
  traumar::seqic_indicator_9(
    level = Level,
    unique_incident_id = Unique_Incident_ID,
    transfer_out_indicator = Acute_Transfer_Out,
    transport_method = Transport_To_Your_Facility_By,
    trauma_team_activated = Trauma_Team_Activated,
    risk_group = Risk_Definition,
    ed_LOS = Length_of_Stay,
    ed_decision_LOS = Time_From_ED_Arrival_to_Decision_Minutes,
    ed_decision_discharge_LOS = Time_From_ED_Decision_to_Discharge,
    groups = c("Year", "Level_I_II"),
    calculate_ci = "w"
  ) |>
  purrr::pluck(3) |>
  format_seqic_comparison(type = "level") |>
  dplyr::select(-c(`lower ci`, `upper ci`))

# Level - activations and risk group
seqic_indicator_9_level_activations_risk <- trauma_2020_2024 |>
  traumar::seqic_indicator_9(
    level = Level,
    unique_incident_id = Unique_Incident_ID,
    transfer_out_indicator = Acute_Transfer_Out,
    transport_method = Transport_To_Your_Facility_By,
    trauma_team_activated = Trauma_Team_Activated,
    risk_group = Risk_Definition,
    ed_LOS = Length_of_Stay,
    ed_decision_LOS = Time_From_ED_Arrival_to_Decision_Minutes,
    ed_decision_discharge_LOS = Time_From_ED_Decision_to_Discharge,
    groups = c("Year", "Level_I_II"),
    calculate_ci = "w"
  ) |>
  purrr::pluck(4) |>
  format_seqic_comparison(type = "level") |>
  dplyr::select(-c(`lower ci`, `upper ci`))

# Agency-specific - risk group
seqic_indicator_9_results_risk <- trauma_2020_2024 |>
  traumar::seqic_indicator_9(
    level = Level,
    unique_incident_id = Unique_Incident_ID,
    transfer_out_indicator = Acute_Transfer_Out,
    transport_method = Transport_To_Your_Facility_By,
    trauma_team_activated = Trauma_Team_Activated,
    risk_group = Risk_Definition,
    ed_LOS = Length_of_Stay,
    ed_decision_LOS = Time_From_ED_Arrival_to_Decision_Minutes,
    ed_decision_discharge_LOS = Time_From_ED_Decision_to_Discharge,
    groups = c("Year", "Level_I_II", "Service Area", "Current Facility Name"),
    calculate_ci = "w"
  ) |>
  purrr::pluck(3) |>
  reshape_seqic_indicators() |>
  match_seqic_indicator(col = indicator, performance_col = performance) |>
  dplyr::left_join(
    seqic_indicator_9_level_risk,
    by = dplyr::join_by(Year, Level_I_II, Risk_Definition, indicator)
  ) |>
  dplyr::left_join(
    seqic_indicator_9_regions_risk,
    by = dplyr::join_by(Year, `Service Area`, Risk_Definition, indicator)
  ) |>
  dplyr::relocate(Risk_Definition, .after = `Service Area`) |>
  dplyr::mutate(
    Risk_Definition = ifelse(
      is.na(Risk_Definition),
      "Missing",
      Risk_Definition
    ),
    Risk_Definition = factor(
      Risk_Definition,
      levels = c(
        "Low",
        "Moderate",
        "High",
        "Missing"
      )
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

# Agency-specific - activations and risk group
seqic_indicator_9_results_activations_risk <- trauma_2020_2024 |>
  traumar::seqic_indicator_9(
    level = Level,
    unique_incident_id = Unique_Incident_ID,
    transfer_out_indicator = Acute_Transfer_Out,
    transport_method = Transport_To_Your_Facility_By,
    trauma_team_activated = Trauma_Team_Activated,
    risk_group = Risk_Definition,
    ed_LOS = Length_of_Stay,
    ed_decision_LOS = Time_From_ED_Arrival_to_Decision_Minutes,
    ed_decision_discharge_LOS = Time_From_ED_Decision_to_Discharge,
    groups = c("Year", "Level_I_II", "Service Area", "Current Facility Name"),
    calculate_ci = "w"
  ) |>
  purrr::pluck(4) |>
  reshape_seqic_indicators() |>
  match_seqic_indicator(col = indicator, performance_col = performance) |>
  dplyr::left_join(
    seqic_indicator_9_level_activations_risk,
    by = dplyr::join_by(
      Year,
      Level_I_II,
      Trauma_Team_Activated,
      Risk_Definition,
      indicator
    )
  ) |>
  dplyr::left_join(
    seqic_indicator_9_regions_activations_risk,
    by = dplyr::join_by(
      Year,
      `Service Area`,
      Trauma_Team_Activated,
      Risk_Definition,
      indicator
    )
  ) |>
  dplyr::relocate(Trauma_Team_Activated, .after = `Service Area`) |>
  dplyr::relocate(Risk_Definition, .after = Trauma_Team_Activated) |>
  dplyr::mutate(
    Trauma_Team_Activated = ifelse(
      is.na(Trauma_Team_Activated),
      "Missing",
      Trauma_Team_Activated
    ),
    Trauma_Team_Activated = factor(
      Trauma_Team_Activated,
      levels = c(
        "Trauma Team Activated",
        "Trauma Team Not Activated",
        "Missing"
      )
    ),
    Risk_Definition = ifelse(
      is.na(Risk_Definition),
      "Missing",
      Risk_Definition
    ),
    Risk_Definition = factor(
      Risk_Definition,
      levels = c(
        "Low",
        "Moderate",
        "High",
        "Missing"
      )
    )
  ) |>
  dplyr::arrange(
    Year,
    `Service Area`,
    Level_I_II,
    `Current Facility Name`,
    Trauma_Team_Activated,
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

# overall results from first two list elements in the traumar output
export_seqic_data(
  agency_names = unique(trauma_2024$`Current Facility Name`),
  facility_name_col = `current facility name`,
  seqic_results = seqic_indicator_9_results,
  indicator = "indicator_9"
)

# results by risk group
export_seqic_data(
  agency_names = unique(trauma_2024$`Current Facility Name`),
  facility_name_col = `current facility name`,
  seqic_results = seqic_indicator_9_results_risk,
  indicator = "indicator_9_risk"
)

# results by activations and risk group
export_seqic_data(
  agency_names = unique(trauma_2024$`Current Facility Name`),
  facility_name_col = `current facility name`,
  seqic_results = seqic_indicator_9_results_activations_risk,
  indicator = "indicator_9_activations_risk"
)
