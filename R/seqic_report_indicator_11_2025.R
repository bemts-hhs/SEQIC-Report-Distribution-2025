###_____________________________________________________________________________
### SEQIC Indicator Calculations 2025 - Indicator 11 Unnecessary transfers
### To run this script, you must first have ran seqic_data_load.R and
### seqic_report_setup_2025.R.  Without running these, some functions and the
### data needed to use this script will not be available in your global
### environment
###_____________________________________________________________________________

### SEQIC indicator 11 ####

# Regions
seqic_indicator_11_regions <- trauma_2020_2024 |>
  traumar::seqic_indicator_11(
    level = Level,
    unique_incident_id = Unique_Incident_ID,
    transfer_out_indicator = Acute_Transfer_Out,
    receiving_indicator = Receiving,
    iss = ISS_Calculated,
    ed_LOS = Length_of_Stay,
    groups = c("Year", "Service Area"),
    calculate_ci = "w"
  ) |>
  format_seqic_comparison(type = "region") |>
  dplyr::select(-c(`lower ci`, `upper ci`))

# Level
seqic_indicator_11_level <- trauma_2020_2024 |>
  traumar::seqic_indicator_11(
    level = Level,
    unique_incident_id = Unique_Incident_ID,
    transfer_out_indicator = Acute_Transfer_Out,
    receiving_indicator = Receiving,
    iss = ISS_Calculated,
    ed_LOS = Length_of_Stay,
    groups = c("Year", "Level_I_II"),
    calculate_ci = "w"
  ) |>
  format_seqic_comparison(type = "level") |>
  dplyr::select(-c(`lower ci`, `upper ci`))

# Agency-specific
seqic_indicator_11_results <- trauma_2020_2024 |>
  traumar::seqic_indicator_11(
    level = Level,
    unique_incident_id = Unique_Incident_ID,
    transfer_out_indicator = Acute_Transfer_Out,
    receiving_indicator = Receiving,
    iss = ISS_Calculated,
    ed_LOS = Length_of_Stay,
    groups = c("Year", "Level_I_II", "Service Area", "Current Facility Name"),
    calculate_ci = "w"
  ) |>
  reshape_seqic_indicators() |>
  match_seqic_indicator(col = indicator, performance_col = performance) |>
  join_comparison_data(
    data_level = seqic_indicator_11_level,
    data_region = seqic_indicator_11_regions
  )

###_____________________________________________________________________________
### State, Region, and Verification Level Performance Reporting
###_____________________________________________________________________________

# state level
seqic_indicator_11_results_state <- trauma_2020_2024 |>
  traumar::seqic_indicator_11(
    level = Level,
    unique_incident_id = Unique_Incident_ID,
    transfer_out_indicator = Acute_Transfer_Out,
    receiving_indicator = Receiving,
    iss = ISS_Calculated,
    ed_LOS = Length_of_Stay,
    groups = c("Year"),
    calculate_ci = "w"
  ) |>
  reshape_seqic_indicators() |>
  match_seqic_indicator(col = indicator, performance_col = performance) |>
  dplyr::mutate(dplyr::across(
    c(performance, goal, `lower ci`, `upper ci`),
    ~ ifelse(
      is.na(.),
      NA_real_,
      traumar::pretty_percent(variable = ., n_decimal = 2)
    )
  ))

# state level - by age group
seqic_indicator_11_results_state_age <- trauma_2020_2024 |>
  traumar::seqic_indicator_11(
    level = Level,
    unique_incident_id = Unique_Incident_ID,
    transfer_out_indicator = Acute_Transfer_Out,
    receiving_indicator = Receiving,
    iss = ISS_Calculated,
    ed_LOS = Length_of_Stay,
    groups = c("Year", "Age_Range"),
    calculate_ci = "w"
  ) |>
  reshape_seqic_indicators() |>
  match_seqic_indicator(col = indicator, performance_col = performance) |>
  dplyr::mutate(dplyr::across(
    c(performance, goal, `lower ci`, `upper ci`),
    ~ ifelse(
      is.na(.),
      NA_real_,
      traumar::pretty_percent(variable = ., n_decimal = 2)
    )
  )) |>
  dplyr::mutate(
    Age_Range = ifelse(is.na(Age_Range), "Missing", Age_Range),
    Age_Range = stringr::str_replace(
      string = Age_Range,
      pattern = "-",
      replacement = " to "
    ),
    Age_Range = factor(
      Age_Range,
      levels = c(
        "0 to 9",
        "10 to 19",
        "20 to 29",
        "30 to 39",
        "40 to 49",
        "50 to 59",
        "60 to 69",
        "70 to 79",
        "80 to 89",
        "90 to 99",
        "100+",
        "Missing"
      )
    )
  ) |>
  dplyr::arrange(Year, Age_Range)

# service areas
seqic_indicator_11_results_service_areas <- trauma_2020_2024 |>
  traumar::seqic_indicator_11(
    level = Level,
    unique_incident_id = Unique_Incident_ID,
    transfer_out_indicator = Acute_Transfer_Out,
    receiving_indicator = Receiving,
    iss = ISS_Calculated,
    ed_LOS = Length_of_Stay,
    groups = c("Year", "Service Area"),
    calculate_ci = "w"
  ) |>
  reshape_seqic_indicators() |>
  match_seqic_indicator(col = indicator, performance_col = performance) |>
  dplyr::mutate(dplyr::across(
    c(performance, goal, `lower ci`, `upper ci`),
    ~ ifelse(
      is.na(.),
      NA_real_,
      traumar::pretty_percent(variable = ., n_decimal = 2)
    )
  ))

# trauma center verification levels
seqic_indicator_11_results_verification <- trauma_2020_2024 |>
  traumar::seqic_indicator_11(
    level = Level,
    unique_incident_id = Unique_Incident_ID,
    transfer_out_indicator = Acute_Transfer_Out,
    receiving_indicator = Receiving,
    iss = ISS_Calculated,
    ed_LOS = Length_of_Stay,
    groups = c("Year", "Level_I_II"),
    calculate_ci = "w"
  ) |>
  reshape_seqic_indicators() |>
  match_seqic_indicator(col = indicator, performance_col = performance) |>
  dplyr::mutate(dplyr::across(
    c(performance, goal, `lower ci`, `upper ci`),
    ~ ifelse(
      is.na(.),
      NA_real_,
      traumar::pretty_percent(variable = ., n_decimal = 2)
    )
  ))

### Export ####

# hospital reporting
export_seqic_data(
  agency_names = unique(trauma_2024$`Current Facility Name`),
  facility_name_col = `current facility name`,
  seqic_results = seqic_indicator_11_results,
  indicator = "indicator_11"
)

# state level reporting
readr::write_csv(
  x = seqic_indicator_11_results_state,
  file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/SEQIC Facility Reports/2024/state/11/seqic_indicator_11_results_state.csv"
)

# state level by age reporting
readr::write_csv(
  x = seqic_indicator_11_results_state_age,
  file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/SEQIC Facility Reports/2024/state/11/seqic_indicator_11_results_state_age.csv"
)

# service area level reporting
readr::write_csv(
  x = seqic_indicator_11_results_service_areas,
  file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/SEQIC Facility Reports/2024/state/11/seqic_indicator_11_results_service_areas.csv"
)

# verification level reporting
readr::write_csv(
  x = seqic_indicator_11_results_verification,
  file = "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/SEQIC Facility Reports/2024/state/11/seqic_indicator_11_results_verification.csv"
)
