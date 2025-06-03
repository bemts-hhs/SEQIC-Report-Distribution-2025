###_____________________________________________________________________________
### SEQIC Indicator Calculations 2025 - Indicator 5 Substance Use
### To run this script, you must first have ran seqic_data_load.R and
### seqic_report_setup_2025.R.  Without running these, some functions and the
### data needed to use this script will not be available in your global
### environment
###_____________________________________________________________________________

### SEQIC indicator 5 ####

# Regions
seqic_indicator_5_regions <- trauma_2020_2024 |>
  traumar::seqic_indicator_5(
    level = Level,
    unique_incident_id = Unique_Incident_ID,
    blood_alcohol_content = Blood_Alcohol_Content,
    drug_screen = ED_Acute_Care_Drugsuspected_or_confirmed_by_test_Drug_Screen,
    groups = c("Year", "Service Area"),
    calculate_ci = "w"
  ) |>
  format_seqic_comparison(type = "region") |>
  dplyr::select(-c(`lower ci`, `upper ci`))

# Level
seqic_indicator_5_level <- trauma_2020_2024 |>
  traumar::seqic_indicator_5(
    level = Level,
    unique_incident_id = Unique_Incident_ID,
    blood_alcohol_content = Blood_Alcohol_Content,
    drug_screen = ED_Acute_Care_Drugsuspected_or_confirmed_by_test_Drug_Screen,
    groups = c("Year", "Level_I_II"),
    calculate_ci = "w"
  ) |>
  format_seqic_comparison(type = "level") |>
  dplyr::select(-c(`lower ci`, `upper ci`))

# Agency-specific
seqic_indicator_5_results <- trauma_2020_2024 |>
  traumar::seqic_indicator_5(
    level = Level,
    unique_incident_id = Unique_Incident_ID,
    blood_alcohol_content = Blood_Alcohol_Content,
    drug_screen = ED_Acute_Care_Drugsuspected_or_confirmed_by_test_Drug_Screen,
    groups = c("Year", "Level_I_II", "Service Area", "Current Facility Name"),
    calculate_ci = "w"
  ) |>
  reshape_seqic_indicators() |>
  match_seqic_indicator(col = indicator, performance_col = performance) |>
  join_comparison_data(
    data_level = seqic_indicator_5_level,
    data_region = seqic_indicator_5_regions
  )

### Export ####
export_seqic_data(
  agency_names = unique(trauma_2024$`Current Facility Name`),
  facility_name_col = `current facility name`,
  seqic_results = seqic_indicator_5_results,
  indicator = "indicator_5"
)
