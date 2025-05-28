###_____________________________________________________________________________
### SEQIC Indicator Calculations 2025
### To run this script, you must first have ran seqic_data_load.R and
### seqic_report_setup_2025.R.  Without running these, some functions and the
### data needed to use this script will not be available in your global
### environment
###_____________________________________________________________________________

### SEQIC indicator 1 ####

# Regions
seqic_indicator_1_regions <- trauma_2020_2024 |>
  traumar::seqic_indicator_1(
    trauma_team_activation_level = Trauma_Team_Activation_Level,
    trauma_team_physician_service_type = Trauma_Team_Physician_Service_Type,
    level = Level,
    unique_incident_id = Unique_Incident_ID,
    response_time = Indicator_1_Time,
    trauma_team_activation_provider = Trauma_Team_Activation_Provider,
    groups = c("Year", "Service Area")
  ) |>
  format_seqic_comparison(type = "region")

# Level
seqic_indicator_1_level <- trauma_2020_2024 |>
  traumar::seqic_indicator_1(
    trauma_team_activation_level = Trauma_Team_Activation_Level,
    trauma_team_physician_service_type = Trauma_Team_Physician_Service_Type,
    level = Level,
    unique_incident_id = Unique_Incident_ID,
    response_time = Indicator_1_Time,
    trauma_team_activation_provider = Trauma_Team_Activation_Provider,
    groups = c("Year", "Level_I_II")
  ) |>
  format_seqic_comparison(type = "level")

# Agency-specific
seqic_indicator_1_results <- trauma_2020_2024 |>
  traumar::seqic_indicator_1(
    trauma_team_activation_level = Trauma_Team_Activation_Level,
    trauma_team_physician_service_type = Trauma_Team_Physician_Service_Type,
    level = Level,
    unique_incident_id = Unique_Incident_ID,
    response_time = Indicator_1_Time,
    trauma_team_activation_provider = Trauma_Team_Activation_Provider,
    groups = c("Year", "Level_I_II", "Service Area", "Current Facility Name"),
    calculate_ci = "w"
  ) |>
  reshape_seqic_indicators() |>
  match_seqic_indicator(col = indicator, performance_col = performance) |>
  join_comparison_data(
    data_level = seqic_indicator_1_level,
    data_region = seqic_indicator_1_regions
  )

### Export ####
export_seqic_data(
  agency_names = unique(trauma_2024$`Current Facility Name`),
  facility_name_col = `current facility name`,
  seqic_results = seqic_indicator_1_results,
  indicator = "indicator_1"
)
