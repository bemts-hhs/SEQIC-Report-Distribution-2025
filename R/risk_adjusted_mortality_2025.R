###_____________________________________________________________________________
### Produce the Risk Adjusted Mortality Metrics for Each Trauma Center
### To run this script, you must first have ran seqic_data_load.R and
### seqic_report_setup_2025.R.  Without running these, some functions and the
### data needed to use this script will not be available in your global
### environment
###_____________________________________________________________________________

### W, M, and Z Scores #####

# given that traumar does not provide a grouping structure for the
# traumar::trauma_performance() function, we will use tidyr::nest() to
# do the grouping

# Clean the data and prepare for TRISS analyses
trauma_2020_2024_clean <- trauma_2020_2024 |>
  dplyr::filter(!is.na(Probability_of_Survival_Calc)) |>
  dplyr::group_by(Year) |>
  dplyr::distinct(Unique_Incident_ID, .keep_all = TRUE) |>
  dplyr::ungroup()

# iterate over groups - Year and facility
trauma_performance_result_years <- trauma_2020_2024_clean |>
  tidyr::nest(data = -c(Year, `Current Facility Name`)) |>
  dplyr::mutate(
    results = purrr::map(
      data,
      ~ traumar::trauma_performance(
        df = .x,
        Ps_col = Probability_of_Survival_Calc,
        outcome_col = Death,
        z_method = "survival"
      )
    )
  ) |>
  dplyr::select(-data) |>
  tidyr::unnest(results) |>
  dplyr::arrange(`Current Facility Name`, Year)

# iterate over groups - facility, mechanism of injury do not add year as a
# stratum given that some counts will be very small in groups
trauma_performance_result_mech <- trauma_2020_2024_clean |>
  tidyr::nest(data = -c(`Current Facility Name`, Trauma_Type)) |>
  dplyr::mutate(
    results = purrr::map(
      data,
      ~ traumar::trauma_performance(
        df = .x,
        Ps_col = Probability_of_Survival_Calc,
        outcome_col = Death,
        z_method = "survival"
      )
    )
  ) |>
  dplyr::select(-data) |>
  tidyr::unnest(results) |>
  dplyr::arrange(`Current Facility Name`, Trauma_Type) |>
  dplyr::relocate(Trauma_Type, .after = `Current Facility Name`) |>
  dplyr::mutate(
    Trauma_Type = ifelse(is.na(Trauma_Type), "Missing", Trauma_Type)
  )

# iterate over groups - facility, mechanism of injury do not add year as a
# stratum given that some counts will be very small in groups
trauma_performance_result_tta <- trauma_2020_2024_clean |>
  tidyr::nest(
    data = -c(`Current Facility Name`, Trauma_Team_Activated)
  ) |>
  dplyr::mutate(
    results = purrr::map(
      data,
      ~ traumar::trauma_performance(
        df = .x,
        Ps_col = Probability_of_Survival_Calc,
        outcome_col = Death,
        z_method = "survival"
      )
    )
  ) |>
  dplyr::select(-data) |>
  tidyr::unnest(results) |>
  dplyr::arrange(`Current Facility Name`, Year, Trauma_Team_Activated) |>
  dplyr::relocate(Trauma_Team_Activated, .after = `Current Facility Name`)

### Relative Mortality Metric ####
