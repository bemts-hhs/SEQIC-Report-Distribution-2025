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
  dplyr::mutate(Alive = ifelse(Death, 0, 1)) |>
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
  dplyr::arrange(`Current Facility Name`, Trauma_Team_Activated) |>
  dplyr::relocate(Trauma_Team_Activated, .after = `Current Facility Name`)

### Relative Mortality Metric ####
rm_summary_results <- trauma_2020_2024_clean |>
  tidyr::nest(data = -c(Year, `Current Facility Name`)) |>
  dplyr::mutate(
    results = purrr::map(
      data,
      ~ dynamic_rm_bin_summary(
        data = .x,
        Ps_col = Probability_of_Survival_Calc,
        outcome_col = Alive,
        group_vars = NULL,
        n_samples = 1000,
        bootstrap_ci = TRUE,
        seed = 10232015
      )
    )
  ) |>
  dplyr::select(-data) |>
  tidyr::unnest(results) |>
  dplyr::arrange(`Current Facility Name`, Year, bin_number)

### EXPORT ####

# W, M, and Z scores grouped by year
export_seqic_data(
  agency_names = unique(trauma_2024$`Current Facility Name`),
  facility_name_col = `Current Facility Name`,
  seqic_results = trauma_performance_result_years,
  indicator = "_wmz_years"
)

# W, M, and Z scores grouped by mechanism of injury
export_seqic_data(
  agency_names = unique(trauma_2024$`Current Facility Name`),
  facility_name_col = `Current Facility Name`,
  seqic_results = trauma_performance_result_mech,
  indicator = "_wmz_mechanism"
)

# W, M, and Z scores grouped by whether a trauma alert was called
export_seqic_data(
  agency_names = unique(trauma_2024$`Current Facility Name`),
  facility_name_col = `Current Facility Name`,
  seqic_results = trauma_performance_result_tta,
  indicator = "_wmz_tta"
)

# RMM statistics by year
export_seqic_data(
  agency_names = unique(trauma_2024$`Current Facility Name`),
  facility_name_col = `Current Facility Name`,
  seqic_results = rm_summary_results,
  indicator = "_rmm_years"
)
