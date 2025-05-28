###_____________________________________________________________________________
### Loading data for the distribution of SEQIC reports in 2025
### All data for these reports will be 2020 - 2024 with 2024 being the 'current'
### year. Date of data download/processing from the Iowa Trauma Registry:
### 5/23/2025 for 2024 data, for 2020-2023 data - 5/1/2025
###_____________________________________________________________________________

# load files from source
trauma_2020 <-
  readr::read_csv(
    "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/TRAUMA DATA/SEQIC/trauma_2020.csv"
  )
trauma_2021 <-
  readr::read_csv(
    "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/TRAUMA DATA/SEQIC/trauma_2021.csv"
  )
trauma_2022 <-
  readr::read_csv(
    "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/TRAUMA DATA/SEQIC/trauma_2022.csv"
  )
trauma_2023 <-
  readr::read_csv(
    "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/TRAUMA DATA/SEQIC/trauma_2023.csv"
  )
trauma_2024 <-
  readr::read_csv(
    "C:/Users/nfoss0/OneDrive - State of Iowa HHS/Analytics/BEMTS/TRAUMA DATA/SEQIC/trauma_2024.csv"
  )

# Get data into one file for all years for reporting across years
# Although identical processing workflows were followed for all files, the zip
# codes seem to get formatted differently between years, fix those
trauma_2020_2024 <- dplyr::bind_rows(
  trauma_2020 |>
    dplyr::mutate(dplyr::across(
      tidyselect::matches("_zip$"),
      ~ as.character(.)
    )),
  trauma_2021 |>
    dplyr::mutate(dplyr::across(
      tidyselect::matches("_zip$"),
      ~ as.character(.)
    )),
  trauma_2022 |>
    dplyr::mutate(dplyr::across(
      tidyselect::matches("_zip$"),
      ~ as.character(.)
    )),
  trauma_2023 |>
    dplyr::mutate(dplyr::across(
      tidyselect::matches("_zip$"),
      ~ as.character(.)
    )),
  trauma_2024 |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::matches("_zip$"),
        ~ as.character(.)
      ),
      Level = ifelse(
        Facility_Name ==
          "Southeast Iowa Regional Medical Center, West Burlington",
        "III",
        Level
      ),
      Level_I_II = ifelse(
        Facility_Name ==
          "Southeast Iowa Regional Medical Center, West Burlington",
        "Level III",
        Level_I_II
      )
    )
)
