###_____________________________________________________________________________
### Loading data for the distribution of SEQIC reports in 2025
### All data for these reports will be 2020 - 2024 with 2024 being the 'current'
### year. Date of data download/processing from the Iowa Trauma Registry:
### 5/23/2025 for 2024 data, for 2020-2023 data - 5/1/2025
###_____________________________________________________________________________

# Secure creds and path
path_2020 <- Sys.getenv("FILE_PATH_2020")
path_2021 <- Sys.getenv("FILE_PATH_2021")
path_2022 <- Sys.getenv("FILE_PATH_2022")
path_2023 <- Sys.getenv("FILE_PATH_2023")
path_2024 <- Sys.getenv("FILE_PATH_2024")

# load files from source
trauma_2020 <-
  readr::read_csv(
    path_2020
  )
trauma_2021 <-
  readr::read_csv(
    path_2021
  )
trauma_2022 <-
  readr::read_csv(
    path_2022
  )
trauma_2023 <-
  readr::read_csv(
    path_2023
  )
trauma_2024 <-
  readr::read_csv(
    path_2024
  )

# Get data into one file for all years for reporting across years
# Although identical processing workflows were followed for all files, the zip
# codes seem to get formatted differently between years, fix those
trauma_2020_2024 <- dplyr::bind_rows(
  trauma_2020 |>
    dplyr::mutate(dplyr::across(
      tidyselect::matches("_zip$|_fips$"),
      ~ as.character(.)
    )),
  trauma_2021 |>
    dplyr::mutate(dplyr::across(
      tidyselect::matches("_zip$|_fips$"),
      ~ as.character(.)
    )),
  trauma_2022 |>
    dplyr::mutate(dplyr::across(
      tidyselect::matches("_zip$|_fips$"),
      ~ as.character(.)
    )),
  trauma_2023 |>
    dplyr::mutate(dplyr::across(
      tidyselect::matches("_zip$|_fips$"),
      ~ as.character(.)
    )),
  trauma_2024 |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::matches("_zip$|_fips$"),
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
) |>
  dplyr::mutate(
    Level_I_II = ifelse(
      `Current Facility Name` ==
        "UnityPoint Health - Trinity Regional Medical Center" &
        ED_Acute_Care_Admission_Date < as.Date("2018-04-01"),
      "Level III",
      ifelse(
        `Current Facility Name` == "Mahaska Health" &
          ED_Acute_Care_Admission_Date < as.Date("2018-07-01"),
        "Level III",
        ifelse(
          `Current Facility Name` == "CHI Health Mercy Council Bluffs" &
            ED_Acute_Care_Admission_Date < as.Date("2018-10-01"),
          "Level IV",
          ifelse(
            `Current Facility Name` ==
              "UnityPoint Health - Marshalltown Hospital" &
              ED_Acute_Care_Admission_Date < as.Date("2018-12-01"),
            "Level III",
            ifelse(
              `Current Facility Name` ==
                "UnityPoint Health - Grinnell Regional Medical Center" &
                ED_Acute_Care_Admission_Date < as.Date("2019-01-01"),
              "Level III",
              ifelse(
                `Current Facility Name` ==
                  "MercyOne North Iowa Medical Center" &
                  ED_Acute_Care_Admission_Date < as.Date("2020-07-01"),
                "Level I & II",
                ifelse(
                  `Current Facility Name` == "Pella Regional Health Center" &
                    ED_Acute_Care_Admission_Date < as.Date("2020-08-01"),
                  "Level III",
                  ifelse(
                    `Current Facility Name` ==
                      "Genesis Medical Center, Davenport" &
                      ED_Acute_Care_Admission_Date < as.Date("2021-10-01"),
                    "Level I & II",
                    ifelse(
                      `Current Facility Name` ==
                        "Southeast Iowa Regional Medical Center, West Burlington" &
                        ED_Acute_Care_Admission_Date < as.Date("2021-12-01"),
                      "Level III",
                      ifelse(
                        `Current Facility Name` ==
                          "Southeast Iowa Regional Medical Center, West Burlington" &
                          ED_Acute_Care_Admission_Date >= as.Date("2024-12-01"),
                        "Level III",
                        ifelse(
                          `Current Facility Name` ==
                            "Southeast Iowa Regional Medical Center, West Burlington" &
                            ED_Acute_Care_Admission_Date >=
                              as.Date("2021-12-01") &
                            ED_Acute_Care_Admission_Date <
                              as.Date("2024-12-01"),
                          "Level IV",
                          ifelse(
                            `Current Facility Name` ==
                              "UnityPoint Health - St. Luke's Hospital, Sioux CIty" &
                              ED_Acute_Care_Admission_Date <
                                as.Date("2024-10-01"),
                            "Level I & II",
                            Level_I_II
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
