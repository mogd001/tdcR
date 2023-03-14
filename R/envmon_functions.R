########## ENVMON FUNCTIONS ##########
# A collection of wrapper functions for envmon sql queries and
# returning tibbles for subsequent data workflows.

# to complete documentation
get_site_information_envmon <- function(site) {
  # Function to read site information from envmon database.
  envmon_string <-
    "driver={SQL Server};server=TSRVSQL14;database=ENVMON;trusted_connection=true"
  envmon <- odbcDriverConnect(envmon_string)
  site_name_wrapped <- paste0("\'", site, "\'")
  site_information <- sqlQuery(
    envmon,
    paste0(
      "SELECT SiteID, Name, Easting, Northing, AuxName1, AuxName2, CatchmentArea, HIRDS
     FROM Site",
      " WHERE Name LIKE ",
      site_name_wrapped
    ),
    stringsAsFactors = FALSE
  ) %>%
    rename(
      site_id = SiteID,
      site = Name,
      easting = Easting,
      northing = Northing,
      first_synonym = AuxName1,
      second_synonym = AuxName2,
      catchment_area = CatchmentArea,
      hirds_data = HIRDS
    )
  # close db connection
  odbcClose(envmon)

  return(site_information)
}


get_lab_data_envmon <- function() {
  # Function to read lab data with site information for analysis from envmon database.
  envmon_string <-
    "driver={SQL Server};server=TSRVSQL14;database=ENVMON;trusted_connection=true"
  envmon <- odbcDriverConnect(envmon_string)

  site_information <- sqlQuery(envmon,
                               "SELECT SiteID, Name, Easting, Northing, AuxName1, AuxName2
     FROM Site",
                               stringsAsFactors = FALSE
  ) %>%
    rename(
      site_id = SiteID,
      site = Name,
      easting = Easting,
      northing = Northing,
      first_synonym = AuxName1,
      second_synonym = AuxName2
    )

  lab_data <- sqlQuery(envmon,
                       "SELECT ResultNumber, SiteID, TestName, TestResultNumeric, NonDetect, TestUnits,
    SampleTakenOn, SourceTable, TestNameGroup
    FROM htsAllSiteData",
                       stringsAsFactors = FALSE
  ) %>%
    rename(
      result_number = ResultNumber,
      site_id = SiteID,
      test_name = TestName,
      test_result_numeric = TestResultNumeric,
      non_detect = NonDetect,
      test_units = TestUnits,
      sample_taken_on = SampleTakenOn,
      source_table = SourceTable,
      test_name_group = TestNameGroup
    )

  # close db connection
  odbcClose(envmon)

  # join lab_data with site_information
  lab_data <- lab_data %>%
    left_join(site_information, by = "site_id")

  return(lab_data)
}


get_bore_data_envmon <- function() {
  # Function to read the bore data from envmon database.
  envmon_string <-
    "driver={SQL Server};server=TSRVSQL14;database=ENVMON;trusted_connection=true"
  envmon <- odbcDriverConnect(envmon_string)

  bore_information <- sqlQuery(envmon,
                               "SELECT BoreID, Bore_No, Bore_Sort_No, Grid_Ref, Easting, Northing, ZoneID,
    LocationDetID, Bore_Depth, Bore_Diameter_1,
    Bore_Diameter_2, Ground_Level, Ground_Level_NZVD2016, Rim_Level, Rim_Level_NZVD2016,
    Level_Book, Yield, Duration, Drawdown, Specific_Capacity, Driller, Drilling_Date, Driller_Log_ID,
    StatusID, PumpTypeID, BoreTypeID, TypeDevID,
    CasingType, CasingDiameter, CasingLength, CollarSet, Screen_1_Set,
    Screen_Type_1, Slot_Size_1, Screen_2_Set, Screen_Type_2, Slot_Size_2, Screen_3_Set, Screen_Type_3,
    Slot_Size_3, SumpSet,
    Bore_Log, Access_To_Bore, Access_To_Water, Point_Of_Measurement_To_Ground, ValNum,
    DateCreated, Verify, VerifiedBy, VerifyDate, Dead
    FROM tbl_Bores WHERE Dead IS NULL or Dead = 0",
                               stringsAsFactors = FALSE
  ) %>%
    rename(
      bore_id = BoreID,
      bore_no = Bore_No,
      bore_sort_no = Bore_Sort_No,
      grid_ref = Grid_Ref,
      easting = Easting,
      northing = Northing,
      zone_id = ZoneID,
      location_det_id = LocationDetID,
      bore_depth = Bore_Depth,
      bore_diameter_1 = Bore_Diameter_1,
      bore_diameter_2 = Bore_Diameter_2,
      ground_level = Ground_Level,
      ground_level_nzvd2016 = Ground_Level_NZVD2016,
      rim_level = Rim_Level,
      rim_level_nzvd2016 = Rim_Level_NZVD2016,
      level_book = Level_Book,
      yield = Yield,
      duration = Duration,
      drawdown = Drawdown,
      specific_capacity = Specific_Capacity,
      driller = Driller,
      drilling_date = Drilling_Date,
      driller_log_id = Driller_Log_ID,
      status_id = StatusID,
      pump_type_id = PumpTypeID,
      bore_type_id = BoreTypeID,
      type_dev_id = TypeDevID,
      casing_type = CasingType,
      casing_diameter = CasingDiameter,
      casing_length = CasingLength,
      collar_set = CollarSet,
      screen_1_set = Screen_1_Set,
      screen_type_1 = Screen_Type_1,
      slot_size_1 = Slot_Size_1,
      screen_2_set = Screen_2_Set,
      screen_type_2 = Screen_Type_2,
      slot_size_2 = Slot_Size_2,
      screen_3_set = Screen_3_Set,
      screen_type_3 = Screen_Type_3,
      slot_size_3 = Slot_Size_3,
      sump_set = SumpSet,
      bore_log = Bore_Log,
      access_to_bore = Access_To_Bore,
      access_to_walker = Access_To_Water,
      point_of_measurement_to_ground = Point_Of_Measurement_To_Ground,
      val_num = ValNum,
      date_created = DateCreated,
      verify = Verify,
      verified_by = VerifiedBy,
      verifiy_date = VerifyDate,
      dead = Dead
    )

  site_bore <- sqlQuery(
    envmon,
    "SELECT BoreID, SiteID
    FROM vw_BoreSite"
  ) %>%
    rename(
      bore_id = BoreID,
      site_id = SiteID
    )

  odbcClose(envmon)

  # get aquifer information
  ncs_string <-
    "driver={SQL Server};server=TSRVSQL14;database=NCS;trusted_connection=true"
  ncs <- odbcDriverConnect(ncs_string)

  bore_aquifer <- sqlQuery(
    ncs,
    "SELECT BoreID, PrimaryUse, AquiferName FROM GIS.vw_LawaBores"
  ) %>%
    rename(
      bore_id = BoreID,
      primary_use = PrimaryUse,
      aquifer = AquiferName,
    )

  odbcClose(ncs)

  bore_information <- bore_information %>%
    left_join(site_bore, by = "bore_id") %>%
    left_join(bore_aquifer, by = "bore_id")

  return(bore_information)
}


tabulate_hirds_data_string <- function(data_string) {
  # Function to map HIRDS string saved in ENVMON database to tibble.
  col_names <-
    c(
      "ari",
      "dur_10min",
      "dur_20min",
      "dur_30min",
      "dur_1hour",
      "dur_2hour",
      "dur_6hour",
      "dur_12hour",
      "dur_1day",
      "dur_2day",
      "dur_3day",
      "dur_4day",
      "dur_5day"
    )

  # split string according to \r\n rows, then convert rows to double and create dataframe.
  rows <- strsplit(data_string, "\r\n")[[1]]

  n_col <- 0
  for (row in rows) {
    x <- as.double(unlist(strsplit(row, ",")))
    if (n_col == 0) {
      n_col <- length(x)
      v <- x
    } else {
      v <- append(v, x)
    }
  }
  m <- matrix(v, nrow = length(v) / n_col, ncol = n_col, byrow = TRUE)

  # add empty columns to m if longer return periods are not defined
  if (n_col < length(col_names)) {
    for (i in 1:(length(col_names) - n_col)) {
      m <- cbind(m, rep(NA, length(m[, 1])))
    }
  }

  hirds_df <- as_tibble(m) %>%
    setNames(col_names)

  return(hirds_df)
}
