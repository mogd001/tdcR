########## HILLTOP SERVER ##########
# A collection of wrapper functions for calling hilltop server commands and
# returning tibbles for subsequent workflows.

#' Get Hilltop Sites
#'
#' @description Function to get Sites from Hilltop Server.
#' @param endpoint A url for the Hilltop endpoint.
#' @param latlong A logical, TRUE returns EPSG:4326 (WGS) coordinates , EPSG:2193 (NZTM) otherwise.
#' @return A tibble of sites and their corresponding coordinates.
#' @examples
#' get_sites("http://envdata.tasman.govt.nz/data.hts?")
#' get_sites("http://envdata.tasman.govt.nz/data.hts?", latlong = FALSE)
get_sites <- function(endpoint = "http://envdata.tasman.govt.nz/data.hts?",
                      latlong = TRUE) {

  url <- paste0(endpoint, "Service=Hilltop&Request=SiteList")

  if (latlong) {
    url <- paste0(url, "&Location=LatLong")
  } else { # return easting and northing
    url <- paste0(url, "&Location=Yes")
  }

  url <- gsub(" ", "%20", url)
  print(url)

  hilltop_data <- read_xml(url)
  sites <- xml_find_all(hilltop_data, "Site") %>% xml_attr("Name")

  hilltop_df <- hilltop_data %>%
    as_list() %>%
    as_tibble() %>%
    slice(1:n() - 1) %>% # drop last row
    slice((n() - length(sites) + 1):n()) %>% # drop first non-site rows
    mutate(site = sites) %>%
    unnest_longer("HilltopServer") %>%
    transmute(
      site = site,
      src = HilltopServer_id,
      data = as.numeric(unlist(HilltopServer))
    ) %>%
    filter(
      !is.na(src)
    ) %>%
    pivot_wider(
      names_from = src,
      values_from = data
    ) %>%
    rename_all(tolower)
}


#' Get Hilltop Measurements
#'
#' @description Function to get Measurements from Hilltop Server.
#' @param endpoint A url for the Hilltop endpoint.
#' @param site A string for the site so to return only measurements associated with that specific site.
#' @return A tibble of measurements.
#' @examples
#' get_measurements("http://envdata.tasman.govt.nz/data.hts?")
#' get_measurements("http://envdata.tasman.govt.nz/data.hts?", site = "HY Anatoki at Happy Sams")
get_measurements <- function(endpoint = "http://envdata.tasman.govt.nz/data.hts?",
                             site = NA) {

  url <- paste0(endpoint, "Service=Hilltop&Request=MeasurementList")

  if (!is.na(site)) {
    url <- paste0(url, "&Site=", site)
  }

  url <- gsub(" ", "%20", url)
  print(url)

  hilltop_data <- read_xml(url)
  if (is.na(site)) {
    measurements <- xml_find_all(hilltop_data, "Measurement") %>% xml_attr("Name")
  } else {
    measurements <- xml_find_all(hilltop_data, "DataSource") %>% xml_attr("Name")
  }
  hilltop_df <- tibble(measurement = measurements)
}


#' Get Hilltop Collections
#'
#' @description Function to get Collections from Hilltop Server.
#' @param endpoint A url for the Hilltop endpoint.
#' @return A tibble of collections.
#' @examples
#' get_collections("http://envdata.tasman.govt.nz/data.hts?")
get_collections <- function(endpoint = "http://envdata.tasman.govt.nz/data.hts?") {

  url <- paste0(endpoint, "Service=Hilltop&Request=CollectionList")

  url <- gsub(" ", "%20", url)
  print(url)

  hilltop_data <- read_xml(url)
  collections <- xml_find_all(hilltop_data, "Collection") %>% xml_attr("Name")

  hilltop_df <- hilltop_data %>%
    as_list() %>%
    as_tibble() %>%
    mutate(collection = collections) %>%
    unnest_longer("HilltopProject") %>%
    filter(HilltopProject_id == "Item") %>% # rows without an "Item" have no sites in the collection
    transmute(collection = collection, data = HilltopProject) %>%
    unnest_wider("data") %>%
    unnest(cols = names(.)) %>%
    unnest(cols = names(.)) %>%
    transmute(
      collection = collection,
      site = SiteName,
      measurement = Measurement
    )
}


#' Interval function to correct for Hilltop's "right-bound" datetimes.
#'
#' @param interval a string representing the interval e.g. "1 months".
#' @noRd
convert_interval_to_offset <- function(interval) {

  if (!is.character(interval)) {
    interval_offset <- NA
  } else {
    if (grepl("minute", interval)) {
      c(n, int) %<-% unlist(str_split(interval, " "))
      interval_offset <- minutes(n)
    } else if (grepl("hour", interval)) {
      interval_offset <- hours(1)
    } else if (grepl("day", interval)) {
      interval_offset <- days(1)
    } else if (grepl("month", interval)) {
      interval_offset <- months(1)
    } else if (grepl("year", interval)) {
      interval_offset <- years(1)
    } else {
      interval_offset <- NA
    }
  }

  interval_offset
}


#' Get Hilltop Data for a Collection
#'
#' @description Function to get Data for a Collection from Hilltop Server.
#' @param endpoint A url for the Hilltop endpoint.
#' @param collection A string for the collection.
#' @param method A string for the statistic computed, currently only tested for Total, Average and Extrema.
#' @param interval A string for the interval over which the "method" is calculated, e.g. "1 day".
#' @param from A string for the start date.
#' @param to A string for the end date.
#' @param time_interval A string for the relative time, e.g. P1Y (Past 1 year). See Hilltop Server documentation
#' @param alignment See Hilltop Server documentation.
#' @return A tibble containing the data for each site comprising the collection.
#' @examples
#' get_data_collection("http://envdata.tasman.govt.nz/data.hts?", collection = "Rainfall",
#'       method = "Total", interval = "1 hour", from = "20220101", to = "202207031")
#' get_data_collection("http://envdata.tasman.govt.nz/data.hts?", collection = "ActiveFlowSites",
#'       method = "Extrema", interval = "15 minutes", from = "20220101", to = "202207031")
get_data_collection <- function(endpoint = "http://envdata.tasman.govt.nz/data.hts?",
                                collection,
                                method = NA,
                                interval = "1 day",
                                from = NA,
                                to = NA,
                                time_interval = NA,
                                alignment = "00:00") {

  if (is.na(method)) {
    url <- ""
    interval_offset <- 0
  } else {
    url <- paste0(
      "&Method=", method,
      "&Interval=", interval,
      "&Alignment=", alignment
    )
    interval_offset <- convert_interval_to_offset(interval)
  }

  if (!is.na(time_interval)) {
    url <- paste0(url, "&TimeInterval=", time_interval)
  } else if (!is.na(from) | !is.na(to)) {
    url <- paste0(url, "&From=", from, "&To=", to)
  } else {
    url <- paste0(url, "&TimeInterval=", "P1D")
  }

  url <-
    paste0(
      endpoint,
      "Service=Hilltop&Request=GetData&Collection=",
      collection,
      url
    )

  url <- gsub(" ", "%20", url)
  print(url)

  hilltop_data <- read_xml(url)
  sites <- xpathSApply(xmlParse(hilltop_data), "//Measurement", xmlGetAttr, "SiteName")

  hilltop_df <- hilltop_data %>%
    as_list() %>%
    as_tibble() %>%
    slice(-1) %>% # drop first node (Agency not required)
    unnest_longer("Hilltop") %>%
    filter(Hilltop_id == "Data") %>%
    select("Hilltop") %>%
    mutate(site = sites) %>%
    unnest(cols = names(.)) %>%
    unnest_wider("Hilltop") %>%
    unnest(cols = names(.)) %>%
    unnest(cols = names(.)) %>%
    transmute(
      site = site,
      datetime = ymd_hms(T, tz = "Etc/GMT+12") - interval_offset,
      value = as.numeric(I1)
    ) %>%
    mutate(
      year = year(datetime),
      yday = yday(datetime),
      month = month(datetime, label = TRUE),
      year_month = floor_date(datetime, unit = "month"),
      day = floor_date(datetime, unit = "day"),
      day_hour = floor_date(datetime, unit = "hour"),
      date = as_date(datetime),
      time = as_hms(datetime)
    )
}


#' Get Hilltop Data for a Site and Measurement
#'
#' @description Function to get Data for a Site and Measurement from Hilltop Server.
#' @param endpoint A url for the Hilltop endpoint.
#' @param site A string for the site.
#' @param measurement A string for the measurement.
#' @param method A string for the statistic computed, currently only tested for Total, Average and Extrema.
#' @param interval A string for the interval over which the "method" is calculated, e.g. "1 day".
#' @param from A string for the start date.
#' @param to A string for the end date.
#' @param time_interval A string for the relative time, e.g. P1Y (Past 1 year). See Hilltop Server documentation
#' @param alignment See Hilltop Server documentation.
#' @return A tibble containing the data for each site comprising the collection.
#' @examples
#' get_data_site_measurement("http://envdata.tasman.govt.nz/data.hts?",
#'       site = "AQ Richmond Central at Plunket", measurement = "PM2.5 (24 Hour)",
#'       interval = "1 hour", from = "20220101", to = "202207031")
#' get_data_site_measurement("http://envdata.tasman.govt.nz/data.hts?",
#'       site = "HY Richmond Weather at TDC Roof", measurement = "Rainfall",
#'       method = "Total", interval = "15 minutes", from = "20220101", to = "20220731")
get_data_site_measurement <- function(endpoint = "http://envdata.tasman.govt.nz/data.hts?",
                                      site,
                                      measurement,
                                      method = NA,
                                      interval = "1 day",
                                      from = NA,
                                      to = NA,
                                      time_interval = NA,
                                      alignment = "00:00") {

  url <- paste0(
    endpoint,
    "Service=Hilltop&Request=GetData",
    "&Site=", site,
    "&Measurement=", measurement
  )

  if (is.na(method)) {
    interval_offset <- 0
  } else {
    url <- paste0(url,
      "&Method=", method,
      "&Interval=", interval,
      "&Alignment=", alignment
    )
    interval_offset <- convert_interval_to_offset(interval)
  }

  if (!is.na(time_interval)) {
    url <- paste0(url, "&TimeInterval=", time_interval)
  } else if (!is.na(from) | !is.na(to)) {
    url <- paste0(url, "&From=", from, "&To=", to)
  } else {
    url <- paste0(url, "&TimeInterval=", "P1D")
  }

  url <- gsub(" ", "%20", url)
  print(url)

  hilltop_data <- read_xml(url)

  hilltop_df <- hilltop_data %>%
    as_list() %>%
    as_tibble() %>%
    slice(-1) %>% # drop first node (Agency not required)
    unnest_longer("Hilltop") %>%
    filter(Hilltop_id == "Data") %>%
    select("Hilltop") %>%
    unnest(cols = names(.)) %>%
    unnest_wider("Hilltop") %>%
    unnest(cols = names(.)) %>%
    unnest(cols = names(.)) %>%
    transmute(
      datetime = ymd_hms(T, tz = "Etc/GMT+12") - interval_offset,
      value = as.numeric(I1)
    ) %>%
    mutate(
      year = year(datetime),
      yday = yday(datetime),
      month = month(datetime, label = TRUE),
      year_month = floor_date(datetime, unit = "month"),
      day = floor_date(datetime, unit = "day"),
      day_hour = floor_date(datetime, unit = "hour"),
      date = as_date(datetime),
      time = as_hms(datetime)
    )
}


load_ratings <- function(site, start_date, end_date) {
  # Function to load ratings for a site.
  endpoint <- "http://envdata.tasman.govt.nz/data.hts?"

  measurement <- "Flow"
  url <-
    paste0(
      endpoint,
      "Service=Hilltop&Request=RatingList&Site=",
      gsub(" ", "%20", site),
      "&Measurement=",
      measurement
    )
  ratings <- as_tibble(as_list(read_xml(GET(url)))) %>%
    unnest_longer("HilltopServer")

  starttimes <- ratings %>%
    filter(HilltopServer_id == "StartTime") %>%
    unnest(cols = names(.)) %>%
    unnest(cols = names(.)) %>%
    mutate(datetime = as.POSIXct(HilltopServer, format = "%Y-%m-%dT%H:%M:%S", tz = "Etc/GMT+12")) %>% # ignore timezone, NZST
    rename(start_time = datetime) %>%
    select(start_time)

  effectivetimes <- ratings %>%
    filter(HilltopServer_id == "EffectiveTime") %>%
    unnest(cols = names(.)) %>%
    unnest(cols = names(.)) %>%
    mutate(datetime = as.POSIXct(HilltopServer, format = "%Y-%m-%dT%H:%M:%S", tz = "Etc/GMT+12")) %>% # ignore timezone, NZST
    rename(effective_time = datetime) %>%
    select(effective_time)

  ratings <- tibble(starttimes, effectivetimes) %>%
    mutate(
      se_equal = start_time == effective_time,
      rating = as.double(row.names(.)),
      start_time = round_date(start_time, "5 mins"),
      effective_time = round_date(effective_time, "5 mins"),
      date = as.Date(start_time, tz = "UTC"), # ignore timezone, NZST
      origin = "rating-change"
    ) %>%
    filter(
      date > as.Date(start_date, format = "%Y%m%d", tz = "Etc/GMT+12") & # ignore timezone, NZST
        date < as.Date(end_date, format = "%Y%m%d", tz = "Etc/GMT+12") # ignore timezone, NZST
    ) # filter ratings to analysis period

  return(ratings)
}


load_gaugings <- function(site, start_date, end_date) {
  # Function to load gaugings for a site.
  endpoint <- "http://envdata.tasman.govt.nz/data.hts?"

  measurement <- "Flow [Gauging Results]"
  url <-
    paste0(
      endpoint,
      "Service=Hilltop&Request=GetData&Site=",
      gsub(" ", "%20", site),
      "&Measurement=",
      gsub(" ", "%20", measurement),
      "&From=Sart&To=Now"
    )
  gaugings <- as_tibble(as_list(read_xml(GET(url)))) %>%
    unnest_longer("Hilltop") %>%
    filter(Hilltop_id == "Data") %>%
    select(Hilltop) %>%
    unnest(cols = names(.)) %>%
    unnest(cols = names(.)) %>%
    unnest(cols = names(.)) %>%
    unnest(cols = names(.)) %>%
    mutate(datetime = as.POSIXct(Hilltop, format = "%Y-%m-%dT%H:%M:%S", tz = "Etc/GMT+12")) %>%
    select(c(datetime)) %>%
    drop_na(datetime) %>%
    mutate(
      gauging = row.names(.),
      datetime = round_date(datetime, "5 mins"),
      date = as.Date(datetime, tz = "UTC"),
    ) %>%
    filter(
      date > as.Date(start_date, format = "%Y%m%d", tz = "Etc/GMT+12") & # ignore timezone, NZST
        date < as.Date(end_date, format = "%Y%m%d", tz = "Etc/GMT+12") # ignore timezone, NZST
    ) # filter ratings to analysis period

  return(gaugings)
}


########## ENVMON ##########
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
