#' @import tidyr
#' @import dplyr
#' @import lubridate
#' @import hms
#' @import XML
#' @import xml2
#' @import httr
#' @import zeallot
#' @import rapport
#' @import stringr
#' @import RODBC

# TODO - improve function definitions.

########## HILLTOP SERVER ##########
get_sites <- function(endpoint = "http://envdata.tasman.govt.nz/data.hts?", latlong = TRUE) {
  # Function to get Sites from Hilltop Server.
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


get_measurements <- function(endpoint = "http://envdata.tasman.govt.nz/data.hts?", site = NA) {
  # Function to get Measurements from Hilltop Server.  Can call with a site to get measurements
  # associated with that site.
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


get_collections <- function(endpoint = "http://envdata.tasman.govt.nz/data.hts?") {
  # Function to get Collection list from Hilltop Server.
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


interval_to_offset <- function(interval) {
  # Function to return offset to correct for Hilltops right-bound datetimes.
  if (!is.character(interval)) {
    return(NA)
  } else {
    if (grepl("minute", interval)) {
      c(n, int) %<-% unlist(str_split(interval, " "))
      return(minutes(n))
    } else if (grepl("hour", interval)) {
      return(hours(1))
    } else if (grepl("day", interval)) {
      return(days(1))
    } else if (grepl("month", interval)) {
      return(months(1))
    } else if (grepl("year", interval)) {
      return(years(1))
    } else {
      return(NA)
    }
  }
}


get_data_collection <- function(endpoint = "http://envdata.tasman.govt.nz/data.hts?", collection, method = NA, time_interval = NA, from = NA, to = NA, interval = "1 day", alignment = "00:00") {
  # Function to get data for a Collection from Hilltop Server.
  if (is.na(method)) {
    url <- ""
    interval_offset <- 0
  } else {
    url <- paste0(
      "&Method=", method,
      "&Interval=", interval,
      "&Alignment=", alignment
    )
    interval_offset <- interval_to_offset(interval)
  }

  #print(paste0("Datetime offset: ", interval_offset))

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


get_data_site_measurement <- function(endpoint = "http://envdata.tasman.govt.nz/data.hts?", site, measurement, time_interval = NA, from = NA, to = NA) {
  # Function to get data for a measurement for a site.
  url <-
    paste0(
      endpoint,
      "Service=Hilltop&Request=GetData",
      "&Site=", site,
      "&Measurement=", measurement
    )

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
      datetime = ymd_hms(T, tz = "Etc/GMT+12"), # ignore timezone, NZST
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
read_site_information_from_envmon <- function(site) {
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
  # Close db connection
  odbcClose(envmon)

  return(site_information)
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

  # Split string according to \r\n rows, then convert rows to double and create dataframe.
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
