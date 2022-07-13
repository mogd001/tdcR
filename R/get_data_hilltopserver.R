#' @import dplyr
#' @import lubridate
#' @import hms
#' @import XML
#' @import xml2
#' @import httr
#' @import zeallot
#' @import rapport
#' @import hillr


get_sites <- function() {
  # Function get Sites from Hillop Server
  endpoint <- "http://envdata.tasman.govt.nz/data.hts?"
  sites <- hillr::getHilltopSites(endpoint = endpoint) %>%
    as_tibble() %>%
    rename_all(tolower) %>%
    mutate(
      latitude = as.numeric(latitude),
      longitude = as.numeric(longitude)
    )
}


get_collections <- function() {
  # Function to get Collection list from Hilltop Server
  url <- "http://envdata.tasman.govt.nz/data.hts?Service=Hilltop&Request=CollectionList"

  hilltop_data <- read_xml(url)
  #collections <- xpathSApply(xmlParse(hilltop_data), "//Collection", xmlGetAttr, "Name")
  collections <- xml_find_all(hilltop_data , "Collection") %>% xml_attr("Name")

  hilltop_df <- hilltop_data %>%
    as_list() %>%
    as_tibble() %>%
    mutate(collection = collections) %>%
    unnest_longer("HilltopProject") %>%
    filter(HilltopProject_id == "Item") %>% # rows without an "Item" have no sites in the collection.
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
  # Function to return offset to correct for Hilltops right-bound datetimes
  if (!is.character(interval)) {
    return(NA)
  } else {
    if (grepl("hour", interval)) {
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


get_data <- function(collection, method, time_interval = "P1Y/now", from = "", to = "", interval = "1 day", alignment = "00:00") {
  # Function to get data for a Collection from Hilltop Server.
  endpoint <- "http://envdata.tasman.govt.nz/data.hts?"

  statistic <- paste0(
    "&Method=", method,
    "&Interval=", interval,
    "&Alignment=", alignment
  )

  interval_offset <- interval_to_offset(interval)
  print(interval_offset)

  if (!is.na(time_interval)) {
    statistic <- paste0(statistic, "&TimeInterval=", time_interval)
  } else if (!is.na(from) | !is.na(to)) {
    statistic <- paste0(statistic, "&From=", from, "&To=", to)
  } else {
    print("Incorrect time_interval or from/to specified.")
  }

  statistic <- gsub(" ", "%20", statistic)

  url <-
    paste0(
      endpoint,
      "Service=Hilltop&Request=GetData&Collection=",
      gsub(" ", "%20", collection),
      statistic
    )
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
      datetime = ymd_hms(T, tz = "NZ") - interval_offset,
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
  # Load ratings for site

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
    mutate(datetime = as.POSIXct(HilltopServer, format = "%Y-%m-%dT%H:%M:%S", tz = "NZ")) %>%
    rename(start_time = datetime) %>%
    select(start_time)

  effectivetimes <- ratings %>%
    filter(HilltopServer_id == "EffectiveTime") %>%
    unnest(cols = names(.)) %>%
    unnest(cols = names(.)) %>%
    mutate(datetime = as.POSIXct(HilltopServer, format = "%Y-%m-%dT%H:%M:%S", tz = "NZ")) %>%
    rename(effective_time = datetime) %>%
    select(effective_time)

  ratings <- tibble(starttimes, effectivetimes) %>%
    mutate(
      se_equal = start_time == effective_time,
      rating = as.double(row.names(.)),
      start_time = round_date(start_time, "5 mins"),
      effective_time = round_date(effective_time, "5 mins"),
      date = as.Date(start_time, tz = "NZ"),
      origin = "rating-change"
    ) %>%
    filter(
      date > as.Date(start_date, format = "%Y%m%d", tz = "NZ") &
        date < as.Date(end_date, format = "%Y%m%d", tz = "NZ")
    ) # filter ratings to analysis period

  return(ratings)
}


load_gaugings <- function(site, start_date, end_date) {
  # Load gaugings for site
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
    mutate(datetime = as.POSIXct(Hilltop, format = "%Y-%m-%dT%H:%M:%S", tz = "NZ"), ) %>%
    select(c(datetime)) %>%
    drop_na(datetime) %>%
    mutate(
      gauging = row.names(.),
      datetime = round_date(datetime, "5 mins"),
      date = as.Date(datetime, tz = "NZ"),
    ) %>%
    filter(
      date > as.Date(start_date, format = "%Y%m%d", tz = "NZ") &
        date < as.Date(end_date, format = "%Y%m%d", tz = "NZ")
    ) # filter ratings to analysis period

  return(gaugings)
}
