########## HILLTOP SERVER FUNCTIONS ##########
# A collection of wrapper functions for calling hilltop server commands and
# returning tibbles for subsequent data workflows.


#' Get Hilltop Server Version
#'
#' @description Function to get Hilltop Server Version.
#' @param endpoint A url for the Hilltop endpoint.
#' @return Version text.
#' @examples
#' get_version("http://envdata.tasman.govt.nz/data.hts?")
get_version <- function(endpoint = "http://envdata.tasman.govt.nz/data.hts?") {
  url <- paste0(endpoint, "Service=Hilltop&Request=Version")
  print(url)

  hilltop_data <- read_xml(url)
  version <- hilltop_data %>% xml_text("Status")

  print(version)
}


#' Reset Hilltop Server
#'
#' @description Function to send reset command to Hilltop Server.
#' @param endpoint A url for the Hilltop endpoint.
#' @return Reset text.
#' @examples
#' reset_hilltop_server("http://envdata.tasman.govt.nz/data.hts?")
reset_hilltop_server <- function(endpoint = "http://envdata.tasman.govt.nz/data.hts?") {
  url <- paste0(endpoint, "Service=Hilltop&Request=Reset")
  print(url)

  hilltop_data <- read_xml(url)
  status <- hilltop_data %>% xml_text("Status")

  print(status)
}


#' Get Sites
#'
#' @description Function to get Sites from Hilltop Server.
#' @param endpoint A url for the Hilltop endpoint.
#' @param latlong A logical, TRUE returns EPSG:4326 (WGS) coordinates , EPSG:2193 (NZTM) otherwise.
#' @param collection A string for the collection to return only sites for that collection.
#' @param site_parameters A logical, TRUE returns additional site parameters.
#' @param synonyms A logical, TRUE returns site synonyms.
#' @return A tibble of sites and their corresponding coordinates.
#' @examples
#' get_sites("http://envdata.tasman.govt.nz/data.hts?")
#' get_sites("http://envdata.tasman.govt.nz/data.hts?", latlong = FALSE)
#' get_sites("http://envdata.tasman.govt.nz/data.hts?", collection = "Rainfall")
#' get_sites("http://envdata.tasman.govt.nz/data.hts?", site_parameters = TRUE, synonyms = TRUE)
get_sites <- function(endpoint = "http://envdata.tasman.govt.nz/data.hts?",
                      latlong = TRUE,
                      collection = NA,
                      site_parameters = FALSE,
                      synonyms = FALSE) {
  url <- paste0(endpoint, "Service=Hilltop&Request=SiteList")

  if (latlong) {
    url <- paste0(url, "&Location=LatLong")
  } else { # return easting and northing
    url <- paste0(url, "&Location=Yes")
  }

  if (!is.na(collection)) {
    url <- paste0(url, "&Collection=", collection)
  }

  if (site_parameters) {
    url <- paste0(url, "&SiteParameters=CatchmentName,CatchmentArea,Altitude,AirTown") # why does catchment name not work?
  }

  url <- gsub(" ", "%20", url)
  print(url)

  hilltop_data <- read_xml(url)
  sites <- xml_find_all(hilltop_data, "Site") %>% xml_attr("Name")

  hilltop_df <- hilltop_data %>%
    as_list() %>%
    as_tibble()

  if (is.na(collection)) { # default
    hilltop_df <- hilltop_df %>%
      slice(1:n() - 1) # drop last row
  }

  hilltop_df <- hilltop_df %>%
    slice((n() - length(sites) + 1):n()) %>% # drop first non-site rows
    mutate(site = sites) %>%
    unnest_longer("HilltopServer") %>%
    mutate(list_length = sapply(HilltopServer, length)) %>%
    filter(list_length > 0) %>%
    transmute(
      site = site,
      src = HilltopServer_id,
      data = as.numeric(unlist(HilltopServer))
    ) %>%
    filter(
      !is.na(src) & !is.na(data)
    ) %>%
    pivot_wider(
      names_from = src,
      values_from = data
    ) %>%
    rename_all(tolower)

  if (synonyms) {
    url2 <- paste0(endpoint, "Service=Hilltop&Request=SiteList", "&Target=HtmlSelect&SynLevel=1")
    first_synonym <- jsonlite::fromJSON(url2)$Options %>%
      tibble() %>%
      rename(site = Value, first_synonym = Option)

    hilltop_df <- hilltop_df %>%
      left_join(first_synonym, by = "site")

    url3 <- paste0(endpoint, "Service=Hilltop&Request=SiteList", "&Target=HtmlSelect&SynLevel=2")
    second_synonym <- jsonlite::fromJSON(url3)$Options %>%
      tibble() %>%
      rename(site = Value, second_synonym = Option)

    hilltop_df <- hilltop_df %>%
      left_join(second_synonym, by = "site")
  }

  hilltop_df # return hilltop_df
}


#' Get Measurements
#'
#' @description Function to get Measurements from Hilltop Server.
#' @param endpoint A url for the Hilltop endpoint.
#' @param collection A string for the collection to return only measurements associated with that specific collection.
#' @param site A string for the site so to return only measurements associated with that specific site.
#' @return A tibble of measurements.
#' @examples
#' get_measurements("http://envdata.tasman.govt.nz/data.hts?")
#' get_measurements("http://envdata.tasman.govt.nz/data.hts?", collection = "Rainfall")
#' get_measurements("http://envdata.tasman.govt.nz/data.hts?", site = "HY Anatoki at Happy Sams")
#' get_measurements("http://envdata.tasman.govt.nz/data.hts?", collection = "Rainfall", site = "HY Anatoki at Happy Sams")
get_measurements <- function(endpoint = "http://envdata.tasman.govt.nz/data.hts?",
                             collection = NA,
                             site = NA) {
  url <- paste0(endpoint, "Service=Hilltop&Request=MeasurementList")

  if (!is.na(collection)) {
    url <- paste0(url, "&Collection=", collection)
  }
  if (!is.na(site)) {
    url <- paste0(url, "&Site=", site)
  }

  url <- gsub(" ", "%20", url)
  print(url)

  hilltop_data <- read_xml(url)

  if (is.na(site) | (!is.na(collection) & !is.na(site))) {
    measurements <- xml_find_all(hilltop_data, "Measurement") %>% xml_attr("Name") # all measurements
    hilltop_df <- tibble(measurement = measurements)
  } else {

    data_sources <- xml_find_all(hilltop_data, "DataSource") %>% xml_attr("Name")
    measurements <- xpathSApply(xmlParse(hilltop_data), "//Measurement", xmlGetAttr, "Name")

    hilltop_df <- hilltop_data %>%
      as_list() %>%
      as_tibble() %>%
      slice(-1) %>% # Remove agency
      transmute(data_source = data_sources, data = HilltopServer)

    ds_data <- hilltop_df %>%
      unnest_longer("data")

    num_items <- ds_data %>% filter(data_id %in% c("NumItems")) %>% select(data) %>% unlist()
    ts_type <- ds_data %>% filter(data_id %in% c("TSType")) %>% select(data) %>% unlist()
    data_type <- ds_data %>% filter(data_id %in% c("DataType")) %>% select(data) %>% unlist()
    interpolation <- ds_data %>% filter(data_id %in% c("Interpolation")) %>% select(data) %>% unlist()
    item_format <- ds_data %>% filter(data_id %in% c("ItemFormat")) %>% select(data) %>% unlist()
    from <- ds_data %>% filter(data_id %in% c("From")) %>% select(data) %>% unlist()
    to <- ds_data %>% filter(data_id %in% c("To")) %>% select(data) %>% unlist()

    hilltop_df <- hilltop_df %>% mutate(
      num_items = as.numeric(num_items),
      ts_type = ts_type,
      data_type = data_type,
      interpolation = interpolation,
      item_format = as.numeric(item_format),
      from = ymd_hms(from, tz = "Etc/GMT-12"),
      to = ymd_hms(to, tz = "Etc/GMT-12")
    )

    ds_data <- hilltop_df %>%
      unnest_longer("data") %>%
      filter(data_id == "Measurement") %>%
      mutate(measurement = measurements) %>%
      select(-c(data, data_id)) %>%
      relocate(measurement)
  }
}


#' Get Collections
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


#' Get Data for a Collection
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
#' get_data_collection("http://envdata.tasman.govt.nz/data.hts?",
#'   collection = "Rainfall",
#'   method = "Total", interval = "1 hour", from = "20220101", to = "202207031"
#' )
#' get_data_collection("http://envdata.tasman.govt.nz/data.hts?",
#'   collection = "ActiveFlowSites",
#'   method = "Extrema", interval = "15 minutes", from = "20220101", to = "202207031"
#' )
get_data_collection <- function(endpoint = "http://envdata.tasman.govt.nz/data.hts?",
                                collection,
                                method = NA,
                                interval = "1 day",
                                from = NA,
                                to = NA,
                                time_interval = NA,
                                alignment = "00:00") {
  url <- paste0(
    endpoint,
    "Service=Hilltop&Request=GetData&Collection=",
    collection
  )

  if (!is.na(method)) {
    url <- paste0(
      url,
      "&Method=", method,
      "&Interval=", interval,
      "&Alignment=", alignment
    )
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
      datetime = ymd_hms(T, tz = "Etc/GMT-12"),
      value = as.numeric(I1)
    )
}


#' Get Data for a Site and Measurement
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
#'   site = "AQ Richmond Central at Plunket", measurement = "PM2.5 (24 Hour)",
#'   interval = "1 hour", from = "20220101", to = "202207031"
#' )
#' get_data_site_measurement("http://envdata.tasman.govt.nz/data.hts?",
#'   site = "HY Richmond Weather at TDC Roof", measurement = "Rainfall",
#'   method = "Total", interval = "15 minutes", from = "20220101", to = "20220731"
#' )
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

  if (!is.na(method)) {
    url <- paste0(
      url,
      "&Method=", method,
      "&Interval=", interval,
      "&Alignment=", alignment
    )
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
      datetime = ymd_hms(T, tz = "Etc/GMT-12"),
      value = as.numeric(I1)
    )
}


#' Get Ratings for a Site
#'
#' @description Function to get Ratings for a Site from Hilltop Server.
#' @param endpoint A url for the Hilltop endpoint.
#' @param site A string for the site.
#' @return A tibble containing the ratings for the site.
#' @examples
#' get_ratings("http://envdata.tasman.govt.nz/data.hts?",
#'   site = "HY Aorere at Devils Boots"
#' )
get_ratings <- function(endpoint = "http://envdata.tasman.govt.nz/data.hts?",
                        site) {
  url <-
    paste0(
      endpoint,
      "Service=Hilltop&Request=RatingList&Site=",
      gsub(" ", "%20", site),
      "&Measurement=Flow"
    )
  print(url)

  ratings <- as_tibble(as_list(read_xml(GET(url)))) %>%
    unnest_longer("HilltopServer")

  starttimes <- ratings %>%
    filter(HilltopServer_id == "StartTime") %>%
    unnest(cols = names(.)) %>%
    unnest(cols = names(.)) %>%
    mutate(start_time = ymd_hms(HilltopServer, tz = "Etc/GMT-12", quiet = TRUE)) %>%
    select(start_time)

  effectivetimes <- ratings %>%
    filter(HilltopServer_id == "EffectiveTime") %>%
    unnest(cols = names(.)) %>%
    unnest(cols = names(.)) %>%
    mutate(effective_time = ymd_hms(HilltopServer, tz = "Etc/GMT-12", quiet = TRUE)) %>%
    select(effective_time)

  ratings <- tibble(starttimes, effectivetimes) %>%
    mutate(
      se_equal = start_time == effective_time,
      rating = as.double(row.names(.)),
      start_time = round_date(start_time, "5 mins"),
      effective_time = round_date(effective_time, "5 mins"),
      date = as.Date(start_time, tz = "Etc/GMT-12"),
      origin = "rating-change"
    )
}


#' Get Gaugings for a Site
#'
#' @description Function to get Gaugings for a Site from Hilltop Server.
#' @param endpoint A url for the Hilltop endpoint.
#' @param site A string for the site.
#' @return A tibble containing the gaugings for the site.
#' @examples
#' get_gaugings("http://envdata.tasman.govt.nz/data.hts?",
#'   site = "HY Aorere at Devils Boots"
#' )
get_gaugings <- function(endpoint = "http://envdata.tasman.govt.nz/data.hts?",
                         site) {
  url <-
    paste0(
      endpoint,
      "Service=Hilltop&Request=GetData&Site=",
      gsub(" ", "%20", site),
      "&Measurement=",
      gsub(" ", "%20", "Flow [Gauging Results]"),
      "&From=Sart&To=Now"
    )
  print(url)

  gaugings <- as_tibble(as_list(read_xml(GET(url)))) %>%
    unnest_longer("Hilltop") %>%
    filter(Hilltop_id == "Data") %>%
    select(Hilltop) %>%
    unnest(cols = names(.)) %>%
    unnest_wider("Hilltop") %>%
    unnest(cols = names(.)) %>%
    unnest(cols = names(.)) %>%
    transmute(
      site = site,
      datetime = ymd_hms(T, tz = "Etc/GMT-12"),
      stage = as.numeric(I1),
      flow = as.numeric(I2),
      area = as.numeric(I3),
      velocity = as.numeric(I4),
      max_depth = as.numeric(I5)
    )
}
