% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hilltopserver_functions.R
\name{reset_hilltop_server}
\alias{reset_hilltop_server}
\title{Reset Hilltop Server}
\usage{
reset_hilltop_server(endpoint = "http://envdata.tasman.govt.nz/data.hts?")
}
\arguments{
\item{endpoint}{A url for the Hilltop endpoint.}
}
\value{
Reset text.
}
\description{
Function to send reset command to Hilltop Server.
}
\examples{
reset_hilltop_server("http://envdata.tasman.govt.nz/data.hts?")
}
