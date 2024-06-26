% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readProduction.R
\name{readProduction}
\alias{readProduction}
\title{Read Production data from FAO}
\usage{
readProduction(source_folder = NULL)
}
\arguments{
\item{source_folder}{Folder where FAOSTAT data (unzipped) is saved}
}
\value{
Emissions from FAO
}
\description{
Read Production data from FAO
}
\examples{
\dontrun{
readProduction(indicator = "crops")
}
}
\author{
Abhijeet Mishra
}
