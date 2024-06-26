% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcImpactEmissions.R
\name{calcImpactEmissions}
\alias{calcImpactEmissions}
\title{Calculate emissions from an IMPACT run}
\usage{
calcImpactEmissions(
  gdx = NULL,
  source_folder = NULL,
  ef_db = NULL,
  efficiency_improvement = 0.02
)
}
\arguments{
\item{gdx}{path to an IMPACT run GDX}

\item{source_folder}{Folder where FAOSTAT data (unzipped) is saved}

\item{ef_db}{name of the object in environment which contains the output of
calcEmissionIntensities(). If this is not provided the function
calcEmissionIntensities() will be called. This is time consuming process and
it is adviced that you run calcEmissionIntensities() before running this
function and feed the output of that function in "ef_db" parameter of this
function.}

\item{efficiency_improvement}{Improvement in Emission intensities annually
(defaults to 2 percent i.e., 0.02)}
}
\value{
Emissions from an IMPACT run
}
\description{
Calculate emissions from an IMPACT run
}
\examples{
\dontrun{
calcImpactEmissions()
}
}
\author{
Abhijeet Mishra
}
