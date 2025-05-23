% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/EEAaq_export.R
\name{EEAaq_export}
\alias{EEAaq_export}
\title{Export and save an \code{EEAaq_df} class object}
\usage{
EEAaq_export(data, filepath, format, shape = FALSE)
}
\arguments{
\item{data}{an \code{EEAaq_df} class object.}

\item{filepath}{character string giving the file path}

\item{format}{character string giving the format of the file. It must be one of 'csv' and 'txt'.}

\item{shape}{logical value (T or F). If \code{TRUE} the shapefile associated to the \code{EEAaq_df} object
given in input is saved in the same directory specified in \code{filepath}. If \code{FALSE} (the default),
only the data frame containing the data is saved.}
}
\value{
No return value, called for side effects.
}
\description{
\code{EEAaq_export} saves an \code{EEAaq_df} class object as a \emph{.csv} or a \emph{.txt} file,
and exports the associated shapefile as well.
}
\examples{
\donttest{
#Download a dataset with the function EEAaq_get_data, which generate an EEAaq_df object.
data <- EEAaq_get_data(zone_name = "15146", NUTS_level = "LAU",
                       LAU_ISO = "IT", pollutants = "PM10",
                       from = "2023-01-01", to = "2023-05-31")
temp <- tempdir()
filepath <- paste0(temp, "/data.csv")
EEAaq_export(data = data, filepath = filepath, format = "csv", shape = TRUE)
}
}
