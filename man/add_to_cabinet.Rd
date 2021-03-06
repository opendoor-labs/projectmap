% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/package_functions.R
\name{add_to_cabinet}
\alias{add_to_cabinet}
\title{Adds a file to the file cabinet}
\usage{
add_to_cabinet(file)
}
\arguments{
\item{file}{A character string giving the full path to the specified file (i.e. "./Codes/Model 1/Model1.R").}
}
\value{
No return value
}
\description{
Adds a file path to the cabinet with the path being relative to the project's
working directory. The cabinet is saved as a package environment variable.
}
\examples{
add_to_cabinet()
}
\author{
Alex Hubbard (hubbard.alex@gmail.com)
}
