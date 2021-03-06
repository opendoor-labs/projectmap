% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/package_functions.R
\name{package.depend}
\alias{package.depend}
\title{This function gets package dependencies}
\usage{
package.depend(pkgs, lib.loc = proj.env$libPath, fields = c("Imports",
  "Depends", "Suggests"))
}
\arguments{
\item{pkgs}{Character vector of package names}
}
\value{
Character vector of package names
}
\description{
This function gets package dependencies
}
\examples{
packages_dependencies("packageName")
}
\author{
Alex Hubbard (hubbard.alex@gmail.com)
}
