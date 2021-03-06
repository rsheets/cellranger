% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ra-ref.R
\name{print.ra_ref}
\alias{print.ra_ref}
\title{Print ra_ref object}
\usage{
\method{print}{ra_ref}(x, fo = c("R1C1", "A1"), ...)
}
\arguments{
\item{x}{an object of class \code{\link{ra_ref}}}

\item{fo}{either \code{"R1C1"} (the default) or \code{"A1"} specifying the
cell reference format; in many contexts, it can be inferred and is optional}

\item{...}{further arguments passed to or from other methods}
}
\description{
Print ra_ref object
}
\examples{
(rar <- ra_ref(3, TRUE, 1, TRUE))
print(ra_ref(), fo = "A1")

}
