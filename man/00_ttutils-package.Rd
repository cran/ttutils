\name{ttutils-package}
\alias{ttutils-package}
\alias{ttutils}
\docType{package}
\title{
  Utility Functions
}
\description{
  The package \pkg{ttutils} contains some auxiliary functions.

  See section \sQuote{Index} for a list of exported functions. Section
  \sQuote{Internals} lists the internal functions of the package, which
  are not exported but may be referenced by \code{ttutils:::.functionName}.  
}
\details{
  \tabular{ll}{
    Version: \tab 1.0-1\cr
    Date: \tab 2009-06-18\cr
    License: \tab GPL-2\cr
    Built: \tab R 2.8.1; ; 2009-06-22 15:18:40; unix
  }
}
\section{Index}{
  \tabular{ll}{
    \code{\link{check}} \tab : Generic function to check the validity of
    a given object\cr
    \code{\link{interval}} \tab :  Interval class\cr
    \code{\link{isInteger}} \tab : Test for integrity \cr
    \code{\link{liesWithin}} \tab : Test for interval coverage \cr
    \code{\link{merge.list}} \tab : Merge two lists \cr
    \code{\link{plotAndSave}} \tab : Display and save a plot
  }
}
\section{Internals}{
  \tabular{ll}{
    \code{.parseRelation} \tab :  Parse a relation symbol and return the
    result of the comparison\cr
    \code{.saveDevice} \tab : Save a plot on a given device \cr
  }
}
\author{
  Thorn Thaler <thorn.thaler@thothal.com>
  
  Maintainer: Thorn Thaler <thorn.thaler@thothal.com>
}
\keyword{ package }
