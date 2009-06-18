\name{interval}
\alias{interval}
\alias{liesWithin}
\title{Interval Class}
\description{
 \code{interval} constructs an object of class \code{interval}
 representing an interval.

 \code{liesWithin} checks if a number lies within a given interval.
}
\usage{
interval(lower, upper, left=c(">=", ">"), right=c("<=", "<"))

liesWithin(x, int)
}
\arguments{
  \item{lower}{the lower boundary of the interval. Can be set to \code{-Inf}.} 
  \item{upper}{the upper boundary of the interval. Can be set to \code{Inf}.}
  \item{left, right}{a comparison symbol. Must be one of (\dQuote{>=},
    \dQuote{>}) for \code{left} and (\dQuote{<=},
    \dQuote{<}) for \code{right}, respectively. Determines whether the
    boundary values are included in the interval or not. The default is
    \dQuote{>=} and \dQuote{<=}, respectively.}
  \item{x}{a numeric vector or array giving the numbers to be checked.}
  \item{int}{an interval object.}
}
\value{
  \code{interval} returns an object of class \code{interval} containing
  the following components:
  \item{lower}{the lower boundary of the interval}
  \item{upper}{the upper boundary of the interval}
  \item{left}{the left comparison operator}
  \item{right}{the right comparison operator}

  \code{liesWithin} returns \code{TRUE} if the given number lies
  within the interval and \code{FALSE} otherwise.
}
\author{Thorn Thaler}
\examples{

i <- interval(-3, 3, left=">")

liesWithin(-3:5, i)
}
