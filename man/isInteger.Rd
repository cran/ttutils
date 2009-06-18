\name{isInteger}
\alias{isInteger}
\title{Test For Integrity}
\description{\code{isInteger} tests if a given number is an integer.}
\usage{isInteger(n, tol = .Machine$double.eps)}
\arguments{
  \item{n}{a vector or an array of values to be tested.}
  \item{tol}{a numeric value giving the tolerance level.}
}
\value{\code{TRUE} if the argument \code{n} has an integer value,
  \code{FALSE} otherwise.}
\details{As opposed to \code{\link{is.integer}} this function tests for
  integrity of a given value, rather than being of \emph{type}
  \code{integer}.

  In \R integers are specified by the suffix \code{L} (e.g. \code{1L}),
  whereas all other numbers are of class \code{\link{numeric}}
  independent of their value. The function \code{\link{is.integer}} does
  not test whether a given variable has an integer value, but whether
  it belongs to the  class \code{\link{integer}}.

  In contrast, the function \code{isInteger} compares the difference
  between its argument and its rounded argument. If it is smaller than
  some predefined tolerance level, the variable is regarded as integer.
}
\note{ The \R function \code{\link{c}} concatenates its argument
  and forms a vector. In doing so, it coerces the values to a common
  type. Hence, attention has to be paid, because \code{isInteger} may
  give some unexpected results in this case. The \R command
  \code{\link{list}}, however,  does not coerce its arguments (see
  the example).
}
\seealso{\code{\link{is.integer}}}
\examples{
# isInteger tests if the _value_ of a variable is an integer
# 'c' as opposed to 'list' coerces its arguments!
isInteger(c("test", 1, 2, 2.1))     # FALSE FALSE FALSE FALSE
isInteger(list("test", 1, 2, 2.1))  # FALSE TRUE TRUE FALSE

class(1L)  # integer
typeof(1L) # integer
class(1)   # numeric
typeof(1)  # double

# is.integer tests if the _class_ of a variable is 'integer'
is.integer(c("test", 1, 2))    # FALSE
is.integer(list("test", 1, 2)) # FALSE
is.integer(1)                  # FALSE
is.integer(1L)                 # TRUE 
}
\author{Thorn Thaler}

