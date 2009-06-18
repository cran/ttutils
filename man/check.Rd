\name{check}
\alias{check}
\title{Check Objects}
\description{
  \code{check} is a generic function that tests the validity of a given
  object.
}
\usage{
check(object, \dots)
}
\arguments{
  \item{object}{an object to be tested for validity.}
  \item{\dots}{further arguments to be passed to the particular
    dispachted function.}
}
\details{
  \code{check} tests if a given object meets the formal requirements of
  being a valid object of its class. If the test fails, additional
  warnings should be provided, giving some information why the test failed.
}
\value{
returns \code{TRUE} if \code{object} passes the validity test for the
specific class and \code{FALSE} otherwise.
}
\author{Thorn Thaler}
\note{\R's dispatching mechanism determines the class of a given object
  and then calls the function \code{check.<class-name>}. If no specific
  \code{check} function is found, \code{check.default} is called. The
  function \code{check.default} does not make much sense, for the
  purpose of \code{check} is to test the validity for a \emph{specific
    class}. Hence, \code{check.default} simply returns \code{FALSE}
  together with a warning message that no specific
  \code{check.<class-name>} function was found.

  The dispatching mechanism has immediately two consequences:
  \enumerate{
    \item a class specific \code{check} routine need not to check
    whether the object belongs to the class itself, because if it would
    not, the function would not have been called.
    \item if no specific \code{check} routine is found, the result for a
    call of \code{check} will be \code{FALSE}, since in this case the
    default function is called which will return \code{FALSE} in any case.
    }
}