\name{merge.list}
\alias{merge.list}
\title{Merge Two Lists}
\description{
  \code{merge.list} merges two lists. If there are identical names in
  both lists, only the elements of the first list are considered. 
}
\usage{
\method{merge}{list}(x, y = NULL, mergeUnnamed = TRUE, \dots)
}
\arguments{
  \item{x}{a list of possibly named elements. All of these are
    in the merged list.}
  \item{y}{a list of possibly named elements or any object,
    which can be coerced to \code{list}. If an element
    has a name occuring also in the argument \code{x}, it will
    not be included in the merged list to avoid duplicate names. If
    \code{NULL}, \code{x} is returned.}
  \item{mergeUnnamed}{logical. If \code{TRUE} (the default) unnamed
    elements in the second list are always included.}
  \item{\dots}{arguments to be passed to or from methods.}
}
\details{
  The purpose of this function is to merge two lists (e.g. argument
  lists). If a named element is found as well in the first list as in
  the second, only the value of the element in the first list is
  considered. One can think of the second list as a list of default
  values, which should be considered only if they are not set
  explicitly in the first list.

  Unnamed elements in \code{y} are included in the merged list
  only if \code{mergeUnnamed} is \code{TRUE}. 
}
\value{
  a list containing all elements of the argument \code{x} and
  those of \code{y} having names not occuring in
  \code{x}. 
}
\author{Thorn Thaler}
\examples{
merge(list(a=1, b="test"), list(3, b=2)) # list(a=1, b="test", 3)
merge(list(1), "test")                   # list(1, "test")
merge(list(1), "test", FALSE)            # list(1)
merge(list(1))                           # list(1)
merge(list(1, a=2, b=3), list(2, b=4))   # list(1, a=2, b=3, 2)
merge(list(1), list(2, b=3), FALSE)      # list(1, b=3)

a <- list(1, 2, 3)
b <- list("a", "b", "c")
names(a)[2] <- names(b)[2] <- "z"
all.equal(merge(a, b), list(1, z=2, 3, "a", "c")) # TRUE
}