\name{plotAndSave}
\alias{plotAndSave}
\title{Display And Save A Plot}
\description{
  \code{plotAndSave} saves a plot as \dQuote{pdf}, \dQuote{(e)ps},
  \dQuote{jp(e)g}, \dQuote{png}, \dQuote{bmp}, \dQuote{tiff},
  \dQuote{emf} and/or \dQuote{wmf} and additionally displays
  the plot. 
}
\usage{
plotAndSave(plot.func, plot.name, \dots, folder=getwd(),
         format=c("eps", "pdf"),
         options=list(eps = list(onefile=TRUE, horizontal=FALSE,
                                 paper="special",
                                 width=7, height=7),
                      ps  = list(onefile=TRUE, horizontal=FALSE,
                                 paper="special",
                                 width=7, height=7),
                      pdf = list(onefile=TRUE)),
         do.plot=TRUE, do.return=do.plot)
}
\arguments{
  \item{plot.func}{either a function or a non-empty character string 
    naming the plotting function to be called.}
  \item{plot.name}{a character string (without any suffix such as
    \dQuote{.pdf} or \dQuote{.eps}) giving the name of the file where
    the plot should be saved to.}
  \item{\dots}{additional arguments to be passed to the plotting
    function.}
  \item{folder}{a character string giving the name of the folder to
    which the plot should be saved. The default is the current
    directory.}
  \item{format}{output format. Must be a subset of (\dQuote{pdf},
    \dQuote{(e)ps}, \dQuote{jp(e)g}, \dQuote{png}, \dQuote{bmp},
    \dQuote{tiff}, \dQuote{emf}, \dQuote{wmf}). The latter two
    can be used only on with a Windows OS. The default is to
    produce both an eps-file and a pdf-file. Can be abbreviated.}
  \item{options}{named list of options to be passed to the 
    respective device driver. Each entry of the list is an option
    list for the device corresponding to the name of the list item.}
  \item{do.plot}{logical. If \code{TRUE} (the default) the plot is
    displayed.}
  \item{do.return}{logical. If \code{TRUE} the return value of the
    plotting function is returned. Defaults to the value of the
    parameter \code{do.plot}.}
}

\value{
  the return value of the plotting function.
}
\details{
  The purpose of this function is to produce a plot on the monitor and
  to save it to a file simultaneously.

  The file name must be given without any file-suffix. Depending on the
  argument \code{format} the function then generates the respective file
  with the appropriate suffix. The path should not be included in the
  file name, since the location where the files should be saved to is
  controlled by the parameter \code{folder}.

  The function needs a plotting function to be defined, which actually
  does the plotting itself. Additional arguments (e.g. further graphical
  parameters) can be passed to \code{plotAndSave}, which in turn, passes
  these arguments down to the plotting function,

  The parameters of devices are controlled by the arguments \code{options}.
}
\note{
  When using Trellis plots from package \pkg{lattice} one has to assure
  that the plotting function actually \emph{does} the plotting. Since
  the default behaviour of Trellis plots is just to return the Trellis
  object, one should wrap the call to the particular \pkg{lattice}
  function in a call of the function \code{print}. The generic function
  \code{print} ensures that the plot is displayed and not just returned
  as an object.}
\seealso{\code{\link{pdf}}, \code{\link{postscript}},
  \code{\link{jpeg}}, \code{\link{png}}, \code{\link{bmp}},
    \code{\link{tiff}} 
  }

\examples{
\dontrun{
## Plotting Function
# For 'lattice' graphics:
# WRONG:
# f <- function(x, ...) xyplot(x~sin(x), ...)
# CORRECT:
# f <- function(x, ...) print(xyplot(x~sin(x), ...))

f <- function(x, ...) plot(x, sin(x), col=2, type="l", ...)

# Save the plot as "Sine_Function.pdf" in the current folder
# and add a title to the plot

plotAndSave(f, "Sine_Function", x=seq(-pi, pi, length=100),
            main="Sine-Function", format="pd")

}}
\author{Thorn Thaler}

