.savePdf <- function(plot.func, plot.name, pdf.options, ...) {
  fileName <- paste(plot.name, "pdf", sep=".")
  pdf.options <- c(pdf.options, list(file=fileName))
  do.call("pdf", pdf.options)
  plot.func(...)
  dev.off()
}

.savePs <- function(plot.func, plot.name, ps.options, is.trellis, ...) {
  fileName <- paste(plot.name, "eps", sep = ".")
  ps.options <- c(ps.options, list(file=fileName))
  do.call("postscript", ps.options)
  plot.func(...)
  dev.off()
}

plotAndSave <- function(plot.func, plot.name, ..., folder=getwd(),
                        format=c("eps", "pdf"),
                        ps.options=list(onefile=TRUE, horizontal=FALSE,
                          paper="special", width=7, height=7),
                        pdf.options=list(onefile=TRUE), do.plot=TRUE,
                        do.return=do.plot) { 
  n <- nchar(folder)
  ch <- substr(folder, n, n)
  if (ch != .Platform$file.sep) {
    folder <- paste(folder, .Platform$file.sep, sep="")
  }
  folder <- path.expand(folder)
  if (do.return && !do.plot) {
    warning("Plot will not be displayed, hence there will be no return value!")
  }
  fileName <- paste(folder, plot.name, sep="")
  chosenFormats <- unique(match.arg(format, several.ok=T))
  plotFunction <- match.fun(plot.func)
  lapply(chosenFormats, function(type)
         switch(type,
                "pdf" = {
                  .savePdf(plotFunction, fileName, pdf.options, ...);
                },
                "eps" = {
                  .savePs( plotFunction, fileName, ps.options,  ...);
                })
         )
  if (do.plot) {
    if (do.return) {
      return(plot.func(...))
    } else {
      invisible(plot.func(...))
    }
  }
}
