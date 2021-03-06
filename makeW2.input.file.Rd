% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/makeW2.input.file.r
\name{makeW2.input.file}
\alias{makeW2.input.file}
\title{\subsection{Write npt file for CE-QUAL-W2}{
}}
\usage{
makeW2.input.file(
  x.df,
  new.npt.filename,
  fwf = TRUE,
  path = "",
  note = "",
  writefiles = TRUE
)
}
\arguments{
\item{x.df}{dataframe with first column JDAY}

\item{new.npt.filename}{character; filename}

\item{fwf}{logical; write a fixed-width file or csv?}

\item{path}{character; define the path to write to}

\item{note}{character; write a note to describe the data in the first 3 lines of the file}

\item{writefiles}{logical; write file or return a dataframe?}
}
\value{
a data.frame if writefiles == F
}
\description{
\subsection{Write npt file for CE-QUAL-W2}{
}
}
\examples{
makeW2.input.file()
}
\author{
Norman Buccola
}
\keyword{CEQUALW2}
\keyword{files}
\keyword{model}
\keyword{npt}
\keyword{write}
