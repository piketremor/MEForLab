% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ek.monserud.index.R
\name{ek.monserud.index}
\alias{ek.monserud.index}
\title{Calculate Ek and Monserud Competition Index}
\usage{
ek.monserud.index(
  overlap,
  heightj,
  crownwidthj,
  crownwidthi,
  heighti,
  focal_area
)
}
\arguments{
\item{overlap}{- overlap (ft2) of the focal and competitor tree}

\item{heightj}{- height (ft) of competitor tree}

\item{crownwidthj}{- crown width (ft) of the competitor tree}

\item{crownwidthi}{- crown width (ft) of the focal tree}

\item{heighti}{- height (ft) of focal tree}

\item{MCW}{- maximum crown width (ft)}
}
\value{
Ek and Monserud Index
}
\description{
Ek and Monesrud
}
\examples{
ek.monserud.index(60,45,20,20,45,130)
}
\references{
Ek, A.R., Monserud, R.A. 1974. FOREST: A model for the growth and reproduction of forest stands. Dept. of Forestry. University of Wisconsin - Madison. Research Report. 93 p.
}
\author{
Premer, M.I. - Maine Forest Lab
}
