% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/steinman.site.R
\name{steinman.site}
\alias{steinman.site}
\title{Estimate site index of spruce/fir using Steinman equations}
\usage{
steinman.site(ht, age)
}
\arguments{
\item{ht}{- mean top height (H40)}

\item{age}{- estimated age of origin}
}
\value{
Estimated site index at base age 50
}
\description{
VSteinman, J.R. 1992. A comprehensive evaluation of spruce-fir growth and yield in Maine as related to physical and chemical soil properties. Ph.D. thesis, University of Maine, Orono, Maine.
}
\details{
Site Index
}
\examples{
steinman.site(65,42)
}
\author{
Premer, M.I. - Maine Forest Lab
}
