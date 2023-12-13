\name{rohirrim}
\alias{rohirrim}
\title{Calculates Over, Under-prediction rates and Potential Presence and Absence of SDM's}
\description{Evaluate SDM performance with Over-prediction, Under-prediction rates, and predict Potential Presence and Absence incremente in a geographic area.}
\usage{
rohirrim(obj)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{An data.frame object containing true positives (tp), false positives (fp),
  false negatives (fn), and true negatives (tn)}
}
\details{This function used the confMaxtrix from SDMtune package, but, any data.frame with the objects will work.}
\value{Return a data.frame}
\author{Rogério Nunes}
\examples{
 obj <- list(tp = 10, fp = 5, fn = 3, tn = 20)
#' result <- rohirrim(obj)
#' print(result)
}

\references{
\itemize{
\item Márcia Barbosa, A., Real, R., Muñoz, A.-.-R. and Brown, J.A. (2013), New measures for assessing model equilibrium and prediction mismatch in species distribution models. Diversity Distrib., 19: 1333-1338. https://doi.org/10.1111/ddi.12100.}
}
