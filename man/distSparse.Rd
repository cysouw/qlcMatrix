\name{distSparse}
\alias{distSparse}
\title{
Sparse distance matrix calculations
}
\description{
Sparse alternative to base \code{\link{dist}} function. WARNING: the result is not a distance metric, see details! Also: distances are calculated between columns (not between rows, as in the base \code{dist} function).
}
\usage{
distSparse(M, method = "euclidean", diag = FALSE)
}
\arguments{
  \item{M}{
a sparse matrix in a format of the \code{Matrix} package, typically \code{dMatrix}. Any other matrices will be converted to such a sparse Matrix. The correlations will be calculated between the columns of this matrix (different from the base \code{dist} function!)
}
  \item{method}{
method to calculate distances. Currently only \code{"euclidean"} is supported.
}
  \item{diag}{
should the diagonal be included in the results?
}
}
\details{
A sparse distance matrix is a slightly awkward concept, because distances of zero are rare in most data. Further, it is mostly the small distances that are of interest, and not the large distanes (which are mostly also less trustwhorthy). Note that for random data, this assumption is not necessarily true.

To obtain sparse results, the current implementation takes a special approach. First, only those distances will be calculated for which there is at least some non-zero data for both columns. The assumption is taken that those distances will be uninteresting (and relatively large anyway).

Second, to differentiate the non-calculated distances from real zero distances, the distances are converted into similarities by substracting them from the maximum. In this way, all non-calculated distances are zero, and the real zeros have value \code{max(M)}.

Euclidean distances are calculated using the following trick:
\deqn{colSums(M^2) + rowSums(M^2) - 2 * M'M}
}
\value{
A symmetric matrix of type \code{dsCMatrix}, consisting of similarity(!) values instead of distances (viz. \code{max(dist)-dist}).
}
\note{
Please note:
\itemize{
  \item{The values in the result are not distances, but similarities computed as \code{max(dist)-dist}.}
  \item{Non-calculated values are zero.}
}
}
\author{
Michael Cysouw <cysouw@mac.com
}
\seealso{
See Also as \code{\link{dist}}.
}
\examples{
# to be done
}

