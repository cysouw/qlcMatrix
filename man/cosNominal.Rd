\name{cosNominal}
\alias{cosNominal}
\alias{assocNominal}
\alias{cosCol}
\alias{cosRow}
\alias{assocCol}
\alias{assocRow}
\title{
Associations-measures for sparsely encoded nominal variables
}
\description{
Nominal variables can be encoded as a combination of a sparse incidence and index matrix. Various functions to compute variations of \code{assocSparse} and \code{cosSparse} for such data are described here.
}
\usage{
cosCol(X, colGroupX, Y = NULL, colGroupY = NULL, norm = norm2 )
assocCol(X, colGroupX, Y = NULL, colGroupY = NULL, method = res, sparse = TRUE)

cosRow(X, rowGroup, Y = NULL, norm = norm2 , weight = NULL)
assocRow(X, rowGroup, Y = NULL, method = res)
}
\arguments{
  \item{X,Y}{
  	sparse matrices in a format of the \code{Matrix} package, typically \code{dgCMatrix} . When \code{Y = NULL}, then the similarity between the columns of X and itself will be taken. If Y is specified, the similarity between the columns of X and the columns of Y will be calculated.
  }
  \item{colGroupX, colGroupY}{
  	sparse matrices (typically pattern matrices) with the same number of columns as X and Y, respectively, indicating which columns belong to the same group. Each row of these matrices represents a group. 
  }
  \item{rowGroup}{
  	sparse matrix (typically pattern matrices) with the same number of rows as X (and Y when not NULL), indicating which rows belong to the same group. Each column of these matrices represents a group.
}
  \item{norm}{
  	norm to be used. See \code{\link{cosSparse}} for details.
  }
  \item{weight}{
  	weighting of rows. See \code{\link{cosSparse}} for details. Note that row-weighting only makes sense with \code{cosRow}.
  }
  \item{method}{
  	method to be used. See \code{\link{assocSparse}} for details.
  }
  \item{sparse}{
  	All methods try to be as sparse as possible. Specifically, when there are no observed co-occurrence, then nothing is computed. This might lead to slight deviations in the results for some methods. Set \code{sparse=F} to force computation for all cells. This leads to non-sparse results, so use with caution with large datasets.
  }
}
\details{
The approaches \code{assoc} and \code{cos} are described in detail in \code{\link{assocSparse}} and \code{\link{cosSparse}}, respectively. Those methods are extended here in case either the columns (\code{.col}) or the rows (\code{.row}) form groups. Specifically, this occurs with sparse encoding of nominal variables (see \code{\link{splitTable}}). In such encoding, the different values of a nominal variable are encoded in separate columns. However, these columns cannot be treated independently, but have to be treated as groups.

The \code{.col} methods should be used when similarities between the different values of nominal variables are to be computed. The \code{.row} methods should be used when similarities between the observations of nominal variables are to be computed.

Note that the calculations of the \code{assoc} functionsreally only makes sense for binary data (i.e. matrices with only ones and zeros). Currently, all input is coerced to such data by \code{as(X, "nMatrix")*1}, meaning that all values that are not one or zero are turned into one (including negative values!).

}
\value{
When \code{Y = NULL}, then all methods return symmetric similarity matrices in the form \code{dsCMatrix}, only specifying the upper triangle. The only exception is when \code{sparse=T} is chose, then the result will be in the form \code{dsyMatrix}.

When a second matrix Y is specified, the result will be of the kind \code{dgCMatrix} or \code{dgeMatrix}, respectively.
}
\note{
Note that these methods automatically take missing data into account. They also work with large amount of missing data, but of course the validity of any similarity with much missing data is problematic.
}
\author{
Michael Cysouw
}
\seealso{
\code{\link{sim.att}, \link{sim.obs}} for convenient shortcuts around these methods.
}
\examples{
# convenience functions are easiest to use
# first a simple example using the farms-dataset from MASS
library(MASS)

# to investigate the relation between the individual values
# This is similar to Multiple Correspondence Analysis (see mca in MASS)
f <- splitTable(farms)
s <- assocCol(f$OV,f$AV)
rownames(s) <- f$values
plot(hclust(as.dist(-s)))
}