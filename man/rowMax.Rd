\name{rowMax}
\alias{rowMax}
\alias{colMax}
\alias{rowMin}
\alias{colMin}

\title{
Row and column extremes (sparse matrices)
}

\description{
Compute maxima and minima for all rows or columns of sparse matrices. Optionally also return which elements are the maxima/minima per row/column.
}
\usage{
rowMax(X, which = FALSE, ignore.zero = TRUE)
colMax(X, which = FALSE, ignore.zero = TRUE)

rowMin(X, which = FALSE, ignore.zero = TRUE)
colMin(X, which = FALSE, ignore.zero = TRUE)
}

\arguments{
  \item{X}{
a sparse matrix in a format of the \code{Matrix} package, typically \code{dgCMatrix} . The maxima or minima will be calculated for each row or column of this matrix.}
  \item{which}{
optionally return a sparse matrix of the same dimensions as \code{X} marking the positions of the columns- or row-wise maxima or minima.
}
  \item{ignore.zero}{
By default, only the non-zero elements are included in the computations. However, when \code{ignore.zero = F} then zeros are also considered. This basically means that for all maxima below zero, the maximum will be set to zero. Likewise, for all minima above zero, the minimum will be set to zero.
}
}
\details{
The basic workhorse of these functions is the `trick':\cr
\code{aggregate(x ~ i, data = summary(X), FUN = max)}\cr
This usage of \code{aggregate} can possibly be used for other row- and column-wise summary statistics of sparse matrices.
}
\value{
By default, these functions returns a \code{\link[Matrix]{sparseVector}} with the non-zero maxima or minima. Eventually use \code{as.vector} to turn this into a regular vector.

When \code{which = T}, the result is a list of two items:

  \item{max/min}{
  	the same sparse vector as described above.
	}
  \item{which}{
  	a sparse pattern matrix of the kind \code{ngCMatrix} indicating the position of the extrema. Note that an extreme might occur more than once per row/column. In that case multiple entries in the row/column are indicated.
	}
}

\author{
Michael Cysouw
}

\examples{
# rowMax(X, ignore.zero = FALSE) is the same as apply(X, 1, max)
X <- rSparseMatrix(1e4, 1e4, 1e5)
system.time(m1 <- rowMax(X, ignore.zero = FALSE))
system.time(m2 <- apply(X, 1, max))
all.equal(as.vector(m1), m2)

# speed depends most strongly on the number of entries in the matrix
# also some performance loss with size of matrix
# up to 1e5 entries is still reasonably fast

X <- rSparseMatrix(1e7, 1e7, 1e5)
system.time(m <- rowMax(X))

\dontrun{
X <- rSparseMatrix(1e7, 1e7, 1e6)
system.time(M <- rowMax(X)) # about ten times as slow
}

# apply is not feasably on such large matrices
# Error: problem too large...
\dontrun{
m <- apply(X, 1, max) 
}
}