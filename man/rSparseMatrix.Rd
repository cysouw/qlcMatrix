\name{rSparseMatrix}
\alias{rSparseMatrix}

\title{
Construct a random sparse matrix
}

\description{
This convenience function constructs a random sparse matrix of specified size, with specified sparsity. This is mainly useful for testing speed and memory load of sparse matrix manipulations
}

\usage{
rSparseMatrix(nrow, ncol, nnz, 
	rand.x = function(n) round(rnorm(nnz), 2), ...)
}

\arguments{
  \item{nrow}{
number of rows of the resulting matrix.
}
  \item{ncol}{
number of columns of the resulting matrix.
}
  \item{nnz}{
number of entries of the resulting matrix.
}
  \item{rand.x}{
randomization used for the construction of the entries. if \code{NULL} then a pattern matrix is constructed (random entries without values).
}
  \item{...}{
Other arguments passed to \code{sparseMatrix} internally.
}
}

\details{
The sparsity of the resulting matrix (i.e. the fraction of non-zero entries to all entries) is \eqn{\frac{nnz}{nrow * ncol}}{nnz/(nrow*ncol)}.
}

\value{
Returns a sparse matrix of the type \code{dgCMatrix}. Defaults to random numeric entries with two decimal digits, generated randomly from a normal distribution with \code{mean = 0} and \code{sd = 1}.

When \code{rand.x = NULL} then the result is a patter\strong{n} matrix of type \code{ngCMatrix}.
}

\author{
Martin Maechler with slight tweaks by Michael Cysouw
}

\seealso{
For random permutation matrices, see \code{\link[Matrix]{pMatrix-class}}. Specifically note the construction option \cr
\code{(p10 <- as(sample(10),"pMatrix"))}.
}

\examples{
# example with reasonably large (100.000 by 100.000) but sparse matrix
# (only one in 10.000 entries is non-zero). On average 10 entries per column.
X <- rSparseMatrix(1e5, 1e5, 1e6)
print(object.size(X), units = "auto")

# speed of cosine similarity
system.time(M <- cosSparse(X))

# reduce memory footprint by removing low values
print(object.size(M), units = "auto")
M <- drop0(M, tol = 0.1)
print(object.size(M), units = "auto")
}
