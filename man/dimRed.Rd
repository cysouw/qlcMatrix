\name{dimRed}
\alias{dimRed}
\title{
Dimensionality Reduction for sparse matrices, based on Cholesky decomposition
}
\description{
To inspect the structure of a large sparse matrix, it is often highly useful to reduce the matrix to a few major dimensions (cf. multidimensional scaling). This functions implements a rough approach to provide a few major dimensions. The function is actually just a simple wrapper around \code{\link{Cholesky}}.
}
\usage{
dimRed(sim, k = 2)
}
\arguments{
  \item{sim}{
Sparse, symmetric, positive-definite matrix (typically a similarity matrix produces by \code{sim} or \code{assoc} functions)
}
  \item{k}{
Number of dimensions to be returned, defaults to two.
}
}
\details{
Based on the Cholesky decomposition, the Matrix \code{sim} is decomposed into:
\deqn{ L D L'}
The D Matrix is a diagonal matrix, the values of which are returned here as \code{$D}. Only the first few columns of the L Matrix are returned (possibly after permutation, see the details at \code{\link{Cholesky}}).
}
\value{
A list of two elements is returned:
\item{L }{: a sparse matrix of type \code{dgCMatrix} with \code{k} columns}
\item{D }{: the diagional values from the Cholesky decomposition, cf. eigenvalues}
}
\author{
Michael Cysouw <cysouw@mac.com>
}
\seealso{
See Also as \code{\link{Matrix::Cholesky}}
}
\examples{
# some random points in two dimensions
coor <- cbind(sample(1:30), sample(1:30))

# using cmdscale() to reconstruct the coordinates from a distance matrix
d <- dist(coor)
mds <- cmdscale(d)

# using dimRed() on a similarity matrix.
# Note that normL works much better than other norms in this 2-dimensional case
s <- cosSparse(t(coor), norm = normL)
red <- as.matrix(dimRed(s)$L)

# show the different point clouds

par(mfrow = c(1,3))

  plot(coor, type = "n", axes = F, xlab = "", ylab = "")
  text(coor, labels = 1:30)
  title("Original coordinates")
  
  plot(mds, type = "n", axes = F, xlab = "", ylab = "")
  text(mds, labels = 1:30)
  title("MDS from euclidean distances")
  
  plot(red, type = "n", axes = F, xlab = "", ylab = "")
  text(red, labels = 1:30)
  title("dimRed from cosSparse similarity")

par(mfrow = c(1,1))

# ======

# example, using the iris data
data(iris)
X <- t(as.matrix(iris[,1:4]))
cols <- rainbow(3)[iris$Species]

sL <- cosSparse(X, norm = normL)
s1 <- cosSparse(X, norm = norm1)
d <- dist(t(X), method = "manhattan")

cL <- as.matrix(dimRed(sL)$L)
c1 <- as.matrix(dimRed(s1)$L)
mds <- cmdscale(d)

par(mfrow = c(1,3))
  plot(mds, col = cols, main = "cmdscale from euclidean distances")
  plot(cL, col = cols, main = "dimRed from cosSparse with normL")
  plot(c1, col = cols, main = "dimRed from cosSparse with norm1")
par(mfrow = c(1,1))
}
