\name{dimRed}
\alias{dimRed}
\title{
Dimensionality Reduction for sparse matrices, based on Cholesky decomposition
}
\description{
To inspect the structure of a large sparse matrix, it is often highly useful to reduce the matrix to a few major dimensions (cf. multidimensional scaling). This functions implements a rough approach to provide a few major dimensions. The function provides a simple wrapper around \code{\link[Matrix]{Cholesky}} and \code{\link[sparsesvd]{sparsesvd}}.
}
\usage{
dimRed(sim, k = 2, method = "svd")
}
\arguments{
  \item{sim}{
Sparse, symmetric, positive-definite matrix (typically a similarity matrix produces by \code{sim} or \code{assoc} functions)
}
  \item{k}{
Number of dimensions to be returned, defaults to two.
}
  \item{method}{
Method used for the decomposition. Currently implemted are \code{svd} and \code{cholesky}. 
}
}
\details{
Based on the Cholesky decomposition, the Matrix \code{sim} is decomposed into:
\deqn{ L D L'}
The D Matrix is a diagonal matrix, the values of which are returned here as \code{$D}. Only the first few columns of the L Matrix are returned (possibly after permutation, see the details at \code{\link[Matrix]{Cholesky}}).

Based on the svd decomposition, the Matrix \code{sim} is decomposed into:
\deqn{ U D V}
The U Matrix and the values from D are returned.
}
\value{
A list of two elements is returned:
\item{L }{: a sparse matrix of type \code{dgCMatrix} with \code{k} columns}
\item{D }{: the diagional values from the Cholesky decomposition, or the eigenvalues from the svd decomposition}
}
\author{
Michael Cysouw <cysouw@mac.com>
}
\seealso{
See Also as \code{\link[Matrix]{Cholesky}} and \code{\link[sparsesvd]{sparsesvd}}
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

oldpar<-par("mfrow")
par(mfrow = c(1,3))

  plot(coor, type = "n", axes = FALSE, xlab = "", ylab = "")
  text(coor, labels = 1:30)
  title("Original coordinates")
  
  plot(mds, type = "n", axes = FALSE, xlab = "", ylab = "")
  text(mds, labels = 1:30)
  title("MDS from euclidean distances")
  
  plot(red, type = "n", axes = FALSE, xlab = "", ylab = "")
  text(red, labels = 1:30)
  title("dimRed from cosSparse similarity")

par(mfrow = oldpar)

# ======

# example, using the iris data
data(iris)
X <- t(as.matrix(iris[,1:4]))
cols <- rainbow(3)[iris$Species]

s <- cosSparse(X, norm = norm1)
d <- dist(t(X), method = "manhattan")

svd <- as.matrix(dimRed(s, method = "svd")$L)
mds <- cmdscale(d)

oldpar<-par("mfrow")
par(mfrow = c(1,2))
  plot(mds, col = cols, main = "cmdscale\nfrom euclidean distances")
  plot(svd, col = cols, main = "dimRed with svd\nfrom cosSparse with norm1")
par(mfrow = oldpar)
}
