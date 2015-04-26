\name{unfold}
\alias{unfold}

\title{
Unfolding of block matrices (sparse matrices)
}
\description{
Utility function for some matrix manipulations for sparse block matrices.
}
\usage{
unfold(X, colGroups, rowGroups = NULL)
}
\arguments{
  \item{X}{
Sparse block matrix to be unfolded into the individual blocks
	}
  \item{colGroups, rowGroups}{
either vectors with group indices of the columns and rows, or sparse pattern matrices with the groups as rows and the columns/rows of the X matrix as columns. See example below. 
	}
}
\details{
For some sparse manipulation it turns out the profitable to `unfold' sparse block matrices, i.e. to separate the blocks into their own rows and columns. Each block can then separately be manipulated by using matrix products with diagonal matrices (see example below). For convenience, the function also returns two matrices to `refold' the unfolded matrix. Specifically, X = L \%*\% U \%*\% R
}
\value{
When \code{rowGroups != NULL} then the result is a list of three matrices:
  \item{U}{The unfolded block matrix}
  \item{L}{The left matrix for refolding the unfolded matrix}
  \item{R}{The right matrix for refolding the unfolded matrix}
  
When \code{rowGroups = NULL} then the R matrix is not returned, and the refolding works with only the L matrix: X = L \%*\% U.
}
\author{
Michael Cysouw
}
\note{
The use of \code{kronecker} for sparse matrices in this function often leads to warnings about the sparse format of the resulting matrices. These warnings can be ignored.
}
\seealso{
This is used in the sparse computation of \code{\link{assocCol}} to divide each block by a different N.
}
\examples{
# specially prepared block matrix. For illustration purpuse, this one is not sparse
( X <- Matrix( c( rep(c(1,1,1,1,2,2,3,3,3,4),3),
		rep(c(5,5,5,5,6,6,7,7,7,8),2)),10,5, sparse = TRUE) )

# this matrix has two column groups, and four row groups
# groups can be specified as sparse matrices, or as grouping vectors
colG <- ttMatrix(c(1,1,1,2,2))$M*1
rowG <- ttMatrix(c(1,1,1,1,2,2,3,3,3,4))$M*1

# unfold the matrix, with the result that each block has it's own rows/columns
# the $L and $R matrices can be used to refold the matrix to it's original state
( M <- unfold(X, colG, rowG) )

# unfold and refold back: result is identical
with(M, all.equal(X, L \%*\% U \%*\% R) )

# Unfolded, each block can be easily reached for computations using diagonal matrices
# for example, multiply each block by the sum of its cells, and then fold back again
# this is a trick to apply computations to blocks in sparse block matrices
sums <- drop(crossprod(kronecker(Diagonal(nrow(colG)),rowG)) \%*\% rowSums(M$U))
S <- Diagonal( x = sums )

with(M, L \%*\% S \%*\% U \%*\% R )
}

