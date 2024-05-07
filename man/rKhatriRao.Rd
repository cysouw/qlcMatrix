\name{rKhatriRao}
\alias{rKhatriRao}
\title{
`reduced' Khatri-Rao product (sparse matrices)
}
\description{
This function performs a Khatri-Rao product (`column-wise Kronecker product', see \code{\link[Matrix]{KhatriRao}} for more info) on two sparse matrices. However, the result of such a product on sparse matrices normally results in very many empty rows. This function removes those empty rows, and, most importantly, it produces row names only for the remaining rows. For large sparse matrices this is \emph{much} more efficient than first producing all rownames, and then removing the one with the empty rows.
}
\usage{
rKhatriRao(X, Y, 
	rownamesX = rownames(X), rownamesY = rownames(Y), 
	simplify = FALSE, binder = ":", FUN = "*")
}
\arguments{
  \item{X,Y}{
  matrices of with the same number of columns.
}
  \item{rownamesX, rownamesY}{
  row names of matrices X and Y. These can be specified separately, but they default to the row names of the matrices.
}
  \item{simplify}{
  by default, the names of rows and columns are not included into the matrix to keep the matrix as lean as possible: the row names are returned separately. Using \code{include.dimnames=T} adds the row names into the matrix. The column names are directly taken from X.
}
  \item{binder}{
  symbol to include between the row names of X and Y for the resulting matrix
}
  \item{FUN}{
  function to be used in the KhatriRao product, passed internally to the workhorse \code{\link{KhatriRao}}
}
}
\details{
Up to 1e6 row names to be produced goes reasonably quick with the basic \code{\link{KhatriRao}} function. However, larger amounts of pasting of row names becomes very slow, and the row names take an enormous amount of RAM. This function solves that problem by only producing row names for the non-empty rows.
}
\value{
By default, the result is a list of two items:
  \item{M}{resulting sparse product matrix with empty rows removed}
  \item{rownames}{a vector with the resulting row names for the non-empty rows}
 
When \code{simplify=T}, then the matrix is return with the row names included.
}
\author{
Michael Cysouw
}
\note{
This function allows for the row names of the input matrices to be added separately, and the resulting row names are returned separately by default. This might seem a bit unusual, given the nice way how R integrates row names into matrices. However, it turns out often to be easier to store row- and column names separately to efficiently work with large sparse matrices.
}
\seealso{
\code{\link[Matrix]{KhatriRao}}
}
\examples{
# two sparse matrices with row names

X <- rSparseMatrix(1e4, 1e3, 1e4)
Y <- rSparseMatrix(1e4, 1e3, 1e4)

rownames(X) <- 1:nrow(X)
rownames(Y) <- 1:nrow(Y)

# the basic KhatriRao product from the Matrix package is very fast
# but almost all rows are empty

system.time(M <- KhatriRao(X, Y))
sum(rowSums(M)==0)/nrow(M) # 99.9\% empty rows

# To produce all row names takes a long time with KhatriRao from Matrix
# with the current example with 1e8 row names it took a minute on my laptop
# so: don't try the following, except on a large machine!

\donttest{
system.time(M <- KhatriRao(X, Y, make.dimnames = TRUE))
}

# Using the current special version works just fine and is reasonably quick
system.time(M <- rKhatriRao(X, Y))

}
