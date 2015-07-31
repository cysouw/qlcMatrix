\name{Array}
\alias{Array}
\alias{sparseArray}
\alias{as.Matrix}
\title{
Sparse Arrays ("Tensors")
}
\description{
Convenient function linking sparse Arrays from the package \code{spam} to the sparse Matrices from the package \code{Matrix}.
}
\usage{
Array(A)
sparseArray(i, v = NULL, ...)

as.Matrix(M)
}
\arguments{
  \item{A}{
An array to be turned into a sparse Array using \code{as.simple_sparse_array}. Can also be a \code{dataframe}, but see Details below about the treatment of data frames here.
}
  \item{i}{
Integer matrix of array indices passed to \code{simple_sparse_array}.
}
  \item{v}{
vector of values passed to \code{simple_sparse_array}. If \code{NULL} (by default), all specified indices (i.e. all rows in \code{i}) are given the value 1.
}
  \item{M}{
Matrix of type \code{simple_triple_matrix} from the package \code{spam} to be turned into a \code{TsparseMatrix} from the packages \code{Matrix}.
}
  \item{...}{
Further arguments passed to \code{simple_sparse_array}.
  }
}
\details{
\code{Array} turns an \code{array} into a sparse Array. There is a special behavior when a \code{dataframe} is supplied. Such a dataframe is treated as `long format`, i.e. the columns of the dataframe are treated as dimensions of the Array, and all rows of the dataframe are interpreted as entries. The coordinates are given by the ordering of the levels in the dataframe, and the dimnames are given by the levels.

\code{sparseArray} constructs sparse Arrays from a matrix of indices and a vector of values. dim and dimnames can be added as in \code{\link{simple_sparse_array}}

\code{as.Matrix} turns a \code{simple_triplet_matrix} into a \code{dgTMatrix}.
}
\value{
Sparse Arrays use the class "simple_sparse_array" from \code{spam}
}
\author{
Michael Cysouw
}
\note{
These functions are only an example of how \code{spam} can be linked to \code{Matrix}.
}
\examples{
x <- matrix(c(1, 0, 0, 2), nrow = 2)
s <- as.simple_triplet_matrix(x)
str(s)

as.Matrix(s)
str(as.Matrix(s))
}

