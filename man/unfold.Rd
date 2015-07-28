\name{unfold}
\alias{unfold}
\alias{unfold_to_matrix}
\alias{tenmat}
\title{
Unfolding of Arrays
}
\description{
Multidimensional Arrays ("Tensors") can be unfolded, i.e. multiple dimensions can be combined into a single dimension in a block-wise fashion. Such unfoldings are central to tensor decomposition. In general, computations on tensors are regularly performed by reducing tensors to matrices ("2-dimensional tensors") and then use regular matrix algebra.
}
\usage{
unfold(x, MARGINS)

unfold_to_matrix(x, ROWS, COLS = NULL)
tenmat(x, ROWS, COLS = NULL)
}
\arguments{
  \item{x}{
Sparse array to be unfolded, using \code{simple_sparse_array} from the package \code{spam}.
}
  \item{MARGINS}{
Margins ("dimensions") to be unfolded. The margins specified will be turned into a single dimension, to be added as the last dimension of the resulting array (see Details).
}
  \item{ROWS}{
Margins of the original array to be unfolded into the rows of the resulting matrix.
}
  \item{COLS}{
Margins of the original array to be unfolded into the columns of the resulting matrix. If \code{NULL}, then all remaining margins, not included in \code{ROWS} are unfolded here.
  }
}
\details{
The function \code{unfold} is a general approach to combining of multiple dimensions into a single dimensions. The function \code{unfold_to_matrix} is a special case in which the result is a 2-dimensional matrix. This second function is made to emulate the functionality of the \code{tenmat} ("tensor to matrix") from the Matlab Tensor Toolbox. For convenience, the function-name \code{tenmat} is also added as a synonym for \code{unfold_to_matrix}.

Unfolding basically works by interspercing margins subsequently. E.g. margin A of size 3 (A1, A2, A3) and a margin B of size 2 (B1, B2) are unfolded through \code{c(A,B)} as (A1B1, A2B1, A3B1, A1B2, A2B2, A3B2), but they are unfolded through \code{c{B,A}} as (B1A1, B2A1, B1A2, B2A2, B1A3, B2A3).
}
\value{
\code{unfold} returns a \code{simple_sparse_array} with the new combined dimension added as the last dimension. All original dimensions are shifted forward. The relation between the original dimensions and the new dimensions is stored as an \code{permutation} attribute, e.g. try \code{attr(x, "p")}. When multiple unfoldings are performed after each other, these permutations can be subsetted on each other to obtain the final permutation. See examples below.

\code{unfold_to_matrix} and \code{tenmat} return a sparse matrix of class \code{dgTMatrix}.
}
\references{
see \url{http://www.cs.cornell.edu/cv/SummerSchool/Unfold.pdf} for some notes on tensor unfolding. The Matlab Tensor Toolbox can be found at \url{http://www.sandia.gov/~tgkolda/TensorToolbox/index-2.6.html}. A newer Matlab implementation is \url{http://www.tensorlab.net}.
}
\author{
Michael Cysouw <cysouw@mac.com>
}
\examples{
# example from \url{http://www.cs.cornell.edu/cv/SummerSchool/Unfold.pdf}
x <- array(c(111, 211, 311, 411, 121, 221, 321, 
    421, 131, 231, 331, 431, 112, 212, 312, 412, 
    122, 222, 322, 422, 132, 232, 332, 432), dim = c(4, 3, 2))
x

s <- as.simple_sparse_array(x)
( s1  <- as.array(unfold_to_matrix(s,1)) )

# note this is identical to:
( s23 <- as.array(unfold(s,c(2,3))) )
all.equal(s23, s1)

# larger example from same source
x <- array(0, dim = c(2,3,2,2,3))
x[1,2,1,1,2] <- 12112
x[2,3,1,2,2] <- 23122
x[2,2,2,1,1] <- 22211
x[2,2,1,2,3] <- 22123
s <- as.simple_sparse_array(x)

as.array(unfold_to_matrix(s, c(1,2,3), c(4,5)))

# use attribute "permutation" to track dimensions
# first step: unfold 1,2,3 to become dimension 3
# original dimensions 4,5 now become 1,2
s1 <- unfold(s, c(1,2,3))
( p1 <- attr(s1, "permutation") )

# now take these dimension 1,2 (originally 4,5) and unfold them
s2 <- unfold(s1, c(1,2))
( p2 <- attr(s2, "permutation") )

# use subsetting to track dimensions through subsequent unfolding
p2[p1]
}
\keyword{array}
\keyword{tensor}
\keyword{unfolding}
