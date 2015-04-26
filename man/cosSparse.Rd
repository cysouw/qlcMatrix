\name{cosSparse}
\alias{cosSparse}
\alias{cosMissing}
\alias{idf}
\alias{isqrt}
\alias{none}
\alias{norm2}
\alias{norm1}

\title{
Cosine similarity between columns (sparse matrices)
}
\description{
\code{cosSparse} computes the cosine similarity between the columns of sparse matrices. Different normalizations and weightings can be specified. Performance-wise, this strongly improves over the approach taken in the \code{\link{corSparse}} function, though the results are almost identical for large sparse matrices. \code{cosMissing} adds the possibility to deal with large amounts of missing data.
}
\usage{
cosSparse(X, Y = NULL, norm = norm2, weight = NULL)
cosMissing(X, availX, Y = NULL, availY = NULL, norm = norm2 , weight = NULL)
}

\arguments{
  \item{X}{
a sparse matrix in a format of the \code{Matrix} package, typically \code{dgCMatrix} . The similarity will be calculated between the columns of this matrix.
}
  \item{Y}{
a second matrix in a format of the \code{Matrix} package. When \code{Y = NULL}, then the similarity between the columns of X and itself will be taken. If Y is specified, the similarity between the columns of X and the columns of Y will be calculated.
}
  \item{availX, availY}{
sparse Matrices (typically pattern matrices) of the same size of X and Y, respectively, indicating the available information for each matrix.
}
  \item{norm}{
The function to be used for the normalization of the columns. Currently \code{norm2} (euclidean norm) and \code{norm1} (manhattan norm) are available, but further methods can be easily specified by the user. See details for more information.
}
  \item{weight}{
The function to be used for the weighting of the rows. Currently \code{idf} (inverse document frequency) and \code{isqrt} (inverse square root) are available, but further methods can be easily specified by the user. See details for more information.
}
}
\details{
This measure is called a `cosine' similarity as it computes the cosine of the angle between high-dimensional vectors. It can also be considered a Pearson correlation without centering. Because centering removes sparsity, and because centering has almost no influence for highly sparse matrices, this cosine similarity performs much better that the Pearson correlation, both related for speed and memory consumption.

The variant \code{cosMissing} can be used when the available information itself is also sparse. In such a situation, a zero in the data matrix X, Y can mean either `zero value' or `missing data'. To deal with the missing data, matrices indicating the available data can be specified. Note that this really only makes sense when the available data is sparse itself. When, say, 90\% of the data is available, the \code{availX} matrix becomes very large, and the results does not differ strongly from the regular \code{cosSparse}, i.e. ignoring the missing data.

Different normalizations of the columns and weightings of the rows can be specified. 

The predefined normalizations are defined as a function of the matrix x and a `summation function' s (to be specified as a sparse matrix or a vector). This slight complexity is needed to be able to deal with missing data. With complete data, then \code{s = rep(1,nrow(X))}, leads to \code{crossprod(X,s) == colSums(X)}.

  \itemize{
    \item{\code{norm2}: 
euclidean norm. The default setting, and the same normalization as used in the Pearson correlation. It is defined as \cr 
\code{norm2 <- function(x,s) \{ drop(crossprod(x^2,s)) ^ 0.5 \} }.
    }

    \item{\code{norm1}:
Manhattan, or taxi-cab norm, defined as \cr 
\code{norm1 <- function(x,s) \{ abs(drop(crossprod(x,s))) \} }.
    }
  }
 
The predefined weightings are defined as a function of the frequency of a row (s) and the number of columns (N):

  \itemize{
    \item{\code{idf}: 
inverse document frequency, used typically in distributional semantics to down-weight high frequent rows. It is defined as \cr 
\code{idf <- function(s,N) \{ log(N/(1+s)) \} }.
    }
    \item{\code{isqrt}:
inverse square root, an alternative to idf, defined as \cr 
\code{isqrt <- function(s,N) \{ s^-0.5 \} }.
    }
    \item{\code{none}}:
no weighting. This is only included for use inside later high-level functions (e.g. \code{\link{sim.words}}). Normally, \code{weight = NULL} gives identical results, but is slightly quicker. \cr
\code{none <- function(s,N) \{ s \} }
  }

It is trivial to define further norms of weighting functions. For example, one might define the following norm:

\code{normL <- function(x,s) \{ abs(drop(crossprod(x,s))) ^ (1/2) \} }\cr

When used on a symmetric pattern Matrix X, using this norm will result in a matrix that is known in spectral clustering as a `normalized Laplacian' of the graph X.
}

\value{
The result is a sparse matrix with the non-zero association values. Values range between -1 and +1, with values close to zero indicating low association.

When \code{Y = NULL}, then the result is a symmetric matrix, so a matrix of type \code{dsCMatrix} with size \code{ncol(X)} by \code{ncol{X}} is returned. When \code{X} and \code{Y} are both specified, a matrix of type \code{dgCMatrix} with size \code{ncol(X)} by \code{ncol{Y}} is returned.
}

\author{
Michael Cysouw
}
\note{
For large sparse matrices, consider this as an alternative to \code{\link[stats]{cor}}. See \code{\link{corSparse}} for a  comparison of performance and results.
}

\seealso{
\code{\link{corSparse}}, \code{\link{assocSparse}} for other sparse association measures. See also \code{\link{cosRow}, \link{cosCol}} for variants of cosSparse dealing with nominal data.
}
\examples{
# functional upto limits of RAM of the hardware used
# consider removing small values of result to improve sparsity

\dontrun{

X <- rSparseMatrix(1e6, 1e6, 1e7)
print(object.size(X), units = "auto") # 118 Mb
system.time(M <- cosSparse(X)) # might take half a minute, depending on hardware
print(object.size(M), units = "auto") # 587 Mb
M <- drop0(M, tol = 0.1) # remove small values
print(object.size(M), units = "auto") # reduced to 141 Mb
}

# Compare various weightings.
# with random data from a normal distribution there is almost no difference
#
# data from a normal distribution
# X <- rSparseMatrix(1e2, 1e2, 1e3) 

# with heavily skewed data there is a strong difference!
X <- rSparseMatrix(1e2, 1e2, 1e3,
	rand.x = function(n){round(rpois(1e3, 10), 2)})

w0 <- cosSparse(X, norm = norm2, weight = NULL)@x
wi <- cosSparse(X, norm = norm2, weight = idf)@x
ws <- cosSparse(X, norm = norm2, weight = isqrt)@x

pairs(~ w0 + wi + ws, 
  labels=c("no weighting","inverse\ndocument\nfrequency","inverse\nsquare root"))

}
