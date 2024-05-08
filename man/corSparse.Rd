\name{corSparse}
\alias{corSparse}

\title{
Pearson correlation between columns (sparse matrices)
}

\description{
This function computes the product-moment correlation coefficients between the columns of sparse matrices. Performance-wise, this improves over the approach taken in the \code{\link[stats]{cor}} function. However, because the resulting matrix is not-sparse, this function still cannot be used with very large matrices.
}
\usage{
corSparse(X, Y = NULL, cov = FALSE)
}

\arguments{
  \item{X}{
a sparse matrix in a format of the \code{Matrix} package, typically \code{dgCMatrix} . The correlations will be calculated between the columns of this matrix.
}
  \item{Y}{
a second matrix in a format of the \code{Matrix} package. When \code{Y = NULL}, then the correlations between the columns of X and itself will be taken. If Y is specified, the association between the columns of X and the columns of Y will be calculated.
}
  \item{cov}{
when \code{TRUE} the covariance matrix is returned, instead of the default correlation matrix.
}
}

\details{
To compute the covariance matrix, the code uses the principle that
\deqn{E[(X - \mu(X))' (Y - \mu(Y))] = E[X' Y] - \mu(X') \mu(Y)}
With sample correction n/(n-1) this leads to the covariance between X and Y as
\deqn{( X' Y - n * \mu(X') \mu(Y) ) / (n-1)}

The computation of the standard deviation (to turn covariance into correlation) is trivial in the case \code{Y = NULL}, as they are found on the diagonal of the covariance matrix. In the case \code{Y != NULL} uses the principle that 
\deqn{E[X - \mu(X)]^2 = E[X^2] - \mu(X)^2}
With sample correction n/(n-1) this leads to 
\deqn{sd^2 = ( X^2 - n * \mu(X)^2 ) / (n-1)}
}

\value{
The result is a regular square (non-sparse!) Matrix with the Pearson product-moment correlation coefficients between the columns of \code{X}. 

When \code{Y} is specified, the result is a rectangular (non-sparse!) Matrix of size \code{nrow(X)} by \code{nrow(Y)} with the correlation coefficients between the columns of \code{X} and \code{Y}.

When \code{cov = T}, the result is a covariance matrix (i.e. a non-normalized correlation).
}

\author{
Michael Cysouw

Slightly extended and optimized, based on the code from a discussion at \href{https://stackoverflow.com/questions/5888287/running-cor-or-any-variant-over-a-sparse-matrix-in-r}{stackoverflow}.
}

\note{
Because of the `centering' of the Pearson correlation, the resulting Matrix is completely filled. This implies that this approach is normally not feasible with resulting matrices with more than 1e8 cells or so (except in dedicated computational environments with lots of RAM). However, in most sparse data situations, the cosine similarity \code{\link{cosSparse}} will almost be identical to the Pearson correlation, so consider using that one instead. For a comparison, see examples below.

For further usage, the many small coefficients are often unnecessary anyway, and can be removed for reasons of sparsity. Consider something like \code{M <- drop0(M, tol = value)} on the resulting \code{M} matrix (which removes all values between -value and +value). See examples below.
}

\seealso{
\code{\link[stats]{cor}} in the base packages, \code{\link{cosSparse}}, \code{\link{assocSparse}} for other sparse association measures.
}

\examples{

# reasonably fast (though not instantly!) with
# sparse matrices 1e4x1e4 up to a resulting matrix size of 1e8 cells.
# However, the calculations and the resulting matrix take up lots of memory

X <- rSparseMatrix(1e3, 1e3, 1e4)
system.time(M <- corSparse(X))
print(object.size(M), units = "auto") # more than 750 Mb

# Most values are low, so it often makes sense 
# to remove low values to keep results sparse

M <- drop0(M, tol = 0.4)
print(object.size(M), units = "auto") # normally reduces size to about a quarter
length(M@x) / prod(dim(M)) # down to less than 0.01\% non-zero entries

\donttest{
# comparison with other methods
# corSparse is much faster than cor from the stats package
# but cosSparse is even quicker than both!
# do not try the regular cor-method with larger matrices than 1e3x1e3
X <- rSparseMatrix(1e3, 1e3, 1e4)
X2 <- as.matrix(X)

# if there is a warning, try again with different random X
system.time(McorRegular <- cor(X2)) 
system.time(McorSparse <- corSparse(X))
system.time(McosSparse <- cosSparse(X))

# cor and corSparse give identical results
all.equal(McorSparse, McorRegular)

# corSparse and cosSparse are not identical, but close
McosSparse <- as.matrix(McosSparse)
dimnames(McosSparse) <- NULL
all.equal(McorSparse, McosSparse) 

# Actually, cosSparse and corSparse are *almost* identical!
cor(as.dist(McorSparse), as.dist(McosSparse))

# So: consider using cosSparse instead of cor or corSparse.
# With sparse matrices, this gives mostly the same results, 
# but much larger matrices are possible
# and the computations are quicker and more sparse
}
}
