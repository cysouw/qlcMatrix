\name{assocSparse}

\alias{assocSparse}
\alias{pmi}
\alias{res}
\alias{poi}
\alias{wpmi}

\title{
Association between columns (sparse matrices)
}

\description{
This function offers an interface to various different measures of association between columns in sparse matrices (based on functions of `observed' and `expected' values). Currently, the following measures are available: pointwise mutual information (aka log-odds), a poisson-based measure and Pearson residuals. Further measures can easily be specifically defined by the user. The calculations are optimized to be able to deal with large sparse matrices. Note that these association values are really only (sensibly) defined for binary data.
}

\usage{
assocSparse(X, Y = NULL, method = res, N = nrow(X), sparse = TRUE )
}

\arguments{
  \item{X}{
a sparse matrix in a format of the \code{Matrix} package, typically a \code{dgCMatrix} with only zeros or ones. The association will be calculated between the columns of this matrix.
}
  \item{Y}{
a second matrix in a format of the \code{Matrix} package with the same number of rows as X. When \code{Y=NULL}, then the associations between the columns of X and itself will be taken. If Y is specified, the association between the columns of X and the columns of Y will be calculated.
}
  \item{method}{
The method to be used for the calculation. Currently \code{res} (residuals), \code{poi} (poisson), \code{pmi} (pointwise mutual information) and \code{wpmi} (weighted pointwise mutual information) are available, but further methods can be specified by the user. See details for more information.
}
  \item{N}{
Variable that is needed for the calculations of the expected values. Only in exceptional situations this should be different from the default value (i.e. the number of rows of the matrix).
}
  \item{sparse}{
By default, nothing is computed when the observed co-occurrence of two columns is zero. This keeps the computations and the resulting matrix nicely sparse. However, for some measures (specifically the Pearson residuals `res') this leads to incorrect results. Mostly the error is negligible, but if the correct behavior is necessary, chose \code{sparse = F}. Note that the result will then be a full matrix, so this is not feasible for large datasets.
}
}
\details{
Computations are based on a comparison of the observed interaction \code{crossprod(X,Y)} and the expected interaction. Expectation is in principle computed as \code{tcrossprod(rowSums(abs(X)),rowSums(abs(Y)))/nrow(X)}, though in practice the code is more efficient than that.

Note that calculating the observed interaction as \code{crossprod(X,Y)} really only makes sense for binary data (i.e. matrices with only ones and zeros). Currently, all input is coerced to such data by \code{as(X, "nMatrix")*1}, meaning that all values that are not one or zero are turned into one (including negative values!).

Any method can be defined as a function with two arguments, \code{o} and \code{e}, e.g. simply by specifying \code{method = function(o,e)\{o/e\}}. See below for more examples.

The predefined functions are:

  \itemize{
    \item{\code{pmi}: 
pointwise mutual information, aka as log-odds in bioinformatics, defined as \cr 
\code{pmi <- function(o,e) \{ log(o/e) \}}.
    }
    \item{\code{wpmi}: 
weighted pointwise mutual information, defined as\cr 
\code{wpmi <- function(o,e) \{ o * log(o/e) \}}.
    }
    \item{\code{res}:
Pearson residuals, defined as \cr 
\code{res <- function(o,e) \{ (o-e) / sqrt(e) \}}.
    }
    \item{\code{poi}:
association assuming a poisson-distribution of the values, defined as \cr 
\code{poi <- function(o,e) \{ sign(o-e) * (o * log(o/e) - (o-e)) \}}. \cr 
Seems to be very useful when the non-zero data is strongly skewed along the rows, i.e. some rows are much fuller than others. A short explanation of this method can be found in Prokić and Cysouw (2013).
    }
  }
}


\value{
The result is a sparse matrix with the non-zero association values. Values range between -Inf and +Inf, with values close to zero indicating low association. The exact interpretation of the values depends on the method used.

When \code{Y = NULL}, then the result is a symmetric matrix, so a matrix of type \code{dsCMatrix} with size \code{ncol(X)} by \code{ncol{X}} is returned. When \code{X} and \code{Y} are both specified, a matrix of type \code{dgCMatrix} with size \code{ncol(X)} by \code{ncol{Y}} is returned.
}

\note{
Care is taken in the implementation not to compute any association between columns that will end up with a value of zero anyway. However, very small association values will be computed. For further usage, these small values are often unnecessary, and can be removed for reasons of sparsity. Consider something like \code{X <- drop0(X, tol = value)} on the resulting \code{X} matrix (which removes all values between -value and +value). See examples below.

It is important to realize, that by default noting is computed when the observed co-occurrence is zero. However, this leads to wrong results with \code{method = res}, as \code{(o-e)/sqrt(e)} will be a negative value when \code{o = 0}. In most practically situations this error will be small and not important. However, when needed, the option \code{sparse = F} will give the correct results (though the resulting matrix will not be sparse anymore). Note that with all other methods implemented here, the default behavior leads to correct results (i.e. for \code{log(O)} nothing is calculated).

The current implementation will not lead to correct results with lots of missing data (that option is simply not yet implemented). See \code{\link{cosMissing}} for now.
}

\references{
Prokić, Jelena & Michael Cysouw. 2013. Combining regular sound correspondences and geographic spread. \emph{Language Dynamics and Change} 3(2). 147--168.
}

\author{
Michael Cysouw
}

\seealso{
See \code{\link{assocCol}} and \code{\link{assocRow}} for this measure defined for nominal data. Also, see \code{\link{corSparse}} and \code{\link{cosSparse}} for other sparse association measures. 
}
\examples{
# ----- reasonably fast with large very sparse matrices -----

X <- rSparseMatrix(1e6, 1e6, 1e6, NULL)
system.time(M <- assocSparse(X, method = poi))
length(M@x) / prod(dim(M)) # only one in 1e6 cells non-zero

\dontrun{

# ----- reaching limits of sparsity -----

# watch out: 
# with slightly less sparse matrices the result will not be very sparse,
# so this will easily fill up your RAM during computation!

X <- rSparseMatrix(1e4, 1e4, 1e6, NULL)
system.time(M <- assocSparse(X, method = poi))
print(object.size(M), units = "auto") # about 350 Mb
length(M@x) / prod(dim(M)) # 30\% filled

# most values are low, so it often makes sense 
# to remove low values to keep results sparse

M <- drop0(M, tol = 2)
print(object.size(M), units = "auto") # reduces to 10 Mb
length(M@x) / prod(dim(M)) # down to less than 1\% filled
}

# ----- defining new methods -----

# Using the following simple 'div' method is the same as
# using a cosine similarity with a 1-norm, up to a factor nrow(X)

div <- function(o,e) {o/e}
X <- rSparseMatrix(10, 10, 30, NULL)
all.equal(
	assocSparse(X, method = div),
	cosSparse(X, norm = norm1) * nrow(X)
	)

# ----- comparing methods -----

# Compare various methods on random data
# ignore values on diagonal, because different methods differ strongly here
# Note the different behaviour of pointwise mutual information (and division)

X <- rSparseMatrix(1e2, 1e2, 1e3, NULL)

p <- assocSparse(X, method = poi); diag(p) <- 0
r <- assocSparse(X, method = res); diag(r) <- 0
m <- assocSparse(X, method = pmi); diag(m) <- 0
w <- assocSparse(X, method = wpmi); diag(w) <- 0
d <- assocSparse(X, method = div); diag(d) <- 0

pairs(~w@x+p@x+r@x+d@x+m@x, 
  labels=c("weighted pointwise\nmutual information","poisson","residuals","division",
           "pointwise\nmutual\ninformation"), cex = 0.7)

% The following is only true for non-zero/one matrices!
%
% # pmi has radically different with very small observed values: 
% # because log(0) = -Inf we get asymptotic behaviour around Observed=zero
%
% par(mfrow = c(1,3))
% O <- crossprod(X)@x
% plot(O, p, xlab = "observed", ylab = "poisson")
% plot(O, r, xlab = "observed", ylab = "residuals")
% plot(O, m, xlab = "observed", ylab = "pointwise mutual information")
% par(mfrow = c(1,1))

}
