\name{ttMatrix}
\alias{ttMatrix}

\title{
Construct a `type-token' (tt) Matrix from a vector
}
\description{
A type-token matrix is a sparse matrix representation of a vector of entities. The rows of the matrix (`types') represent all different entities in the vector, and the columns of the matrix (`tokens') represent the entities themselves. The cells in the matrix represent which token belongs to which type. This is basically a convenience wrapper around \code{factor} and \code{sparseMatrix}, with an option to influence the ordering of the rows (`types') based on locale settings.
}
\usage{
ttMatrix(vector, collation.locale = "C", simplify = FALSE)
}

\arguments{
  \item{vector}{
a vector of tokens to be represented as a sparse matrix. It will work without complaining just as well when given a factor, but be aware that the ordering of the levels in the factor depends on the locale, which is transparently handled by this function. So better let this function turn the vector into a factor.
}
   \item{simplify}{
by default, the row and column names are not included into the matrix to keep the matrix as lean as possible. The row names (`types') are returned separately. Using \code{simplify = T} the row and columns names will be added into the matrix. Note that the column names are simply the vector that went into the function.
}
  \item{collation.locale}{
locale determining the ordering (`collation') of the entities. By default R mostly uses `en_US.UTF-8', though this might depend on the installation. By default, this function sets the ordering to `C', which means that characters are ordered according to their Unicode-number. For more information about locale settings, see \code{\link{locales}}.
}
}

\details{
This function is a rather low-level preparation for later high level functions. A few simple uses are described in the examples.
}
\value{
By default (\code{simplify = F}), then the output is a list with two elements:
  \item{M}{sparse pattern Matrix of type \code{ngCMatrix}. Because of the structure of these matrices, row-based encoding would be slightly more efficient. If RAM is crucial, consider storing the matrix as its transpose}
  \item{rownames}{a separate vector with the names of the types in the order of occurrence in the matrix. This vector is separated from the matrix itself for reasons of efficiency when dealing with many matrices.}

When \code{simplify = T}, then only the matrix M with row and columns names is returned.
}

\author{
Michael Cysouw
}

\seealso{
This function is used in various high-level functions like \code{\link{pwMatrix}}, \code{\link{splitText}}, \code{\link{splitTable}} and \code{\link{splitWordlist}}.
}
\examples{
# Consider two nominal variables
# one with eight categories, and one with three categories
var1 <- sample(8, 1000, TRUE)
var2 <- sample(3, 1000, TRUE)

# turn them into type-token matrices
M1 <- ttMatrix(var1, simplify = TRUE)
M2 <- ttMatrix(var2, simplify = TRUE)

# Then taking  the `residuals' from assocSparse ...
x <- as.matrix(assocSparse(t(M1), t(M2), method = res))

# ... is the same as the residuals as given by a chi-square
x2 <- chisq.test(var1, var2)$residuals
class(x2) <- "matrix"
all.equal(x, x2, check.attributes = FALSE) # TRUE

\donttest{
# A second quick example: consider a small piece of English text:
text <- "Once upon a time in midwinter, when the snowflakes were 
falling like feathers from heaven, a queen sat sewing at her window, 
which had a frame of black ebony wood. As she sewed she looked up at the snow 
and pricked her finger with her needle. Three drops of blood fell into the snow. 
The red on the white looked so beautiful that she thought to herself: 
If only I had a child as white as snow, as red as blood, and as black 
as the wood in this frame. Soon afterward she had a little daughter who was 
as white as snow, as red as blood, and as black as ebony wood, and therefore 
they called her Little Snow-White. And as soon as the child was born, 
the queen died." 

# split by characters, make lower-case, and turn into a type-token matrix
split.text <- tolower(strsplit(text,"")[[1]])
M <- ttMatrix(split.text, simplify = TRUE)

# rowSums give the character frequency
freq <- rowSums(M)
names(freq) <- rownames(M)
sort(freq, decreasing = TRUE)

# shift the matrix one character to the right using a bandSparse matrix
S <- bandSparse(n = ncol(M), k = 1)
N <- M \%*\% S

# use rKhatriRao on M and N to get frequencies of bigrams
B <- rKhatriRao(M, N, binder = "")
freqB <- rowSums(B$M)
names(freqB) <- B$rownames
sort(freqB, decreasing = TRUE)

# then the association between N and M is related 
# to the transition probabilities between the characters. 
P <- assocSparse(t(M), t(N))
plot(hclust(as.dist(-P), method = "ward.D"))
}
}
