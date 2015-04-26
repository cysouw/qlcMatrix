\name{pwMatrix}
\alias{pwMatrix}

\title{
Construct `part-whole' (pw) Matrices from tokenized strings
}
\description{
A part-whole Matrix is a sparse matrix representation of a vector of strings (`wholes') split into smaller parts by a specified separator. It basically summarizes which strings consist of which parts. By itself, this is not a very interesting transformation, but it allows for quite fancy computations by simple matrix manipulations.
}
\usage{
pwMatrix(strings, sep = "", gap.length = 0, gap.symbol = "·", simplify = FALSE)
}

\arguments{
  \item{strings}{
a vector (or list) of strings to be separated into parts
}
  \item{sep}{
The separator to be used. Defaults to space \code{sep = " "}. If separation in individual characters is needed, use \code{sep = ""}. There is no fancy parsing of strings implemented (e.g. to catch complex unicode combined characters), that has to be done externally. The preferred route is to prepare the separation of the strings by using spaces, and then call this function.
}
  \item{gap.length}{
This adds the specified number of gap symbols between each pair of strings. This is only important for generating higher ngram-statistics later on, when no ordering of the strings is implied. For example, when the strings are alphabetically ordered words, any bigram-statistics should not count the bigrams consisting of the last character of the a word with the first character of the next word. 
}
  \item{gap.symbol}{
The gap symbol to insert (see gap.length above). It defaults to U+8901 ( · ) on the assumption that this character will not often be included in data.
}
  \item{simplify}{
 by default, the row and column names are not included into the matrix to keep the matrix as lean as possible. The row names (`parts') are returned separately. Using \code{simplify = T} the row and column names will be added into the matrix. Note that the column names are simply the vector that went into the function.
}
}
\details{
Internally, this is basically using \code{strsplit} and some cosmetic changes, returning a sparse matrix.
}
\value{
By default (when \code{simplify = F}) the output is a list with two elements, containing:
  \item{M}{
  	a sparse pattern Matrix of type \code{ngCMatrix} with all input strings as columns, and all separated elements as rows.
  	}
  \item{rownames}{
  	all different  characters from the strings in order (i.e. all individual tokens of the original strings).
  }
  
 When \code{simplify = T}, then only the matrix M with row and column names is returned.
}
\author{
Michael Cysouw
}
\seealso{
Used in \code{\link{splitStrings}} and \code{\link{splitWordlist}}
}
\examples{
# By itself, this functions does nothing really interesting
example <- c("this","is","an","example")
pw <- pwMatrix(example)
pw

# However, making a type-token Matrix (with ttMatrix) of the rownames
# and then taking a matrix product, results in frequencies of each element in the strings
tt <- ttMatrix(pw$rownames)
distr <- (tt$M*1) \%*\% (pw$M*1)
rownames(distr) <- tt$rownames
colnames(distr) <- example
distr

# Use banded sparse matrix with superdiagonal ('shift matrix') to get co-occurrence counts
# of adjacent characters. Rows list first character, columns adjacent character. 
# Non-zero entries list number of co-occurrences
S <- bandSparse( n = ncol(tt$M), k = 1) * 1
TT <- tt$M * 1
( C <- TT \%*\% S \%*\% t(TT) )

# show the non-zero entries as triplets:
s <- summary(C)
first <- tt$rownames[s[,1]]
second <- tt$rownames[s[,2]]
freq <- s[,3]
data.frame(first,second,freq)	
}
