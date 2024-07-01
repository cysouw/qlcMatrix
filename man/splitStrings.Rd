\name{splitStrings}
\alias{splitStrings}
\title{
Construct ngram matrices from a vector of strings
}
\description{
A (possibly large) vector of strings is separated into sparse pattern matrices, which allows for efficient computations on the strings.
}
\usage{
splitStrings(strings, sep = "", n = 2, boundary = TRUE,
	ngram.binder = "", left.boundary = "#", right.boundary = "#", simplify = FALSE)
}
\arguments{
  \item{strings}{
Vector of strings to be separated into sparse matrices
}
  \item{sep}{
Separator used to split the strings into parts. This will be passed to \code{\link{strsplit}} internally, so there is no fine-grained control possible over the splitting. If it is important to get the splitting exactly right, consider pre-processing the splitting by inserting a special symbol on the split-positions, and then choosing to split by this specific symbol.
}
  \item{n}{
Size of ngrams. By default, bigrams are computed. Unigrams are computed by setting \code{n = 1}. By setting a higher value for \code{n} larger ngrams are computed. Note that only this precise size of ngrams is provided in the output, not all ngrams smaller than \code{n}.
}
  \item{boundary}{
Should a start symbol and a stop symbol be added to each string?.
  }
  \item{ngram.binder}{
Only when \code{n > 1}. What symbol(s) should occur between the two parts of the ngrams?
}
  \item{left.boundary, right.boundary}{
Symbols to be used as boundaries, only used when \code{n > 1}.
  }
  \item{simplify}{
By default, various vectors and matrices are returned. However, when \code{simplify = T}, only a single sparse matrix is returned. See Value.
  }
}
\value{
By default, the output is a list of four elements:
  \item{segments}{
  	A vector with all splitted parts (i.e. all tokens) in order of occurrence, separated between the original strings with gap symbols.
  }
  \item{ngrams}{
  	A vector with all unique ngrams.
  }
  \item{SW}{
  	A sparse pattern matrix of class \code{ngCMatrix} specifying the distribution of segments (S) over the original strings (W, think `words'). This matrix is only interesting in combination with the following matrices.
  }
  \item{NS}{
    A sparse pattern matrix of class \code{ngCMatrix} specifying the distribution of the unique ngrams (N) over the tokenized segments (S)
  }

When \code{simplify = T} the output is a single sparse matrix of class \code{dgCMatrix}. This is basically NS \%*\% SW with rows and column names added into the matrix.
}
\author{
Michael Cysouw
}
\note{
Because of some internal idiosyncrasies, the ordering of the ngrams is reversed alphabetically. This might change in future versions.
}
\seealso{
\code{\link{sim.strings}} is a convenience function to quickly compute pairwise strings similarities, based on \code{splitStrings}.
}
\examples{
# a simple example to see the function at work
example <- c("this","is","an","example")
splitStrings(example)
splitStrings(example, simplify = TRUE)

# larger ngrams
splitStrings(c("test", "testing", "tested"), n = 4, simplify = TRUE)

\donttest{
# a bit larger, but still quick and efficient
# taking 15526 wordforms from the English Dalby Bible and splitting them into bigrams
data(bibles)
words <- splitText(bibles$eng)$wordforms
system.time( S <- splitStrings(words, simplify = TRUE) )

# and then taking the cosine similarity between the bigram-vectors for all word pairs
system.time( sim <- cosSparse(S) )

# most similar words to "father"
sort(sim["father",], decreasing = TRUE)[1:20]
}
}

