\name{splitStrings}
\alias{splitStrings}
\title{
Construct unigram and bigram matrices from a vector of strings
}
\description{
A (possibly large) vector of strings is separated into sparse pattern matrices, which allows for efficient computations on the strings.
}
\usage{
splitStrings(strings, sep = "", n = 2, boundary = TRUE,
	bigram.binder = "", gap.symbol = "\u2043", left.boundary = "#",
	right.boundary = "#", simplify = FALSE)
}
\arguments{
  \item{strings}{
Vector of strings to be separated into sparse matrices
}
  \item{sep}{
Separator used to split the strings into parts. This will be passed to \code{\link{strsplit}} internally, so there is no fine-grained control possible over the splitting. If it is important to get the splitting exactly right, consider pre-processing the splitting by inserting a special symbol on the split-positions, and then choosing to split by this specific symbol.
}
  \item{n}{
Size of ngrams. By default, unigrams and bigrams are computed. If bigrams are not needed, setting \code{n = 1} will save on resources. By setting a higher value for \code{n} larger ngrams are computed. Note that only this precise size of ngrams is provided in the output.
}
  \item{boundary}{
Should a start symbol and a stop symbol be added to each string? This will only be used for the determination of ngrams, and will be ignored if \code{n = 1}.
  }
  \item{bigram.binder}{
Only when \code{n >1}. What symbol(s) should occur between the two parts of the ngrams?
}
  \item{gap.symbol}{
Only when \code{n > 1}. What symbol should be included to separate the strings? It defaults to U+2043 \code{HYPHEN BULLET} on the assumption that this character will not often be included in data. See \code{\link{pwMatrix}} for some more explanation about the necessity of this gap symbol.
}
  \item{left.boundary, right.boundary}{
Symbols to be used as boundaries, only used when \code{n > 1}.
  }
  \item{simplify}{
By default, various vectors and matrices are returned. However, when \code{simplify = T}, only a single sparse matrix is returned. See Value.
  }
}
\value{
By default, the output is a list of six elements:
  \item{segments}{
  	A vector with all splitted parts (i.e. all tokens) in order of occurrence, separated between the original strings with gap symbols.
  }
  \item{unigrams}{
  	A vector with all unique parts occuring in the segments.
  }
  \item{ngrams}{
  	Only present when \code{n > 1}. A vector with all unique ngrams.
  }
  \item{SW}{
  	A sparse pattern matrix of class \code{ngCMatrix} specifying the distribution of segments (S) over the original strings (W, think `words'). This matrix is only interesting in combination with the following matrices.
  }
  \item{US}{
  	A sparse pattern matrix of class \code{ngCMatrix} specifying the distribution of the unique unigrams (U) over the tokenized segments (S).
  }
  \item{NS}{
  	Only present when \code{n > 1}. A sparse pattern matrix of class \code{ngCMatrix} specifying the distribution of the unique ngrams (N) over the tokenized segments (S)
  }

When \code{simplify = T} the output is a single sparse matrix of class \code{dgCMatrix}. This is basically NS \%*\% SW (when \code{n > 1}) or US \%*\% SW (when \code{n = 1}) with rows and column names added into the matrix.
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

