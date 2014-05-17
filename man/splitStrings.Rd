\name{splitStrings}
\alias{splitStrings}
\title{
Construct unigram and bigram matrices from a vector of strings
}
\description{
A (possibly large) vector of strings is separated into sparse pattern matrices, which allows for efficient computation on the strings.
}
\usage{
splitStrings(strings, sep = "", bigrams = TRUE, boundary = TRUE,
	bigram.binder = "", gap.symbol = "·", left.boundary = "#", right.boundary = "#",
	simplify = FALSE)
}
\arguments{
  \item{strings}{
Vector of strings to be separated into sparse matrices
}
  \item{sep}{
Seperator used to split the strings into parts. This will be passed to \code{\link{strsplit}} internally, so there is no fine-grained control possible over the splitting. If it is important to get the splitting exactly right, consider pre-processing the splitting by inserting a special symbol on the split-positions, and then choosing to split by this specific symbol.
}
  \item{bigrams}{
By default, both unigrams and bigrams are computer. If bigrams are not needed, setting \code{bigrams = F} will save on resources. 	
}
  \item{boundary}{
Should a start symbol and a stop symbol be added to each string? This will only be used for the determination of bigrams, and will be ignored if \code{bigrams = F}.
  }
  \item{bigram.binder}{
Only when \code{bigrams = T}. What symbol(s) should occur between the two parts of the bigram?
}
  \item{gap.symbol}{
Only when \code{bigram = T}. What symbol should be included to separate the strings? It defaults to U+8901 ( · ) on the assumption that this character will not often be included in data. See \code{\link{pwMatrix}} for some more explanation about the necessity of this gap symbol.
}
  \item{left.boundary, right.boundary}{
Symbols to be used as boundaries, only used when \code{boundary = T}.
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
  \item{bigrams}{
  	Only present when \code{bigrams = T}. A vector with all unique bigrams.
  }
  \item{SW}{
  	A sparse pattern matrix of class \code{ngCMatrix} specifying the distribution of segments (S) over the original strings (W, think 'words'). This matrix is only interesting in combination with the following matrices.
  }
  \item{US}{
  	A sparse pattern matrix of class \code{ngCMatrix} specifying the distribution of the unique unigrams (U) over the tokenized segments (S).
  }
  \item{BS}{
  	Only present when \code{bigrams = T}. A sparse pattern matrix of class \code{ngCMatrix} specifying the distribution of the unique bigrams (B) over the tokenized segments (S)
  }

When \code{simplify = T} the output is a single sparse matrix of class \code{dgCMatrix}. This is basically BS \%8\% SW (when \code{bigrams = T}) or US \%*\% SW (when \code{bigrams = F}) with rows and column names added into the matrix.
}
\author{
Michael Cysouw
}
\note{
Because of some internal idiosyncracies, the ordering of the bigrams is first by second element, and then by first element. This might change in future versions.
}
\seealso{
\code{\link{sim.strings}} is a convenience function to quickly compute pairwise strings similarities, based on \code{splitStrings}.
}
\examples{
# a simple example to see the function at work
example <- c("this","is","an","example")
splitStrings(example)
splitStrings(example, simplify = TRUE)

\dontrun{
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

