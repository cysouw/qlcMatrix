\name{splitText}
\alias{splitText}
\alias{read.text}

\title{
Construct sparse matrices from parallel texts
}
\description{
Convenience functions to read parallel texts and to split parallel texts into sparse matrices.
}
\usage{
splitText(text, globalSentenceID = NULL, localSentenceID = names(text), sep = " ",
	simplify = FALSE, lowercase = TRUE)

read.text(file)
}
\arguments{
  \item{text}{
vector of strings, typically sentences with wordforms separated by space (see \code{sep} below). names of the vector elements are typically IDs to link across texts (cf. the format as used in \code{\link{bibles}}).
}
  \item{globalSentenceID}{
Vector of all IDs that might possibly occur in the parallel texts, used to parallelize the texts. Can for example be constructed by using \code{union} on the localSentenceIDs.
}
  \item{localSentenceID}{
Vector of the IDs for the actual sentences in the present text. Typically present as names of the text.
}
  \item{sep}{
Separator on which the sentences should be parsed into wordforms. The implementation is very simple here, there are no advanced options for guessing punctuation. The variation in punctuation across a wide variety of languages and scripts normally turns out to be too large to be easily automatically parsed. Any advanced parsing has to be done externally, and here simply the parsed symbol is used to actually split the text into parts. Typically, this parsing of sentences into wordforms will be performed using space \code{sep = " "}. See also \code{\link{bibles}} for some examples of such pre-parsing.
}
  \item{simplify}{
By default (when \code{simplify = F}), this function returns a list of objects that represent the encoding of the text into sparse matrices. With \code{simplify = T} this list is reduced to a single matrix (wordforms x globalSentenceID), with the actual wordforms as row names.
}
  \item{lowercase}{
By default, a mapping between the text and a lowercase version of the same text. In the default output (with \code{simplify = F}), this is a sparse matrix linking strings with mixed upper/lower case to string with only lower case. Note that case folding is locale-specific, but here a simple universal case-folding is used (as available through \code{\link{tolower}}).
  }
  \item{file}{
file name (or full path) for a file to be read.
  }
}
\details{
The function \code{splitText} is actually just a nice examples of how \code{\link{pwMatrix}}, \code{\link{jMatrix}}, and \code{\link{ttMatrix}} can be used to work with parallel texts. 

The function \code{read.text} is a convenience function to read parallel texts as prepared in the \url{paralleltext.info} project.
}
\value{
When \code{simplify = F}, a list is returned with the following elements:
  \item{runningWords}{single vector with complete text (ignoring original sentence breaks), separated into strings according to \code{sep}}
  \item{wordforms}{vector with all wordforms as attested in the text (according to the specified separator). Ordering of wordforms is done by \code{\link{ttMatrix}}, which by default uses the "C" collation locale.}
  \item{lowercase}{only returned when \code{lowercase = T}. Vector with all unique wordforms after conversion to lowercase.}
  \item{RS}{Sparse pattern matrix of class \code{ngCMatrix} with runningWords (R) as rows and sentence IDs (S) as columns. When \code{globalSentenceID = NULL}, then the sentences are the elements of the original text. Else, they are the specified globalSentenceIDs.}
  \item{WR}{Sparse pattern matrix of class \code{ngCMatrix} with wordforms (W) as rows and running words (R) as columns.}
  \item{wW}{only returned when \code{lowercase = T}. Sparse pattern matrix of class \code{ngCMatrix} linking between lowercased wordforms and original wordforms.}

When \code{simplify = T} the result is a single sparse Matrix (of type \code{dgCMatrix}) linking wordforms (either with or without case) to sentences (either global or local). Note that the result with options \code{(simplify = T, lowercase = F)} will result in the sparse matrix as available at paralleltext.info (there the matrix is in \code{.mtx} format), with the wordforms included into the matrix as row names. However, note that the resulting matrix from the code here will include frequencies for words that occur more than once per sentence. These have been removed for the \code{.mtx} version available online.
}
\author{
Michael Cysouw
}
\seealso{
\code{\link{bibles}} for some texts that led to the development of this function.
\code{\link{sim.words}} for a convenience function to easily extract possible translations equivalents through co-occurrence (using \code{splitText} for the data-preparation.)
}
\examples{
# a trivial examples to see the results of this function:
text <- c("This is a sentence .","A sentence this is !","Is this a sentence ?")
splitText(text)
splitText(text, simplify = TRUE, lowercase = FALSE)

\dontrun{
# reasonably quick with complete bibles (about 1-2 second per complete bible)
# texts with only New Testament is even quicker
data(bibles)
system.time(eng <- splitText(bibles$eng, bibles$verses))
system.time(deu <- splitText(bibles$deu, bibles$verses))

# Use example: Number of co-ocurrences between two bibles
# How often do words from the one language cooccur with words from the other language?
ENG <- (eng$wW * 1) \%*\% (eng$WR * 1) \%*\% (eng$RS * 1)
DEU <- (deu$wW * 1) \%*\% (deu$WR * 1) \%*\% (deu$RS * 1)
C <- tcrossprod(ENG,DEU)
rownames(C) <- eng$lowercase
colnames(C) <- deu$lowercase
C[	c("father","father's","son","son's"),
	c("vater","vaters","sohn","sohne","sohnes","sohns")
	]

# Pure counts are not very interesting. This is better:
R <- assocSparse(t(ENG), t(DEU))
rownames(R) <- eng$lowercase
colnames(R) <- deu$lowercase
R[	c("father","father's","son","son's"),
	c("vater","vaters","sohn","sohne","sohnes","sohns")
	]

# For example: best co-occurrences for the english word "mine"
sort(R["mine",], decreasing = TRUE)[1:10]

# To get a quick-and-dirty translation matrix:
# adding maxima from both sides work quite well
best <-	  colMax(R, which = TRUE, ignore.zero = FALSE)$which 
	+ rowMax(R,which = TRUE, ignore.zero = FALSE)$which
best <- as(best, "nMatrix")

which(best["your",])
which(best["went",])

# all of the above is also performed by the function sim.words

# A final speed check:
# split all 4 texts, and simplify them into one matrix
# They have all the same columns, so they can be rbind
system.time(all <- sapply(bibles[-1], function(x){splitText(x, bibles$verses, simplify = TRUE)}))
all <- do.call(rbind, all)

# then try a single co-occerrence measure on all pairs from these 72K words
# (so we are doing about 2.6e9 comparisons here!)
system.time( S <- cosSparse(t(all)) )

# this goes extremely fast! As long as everything fits into RAM this works nicely.
# Note that S quickly gets large
print(object.size(S), units = "auto")

# but most of it can be thrown away, because it is too low anyway
# this leads to a factor 10 reduction in size:
S <- drop0(S, tol = 0.2)
print(object.size(S), units = "auto")
}
}
