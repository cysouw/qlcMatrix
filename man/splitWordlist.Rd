\name{splitWordlist}
\alias{splitWordlist}
\title{
Construct sparse matrices from comparative wordlists (aka `Swadesh list')
}
\description{
A comparative wordlist (aka `Swadesh list') is a collection of wordforms from different languages, which are translations of a selected set of meanings. This function dismantles this data structure into a set of sparse matrices.
}
\usage{
splitWordlist(data,
	doculects = "DOCULECT", concepts = "CONCEPT", counterparts = "COUNTERPART",
	splitstrings = TRUE, sep =  "", bigram.binder = "", grapheme.binder = "_", 
	simplify = FALSE)
}
\arguments{
	\item{data}{
		A dataframe or matrix with each row describing a combination of language (DOCULECT), meaning (CONCEPT) and translation (COUNTERPART).
	}
	\item{doculects, concepts, counterparts}{
		The name (or number) of the column of \code{data} in which the respective information is to be found.  The defaults are set to coincide with the naming of the example dataset included in this package: \code{\link{huber}}.
	}
	\item{splitstrings}{
		Should the counterparts be separated into unigrams and bigrams (using \code{\link{splitStrings}})?
	}
	\item{sep}{
		Separator to be passed to \code{splitStrings} to specify where to split the strings. Only used when \code{splitstrings = T}, ignored otherwise.
	}
	\item{bigram.binder}{
		Separator to be passed to \code{splitStrings} to be inserted between the parts of the bigrams
	}
	\item{grapheme.binder}{
		Separator to be used to separate a grapheme from the language name. Graphemes are language-specific symbols (i.e. the 'a' in the one language is not assumed to be the same as the 'a' from another language).
	}
	\item{simplify}{
		Should the output be reduced to the most important matrices only, with the row and columns names included in the matrices? Defaults to \code{simplify = F}, separating everything into different object. See Value below for details on the format of the results.
	}
}
\details{
The meanings that are selected for a wordlist are called CONCEPTS here, and the translations into the various languages COUNTERPARTS (following Poornima & Good 2010). The languages are called DOCULECTS (`documented lects') to generalize over their status as dialects, languages, or even small families (following Cysouw & Good 2013).
}
\value{
There are four different possible outputs, depending on the option chosen.

By default, when \code{splitstrings = T, simplify = F}, the following list of 15 objects is returned. It starts with 8 different character vectors, which are actually the row/column names of the following sparse pattern matrices. The naming of the objects is an attempt to make everything easy to remember.
	\item{doculects}{Character vector with names of doculects in the data}
	\item{concepts}{Character vector with names of concepts in the data}
	\item{words}{Character vector with all words, i.e. unique counterparts per language. The same string in the same language is only included once, but an identical string occurring in different doculect is separately included for each doculects.}
	\item{segments}{Character vector with all unigram-tokens in order of appearance, including boundary symbols and gap symbols (see \code{\link{splitStrings}} for more information about the gap symbols)}
	\item{unigrams}{Character vector with all unique unigrams in the data}
	\item{bigrams}{Character vector with all unique bigrams in the data}
	\item{graphemes}{Character vector with all unique graphemes (i.e. combinations of unigrams+doculects) occurring in the data}
	\item{digraphs}{Character vector with all unique digraphs (i.e. combinations of bigrams+doculects) occurring in the data}
	\item{DW}{Sparse pattern matrix of class \code{ngCMatrix} linking doculects (D) to words (W)}
	\item{CW}{Sparse pattern matrix of class \code{ngCMatrix} linking concepts (C) to words (W)}
	\item{SW}{Sparse pattern matrix of class \code{ngCMatrix} linking all token-segments (S) to words (W)}
	\item{US}{Sparse pattern matrix of class \code{ngCMatrix} linking unigrams (U) to segments (S)}
	\item{BS}{Sparse pattern matrix of class \code{ngCMatrix} linking bigrams (B) to segments (S)}
	\item{GS}{Sparse pattern matrix of class \code{ngCMatrix} linking language-specific graphemes (G) to segments (S)}
	\item{TS}{Sparse pattern matrix of class \code{ngCMatrix} linking digraphs (T, as no other letter was available) to segments (S)}

When \code{splitstrings = F, simplify = F}, only the following objects from the above list are returned:
	\item{doculects}{Character vector with names of doculects in the data}
	\item{concepts}{Character vector with names of concepts in the data}
	\item{words}{Character vector with all words, i.e. unique counterparts per language. The same string in the same language is only included once, but an identical string occurring in different doculect is separately included for each doculects.}
	\item{DW}{Sparse pattern matrix of class \code{ngCMatrix} linking doculects (D) to words (W)}
	\item{CW}{Sparse pattern matrix of class \code{ngCMatrix} linking concepts (C) to words (W)}

When \code{splitstrings = T, simplify = T} only the bigram-separation is returned, and all row and columns names are included into the matrices. However, for reasons of space, the \code{words} vector is only included once:
	\item{DW}{Sparse pattern matrix of class \code{ngCMatrix} linking doculects (D) to words (W). Doculects are in the rownames, colnames are left empty.}
	\item{CW}{Sparse pattern matrix of class \code{ngCMatrix} linking concepts (C) to words (W). Concepts are in the rownames, colnames are left empty.}
	\item{BW}{Sparse pattern matrix of class \code{ngCMatrix} linking bigrams (B) to words (W). Bigrams (note: not digraphs!) are in the rownames. This matrix includes all words as colnames.}

Finally, when \code{splitstrings = F, simplify = T}, only the following subset of the above is returned.
	\item{DW}{Sparse pattern matrix of class \code{ngCMatrix} linking doculects (D) to words (W). Doculects are in the rownames, colnames are left empty.}
	\item{CW}{Sparse pattern matrix of class \code{ngCMatrix} linking concepts (C) to words (W). Concepts are in the rownames, colnames are left empty.}

}
\note{
Note that the default behavior probably overgenerates information (specifically when \code{splitstrings = T}), and might be performing unnecessary computation for specific goals. In practice, it might be useful to tweak the underlying code (mainly by throwing out unnecessary steps) to optimize performance.
}
\references{
Cysouw, Michael & Jeff Good. 2013. Languoid, Doculect, Glossonym: Formalizing the notion “language”. \emph{Language Documentation and Conservation} 7. 331-359.

Poornima, Shakthi & Jeff Good. 2010. Modeling and Encoding Traditional Wordlists for Machine Applications. \emph{Proceedings of the 2010 Workshop on NLP and Linguistics: Finding the Common Ground.}
}
\author{
Michael Cysouw
}
\seealso{
\code{\link{sim.wordlist}} for various quick similarities that can be computed using these matrices.
}
\examples{
# ----- load data -----

# an example wordlist, see the help(huber) for details
data(huber)

# ----- show output -----

# a selection, to see the result of splitWordlist
# only show the simplified output here, 
# the full output is rather long even for just these six words
sel <- c(1:3, 1255:1258)
splitWordlist(huber[sel,], simplify = TRUE)

\dontrun{

# ----- split complete data -----

# splitting the complete wordlist is a lot of work !
# it won't get much quicker than this
# most time goes into the string-splitting of the almost 26,000 words
# Default version, included splitStrings:
system.time( H <- splitWordlist(huber) )

# Simplified version without splitStrings is much quicker:
system.time( H <- splitWordlist(huber, splitstrings = FALSE, simplify = TRUE) )

# ----- investigate colexification -----

# The simple version can be used to check how often two concepts 
# are expressed identically across all languages ('colexification')
H <- splitWordlist(huber, splitstrings = FALSE, simplify = TRUE)
sim <- tcrossprod(H$CW*1)

# select only the frequent colexifications for a quick visualisation
diag(sim) <- 0
sim <- drop0(sim, tol = 5)
sim <- sim[rowSums(sim) > 0, colSums(sim) > 0]
plot( hclust(as.dist(-sim), method = "average"), cex = .5)

# ----- investigate regular sound correspondences -----

# One central problem with data from many languages is the variation of orthography
# It is preferred to solve that problem separately
# e.g. check the column "TOKENS" in the huber data
# This is a grapheme-separated version of the data.
# can be used to investigate co-occurrence of graphemes (approx. phonemes)
H <- splitWordlist(huber, counterparts = "TOKENS", sep = " ")

# co-occurrence of all pairs of the 2150 different graphemes through all languages
system.time( G <- assocSparse( (H$CW*1) \%*\% t(H$SW*1) \%*\% t(H$GS*1), method = poi))
rownames(G) <- colnames(G) <- H$graphemes
G <- drop0(G, tol = 1)

# select only one language pair for a quick visualisation
# check the nice sound changes between bora and muinane!
GD <- H$GS \%*\% H$SW \%*\% t(H$DW)
colnames(GD) <- H$doculects
correspondences <- G[GD[,"bora"],GD[,"muinane"]]
heatmap(as.matrix(correspondences))
}
}
