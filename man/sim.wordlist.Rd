\name{sim.wordlist}
\alias{sim.wordlist}
\alias{sim.con}
\alias{sim.lang}
\alias{sim.graph}

\title{
Similarity matrices from wordlists
}
\description{
A few different approaches are implemented here to compute similarities from wordlists. \code{sim.lang} computes similarities between languages, assuming a harmonized orthography (i.e. symbols can be equated across languages). \code{sim.con} computes similarities between concepts, using only language-internal similarities. \code{sim.graph} computes similarities between graphemes (i.e. language-specific symbols) between languages, as a crude approximation of regular sound correspondences.

WARNING: All these methods are really very crude! If they seem to give expected results, then this should be a lesson to rethink more complex methods proposed in the literature. However, in most cases the methods implemented here should be taken as a proof-of-concept, showing that such high-level similarities can be computed efficiently for large datasets. For actual research, I strongly urge anybody to adapt the current methods, and fine-tune them as needed.
}
\usage{
sim.lang(wordlist, 
	doculects = "DOCULECT", concepts = "CONCEPT", counterparts = "COUNTERPART",
	method = "parallel", assoc.method =  res, weight = NULL, sep = "")

sim.con(wordlist,
	doculects = "DOCULECT", concepts = "CONCEPT", counterparts = "COUNTERPART",
	method = "bigrams", assoc.method = res, weight = NULL, sep = "")

sim.graph(wordlist,
	doculects = "DOCULECT", concepts = "CONCEPT", counterparts = "TOKENS",
	method = "cooccurrence", assoc.method = poi, weight = NULL, sep = " ")
}
\arguments{
	\item{wordlist}{
		Dataframe or matrix containing the wordlist data. Should have at least columns corresponding to languages (DOCULECT), meanings (CONCEPT) and translations (COUNTERPART). 
	}
	\item{doculects, concepts, counterparts}{
		The name (or number) of the column of \code{wordlist} in which the respective information is to be found. The defaults are set to coincide with the naming of the example dataset included in this package. See \code{\link{huber}}.
	}
	\item{method}{
		Specific approach for the computation of the similarities. See Details below.
	}
	\item{assoc.method, weight}{
		Measures to be used internally (passed on to \code{assocSparse} or \code{cosSparse}). See Details below.
	}
	\item{sep}{Separator to be used to split strings. See \code{link{splitStrings}} for details.
	}
}
\details{
The following methods are currently implemented (all methods can be abbreviated):

For \code{sim.lang}:
\itemize{
	\item{\code{global: }}{Global bigram similarity, i.e. ignoring the separation into concepts, and simply taking the bigram vector of all words per language. Probably best combined with \code{weight = idf}.}
	\item{\code{parallel: }}{By default, computes a parallel bigram similarity, i.e. splitting the bigram vectors per language and per concepts, and then simply making one long vector per language from all individual concept-bigram vectors. This approach seems to be very similar (if not slightly better) than the widespread `average Levenshtein' distance.}
}
For \code{sim.con}:
\itemize{
	\item{\code{colexification: }}{Simply count the number of languages in which two concepts have at least one complete identical translations. No normalisation is attempted, and \code{assoc.method} and \code{weight} are ignored (internally this just uses \code{tcrossprod} on the \code{CW (concepts x words)} sparse matrix). Because no splitting of strings is necessary, this method is very quick.}
	\item{\code{global: }}{Global bigram similarity, i.e. ignoring the separation into languages, and simply taking the bigram vector of all words per concept. Probably best combined with \code{weight = idf}.}
	\item{\code{bigrams: }}{By default, compute the similarity between concepts by comparing bigraphs, i.e. language-specific bigrams. In that way, cross-linguistically recurrent partial similarities are uncovered. It is very interesting to compare this measure with \code{colexification} above.}
}	
For \code{sim.graph}:
\itemize{
	\item{\code{cooccurrence: }}{Currently the only method implemented. Computes the co-occurrence statstics for all pair of graphemes (e.g. between symbol x from language L1 and symbol y from language L2). See Prokic & Cysouw (2013) for an example using this approach.}
}
All these methods (except for \code{sim.con(method = "colexification")}) use either \code{\link{assocSparse}} or \code{\link{cosSparse}} for the computation of the similarities. For the different measures available, see the documentation there. Currently implemented are \code{res, poi, pmi, wpmi} for \code{assocSparse} and \code{idf, isqrt, none} for \code{cosWeight}. It is actually very easy to define your own measure.  

When \code{weight = NULL}, then \code{assocSparse} is used with the internal method as specified in \code{assoc.method}. When \code{weight} is specified, then \code{cosSparse} is used with an Euclidean norm and the weighting as specified in \code{weight}. When \code{weight} is specified, and specification of \code{assoc.method} is ignored.
}
\value{
A sparse similarity matrix of class \code{dsCMatrix}. The magnitude of the actual values in the matrices depend strongly on the methods chosen.

With \code{sim.graph} a list of two matrices is returned.
	\item{GG}{The grapheme by grapheme similarity matrix of class \code{dsCMatrix}}
	\item{GD}{A pattern matrix of class {} indicating which grapheme belongs to which language.}
}
\references{
Prokic, Jelena and Michael Cysouw. 2013. Combining regular sound correspondences and geographic spread. \emph{Language Dynamics and Change} 3(2). 147--168.
}
\author{
Michael Cysouw
}
\seealso{
Based on \code{\link{splitWordlist}} for the underlying conversion of the wordlist into sparse matrices. The actual similarities are mostly computed using \code{\link{assocSparse}} or \code{\link{cosSparse}}.
}
\examples{
# ----- load data -----

# an example wordlist, see help(huber) for details
data(huber)

# ----- similarity between languages -----

# most time is spend splitting the strings
# the rest does not really influence the time needed
system.time( sim <- sim.lang(huber, method = "p") )

# a simple distance-based UPGMA tree
plot(hclust(as.dist(-sim), method = "average"), cex = .7)

\dontrun{

# ----- similarity between concepts -----

# similarity based on bigrams
system.time( simB <- sim.con(huber, method = "b") )
# similarity based on colexification. much easier to calculate
system.time( simC <- sim.con(huber, method = "c") )

# As an example, look at all adjectival concepts
adj <- c(1,5,13,14,28,35,40,48,67,89,105,106,120,131,137,146,148,
	171,179,183,188,193,195,206,222,234,259,262,275,279,292,
	294,300,309,341,353,355,359)

# show them as trees
par(mfrow = c(1,2)) 
plot(hclust(as.dist(-simB[adj,adj]), method = "ward"), 
	cex = .5, main = "bigrams")
plot(hclust(as.dist(-simC[adj,adj]), method = "ward"), 
	cex = .5, main = "colexification")
par(mfrow = c(1,1))

# ----- similarity between graphemes -----

# this is a very crude approach towards regular sound correspondences
# when the languages are not too distantly related, it works rather nicely 
# can be used as a quick first guess of correspondences for input in more advanced methods

# all 2080 graphemes in the data by all 2080 graphemes, from all languages
system.time( X <- sim.graph(huber) )

# throw away the low values
# select just one pair of languages for a quick visualisation
X$GG <- drop0(X$GG, tol = 1)
colnames(X$GG) <- rownames(X$GG)
correspondences <- X$GG[X$GD[,"bora"],X$GD[,"muinane"]]
heatmap(as.matrix(correspondences))
}
}
