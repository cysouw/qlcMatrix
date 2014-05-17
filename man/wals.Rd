\name{WALS}
\alias{wals}
\docType{data}
\title{
The World Atlas of Language Structures (WALS)
}
\description{
The World Atlas of Language Structures (WALS) is a large database of structural (phonological, grammatical, lexical) properties of languages gathered from descriptive materials (such as reference grammars) by a team of 55 authors.

The first version of WALS was published as a book with CD-ROM in 2005 by Oxford University Press. The first online version was published in April 2008. The second online version was published in April 2011. The current dataset is WALS 2013, published on 14 November 2013.

The included dataset \code{wals} takes a somewhat sensible selection from the complete WALS data. It excludes attributes ("features" in WALS-parlance) that are definitially duplicates of others (3, 25, 95, 96, 97), those attributes that only list languages that are incompatible with other attributes (132, 133, 134, 135, 139, 140, 141, 142), and the `additional' attributes that are marked as "B" through "Z". Further, it removes those languages that do not have any data left after removing those attributes. The result is a dataset with 2566 languages and 131 attributes.

}
\usage{
data(wals)
}
\format{
A list with two dataframes:
  \describe{
  	\item{\code{data}}{the actual WALS data. The object \code{wals$data} contains a dataframe with data from 2566 languages on 131 different attributes. The column names identify the WALS features. For details about these features, see \url{http://wals.info/chapter}}
  	\item{\code{meta}}{some metadata for the languages. The object \code{wals$meta} contains a dataframe with some limited meta-information about these 2566 languages.}
  }
  
The three-letter WALS-codes are used as rownames in both dataframes. Further, the object \code{wals$meta} contains the following variables.
  \describe{
    \item{\code{name}}{a character vector giving a name for each language}
    \item{\code{genus}}{a factor with 522 levels with the genera according to M. Dryer}
    \item{\code{family}}{a factor with 215 levels with the familes accoring to M. Dryer}
    \item{\code{longitude}}{a numeric vector with geo coordinates for all languages}
    \item{\code{latitude}}{a numeric vector with geo coordinates for all languages}
  }
}
\details{
All details about the meaning of the variables and much more meta-information is available at \url{http://wals.info}.
}
\source{
The current data was downloaded from \url{http://wals.info} in May 2014. The data is licenced as \url{http://creativecommons.org/licenses/by-nc-nd/2.0/de/deed.en}. Some minor corrections on the metadata have been performed (naming of variables, addition of missing coordinates).
}
\references{
Dryer, Matthew S. & Haspelmath, Martin (eds.) 2013. \emph{The World Atlas of Language Structures Online.} Leipzig: Max Planck Institute for Evolutionary Anthropology. 
(Available online at http://wals.info, Accessed on 2013-11-14.) }
\examples{
data(wals)

# plot all locations of the WALS languages, looks like a worldmap
plot(wals$meta[,4:5])

# turn the large and mostly empty dataframe into sparse matrices
# recoding is nicely optimized and quick for this reasonably large dataset
# this works perfect as long as things stay within available RAM of the computer
system.time(
  W <- splitTable(wals$data)
)

# as an aside: note that the recoding takes only about 30% of the space
as.numeric( object.size(W) / object.size(wals$data) )

# compute similarities (Chuprov's T, similar to Cramer's V) 
# between all pairs of variables using sparse Matrix methods
system.time(sim <- sim.att(wals$data, method = "chuprov"))

# some structure visible
rownames(sim) <- colnames(wals$data)
plot(hclust(as.dist(1-sim), method = "ward"), cex = 0.5)

}
\keyword{datasets}