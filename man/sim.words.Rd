\name{sim.words}
\alias{sim.words}

\title{
Similarity-measures for words between two languages, based on co-occurrences in parallel text
}
\description{
Based on co-occurrences in a parallel text, this convenience function (a wrapper around various other functions from this package) efficiently computes something close to translational equivalence.
}
\usage{
sim.words(text1, text2 = NULL, method = res, weight = NULL, 
	lowercase = TRUE, best = FALSE, tol = 0)}
\arguments{
  \item{text1, text2}{
Vectors of strings representing sentences. The names of the vectors should contain IDs that identify the parallelism between the two texts. If there are no specific names, the function assumes that the two vectors are perfectly parallel. Within the strings, wordforms are simply separated based on spaces (i.e. everything between two spaces is a wordform). For more details about the format-assumptions, see \code{\link{splitText}}, which is used internally here.
}
  \item{method}{
Method to be used as a co-occurrence statistic. See \code{\link{assocSparse}} for a detailed presentation of the available methods. It is possible to define your own statistic, when it can be formulated as a function of observed and expected frequencies.
}
  \item{weight}{
When \code{weight} is specified, the function \code{\link{cosSparse}} is used for the co-occurrence statistics (with a Euclidean normalization, i.e. \code{norm2}). The specified weight function will be used, currently \code{idf}, \code{sqrt}, and \code{none} are available. For more details, and for instructions how to formulate your own weight function, see the discussion at \code{\link{cosSparse}}. When \code{weight} is specified, any specification of \code{method} is ignored.
  }
  \item{lowercase}{
Should all words be turned into lowercase? See \code{\link{splitText}} for discussion how this is implemented.
  }
  \item{best}{
When \code{best = T}, an additional sparse matrix is returned with a (simplistic) attempt to find the best translational equivalents between the texts.
  }
  \item{tol}{
Tolerance: remove all values between \code{-tol} and \code{+tol} in the result. Low values can mostly be ignored for co-occurrence statistics without any loss of information. However, what is considered `low' depends on the methods used to calculate the statistics. See discussion below.
  }
}
\details{
Care is taken in this function to match multiple verses that are translated into one verse, see \code{\link{bibles}} for a survey of the encoding assumptions taken here.

The parameter \code{method} can take anything that is also available for \code{\link{assocSparse}}. Similarities are computed using that function.

When \code{weight} is specified, the similarities are computed using \code{\link{cosSparse}} with default setting of \code{norm = norm2}. All available weights can also be used here.

The option \code{best = T} uses \code{\link{rowMax}} and \code{\link{colMax}}. This approach to get the `best' translation is really crude, but it works reasonably well with one-to-one and many-to-one situations. This option takes rather a lot more time to finish, as row-wise maxima for matrices is not trivial to optimize. Consider raising \code{tol}, as this removes low values that won't be important for the maxima anyway. See examples below.
	
Guidelines for the value of \code{tol} are difficult to give, as it depends on the method used, but also on the distribution of the data (i.e. the number of sentences, and the frequency distribution of the words in the text). Some suggestions:
\itemize{
	\item{when \code{weight} is specified, results range between -1 and +1. Then \code{tol = 0.1} should never lead to problems, but often even \code{tol = 0.3} or higher will lead to identical results.
	}
	\item{when \code{weight} is not specified (i.e. \code{assocSparse} will be used), then results range between \code{-inf} and \code{+inf}, so the tolerance is more problematic. In general, \code{tol = 2} seems to be unproblematic. Higher tolerance, e.g. \code{tol = 10} can be used to find the `obvious' translations, but you will loose some of the more incidental co-occurrences.
	}
  }
}
\value{
When \code{best = F}, a single sparse matrix is returned of type \code{dgCMatrix} with the values of the statistic chosen. All unique wordforms of text1 are included as row names, and those from text2 as column names.

When \code{best = T}, a list of two sparse matrices is returned:
	\item{\code{sim}}{the same matrix as above}
	\item{\code{best}}{a sparse pattern matrix of type \code{ngCMatrix} with the same dimensions as the previous matrix. Only the `best' translations between the two languages are marked}
}
\references{
Mayer, Thomas and Michael Cysouw. 2012. Language comparison through sparse multilingual word alignment. \emph{Proceedings of the EACL 2012 Joint Workshop of LINGVIS & UNCLH}, 54--62. Avignon: Association for Computational Linguistics.
}
\author{
Michael Cysouw
}
\seealso{
\code{\link{splitText}}, \code{\link{assocSparse}} and \code{\link{cosSparse}} are the central parts of this function. Also check \code{\link{rowMax}}, which is used to extract the `best' translations.
}
\examples{
data(bibles)

# ----- small example of co-occurrences -----

# as an example, just take partially overlapping parts of two bibles
# sim.words uses the names to get the paralellism right, so this works
eng <- bibles$eng[1:5000]
deu <- bibles$deu[2000:7000]
sim <- sim.words(eng, deu, method = res)

# but the statistics are not perfect (because too little data)
# sorted co-occurrences for the english word "your" in German:
sort(sim["your",], decreasing = TRUE)[1:10]

\donttest{
# ----- complete example of co-occurrences -----

# running the complete bibles takes a bit more time (but still manageable)
system.time(sim <- sim.words(bibles$eng, bibles$deu, method = res))

# results are much better
# sorted co-occurrences for the english word "your" in German:
sort(sim["your",], decreasing = TRUE)[1:10]


# ----- look for 'best' translations -----

# note that selecting the 'best' takes even more time
system.time(sim2 <- sim.words(bibles$eng, bibles$deu, method = res, best = TRUE))

# best co-occurrences for the English word "your"
which(sim2$best["your",])

# but can be made faster by removing low values
# (though the boundary in \code{tol =  5} depends on the method used
system.time(sim3 <- sim.words(bibles$eng, bibles$deu, best = TRUE, method = res, tol = 5))

# note that the decision on the 'best' remains the same here
all.equal(sim2$best, sim3$best)
}

# ----- computations also work with other languages -----

# All works completely language-independent
# translations for 'we' in Tagalog:
sim <- sim.words(bibles$eng, bibles$tgl, best = TRUE, weight = idf, tol = 0.1)
which(sim$best["we",])

}