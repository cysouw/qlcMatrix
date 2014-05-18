\name{sim.nominal}
\alias{sim.nominal}
\alias{sim.att}
\alias{sim.obs}
\title{
Similarity-measures for nominal variables
}
\description{
Nominal variables can be encoded as a combination of a sparse incidence and index matrix, as discussed at \code{\link{splitTable}}. The present two functions are easy-to-use shortcuts to use those sparse matrices to computes pairwise similarities, either between observations (\code{sim.obs}) or attributes (\code{sim.att}).
}
\usage{
sim.att(D, method = "chuprov", sparse = TRUE)
sim.obs(D, method = "hamming", sparse = TRUE)
}
\arguments{
  \item{D}{
Dataframe with nominal attributes (`variables') as columns and observations as rows.
  }
  \item{method}{
method to be used for similarity computation. See Details below.
  }
  \item{sparse}{
All methods try to be as sparse as possible. Specifically, when there are no observed co-occurrence, then nothing is computed. This might lead to slight deviations in the results for some methods. Set \code{sparse=F} to force computation for all cells. This leads to non-sparse results, so use with caution with large datasets.
  }
}
\details{
The function \code{sim.att} and \code{sim.obs} are convenience wrappers around the basic \code{\link{cosRow}, \link{cosCol}} and \code{\link{assocRow}, \link{assocCol}} functions. The \code{sim} functions take a dataframe as input, internally calling \code{splitTable} to turn the dataframe into sparse matrices, and then applying sparse matrix algebra to efficiently compute similarities. Currently only a few exemplary methods are encoded.

\code{sim.att} computes similarities between the different nominal variables. The method \code{chuprov} computes Chuprov's T (very similar to Cramer's V, but easier to compute efficiently). The method \code{g} computes the G-test from Sokal and Rohlf (1982), also known as Dunning's G from Dunning (1993). This G is closely related to Mutual Information (G = 2*N*MI, with N being the sample size). The method \code{mutual} returns the mutual information, and the method \code{variation} returns the so-called `variation of information' (join information - mutual information). Note that the this last one is a metric, not a similarity. All these methods can be abbreviated, e.g use "c", "g", "m", and "v".

\code{sim.obs} computes similarities between the different observation for the nominal variables. The method \code{hamming} computes the relative Hamming similarity, i.e. the number of similarities devided by the number of comparisons made (Goebl 1984 calls this the `Relativer Identitaetswert'). The method \code{weighted} uses an inverse square root weighting on all similarities, i.e. rare similarities count more. This is very similar to Goebl's "Gewichteter Identitaetswert", though note that his definition is slightly different from the one used here. Further, all methods as defined for \code{\link{assocSparse}} can be used here, i.e. \code{res, pmi, wpmi, poi}, and new methods can be defined according to the explanations as \code{assocSparse}.
}
\value{
All methods return symmetric similarity matrices in the form \code{dsCMatrix}, only specifying the upper triangle. The only exception is when \code{sparse=T} is chose, then the result will be in the form \code{dsyMatrix}.
}
\note{
Note that these methods automatically take missing data into account. They also work with large amount of missing data, but of course the validity of any similarity with much missing data is problematic.

The \code{sim.att} and \code{sim.obs} methods by default use sparse computations, which leads (among other effects) to errors on the diagonal. The main diagonal should be one everywhere by definition, but this will only be the case with the option \code{sparse = F}. The deviations with \code{sparse = T} should be minimal in the non-diagonal entries, but computations should be faster, and the results often take up less space. 
}
\author{
Michael Cysouw
}
\references{
Goebl, Hans. 1984. \emph{Dialektometrische Studien: anhand italoromanischer, raetoromanischer und galloromanischer Sprachmaterialien aus AIS und AFL.} (Beihefte zur Zeitschrift fuer Romanische Philologie). Tuebingen: Niemeyer.

Dunning, Ted. 1993. Accurate methods for the statistics of surprise and coincidence. \emph{Computational linguistics} 19(1). 61-74.

}
\examples{
# first a simple example using the farms-dataset from MASS
library(MASS)

# similarities between farms
s <- sim.obs(farms)
plot(hclust(as.dist(1-s), method = "ward"))

# similarities between attributes (`variables`)
s <- sim.att(farms)
plot(hclust(as.dist(1-s), method = "ward"))

\dontrun{
# a larger example with lots of missing data: the WALS-data as included here
# computations go reasonably quick
# (on 2566 observations and 131 attributes with 630 different values in total)
data(wals)
system.time(s <- sim.att(wals$data))
rownames(s) <- colnames(wals$data)
plot(hclust(as.dist(1-s), method = "ward"), cex = 0.5)

# Note that using sparse=T speeds up computations because it 
# ignores zero co-occurrences
# This leads to small errors in the computation of Chuprov's T
system.time( # faster
	chup.sparse <- sim.att(wals$data,  method = "chuprov", sparse = TRUE)
)
system.time( # slower
	chup.full <- sim.att(wals$data, method = "chuprov", sparse = FALSE)
)

# The sparse approach is almost identical to the full approach.
# sparse sligtly underestimates the real values for Chuprov's T 
plot(as.dist(chup.sparse), as.dist(chup.full))

# some more similarities on the attributes
g <- sim.att(wals$data, method = "g") # Dunning's G
m <- sim.att(wals$data, method = "mutual") # Mutual Information
v <- sim.att(wals$data, method = "variation") # Variation of Information

# Note the strong differences between these approaches
pairs(~ as.dist(chup.sparse) + as.dist(m) + as.dist(g) + as.dist(v),
	labels=c("Chuprov's T","Mutual Information","G-statistic","Variation of Information"))
	
# Relative Hamming similarity on all observations (languages) in WALS
# time is not a problem, but the data is so sparse
# that for many language-pairs there is no shared data
system.time( s <- sim.obs(wals$data))

# select only the 168 language with more than 80 datapoints
sel <- wals$data[apply(wals$data,1,function(x){sum(!is.na(x))})>80,]

# compare different similarities
w <- sim.obs(sel, "weighted")
h <- sim.obs(sel, "hamming")
r <- sim.obs(sel, "res")
p <- sim.obs(sel, "poi")
m <- sim.obs(sel, "wpmi")
i <- sim.obs(sel, "pmi")

pairs(~ as.dist(w) + as.dist(h) + as.dist(r) + as.dist(p) + as.dist(m) + as.dist(i),
	labels = c("weighted","hamming","residuals","poisson","weighted PMI","PMI"))
}
}