\name{sim.strings}
\alias{sim.strings}

\title{
String similarity by cosine similarity between bigram vectors
}
\description{
Efficient computation of pairwise string similarities using a cosine similarity on bigram vectors.
}
\usage{
sim.strings(strings1, strings2 = NULL, sep = "", boundary = TRUE)
}
\arguments{
  \item{strings1, strings2}{
Vector with strings to be compared, will be treated \code{as.character}. When only \code{strings1} is provided, all pairwise similarities between its elements are computed. When two different input vectors are provided, the pairwise similarities between all elements from the first and the second vector are computed.
}
  \item{sep}{
Seperator used to split the strings into parts. This will be passed to \code{\link{strsplit}} internally, so there is no finegrained control possible on the splitting. If it is important to get the splitting exactly right, consider pre-processing the splitting by inserting a special symbol on the split-positions, and then choosing here to split by this special symbol.
}
  \item{boundary}{
In the default setting \code{boundary = T}, a special symbol is added to the front and to the end of each string, adding special bigrams for the initial and the final character. With words from real languages (which are mostly not very long) this has a strong impact.
}
}
\details{
The strings are converted into sparse matrices by \code{\link{splitStrings}}, and then \code{\link{assocSparse}} computes a cosine similarity on the bigram vectors. Only the option of bigrams is currently used, because for long lists of real words from a real language this seems to be an optimal tradoff between speed and useful similarity.
}
\value{
When either \code{length(strings1) == 1} or \code{length(strings2) == 1}, the result will be a normal vector with similarities between 0 and 1.

When both the input vectors are longer than 1, then the result will be a sparse matrix with similarities. When only \code{strings1} is provided, then the result is of type \code{dsCMatrix}. When two input vectors are provided, the result is of type \code{dgCMatrix}.
}
\author{
Michael Cysouw
}
\note{
The overhead of converting the strings into sparse matrices makes this function not optimal for small datasets. For large datasets the time of the conversion is negligable compared to the actual similarity computation, and then this approach becomes very worthwhile, because fast, and based on sparse matrix computation, that can be sped up by multicore processing in the future.

The result of \code{sim.strings(a)} and \code{sim.strings(a,a)} is identical, but the first version is more efficient, both as to processing time, as well as to the size of the resulting objects.
}
\seealso{
\code{\link{splitStrings}, \link{cosSparse}} on which this function is based. Compare with \code{\link[utils:adist]{adist}} from the utils package. On large datasets, \code{sim.strings} seems to be about a factor 30 quicker. The package \code{stringdist} offers many more string comparison methods.
}
\examples{
# ----- simple example -----

example <- c("still","till","stable","stale","tale","tall","ill","all")
( sim <- round( sim.strings(example), digits = 3) )

# show similarity in non-metric MDS
library(MASS)
mds <- isoMDS( as.dist(1-sim) )$points
plot(mds, type = "n", ann = FALSE, axes = FALSE)
text(mds, labels = example, cex = .7)

\dontrun{

# ----- large example -----

# This similarity is meant to be used for large lists of wordforms.
# for example, all 15526 wordforms from the English Dalby Bible
# takes just a few seconds for the more than 1e8 pairwise comparisons
data(bibles)
words <- splitText(bibles$eng)$wordforms
system.time( sim <- sim.strings(words) )

# see most similar words
rownames(sim) <- colnames(sim) <- words
sort(sim["walk",], decreasing = TRUE)[1:10]

# just compare all words to "walk". This is the same as above, but less comparisons
# note that the overhead for the sparse conversion and matching of matrices is large
# this one is faster than doing all comparisons, but only be a factor 10
system.time( sim <- sim.strings(words, "walk"))
names(sim) <- words
sort(sim, decreasing = TRUE)[1:10]

# ----- comparison with Levinshtein -----

# don't try this with 'adist' from the utils package, it will take long!
# for a comparison, only take 2000 randomly selected strings: about a factor 30 slower
w <- sample(words, 2000)
system.time( sim1 <- sim.strings(w) )
system.time( sim2 <- adist(w) )

# compare the current approach with relative levenshtein similarity
# = number of matches / ( number of edits + number of matches)
# for reasons of speed, just take 1000 random words from the english bible
w <- sample(words, 1000)
sim1 <- sim.strings(w)
tmp <- adist(w, counts = TRUE)
sim2 <- 1- ( tmp / nchar(attr(tmp, "trafos")) )

# plotting relation between the two 'heatmap-style'
# not identical, but usefully similar
image( log(table(
		round(as.dist(sim1) / 3, digits = 2) * 3,
		round(as.dist(sim2) / 3, digits = 2) * 3 )),
	xlab = "bigram similarity", ylab = "relative levenshtein similarity")

% benchmarking?
% s <- seq(500,10500,by=1000)
% t <- sapply(s,function(x){system.time(sim.strings(sample(words,x)))[3]})
% plot(log(s),log(t))
% lm(log(t[-c(1:5)])~log(s[-c(1:5)]))
}
}