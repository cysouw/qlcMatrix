\name{sim.strings}
\alias{sim.strings}

\title{
String similarity by cosine similarity between bigram vectors
}
\description{
Efficient computation of pairwise string similarities using a cosine similarity on bigram vectors.
}
\usage{
sim.strings(strings1, strings2 = NULL, sep = "", boundary = TRUE, ...)
}
\arguments{
  \item{strings1, strings2}{
Vector with strings to be compared, will be treated \code{as.character}. When only \code{strings1} is provided, all pairwise similarities between its elements are computed. When two different input vectors are provided, the pairwise similarities between all elements from the first and the second vector are computed.
}
  \item{sep}{
Separator used to split the strings into parts. This will be passed to \code{\link{strsplit}} internally, so there is no fine-grained control possible on the splitting. If it is important to get the splitting exactly right, consider pre-processing the splitting by inserting a special symbol on the split-positions, and then choosing here to split by this special symbol.
}
  \item{boundary}{
In the default setting \code{boundary = T}, a special symbol is added to the front and to the end of each string, adding special bigrams for the initial and the final character. With words from real languages (which are mostly not very long) this has a strong impact.
}
  \item{...}{
Further arguments passed to \code{\link{splitStrings}}.
}
}
\details{
The strings are converted into sparse matrices by \code{\link{splitStrings}}, and then \code{\link{assocSparse}} computes a cosine similarity on the bigram vectors. Only the option of bigrams is currently used, because for long lists of real words from a real language this seems to be an optimal tradeoff between speed and useful similarity.
}
\value{
When either \code{length(strings1) == 1} or \code{length(strings2) == 1}, the result will be a normal vector with similarities between 0 and 1.

When both the input vectors are longer than 1, then the result will be a sparse matrix with similarities. When only \code{strings1} is provided, then the result is of type \code{dsCMatrix}. When two input vectors are provided, the result is of type \code{dgCMatrix}.
}
\author{
Michael Cysouw
}
\note{
The overhead of converting the strings into sparse matrices makes this function not optimal for small datasets. For large datasets the time of the conversion is negligible compared to the actual similarity computation, and then this approach becomes very worthwhile, because fast, and based on sparse matrix computation, that can be sped up by multicore processing in the future.

The result of \code{sim.strings(a)} and \code{sim.strings(a,a)} is identical, but the first version is more efficient, both as to processing time, as well as to the size of the resulting objects.
}
\note{
There is a bash-executable \code{simstrings} distributed with this package (based on the \code{docopt} package) that let you use this function directly in a bash-terminal. The easiest way to use this executable is to softlink the executable to some directory in your bash PATH, for example \code{/usr/local/bin} or simply \code{~/bin}. To softlink the function \code{sim.strings} to this directory, use something like the following in your bash terminal:

\code{ln -is `Rscript -e 'cat(system.file("exec/simstrings", package="qlcMatrix"))'` ~/bin}

From within R your can also use the following (again, optionally changing the linked-to directory from \code{~/bin} to anything more suitable on your system):

\code{file.symlink(system.file("exec/simstrings", package="qlcMatrix"), "~/bin")}
}
\seealso{
\code{\link{splitStrings}, \link{cosSparse}} on which this function is based. Compare with \code{\link[utils:adist]{adist}} from the utils package. On large datasets, \code{sim.strings} seems to be about a factor 30 quicker. The package \code{stringdist} offers many more string comparison methods.
}
\examples{
# ----- simple example -----

example <- c("still","till","stable","stale","tale","tall","ill","all")
( sim <- round( sim.strings(example), digits = 3) )

\donttest{

# show similarity in non-metric MDS
mds <- MASS::isoMDS( as.dist(1-sim) )$points
plot(mds, type = "n", ann = FALSE, axes = FALSE)
text(mds, labels = example)

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
# for a comparison, only take 2000 randomly selected strings: about a factor 20 slower
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
	xlab = "bigram similarity", ylab = "relative Levenshtein similarity")
}
}