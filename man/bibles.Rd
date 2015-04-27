\name{bibles}
\alias{bibles}
\docType{data}
\title{
A selection of bible-texts
}
\description{
A selection of six bible texts as prepared by the \url{paralleltext.info} project.
}
\usage{data(bibles)}
\format{
A list of five elements
  \describe{
    \item{\code{verses}}{a character vector with all 43904 verse numbers as occurring throughout all translations as collected in the paralleltext.info project. This vector is used to align all texts to each other. The verse-numbers are treated as characters so ordering and matching works as expected.}
    \item{\code{eng}}{The English `Darby' Bible translation from 1890 by John Nelson Darby. Paralleltext.info code of this text is \href{http://paralleltext.info/data/eng-x-bible-darby/}{eng-x-bible-darby}.}
	\item{\code{deu}}{The Bible in German. Schlachter Version von 1951. Genfer Bibelgesellschaft 1951. Paralleltext.info code of this text is \href{http://paralleltext.info/data/deu-x-bible-schlachter/}{deu-x-bible-schlachter}.}
	\item{\code{tgl}}{The New Testament in Tagalog. Philippine Bible Society 1996. Paralleltext.info code of this text is \href{http://paralleltext.info/data/tgl-x-bible-1996/}{tgl-x-bible-1996}.}
	\item{\code{aak}}{The New Testament in the Ankave language of Papua New Guinea. Wycliffe Bible Translators, Inc. 1990. Paralleltext.info code of this text is \href{http://paralleltext.info/data/aak-x-bible-aak/}{aak-x-bible-aak}.}
  }
}
\details{
For details on the formatting, see \url{paralleltext.info}. Basically, all verse-numbering is harmonized, the text is unicode normalized, translations that capture multiple verses are included in the first of those verses, with the others left empty. Empty verses are thus a sign of combined translations. Verses that are not translated simply do not occur in the original files. Most importantly, the text are tokenized as to wordform, i.e. all punctuation and other non-word-based symbols are separated by spaces. In this way, space can be used for a quick wordform-based tokenization. The addition of spaces has been manually corrected to achieve a high precision of language-specific wordform tokenization.

The Bible texts are provided as named vectors of strings, each containing one verse. The names of the vector are codes for the verses. See Mayer & Cysouw (2014) for more information about the verse IDs and other formatting issues.
}
\source{
All data is taken from \url{paralleltext.info}. For further information and links to the original versions, please check the detailed meta-information for all texts as provided on the webpage.
}
\references{
Mayer, Thomas and Michael Cysouw. 2014. Creating a massively parallel Bible corpus. \emph{Proceedings of LREC 2014}.}
\examples{
# ----- load data -----

data(bibles)

# ----- separate into sparse matrices -----

# use splitText to turn a bible into a sparse matrix of wordforms x verses
E <- splitText(bibles$eng, simplify = TRUE, lowercase = FALSE)

# all wordforms from the first verse
# (internally using pure Unicode collation, i.e. ordering is determined by Unicode numbering)
which(E[,1] > 0)

# ----- co-occurrence across text -----

# how often do 'father' and 'mother' co-occur in one verse?
# (ignore warnings of chisq.test, because we are not interested in p-values here)

( cooc <- table(E["father",] > 0, E["mother",] > 0) )
suppressWarnings( chisq.test(cooc)$residuals )

# the function 'sim.words' does such computations efficiently 
# for all 15000 x 15000 pairs of words at the same time

system.time( sim <- sim.words(bibles$eng, lowercase = FALSE) )
sim["father", "mother"]
}
\keyword{datasets}