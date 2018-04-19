\name{huber}
\alias{huber}
\docType{data}
\title{
Comparative vocabulary for indigenous languages of Colombia (Huber & Reed 1992)
}
\description{
Data from Huber & Reed (1992), containing a comparative vocabulary (a `wordlist') for 69 indigenous languages from Colombia.
}
\usage{data(huber)}
\format{
  A data frame with 27521 observations on the following 4 variables.
  \describe{
    \item{\code{CONCEPT}}{a factor with 366 levels, indicating the comparative concepts}
    \item{\code{COUNTERPART}}{a character vector listing the actual wordforms as described in Huber & Reed 1992}
	\item{\code{DOCULECT}}{a factor with 71 levels, indicating the languages from which the wordforms are taken (`documented lects', abbreviated as `doculect'). These are 69 indigenous languages from Colombia, and English and Spanish.}
	\item{\code{TOKENS}}{a tokenized version of the counterparts: spaces are added between graphemic units (i.e. groups of unicode characters that are functioning as a single unit in the orthography)}
  }
}
\details{
The editors have attempted to use a harmonized orthography throughout all languages, approximately based on IPA, though there are still many language-specific idiosyncrasies included. However, the translations into English and Spanish are written in their regular orthography, and not in the IPA-dialect as used for the other languages. In general, the `translations' into English and Spanish are simply lowercase versions of the concept-names, included here to more flexibly identify the meaning of words in the Colombian languages. In many cases these translations are somewhat clunky (e.g. `spring of water'), and are missing the proper orthography details (e.g. `Adams apple').

The book was digitized in the QuantHistLing project and provided here as an example of dealing efficiently with reasonably large data. Care has been taken to faithfully represent the original transcription from the printed version.
}
\source{
Huber, Randall Q. & Robert B. Reed. 1992. \emph{Vocabulario Comparativo: Palabras Selectas de Lenguas Indigenas de Colombia.} Bogota: Instituto Linguistico de Verano. available online at \url{http://www.sil.org/americas/COLOMBIA/show_work.asp?id=928474518886}. Copyright 2014 SIL International.}
\examples{
data(huber)
head(huber)
}
\keyword{datasets}