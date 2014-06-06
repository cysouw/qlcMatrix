\name{recode}
\alias{recode}
\alias{write.recoding.template}
\title{
Recoding nominal data
}
\description{
Nominal data (`categorical data') are data that consist of attributes, and each attribute consists of various discrete values (`types'). The different values that are distinguished in comparative linguistics are mostly open to debate, and different scholars like to make different decisions as to the definition of values. The \code{recode} function allows for an easy and transparent way to specify a recoding of an existing dataset.
}
\usage{
recode(data, recoding)

write.recoding.template(attributes, data, file, yaml = TRUE)
}
\arguments{
  \item{data}{
  a data frame with nominal data, attributes as columns, observations as rows.
  }
  \item{recoding}{
  a \code{recoding} data structure, specifying the decisions of the recoding. It can also be a path to a file containing the specifications in YAML format. See Details.
  }
  \item{attributes}{
  a list of attributes to be recoded. Vectors (as elements of the list) are possible to specify combinations of attributes to be recoded as a single complex attribute.
  }
  \item{file}{
  file in which the recoding template should be written.
  }
  \item{yaml}{
  the recoding template is by default written to a file in YAML format. When \code{yaml=FALSE}, the template is not converted to YAML, but returned inside R as a nested list.
  }
}
\details{
Recoding nominal is considered too complex to be performed purely within R. It is proposed here to use a YAML document to specify the decisions that are taken in the recoding. The typical process of recoding will be to use \code{write.recoding.template} to prepare a skeleton that allows for quick and easy specification of a recoding. The function \code{recode} then combines the original data with the recoding, and produces a recoded data frame.

The \code{recoding data structure} in the YAML document basically consists of a list of recodings, each of which describes a new attribute, based on one or more attributes from the original data. Each new attribute is described by:
\itemize{
  \item{attribute}: the new attribute name.
  \item{values}: a character vector with the new value names.
  \item{link}: a numeric vector with length of the original number of values. Each entry specifies the number of the new value. Zero can be used for any values that should be ignored in the new attribute.
  \item{recodingOf}: the name(s) of the original attribute that forms the basis of the recoding. If there are multiple attributes listed, then the new attribute will be a combination of the original attributes.
  \item{OriginalValues}: a character vector with the value names from the original attribute. These are only added to the template to make it easier to specify the recoding. In the actual recoding the listing in this file will be ignored. It is important to keep the ordering as specified, otherwise the linking will be wrong. The ordering of the values follows the result of \code{levels}, which is determined by the current locale.
}

For writing recodings by hand, there are various shortcuts allowed:
\itemize{
  \item the names \code{attributes}, \code{values}, etc. can be abbreviated. The first letter should be sufficient.
  \item the \code{recodingOf} can be the full name of the attribute in the original data, or simply a number of the column in the data frame.
  \item the specification of \code{attribute} and \code{values} can be left out, although the result will be uninformative names like `Att1' and `Val1'.
  \item it is also possible to add an item \code{doNotRecode} with a vector of original attribute names (or column numbers). These original attributes will then be included unchanged in the recoded data table.
}

A minimal recoding consist thus of a specification of \code{recodingOf} and \code{link}. Without \code{link} nothing will be recoded. Omitting \code{recodingOf} will lead to an error.

There is a vignette available with detailed information about the process of recoding, check \code{\link{recoding nominal data}}.
}
\value{
\code{recode} returns a data frame with the recoded attributes

\code{write.recoding.template} by default (when \code{yaml=TRUE}) writes a YAML structure to the specified file. When \code{yaml=FALSE} the same structure is returned inside R as a nested list.
}
\references{
Cysouw, Michael, Jeffrey Craig Good, Mihai Albu and Hans-Jörg Bibiko. 2005. Can GOLD "cope" with WALS? Retrofitting an ontology onto the World Atlas of Language Structures. \emph{Proceedings of E-MELD Workshop 2005}, \url{http://emeld.org/workshop/2005/papers/good-paper.pdf}
}
\author{
Michael Cysouw
}
\seealso{
\code{\link{wals}} contains typical data that most people would very much like to recode before using for further analysis.
}
\examples{

}