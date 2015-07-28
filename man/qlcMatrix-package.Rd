\name{qlcMatrix-package}
\alias{qlcMatrix}
\docType{package}
\title{
Utility sparse matrix functions for Quantitative Language Comparison (QLC)
}
\description{
This package contains various functions that extend the functionality of the \code{Matrix} package for using sparse matrices. Some of the functions are very general, while other are highly specific for special data format as used for quantitative language comparison.
}
\details{
\tabular{ll}{
Package: \tab qlcMatrix\cr
Type: \tab Package\cr
Version: \tab 0.9.5\cr
Date: \tab 2015-07-28\cr
License: \tab GPL-3\cr
}
This package contains various different kinds of function. 

First, some general utility functions to deal with sparse matrices: (i) \code{\link{rowMax}} to compute and identify row-wise maxima and minima in sparse matrices, (ii) \code{\link{rKhatriRao}} to remove empty rows in a KhatriRao product (but still get the right rownames) and (iii) \code{\link{rSparseMatrix}} to produce random sparse matrices. There are also some experimental basic methods for handling sparse arrays ("tensors"), most interestingly \code{\link{unfold}}.

Second, some general functions to compute associations between the columns of sparse matrices, with possibilities for extension for ad-hoc measures: \code{\link{cosSparse}}, \code{\link{corSparse}}, and \code{\link{assocSparse}}  There are special versions of these for nominal data \code{\link{cosNominal}, \link{assocNominal}}. 

Third, there are three central functions needed to efficiently turn data from quantitative language comparison into sparse matrices. These basic functions are then used by high-level function in this package. Although these functions might seem almost trivial, they form the basis for many highly complex computations. They are \code{\link{ttMatrix}}, \code{\link{pwMatrix}} and \code{\link{jMatrix}}.

Fourth, there are some high-level convenience function that take specific data formats from quantitative language comparison and turn them into set of sparse matrices for efficient computations. They might also be useful for other data types, but various details decisions are specifically tailored to the envisioned data types. These functions are \code{\link{splitTable}} \code{\link{splitStrings}}, \code{\link{splitWordlist}}, and \code{\link{splitText}}.

Finally, there are various shortcuts to directly compute similarity matrices from various kinds of data: \code{\link{sim.nominal}, \link{sim.words}, \link{sim.strings}, \link{sim.wordlist}}. These are specifically tailored towards specific kinds of data, though they might also be useful elsewhere. Also, the code is mostly easy wrappers around the \code{split} and \code{cos/assoc} functions, so it should not be difficult to adapt these functions to other needs.
}
\author{
Michael Cysouw <cysouw@mac.com>
}
\references{
Cysouw, Michael. 2014. \emph{Matrix Algebra for Language Comparison}. Manuscript.

Mayer, Thomas and Michael Cysouw. 2012. Language comparison through sparse multilingual word alignment. \emph{Proceedings of the EACL 2012 Joint Workshop of LINGVIS & UNCLH}, 54--62. Avignon: Association for Computational Linguistics.

Prokić, Jelena and Michael Cysouw. 2013. Combining regular sound correspondences and geographic spread. \emph{Language Dynamics and Change} 3(2). 147--168.
}
\keyword{ package }
