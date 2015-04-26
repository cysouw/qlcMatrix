\name{splitTable}
\alias{splitTable}

\title{
Construct sparse matrices from a nominal matrix/dataframe
}
\description{
This function splits a matrix or dataframe into two sparse matrices: an incidence and an index matrix. The incidence matrix links the observations (rows) to all possible values that occur in the original matrix. The index matrix links the values to the attributes (columns). This encoding allows for highly efficient calculations on nominal data.
}
\usage{
splitTable(data, 
    attributes = colnames(data), observations = rownames(data),
		name.binder = ":", split = NULL)
}

\arguments{
  \item{data}{
a matrix (or data.frame) with observations as rows and nominal attributes as columns. Numerical values in the data will be interpreted as classes (i.e. as nominal data, aka categorical data). 
}
  \item{attributes, observations}{
The row names and column names of the data will by default be extracted from the input matrix. However, in special situations they can be added separately. Note that names of the attributes (`column names') are needed for the production of unique value names. In case of absent column names, new column names of the form `X1' are automatically generated.
}
  \item{name.binder}{
Character string to be added between attribute names and value names. Defaults to colon `:'.
}
  \item{split}{
Character string to split values in each cell of the table, e.g. a comma or semicolon.
  }
}
\value{
A list containing the various row and column names, and the two sparse pattern matrices of format \code{ngCMatrix}:
	\item{attributes}{vector of attribute names}
	\item{values}{vector of unique value names}
	\item{observations}{vector of observation names}
	\item{OV}{sparse pattern matrix with observations as rows (O) and values as columns (V)}
	\item{AV}{sparse pattern matrix with attributes as rows (A) and values as columns (V)}
}
\author{
Michael Cysouw
}
\note{
Input of data as a matrix or as a data.frame might lead to different ordering of the values because collation differs per locale (see the discussion at \code{\link{ttMatrix}}, which does the heavy lifting here).

The term `attribute' is used in instead of the more common term `variable' to allow  for the capital A to uniquely identify attributes and V to identify values.
}
\seealso{
	More methods to use such split tables can be found at \code{\link{sim.nominal}}.
}
\examples{
# start with a simple example from the MASS library
# compare the original data with the encoding as sparse matrices
library(MASS)
farms
splitTable(farms)

# As a more involved example, consider the WALS data included in this package
# Transforming the reasonably large WALS data.frame \code{wals$data} is fast
# (2566 observations, 131 attributes, 630 unique values)
# The function `str' gives a useful summary of the result of the splitting
data(wals)
system.time(W <- splitTable(wals$data))
str(W) 

# Some basic use examples on the complete WALS data.
# The OV-matrix can be used to quickly count the number of similarities 
# between all pairs of observations. Note that with the large amount of missing values
# the resulting numbers are not really meaningfull. Some normalisation is necessary.
system.time( O <- tcrossprod(W$OV*1) )
O[1:10,1:10]

# The number of comparisons available for each pair of attributes
system.time( N <- crossprod(tcrossprod(W$OV*1, W$AV*1)) )
N[1:10,1:10]

# compute the number of available datapoints per observation (language) in WALS
# once the sparse matrices W are computed, such calculations are much quicker than 'apply'
system.time( avail1 <- rowSums(W$OV) )
system.time( avail2 <- apply(wals$data,1,function(x){sum(!is.na(x))}))
names(avail2) <- NULL
all.equal(avail1, avail2)

# Very unequal availability of data over languages in WALS
hist(avail1)
}
