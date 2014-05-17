\name{jMatrix}
\alias{jMatrix}
\alias{jcrossprod}
\alias{tjcrossprod}

\title{
Harmonize (`join') sparse matrices
}
\description{
A utility function to make sparse matrices conformable semantically. Not only are the dimensions made conformable in size, but also the content of the dimensions. Formulated differently, this function harmonizes two matrices on a dimensions that have the same entities, but in a different order (and possibly with different subsets). Given two matrices with such (partly overlapping) dimensions, two new matrices are generated to reorder the original matrices via a matrix product to make them conformable. In an abstract sense, this is similar to an SQL `inner join' operation.
}
\usage{
jMatrix(rownamesX, rownamesY, collation.locale = "C")

jcrossprod(X, Y, rownamesX = rownames(X), rownamesY = rownames(Y))
tjcrossprod(X, Y, colnamesX = colnames(X), colnamesY = colnames(Y))
}
\arguments{
  \item{rownamesX, rownamesY}{
rownames to be joined from two matrices.
  }
  \item{X, Y}{
sparse matrices to be made (semantically) conformable.
  }
  \item{colnamesX, colnamesY}{
colnames to be joined from two matrices.
  }
  \item{collation.locale}{
locale to be used for ordering of the joined dimension. Defaults to pure numerical unicode ordering "C". See \code{\link{ttMatrix}} for details.
  }
}
\details{
Given a sparse matrix X with rownames rX and a sparse matrix Y with rownames rY, the function \code{jMatrix} produces joined rownames rXY with all unique entries in \code{c(rX, rY)}, reordered according to the specified locale, if necessary.

Further, two sparse matrices M1 and M2 are returned to link X and Y to the new joined dimension rXY. Specifically, X2 = M1 \%*\% X and Y2 = M2 \%*\% Y will have conformable rXY rows, so crossprod(X2, Y2) can be computed. Note that the result will be empty when there is no overlap between the rownames of X and Y.

The function \code{jcrossprod} is a shortcut to compute the above crossproduct immediately, using \code{jMatrix} internally to harmonize the rows. Similarly, \code{tjcrossprod} computes the tcrossprod, harmonizing the \emph{columns} of two matrices using \code{jMatrix}.
}
\value{
\code{jMatrix} returns a list of three elements (for naming, see Details above):
 \item{M1}{
 	sparse pattern matrix of type \code{ngCMatrix} with dimensions \code{c(length(rXY),length(rX))}
 	}
 \item{M2}{
 	sparse pattern matrix of type \code{ngCMatrix} with dimensions \code{c(length(rXY),length(rY))}
 	}
 \item{rownames}{
 	unique joined row names rXY
 	}

\code{jcrossprod} and \code{tjcrossprod} return a sparse Matrix of type \code{ngCMatrix} when both X and Y are pattern matrices. Otherwise they return a sparse Matrix of type \code{dgCMatrix}.
}
\author{
Michael Cysouw
}
\note{
Actually, it is unimportant whether the inputs to \code{jMatrix} are row or column names. However, care has to be taken to use the resulting matrices in the right transposition. To make this function easier to explain, I consistently talk only about row names above.
}
\examples{
# example about INNER JOIN from wikipedia
# http://en.wikipedia.org/wiki/Sql_join#Inner_join
# this might look complex, but it is maximally efficient on large sparse matrices

# Employee table as sparse Matrix
Employee.LastName <- c("Rafferty","Jones","Heisenberg","Robinson","Smith","John")
Employee.DepartmentID <- c(31,33,33,34,34,NA)
E.LN <- ttMatrix(Employee.LastName, simplify = TRUE)
E.DID <- ttMatrix(Employee.DepartmentID, simplify = TRUE)

( Employees <- tcrossprod(E.LN, E.DID) )

# Department table as sparse Matrix
Department.DepartmentID <- c(31,33,34,35)
Department.DepartmentName <- c("Sales","Engineering","Clerical","Marketing")
D.DID <- ttMatrix(Department.DepartmentID, simplify = TRUE)
D.DN <- ttMatrix(Department.DepartmentName, simplify = TRUE)

( Departments <- tcrossprod(D.DN, D.DID) )

# INNER JOIN on DepartmentID (i.e. on the columns of these two matrices)
# result is a sparse matrix linking Employee.LastName to Department.DepartmentName, 
# internally having used the DepartmentID for the linking

( JOIN <- tjcrossprod(Employees, Departments) )

# Note that in this example it is much easier to directly use jMatrix on the DepartmentIDs
# instead of first making sparse matrices from the data
# and then using tjcrossprod on the matrices to get the INNER JOIN
# (only the ordering is different in this direct approach)

J <- jMatrix(Employee.DepartmentID, Department.DepartmentID)
JOIN <- crossprod(J$M1, J$M2)
rownames(JOIN) <- Employee.LastName
colnames(JOIN) <- Department.DepartmentName
JOIN
}