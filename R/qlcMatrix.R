# author: Michael Cysouw
# title: collection of functions using sparse matrices for language comparison

# needs the sparse matrix library
# require(Matrix, quietly = TRUE)
# specified in NAMESPACE

# ============================================================
# Low level functions to make special kinds of sparse matrices
# ============================================================

# make type-token (tt) Matrix from vector
# result: Types x Tokens, types are returned as separate rownames

ttMatrix <- function(vector, collation.locale = "C", simplify = FALSE) {

	# change locale for collation, defaults to pure unicode locale "C"
	# setting NULL takes current locale on system
	Sys.getlocale("LC_COLLATE") -> current.locale
	if (!is.null(collation.locale)) {
		Sys.setlocale("LC_COLLATE", collation.locale)
	}

	# factorization
	factor <- factor(vector, exclude = NULL) # remove non-used levels
	indices <- as.numeric(factor)
	names <- levels(factor)

	# just in case that there is missing data
	indices <- na.omit(indices)
	available <- which(!is.na(factor))
	rows <- max(indices)
	cols <- length(factor)

	# make sparse matrix
	M <- sparseMatrix(	i = indices,
						j = available,
						dims = c(rows,cols)
						)

	# change locale back to original
	Sys.setlocale("LC_COLLATE", current.locale)
	
	if (simplify) {
		rownames(M) <- names
		colnames(M) <- vector
		return(M)
	} else {			
		return(	list(	M = M, 
						rownames = names
						))
	}
}

# make part-whole (pw) Matrix from tokenized strings
# result: Segments x Strings, segments are returns as separate rownames
# gap is needed for not obtaining overlap for ngrams.
# gap.length = 1 is sufficient for bigrams, gap.length = 2 for 3-grams, etc.

pwMatrix <- function(strings, sep = "", gap.length = 0, gap.symbol = "\u00B7", simplify = FALSE) {

	# just to be sure that we are dealing with strings
	strings <- as.character(strings)
	
	# split the strings by specified separator
	parsed.strings <- strsplit(strings,split=sep)
	
	# count the number of segments per string	
	strings.length <- sapply(parsed.strings,length)
	
	# the following is simply used when no gaps are necessary
	# also a special case when there is only one string, just to catch errors
	if (gap.length == 0 | length(strings) == 1) {
		segments <- unlist(parsed.strings)
		indices <- 1:length(segments)		
	}

	# for unconnected lists of strings, add gaps to get ngrams right later on
	if (gap.length > 0 & length(strings) > 1) {
		
		# add gaps after strings and make one long vector with all segments
		gap <- rep(gap.symbol, gap.length)
		add.gap <- function(x) {c(x,gap)}
		segments <- unlist(sapply(parsed.strings, add.gap, simplify = FALSE))
		
		# and remove gap at the end of the long string
		segments <- head(segments,-gap.length)
		
		# make indices, and ignore gaps for segment indices in matrix
		indices <- (1:length(segments))[-which(segments == gap.symbol)]		
	}
						
	# part-whole Matrix: segments x strings
	M <- sparseMatrix(	i = indices,
						j = rep.int(1:length(strings),strings.length)
						)

	if (simplify) {
		rownames(M) <- segments
		colnames(M) <- strings
		return(M)
	} else {			
		return(	list(	M = M, 
						rownames = segments
						))
	}
}


# Harmonize (alike to SQL "join") two matrices on a dimensions that have the same entities, but in a different order (and possibly with different subsets)
# The idea is to take two factors, and return two Type-Token matrices, in which the types (in the rows) are harmonized, and returned as rownames
# The matrix t(M1) %*% M2 (with the harmonized rows in the middle) can be used to JOIN two tables.

jMatrix <- function(rownamesX, rownamesY, collation.locale = "C") {

	# joined matrix
	J <- ttMatrix(c(rownamesX,rownamesY), collation.locale =  collation.locale)
	rownames <- J$rownames

	# split the joined matrix
	M <- t(J$M)
	M1 <- t(head(M,length(rownamesX)))
	M2 <- t(tail(M,length(rownamesY)))
	
	return(	list(	M1 = M1,
					M2 = M2,
					rownames = rownames
					))
}

jcrossprod <- function(X, Y, rownamesX = rownames(X), rownamesY = rownames(Y)) {
	
	J <- jMatrix(rownamesX, rownamesY)

	if (is(X,"nMatrix") & is(Y,"nMatrix")) {
		M <- crossprod( J$M1 %*% X, J$M2 %*% Y )
	} else {
		M <- crossprod( (J$M1*1) %*% X, (J$M2*1) %*% Y )
	}
	return(M)
}

tjcrossprod <- function(X, Y, colnamesX = colnames(X), colnamesY = colnames(Y)) {
	
	J <- jMatrix(colnamesX, colnamesY)

	if (is(X,"nMatrix") & is(Y,"nMatrix")) {
		M <- tcrossprod( X %*% t(J$M1), Y %*% t(J$M2) )
	} else {
		M <- tcrossprod( X %*% t(J$M1*1), Y %*% t(J$M2*1) )
	}
	return(M)
}

# ============================================================
# Special "reduced" KhatriRao version in which empty rows are removed.
# ============================================================

# This is only important for the new rownames, as now only rownames are produced for the non-empty rows. That is more efficient than making all rownames, as in the offical version in the Matrix package. It is *extremely* tricky to get the names right: watch out with the order of the indices!

rKhatriRao <- function(X, Y, rownamesX = rownames(X), rownamesY = rownames(Y), simplify = FALSE, binder = ":", FUN = "*") {

	# sparse KhatriRao
	M <- KhatriRao(X, Y, FUN = FUN)

	# remove empty rows
	selection <- rowSums(M, sparseResult = TRUE) > 0
	M <- M[selection@i,]
	
	# make names for the non-empty rows
	nonzero <- Matrix(	selection,
						nrow = nrow(Y),
						ncol = nrow(X),
						sparse = TRUE
						)
	nonzero <- as(nonzero,"TsparseMatrix")

	rownamesM <- paste(	rownamesY[nonzero@i + 1],
						rownamesX[nonzero@j + 1],	
						sep = binder
						)
	
	if (simplify) {
		rownames(M) <- rownamesM
		colnames(M) <- colnames(X)
		return(M)
	} else {
		return(	list(	M = M,
						rownames = rownamesM
						))
	}
}

# ============================================================
# Construct random sparse matrices, useful for testing
# ============================================================

# code from Martin Maechler

rSparseMatrix <- function(nrow, ncol, nnz, 
                          rand.x = function(n) round(rnorm(nnz), 2), ...)
{
    stopifnot((nnz <- as.integer(nnz)) >= 0,
              nrow >= 0, ncol >= 0, nnz <= nrow * ncol)
    if (is.null(rand.x)) {
    	sparseMatrix(i = sample(nrow, nnz, replace = TRUE),
                     j = sample(ncol, nnz, replace = TRUE),
                     dims=c(nrow,ncol))
    } else {
    sparseMatrix(i = sample(nrow, nnz, replace = TRUE),
                 j = sample(ncol, nnz, replace = TRUE),
                 x = rand.x(nnz), dims = c(nrow, ncol), ...)
    }
}

# ============================================================
# Unfold blockmatrix, first by column groups, optionally also by rowgroups
# ============================================================

unfold <- function(X, colGroups, rowGroups = NULL) {

	if (is.vector(colGroups)) {
		colGroups <- ttMatrix(colGroups)$M
	} else {
		colGroups <- as(colGroups, "dgCMatrix")
	}

	U <- KhatriRao(colGroups,X)
	L <- as(kronecker( t(rep(1,nrow(colGroups))), Diagonal(nrow(X)) ),"CsparseMatrix")
		
	if (is.null(rowGroups)) {
		return( list( U=U, L=L ) )
	} else {
		
		if (is.vector(rowGroups)) {
			rowGroups <- ttMatrix(rowGroups)$M
		} else {
			rowGroups <- as(rowGroups, "dgCMatrix")
		}

		R <- as(kronecker( rep(1,nrow(rowGroups)), Diagonal(ncol(U))),"CsparseMatrix")
		rowGroups <- kronecker( t(rep(1,nrow(colGroups))), rowGroups )
		U <- t( KhatriRao(rowGroups , t(U)))

		return( list( U=U, L=L, R=R ))
	}
}

# ======================
# Maximum per row/column
# ======================

# returns sparse vector with maximum values. Optionally returns a sparse matrix with the position of these maxima in the original matrix
# becomes very slow when number of entries in the table is larger than 1e5.

rowMax <- function(X, which = FALSE, ignore.zero = TRUE) {
	m <- aggregate(x~i, data = summary(X), FUN = max)
	maximum <- sparseVector(x = m$x, i = m$i, length = nrow(X))
	
	if(!ignore.zero) {
		maximum@x[maximum@x<0] <- 0
	}
	
	if (which) {
		d <- Diagonal(x = as(maximum,"vector"))
		W <- as(X,"nMatrix") * 1
		Xmax <- d %*% W
		W@x <- (X@x == Xmax@x) * 1
		W <- as(drop0(W), "nMatrix")
		return(list(max = maximum, which = W))
		} else {
			return(maximum)
	}
}

colMax <- function(X, which = FALSE, ignore.zero = TRUE) {
	tmp <- rowMax(t(X), which = which, ignore.zero = ignore.zero)
	if (which) {
		return(list(max = tmp$max, which = t(tmp$which)))
	} else {
		return(tmp)
	}
}

rowMin <- function(X, which = FALSE, ignore.zero = TRUE) {
	m <- aggregate(x~i, data = summary(X), FUN = min)
	minimum <- sparseVector(x = m$x, i = m$i, length = nrow(X))
	
	if(!ignore.zero) {
		minimum@x[minimum@x > 0] <- 0
	}
	
	if (which) {
		d <- Diagonal(x = as(minimum, "vector"))
		W <- as(X, "nMatrix") * 1
		Xmin <- d %*% W
		W@x <- (X@x == Xmin@x) * 1
		W <- as(drop0(W), "nMatrix")
		return(list(min = minimum, which = W))
		} else {
			return(minimum)
	}
}

colMin <- function(X, which = FALSE, ignore.zero = TRUE) {
	tmp <- rowMin(t(X), which = which, ignore.zero = ignore.zero)
	if (which) {
		return(list(min = tmp$min, which = t(tmp$which)))
	} else {
		return(tmp)
	}
}

# ============================================================
# various association measures between sparse matrices
# ============================================================

# Pearson correlation matrix between columns of X, Y
# http://stackoverflow.com/questions/5888287/running-cor-or-any-variant-over-a-sparse-matrix-in-r
#
# covmat uses E[(X-muX)'(Y-muY)] = E[X'Y] - muX'muY
# with sample correction n/(n-1) this leads to cov = ( X'Y - n*muX'muY ) / (n-1)
#
# the sd in the case Y!=NULL uses E[X-mu]^2 = E[X^2]-mu^2
# with sample correction n/(n-1) this leads to sd^2 = ( X^2 - n*mu^2 ) / (n-1)
#
# Note that results larger than 1e4 x 1e4 will become very slow, because the resulting matrix is not sparse anymore. 

cor.sparse <- function(X, Y = NULL, cov = FALSE) {

	X <- as(X,"dgCMatrix")
	n <- nrow(X)
	muX <- colMeans(X)
	
	if (!is.null(Y)) {
		stopifnot( nrow(X) == nrow(Y) )
		Y <- as(Y,"dgCMatrix")
		muY <- colMeans(Y)
		covmat <- ( as.matrix(crossprod(X,Y)) - n*tcrossprod(muX,muY) ) / (n-1)
		sdvecX <- sqrt( (colSums(X^2) - n*muX^2) / (n-1) )
		sdvecY <- sqrt( (colSums(Y^2) - n*muY^2) / (n-1) )
		cormat <- covmat/tcrossprod(sdvecX,sdvecY)
	} else {		
		covmat <- ( as.matrix(crossprod(X)) - n*tcrossprod(muX) ) / (n-1)
		sdvec <- sqrt(diag(covmat))
		cormat <- covmat/tcrossprod(sdvec)	
	}
	
	if (cov) {
		dimnames(covmat) <- NULL
		return(covmat)
	} else {
		dimnames(cormat) <- NULL
		return(cormat)
	}
}

# cosine similarity matrix between columns of X, Y
# results larger than 1e7 x 1e7 are just barely manageable on my laptop
# a random sparse 1e8 x 1e8 takes about 3 minutes, and 1.5 Gb Memory.
#
# different weightings of normalisations can be used	

	# allow for different weighting functions: can also be defined externally!
	# defined as a function of rowvalues (s) and number of columns (N)
	# no weighting (NULL) is taken as default
	idf <- function(s,N) { log(N/(1+s)) }
	# inverse square root
	isqrt <- function(s,N) { s^-0.5 }	
	# for use in later functions (e.g. sim.words)
	# weight = NULL leads to identical results, but is quicker
	none <- function(s,N) { s }
	
	# allow for different normalisation functions: can also be defined externally!
	# Euclidean 2-norm is taken as default
	norm2 <- function(x,s) { drop(crossprod(x^2,s)) ^ 0.5 }
	# Alternatively take 1-norm
	norm1 <- function(x,s) { drop(crossprod(abs(x),s)) }

cosSparse <- function(X, Y = NULL, norm = norm2 , weight = NULL) {

	X <- as(X,"dgCMatrix")
	if (!is.null(Y)) {
		stopifnot( nrow(X) == nrow(Y) )
		Y <- as(Y,"dgCMatrix")
	}

	if (!is.null(weight)) {	
		Nx <- ncol(X)
		Sx <- rowSums(abs(X))
		Wx <- Diagonal( x = match.fun(weight)(Sx,Nx) )
		X <- Wx %*% X
		if (!is.null(Y)) {
			Ny <- ncol(Y)
			Sy <- rowSums(abs(Y))
			Wy <- Diagonal( x = match.fun(weight)(Sy,Ny) )
			Y <- Wy %*% Y
		}
	}
			
	S <- rep(1,nrow(X))			
	N <- Diagonal( x = match.fun(norm)(X,S)^-1 )
	X <- X %*% N
	if (!is.null(Y)) {
		N <- Diagonal( x = match.fun(norm)(Y,S)^-1 )
		Y <- Y %*% N
		return(crossprod(X,Y))	
	} else {
		return(crossprod(X))
	}
}


cosMissing <- function(X, availX, Y = NULL, availY = NULL, norm = norm2 , weight = NULL) {

	X <- as(X,"dgCMatrix")
	if (!is.null(Y)) {
		stopifnot( nrow(X) == nrow(Y) )
		Y <- as(Y,"dgCMatrix")
	}

	if (!is.null(weight)) {	
		Nx <- ncol(X)
		Sx <- Nx * rowSums(abs(X)) / rowSums(availX)
		Wx <- Diagonal( x = match.fun(weight)(Sx,Nx) )
		X <- Wx %*% X
		if (!is.null(Y)) {
			Ny <- ncol(Y)
			Sy <- Ny * rowSums(abs(Y)) / rowSums(availY)
			Wy <- Diagonal( x = match.fun(weight)(Sy,Ny) )
			Y <- Wy %*% Y
		}
	}
	
	if (is.null(Y)) {
		N <- tcrossprod(match.fun(norm)(X,availX))
		R <- crossprod(X)
	} else {
		stopifnot(!is.null(availY))
		N <- tcrossprod(match.fun(norm)(X,availY),match.fun(norm)(Y,availX))
		R <- crossprod(X,Y)
	}
	N <- N*(as(R,"nMatrix")*1)
	R@x <- R@x/N@x
	return(R)
}

# rowGroup is a sparse matrix with same number of rows as X, or a vector of length nrow(X) specifying the grouping of the rows

cosRow <- function(X, rowGroup, Y = NULL, norm = norm2 , weight = NULL) {

	if (is.vector(rowGroup)) { 
		rowGroup <- t(ttMatrix(rowGroup)$M*1) 
	} else {
		rowGroup <- as(rowGroup,"dgCMatrix")
	}

	X <- as(X,"dgCMatrix")
	if (!is.null(Y)) {
		stopifnot( nrow(X) == nrow(Y) )
		Y <- as(Y,"dgCMatrix")
	}

	if (!is.null(weight)) {	
		N <- ncol(X)
		s <- rowSums(X)
		S <- N * s / ( drop(s %*% tcrossprod(rowGroup)) )
		W <- Diagonal( x = match.fun(weight)(S,N) )
		X <- W %*% X
		if (!is.null(Y)) {
			N <- ncol(Y)
			s <- rowSums(Y)
			S <- N * s / ( drop(s %*% tcrossprod(rowGroup)) )
			W <- Diagonal( x = match.fun(weight)(S,N) )
			Y <- W %*% Y
		}
	}
	
	if (is.null(Y)) {
		N <- tcrossprod(match.fun(norm)(X,rowGroup))
		R <- crossprod(X)
	} else {
		N <- tcrossprod(match.fun(norm)(X,rowGroup),match.fun(norm)(Y,rowGroup))
		R <- crossprod(X,Y)
	}
	N <- N*(as(R,"nMatrix")*1)
	R@x <- R@x/N@x
	return(R)
}

# colGroupX is either a sparse Matrix with the same number of columns as X, rows represent grouping. Or a grouping vector of length ncol(X), specifying group indices.

# weight does not really seem to make sense here: complete data each row should have an equal amount of entries! Any row-weighting would only measure the amount of missing data.

cosCol <- function(X, colGroupX, Y = NULL, colGroupY = NULL, norm = norm2 ) {
	
	X <- as(X,"dgCMatrix")
	if (is.vector(colGroupX)) {
		colGroupX <- ttMatrix(colGroupX)$M * 1 
	} else {
		colGroupX <- as(colGroupX,"dgCMatrix")
	}

	S <- rep(1,nrow(X))
	Freq <- tcrossprod(X,colGroupX)
	N <- Diagonal( x = drop(crossprod(colGroupX, match.fun(norm)(Freq,S)))^-1 )
	X <- X %*% N
	
	if (!is.null(Y)) {
		stopifnot( nrow(X) == nrow(Y) )
		stopifnot(!is.null(colGroupY))
		Y <- as(Y,"dgCMatrix")
		if (is.vector(colGroupY)) { 
			colGroupY <- ttMatrix(colGroupY)$M * 1 
		} else {
			colGroupX <- as(colGroupX,"dgCMatrix")
		}	

		Freq <- tcrossprod(Y,colGroupY)
		N <- Diagonal( x = drop(crossprod(colGroupY, match.fun(norm)(Freq,S)))^-1 )
		Y <- Y %*% N

		return(crossprod(X,Y))
		
	} else {
		return(crossprod(X))
	}	
}

# association matrix between columns of X, Y

	# allow for different functions to be specified: can be defined externally!
	# defaults to poisson
	poi <- function(o,e) { sign(o-e) * (o * log(o/e) - (o-e)) }
	# pointwise mutual information, aka "log-odds" in bioinformatics
	pmi <- function(o,e) { log(o/e) }
	# weighted pointwise mutual information, i.e the basis for MI
	wpmi <- function(o,e) { o * log(o/e)}
	# good old pearson residuals
	res <- function(o,e) { (o-e) / sqrt(e) }

# WATCH OUT! it might not always be the right decision to ignore the cases in which O=zero!!! e.g. residuals should not be zero then (but negative!!!).

assocSparse <- function(X, Y = NULL, method = res, N = nrow(X), sparse = TRUE) {

	X <- as(X,"ngCMatrix")*1
	Fx <- colSums(X)

	# observed coocurrences "O"
	if (is.null(Y)) {
		O <- crossprod(X)
		} else {
		stopifnot( nrow(X) == nrow(Y) )
		Y <- as(Y,"ngCMatrix")*1
		Fy <- colSums(Y)
		O <- crossprod(X,Y)
	}

	# The trick to keep things sparse here is to not compute anything when O is zero. In most practical situations this seems to be fine, but note that in cases in which the expectation is high, though observed is zero this might lead to large discrepancies.
	if (sparse) {	
		R <- as(O,"nMatrix")/N
		Fx <- Diagonal( x = Fx )
		if (is.null(Y)) {
			E <- Fx %*% R %*% Fx
			E <- as(E,"symmetricMatrix")
		} else {
			Fy <- Diagonal( x = Fy )
			E <- Fx %*% R %*% Fy
		}	
		R@x <- match.fun(method)(O@x,E@x)

	# this is the easy non-sparse method. Note that the result will be non-sparse, so this is not feasable for very large datasets
	} else {
		if (is.null(Y)) {
			E <- tcrossprod(Fx)/N
		} else {
			E <- tcrossprod(Fx,Fy)/N
		}
		R <- match.fun(method)(O,E)
	}
	return(R)
}

# still TO DO: assoc.missing

# The following is a version of assoc for cases in which the columns form groups. Typically found in case of a large collection of nominal variables (e.g. WALS) with an index matrix. If you want to establish the association between the variables, you'd need this version to get the expectation right.

# colGroupX is either a sparse Matrix with the same number of columns as X, rows represent grouping. Or a grouping vector of length ncol(X), specifying group indices.

assocCol <- function(X, colGroupX, Y = NULL, colGroupY = NULL, method = res, sparse = TRUE) {

	X <- as(X,"dgCMatrix")
	
	if (is.vector(colGroupX)) { 
		colGroupX <- ttMatrix(colGroupX)$M*1 
	} else {
		colGroupX <- as(colGroupX,"dgCMatrix")	
	}	
	Gx <- crossprod(colGroupX)
	
	# Fx: Frequencies of columns over all data, not just matched cases!
	# divided by 'N', occurrences of Fx, leading to probability of columns
	# will lead to slightly different values from traditional chisquare
	Fx <- colSums(abs(X))
	Fx <- Diagonal( x = Fx / drop(Gx %*% Fx) )

	if (is.null(Y)) {
		O <- crossprod(X)

		} else {
		stopifnot( nrow(X) == nrow(Y) )
		stopifnot(!is.null(colGroupY))

		Y <- as(Y,"dgCMatrix")
		
		if (is.vector(colGroupY)) { 
			colGroupY <- ttMatrix(colGroupY)$M*1
		} else {
			colGroupY <- as(colGroupY,"dgCMatrix")
		}		
		Gy <- crossprod(colGroupY)

		Fy <- colSums(abs(Y))
		Fy <- Diagonal( x = Fy / drop(Gy %*% Fy) )
		
		O <- crossprod(X, Y)
	}

	# in a sparse way: tricky to do, but possible. Ignore items where Observed is zero. Not that this is an approximation that might lead to somewhat unexpected results in specific situations.
	if (sparse) {	

		# Number of cases per block. To keep this sparse needs some tricks (unfold-refold)	
		if (is.null(Y)) {			
			N <- unfold(O, colGroupX)	
		} else {			
			N <- unfold(O, colGroupY)
		}
		G <- crossprod(kronecker( Diagonal(nrow(colGroupX)), colGroupX ))
		sums <- G %*% rowSums(N$U)
		S <- Diagonal( x = drop(sums) )
		N <- N$L %*% S %*% (as(N$U,"nMatrix")*1) # refold with 'weights' S

		# Expectation
		if (is.null(Y)) {
			E <- forceSymmetric(Fx %*% N %*% Fx)
		} else {
			E <- Fx %*% N %*% Fy
		}
		
		result <- O
		result@x <- match.fun(method)(O@x,E@x)	

	# in a non-sparse way: this is much more elegant! But the result is not sparse, so this is not feasable for very large datasets.
	} else {
		if (is.null(Y)) {
			E <- crossprod(X %*% Gx %*% Fx)
		} else {
			E <- crossprod(X %*% Gx %*% Fx, X %*% Gy %*% Fy)
		}
		result <- match.fun(method)(O,E)
	}
	return(result)
}

# same as above here, but for groups of rows. E.g. with WALS when looking at the similarity between languages, given the expectation of groups (features) as 1/nr.of.values 

# rowGroup is a sparse matrix with same number of rows as X, or a vector of length nrow(X) specifying the grouping of the rows

assocRow <- function(X, rowGroup, Y = NULL, method = res) {

	X <- as(X,"dgCMatrix")
	
	if (is.vector(rowGroup)) {
		rowGroup <- t(ttMatrix(rowGroup)$M*1)
	} else {
		rowGroup <- as(rowGroup,"dgCMatrix")
	}
	W <- Diagonal( x = rowSums(tcrossprod(rowGroup))^-0.5 )

	if (is.null(Y)) {
		O <- crossprod(X) 	
		E <- crossprod(t(rowGroup) %*% W %*% X)
	} else {
		stopifnot( nrow(X) == nrow(Y) )
		Y <- as(Y,"dgCMatrix")
		O <- crossprod(X,Y)
		E <- crossprod(t(rowGroup) %*% W %*% X, t(rowGroup) %*% W %*% Y)
	}

	E <- E * (as(O,"nMatrix")*1) # this is necessary for cases in which O is zero
	O@x <- match.fun(method)(O@x,E@x)
	return(O)
}

# ==================================================
# some shortcuts for computing similarities directly
# ==================================================

# similarities between nominal attributes, i.e nominal variables
# this code could use some clean-up and harmonization :-)

sim.att <- function(D, method = "chuprov", sparse = TRUE) {
	
	X <- splitTable(D)
	
	# Chuprov's T, almost the same as Cramér's V, but easier to implement
	if (!is.na(pmatch(method,"chuprov"))) {

		r <- assocCol(X$OV, X$AV, method = res, sparse = sparse)
		if (!sparse) {
			r@x[is.na(r@x)] <- 0 # residuals can be NA when E==zero
		}

		X2 <- (X$AV*1) %*% r^2 %*% t(X$AV*1)
		N <- crossprod(tcrossprod(X$OV*1,X$AV*1))
		D <- Diagonal( x = sqrt(rowSums(X$AV) - 1) )
		R <- D %*% (as(N,"nMatrix")*1) %*% D

		if (sparse) {
			X2 <- as( X2 , "symmetricMatrix" )
			R <- as( R , "symmetricMatrix" )		
		} else {
			X2 <- as(as( X2, "dgCMatrix"), "symmetricMatrix" )
			R <- as(as( R, "dgCMatrix"), "symmetricMatrix" )
		}
		
		result <- N
		result@x <- sqrt( X2@x/(N@x * R@x) )
	}

	# G-test from Sokal and Rohlf (1981), also known as 'Dunning's G'
	# related to Mutual Information by a factor N
	if (!is.na(pmatch(method,"g-test"))) {
		r <- assocCol(X$OV, X$AV, method = wpmi, sparse = sparse)
		g <- (X$AV*1) %*% r %*% t(X$AV*1)
		g <- as(as( g, "dgCMatrix"), "symmetricMatrix" )
		g@x <- 2*g@x
		result <- g
	}
	
	# Mutual Information
	if (!is.na(pmatch(method,"mutual information"))) {
		r <- assocCol(X$OV, X$AV, method = wpmi)
		g <- (X$AV*1) %*% r %*% t(X$AV*1)
		g <- as(as( g, "dgCMatrix"), "symmetricMatrix" )
		N <- crossprod(tcrossprod(X$OV*1,X$AV*1))
		if ( length(g@x) != length(N@x) ) {
			N <- N * (as(g,"nMatrix")*1)
		}
		g@x <- g@x/N@x
		result <- g
	}
	
	# Variation of Information = Mutual information metric
	if (!is.na(pmatch(method,"variation of information"))) {
		r <- assocCol(X$OV, X$AV, method = wpmi)
		g <- (X$AV*1) %*% r %*% t(X$AV*1)
		g <- as(as( g, "dgCMatrix"), "symmetricMatrix" )
		N <- crossprod(tcrossprod(X$OV*1,X$AV*1))
		if ( length(g@x) != length(N@x) ) {
			N <- N * (as(g,"nMatrix")*1)
		}
		g@x <- g@x/N@x

		O <- crossprod(X$OV*1)
		H1 <- (X$AV*1) %*% O %*% t(X$AV*1)
		H1 <- as(H1, "symmetricMatrix")
		H1@x <- H1@x * log(N@x) / N@x
		
		O2 <- O
		O2@x <- O@x * log(O@x)

		H2 <- (X$AV*1) %*% O2 %*% t(X$AV*1)
		H2 <- as(H2, "symmetricMatrix")
		H2@x <- H2@x / N@x
		
		H <- g
		H@x <- (H1@x - H2@x - g@x)
		result <- H
	}
	
	rownames(result) <- X$attributes
	return(result)

}

# similarities between observations from nominal data
# this is a very simple wrapper around cosRow and assocRow

sim.obs <- function(D, method = "hamming", sparse = TRUE) {
	
	X <- splitTable(D)
	
	# Relative Hamming similarity (Goebl's "Relativer Identitätswert"), i.e. the number of similarities divided by the number of comparisons made
	if (!is.na(pmatch(method,"hamming"))) {
		result <- cosRow(t(X$OV), t(X$AV), norm = norm1)

	# weighted similarity very similar to Goebl's "Gewichteter Identitätswert". Note that his definition is slightly different, but that one is tricky to replicate
	} else	if (!is.na(pmatch(method,"weighted"))) {
		result <- cosRow(t(X$OV), t(X$AV), norm = norm2, weight = isqrt)

	#assoc methods
	} else {
		result <- assocRow(t(X$OV), t(X$AV), method = method)
	}

	rownames(result) <- X$observations
	return(result)
	
}

# similarity for words in parallel text. If weight is specified, method is ignored: cosSparse is used with norm2 and specified weight
# best uses rowMax/colMax, which is not very quick

sim.words <- function(text1, text2 = NULL, method = res, weight = NULL, 
						lowercase = TRUE, best = FALSE, tol = 0) {

	if (is.null(text2)) {
		T1 <- splitText( text1, simplify = TRUE, lowercase = lowercase )
		# compute co-occurrence statistics
		if (!is.null(weight)) {
			R <- cosSparse( t(T1), weight =  weight )
		} else {
			R <- assocSparse( t(T1), method =  method )
		}
	} else {
		globalID <- union(names(text1), names(text2))

		T1 <- splitText( text1, globalID, simplify = TRUE, lowercase = lowercase )
		T2 <- splitText( text2, globalID, simplify = TRUE, lowercase = lowercase )

		# collapse verses in which one of the translation is empty (i.e. combined translation of multiple verses into one verse)
		m1 <- which(text1 == "")
		m2 <- which(text2 == "")
		m <- union(m1, m2)
		
		M <- Diagonal(n = length(globalID))
		while ( sum(M[,m]) > 0 ) {
			tmp <- M[,m]
			M[,m] <- 0
			M[,(m-1)] <- M[,(m-1)] + tmp	
		}
		M <- M[,colSums(M)>0]
		
		# remap to collapse verses
		T1 <- T1 %*% M
		T2 <- T2 %*% M
	
		# compute co-occurrence statistics
		if (!is.null(weight)) {
			R <- cosSparse( t(T1), t(T2), weight =  weight )
		} else {
			R <- assocSparse( t(T1), t(T2), method =  method )
		}
	}
	
	if (tol > 0) {
		R <- drop0(R, tol = tol)
	}
	
	if (best) {
		choice <-	colMax(R, which = TRUE, ignore.zero = FALSE)$which +
					rowMax(R, which = TRUE, ignore.zero = FALSE)$which
		choice <- as(choice, "nMatrix")
		return(list(sim = R, best = choice))
	} else {
		return(R)
	}	
}

# quick string comparison based on cosine similarity between bigrams

sim.strings <- function(strings1, strings2 = NULL, sep = "", boundary = TRUE) {

	S1 <- splitStrings(strings1, sep = sep, boundary = boundary, simplify = TRUE)

	if (is.null(strings2)) {
		sim <- cosSparse(S1)
	} else {
		S2 <- splitStrings(strings2, sep = sep, boundary = boundary, simplify = TRUE)
		M <- jMatrix( rownames(S1), rownames(S2) )
		sim <- cosSparse( (M$M1*1) %*% S1, (M$M2*1) %*% S2 )
	}
	return(drop(sim))
}

# various similarities for wordlists
# sim.con: similarity between concepts, based on colexification, or bigram similarity
# sim.lang: similarity between languages, based on average bigram similarity
# sim.graph: similarity between graphemes, based on cooccurrences in context

sim.graph <- function(
		wordlist,
		doculects = "DOCULECT", concepts = "CONCEPT", counterparts = "TOKENS",
		method = "cooccurrence", assoc.method = poi, weight = NULL, sep = " "
		) {	
			
	W <- splitWordlist(
		wordlist, doculects =  doculects, concepts = concepts, counterparts = counterparts, sep = sep
		)
	CG <- (W$CW*1) %*% t(W$SW*1) %*% t(W$GS*1)		
	if (!is.null(weight)) {
			sim <- cosSparse( CG, weight =  weight )
		} else {
			sim <- assocSparse( CG, method = assoc.method )
		}
	rownames(sim) <- W$graphemes

	# additional matrix to identify the graphemes per language
	# without needing to parse the rownames...
	GD <- W$GS %*% W$SW %*% t(W$DW)
	colnames(GD) <- W$doculects
	rownames(GD) <- W$graphemes

	return(list(GG = sim, GD = GD))
}

# similarity between languages

sim.lang <- function(
		wordlist, 
		doculects = "DOCULECT", concepts = "CONCEPT", counterparts = "COUNTERPART",
		method = "parallel", assoc.method =  res, weight = NULL, sep = ""
		) {	
			
	W <- splitWordlist(
		wordlist, doculects = doculects, concepts = concepts, counterparts = counterparts, sep = sep
		)	

	if (!is.na(pmatch(method,"global"))) {
		BD <- (W$BS*1) %*% (W$SW*1) %*% t(W$DW*1)
		if (!is.null(weight)) {
			sim <- cosSparse( BD, weight =  weight )
		} else {
			sim <- assocSparse( BD, method = assoc.method )
		}
	}	
	if (!is.na(pmatch(method,"parallel"))) {
		BW <- (W$BS*1) %*% (W$SW*1)
		CBxW <- KhatriRao(BW, (W$CW*1))
		CBxD <- CBxW %*% t(W$DW*1)
		if (!is.null(weight)) {
			sim <- cosSparse( CBxD, weight =  weight )
		} else {
			sim <- assocSparse( CBxD, method = assoc.method )
		}
	} 	
	colnames(sim) <- rownames(sim) <- W$doculects
	return(sim)
}

# Similarity between concepts

sim.con <- function(
		wordlist,
		doculects = "DOCULECT", concepts = "CONCEPT", counterparts = "COUNTERPART",
		method = "bigrams", assoc.method = res, weight = NULL, sep = ""
		) {
	if (!is.na(pmatch(method,"colexification"))) {
		W <- splitWordlist(
			wordlist, doculects = doculects, concepts = concepts, counterparts = counterparts, 
			splitstrings = FALSE, simplify = TRUE
			)
		sim <- tcrossprod(W$CW*1)
	}
	if (!is.na(pmatch(method,"global"))) {
		W <- splitWordlist(
			wordlist, doculects = doculects, concepts = concepts, counterparts = counterparts, 
			sep = sep
			)
		BC <- (W$BS*1) %*% (W$SW*1) %*% t(W$CW*1)
		if (!is.null(weight)) {
			sim <- cosSparse( BC, weight =  weight )
		} else {
			sim <- assocSparse( BC, method = assoc.method )
		}		
	}	
	if (!is.na(pmatch(method,"bigrams"))) {
		W <- splitWordlist(
			wordlist, doculects = doculects, concepts = concepts, counterparts = counterparts, 
			sep = sep
			)
		TC <- (W$TS*1) %*% (W$SW*1) %*% t(W$CW*1)
		if (!is.null(weight)) {
			sim <- cosSparse( TC, weight =  weight )
		} else {
			sim <- assocSparse( TC, method = assoc.method )
		}		
	}		
	colnames(sim) <- rownames(sim) <- W$concepts
	return(sim)		
}


# =======================================================================
# High-level functions to convert data into collection of sparse matrices
# =======================================================================

# split data table with nominal data into matrices
# consider for future to do this via long-form ("reshape")

splitTable <- function(	data,
						attributes = colnames(data),
						observations = rownames(data),
						name.binder = ":"
						) {

	# assuming a data table with
	#	- variables ("A"ttributes) as columns and 
	#	- observations (O) as rows
	#	- values (V) in the cells (different for each column)
	
	cls <- ncol(data)

	# basic rewrite, the rest is cosmetic
	tt <- apply(data,2,ttMatrix)
	OV <- tt[[1]]$M
	for (i in tt[-1]) {
		OV <- rBind(OV,i$M)
	}
	OV <- t(OV)
	
	# the following approach is slow... strange...
	# http://www.r-bloggers.com/the-rbinding-race-for-vs-do-call-vs-rbind-fill/
	# OV <- t(do.call(rBind,sapply(tt,function(x){x[[1]]})))

	# index matrix of variables to values
	nrValues <- sapply(tt,function(x){length(x$rownames)})
	AV <- ttMatrix(rep.int(1:cls,nrValues))$M
	
	# we need some variable names to make value names, but entity names might be NULL

	if (is.null(attributes)) {
		attributes <- paste("X", 1:cls, sep="")
	}
	
	# make value names
	values <- unlist(sapply(1:cls, function(x) {
						paste(attributes[x],tt[[x]]$rownames,sep=name.binder)
							}, simplify = FALSE
						)
					)
	
	return(list(	attributes = attributes,
					values = values,
					observations = observations,
						
					OV = OV,	# Observations x Values
					AV = AV		# Variables ("Attributes") x Values
					))
}

# make unigram and bigram matrices from a vector of strings

splitStrings <- function(	strings,
							sep = "",
							bigrams = TRUE,
							boundary = TRUE,
							bigram.binder = "",
							gap.symbol = "\u00B7",
							left.boundary = "#",
							right.boundary = "#",
							simplify = FALSE
							) {
	if (bigrams) {
		gap.length <- 1
		originals <- strings
		if (boundary) {
			strings <- paste(left.boundary, strings, right.boundary, sep = sep)
		} 
	} else {
		gap.length <- 0
	}

	# SW: Segments x Strings ("Words")
	tmp <- pwMatrix(strings, sep=sep, gap.length=gap.length, gap.symbol=gap.symbol)
	SW <- tmp$M
	segments <- tmp$rownames

	# US: unigrams x segments
	tmp <- ttMatrix(segments)
	US <- tmp$M
	unigrams <- tmp$rownames

	# Bisymbols x Segments
	if (bigrams) {
		
		# remove gap character from US and symbols
		if (length(strings) > 1) {
			gap.char <- which(unigrams == gap.symbol)
			unigrams <- unigrams[-gap.char]
			US <- US[-gap.char,]
		}
		
		S <- bandSparse(n = dim(US)[2], k = -1)
		tmp <- rKhatriRao(US %*% S, US, unigrams, unigrams, binder = bigram.binder)
		BS <- tmp$M
		bisymbols <- tmp$rownames

		# remove boundary from US and symbols
		if (boundary) {
			boundary.char <- which(unigrams == left.boundary | unigrams == right.boundary)
			unigrams <- unigrams[-boundary.char]
			US <- US[-boundary.char,]
		}
		
	# various forms of output
		if (simplify) {
			result <- (BS*1) %*% (SW*1)
			rownames(result) <- bisymbols
			colnames(result) <- originals
			return(result)
		} else {
			return(list(	segments = segments,
		 					unigrams = unigrams,
							bigrams = bisymbols,
							SW = SW, # Segments x Words
							US = US, # Unigrams x Segments
							BS = BS # Bigrams x Segments
							))
		}
	} else {
		if (simplify) {
			result <- (US*1) %*% (SW*1)
			rownames(result) <- unigrams
			colnames(result) <- strings
			return(result)
		}
		return(list(	segments = segments,
						unigrams = unigrams,
						SW = SW, # Segments x Words
						US = US # Unigrams x Segments
						))
	}
}

# =========================================================
# convenience function specifically made for parallel texts
# =========================================================

# Read texts from the parallel-text project
# the long version around "scan" is not really quicker as the shortcut using read.table

read.text <- function(file) {

#	data <- scan(file,sep="\t",comment.char="#",what="character",quote="")
#	dim(data) <- c(2,length(data)/2)
#	result <- data[2,,drop = TRUE]
#	names(result) <- data[1,,drop = TRUE]
#	return(result)

	drop(as.matrix(read.table(	file
								, sep = "\t"
								, quote = ""
								, colClasses = "character"
								, row.names = 1
								, encoding = "UTF-8"
								)))

}

# make matrices from parallel texts (bible and the like)
# takes three arguments: 
# - the text (as vector of strings)
# - the IDs for the sentences as found in this text
# - the IDs for the sentences as found in all texts (important for the parallelism)

splitText <- function(	text,
						globalSentenceID = NULL,
						localSentenceID = names(text),
						sep = " ",
						simplify = FALSE,
						lowercase = TRUE
						) {

	# make RunningWords x Verses, i.e. all words of the text in the order as they appear as rows, linked to the localSentenceIDs as columns.	
	tmp <- pwMatrix(text, sep = sep, gap.length = 0)
	RS <- tmp$M
	runningWords <- tmp$rownames

	# make type/token matrix linking the different wordforms to the individual words
	# Wordforms x RunningWords
	tmp <- ttMatrix(runningWords)
	WR <- tmp$M
	wordforms <- tmp$rownames
	
	# link the localSentenceIDs "S" to the globalSentenceIDs "U"
	if (!is.null(globalSentenceID)) {
		tmp <- jMatrix(localSentenceID, globalSentenceID)
		US <- tmp$M1
		# relink
		RS <- RS %*% t(US)
	}
	
	# remove upper/lowercase distinction for better statistics
	if (lowercase) {
		tmp <- ttMatrix(tolower(wordforms))
		wW <- tmp$M
		lower <- tmp$rownames
	}
	
	# various versions of output

	if (!lowercase) {
		if (simplify) {
			R <- (WR*1) %*% (RS*1)
			rownames(R) <- wordforms
			return(R)
		} else {
			return(
				list(	runningWords = runningWords,
						wordforms = wordforms,	
						
						RS = RS,	# Running words x global sentence ID ("Sentence")
						WR = WR		# Wordforms x Running words
						))
		}
	} else {
		if (simplify) {
			R <- (wW*1) %*% (WR*1) %*% (RS*1)
			rownames(R) <- lower
			return(R)
		} else {
			return(
				list(	runningWords = runningWords,
						wordforms = wordforms,	
						lowercase = lower,
						
						RS = RS,	# Running words x global sentence ID ("Sentence")
						WR = WR,	# Wordforms x Running words
						wW = wW		# lowercased wordforms x uppercase Wordforms
						))
		}
	}
}


# ========================================================
# convenience function specifically made for QLC-wordlists
# ========================================================

splitWordlist <- function(	data, 
							doculects = "DOCULECT", 
							concepts = "CONCEPT",
							counterparts = "COUNTERPART",
							splitstrings = TRUE,
							sep =  "",
							bigram.binder = "",
							grapheme.binder = "_",
							simplify = FALSE
							) {

	# just a placeholder, assuming this one does not occur in the data
	binder <- "\u2295"

	# make doculect+counterpart combinations
	# this is needed because sometimes the same string is found in different languages
	combined <- paste( data[,doculects], data[,counterparts], sep = binder)	

	# DL: Doculects x Lines of data
	tmp <- ttMatrix(data[,doculects])
	DL <- tmp$M
	doculects <- tmp$rownames

	# CL: Concepts x Lines of data
	tmp <- ttMatrix(data[,concepts])
	CL <- tmp$M
	concepts <- tmp$rownames
	
	# WL: Counterparts ("Words") x Lines of data
	tmp <- ttMatrix(combined)
	WL <- tmp$M
	words <- tmp$rownames
	# split counterparts again from doculects
	words <- sapply(strsplit(words,binder),head)[2,]

	# relink	
	DW <- DL %*% t(WL)
	CW <- CL %*% t(WL)
	
	if (splitstrings) {
	
		# split strings
		S <- splitStrings(words, sep = sep, bigram.binder = bigram.binder)
	
		# return results
		if (simplify) {
			
			BW <- (S$BS*1) %*% (S$SW*1)
			
			# only use column names once because of size
			rownames(DW) <- doculects
			rownames(CW) <- concepts
			rownames(BW) <- S$bigrams
			colnames(BW) <- words
					
			return(list(DW = DW, CW = CW, BW = BW))
			
		} else {
			# separate characters to languages
			# and prepare full output (rather long!)
			
			# link to segments to doculects
			DS <- DW %*% t(S$SW)
	
			# Graphemes x Segments
			tmp <- rKhatriRao(	DS, S$US, 
								doculects, S$unigrams, 
								binder = grapheme.binder
								)
			GS <- tmp$M
			graphemes <- tmp$rownames
	
			# another KhatriRao to turn bisymbols into 
			# language-specific Bigraphs x Segments
			tmp <- rKhatriRao(	DS, S$BS, 
								doculects, S$bigrams, 
								binder = grapheme.binder
								)
			TS <- tmp$M
			digraphs <- tmp$rownames
	
			return(list(	doculects = doculects,
							concepts = concepts,
							words = words,
						
							segments = S$segments,
							unigrams = S$unigrams,
							bigrams = S$bigrams, 
							graphemes = graphemes,
							digraphs = digraphs,
	
							DW = DW, # Doculects x Words
							CW = CW, # Concepts x Words

							SW = S$SW,	# Segments x Words
							US = S$US,	# Unigrams x Segments
							BS = S$BS,	# Bigrams x Segments
							GS = GS, 	# Graphemes x Segments
							TS = TS 	# Digraphs x Segments	
							))
		}
	} else {
		# without splitString
		# much quicker, but only returns the basic structure
		if (simplify) {
			
			rownames(DW) <- doculects
			rownames(CW) <- concepts
			colnames(CW) <- words
					
			return(list(DW = DW, CW = CW))
		} else {
			return(list(	doculects = doculects,
							concepts = concepts,
							words = words,
	
							DW = DW, # Doculects x Words
							CW = CW  # Concepts x Words
							))
		}
	}
}