# ==================================================
# some shortcuts for computing similarities directly
# ==================================================

# similarities between nominal attributes, i.e nominal variables
# this code could use some clean-up and harmonization :-)

sim.att <- function(D, method = "chuprov", sparse = TRUE, ...) {
	
	X <- splitTable(D, ...)
	
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
			X2 <- as(as( X2, "dMatrix"), "symmetricMatrix" )
			R <- as(as( R, "dMatrix"), "symmetricMatrix" )
		}
		
		result <- N # just to get the right sparsity structure
		result@x <- sqrt( X2@x/(N@x * R@x) )
	}

	# The following options are highly similar, using the same base functions
	get_wpmi_assoc <- function(X) {
		r <- assocCol(X$OV, X$AV, method = wpmi)
		g <- (X$AV*1) %*% r %*% t(X$AV*1)
		g <- as(as( g, "dMatrix"), "symmetricMatrix" )
		return(g)
	}
	get_N <- function(X,g) {
		N <- crossprod(tcrossprod(X$OV*1,X$AV*1))
		if ( length(g@x) != length(N@x) ) {
			N <- N * (as(g,"nMatrix")*1)
		}
		return(N)
	}

	# G-test from Sokal and Rohlf (1981), also known as 'Dunning's G'
	# related to Mutual Information by a factor N
	if (!is.na(pmatch(method,"g-test"))) {
		g <- get_wpmi_assoc(X)		
		g@x <- 2*g@x
		result <- g
	}
	
	# Mutual Information
	if (!is.na(pmatch(method,"mutual information"))) {
		g <- get_wpmi_assoc(X)
		N <- get_N(X,g)
		g@x <- g@x/N@x
		result <- g
	}
	
	# Variation of Information = Mutual information metric
	if (!is.na(pmatch(method,"variation of information"))) {
		g <- get_wpmi_assoc(X)
		N <- get_N(X,g)
		g@x <- g@x/N@x

		O <- crossprod(X$OV*1)
		H1 <- (X$AV*1) %*% O %*% t(X$AV*1)
		H1 <- as(H1, "symmetricMatrix")
		H1@x <- H1@x * log(N@x) / N@x
		
		O@x <- O@x * log(O@x)
		H2 <- (X$AV*1) %*% O %*% t(X$AV*1)
		H2 <- as(H2, "symmetricMatrix")
		H2@x <- H2@x / N@x
		
		H <- g # just to get the right sparsity structure
		H@x <- (H1@x - H2@x - g@x)
		result <- H
	}
	
	rownames(result) <- X$attributes
	return(result)

}

# similarities between observations from nominal data
# this is a very simple wrapper around cosRow and assocRow

sim.obs <- function(D, method = "hamming", sparse = TRUE, ...) {
	
	X <- splitTable(D, ...)
	
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

# quick string comparison based on cosine similarity between ngrams

sim.strings <- function(strings1, strings2 = NULL, sep = "", ngrams = 2, assoc.method = res, weight = NULL, boundary = TRUE, ...) {

	S1 <- splitStrings(strings1, sep = sep, n = ngrams, boundary = boundary, simplify = TRUE, ...)

	if (is.null(strings2)) {
	  if (is.null(weight)) {
	    sim <- assocSparse( S1, method = assoc.method )
	  } else {
	    sim <- cosSparse( S1 , weight = weight )
	  }
	} else {
		S2 <- splitStrings(strings2, sep = sep, n = ngrams, boundary = boundary, simplify = TRUE, ...)
		M <- jMatrix( rownames(S1), rownames(S2) )
		if (is.null(weight)) {
		  sim <- assocSparse( M$M1 %*% S1, M$M2 %*% S2, method = assoc.method )
		} else {
		  sim <- cosSparse( M$M1 %*% S1, M$M2 %*% S2, weight = weight )
		}
	}
	
	return(drop(sim))
}

# various similarities for wordlists
# sim.graph: similarity between graphemes, based on cooccurrences in context

sim.graph <- function(
		wordlist,
		doculects = "DOCULECT", concepts = "CONCEPT", counterparts = "TOKENS",
		method = "cooccurrence", assoc.method = poi, weight = NULL, sep = " "
		) {	
			
	W <- splitWordlist(wordlist, 
	      doculects = doculects, concepts = concepts, counterparts = counterparts, 
	      ngrams = 1, sep = sep
		)
	CG <- (W$CW) %*% t(W$SW) %*% t(W$GS)		
	if (!is.null(weight)) {
			sim <- cosSparse( CG, weight =  weight )
		} else {
			sim <- assocSparse( CG, method = assoc.method )
		}
	rownames(sim) <- W$ngraphs

	# additional matrix to identify the graphemes per language
	# without needing to parse the rownames...
	GD <- W$GS %&% W$SW %&% t(W$DW)
	colnames(GD) <- W$doculects
	rownames(GD) <- W$ngraphs

	return(list(GG = sim, GD = GD))
}

# sim.lang: similarity between languages

sim.lang <- function(wordlist, 
		doculects = "DOCULECT", concepts = "CONCEPT", counterparts = "COUNTERPART",
		method = "parallel", assoc.method = res, weight = NULL, ngrams = 2, sep = ""
		) {	
  
  if (!is.na(pmatch(method, "parallel"))) {

	  W <- splitWordlist(wordlist, 
	    doculects = doculects, concepts = concepts, counterparts = counterparts, 
	    ngrams = ngrams, sep = sep
	  )
		NW <- (W$NS) %*% (W$SW)
		CNxW <- KhatriRao(NW, (W$CW*1))
		ND <- CNxW %*% t(W$DW)
		names <- W$doculects
	}
  
  if (!is.na(pmatch(method, "global"))) {
    
    W <- splitWordlist(wordlist,
      doculects = doculects, concepts = concepts, counterparts = counterparts, 
      ngrams = ngrams, sep = sep, simplify = T
    )
    ND <- W$NW %*% t(W$DW)
    names <- colnames(ND)
    
    # paste all counterparts together
    # note: ngrams >= 5 leads to ngram-overlap because of collapse setting
  #  combined <- stats::aggregate(get(counterparts) ~ get(doculects), data = wordlist, 
  #                               paste, collapse = "# #")
  #  colnames(combined) <- c("doculects", "counterparts")
    # order by ttMatrix locale
  #  collation <- order(ttMatrix(combined$doculects)$rownames)
  #  combined <- combined[collation,]
    
  #  W <- splitStrings(combined$counterparts, n = ngrams, sep = sep)
  #  ND <- W$NS %*% W$SW
  #  names <- combined$doculects
    

  }
  
  if (is.null(weight)) {
    sim <- assocSparse( ND, method = assoc.method )
  } else {
    sim <- cosSparse( ND, weight = weight )
  }
	colnames(sim) <- rownames(sim) <- names
	return(sim)
}

# sim.con: Similarity between concepts

sim.con <- function(
		wordlist,
		doculects = "DOCULECT", concepts = "CONCEPT", counterparts = "COUNTERPART",
		method = "bigrams", assoc.method = res, weight = NULL, sep = ""
		) {
	if (!is.na(pmatch(method,"colexification"))) {
		W <- splitWordlist(wordlist, 
		  doculects = doculects, concepts = concepts, counterparts = counterparts, 
			ngrams = NULL, simplify = FALSE
			)
		sim <- tcrossprod(W$CW*1)
	}
	if (!is.na(pmatch(method,"global"))) {
		W <- splitWordlist(wordlist, 
		  doculects = doculects, concepts = concepts, counterparts = counterparts, 
			sep = sep
			)
		BC <- W$NS %*% W$SW %*% t(W$CW)
		if (is.null(weight)) {
			sim <- assocSparse( BC, method = assoc.method )
		} else {
			sim <- cosSparse( BC, weight =  weight )
		}		
	}	
	if (!is.na(pmatch(method,"bigrams"))) {
		W <- splitWordlist(wordlist, 
		  doculects = doculects, concepts = concepts, counterparts = counterparts, 
			ngrams = 2, sep = sep
			)
		TC <- W$GS %*% W$SW %*% t(W$CW)
		if (is.null(weight)) {
			sim <- assocSparse( TC, method = assoc.method )
		} else {
			sim <- cosSparse( TC, weight = weight )
		}		
	}		
	colnames(sim) <- rownames(sim) <- W$concepts
	return(sim)		
}