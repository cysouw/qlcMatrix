# ==================================================
# High-level functions 
# to convert data into collection of sparse matrices
# ==================================================

# split data table with nominal data into matrices
# consider for future to do this via long-form ("reshape")

splitTable <- function(	data,
						attributes = colnames(data),
						observations = rownames(data),
						name.binder = ":",
            split = NULL
						) {

	# assuming a data table with
	#	- variables ("A"ttributes) as columns and 
	#	- observations (O) as rows
	#	- values (V) in the cells (different for each column)
  # - optionally, multiple values can occur in each cell
  #   separated by character indicated in "split"
	
  # multiple values in one cell can be splitted
  splitColumn <- function(column, split) {
    parts <- strsplit(column, split)
    all <- unique(unlist(parts))
    matches <- sapply(parts, function(x){match(x, all)})
    M <- sparseMatrix(i = unlist(matches),
                      j = rep.int(1:length(matches),
                                  sapply(matches,length)
                                  )
                      )
    return(list( M = M,
                 rownames = all
                ))
  }

  # basic rewrite, the rest is cosmetic
  if (!is.null(split)) {
    tt <- apply(data,2,function(x){splitColumn(x, split = split)})
  } else {
    tt <- apply(data,2,ttMatrix)
  }
  
	OV <- tt[[1]]$M
	for (i in tt[-1]) {
		OV <- rbind(OV,i$M)
	}
	OV <- t(OV)
	
	# the following approach is slow... strange...
	# http://www.r-bloggers.com/the-rbinding-race-for-vs-do-call-vs-rbind-fill/
	# OV <- t(do.call(rbind,sapply(tt,function(x){x[[1]]})))

  cls <- ncol(data)
  
	# index matrix of variables to values
	nrValues <- sapply(tt,function(x){length(x$rownames)})
	AV <- ttMatrix(rep.int(1:cls,nrValues))$M
	
	# we need some variable names to make value names, 
	# but entity names might be NULL

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

# make ngram matrices from a vector of strings

splitStrings <- function(	strings,
							sep = "",
							n = 2,
							boundary = TRUE,
							ngram.binder = "",
							left.boundary = "#",
							right.boundary = "#",
							simplify = FALSE
							) {

  # add boundary symbols
	originals <- strings
	if (n > 1 && boundary) {
		strings <- paste(left.boundary, strings, right.boundary, sep = sep)
	}

	# SW: Segments x Strings ("Words")
	tmp <- pwMatrix(strings, sep = sep, gap.length = n-1)
	SW <- tmp$M
	segments <- tmp$rownames

	# NS: ngrams x segments (starting with unigrams)
	tmp <- ttMatrix(segments)
	NS <- tmp$M
	ngrams <- tmp$rownames

	# larger ngrams
	if (n > 1) {
	  
  	# remove gap character from ngrams
  	remove <- which(ngrams == getOption("qlcMatrix.gap"))
  	if (length(remove) > 0) {
    	ngrams <- ngrams[-remove]
    	NS <- NS[-remove,]
  	}
  	
  	# remember unigrams
	  US <- NS
	  unigrams <- ngrams
	  
	  # construct ngrams
	  for (i in 2:n) {
		  S <- bandSparse(n = dim(NS)[2], k = i-1)
		  tmp <- rKhatriRao(NS, US %&% S, ngrams, unigrams, binder = ngram.binder)
		  NS <- tmp$M
		  ngrams <- tmp$rownames
	  }
	}
	
	# remove gap symbol
	remove <- which(segments == getOption("qlcMatrix.gap"))
	if (length(remove > 0)) {
	  segments <- segments[-remove]
	  SW <- SW[-remove, ]
	  NS <- NS[, -remove]
	}
	
	# order ngrams according to ttMatrix locale
	NN <- ttMatrix(ngrams)
	NS <- NN$M %&% NS
	ngrams <- NN$rownames
	
	# prepare result
	if (simplify) {
		result <- NS %*% SW
		rownames(result) <- ngrams
		colnames(result) <- originals
	} else {
		result <- list(	
		  segments = segments,
			ngrams = ngrams,
			SW = SW, # Segments x Words
			NS = NS # Unigrams x Segments
			)
	}
	return(result)
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

	# make RunningWords x Verses, i.e. all words of the text in the order 
  # as they appear as rows, linked to the localSentenceIDs as columns.	
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
		US <- t(tmp$M2) %&% tmp$M1
		# relink
		RS <- RS %&% t(US)
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
			R <- (WR) %*% (RS)
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
			R <- (wW) %*% (WR) %*% (RS)
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
							ngrams = 2,
							sep =  "",
							ngram.binder = "",
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
	words <- sapply(strsplit(words, binder), head)[2,]

	# relink	
	DW <- DL %&% t(WL)
	CW <- CL %&% t(WL)
	
	if (!is.null(ngrams)) {
	
		# split strings
		S <- splitStrings(words, sep = sep, n = ngrams, ngram.binder = ngram.binder)
	
		# return results
		if (simplify) {
			
			NW <- S$NS %*% S$SW
			
			# only use column names once because of size
			rownames(DW) <- doculects
			rownames(CW) <- concepts
			colnames(NW) <- words
			rownames(NW) <- S$ngrams
					
			return(list(DW = DW, CW = CW, NW = NW))
			
		} else {
			# separate characters to languages
			# and prepare full output (rather long!)
			
			# link to segments to doculects
			DS <- DW %&% t(S$SW)
	
			# KhatriRao to turn N-symbols into 
			# language-specific N-graphs x Segments
			tmp <- rKhatriRao(	DS, S$NS, 
								doculects, S$ngrams, 
								binder = grapheme.binder
								)
			GS <- tmp$M
			ngraphs <- tmp$rownames
	
			return(list(	doculects = doculects,
							concepts = concepts,
							words = words,
						
							segments = S$segments,
							ngrams = S$ngrams, 
							ngraphs = ngraphs,
	
							DW = DW, # Doculects x Words
							CW = CW, # Concepts x Words

							SW = S$SW,	# Segments x Words
							NS = S$NS,	# N-grams x Segments
							GS = GS 	# N-graphs x Segments	
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