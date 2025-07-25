# qlcMatrix 0.10 (upcoming)

# qlcMatrix 0.9.9

## changes

* adding support for ngrams for n > 2 in splitStrings. this opens up the possibility to use larger ngrams in sim.strings, sim.lang and sim.wordlist
* splitStrings now only gives ngrams as output, not unigrams and bigrams as before
* adding global options "qlcMatrix.gap" and "qlcMatrix.locale" for package defaults that might in some special cases be changed by a user.

## bugs

* more cleanup of unnecessary nMatrix to dMatrix conversions

# qlcMatrix 0.9.8

## bugs

* corrected behavior of multiplying two nMatrices in accordance with the corrected implementation from the Matrix package using %&% instead of %*%
* various minor corrections for CRAN compliancy.

# qlcMatrix 0.9.7

## changes

* adding experimental distSparse function
* adding option to pass arguments from sim.obs and sim.att to splitTable
* adding option to pass arguments from sim.strings to splitStrings
* changed internal gap symbol in pwMatrix and splitStrings to \u2043
* adding dimRed() function for sparse dimensionality Reduction based on Cholesky()
* adding normalized Laplacian norm 'normL' for cosSparse

## bugs

* removed outdated links
* replacing rBind/cBind with rbind/cbind as of R 3.2
* corrected names of rKhatriRao for the complete symmetric case

# qlcMatrix 0.9.6

* adding commandline execs
* importing docopt

# qlcMatrix 0.9.5

* finally adding a NEWS list

## adding some first sparse array functionality, building on spam

* adding unfold() to unfold sparse arrays to matrices
* rename earlier unfold() to unfoldBlockMatrix()
* adding as.Matrix() to link spam to Matrix
* proposing Array() and sparseArray() as function names

## bugs

* ttMatrix now gives NULL when vector is completely NA

# qlcMatrix 0.9.4

* no NEWS documented until here