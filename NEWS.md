# qlcMatrix 0.9.6

* adding option to pass arguments from sim.obs and sim.att to splitTable
* adding option to pass arguments from sim.strings to splitStrings
* changed internal gap symbol in pwMatrix to \u2043
* adding dimRed() function for sparse dimensionality Reduction based on Cholesky()
* adding normalized Laplacian norm for cosSparse
* adding commandline execs

## bugs
* corrected names of rKhatriRao for the complete symmetric case

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