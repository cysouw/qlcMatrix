# =======================
# sparse distance metrics
# =======================

# Sparsity is strange for distance metrics
# because low values (close to zero) are the most interesting ones
# so distance metrics are normally not sparse.

distSparse <- function(M, method = "euclidean", diag = FALSE) {
  
  if (!is(M, "dMatrix")) {
    M <- Matrix(M, sparse = T)
  }
  
  P <- crossprod(M)
  
  # only do elements that have shared non-zeros, i.e. P is non-zero
  N <- as(P, "nMatrix") * 1
  S <- Diagonal(x = colSums(M^2)) %*% N
  
  # euclidean distance
  D <- S + t(S) - 2 * P
  D <- as(D, "symmetricMatrix")
  D@x <- sqrt(D@x)
  
  # reverse to similarity
  D <- max(D)-D	

  if (!diag) {
    diag(D) <- 0
  }
  
  return(drop0(D))
  
}
