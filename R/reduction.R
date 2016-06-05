dimRed <- function(sim, k = 2) {
  
  ch <- Cholesky(sim)
  M <- sparseMatrix( x = ch@x
                   , p = ch@p
                   , i = ch@i
                   , index1 = FALSE
  )
  P <- as(ch, "pMatrix")
  D <- diag(M)
  diag(M) <- 1
  L <- P %*% M[,1:k]
  
  return( list( L = L, D = D) )
  
}