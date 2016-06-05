dimRed <- function(sim, k = 2, method = "svd") {
  
  if (!is.na(pmatch(method,"cholesky"))) {
    
    ch <- Cholesky(sim)
    M <- sparseMatrix( x = ch@x
                     , p = ch@p
                     , i = ch@i
                     , index1 = FALSE
    )
    P <- as(ch, "pMatrix")
    D <- diag(M)[1:k]
    diag(M) <- 1
    L <- P %*% M[,1:k]
    
    return( list( L = L, D = D) )
    
  } else if (!is.na(pmatch(method,"svd"))) {
    
    svd <- sparsesvd::sparsesvd(sim, rank = k)
    return( list( L = svd$u, D = svd$d))
    
  }

}