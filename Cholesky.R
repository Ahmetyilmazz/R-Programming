

# Programmer : AHMET YILMAZ
# Matematik - Bilgisayar / 4

# Cholesky #


A <- A<- matrix(c(4,-1,1,
                  -1,4.25,2.75,
                  1,2.75,3.5) ,byrow=T, nrow=3)

Cholesky <- function(A){
  
  n <- nrow(A)
  L <- matrix(c(0),n,n)
  
  # Step - 1
  L[1,1] <- sqrt(A[1,1])
  
  # Step - 2
  L[2:n,1] <- A[2:n,1] / L[1,1]
  
  # Step - 3
  for(i in 2:(n-1)){
    #Step - 4
    L[i,i] <- sqrt(A[i,i] - sum(L[i,1:(i-1)]^2))
    
    # Step - 5
    
    for(j in (i+1):n){
      L[j,i] <- (A[j,i] - sum(L[j,1:(i-1)] * L[i, 1:(i-1)])) / L[i,i]
    }
  }
  
  # Step - 6
  L[n,n] <- sqrt(A[n,n] - sum(L[n,1:(n-1)]^2))
  
  # Step - 7
  list(L=L)
  
}
