

# Programmer : AHMET YILMAZ
# Matematik - Bilgisayar / 4

# LDLt_Factorization #


A <- A<- matrix(c(4,-1,1,
                  -1,4.25,2.75,
                  1,2.75,3.5) ,byrow=T, nrow=3)

LDLt <- function(A){
  
  n <- nrow(A)
  L <- matrix(c(0),n,n)
  
  # Step - 1
  for(i in 1:n){
    # Step - 2
    for (j in 1:(i-1)){
      V[j] <- L[i,j] * D(j)
    }
    
    # Step - 3
    D[i] <- A[i,i] - sum(L[i,1:(i-1)] * V[j])
    
    # Step - 4
    for (j in (i+1):n){
      L[j,i] <- ((A[j,i]) - sum()) / D[i]
    }
  }
  
}