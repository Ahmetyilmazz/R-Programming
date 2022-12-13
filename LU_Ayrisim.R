

# Programmer: AHMET YILMAZ 
# Matematik - Bilgisayar / 19040181039
# R_Lu_Ayrisimi

# LU_AYRIŞIM_ÇALIŞMASI_Sayfa_424

A <- matrix(c(1,2,3,-1,1,1,-1,2,0,-1,-1,3,3,1,2,-1), nrow = 4)


luayrisimi <- function(A){
  
  n<- nrow(A)
  L <- U <- matrix(c(0), nrow = n, ncol = n) # U <- ile de yazılabilirdi.
  diag(L) <- 1 # Köşegenlerinin 1 olmasını sağlamak için. (L) matrisi için
  
  
  # Step - 1
  U[1,1] <- A[1,1] / L[1,1]
  if((L[1,1] * U[1,1]) == 0){
    stop("MATRİSİN LU AYRIŞIMI MÜMKÜN DEĞİLDİR.")
    
  }
  
  # Step - 2
  for (j in 2:n){
    U[1,j] <- A[1,j] / L[1,1]
    L[j,1] <- A[j,1] / U[1,1] 
  }
  
  # Step - 3 
  for (i in 2:(n-1)){
    
    # Step - 4
    L[i,i] <- (A[i,i] - sum(L[i,1:(i-1)] * U[1,1:(i-1)])) / U[i,i]
    #U[i,i] <- (A[i,i] - sum(L[i,1:(i-1)] * U[1,1:(i-1)])) / U[i,i]
    if(L[i,i] * U[i,i] == 0){
      stop("MATRİSİN LU AYRIŞIMI MÜMKÜN DEĞİLDİR.")
    }
    
    # Step - 5  - !!!!!!!!!! -
    for (j in (i + 1):n){
      U[i,j] <- (1 / L[i,i]) * ((A[i,j]) - sum(L[i,1:(i-1)] * U[1,1:(i-1)]))
      L[j,i] <- (1 / U[i,i]) * ((A[j,i]) - sum(L[i,1:(i-1)] * U[1,1:(i-1)]))
    }
    
  }
  
  
  # Step - 6
  L[n,n] <- ((A [n,n]) - (sum(L[n,1:(n-1)] * U[1,n:(n-1)]))) / U[n,n]
  if(L[n,n] * U[n,n] == 0){
    stop("A = LU ve A, TEKİLDİR.")
  }
  
  # Step - 7
  for (j in 1:i) {
    for(i in 1:n){
      L[i,j] <- L
    }
  }
  
  for (j in i:n){
    for(i in 1:n){
      U[i,j] <- U
    }
  }
  list(L=L, U=U)
  stop("Algoritma Bitmiştir. \n")
}
