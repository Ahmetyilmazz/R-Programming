
# # Toplu yorum ctrl + shift + c
# 
# # GAUSS_ELİMİNASYON
# gauss.elim <- function(A,b) {
#   n <- nrow(A)
#   A <- cbind(A,b) # Eklenmiş matris elde edilir
#   x <- array(c(NA), dim = c(n,1)) # Çözüm
#   
#   #######################################################
#   # Step 1: for i = 1:n-1 do Steps 2-4 (Elimination Process)
#   
#   
#   
#   
#   
#   print("Step 1")
#   
#   for (i in 1:(n-1)) { 
#     # Step 2
#     print("Step 2")
#     i_index <- which(A[,i] != 0)
#     i_index >= i
#     
#     p <- min(i_index[i_index >= i])          # ÇALIŞIYOR
#     #p <- min(which(A[i:n, i] != 0)) + i - 1 # ÇALIŞIYOR
#     
#     
#     if (is.definite(p) || is.null(p)){
#       cat("Tek bir çözüm yoktur !!! \n")
#       break
#     }
#     
#     #Step-3
#     
#     print("Step -3")
#     if(p != i){
#       A <- swaprows(A, p, i)
#     }
#     
#     #Step -4
#     
#     print("Step -4")
#     for (j in (i + 1):n){
#       # Step -5
#       m <- A[j,i] / A[i,i]
#       #Step -6
#       A <- replacerow(A, row1 = i, row2 = j, k = -m)
#       
#     }
#   } #en dıştaki for döngüsü bitti
#   # Step - 7
#   print("Step - 7")
#   
#   if(A [n,n] == 0)
#     cat("Tek bir çözüm yoktur !!! \n")
#     break
# 
# }
# 
# # Step - 8
# 
# x[n] <- A[n,(n+1)] / A[n,n]
# 
# # Step - 9
# 
# for (i in (n-1):1){
#   
#   x[i] <- (A[i,(n + 1)] - sum(A[i,(i+1):n] * X[(i + 1):n])) / A[i,i] #365_sayfa
# }
# 
# cat("Algoritma Başarılı.. \n")
# 
# list(A=A[, -1], x=x )
# 
# 
# }







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
    (L[i,i] * U[i,i]) <- A[i,i] - sum(L[i,1:(i-1)] * U[1,1:(i-1)])
    
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
  (L[n,n] * U[n,n]) <- (A [n,n]) - (sum(L[n,1:(n-1)] * U[1,n:(n-1)]))
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





