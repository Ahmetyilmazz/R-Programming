

# Programmer : AHMET YILMAZ
# Matematik - Bilgisayar / 4 


f <- function(x){cos(x) - x} # ÖRNEK FONKSİYON
secant <- function(f,p0,p1,TOL,N0){
  
  # Step - 1
  i <- 2
  q0 <- f(p0)
  q1 <- f(p1)
  
  
  p_seq <- c()
  
  # Step - 2
  while(i <= N0) {
    
    # Step - 3
    p <- p1 - (q1*(p1 - p0) / (q1 - q0))
    p_seq[i] <- p
    
    # Step - 4
    if(abs(p-p1) < TOL){
      cat("Algoritma başarılı Kök değeri=",p,"\n")
      return(data.frame(p_seq))
    }
    
    # Step - 5
    i <- i+1
    
    # Step - 6
    p0 <- p1
    q0 <- q1
    p1 <- p
    q1 <- f(p)
  }
  
  # Step - 7
  list(p=p)
  stop("Maksimum iterasyon sayısına ulaşıldı")
  return(data.frame(p_seq))
}
