

# Programmer : AHMET YILMAZ
# Matematik - Bilgisayar / 4

# Sabit_Nokta #



g <- function(x){1/2*sqrt(10-(x^3)) }
fixe4d_point <- function(g,p0,TOL,N){
  # Step - 1
  i<-1 
  p_seq <- c()
  
  # Step - 2
  while(i<=N){
    
    # Step - 3
    p <- g(p0)
    
    p_seq[i] <- p
    
    # Step - 4
    if( abs(p-p0) < TOL){
      cat("Algoritma başarılı Kök değeri=",p,"\n")
      return(data.frame(p_seq))
    }
    
    # Step - 5
    i<-i+1
    
    # Step - 6
    p0 <- p 
    
    
  }
  
  # Step - 7
  list(p=p)
  stop("maksimum iterasyon sayısı aşıldı")
}