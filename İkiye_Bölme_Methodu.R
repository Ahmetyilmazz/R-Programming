

# Programmer : AHMET YILMAZ
# Matematik - Bilgisayar / 4

# İkiye_Bölme_Methodu #



f <-function(x){
  return( (x^3) + 4*(x^2) -10)
} 
bisection <- function(f,a,b,TOL,N=100){
  # Step - 1
  i<-1
  FA <- f(a)
  
  # Step - 2
  while ( i<= N) {
    
    # Step - 3
    p <- a + ((b-a)/2) 
    FP <- f(p)
    
    # Step - 4
    if( FP==0 | (b-a)/2 < TOL){
      cat("Algoritma başarılı Kök değeri=",p,"\n")
      return(p)
    }
    
    # Step - 5
    i <- i+1
    
    # Step - 6
    if( FA*FP > 0 ){
      a  <- p
      FA <- FP
    }
    else{
      b <- p
    }
  }
  
  # Step - 7
  stop("maksimum iterasyon sayısı aşıldı")
}





























