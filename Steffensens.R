

# Programmer : AHMET YILMAZ
# Matematik - Bilgisayar / 4

# Steffensens #


g <- function(x){
  sqrt(10/(x+4))
}

steffensens<-function(g,p0,TOL,N){
  # Step - 1
  i <- 1
  p_cozum<- c()
  
  # Step - 2
  while(i<=N){
    
    # Step - 3
    p1<-g(p0)
    p2<-g(p1)
    p<- p0-((p1-p0)^2 / (p2-2*p1+p0))
    p_cozum[i]<-p
    
    # Step - 4
    if(abs(p-p0)<TOL){
      cat("Algoritma başarılı Kök değeri = ",p,"\n")
      return(data.frame(p))
    }
    
    # Step - 5
    i<-i+1
    
    # Step - 6
    p0<-p
    
  }
  
  # Step - 7
  list(p=p)
  stop("Maksimum iterasyon sayısı aşıldı")
}
