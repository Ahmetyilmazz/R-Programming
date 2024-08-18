g <- function(x){
  sqrt(10/(x+4))
}
steffensens<-function(g,p0,TOL,N){
  #step1
  i <- 1
  p_cozum<- c()
  #step2
  while(i<=N){
    #step3
    p1<-g(p0)
    p2<-g(p1)
    p<- p0-((p1-p0)^2 / (p2-2*p1+p0))
    p_cozum[i]<-p
    #step4
    if(abs(p-p0)<TOL){
      cat("Algoritma başarılı Kök değeri=",p,"\n")
      return(data.frame(p))
    }
    #step5
    i<-i+1
    #step6
    p0<-p
    
  }
  #step7
  list(p=p)
  stop("maksimum iterasyon sayısı aşıldı")
}




