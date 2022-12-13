

# Programmer : Ahmet YILMAZ


f <- function(x){cos(x) - x}

dif_f <- function(x){(-sin(x))-1}

newton <- function(f,dif_f,p0,TOL,N){
  
  # Step - 1
  i <- 1
  
  p_seq <- c()
  
  # Step - 2
  while(i<= N) {
    
    # Step - 3
    p <- p0 - (f(p0)/ dif_f(p0))
    p_seq[i] <- p
    
    # Step - 4
    if(abs(p-p0) < TOL){
      cat("Algoritma başarılı Kök değeri=",p,"\n")
      return(data.frame(p_seq))
    }
    i <- i+1
    p0<-p
  }
  list(p=p)
  stop("Maksimum iterasyon sayısına ulaşıldı")
  return(data.frame(p_seq))
}


