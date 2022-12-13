
# Programmer : Ahmet YILMAZ 
# 

y <- function(t) {(t + 1)^2 - 0.5* exp(t)}

y_dif <- function(t,y){return(y - t^2 + 1)}

Euler <- function(y,y_dif,a,b,N) 
{
  
  # Step - 1
  
  h <- (b - a) / N
  t <- a
  w <- a
  list(t=t , w=w)
  
  # Step - 2
  
  for (i in 1: (N + 1)){
    # Step - 3
    w <- w + h*y_dif(t,w)  #wi
    t <- a + i * h         #ti
    
    # Step - 4 
    cat("\nt = ",t,"\nw =",w)
  }
  
  # Step - 5
  return()
}