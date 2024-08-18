

# Koşegen Matris

C <- diag(1,nrow=3) 
C

#####
A <- matrix(1:25,byrow = T,nrow = 5)
A

colSums(A) # Matris stünlarının toplamı
rowSums(A) # Matris satırlarının 




##########




fkryl <- function(x){
  if(x<0){
    return("Başka bir değer giriniz")
  }
  sayi <- 1
  if(x==0){
    sayi
  }else{
    for (i in x:1) {
      sayi <- sayi*i
    }
  }
  return(sayi)
}
fkryl(5)
fkryl(2)
fkryl(0)
fkryl(-4)
fkryl(4)
