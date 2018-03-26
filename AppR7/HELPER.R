OUT1 <- read.table(file="data/OUT3_noDMSO_noUntreated.txt",sep ="\t", header=TRUE,row.names=1)



F1 <- matrix(nrow = dim(OUT1)[1], ncol =dim(OUT1)[1])
for (i in 1:dim(OUT1)[1])
{
  print(i)
  for (j in 1:dim(OUT1)[1])
  {
    c=0
    b=0
    d=0
    
    Prod <- OUT1[i,]*OUT1[j,]
    b <- length(Prod[Prod>0])
    c <- length(Prod[Prod<0])
    
    
    if (c==0)
    {c<- 1}
    R <- b/c
  F1[i,j] <- R
  
  
  }
}
