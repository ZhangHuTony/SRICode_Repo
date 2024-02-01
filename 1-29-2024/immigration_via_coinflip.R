# this file add coin flips
N<-200
y<-sample(c(0,1),N,replace=TRUE)
z<-cumsum(y)
plot(z)

# now do this M times
Gs<-0
M<-10000
for (j in 1:M){
  
  y<-sample(c(0,1),N,replace=TRUE)
  z<-cumsum(y) 
  
  
  
  Gs[j]<-z[N]
}
hist(Gs,breaks=M/20)


