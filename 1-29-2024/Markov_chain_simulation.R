# a simple Markov chain
# number of steps to take
N<-100
Statestate<-c("Occupied","Extinct")
# a simple Markov chain
pOccOcc<-0.9
pOccExt<-0.1
pExtExt<-0.8
pExtOcc<-0.2
 
Transition<-matrix(c(pOccOcc,pExtOcc,pOccExt,pExtExt),nrow=2,dimnames = list(Statestate,Statestate))
print(Transition)  # state 2 is absorbing
state<- c(1,0)  # initial state vector
 
out<-numeric(N)
out[1]<-sample(1:2,1,prob=state)
for(i in 2:N)
  out[i]<-sample(1:2,1,prob=Transition[out[i-1],])
                                   
plot(out) 
 