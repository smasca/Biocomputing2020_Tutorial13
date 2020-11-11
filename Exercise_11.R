# Samantha Masca
# BIOS 30318
# Exercise 11
# TA: Elizabeth Brooks

rN <- 0.1
rM <- 0.1 
K <- 1000000 # carrying capacity 

rND <- -0.1 # cancer drug present, non-mutant 
rMD <- 0.05 # cancer drug present, mutant 

times <- 1:500

Ns <- matrix(data=NA, nrow=length(times), ncol=3)
Ns[,1] <- times
Ns[1,2] <- 2
Ns[1,3] <- 2


# growth rate with mutation at 100 cells
for(i in times[-1]){
  if(Ns < 100) {
    Ns[i,2] <- Ns[(i-1),2]+rN*Ns[(i-1),2]*(1-(Ns[(i-1),2]+Ns[(i-1),3])/K)
    Ns[i,3] <- Ns[(i-1),3]+rM*Ns[(i-1),3]*(1-(Ns[(i-1),2]+Ns[(i-1),3])/K) 
  }
  else {
    Ns[i,2] <- Ns[(i-1),2]+rND*Ns[(i-1),2]*(1-(Ns[(i-1),2]+Ns[(i-1),3])/K)
    Ns[i,3] <- Ns[(i-1),3]+rMD*Ns[(i-1),3]*(1-(Ns[(i-1),2]+Ns[(i-1),3])/K)
  }
}


# plotting results
df <- data.frame(time=Ns[,1], pop1=Ns[,2], pop2=Ns[,3])

ggplot(data=df, aes(x=time, y=pop1)) +
  geom_point() +
  theme_classic()

