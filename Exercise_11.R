# Samantha Masca
# BIOS 30318
# Exercise 11
# TA: Elizabeth Brooks

rN <- 0.1
rM <- 0.1 
K <- 1000000 # carrying capacity 

rND <- -0.1 # cancer drug present, non-mutant 
rMD <- 0.05 # cancer drug present, mutant 

times <- 1:500 # set timesteps

Ns <- matrix(data=NA, nrow=length(times), ncol=3) # set matrix to hold updated population size
Ns[,1] <- times
Ns[1,2] <- 100
Ns[1,3] <- 1


# growth rate of both populations, with mutation at 100 cells
for(i in times[-1]){
  if(i < 150) {
    Ns[i,2] <- Ns[(i-1),2]+rN*Ns[(i-1),2]*(1-(Ns[(i-1),2]+Ns[(i-1),3])/K)
    Ns[i,3] <- Ns[(i-1),3]+rM*Ns[(i-1),3]*(1-(Ns[(i-1),2]+Ns[(i-1),3])/K) 
  }
  else if(i > 149) {
    Ns[i,2] <- Ns[(i-1),2]+rND*Ns[(i-1),2]*(1-(Ns[(i-1),2]+Ns[(i-1),3])/K)
    Ns[i,3] <- Ns[(i-1),3]+rMD*Ns[(i-1),3]*(1-(Ns[(i-1),2]+Ns[(i-1),3])/K)
  }
}


# plotting results
df1 <- data.frame(time=Ns[,1], pop1=Ns[,2]) # non-mutant population
df2 <- data.frame(time=Ns[,1],pop2=Ns[,3]) # mutant population

ggplot() +
  geom_line(data=df1, aes(x=time, y=pop1), color='blue') +
  geom_line(data=df2, aes(x=time, y=pop2), color='red') +
  xlab("Time") +
  ylab("Population Size") +
  theme_classic()

