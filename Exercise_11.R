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

Ns <- matrix(data=NA, nrow=length(times), ncol=2)
Ns[,1] <- times
Ns[1,2] <- 2

Ms <- matrix(data=NA, nrow=length(times), ncol=2)
Ms[,1] <- times 
Ms[1,2] <- 2



# growth rate with mutation at 100 cells
for(i in times[-1]){
  if(Ns[,2] < 100) {
    Ns[i,2] <- Ns[(i-1),2]+rN*Ns[(i-1),2]*(1-(Ns[(i-1),2]+Ms[(i-1),2])/K)
    Ms[i,2] <- Ms[(i-1),2]+rM*Ms[(i-1),2]*(1-(Ns[(i-1),2]+Ms[(i-1),2])/K) 
  }
  else {
    Ns[i,2] <- Ns[(i-1),2]+rND*Ns[(i-1),2]*(1-(Ns[(i-1),2]+Ms[(i-1),2])/K)
    Ms[i,2] <- Ms[(i-1),2]+rMD*Ms[(i-1),2]*(1-(Ns[(i-1),2]+Ms[(i-1),2])/K)
  }
}


# plotting results
df <- data.frame(time=Ns[,1],pop=Ns[,2])

ggplot(data=df, aes(x=time, y=pop)) +
  geom_point() +
  theme_classic()

