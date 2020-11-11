# Samantha Masca
# BIOS 30318
# Exercise 11
# TA: Elizabeth Brooks

rN <- 0.1 # drug absent growth rate
rM <- 0.05
K <- 1000000

times <- 1:300

# for non-treated population
Ns <- matrix(data=NA, nrow=length(times), ncol=2)
Ns[,1] <- times
Ns[1,2] <- 2

# for treated population
Ms <- matrix(data=NA, nrow=length(times), ncol=2)
Ms[,1] <- times 
Ms[1,2] <- 2

for(i in times[-1]){
  Ns[i,2] <- Ns[(i-1),2]+rN*Ns[(i-1),2]*(1-(Ns[(i-1),2]+Ms[(i-1),2])/K)
}

for(i in times[-1]){
  Ms[i,2] <- Ms[(i-1),2]+rM*Ms[(i-1),2]*(1-(Ns[(i-1),2]+Ms[(i-1),2])/K)
}


# plotting results
df <- data.frame(time=Ns[,1],pop=Ns[,2])

ggplot(data=df, aes(x=time, y=pop)) +
  geom_point() +
  theme_classic()

