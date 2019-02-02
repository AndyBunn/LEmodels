# Illustrate how weird things can get with a distrubance in a simple markov model
# here we use a Watt's beech seral succession as an example
rm(list=ls())
# state matrix (cover type) 25% in each cover type
sMat <- matrix(rep(0.25,4),ncol=1)
rownames(sMat) <- c("gap","bare","oxalis","rubus")

# transition matrix
tMat <- matrix(c(0.933,0.067,0,0,
                 0,0.985,0.015,0,
                 0.018,0,0.932,0.05,
                 0.018,0,0,0.982),ncol=4)
colnames(tMat) <- c("gap","bare","oxalis","rubus")
rownames(tMat) <- c("gap_t+1","bare_t+1","oxalis_t+1","rubus_t+1")

sMat
tMat
# note colSums of tMat
colSums(tMat)

# watch equlibrium 
n <- 100
res <- matrix(NA,ncol=4,nrow=n)
res[1,] <- t(sMat)
for(i in 2:n){
  sMat <- tMat %*% sMat
  res[i,] <- t(sMat)
}
plot(1:n,res[,1],type="n",ylim=c(0,0.6),
     xlab="Time",ylab="Proportion")
lines(1:n,res[,1],col="gold",lwd=2)
lines(1:n,res[,2],col="brown",lwd=2)
lines(1:n,res[,3],col="green",lwd=2)
lines(1:n,res[,4],col="darkgreen",lwd=2)
legend(c("gap","bare","oxalis","rubus"), x="topleft",
       col=c("gold","brown","green","darkgreen"),
       lwd=2,ncol=4)

# Now with disturbance at yr 100
# reset sMat
sMat <- matrix(rep(0.25,4),ncol=1)
rownames(sMat) <- c("gap","bare","oxalis","rubus")

n <- 250
res <- matrix(NA,ncol=4,nrow=n)
res[1,] <- t(sMat)
for(i in 2:n){
  sMat <- tMat %*% sMat
  res[i,] <- t(sMat)
  if(i==101) {
    # Disturbance in yr 100 killed 1/3 of “bare” and half of “oxalis” and “rubus”
    sMat <- sMat * c(1,1/3,1/2,1/2)
    sMat[1,1] <- 1- sum(sMat[-1,1])
  }
}
plot(1:n,res[,1],type="n",ylim=c(0,0.7),
     xlab="Time",ylab="Proportion")
lines(1:n,res[,1],col="gold",lwd=2)
lines(1:n,res[,2],col="brown",lwd=2)
lines(1:n,res[,3],col="green",lwd=2)
lines(1:n,res[,4],col="darkgreen",lwd=2)
legend(c("gap","bare","oxalis","rubus"), x="topleft",
       col=c("gold","brown","green","darkgreen"),
       lwd=2,ncol=4)
