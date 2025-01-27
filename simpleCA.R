# Make a simple cellular automata to show how dispersal causes pattern 
# from a random start
rm(list=ls())
# set grid dimensions
n <- 100
nCells <- n*n
# Here is an empty grid that we will fill in
mat <- matrix(0,ncol=n,nrow=n)
# Randomy seed 10% of cells with presence (1) vs absence (0)
cells2sees <- sample(x = 1:nCells, size = nCells * 0.1,replace = FALSE)
mat[cells2sees] <- 1
# make a copy of this to simulate dispersal and mortality on

# Here are the mortality and dispersal rates
pMort <- 0.01
pRepro <- 0.015

# Now simulate

nIter <- 1000
resArray <- array(0,dim=c(n,n,nIter))
for(k in 1:nIter){
  for(i in 1:n){
    for(j in 1:n){
      if(mat[i,j] == 1){
        # mortality
        reaper <- runif(n = 1, min = 0, max = 1)
        if(reaper < pMort) {
          mat[i,j] <- 0 
          next
        }
        # reproduction
        stork <- runif(n = 1, min = 0, max = 1)
        if(stork < pRepro){
          # pick a neighbor to disperse to. Queen's rules
          iNew <- i + sample(c(-1,0,1),1)
          jNew <- j + sample(c(-1,0,1),1)
          # check to make sure we aren't off edge of map
          if(iNew < 1 | iNew > n) next
          if(jNew < 1 | jNew > n) next
          # and disperse
          mat[iNew,jNew] <- 1
        }
      }
    }
  }
  resArray[,,k] <- mat
}

library(ggplot2)
library(reshape2)
library(animation)


saveGIF({
  ani.options(interval = 0.5)
  for(k in c(1,seq(20,nIter,by=20))){
    resMelt <- melt(resArray[,,k],varnames = c("x","y"),value.name = "presence")
    resMelt$presence <- factor(resMelt$presence)
    p1 <- ggplot(data=resMelt,aes(x=x, y=y, fill=presence)) 
    p1 <- p1 + geom_raster()
    p1 <- p1 + coord_fixed() + labs(x="",y="",subtitle=paste("iter",k))
    p1 <- p1 + scale_fill_manual(values=c("grey80","grey10"))
    p1 <- p1 + theme(axis.title.y=element_blank(),
                     axis.text.y=element_blank(),
                     axis.ticks.y=element_blank(),
                     axis.title.x=element_blank(),
                     axis.text.x=element_blank(),
                     axis.ticks.x=element_blank(),
                     legend.position="none")
    print(p1)
  }
}, movie.name = "dispersalCA.gif", ani.width = 600, ani.height = 600)

    
