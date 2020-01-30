# Make a simple cellular automata to show fire contagion
rm(list=ls())
setwd("~/Documents/teaching/AY2019_2020/ESCI435/w04_dist/")
# need floodfill
library(EBImage)


#####################################
#
## Parameters
#
#####################################

##########
#
## Mortalilty 
#
##########
# prob of random mort
pRanMort <- 0.009
# and age-related mort curve -- maxes at 200
#pAgeMortFunc <- function(x){4e-07 * exp(0.0694 * x)}
a <- 6.5518492120788311E-12
b <- 1.2879586365885426E-01
pAgeMortFunc <- function(x){a * exp(b * x)}
ages <- 1:200
plot(ages,pAgeMortFunc(ages),type="l")

##########
#
## Contagion probability
#
##########

# here is the function that will determine if something will burn or not
# x0 is where p=0.5
x0 <- 50
# steepness
k <- 0.2
pContagionFunc <- function(x){1/(1+exp(-k*(x-x0)))}
#x <- 1:100
#y <- pContagionFunc(x)
#plot(x,y,type="l")
#abline(v=x0)
#abline(h=0.5)
# so if there is a pixel with value of 60 the p it will 
# allow a fire to spread to it is: 1/(1+exp(-k*(60-x0)))

# here is the function that gets called every iteration and chooses the 
# threshold above which the burn surface is contagious. 
# goofy curve
tContagion <- function(){1 - abs(rnorm(n = 1, mean = 0, sd = 0.1))}

##########
#
## Grid dimensions 
## and age init
#
##########
n <- 128
# random start of ages
initAges <- floor(runif(n^2,min = 1,max = 100))
age <- matrix(initAges,n,n)

##########
#
## Length of sim
## and init output structures
#
##########
nIter <- 2000
ageArray <- burnArray <- array(0,dim=c(n,n,nIter))

#####################################
#
## Run it
#
#####################################
for(i in 1:nIter){
  pContagion <- pContagionFunc(age)
  canBurn <- matrix(0,n,n)
  canBurn[pContagion > tContagion()] <- 1
  # select random point for ignition
  ranX <- floor(runif(n = 1,min = 1,max = n+1))
  ranY <- floor(runif(n = 1,min = 1,max = n+1))
  # if canBurn == 1 then
  if(canBurn[ranX,ranY] == 1){
    burned <- floodFill(canBurn,c(ranX,ranY), col = 999)
    age[burned==999] <- 0
    burnArray[,,i] <- burned
  }
  # random mort -- select pRanMort cells and kill em
  age[sample(1:n^2,size=n^2 * pRanMort)] <- 0
  # age mort
  #age[age > maxAge] <- 0
  # age mort using prob curve
  # first gen randoms
  pMortRan <- matrix(runif(n^2),n,n)
  pAgeMort <- pAgeMortFunc(age)
  age[pAgeMort > pMortRan] <- 0
  age <- age + 1
  ageArray[,,i] <- age
}


library(ggplot2)
library(gridExtra)
library(reshape2)
library(animation)
library(viridis)

burnPer <- rep(0,nIter) 
for(i in 1:nIter){
  burnPer[i] <- sum(burnArray[,,i]==999)/n^2
}

layMat <- rbind(c(1, 1, 1, 1, 2,2),
                c(1, 1, 1, 1, 2,2),
                c(1, 1, 1, 1, 3,3),
                c(1, 1, 1, 1, 3,3))

saveGIF({
  ani.options(interval = 0.5)
  for(j in c(1,seq(10,nIter,by=10))){
    ageGridMelt <- melt(ageArray[,,j],varnames = c("x","y"),value.name = "age")
    p1 <- ggplot(data=ageGridMelt,aes(x=x, y=y, fill=age)) 
    p1 <- p1 + geom_raster()
    p1 <- p1 + coord_fixed() + labs(x="",y="",subtitle=paste("iter",j))
    p1 <- p1 + scale_fill_gradientn(colors=viridis(10),
                                    breaks=seq(0,200,by=20),limits=c(0,200))
    p1 <- p1 + theme(axis.title.y=element_blank(),
                     axis.text.y=element_blank(),
                     axis.ticks.y=element_blank(),
                     axis.title.x=element_blank(),
                     axis.text.x=element_blank(),
                     axis.ticks.x=element_blank(),
                     legend.position="none")
    
    p2 <- ggplot(data=ageGridMelt,aes(x=age)) 
    p2 <- p2 + geom_histogram(aes(y=(..count..)/sum(..count..)),
                              breaks = seq(0,200,by=20),
                              fill=viridis(10))
    p2 <- p2 + labs(x="Age",y="% Cells",subtitle="")
    p2 <- p2 + scale_y_continuous(labels = percent_format())
#    p2 <- p2 + theme(axis.title.y=element_blank(),
#                     axis.text.y=element_blank(),
#                     axis.ticks.y=element_blank())
    
    burnAreaDF <- data.frame(x=1:j,y=burnPer[1:j])
    p3 <- ggplot(data=burnAreaDF,aes(x=x,y=y)) 
    p3 <- p3 + geom_line()
    p3 <- p3 + xlim(0,nIter) + ylim(0,max(burnAreaDF$y))
    p3 <- p3 + labs(x="Iteration",y="Burn %")
    
    # this is not a good layout
    grid.arrange(p1,p2,p3, layout_matrix = layMat)
  }
}, movie.name = "fireCA.gif", ani.width = 600, ani.height = 600)

