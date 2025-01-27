###################################################################################
# Make a simple cellular automata to show how dispersal causes pattern
# from a random start

## Jan 25 2025 -- added 2nd spp and a good phys temp
## next: add switches to do neighborhood x phys temp to get
## 4x4 pattern a la Urban's siera example. Make cooler mort funcs

rm(list=ls())
library(tidyverse)
library(ambient)
library(animation)
library(terra)
library(tidyterra)
library(sf)
library(ggnewscale)

set.seed(567)
# set dimensions
n <- 128
nCells <- n^2
y_coords <- x_coords <- seq(1, 128, by= 1)

# Create a grid of coordinates
trendSurfaceDF <- expand.grid(x = x_coords, y = y_coords)
# Create a nonlinear trend along the diagonal
# The trend increases as x + y increases
trendSurfaceDF$z <- (trendSurfaceDF$x + trendSurfaceDF$y)^1.5
# Scale [0,1]
trendSurfaceDF$z <- normalise(trendSurfaceDF$z, 
                              from = range(trendSurfaceDF), 
                              to = c(0, 1))
# quick peak
tmp <- rast(trendSurfaceDF)
ggplot() + 
  geom_spatraster(data=tmp) +
  scale_fill_terrain_c() +
  coord_fixed() +
  theme_void()

# Make some Perlin noise
noiseSurfaceDF <- trendSurfaceDF[,1:2]
noiseSurfaceDF$z <- gen_perlin(x = normalise(noiseSurfaceDF$x,to = c(0,1)),
                               y = normalise(noiseSurfaceDF$y,to = c(0,1)),
                               frequency = 5)
# Scale [0,1]
noiseSurfaceDF$z <- normalise(noiseSurfaceDF$z, 
                              from = range(noiseSurfaceDF$z), 
                              to = c(0,1))

# quick peak
tmp <- rast(noiseSurfaceDF)
ggplot() + 
  geom_spatraster(data=tmp) +
  scale_fill_terrain_c() +
  coord_fixed() +
  theme_void()

# Combining trend and noise
physTempSurfaceDF <- trendSurfaceDF
# weight trend and noise
physTempSurfaceDF$z <- noiseSurfaceDF$z + physTempSurfaceDF$z/2
# Scale [0,1]
physTempSurfaceDF$z <- normalise(physTempSurfaceDF$z,
                                 from = range(physTempSurfaceDF$z),to = c(0,1))

physTempRast <- rast(physTempSurfaceDF)


p1 <- ggplot() + 
  geom_spatraster(data=physTempRast) +
  scale_fill_terrain_c(name = "Some gradient") +
  new_scale_fill() +
  geom_spatraster_contour_filled(data=physTempRast,
                                 color="white",
                                 alpha=0.1,
                                 breaks=seq(0,1,by=0.2)) +
  scale_fill_terrain_d(guide = "none") +
  coord_sf(default = TRUE) +
  theme_void()
p1

# why do this on a matrix as opposed to a spat raster?
physTempMat <- matrix(physTempSurfaceDF$z,nrow=n,ncol=n)
#image(physTempMat,asp=1) # orientation looks OK
# Now simulate

# how many iterations to use
nIter <- 500

# Randomly seed some % of cells with presence (1) vs absence (0)
pSeed <- 0.05

## Stuff for sppA
# Here is the raster for sppA pres/abs
matSppA <- matrix(0,ncol=n,nrow=n)
cells2seedSppA <- sample(x = 1:nCells, 
                         size = nCells * pSeed,
                         replace = FALSE)
matSppA[cells2seedSppA] <- 1

# Structures for outputs
resArraySppA <- array(0,dim=c(n,n,nIter))
occupiedSppA <- numeric()
sppAdf <- data.frame(row=numeric(),col=numeric(),iter=numeric())

## Stuff for sppB
# Here is the raster for sppB pres/abs
matSppB <- matrix(0,ncol=n,nrow=n)
cells2seedSppB <- sample(x = 1:nCells, 
                         size = nCells * pSeed,
                         replace = FALSE)
matSppB[cells2seedSppB] <- 1

# Structures for outputs
resArraySppB <- array(0,dim=c(n,n,nIter))
occupiedSppB <- numeric()
sppBdf <- data.frame(row=numeric(),col=numeric(),iter=numeric())


# Here are the mortality and dispersal rates
pMortA <- 0.01
pReproA <- 0.02

pMortB <- 0.01
pReproB <- 0.02

for(k in 1:nIter){
  for(i in 1:n){
    for(j in 1:n){
      # do sppA
      if(matSppA[i,j] == 1){
        # mortality
        reaper <- runif(n = 1, min = 0, max = 1)
        reaper <- reaper * (1 - physTempMat[i,j])
        if(reaper < pMortA) {
          matSppA[i,j] <- 0
          next
        }
        # reproduction
        stork <- runif(n = 1, min = 0, max = 1)
        if(stork < pReproA){
          # pick a neighbor to disperse to. Queen's rules
          iNew <- i + sample(c(-1,0,1),1)
          jNew <- j + sample(c(-1,0,1),1)
          # check to make sure we aren't off edge of map
          if(iNew < 1 | iNew >= n) next
          if(jNew < 1 | jNew >= n) next
          # and disperse
          matSppA[iNew,jNew] <- 1
        }
      }
      # do sppB
      if(matSppB[i,j] == 1){
        # mortality
        reaper <- runif(n = 1, min = 0, max = 1)
        reaper <- reaper * (physTempMat[i,j]) # opposite mort prob as sppA
        if(reaper < pMortB) {
          matSppB[i,j] <- 0
          next
        }
        # reproduction
        stork <- runif(n = 1, min = 0, max = 1)
        if(stork < pReproB){
          # pick a neighbor to disperse to. Queen's rules
          iNew <- i + sample(c(-1,0,1),1)
          jNew <- j + sample(c(-1,0,1),1)
          # check to make sure we aren't off edge of map
          if(iNew < 1 | iNew >= n) next
          if(jNew < 1 | jNew >= n) next
          # and disperse
          matSppB[iNew,jNew] <- 1
        }
      }
    }
  }
  # book keep sppA
  resArraySppA[,,k] <- matSppA
  occupiedSppA[k] <- sum(matSppA)/n^2
  # book keep sppB
  resArraySppB[,,k] <- matSppB
  occupiedSppB[k] <- sum(matSppB)/n^2
  
  tmp <- data.frame(which(resArraySppA[,,k]==1,arr.ind = TRUE),
                    iter=k)
  sppAdf <- rbind(sppAdf,tmp)
  
  tmp <- data.frame(which(resArraySppB[,,k]==1,arr.ind = TRUE),
                    iter=k)
  sppBdf <- rbind(sppBdf,tmp)
}
occupiedDF <- data.frame(iteration = 1:nIter,
                         A = occupiedSppA,
                         B = occupiedSppB) %>%
  pivot_longer(cols=-1, names_to = "Species", values_to = "pct")
ggplot(data = occupiedDF,
       mapping = aes(x=iteration,y=pct,color=Species)) +
  geom_line() +
  scale_color_manual(values = c("A" = "darkred",
                                "B" = "darkblue")) +
  labs(y="Percent landscape occupied", x = "Iteration") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()

names(sppAdf)[1:2] <- c("x","y")
sppAdf$spp <- "A"

names(sppBdf)[1:2] <- c("x","y")
sppBdf$spp <- "B"
head(sppBdf)
head(sppAdf)

pointSF <- bind_rows(sppAdf,sppBdf)
pointSF <- pointSF %>% st_as_sf(coords = c("x","y"))
pointSF <- pointSF %>% rename("Species" = "spp")

k <- 500
tmp <- pointSF %>% filter(iter==k)

p1 +
  new_scale_fill() + 
  new_scale_color() + 
  geom_sf(data=tmp,mapping = aes(fill=Species,shape=Species),
          alpha = 0.5,
          size=3) +
  scale_shape_manual(values = c("A" = 23,
                                "B" = 24)) +
  scale_fill_manual(values = c("A" = "darkred",
                               "B" = "darkblue")) +
  labs(subtitle = paste0("Iteration: ", k))




saveVideo(
  expr = {
    for(k in c(seq(50,nIter,by=1))){
      
      tmp <- pointSF %>% filter(iter==k)
      
      p2 <- p1 +
        new_scale_fill() + 
        new_scale_color() + 
        geom_sf(data=tmp,mapping = aes(fill=Species,shape=Species),
                color="white",
                alpha = 0.5,
                size=4) +
        scale_shape_manual(values = c("A" = 23,
                                      "B" = 24)) +
        scale_fill_manual(values = c("A" = "darkred",
                                     "B" = "darkblue"))
      
      print(p2 +
              labs(subtitle = paste0("Iteration: ", k)) +
              theme(plot.subtitle=element_text(size=15)))
    }
  }, 
  video.name = "sppSorting_SimpleCA.mp4", 
  interval = 0.2, ani.width = 600, ani.height = 600)


saveGIF(
  expr = {
    for(k in c(seq(50,nIter,by=1))){
      
      tmp <- pointSF %>% filter(iter==k)
      
      p2 <- p1 +
        new_scale_fill() + 
        new_scale_color() + 
        geom_sf(data=tmp,mapping = aes(fill=Species,shape=Species),
                color="white",
                alpha = 0.5,
                size=4) +
        scale_shape_manual(values = c("A" = 23,
                                      "B" = 24)) +
        scale_fill_manual(values = c("A" = "darkred",
                                     "B" = "darkblue")) +
        labs(subtitle = paste0("Iteration: ", k))
      
      print(p2 +
              theme(plot.subtitle=element_text(size=15)))
    }
  }, movie.name = "sppSorting_SimpleCA.gif", 
  interval = 0.2, ani.width = 600, ani.height = 600)
