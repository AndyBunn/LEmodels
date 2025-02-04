###################################################################################
# Make a simple cellular automata to show how dispersal causes pattern
# from a random start

## Jan 25 2025 -- added 2nd spp and a good phys temp with custom mort funcs
## next: add switches to do neighborhood x phys temp to get
## 4x4 pattern a la Urban's sierra example.
## Add in competition? Right now spp can occupy same cell

rm(list=ls())
#### Libs #### 
library(truncnorm)
library(tidyverse)
library(scales)
library(ambient)
library(gganimate)
library(terra)
library(tidyterra)
library(sf)
library(ggnewscale)
library(cowplot)
#### end #### 


#### Params ####
## Set up global dimensions for landscape
n <- 100

trendSteepness <- 1.75
noiseFreq <- 5
snr <- 0.5

# mean and std of mort functions for spp
meanSppA <- 0.9
sdSppA <- 0.2

meanSppB <- 0.01
sdSppB <- 0.2

# Randomly seed some % of cells with presence vs absence
pctSeed <- 0.05

# Reproduction p
pReproA <- 0.02
pReproB <- 0.02

# Random mortality
pRanMortA <- 0.01
pRanMortB <- 0.01

# how many iterations to use
nIter <- 100

# animation interval 
animInterval <- 5

## Some variables
animSeq <- seq(1,nIter,by=animInterval)
nCells <- n^2
y_coords <- x_coords <- seq(1, n, by= 1)

#### end ####

#### Make surfaces ####
## The phys template will have a gradient and noise
# Create a grid of coordinates for the trend surface
trendSurfaceDF <- expand.grid(x = x_coords, y = y_coords)
# Create a nonlinear trend along the diagonal
# The trend increases as x + y increases
trendSurfaceDF$z <- (trendSurfaceDF$x + trendSurfaceDF$y)^trendSteepness
# Scale [0,1]
trendSurfaceDF$z <- scales::rescale(trendSurfaceDF$z, 
                                    to = c(0, 1))

# Make some Perlin noise. Note freq argument
noiseSurfaceDF <- trendSurfaceDF[,1:2]
noiseSurfaceDF$z <- gen_perlin(x = scales::rescale(noiseSurfaceDF$x,to = c(0,1)),
                               y = scales::rescale(noiseSurfaceDF$y,to = c(0,1)),
                               frequency = noiseFreq)
# Scale [0,1]
noiseSurfaceDF$z <- scales::rescale(noiseSurfaceDF$z, 
                                    from = range(noiseSurfaceDF$z), 
                                    to = c(0,1))
# Combining trend and noise
physTempSurfaceDF <- trendSurfaceDF
# weight trend and noise
physTempSurfaceDF$z <- noiseSurfaceDF$z * snr + physTempSurfaceDF$z * (1-snr)
# Scale [0,1]
physTempSurfaceDF$z <- scales::rescale(physTempSurfaceDF$z,
                                       to = c(0,1))

# make a raster to use in plotting
physTempRast <- rast(physTempSurfaceDF)

# This is the base plot to be used as we go forward
plotPhysTemp <- ggplot() + 
  geom_spatraster(data=physTempRast) +
  scale_fill_terrain_c(name = "Env gradient", guide="none") +
  new_scale_fill() +
  geom_spatraster_contour_filled(data=physTempRast,
                                 color="white",
                                 alpha=0.1,
                                 breaks=seq(0,1,by=0.2)) +
  scale_fill_terrain_d(guide = "none") +
  coord_sf(default = TRUE) +
  theme_void()

zDens <- density(physTempSurfaceDF$z, n = 2^12)
zDensDF <- data.frame(x=zDens$x,y=zDens$y) 
# rescale to line up?
zDensDF$y <- scales::rescale(zDensDF$y,to = c(0,n))
plotPhyTempDens <- ggplot(data = zDensDF,aes(x, y)) +  
  geom_segment(aes(xend = x, yend = 0, color = x)) + 
  scale_color_terrain_c(name = "Env gradient") +
  #  geom_line() +
  labs(x = "Env gradient",y="Density") +
  lims(x=c(0,1)) +
  theme_minimal() +
  guides(color="none")  +
  theme(
    axis.title.y = element_blank(), 
    axis.text.y = element_blank(),  
    axis.ticks.y = element_blank(), 
    axis.line.y = element_blank(),  
    panel.grid = element_blank()
  )
plot_grid(plotPhysTemp,plotPhyTempDens,nrow = 1)

# meh. see here to get silly fancy
# https://dieghernan.github.io/202212_tidyterra-hillshade-2/

#### end ####

#### Make niches ####

## With the physical template done, let's make niches for the spp. 
nicheCurveSppA <- data.frame(z = seq(0, 1, length.out = 100))
nicheCurveSppA$p <- dtruncnorm(nicheCurveSppA$z, 
                               a = 0, b = 1, 
                               mean = meanSppA, sd = sdSppA)
nicheCurveSppA$p <- scales::rescale(nicheCurveSppA$p,
                                    to = c(0,1))

plotNicheFuncSppA <- ggplot(nicheCurveSppA) + geom_line(aes(x=z,y=p)) +
  labs(y="Suitability",x="Env gradient", title = "SppA") +
  theme_minimal()

plotNicheFuncSppA

# make surface
nicheSurfaceSppA <- physTempSurfaceDF[,1:2]
nicheSurfaceSppA$p <- dtruncnorm(physTempSurfaceDF$z, 
                                 a = 0, b = 1, 
                                 mean = meanSppA, sd = sdSppA)
nicheSurfaceSppA$p <- scales::rescale(nicheSurfaceSppA$p,
                                      to = c(0,1))

nicheRastSppA <- rast(nicheSurfaceSppA)

plotNicheSppA <- ggplot() + 
  geom_spatraster(data=nicheRastSppA) +
  scale_fill_terrain_c(name = "Suitability") +
  new_scale_fill() +
  geom_spatraster_contour_filled(data=nicheRastSppA,
                                 color="white",
                                 alpha=0.1,
                                 breaks=seq(0,1,by=0.2)) +
  scale_fill_terrain_d(guide = "none") +
  coord_sf(default = TRUE) +
  labs(title = "SppA niche") +
  theme_void()

plotNicheSppA

nicheCurveSppB <- data.frame(z = seq(0, 1, length.out = 100))
nicheCurveSppB$p <- dtruncnorm(nicheCurveSppB$z, 
                               a = 0, b = 1, 
                               mean = meanSppB, sd = sdSppB)
nicheCurveSppB$p <- scales::rescale(nicheCurveSppB$p,
                                    to = c(0,1))

plotNicheFuncSppB <- ggplot(nicheCurveSppB) + geom_line(aes(x=z,y=p)) +
  labs(y="Suitability",x="Env gradient", title = "SppB") +
  theme_minimal()

plotNicheFuncSppB

# make surface
nicheSurfaceSppB <- physTempSurfaceDF[,1:2]
nicheSurfaceSppB$p <- dtruncnorm(physTempSurfaceDF$z, 
                                 a = 0, b = 1, 
                                 mean = meanSppB, sd = sdSppB)
nicheSurfaceSppB$p <- scales::rescale(nicheSurfaceSppB$p,
                                      to = c(0,1))

nicheRastSppB <- rast(nicheSurfaceSppB)

plotNicheSppB <- ggplot() + 
  geom_spatraster(data=nicheRastSppB) +
  scale_fill_terrain_c(name = "Suitability") +
  new_scale_fill() +
  geom_spatraster_contour_filled(data=nicheRastSppB,
                                 color="white",
                                 alpha=0.1,
                                 breaks=seq(0,1,by=0.2)) +
  scale_fill_terrain_d(guide = "none") +
  coord_sf(default = TRUE) +
  labs(title = "SppB niche") +
  theme_void()

plotNicheSppB


# make a plot of both niche functions
nicheCurves <- bind_rows(nicheCurveSppA %>% mutate(Species = "A"),
                         nicheCurveSppB %>% mutate(Species = "B"))

plotNicheFuncs <- ggplot(nicheCurves) + 
  geom_ribbon(aes(x=z,ymax=p,ymin=0,fill=Species),alpha=0.5) +
  scale_fill_manual(values = c("A" = "darkred",
                               "B" = "darkblue")) +
  labs(y="Suitability",x="Env gradient") +
  theme_minimal()
plotNicheFuncs

#### end ####

#### Set up for run ####
# Use a matrix for the loop and not the raster object. This is
# 100s of times faster
physTempMat <- matrix(physTempSurfaceDF$z,nrow=n,ncol=n)
nicheMatSppA <- matrix(nicheSurfaceSppA$p,nrow=n,ncol=n)
nicheMatSppB <- matrix(nicheSurfaceSppB$p,nrow=n,ncol=n)
#image(physTempMat,asp=1) # orientation looks OK

## Simulate
# 0 = dead
# 1 = living (subject to random mort)
# 2 = newborn (subject to mort func)

# Stuff for sppA
# Here is the raster for sppA pres/abs
matSppA <- matrix(0,ncol=n,nrow=n)
cells2seedSppA <- sample(x = 1:nCells, 
                         size = nCells * pctSeed,
                         replace = FALSE)
matSppA[cells2seedSppA] <- 2

# Structures for outputs
resArraySppA <- array(0,dim=c(n,n,nIter))
occupiedSppA <- numeric()
sppAdf <- data.frame(row=numeric(),col=numeric(),iteration=numeric())

# Stuff for sppB
# Here is the raster for sppB pres/abs
matSppB <- matrix(0,ncol=n,nrow=n)
cells2seedSppB <- sample(x = 1:nCells, 
                         size = nCells * pctSeed,
                         replace = FALSE)
matSppB[cells2seedSppB] <- 2

# Structures for outputs
resArraySppB <- array(0,dim=c(n,n,nIter))
occupiedSppB <- numeric()
sppBdf <- data.frame(row=numeric(),col=numeric(),iteration=numeric())
#### end ####

#### Start sim ####
for(k in 1:nIter){
  for(i in 1:n){
    for(j in 1:n){
      # do sppA
      reaper <- runif(n = 1, min = 0, max = 1)
      if(matSppA[i,j] == 2){
        # function mort
        matSppA[i,j] <- ifelse(reaper > nicheMatSppA[i,j],0,1) 
      }
      reaper <- runif(n = 1, min = 0, max = 1)
      if(matSppA[i,j] == 1){
        # ran mort on established cells only
        if(reaper < pRanMortA) {
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
          matSppA[iNew,jNew] <- 2
        }
      }
      # do sppB
      reaper <- runif(n = 1, min = 0, max = 1)
      if(matSppB[i,j] == 2){
        # function mort
        matSppB[i,j] <- ifelse(reaper > nicheMatSppB[i,j],0,1) 
      }
      reaper <- runif(n = 1, min = 0, max = 1)
      if(matSppB[i,j] == 1){
        # ran mort on established cells only
        if(reaper < pRanMortB) {
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
          matSppB[iNew,jNew] <- 2
        }
      }
    }
  }
    # book keeping after demographics this iteration
    resArraySppA[,,k] <- matSppA
    occupiedSppA[k] <- sum(matSppA==1)/nCells # just values of one not all? Or 
    tmp <- data.frame(which(resArraySppA[,,k]>0,arr.ind = TRUE),
                      iteration=k)
    sppAdf <- rbind(sppAdf,tmp)
    
    resArraySppB[,,k] <- matSppB
    occupiedSppB[k] <- sum(matSppB==1)/nCells
    tmp <- data.frame(which(resArraySppB[,,k]>0,arr.ind = TRUE),
                      iteration=k)
    sppBdf <- rbind(sppBdf,tmp)
  
}
#### end ####

#### Collect and wrangle output ####
# make a spp abbundance df
occupiedDF <- data.frame(iteration = 1:nIter,
                         A = occupiedSppA,
                         B = occupiedSppB) %>%
  pivot_longer(cols=-1, names_to = "Species", values_to = "pct")
occupiedDF %>% ggplot() + geom_line(aes(x=iteration,y=pct,color=Species))

# make sf object for spp by iteration
names(sppAdf)[1:2] <- c("x","y")
sppAdf$spp <- "A"
names(sppBdf)[1:2] <- c("x","y")
sppBdf$spp <- "B"

pointSF <- bind_rows(sppAdf,sppBdf)
pointSF <- pointSF %>% st_as_sf(coords = c("x","y"))
pointSF <- pointSF %>% rename("Species" = "spp")
pointSF <- pointSF %>% mutate(z = terra::extract(physTempRast,pointSF)$z)
#### end ####

#### Set up plots ####
# plots by iteration
k <- nIter
tmp <- pointSF %>% filter(iteration==k)

plotIterationMap <- plotPhysTemp +
  new_scale_fill() + 
  new_scale_color() + 
  geom_sf(data=tmp,mapping = aes(fill=Species,shape=Species),
          alpha = 0.5,
          size=3) +
  scale_shape_manual(values = c("A" = 23,
                                "B" = 24)) +
  scale_fill_manual(values = c("A" = "darkred",
                               "B" = "darkblue")) +
  labs(caption = paste0("Iteration: ", k)) +
  theme(plot.caption = element_text(size = 12)) 

# and get max density for niche so plots are comparable. yuck.
max_density_values <- pointSF %>%
  group_by(Species, iteration) %>%
  summarise(max_density = max(density(z)$y), .groups = "drop")

maxD <- max(max_density_values$max_density)


plotIterationNiche <- ggplot(data = tmp) +
  geom_density(aes(x=z,fill=Species),alpha=0.5) +
  lims(x = c(0,1),y=c(0,maxD)) +
  scale_fill_manual(values = c("A" = "darkred",
                               "B" = "darkblue")) +
  labs(x = "Env gradient",y=element_blank(),title = "Density") +
  theme_minimal()

tmp2 <- occupiedDF %>% filter(iteration == k)
plotIterationSppAbundance <- ggplot(data = occupiedDF,
                                    mapping = aes(x=iteration,y=pct,color=Species)) +
  geom_line() +
  scale_color_manual(values = c("A" = "darkred",
                                "B" = "darkblue")) +
  geom_point(data = tmp2, 
             mapping = aes(x=iteration,y=pct,
                           fill=Species,shape = Species),
             size = 3, alpha=0.5) +
  scale_shape_manual(values = c("A" = 23,
                                "B" = 24)) +
  scale_fill_manual(values = c("A" = "darkred",
                               "B" = "darkblue")) +
  labs(title="Percent landscape occupied", y = element_blank(), x = "Iteration") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()

# let's merge -- clean up guides
plotIterationSppAbundance <- plotIterationSppAbundance +
  guides(fill="none",color="none", shape = "none")

plotIterationNiche <- plotIterationNiche + guides(fill = "none")


leftCol <- plot_grid(plotIterationSppAbundance, plotIterationNiche,
                     nrow = 2,
                     ncol = 1)
rightCol <- plot_grid(plotIterationMap)

plot_grid(leftCol,rightCol,nrow=1)
#### end ####

#### Anim cheap
# plots by iteration

plotIterationMap <- plotPhysTemp +
  new_scale_fill() + 
  new_scale_color() + 
  geom_sf(data=pointSF,mapping = aes(fill=Species,shape=Species),
          alpha = 0.5,
          size=3) +
  scale_shape_manual(values = c("A" = 23,
                                "B" = 24)) +
  scale_fill_manual(values = c("A" = "darkred",
                               "B" = "darkblue")) +
  theme(plot.caption = element_text(size = 12)) 

anim1 <- plotIterationMap + transition_time(iteration) + labs(caption = "Iteration: {frame_time}")
anim_save(filename = "anim.gif", animation = anim1, nframes = nIter, duration = 10)
