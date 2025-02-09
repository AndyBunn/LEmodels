###
###
### AGB Simple CA model
### Feb 2025
###
###

## Make help in popups like treesong


#### libs ####
# install packages if needed
list_of_packages = c("shiny","shinyjs","truncnorm","tidyverse",
                     "scales","ambient","gganimate","cowplot","shinythemes",
                     "shinyWidgets","terra","tidyterra","sf","ggnewscale",
                     "prettyunits","gifski","shinyBS","markdown")

lapply(list_of_packages, 
       function(x) if(!require(x,character.only = TRUE)) install.packages(x))

library(shiny)
library(shinyjs)
library(shinyWidgets)
library(markdown)
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
library(shinythemes)
library(prettyunits)
library(gifski)
library(shinyBS)

addResourcePath(prefix = "www", directoryPath = "www")
addResourcePath(prefix = "videos", directoryPath = "www")


#### UI ####
ui <- fluidPage(
  useShinyjs(),
  theme = shinytheme("spacelab"),
  
  # for fancier progress bar
  tags$head(
    tags$style(
      HTML(".shiny-notification {
              background-color:#112446;
              color: white;
              font-size: 20px;
              font-weight: bold;
              height: 80px;
              width: 400px;
              position:fixed;
              top: calc(50% - 40px);;
              left: calc(50% - 200px);;
            }
           "
      )
    )
  ),
  # tags$style(type="text/css",
  #            ".recalculating {opacity: 1.0;}"
  # ),
  #### Modals for help ####
  
  bsModal(id = "aboutModal",
          title = "About",
          trigger = "about",
          size = "medium",
          includeMarkdown("text_intro.rmd")),
  
  bsModal(id = "animModal",
          title = "Animation",
          trigger = "animHelp",
          size = "medium",
          includeMarkdown("anim_help.rmd")),
  
  bsModal(id = "ptModal",
          title = "Adjust the Environmtenal Graient",
          trigger = "ptHelp",
          size = "medium",
          includeMarkdown("pt_help.rmd")),
  
  bsModal(id = "nicheModal",
          title = "Adjust the Niches",
          trigger = "nicheHelp",
          size = "medium",
          includeMarkdown("niche_help.rmd")),
  
  bsModal(id = "dispersalModal",
          title = "Adjust Dispersal",
          trigger = "dispersalHelp",
          size = "medium",
          includeMarkdown("demographics_help.rmd")),
  
  bsModal(id = "demographicsModal",
          title = "Adjust Demographics",
          trigger = "demographicsHelp",
          size = "medium",
          includeMarkdown("demographics_help.rmd")),
  
  bsModal(id = "runModal",
          title = "Run",
          trigger = "runHelp",
          size = "medium",
          includeMarkdown("run_help.rmd")),
  #### sidebar ####
  sidebarLayout(
    sidebarPanel(width = 4, 
                 div(style="display: inline-block;",tags$h4("Physical Template")),
                 actionButton(inputId = "ptHelp", label = NULL,icon=icon("circle-info"),style = "padding: 0"),
                 
                 
                 fluidRow(
                   sliderInput(inputId = "n", 
                               label="Number of Rows/Columns",
                               min = 50,
                               max = 200,
                               value = 100,
                               step = 50),
                   column(6, 
                          sliderInput(inputId = "noiseFreq",
                                      label="Noise Frequency",
                                      min = 0,
                                      max = 50,
                                      value = 5,
                                      step = 0.5)
                   ),
                   column(6,
                          sliderInput(inputId = "snr",
                                      label="Pct Noise",
                                      min = 0,
                                      max = 1,
                                      value = 0.5,
                                      step = 0.1)
                   )
                 ),
                 hr(style = "border-top: 1px solid #000000;"),
                 #### Niches ####
                 div(style="display: inline-block;",tags$h4("Habitat Suitability")),
                 actionButton(inputId = "nicheHelp", label = NULL,icon=icon("circle-info"),style = "padding: 0"),
                 
                 fluidRow(
                   column(6,
                          strong("Spp A"), 
                          sliderInput(inputId = "meanSppA",
                                      label="Mean",
                                      min = 0,
                                      max = 1,
                                      value = 0.1,
                                      step = 0.1),
                          sliderInput(inputId = "sdSppA",
                                      label="Std Dev",
                                      min = 0.05,
                                      max = 1,
                                      value = 0.3,
                                      step = 0.05)
                   ),
                   column(6,
                          strong("Spp B"),
                          sliderInput(inputId = "meanSppB",
                                      label="Mean",
                                      min = 0,
                                      max = 1,
                                      value = 0.8,
                                      step = 0.1),
                          sliderInput(inputId = "sdSppB",
                                      label="Std Dev",
                                      min = 0.05,
                                      max = 1,
                                      value = 0.3,
                                      step = 0.05)
                          
                   ),
                 ),
                 hr(style = "border-top: 1px solid #000000;"),
                 div(style="display: inline-block;",tags$h4("Neighborhood Dispersal")),
                 actionButton(inputId = "dispersalHelp", label = NULL,icon=icon("circle-info"),style = "padding: 0"),
                 fluidRow(
                   column(6, 
                          checkboxInput(inputId = "neighborHoodsppA",
                                        label = "Species A",
                                        value = TRUE)
                   ),
                   column(6, 
                          checkboxInput(inputId = "neighborHoodsppB",
                                        label = "Species B",
                                        value = TRUE)
                   )
                 ),
                 
                 hr(style = "border-top: 1px solid #000000;"),
                 div(style="display: inline-block;",tags$h4("Demographics")),
                 actionButton(inputId = "demographicsHelp", label = NULL,icon=icon("circle-info"),style = "padding: 0"),
                 fluidRow(
                   column(6, 
                          strong("Spp A"),
                          sliderInput(inputId = "pReproA",
                                      label="Prob Reproduction",
                                      min = 0,
                                      max = 0.1,
                                      value = 0.02,
                                      step = 0.001),
                          sliderInput(inputId = "pRanMortA",
                                      label="Prob Random Mortality",
                                      min = 0,
                                      max = 0.1,
                                      value = 0.01,
                                      step = 0.001)
                          
                   ),
                   column(6,
                          strong("Spp B"),
                          sliderInput(inputId = "pReproB",
                                      label="Prob Reproduction",
                                      min = 0,
                                      max = 0.1,
                                      value = 0.02,
                                      step = 0.001),
                          sliderInput(inputId = "pRanMortB",
                                      label="Prob Random Mortality",
                                      min = 0,
                                      max = 0.1,
                                      value = 0.01,
                                      step = 0.001)
                   )
                 ),
                 hr(style = "border-top: 1px solid #000000;"),
                 #### Run Model ####
                 div(style="display: inline-block;",tags$h4("Run")),
                 actionButton(inputId = "runHelp", label = NULL,icon=icon("circle-info"),style = "padding: 0"),
                 sliderInput(inputId = "nIter",
                             label="Iterations",
                             min = 10,
                             max = 200,
                             value = 100,
                             step = 10),
                 actionButton(inputId = "goButton",label = "Run Model"),
                 p(""),
                 actionButton(inputId = "resetParams",label = "Reset Default Parameters"),
                 hr(style = "border-top: 1px solid #000000;"),
                 #### Gen anim ####
                 conditionalPanel(
                   condition = "output.show_results",
                   div(style="display: inline-block;",tags$h4("Animation ")),
                   actionButton(inputId = "animHelp", label = NULL,icon=icon("circle-info"),style = "padding: 0"),
                   uiOutput("getAnimInterval"),
                   uiOutput("getDuration"),
                   actionButton(inputId = "makeAnim",label = "Make Animation")
                 ) # end conditionalPanel
    ), #end sidebarPanel
    #### main panel ####
    mainPanel(
      # spacing bugs me
      # div(style="display: inline-block;",tags$h2("Simple Species Sorting ")),
      h2("Simple Species Sorting "),
      actionButton(inputId = "about", label = "About",
                   icon = icon("circle-info")),
      
      #### Phys temp ####
      fluidRow(
        column(1),
        column(10,
               plotOutput(outputId = "plotPhysTemp")),
        column(1)
      ),
      fluidRow(
        column(1),
        column(10,
               plotOutput(outputId = "plotSppNiches")),
        column(1)
      ),
      #### Results ####
      hr(style = "border-top: 1px solid #000000;"),
      conditionalPanel(
        condition = "output.show_results",
        #### Model has run
        fluidRow(
          plotOutput(outputId = "modelThreePlot")
        ),
        fluidRow(
          column(4),
          column(4,
                 uiOutput("iter2plotSlider")
          ),
          column(4)
        ),
        
        # add cond
        conditionalPanel(
          condition = "output.show_animDownload",
          hr(style = "border-top: 1px solid #000000;"),
          fluidRow(align = 'center',
                   downloadButton("downloadAnim", "Download GIF"),
                   imageOutput("previewAnim")
          )
        ) #downloadbutton condition end
      ) # results condition end
    ), #end mainPanel
  ) # end sidebarLayout
)

#### Server ####
server <- function(input, output, session) {
  #### RVs ####
  rv <- reactiveValues()
  animation_file <- reactiveVal(NULL)
  
  getEnvParams <- reactive({
    
    rv$n <- input$n
    rv$trendSteepness <- 1 # makes more non linear 1.75
    rv$noiseFreq <- input$noiseFreq
    rv$snr <- input$snr
    rv$nCells <- rv$n^2
    rv$y_coords <- seq(1, rv$n, by= 1)
    rv$x_coords <- seq(1, rv$n, by= 1)
  })
  
  getSppParams <- reactive({
    
    rv$neighborHoodsppA <- input$neighborHoodsppA
    rv$neighborHoodsppB <- input$neighborHoodsppB
    # mean and std of mort functions for spp
    rv$meanSppA <- input$meanSppA
    rv$sdSppA <- input$sdSppA
    
    rv$meanSppB <- input$meanSppB
    rv$sdSppB <- input$sdSppB
    
    # Reproduction p
    rv$pReproA <- input$pReproA
    rv$pReproB <- input$pReproB
    
    # Random mortality
    rv$pRanMortA <- input$pRanMortA
    rv$pRanMortB <- input$pRanMortB
    
  })
  
  getOtherParams <- reactive({
    # how many iterations to use
    rv$nIter <- input$nIter
    
    # pctSeed
    rv$pctSeedA <- input$pReproB * 2
    rv$pctSeedB <- input$pReproB * 2
    
  })
  
  #### Conditional hide/reset ####
  # This is the server logic to show or hide the model results.
  # Show if model is run.
  # Hide if input sliders change.
  
  model_ran <- reactiveVal(FALSE)  # Initial state is FALSE
  
  output$show_results <- reactive({
    model_ran()  # Must return TRUE/FALSE
  })
  
  outputOptions(output, "show_results", suspendWhenHidden = FALSE)  # Ensures reactivity
  
  observeEvent(input$goButton, {
    model_ran(TRUE)  # Set to TRUE when button is clicked
  })
  
  # do same for anim download button -- want anim made first
  anim_made <- reactiveVal(FALSE)  # Initial state is FALSE
  
  output$show_animDownload <- reactive({
    anim_made()  # Must return TRUE/FALSE
  })
  
  outputOptions(output, "show_animDownload", suspendWhenHidden = FALSE)  # Ensures reactivity
  
  # hide results when inputs change
  observeEvent(list(input$n,
                    input$noiseFreq, input$snr, 
                    input$meanSppA, input$sdSppA, 
                    input$meanSppB, input$sdSppB, 
                    input$neighborHoodsppA,
                    input$neighborHoodsppB,
                    input$pReproA, input$pRanMortA, 
                    input$pReproB, input$pRanMortB,
                    input$nIter), 
               {model_ran(FALSE)
                 anim_made(FALSE)}
  )  
  
  # hide anim when inputs change
  observeEvent(list(input$animInterval,input$tSec), 
               {anim_made(FALSE)}
  )  
  # reset params
  observeEvent(input$resetParams, {
    updateSliderInput(session, "n", value = 100)
    updateSliderInput(session, "noiseFreq", value = 5)
    updateSliderInput(session, "snr", value = 0.5)
    updateSliderInput(session, "meanSppA", value = 0.1)
    updateSliderInput(session, "meanSppB", value = 0.8)
    updateSliderInput(session, "sdSppA", value = 0.3)
    updateSliderInput(session, "sdSppB", value = 0.3)
    updateSliderInput(session, "neighborHoodsppA", value = TRUE)
    updateSliderInput(session, "neighborHoodsppB", value = TRUE)
    updateSliderInput(session, "pReproA", value = 0.02)
    updateSliderInput(session, "pReproB", value = 0.02)
    updateSliderInput(session, "pRanMortA", value = 0.01)
    updateSliderInput(session, "pRanMortA", value = 0.01)
    updateSliderInput(session, "nIter", value = 100)
  })
  
  #### plotPhysTemp ####
  
  output$plotPhysTemp <- renderPlot({
    req(getEnvParams())
    
    ## The phys template will have a gradient and noise
    # Create a grid of coordinates for the trend surface
    trendSurfaceDF <- expand.grid(x = rv$x_coords, y = rv$y_coords)
    # Create a nonlinear trend along the diagonal
    # The trend increases as x + y increases
    trendSurfaceDF$z <- (trendSurfaceDF$x + trendSurfaceDF$y)^rv$trendSteepness
    # Scale [0,1]
    trendSurfaceDF$z <- scales::rescale(trendSurfaceDF$z, 
                                        to = c(0, 1))
    
    # Make some Perlin noise. Note freq argument
    noiseSurfaceDF <- trendSurfaceDF[,1:2]
    noiseSurfaceDF$z <- gen_perlin(x = scales::rescale(noiseSurfaceDF$x,to = c(0,1)),
                                   y = scales::rescale(noiseSurfaceDF$y,to = c(0,1)),
                                   frequency = rv$noiseFreq)
    # Scale [0,1]
    noiseSurfaceDF$z <- scales::rescale(noiseSurfaceDF$z,
                                        from = range(noiseSurfaceDF$z),
                                        to = c(0,1))
    # Combining trend and noise
    physTempSurfaceDF <- trendSurfaceDF
    # weight trend and noise
    physTempSurfaceDF$z <- noiseSurfaceDF$z * rv$snr + physTempSurfaceDF$z * (1-rv$snr)
    # Scale [0,1]
    physTempSurfaceDF$z <- scales::rescale(physTempSurfaceDF$z,
                                           to = c(0,1))
    # save to rv
    rv$physTempSurfaceDF <- physTempSurfaceDF
    # make a raster to use in plotting
    rv$physTempRast <- rast(physTempSurfaceDF)
    
    # This is the base plot to be used as we go forward
    plotPhysTemp <- ggplot() +
      geom_spatraster(data=rv$physTempRast) +
      scale_fill_terrain_c(name = "Env gradient") +
      new_scale_fill() +
      geom_spatraster_contour_filled(data=rv$physTempRast,
                                     color="white",
                                     alpha=0.1,
                                     breaks=seq(0,1,by=0.2)) +
      scale_fill_terrain_d(guide = "none") +
      coord_sf(default = TRUE) +
      theme_void()
    
    plotPhysTemp
    
  })
  #### plotSppNiches ####
  output$plotSppNiches <- renderPlot({
    req(getSppParams(),getEnvParams())
    nicheCurveSppA <- data.frame(z = seq(0, 1, length.out = 100))
    nicheCurveSppA$p <- dtruncnorm(nicheCurveSppA$z, 
                                   a = 0, b = 1, 
                                   mean = rv$meanSppA, sd = rv$sdSppA)
    nicheCurveSppA$p <- scales::rescale(nicheCurveSppA$p,
                                        to = c(0,1))
    
    # make surface
    nicheSurfaceSppA <- rv$physTempSurfaceDF[,1:2]
    nicheSurfaceSppA$p <- dtruncnorm(rv$physTempSurfaceDF$z, 
                                     a = 0, b = 1, 
                                     mean = rv$meanSppA, sd = rv$sdSppA)
    nicheSurfaceSppA$p <- scales::rescale(nicheSurfaceSppA$p,
                                          to = c(0,1))
    
    rv$nicheRastSppA <- rast(nicheSurfaceSppA)
    
    nicheCurveSppB <- data.frame(z = seq(0, 1, length.out = 100))
    nicheCurveSppB$p <- dtruncnorm(nicheCurveSppB$z, 
                                   a = 0, b = 1, 
                                   mean = rv$meanSppB, sd = rv$sdSppB)
    nicheCurveSppB$p <- scales::rescale(nicheCurveSppB$p,
                                        to = c(0,1))
    
    # make surface
    nicheSurfaceSppB <- rv$physTempSurfaceDF[,1:2]
    nicheSurfaceSppB$p <- dtruncnorm(rv$physTempSurfaceDF$z, 
                                     a = 0, b = 1, 
                                     mean = rv$meanSppB, sd = rv$sdSppB)
    nicheSurfaceSppB$p <- scales::rescale(nicheSurfaceSppB$p,
                                          to = c(0,1))
    rv$nicheRastSppB <- rast(nicheSurfaceSppB)
    # save to rv
    rv$nicheSurfaceSppA <- nicheSurfaceSppA
    rv$nicheSurfaceSppB <- nicheSurfaceSppB
    
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
    
  })
  
  #### run model ####
  runModel <- eventReactive(input$goButton, {
    withProgress({
      #####################################
      #
      ## Parameters
      #
      #####################################
      getEnvParams()
      getSppParams()
      getOtherParams()
      
      physTempSurfaceDF <- rv$physTempSurfaceDF
      nicheSurfaceSppA <- rv$nicheSurfaceSppA
      nicheSurfaceSppB <- rv$nicheSurfaceSppB
      
      n <- rv$n
      nIter <- rv$nIter
      nCells <- n^2
      pctSeedA <- rv$pctSeedA
      pctSeedB <- rv$pctSeedB
      neighborHoodsppA <- rv$neighborHoodsppA
      neighborHoodsppB <- rv$neighborHoodsppB
      pRanMortA <- rv$pRanMortA
      pRanMortB <- rv$pRanMortB
      pReproA <- rv$pReproA
      pReproB <- rv$pReproB
      
      #### Set up for run ####
      # Use a matrix for the loop and not the raster object. This is
      # 100s of times faster
      physTempMat <- matrix(physTempSurfaceDF$z,nrow=n,ncol=n)
      nicheMatSppA <- matrix(nicheSurfaceSppA$p,nrow=n,ncol=n)
      nicheMatSppB <- matrix(nicheSurfaceSppB$p,nrow=n,ncol=n)
      
      ## Simulate
      # 0 = dead
      # 1 = living (subject to random mort)
      # 2 = newborn (subject to mort func)
      
      # Stuff for sppA
      # Here is the raster for sppA pres/abs
      matSppA <- matrix(0,ncol=n,nrow=n)
      cells2seedSppA <- sample(x = 1:nCells,
                               size = nCells * pctSeedA,
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
                               size = nCells * pctSeedB,
                               replace = FALSE)
      matSppB[cells2seedSppB] <- 2
      
      # Structures for outputs
      resArraySppB <- array(0,dim=c(n,n,nIter))
      occupiedSppB <- numeric()
      sppBdf <- data.frame(row=numeric(),col=numeric(),iteration=numeric())
      #### end ####
      
      #### Start sim ####
      
      # save iteration 0
      sppAdf <- data.frame(which(matSppA>0,arr.ind = TRUE),
                        iteration=0)
      sppBdf <- data.frame(which(matSppB>0,arr.ind = TRUE),
                           iteration=0)
      
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
                if(neighborHoodsppA){
                  # pick a neighbor to disperse to. Queen's rules
                  iNew <- i + sample(c(-1,0,1),1)
                  jNew <- j + sample(c(-1,0,1),1)
                  # check to make sure we aren't off edge of map
                  if(iNew < 1 | iNew >= n) next
                  if(jNew < 1 | jNew >= n) next
                  # and disperse
                  matSppA[iNew,jNew] <- 2
                }
                else{
                  iNew <- sample(1:n,1)
                  jNew <- sample(1:n,1)
                  matSppA[iNew,jNew] <- 2
                }
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
                if(neighborHoodsppB){
                  # pick a neighbor to disperse to. Queen's rules
                  iNew <- i + sample(c(-1,0,1),1)
                  jNew <- j + sample(c(-1,0,1),1)
                  # check to make sure we aren't off edge of map
                  if(iNew < 1 | iNew >= n) next
                  if(jNew < 1 | jNew >= n) next
                  # and disperse
                  matSppB[iNew,jNew] <- 2
                }
                else{
                  iNew <- sample(1:n,1)
                  jNew <- sample(1:n,1)
                  matSppB[iNew,jNew] <- 2
                }
              }
            }
          }
        }
        # book keeping after demographics this iteration.
        resArraySppA[,,k] <- matSppA
        occupiedSppA[k] <- sum(matSppA==1)/nCells # just values of one not all?
        
        #guard against TPK
        if(any(resArraySppA[,,k]>0)){
          tmp <- data.frame(which(resArraySppA[,,k]>0,arr.ind = TRUE),
                            iteration=k)
          sppAdf <- rbind(sppAdf,tmp)
        }
        
        resArraySppB[,,k] <- matSppB
        occupiedSppB[k] <- sum(matSppB==1)/nCells
        #guard against TPK
        if(any(resArraySppB[,,k]>0)){
          tmp <- data.frame(which(resArraySppB[,,k]>0,arr.ind = TRUE),
                            iteration=k)
          sppBdf <- rbind(sppBdf,tmp)
        }
        
        incProgress(1/nIter)
      }
      #### end ####
      
      #### Collect and wrangle output ####
      # make a spp abbundance df
      occupiedDF <- data.frame(iteration = 1:nIter,
                               A = occupiedSppA,
                               B = occupiedSppB) %>%
        pivot_longer(cols=-1, names_to = "Species", values_to = "pct")
      
      # make sf object for spp by iteration
      names(sppAdf)[1:2] <- c("x","y")
      sppAdf$spp <- "A"
      names(sppBdf)[1:2] <- c("x","y")
      sppBdf$spp <- "B"
      
      pointSF <- bind_rows(sppAdf,sppBdf)
      pointSF <- pointSF %>% st_as_sf(coords = c("x","y"))
      pointSF <- pointSF %>% rename("Species" = "spp")
      pointSF <- pointSF %>% mutate(z = terra::extract(rv$physTempRast,pointSF)$z)
      #### end ####
      
      rv$pointSF <- pointSF
      rv$occupiedDF <- occupiedDF
      
      rv$modelHasRun <- TRUE
    }, message = 'Model Go Brrrr...', value=0,)
  })
  
  #### Output plot ####
  output$iter2plotSlider <- renderUI({
    req(getOtherParams())
    sliderInput(inputId = "iter2plot",
                label="Iteration to plot",
                min = 0,
                max = rv$nIter,
                value = rv$nIter,
                step = 1)
  })
  
  output$modelThreePlot <- renderPlot({
    req(input$goButton)
    runModel()
    
    pointSF <- rv$pointSF
    occupiedDF <- rv$occupiedDF
    physTempRast <- rv$physTempRast
    
    # input
    k <- input$iter2plot
    
    plotPhysTemp <- ggplot() +
      geom_spatraster(data=physTempRast) +
      scale_fill_terrain_c(name = "Env gradient") +
      new_scale_fill() +
      geom_spatraster_contour_filled(data=physTempRast,
                                     color="white",
                                     alpha=0.1,
                                     breaks=seq(0,1,by=0.2)) +
      scale_fill_terrain_d(guide = "none") +
      coord_sf(default = TRUE) +
      theme_void()
    
    #### Set up plots ####
    # plots by iteration
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
      dplyr::filter(n() > 1) %>% #guard against TPK
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
    
    
    # toprow <- plot_grid(plotIterationSppAbundance, plotIterationNiche,
    #                     nrow = 1,
    #                     ncol = 2)
    # bottomrow <- plot_grid(plotIterationMap)
    leftCol <- plot_grid(plotIterationSppAbundance, plotIterationNiche,
                         nrow = 2,
                         ncol = 1)
    rightCol <- plot_grid(plotIterationMap)
    
    plot_grid(leftCol,rightCol,nrow=1)
    #### end ####
    
  })
  #### Make anim ####
  # see here for mp4: https://stackoverflow.com/questions/53241425/save-gganimate-animation-as-mp4-file
  # to do this I think you want to filter the point data according to animSeq with reduce file size.
  # and is there a way to make the background static or something? Some way to improve speed.
  # and change to download and not display.
  output$getAnimInterval <- renderUI({
    req(getOtherParams())
    tmp <- seq(0,rv$nIter/2,by=5)
    tmp[1] <- 1
    shinyWidgets::sliderTextInput(inputId = "animInterval",
                                  label="Interval",
                                  choices = tmp,
                                  selected = tmp[2])
  })
  
  output$getDuration <- renderUI({
    req(getOtherParams())
    sliderInput(inputId = "tSec",
                label="Duration (sec)",
                min = 10,max = 60,value = 10,step = 5)
    
  })
  
  observeEvent(input$makeAnim, {
    
    outfile <- tempfile(fileext='.gif')
    #    runModel()
    temp_file <- "www/anim.gif"
    if(file.exists(temp_file)){
      file.remove("www/anim.gif")
    }
    animation_file(temp_file)
    
    pointSF <- rv$pointSF
    physTempRast <- rv$physTempRast
    
    animSeq <- c(0,1,seq(input$animInterval,rv$nIter,by=input$animInterval))
    # make sure it ends on last iter
    if(animSeq[length(animSeq)] != rv$nIter){
      animSeq <- c(animSeq,rv$nIter)
    } 
    
    print(animSeq)
    filteredPointSF <- pointSF %>% dplyr::filter(iteration %in% animSeq)
    
    # See here: https://github.com/thomasp85/gganimate/pull/331
    # for me bar doesn't increment. b/c update_progress PR not merged?
    
    # as per https://shiny.rstudio.com/articles/progress.html#a-more-complex-progress-example
    # but set max value to pre-determined total frame count
    progress <- shiny::Progress$new(max = length(animSeq))
    progress$set(message = "Rendering Go Brrrr...", value = 0)
    on.exit(progress$close())
    
    updateShinyProgress <- function(detail) {    
      progress$inc(1, detail = detail)
    }
    
    # input
    plotPhysTemp <- ggplot() +
      geom_spatraster(data=physTempRast) +
      scale_fill_terrain_c(name = "Env gradient") +
      new_scale_fill() +
      geom_spatraster_contour_filled(data=physTempRast,
                                     color="white",
                                     alpha=0.1,
                                     breaks=seq(0,1,by=0.2)) +
      scale_fill_terrain_d(guide = "none") +
      coord_sf(default = TRUE) +
      theme_void()
    
    plotIterationMap <- plotPhysTemp +
      new_scale_fill() +
      new_scale_color() +
      geom_sf(data=filteredPointSF,mapping = aes(fill=Species,shape=Species),
              alpha = 0.5,
              size=3) +
      scale_shape_manual(values = c("A" = 23,
                                    "B" = 24)) +
      scale_fill_manual(values = c("A" = "darkred",
                                   "B" = "darkblue")) +
      theme(plot.caption = element_text(size = 12))
    
    pAnim <- plotIterationMap + 
      transition_states(iteration) + 
      labs(caption = "Iteration: {closest_state}") +
      enter_appear() +
      exit_disappear()
    
    
    theAmin <- gganimate::animate(
      plot = pAnim,
      nframes = length(animSeq),
      duration = input$tSec,
      height = 600,
      width = 600,
      units = "px",
      renderer = gifski_renderer(),
      update_progress = updateShinyProgress #update_progress doesn't exist see PR above
    )
    
    anim_save(filename = animation_file(),animation = theAmin)
    
    output$previewAnim <- renderImage({
      list(src = animation_file(), contentType = "image/gif")
    }, deleteFile = FALSE)
    
    anim_made(TRUE)  # Set to TRUE when gif is made, shows download button
    
    
  })
  output$downloadAnim <- downloadHandler(
    filename = "animation.gif",
    content = function(file) {
      req(animation_file())
      file.copy(from = animation_file(), to = file, overwrite = TRUE)
    }
  )
}
# Run the application
shinyApp(ui = ui, server = server)
