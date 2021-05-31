#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinythemes)
library(raster)
library(sf)
library(gdalUtils)
library(DBI)
library(RPostgres)
library(rpostgis)
library(dplyr)
library(dbplyr)
library(VTatlas)
library(mapview)
library(leaflet)
library(leafpop)
library(shinyWidgets)
library(lwgeom)

source("plotSppAccum.R")
source("predict_num_species.R")

# All the data are created using the create_data_files.R script #
# the data are populated into input_data #
################################################################################
#                                                                              #
#                           Data for the front page                            #
#                                                                              #
################################################################################
VTshape <- sf::st_read("input_data/VTboundary.shp")

num_spp <- read.csv("input_data/num_spp.csv")

num_obs <- read.csv("input_data/num_obs.csv")

tot_obs <- read.csv("input_data/tot_obs.csv")

county_class_sf <- sf::st_read("input_data/county_class_sf.shp", quiet = TRUE)

names(county_class_sf) <- c("County","Total_spp","Animalia","Plantae","Fungi","Protozoa","Bacteria","Chromista","Archaea","Viruses","incertae.sedis","geometry")

block_class_sf <- sf::st_read("input_data/block_class_sf.shp", quiet = TRUE)

names(block_class_sf) <- c("Survey_Block","Total_spp","Animalia","Plantae","Fungi","Protozoa","Bacteria","Chromista","Archaea","Viruses","incertae.sedis","geometry")


towns_class_sf <- sf::st_read("input_data/towns_class_sf.shp", quiet = TRUE)

names(towns_class_sf) <- c("Town","Total_spp","Animalia","Plantae","Fungi","Protozoa","Bacteria","Chromista","Archaea","Viruses","incertae.sedis","geometry")

sppCumm <- read.csv("input_data/sppCumm.csv")

date_of_data_acquisition <- file.info("input_data/num_spp.csv")$ctime

data_citation <- readLines("input_data/data_citation.R")

# read in class information 
overall_classes <- read.csv("input_data/summary_of_species_classes.csv")

# remove rows without class information
overall_classes <- overall_classes[overall_classes$class!="",]
names(overall_classes) <- c("row","Class","Kingdom","Species observed")
summarizeAnimals <- overall_classes[overall_classes$Kingdom=="Animalia",]
summarizeAnimals <- head(summarizeAnimals[order(summarizeAnimals$"Species observed",decreasing = TRUE),], 10)
summarizePlants <- overall_classes[overall_classes$Kingdom=="Plantae",]
summarizePlants <- head(summarizePlants[order(summarizePlants$"Species observed",decreasing = TRUE),], n=10)



kingdom_spp_val <- readRDS("input_data/Kingdom_spp_accum.rds")
kingdom_samp_val <- readRDS("input_data/Kingdom_samp_accum.rds")

class_spp_val <- readRDS("input_data/Class_spp_accum.rds")
class_samp_val <- readRDS("input_data/Class_samp_accum.rds")

class_ABC <- dimnames(class_spp_val)[[3]]
class_ABC <- class_ABC[order(class_ABC)]
################################################################################
#
#
#                    Start of User Interface (ui)
#
################################################################################

ui <- bootstrapPage(
#  div(class = "container-fluid",
  navbarPage(theme = shinytheme("flatly"), 
             collapsible = FALSE,
             HTML('<img style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">VT Atlas of Life</a>'), 
             windowTitle = "VT Atlas Dashboard", # name of tab in browser
             tabPanel("Biodiversity",
                   sidebarLayout(
                     sidebarPanel(width = 3,
                       verticalLayout(span(tags$i(h2("Taxa breakdown"), align = "center"), style="color:#045a8d"),
                         wellPanel(span(h3("Animals"), align = "center"),
                         tableOutput("summaryAnimal")),
                         wellPanel(span(h3("Plants"), align = "center"),
                         tableOutput("summaryPlant"))
                       ),
                     ),
                     mainPanel(
                       fluidRow(
                         column("", width = 3,
                                selectInput('maptoshow',
                                            label = "Select a map",
                                            choices = c("County","Town","Survey block"),
                                            selected = "Town")
                                )
                       ),
                       div(class="outer", tags$head(includeCSS("vt_dashboard.css")),
                           
                           mapviewOutput("county_Plot", width = "100%", height = "100%"),
                           
                           absolutePanel(id = "controls", class = "panel panel-default",
                                         top = 300, left = 550, width = "25%", fixed = TRUE,
                                         draggable = TRUE, height = "auto",
                                         
                                         span(tags$i(h1("Atlas of Life by the numbers"), align = "center"), style="color:#045a8d"),
                                         h3(textOutput("num_species"), align = "center"),
                                         h4(textOutput("num_obs"), align = "center"),
                                         h4(textOutput("num_observers"), align = "center"),
                                         plotOutput("spp_accum", height = 300),
                                         # plotOutput("SppTIME_plot", height = 300),
                                         span(("These data are preliminary"),align = "left", style = "font-size:120%")
                                         #span(("These data are preliminary."),align = "left", style = "font-size:80%")
                                         
                           )
                           )
                     )
                   )
                      
                      
                      # set the first fluid row for the left panel 
                      
                          #absolutePanel(id = "logo", class = "card", bottom = 20, left = 60, width = 80, fixed=TRUE, draggable = FALSE, height = "auto",
                          #              tags$a(href='https://val.vtecostudies.org/', 
                          #                     tags$img(src='http://val.vtecostudies.org/wp-content/uploads/2020/04/VAL-Shirt-Design.jpg',
                          #                              height='200',
                          #                              width='120'))),
                          
                          #absolutePanel(id = "logo", class = "card", bottom = 20, left = 20, width = 80, fixed=TRUE, draggable = FALSE, height = "auto",
                          #              actionButton("twitter_share", 
                          #                           label = "", 
                          #                           icon = icon("twitter"),
                          #                           style='padding:5px',
                          #                           onclick = sprintf("window.open('%s')", 
                          #                                             "https://twitter.com/intent/tweet?text=%20@VTecostudies%20Vermont%20Atlas%20of%20Life&url=https://vt-ecostudies-atlas.shinyapps.io/simple_dashboard/&hashtags=Conservation"))
                          #              )
                      
                          
                          
             ),
             tabPanel("State of Biodiversity",
                      sidebarLayout(
                        sidebarPanel(width = 3,
                                     verticalLayout("Select a kindgom of organisms",
                                       selectInput('kingdomSpp',
                                                   label = "Select a Kingdom",
                                                  # choices = c("Animalia","Plantae","Fungi","Protozoa","Bacteria","Chromista","Archaea","Viruses"),
                                                   choices = c("Animalia","Plantae","Fungi"),
                                                   selected = "Animalia")),
                                     
                                       plotOutput("VT_Kingdom_thermometer")
                                     ),
                        mainPanel(
                         
                          div(class="cummulplot", tags$head(includeCSS("vt_dashboard.css")),
                              plotOutput("Kingdom_AccumulationPlot_predict")
                              )
                              
                              
                          )
                        )
             ),
                     # mainPanel(
                     #   fluidRow(
                     #     column("", width = 2,
                      #           selectInput('kingdomSpp',
                       #                      label = "Select a Kingdom",
                      #                       choices = c("Animalia","Plantae","Fungi","Protozoa","Bacteria","Chromista","Archaea","Viruses"),
                      #                       selected = "Animalia")),
                          #column("", width = 8, offset = 4,
                          #       selectInput('classSpp',
                          #                   label = "Select a Class",
                          #                   choices = class_ABC,
                          #                   selected = "Aves"))
                        #  column("",width = 4, offset = 2,
                        #           plotOutput("VT_Kingdom_thermometer"))
                       # ),
                        #fluidRow(
                       # column("", width = 3,
                       #        plotOutput("Kingdom_AccumulationPlot")),
                       # column("", width = 3, offset = 3,
                       #        plotOutput("Class_AccumulationPlot"))
                      #),
                     # fluidRow(
                     # column('', width = 2, offset = 2,
                     #        numericInput('king_predictSamples',
                     #                     "Number of samples for prediction:",
                     #                     value = 1000,
                     #                     min = 1,
                      #                    max = 100000)
                     # ),
                    #  column('', width = 2, offset = 2,
                     #        numericInput('class_predictSamples',
                     #                     "Number of samples for prediction:",
                     #                     value = 1000,
                     #                     min = 1,
                     #                    max = 100000)
                      #)
                     #),
                     # fluidRow(
                     #   column("", width = 3,
                     #          plotOutput("Kingdom_AccumulationPlot_predict")),
                      #  column("", width = 3, offset = 3,
                      #         plotOutput("Class_AccumulationPlot_predict"))
                     # )
                     # )
                      #),
                      
             tabPanel("Climate Change"),
             tabPanel("Species on the edge"),
             tabPanel("Data source",
                      fluidRow(
                        column(12,
                               textOutput("data_citation"))
                      ),
                      fluidRow(
                        column(1,offset = 10,
                               textOutput("created_by"))
                      ))
             )
#  )
)
################################################################################
#
#
#                    Start of server (ui)
#
################################################################################


server <- function(input, output, session) {

# RENDER TEXT OUTPUT
output$num_species <- renderText(paste("Taxa reported:", prettyNum(num_spp, big.mark=",",scientific=FALSE)))
output$num_obs <- renderText(paste("Total observations:", prettyNum(tot_obs, big.mark=",",scientific=FALSE)))
output$num_observers <- renderText(paste("Observers:", prettyNum(num_obs, big.mark=",",scientific = FALSE)))
output$data_update <- renderText(paste('Data acquired:', date_of_data_acquisition))
output$data_citation <- renderText(data_citation)
output$created_by <- renderText("Dashboard created by M. T. Hallworth")

# RENDER BOX OUTPUT
output$speciesbox_animals <- renderInfoBox({
  infoBox("Progress", paste0(25 + 8, "%"), icon = icon("list"),
    color = "purple", fill = TRUE)
})

# RENDER TABLE OUTPUTS
output$summaryAnimal <- renderTable({summarizeAnimals[,c("Class","Species observed")]})
output$summaryPlant <- renderTable({summarizePlants[,c("Class","Species observed")]})

# RENDER PLOTS
output$spp_accum <- renderPlot({
  par(bty = "l")
  plot(sppCumm$cum_species~sppCumm$year,
       pch = 19, type = "o",
       ylab = "Species", las = 1,
       xlab = "Year",
       main = "Number of species through time")
})

# RENDER PLOTS
output$Kingdom_AccumulationPlot <- renderPlot({

  plotSppAccum(data_array = kingdom_spp_val,
               sample_array = kingdom_samp_val,
               columnValue = input$kingdomSpp)
})

output$Class_AccumulationPlot <- renderPlot({
  
  plotSppAccum(data_array = class_spp_val,
               sample_array = class_samp_val,
               columnValue = input$classSpp)
})


output$Kingdom_AccumulationPlot_predict <- renderPlot({
  
  #plotSppAccum(data_array = kingdom_spp_val,
  #             sample_array = kingdom_samp_val,
  #             columnValue = input$kingdomSpp,
  #             predSamp = input$king_predictSamples)
  plotSppAccum(data_array = kingdom_spp_val,
               sample_array = kingdom_samp_val,
               columnValue = input$kingdomSpp,
               predSamp = max(kingdom_samp_val[,,input$kingdomSpp]*4))
})

output$Class_AccumulationPlot_predict <- renderPlot({
  
  plotSppAccum(data_array = class_spp_val,
               sample_array = class_samp_val,
               columnValue = input$classSpp,
               predSamp = input$class_predictSamples)
})

output$VT_Kingdom_thermometer <- renderPlot({
  # create polygon # 

  obs_pred_diff <- predictNumSpp(data_array = kingdom_spp_val,
                                 sample_array = kingdom_samp_val,
                                 columnValue = input$kingdomSpp,
                                 predSamp = max(kingdom_samp_val[,,input$kingdomSpp]*4))
  # Here are some values to use for the polygon creation 
  # xmin, ymin, xmax, ymax
  # calculate total area and our 30% value
  totalArea <- sf::st_area(VTshape)
  thirtyPC <- totalArea * obs_pred_diff$perObs
  
  # Plot it
  plot(st_geometry(VTshape), col = 'lightgrey', border = 'darkgrey',reset=F,
       main = paste0(round(obs_pred_diff$perObs,2)*100,"% of ",input$kingdomSpp," observed"))
  
  # Find the bounding box of the feature
  bbox <- st_bbox(VTshape)
  
  thisArea <- totalArea - totalArea # zero with correct units
  
  i <- 1
  increment <- 0.1
  
  # While our subarea is less than assigned percentage..
  while (thisArea < thirtyPC) {
    
    # Starting at bottom, create a bounding box that is smaller than full bounding box
    thisBBox <- bbox
    thisBBox['ymax'] <- thisBBox$ymin + (increment * i)
    
    # Clip shp to this bounding box
    thisSubarea <- suppressMessages(st_crop(VTshape, y=thisBBox))
    thisArea <- suppressMessages(st_area(thisSubarea))
    
    i <- i + 1
    
  }
  plot(st_geometry(thisSubarea), max.plot=1, add=T, col='red', border=NA)
  legend("bottomright", legend = c("Observed","Unobserved"),fill = c("red","lightgray"),bty="n", title = "Species")
})

# RENDER MAPS
#generate a map
output$county_Plot <- renderLeaflet({
  mapviewOptions(layers.control.pos = "bottomleft", legend.pos = "bottomright")
                        M1 <- mapview(county_class_sf, 
                                         layer.name = "County",
                                         zcol = "Total_spp",
                                         
                                         layers.control.pos = "bottomleft",
                                         popup = popupTable(county_class_sf, 
                                                            zcol = c("Total_spp",
                                                                     "Animalia",
                                                                     "Plantae",
                                                                     "Fungi",
                                                                     "Protozoa",
                                                                     "Chromista",
                                                                     "Bacteria",
                                                                     "Archaea",
                                                                     "Viruses",
                                                                     "incertae.sedis")),
                                         label = "County: Total Species", legend = TRUE)
                          

                      M2 <- mapview(towns_class_sf, 
                                        layer.name = "Town",
                                        zcol = "Total_spp",
                                        
                                        layers.control.pos = "bottomleft",
                                        popup = popupTable(towns_class_sf, 
                                                           zcol = c("Town",
                                                                    "Total_spp",
                                                                    "Animalia",
                                                                    "Plantae",
                                                                    "Fungi",
                                                                    "Protozoa",
                                                                    "Chromista",
                                                                    "Bacteria",
                                                                    "Archaea",
                                                                    "Viruses",
                                                                    "incertae.sedis")),
                                        label = "Town: Total Species", legend = TRUE)

     M3 <- mapview(block_class_sf, 
            layer.name = "Block",
            zcol = "Total_spp",
            popup = popupTable(block_class_sf, 
                               zcol = c("Total_spp",
                                        "Animalia",
                                        "Plantae",
                                        "Fungi",
                                        "Protozoa",
                                        "Chromista",
                                        "Bacteria",
                                        "Archaea",
                                        "Viruses",
                                        "incertae.sedis")),
            label = "Survey block: Total species", legend = TRUE)

     (M1+M2+M3)@map
                          })

output$survey_block_Plot <- renderLeaflet({
                                    mapview(block_class_sf, 
                                           layer.name = "Total_spp",
                                           zcol = "Total_spp",
                                    popup = popupTable(block_class_sf, 
                                             zcol = c("Total_spp",
                                      "Animalia",
                                      "Plantae",
                                      "Fungi",
                                      "Protozoa",
                                      "Chromista",
                                      "Bacteria",
                                      "Archaea",
                                      "Viruses",
                                      "incertae.sedis")),
          label = "Survey block: Total species", legend = TRUE)@map
})







}
shinyApp(ui, server)