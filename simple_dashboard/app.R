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

# All the data are created using the create_data_files.R script #
# the data are populated into input_data #
################################################################################
#                                                                              #
#                           Data for the front page                            #
#                                                                              #
################################################################################
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

################################################################################
#
#
#                    Start of User Interface (ui)
#
################################################################################

ui <- bootstrapPage(
#  div(class = "container-fluid",
  navbarPage(theme = shinytheme("flatly"), collapsible = FALSE,
             HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">VT Atlas of Life</a>'), id="nav",
             windowTitle = "Biodiversity", 
             
             tabPanel("Biodiversity",

                      div(class="outer", tags$head(includeCSS("vt_dashboard.css")),
                          
                       mapviewOutput("county_Plot", width = "100%", height = "100%"),
                          
                          absolutePanel(id = "controls", class = "panel panel-default",
                                        top = 300, left = 400, width = "25%", fixed = TRUE,
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
                                     
                       ),
                                        
                      fluidRow(
                        column(width = 2, 
                               offset = 0,
                               infoBoxOutput("speciesbox_animals", 
                                             width = 4)
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
             tabPanel("Climate Change",
                      fluidRow(
                        column(12,
                               textOutput("data_citation"))
                      )),
             tabPanel("Species on the edge")
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

# RENDER BOX OUTPUT
output$speciesbox_animals <- renderInfoBox({
  infoBox("Progress", paste0(25 + 8, "%"), icon = icon("list"),
    color = "purple", fill = TRUE)
})

# RENDER PLOTS
output$spp_accum <- renderPlot({
  par(bty = "l")
  plot(sppCumm$cum_species~sppCumm$year,
       pch = 19, type = "o",
       ylab = "Species", las = 1,
       xlab = "Year",
       main = "Species Accumulation Curve")
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