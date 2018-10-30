#load required library for creating a dashboard in shiny

library(shiny)
library(shinydashboard)
library(readxl)
library(dplyr)
library(plotly)
library(tidyr)
library(xlsx)
library(rJava)
library(xlsxjars)
library(grDevices)
library(utils)
library(DT)
library(lubridate)
library(leaflet)
library(leaflet.esri)
library(leaflet.extras)

#create a dashboard header

header <- dashboardHeader(title = span(
  tagList(
    img(src = "logo.png"),
    "Dynamic Soil Properties App"
  )
), titleWidth = 350)

#create a sidebar and menu structure

sidebar <- dashboardSidebar(width = 250,
                            sidebarMenu(
                              id = "tabs",
                              
                              #Home Page Menu
                              menuItem(
                                "Home",
                                tabName = "Home",
                                selected = TRUE,
                                icon = icon("home")
                              ),
                              
                              menuItem(
                                "Sample Locations",
                                tabName = "projectextent",
                                icon = icon("map-pin")
                                ),
                                
                              
                              #Source Code Menu
                              menuItem(
                                "Source Code",
                                icon = icon("file-code-o"),
                                href = "https://github.com/ncss-tech/vitrusa/tree/master/dsp_app"
                              ),
                              
                              # Submit Issues on GitHub
                              menuItem("Submit App Issues", icon =
                                         icon("bug"), href = "https://github.com/ncss-tech/vitrusa/issues")
                            ))

#Create a body for the dashboard

body <- dashboardBody(#create tabs to match the menu items
  tabItems(
    #Home tab
    tabItem(
      tabName = "Home",
      titlePanel("Welcome to the Dynamic Soil Properties App"),
      verticalLayout(
        infoBox(
          "About this App",
          "The Dynamic Soil Properties App is designed to explore dynamic soil property data",
          width = 12,
          icon = icon("database"),
          color = "blue"
        ),
        box(includeHTML("home.html"), width = 12),
        box("This application was developed by John Hammerly.", width =
              12)
      )
    ),
    
    #Map tab
    
    tabItem(
      tabName = "projectextent",
      class = "active",
      titlePanel("DSP Sites"),
      verticalLayout(fluidRow(
  box(leafletOutput("projectextentmap"), width = 12)
        ),
        
        box("This application was developed by John Hammerly.", width = 12)
      ))
    )
  )

ui <-
  dashboardPage(header, sidebar, body, title = "Dynamic Soil Properties App")

#create a function for the server
server <- function(input, output, session) {
  
  DSP_sites <- read_excel("DSP_project_master_Mar2018.xlsx", sheet = "DSP_site")
  

  
  output$projectextentmap<-renderLeaflet({ input$extentsubmit
    
    withProgress(message="Preparing Extent viewing", detail="Please Wait", value=1, {
      
      site_icons <-awesomeIcons(
        icon="circle",
        iconColor = 'black',
        library="fa",
        markerColor="blue")
      
      m<-leaflet()
      m<-addTiles(m, group="Open Street Map")
      
      
      incProgress(1/10, detail =paste("Adding Data to Map"))
      
      m<-addProviderTiles(m, providers$Esri.WorldImagery, group="ESRI Imagery")
      m<-addProviderTiles(m, providers$OpenMapSurfer.AdminBounds, group="Admin Boundaries")
      m<-hideGroup(m, c("Admin Boundaries","MLRA"))
      m<-addEasyButton(m, easyButton(icon="fa-globe", title="Zoom to CONUS", onClick=JS("function(btn, map){map.setZoom(4);}")))
      m<-addProviderTiles(m, providers$Esri.WorldStreetMap, group="ESRI Street")
      m<-addProviderTiles(m, providers$Esri.WorldTopoMap, group="ESRI Topo")
      m<-addProviderTiles(m, providers$Stamen.Terrain, group="Stamen Terrain")
      m<-addProviderTiles(m, providers$Stamen.TonerLite, group="Stamen TonerLite")
      m<-addWMSTiles(m, "https://SDMDataAccess.sc.egov.usda.gov/Spatial/SDM.wms?", options= WMSTileOptions(version="1.1.1", transparent=TRUE, format="image/png"), layers="mapunitpoly", group="Soil Polygons")
      m<-addMarkers(m, data=DSP_sites, lng=DSP_sites$`Std Longitude`, lat=DSP_sites$`Std Latitude`, label=paste0("User site ID: ", DSP_sites$`User Site ID`), group="Dynamic Soil Property Sites")
      m<-addLayersControl(m, baseGroups=c("ESRI Street", "ESRI Topo", "ESRI Imagery","Open Street Map", "Stamen Terrain", "Stamen TonerLite"),overlayGroups=c("Soil Polygons", "Admin Boundaries"))

      incProgress(1/10, detail =paste("Your Map is on its way!"))
    })
    m
  })
  
}


#combine the user interface and server to generate the shiny app
shinyApp(ui = ui, server = server)