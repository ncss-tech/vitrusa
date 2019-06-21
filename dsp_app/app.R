#load required library for creating a dashboard in shiny

library(shiny)
library(shinydashboard)
library(readxl)
library(dplyr)
library(plotly)
library(tidyr)
library(grDevices)
library(utils)
library(DT)
library(lubridate)
library(leaflet)
library(leaflet.esri)
library(leaflet.extras)
library(crosstalk)
library(htmlwidgets)
library(rmarkdown)
library(knitr)
library(tidyverse)
library(purrr)
library(aqp)
library(kableExtra)
library(sharpshootR)
library(igraph)

jsfile<- "https://cdn.datatables.net/1.10.16/js/jquery.dataTables.min.js"

#create a dashboard header

header <- dashboardHeader(title = span(tagList(
  img(src = "logo.png"),
  "Dynamic Soil Properties App"
)), titleWidth = 350)

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
                              
                              menuItem("Analysis Report", icon=icon("file-invoice"),
                                       menuSubItem("Report", tabName="projectreport", icon=icon("file-text")),
                                      uiOutput("prj_list"),
                                      uiOutput("prp_list"),

                                       actionButton("reportsubmit", "Submit")),
                              
                              
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
  
  #styling for the progress bar position
  tags$style(type="text/css", ".shiny-notification{position: fixed; top:33%;left:33%;right:33%;}"),
  tags$head(tags$script(src = jsfile),tags$link(rel="stylesheet", type="text/css",href="https://cdn.datatables.net/1.10.16/css/jquery.dataTables.min.css")),
  
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
        box("This application was developed by John Hammerly and Skye Wills.", width =
              12)
      )
    ),
    
    
    #Map tab
    
    tabItem(
      tabName = "projectextent",
      class = "active",
      titlePanel("DSP Sites"),
      verticalLayout(
        fluidRow(box(
          bscols(leafletOutput("projectextentmap"), DTOutput("site_table")), width = 12)
        ),
        
        box("This application was developed by John Hammerly and Skye Wills.", width = 12)
      )
    ),
    
    tabItem(
      tabName = "projectreport",
      titlePanel("General Analysis of DSP Master Data"),
      verticalLayout(
        fluidRow(
          # box(uiOutput("hzn_in"), width = 2),
          # box(uiOutput("hzn_ot"), width = 2),
          # box(actionButton("gnl_hzn", "Combine")),
          # box(DTOutput("gnl_table")),
          box(uiOutput("projectreport"), width = 12),
          box("This application was developed by John Hammerly and Skye Wills.", width = 12)
        )
      )
    )
    
  ))

ui <-
  dashboardPage(header, sidebar, body, title = "Dynamic Soil Properties App")

#create a function for the server
server <- function(input, output, session) {
  
  output$prj_list <- renderUI({
    dsp<-pedon_data()
    selectInput("projectreport", "Choose a Project:", sort(unique(dsp$Name)))
  })
  
  output$prp_list <- renderUI({ 
    dsp<-pedon_data()
    selectInput("prp_in", "Choose Properties:", sort(names(dsp)), multiple = TRUE)
  })
  
  observeEvent(input$reportsubmit,{
    updateTabItems(session, "tabs", "projectreport")
  })
  
  pedon_data <- reactive({DSP <- read_excel("DSP_project_master_Mar2018.xlsx", sheet = "All Data Assembled")
  names(DSP) <- gsub(" ", "_", names(DSP))
  return(DSP)
  })  
  
  dsp_labels <- reactive({dsp_labels <- read_excel("DSP_project_master_Mar2018.xlsx", sheet ="dsp_coredata_label")
  names(dsp_labels) <- gsub(" ", "_", names(dsp_labels))
  return(dsp_labels)
    })
  
output$hzn_in <- renderUI({
  dsp <- pedon_data()
  dsp <- dsp %>%
    filter(Name == isolate(input$projectreport))
  selectInput("hz_in", "Choose Horizons to Combine:", sort(unique(dsp$hor_desg)), multiple = TRUE, selectize = FALSE, size = 20)
})

output$hzn_ot <- renderUI({
  dsp <- pedon_data()
  dsp <- dsp %>%
    filter(Name == isolate(input$projectreport))
  textInput("hz_ot", "Assign a Generalized Horizon Name:")
})
  
gnl_pedon_data <- reactive({ input$gnl_hzn
  dsp <- pedon_data()
  dsp <- dsp %>%
    filter(Name == isolate(input$projectreport))
  dsp <- dsp %>%
    filter(hor_desg %in% isolate(input$hzn_in))
  dsp <- dsp %>%
    mutate(gnl_hzn = isolate(input$hnz_ot))
})

output$gnl_table <- renderDT({ input$gnl_hzn
  dsp <- gnl_pedon_data()
  return(dsp)
}, server = FALSE, extensions = "Scroller", width="100%", options = list(deferRender = FALSE, scrollY=250, scroller=TRUE, scrollX = TRUE))
  
  output$srg <- renderPlot({ input$reportsubmit
    DSP <- pedon_data()
    

    
    DSP <- DSP %>%
      filter(Name == isolate(input$projectreport))
    
    #Assign desired comparable layers (group horizons for comparisons and statistical analysis) #most horizons are covered by this list, but not all
    cl <- c("O horizons",
            "L horizons",
            "A horizons", 
            "E horizons",
            "Bk horizons",
            "Bt horizons",
            "Other B horizons",
            "C horizons",
            'Cr and R horizons')
    
    # use REGEX rules to find matching horizons to assign to comparable layers
    #adjust as needed
    # the $ sign signifies that any character is acceptable in that position
    cl_hor <- c('O|$O$|O$|$O' ,
                'L|$L$|L$|$L' ,
                'A|$A$|A$|$A' ,
                'E|$E$|E$|$E' ,
                'Bk|$Bk$|Bk$|$Bk' ,
                'Bt|$Bt$|Bt$|$Bt' ,
                'B|$B$|B$|$B' ,
                'C|$C$|C$|$C' ,
                'Cr|$Cr$|Cr$|$Cr|R|$R$|R$|$R' ,
                '$Cr$|$R$')
    
    
    DSP$Comp_layer <- generalize.hz(DSP$hor_desg, cl, cl_hor)
    
    tab <- table(DSP$Comp_layer, DSP$hor_desg)
    m <- genhzTableToAdjMat(tab)
    plotSoilRelationGraph(m, graph.mode = 'directed', edge.arrow.size=0.5)
  
  })
  
  shared_DSP_sites <-
    
    SharedData$new(reactive({
      
      
      sd_t <-filter(
        read_excel("DSP_project_master_Mar2018.xlsx", sheet = "DSP_site"),
        !is.na(`Std Latitude`)
      )
      
      sd_t <- mutate(sd_t, lng = sd_t$`Std Longitude`)
      sd_t <- mutate(sd_t, lat = sd_t$`Std Latitude`)
      
    }))
  
  output$projectextentmap <- renderLeaflet({
    input$extentsubmit
    
    withProgress(message = "Preparing Extent viewing",
                 detail = "Please Wait",
                 value = 1,
                 {
                   
                   site_icons <- awesomeIcons(
                     icon = "circle",
                     iconColor = 'black',
                     library = "fa",
                     markerColor = "blue"
                   )
                   
                   m <- leaflet(data = shared_DSP_sites)
                   m <- addTiles(m, group = "Open Street Map")
                   
                   
                   incProgress(1 / 10, detail = paste("Adding Data to Map"))
                   
                   m <-
                     addProviderTiles(m, providers$Esri.WorldImagery, group = "ESRI Imagery")
                   m <-
                     addProviderTiles(m, providers$OpenMapSurfer.AdminBounds, group = "Admin Boundaries")
                   m <- hideGroup(m, c("Admin Boundaries", "MLRA"))
                   m <-
                     addEasyButton(m,
                                   easyButton(
                                     icon = "fa-globe",
                                     title = "Zoom to CONUS",
                                     onClick = JS("function(btn, map){map.setZoom(4);}")
                                   ))
                   m <-
                     addProviderTiles(m, providers$Esri.WorldStreetMap, group = "ESRI Street")
                   m <-
                     addProviderTiles(m, providers$Esri.WorldTopoMap, group = "ESRI Topo")
                   m <-
                     addProviderTiles(m, providers$Stamen.Terrain, group = "Stamen Terrain")
                   m <-
                     addProviderTiles(m, providers$Stamen.TonerLite, group = "Stamen TonerLite")
                   m <-
                     addWMSTiles(
                       m,
                       "https://SDMDataAccess.sc.egov.usda.gov/Spatial/SDM.wms?",
                       options = WMSTileOptions(
                         version = "1.1.1",
                         transparent = TRUE,
                         format = "image/png"
                       ),
                       layers = "mapunitpoly",
                       group = "Soil Polygons"
                     )
                   m <-
                     addMarkers(
                       m,
                       group = "Dynamic Soil Property Sites"
                     )

                   m <-
                     addLayersControl(
                       m,
                       baseGroups = c(
                         "ESRI Street",
                         "ESRI Topo",
                         "ESRI Imagery",
                         "Open Street Map",
                         "Stamen Terrain",
                         "Stamen TonerLite"
                       ),
                       overlayGroups = c("Soil Polygons", "Admin Boundaries")
                     )
                   
                   incProgress(1 / 10, detail = paste("Your Map is on its way!"))
                 })
    m
  })
  
  # When map is clicked, show a table with site info
  
  # data <- reactiveValues(clickedMarker=NULL)
  # 
  # observeEvent(input$projectextentmap_marker_click,{
  #   data$clickedMarker <- input$projectextentmap_marker_click
  #   output$site_table <- renderDT({return(
  #     subset(DSP_sites, `User Site ID` == data$clickedMarker$id)
  #   )})
  #   
  #   
  # })
  
  output$site_table <-DT::renderDataTable(shared_DSP_sites, server = FALSE, extensions = "Scroller", width="100%", options = list(deferRender = FALSE, scrollY=250, scroller=TRUE, scrollX = TRUE))



output$projectreport<-renderUI({ input$reportsubmit


  withProgress(message="Generating Report", detail="Please Wait", value=1, {

    includeHTML(rmarkdown::render(input = "report.Rmd",
                                  output_dir = tempdir(),
                                  output_file = "temp.html",
                                  envir = new.env(),
                                  params = list(projectname = isolate(input$projectreport), prop = isolate(input$prp_in)),
                                  html_fragment(pandoc_args = "--toc")
    ))
  })
})

}


#combine the user interface and server to generate the shiny app
shinyApp(ui = ui, server = server)