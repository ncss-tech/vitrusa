#load required library for creating a dashboard in shiny

library(shinydashboard)
library(plyr)
library(leaflet)
library(rmarkdown)
library(soilReports)
library(shinyjs)

source("wt.R")
source("om.R")
#source("c:/workspace2/pr.R")
jsfile<- "https://cdn.datatables.net/1.10.16/js/jquery.dataTables.min.js"
pljs <- "https://cdn.plot.ly/plotly-latest.min.js"


#create a dashboard header

header<-dashboardHeader(
  title= span(tagList(img(src="logo.png"), "North Central Region Web App")), titleWidth=350)

#create a sidebar and menu structure

sidebar<-dashboardSidebar( width = 250,
  sidebarMenu(id="tabs",
            
              #Home Page Menu    
              menuItem("Home", tabName="Home", selected=TRUE, icon=icon("home")),
              
              
              menuItem("NASIS Reports", icon=icon("flask"), href ="https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=Region-11-Masterlist"),
              
              #Water Table Menu
              menuItem("Water Table", icon=icon("tint"),
                       menuSubItem("Plot", tabName="WTPlots", icon=icon("area-chart")),
                       menuSubItem("Data", tabName="Data", icon=icon("table")), 
                       wt_selectInput("wt_query")
              ),
              
              #Organic Matter Menu
              
              menuItem("Organic Matter", icon=icon("leaf"),
                       menuSubItem("Plot", tabName="OMPlots", icon=icon("area-chart")),
                       menuSubItem("Data", tabName="omdata", icon=icon("table")),
                       om_selectInput("om_query")
              ),
              
              #Project Report Menu
              
              # menuItem("Project Report", icon=icon("stack-overflow"),
                       # menuSubItem("Report", tabName="projectreport", icon=icon("file-text")),
                        # pr_textAreaInput("pr_query")
                       # ),

              menuItem("Project Report", icon=icon("stack-overflow"),
                       menuSubItem("Report", tabName="projectreport", icon=icon("file-text")),
                       textAreaInput(
                         inputId="projectreport",
                         label="Enter Project Name(s) -", "EVAL - MLRA 111A - Ross silt loam, 0 to 2 percent slopes, frequently flooded",
                         resize="none",
                         rows=5),
                       actionButton("reportsubmit", "Submit"), p(downloadLink("projectreportdownload", "Save a copy of this Report")), br(),p()),
              
                            
              #Project Extent Menu
              
              menuItem("Project Extent", icon=icon("map-signs"),
                       menuSubItem("Extent", tabName="projectextent", icon=icon("map")),
                       textInput(inputId="fyinput", label="Enter Fiscal Year -", 2019),
                       selectizeInput("office", "Choose an Office -",
                                      c("11-ATL"="11-ATL",
                                        "11-AUR"="11-AUR",
                                        "11-CLI"="11-CLI",
                                        "11-FIN"="11-FIN",
                                        "11-GAL"="11-GAL",
                                        "11-IND"="11-IND",
                                        "11-JUE"="11-JUE",
                                        "11-MAN"="11-MAN",
                                        "11-SPR"="11-SPR",
                                        "11-UNI"="11-UNI",
                                        "11-WAV"="11-WAV"),
                                      selected="11-CLI",multiple=FALSE, options=list(create=TRUE)),
                       textAreaInput(
                         inputId="projectextent",
                         label="Enter Project Name -","EVAL - MLRA 112 - Eram silt loam, 5 to 9 percent slopes",
                         resize="none",
                         rows=5),
                       actionButton("extentsubmit", "Submit"), p(downloadLink("projectextentdownload", "Save spatial data")), br(),p()
              ),
              
              #Series Extent Map
              
              menuItem("Series Extent", icon=icon("fire"),
                       menuSubItem("Extent", tabName = "seriesextent", icon =icon("map")),
                       textInput(inputId = "seriesexinput", label = "Enter Series Name(s) -", "Marshall"),
                       actionButton("seriesextentsubmit", "Submit"), br(),p()
              ),
              
              #Long Range Plan
              
              menuItem("Long Range Plan", icon=icon("plane"),
                       menuSubItem("Report", tabName="lrp", icon=icon("calendar")),
                       textInput(inputId="lrpinput", label="Enter SSO Symbol -", "11"),
                       actionButton("lrpsubmit", "Submit"), p(downloadLink("lrpdownload", "Save a copy of this Report")),br(),p()
              ),
              
              #Project Staff
              
              menuItem("Project Staff", icon=icon("users"),
                       menuSubItem("Report", tabName="ps", icon=icon("flag-checkered")),
                       textInput(inputId="psso", label="Enter SSO Symbol -", "11"),
                       textInput(inputId="pfy", label="Enter Fiscal Year", "2021"),
                       actionButton("pssubmit", "Submit"),br(),p()
              ),
              
              #Interpretations
              
              menuItem("Interpretations", icon=icon("archive"),
                       menuSubItem("Report", tabName="ir", icon=icon("file-text")),
                       textInput(inputId="irinput", label="Enter NATSYM -", "2xbld"),
                       actionButton("irsubmit", "Submit"), br(),p()
              ),
              #Source Code Menu
              
              menuItem("Source Code", icon=icon("file-code-o"), href="https://github.com/ncss-tech/vitrusa/tree/master/r11_app"),
              
              # Submit Issues on GitHub
              menuItem("Submit App Issues", icon=icon("bug"), href="https://github.com/ncss-tech/vitrusa/issues"),
              
              #Help Menu
              
              menuItem("Help", tabName="help", icon=icon("question"))
              

  )
)

#Create a body for the dashboard

body<-dashboardBody(
  
  #add style tags for customizations  
  
  #styling for the project extent map
  tags$style(type="text/css", "#projectextentmap {height: calc(100vh - 80px) !important;}"),
  tags$style(type="text/css", "#seriesextentmap {height: calc(100vh - 80px) !important;}"),
  #styling for the progress bar position
  tags$style(type="text/css", ".shiny-notification{position: fixed; top:33%;left:33%;right:33%;}"),
  tags$head(tags$script(src = jsfile),tags$link(rel="stylesheet", type="text/css",href="https://cdn.datatables.net/1.10.16/css/jquery.dataTables.min.css")),
  tags$head(tags$script(src = pljs), tags$link(rel ="stylesheet", type="text/css", href = "https://cdn.plot.ly/plotly-latest.min.js")),
  
  useShinyjs(),
  
  #create tabs to match the menu items
  tabItems(
    #Home tab
    tabItem(tabName="Home",
            titlePanel("Welcome to the North Central Region Web App"),
            verticalLayout(
              infoBox("About this App", "The North Central Region Web App is a tool for soil scientists, geographers, and ecologists to get soils information on the web.", width=12, icon=icon("users"), color="blue"),
              box(includeHTML("home.html"), width=12),
              box("This application was developed by John Hammerly, Stephen Roecker, and Dylan Beaudette.", width=12)
            )),
    
    #LIMS Reports
    
    tabItem(tabName="LIMS",
            titlePanel("NASIS Reports"),
            verticalLayout(
              fluidRow(
                box(tags$div(
            includeHTML("lims.html"), style="width:100%; overflow-x: scroll"), width = 12)))),
    
    
    #water table plot tab   
    tabItem(tabName="WTPlots", class="active",
            titlePanel("Water Table Plots"),
            wt_tabItem("wt_query")
    ),
    
    #water table data tab
    tabItem(tabName="Data",
      titlePanel("Water Table Data"),
        wt_tabItem2("wt_query"),
      verticalLayout(        
          box("This application was developed by John Hammerly and Stephen Roecker.", width=12))
      ),
    
    #project report tab
    # tabItem(
    #   tabName="projectreport",
    #   titlePanel("Component Report from LIMS"),
    #   pr_tabItem("pr_query")
    #   ),
    
    tabItem(
      tabName="projectreport",
      titlePanel("Project Report"),
      verticalLayout(
        fluidRow(
          box(tags$div(uiOutput("projectreport", inline=TRUE, container=span), style="width:100%; overflow-x: scroll"), width=12),
          box("This application was developed by John Hammerly and Stephen Roecker.", width=12))
      )),
    
    #project extent tab
    tabItem(
      tabName="projectextent",
      leafletOutput("projectextentmap")
      ),
    
    #series extent tab
    tabItem(
      tabName="seriesextent",
      leafletOutput("seriesextentmap")
    ),
    
    #Long Range Plan tab
    tabItem(
      tabName="lrp",
      titlePanel("Long Range Plan"),
      verticalLayout(
        fluidRow(
          box(tags$div(uiOutput("lrp", inline=TRUE, container=span), style="width:100%; overflow-x: scroll"), width=12),
          box("This application was developed by John Hammerly and Stephen Roecker.", width=12)))
    ),
    
    #Project Staff tab
    tabItem(
      tabName="ps",
      titlePanel("Project Staff"),
      verticalLayout(
        fluidRow(
          box(tags$div(uiOutput("ps", inline=TRUE, container=span), style="width:100%; overflow-x: scroll"), width=12),
          box("This application was developed by John Hammerly and Stephen Roecker.", width=12)))
    ),
    
    #Interpretation Report tab
    tabItem(
      tabName="ir",
      titlePanel("Interpretation Report"),
      verticalLayout(
        fluidRow(
          box(tags$div(uiOutput("ir", inline=TRUE, container=span), style="width:100%; overflow-x: scroll"), width=12),
          box("This application was developed by John Hammerly and Stephen Roecker.", width=12)))
    ),
    
    #Organic Matter Plot Tab
    tabItem(
      tabName="OMPlots",
      titlePanel("Organic Matter Plots"),
      om_tabItem("om_query")
    ),
    #organic matter data tab
    tabItem(tabName="omdata",
            titlePanel("Organic Matter Data"),
            om_tabItem2("om_query")),
    
    #Help Tab
    tabItem(tabName="help",
            titlePanel("Help"),
            fluidRow(
            box(includeHTML("help.html"), width=12),
            box("This application was developed by John Hammerly and Stephen Roecker.", width=12)
              )

  )))

#combine the header, sidebar, and body into a complete page for the user interface
ui <- dashboardPage(header, sidebar, body, title = "North Central Region Web App")

#create a function for the server
server <- function(input, output, session){

  #water table plot render

wt_plot <- callModule(wt, "wt_query")

om_plot <- callModule(om, "om_query")

# pr <- callModule(pr, "pr_query")

# library(knitr)
# render project report markdown

observeEvent(input$reportsubmit,{
  updateTabItems(session, "tabs", "projectreport")
})

  output$projectreport<-renderUI({ input$reportsubmit

    
    withProgress(message="Generating Report", detail="Please Wait", value=1, {
      download.file("https://raw.githubusercontent.com/ncss-tech/soilReports/master/inst/reports/region11/component_summary_by_project/report.Rmd", file.path(tempdir(), "report.Rmd"))
      
      includeHTML(rmarkdown::render(input = file.path(tempdir(), "report.Rmd"),
                                    output_dir = tempdir(), 
                                    output_file = "temp.html", 
                                    envir = new.env(),
                                    params = list(projectname = isolate(input$projectreport)),
                                    html_fragment(number_sections=TRUE, pandoc_args = "--toc")
                                    ))
      })
    })
  
  output$lrpdownload<- downloadHandler(      
    filename = function() {input$lrpsubmit
      paste("lrpreport", Sys.Date(), ".html", sep="")
    },
    content= function(file) {
      lrptempReport <-file.path(tempdir(), "lrpreport.Rmd")
      file.copy("r11_long_range_plan.Rmd", lrptempReport, overwrite=TRUE)
      
      withProgress(message="Preparing Report for Saving", detail="Please Wait", value=1, {
        rmarkdown::render(lrptempReport, output_file=file, html_document(number_sections=FALSE, toc=FALSE, toc_float=FALSE))
      }
      )}
  )  
  
  output$projectreportdownload<- downloadHandler(      
    filename = function() { input$reportsubmit
      paste("projectreport", Sys.Date(), ".html", sep="")
    },
    
    content= function(file) {
      withProgress(message = "Preparing Report for Saving", detail = "Please Wait", value = 1, {
        download.file("https://raw.githubusercontent.com/ncss-tech/soilReports/master/inst/reports/region11/component_summary_by_project/report.Rmd", file.path(tempdir(), "report.Rmd"))
        
        rmarkdown::render(input = file.path(tempdir(), "report.Rmd"),
                          output_dir = tempdir(), 
                          output_file = file, 
                          envir = new.env(),
                          params = list(projectname = isolate(input$projectreport)),
                          output_format = html_document(number_sections=TRUE, toc=TRUE, toc_float=TRUE)
                          )
      }
    )}
  )
  
  #render long range plan report markdown
  
  observeEvent(input$lrpsubmit,{
    updateTabItems(session, "tabs", "lrp")
  })
  output$lrp<-renderUI({ input$lrpsubmit
    withProgress(message="Generating Report", detail="Please Wait", value=1, {includeHTML(rmarkdown::render("r11_long_range_plan.Rmd", html_fragment(number_sections=TRUE, toc=TRUE)))})
  })
  
  #render staff report markdown
  observeEvent(input$pssubmit,{
    updateTabItems(session, "tabs", "ps")
  })
  output$ps<-renderUI({ input$pssubmit
    withProgress(message="Generating Report", detail="Please Wait", value=1, {includeHTML(rmarkdown::render("staff_report.Rmd", html_fragment(number_sections=TRUE, toc=TRUE)))})
  })
  
  #render interpretation report markdown
  
  observeEvent(input$irsubmit,{
    updateTabItems(session, "tabs", "ir")
  })
  output$ir<-renderUI({ input$irsubmit
    withProgress(message="Generating Report", detail="Please Wait", value=1, {includeHTML(rmarkdown::render("interp_report.Rmd", html_fragment(number_sections=TRUE, toc=TRUE)))})
  })

  #render project extent map
  
  observeEvent(input$extentsubmit,{
    updateTabItems(session, "tabs", "projectextent")
    shinyjs::addClass(selector = "body", class = "sidebar-collapse")
  })
  
  extentdata <- reactive({ input$extentsubmit
    
    #load required libraries  
    library(rmarkdown)
    library(rvest)
    library(soilDB)
    library(sp)
    library(rgdal)
    library(leaflet)
    library(rgeos)
    library(leaflet.esri)
    library(leaflet.extras)
    
      ##### The following code is mostly from Dylan Beaudette #####
      
      # NASIS WebReport
      # get argument names from report HTML source
      url <- 'https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=WEB-PROJECT_MUKEY_BY_GOAL_YEAR'
      
      ##### swap comment marks on the next 2 lines when testing the app locally#####
      args <- list(msso=paste0(isolate(input$office)), fy=paste0(isolate(input$fyinput)), asym='%', proj='0')
      # args <- list(msso="11-WAV", fy=2018, asym='%', proj='0')

      
      # this is now implemented in soilDB
      # get first table
      d <- parseWebReport(url, args, index=1)

      
      # make names legal
      names(d) <- make.names(names(d))
      
      # rename columns so they match later
      names(d)[13] <- "mukey"
      
      # check: OK
      str(d)
      
      # # pre-filter projects so that they only include MLRA / SDJR
      
      ##### swap comment marks on the next 2 lines when testing the app locally#####
      idx <- grep(paste0(isolate(input$projectextent)), d$Project.Name)
      # idx <- grep(paste0('MLRA 104 - Readlyn soils texture and slope phases'), d$Project.Name)
      
      d <- d[idx, ]

      
      # get geometry associated with a specific project
      # note: requests for the full geometry will fail when results are too large for JSON serializer
      # solution: use BBOX instead
      getFeatures <- function(i) {
        # get only feature envelopes (BBOX)
        q <- "SELECT 
        mupolygongeo.STEnvelope().STAsText() AS geom, muname, P.mukey
        FROM mupolygon AS P
        INNER JOIN mapunit AS M ON P.mukey = M.mukey
        WHERE P.mukey IN "
        
        # interpolate mukey via IN statement
        q <- paste0(q, format_SQL_in_statement(i$mukey), ';')
        # q <- paste0(q, format_SQL_in_statement(d$MUKEY), ';')
        
        # get the results quietly
        res <- suppressMessages(SDA_query(q))
        if(is.null(res))
          return(NULL)
        
        # convert to sp object
        s <- processSDA_WKT(res)
        
        # join relevant original metadata via mukey
        # this only works within a single project ID
        s <- sp::merge(s, unique(i[, c('mukey', 'NATMUSYM', 'Project.Name', 'Project.Type', 'Area_Sym', 'Project.Rec.ID')]), by='mukey', all.x=TRUE)
        # s <- sp::merge(s, unique(d[, c('mukey', 'NATMUSYM', 'Project.Name', 'Project.Type', 'Area_Sym', 'Project.Rec.ID')]), by='mukey', all.x=TRUE)
        return(s)
      }
      
      # get / process features by mapunit

      
      sp <- dlply(d, 'Project.Name', getFeatures, .progress='text')

      
      # remove NULL features
      idx <- which(! sapply(sp, is.null))
      sp <- sp[idx]
      
      # convert into single SPDF
      sp.final <- do.call('rbind', sp)
      
      # sanity check: any projects missing from the results?
      # no, they are all there
      setdiff(unique(sp.final$projectiid), unique(d$projectiid))

      ##### End of Dylan's code #####
      
      return(sp.final)
  })
  
  output$projectextentmap<-renderLeaflet({ input$extentsubmit
    
    withProgress(message="Preparing Extent viewing", detail="Please Wait", value=1, {
    
      sp.final <- extentdata()
      
      pal<- colorFactor(palette="viridis", domain=sp.final$muname)
      
      m<-leaflet()
      m<-addTiles(m, group="Open Street Map")
      
      
      incProgress(1/10, detail =paste("Adding Data to Map"))
      
      m<-addProviderTiles(m, providers$Esri.WorldImagery, group="ESRI Imagery")
      m<-addProviderTiles(m, providers$OpenMapSurfer.AdminBounds, group="Admin Boundaries")
      m<-hideGroup(m, c("Admin Boundaries","MLRA"))
      m<-addEasyButton(m, easyButton(icon="fa-globe", title="Zoom to CONUS", onClick=JS("function(btn, map){map.setZoom(4);}")))
      m<-addEsriFeatureLayer(map=m, url='https://services.arcgis.com/SXbDpmb7xQkk44JV/arcgis/rest/services/US_MLRA/FeatureServer/0/',
                             group="MLRA", useServiceSymbology = TRUE, popupProperty =propsToHTML(props=c("MLRARSYM","MLRA_NAME")), fill = TRUE, fillColor = "white", fillOpacity = 0.1, color = "white")
      m<-addProviderTiles(m, providers$Esri.WorldStreetMap, group="ESRI Street")
      m<-addProviderTiles(m, providers$Esri.WorldTopoMap, group="ESRI Topo")
      m<-addProviderTiles(m, providers$Stamen.Terrain, group="Stamen Terrain")
      m<-addProviderTiles(m, providers$Stamen.TonerLite, group="Stamen TonerLite")
      m<-addWMSTiles(m, "https://SDMDataAccess.sc.egov.usda.gov/Spatial/SDM.wms?", options= WMSTileOptions(version="1.1.1", transparent=TRUE, format="image/png"), layers="mapunitpoly", group="Soil Polygons")
      m<-addPolygons(m, data=sp.final, stroke=TRUE, color= ~pal(muname), weight=2, popup= paste("<b>MLRA SSO Area Symbol:  </b>", sp.final$Area_Sym, "<br>",
                                                                                                "<b>Project Type:  </b>", sp.final$Project.Type, "<br>",
                                                                                                "<b>Project Name:  </b>", sp.final$Project.Name, "<br>",
                                                                                                "<b>Mapunit Key:  </b>", sp.final$mukey, "<br>",
                                                                                                "<b>National Mapunit Symbol:  </b>", sp.final$NATMUSYM, "<br>",
                                                                                                "<b>Mapunit Name:  </b>", sp.final$muname), group="Mapunits")
      m<-addLayersControl(m, baseGroups=c("ESRI Street", "ESRI Topo", "ESRI Imagery","Open Street Map", "Stamen Terrain", "Stamen TonerLite"),overlayGroups=c("Soil Polygons", "MLRA", "Admin Boundaries", "Mapunits"))
      m<-addLegend(m, pal=pal, position="bottomleft", values= sp.final$muname)
      incProgress(1/10, detail =paste("Your Map is on its way!"))
    })
    m
  })
  
  observeEvent(input$seriesextentsubmit,{
    updateTabItems(session, "tabs", "seriesextent")
    shinyjs::addClass(selector = "body", class = "sidebar-collapse")
  })
  
  seriesextentdata <- reactive({ input$seriesextentsubmit
    library(aqp)
    library(soilDB)
    library(sf)
    library(dplyr)
    library(ggplot2)
    library(purrr)
    library(DT)
    library(leaflet)
    library(leaflet.esri)
    library(leaflet.extras)

    
    ser <- unlist(strsplit(isolate(input$seriesexinput), ","))
    
    sss <- lapply(ser, get_soilseries_from_NASISWebReport)
    
    srb <- do.call("rbind", sss)
    
    sl <- sapply(ser, seriesExtent)
    
    sf <- lapply(sl, st_as_sf, stringsAsFactors = FALSE)
    
    alsfs <- do.call("rbind", sf)
    
    mlra <- st_read("mlra_v42.shp")
    
    mlra <- st_transform(mlra, 4326)
    
    alsfs1 <- alsfs %>% dplyr::summarize(geometry = st_union(geometry))
    
    alsfs1 <- st_as_sf(alsfs1)
    
    mlra_sub <- mlra %>% filter(st_intersects(mlra, alsfs1, sparse = FALSE))
    
    mlra_sub <- st_as_sf(mlra_sub)
    
    rgs <- data.frame(ssr = c(1:12), mlraoffice = c("portland, or", "davis, ca", "raleigh, nc", "bozeman, mt", "salina, ks", "morgantown, wv", "auburn, al", "phoenix, az", "temple, tx", "st. paul, mn", "indianapolis, in", "amherst, ma"))
    
    srb <- srb %>% left_join(rgs, by = "mlraoffice")
    
    seriestable <- srb %>% left_join(alsfs, by = c("soilseriesname" = "series"))
    
    seriestable <- st_as_sf(seriestable)
    
    stlist <- list("seriestable" = seriestable, "mlra_sub" = mlra_sub)
    
    return(stlist)
    
  })
  
  output$seriesextentmap<-renderLeaflet({ input$seriesextentsubmit
    
    withProgress(message="Preparing Extent viewing", detail="Please Wait", value=1, {
      
      
      stlist <- seriesextentdata()
      
      seriestable <- stlist$seriestable
      
      mlra_sub <- stlist$mlra_sub
      
      pal2<- colorFactor(palette="viridis", domain=seriestable$soilseriesname)
      
      m2<-leaflet()
      m2<-addTiles(m2, group="Open Street Map")
      m2<-addProviderTiles(m2, providers$Esri.WorldImagery, group="ESRI Imagery")
      m2<-addProviderTiles(m2, providers$HikeBike.HillShading, group="Hillshade")
      m2<-addEasyButton(m2, easyButton(icon="fa-globe", title="Zoom to CONUS", onClick=JS("function(btn, map){map.setZoom(4);}")))
      # m<-addEsriFeatureLayer(map=m, url='https://services.arcgis.com/SXbDpmb7xQkk44JV/arcgis/rest/services/US_MLRA/FeatureServer/0/',
      #                        group="MLRA", useServiceSymbology = TRUE, popupProperty =propsToHTML(props=c("MLRARSYM","MLRA_NAME")), fill = TRUE, fillColor = "white", fillOpacity = 0.1, color = "white")
      m2<-addProviderTiles(m2, providers$Esri.WorldStreetMap, group="ESRI Street")
      m2<-addProviderTiles(m2, providers$Esri.WorldTopoMap, group="ESRI Topo")
      m2<-addProviderTiles(m2, providers$Stamen.Terrain, group="Stamen Terrain")
      m2<-addProviderTiles(m2, providers$Stamen.TonerLite, group="Stamen TonerLite")
      m2<-addWMSTiles(m2, "https://SDMDataAccess.sc.egov.usda.gov/Spatial/SDM.wms?", options= WMSTileOptions(version="1.1.1", transparent=TRUE, format="image/png"), layers="mapunitpoly", group="Soil Polygons")
      m2<-addPolygons(m2, data=mlra_sub, stroke = TRUE, color = "red", fill = TRUE, fillColor = "white", fillOpacity = 0.1, weight = 2, popup= paste("<b>MLRA SYM:  </b>", mlra_sub$MLRARSYM, "<br>",
                                                                                                                                                   "<b>MLRA NAME:  </b>", mlra_sub$MLRA_NAME, "<br>"
      ), group = "MLRA")
      m2<-addPolygons(m2, data=seriestable, stroke=TRUE, color= ~pal2(soilseriesname), weight=2, popup= paste("<b>Series Name:  </b>", seriestable$soilseriesname, "<br>",
                                                                                                           "<b>Acres:  </b>", seriestable$acres, "<br>",
                                                                                                           "<b>Status:  </b>", seriestable$soilseriesstatus, "<br>",
                                                                                                           "<b>Benchmark:  </b>", seriestable$benchmarksoilflag, "<br>",
                                                                                                           "<b>STATSGO:  </b>", seriestable$statsgoflag, "<br>",
                                                                                                           "<b>Region Resp:  </b>", seriestable$mlraoffice, "<br>",
                                                                                                           "<b>SSRSYM:  </b>", seriestable$ssr, "<br>",
                                                                                                           "<b>Type Loc. ST:  </b>", seriestable$areasymbol, "<br>",
                                                                                                           "<b>Classification:  </b>", seriestable$taxclname, "<br>",
                                                                                                           "<b>Proposed:  </b>", seriestable$originyear, "<br>",
                                                                                                           "<b>Established:  </b>", seriestable$establishedyear, "<br>",
                                                                                                           "<b>Updated:  </b>", seriestable$soiltaxclasslastupdated, "<br>"
      ), group="Series")
      
      m2<-addLayersControl(m2, baseGroups=c("Stamen TonerLite", "ESRI Street", "ESRI Topo", "ESRI Imagery","Open Street Map", "Stamen Terrain", "Hillshade"),overlayGroups=c("Soil Polygons", "Series", "MLRA"))
      m2<-addLegend(m2, pal=pal2, position="bottomleft", values= seriestable$soilseriesname)
      incProgress(1/10, detail =paste("Your Map is on its way!"))
    })
    m2
  })

  output$projectextentdownload<- downloadHandler(      
    filename = function() {input$extentsubmit
      paste("projectextent", Sys.Date(), ".zip", sep="")
    },
    content= function(file) {

      withProgress(message="Preparing Extent for Saving", detail="Please Wait", value=1, {
        sp.final <- extentdata()
        writeOGR(sp.final, ".", "extent", driver = "ESRI Shapefile", overwrite_layer=TRUE)
        
        tempExtentzip <-file.path(tempdir(), "extent.zip")
        file.copy("extent.zip", tempExtentzip, overwrite=TRUE)
        
        # zip(file, c("extent.dbf","extent.prj","extent.shp","extent.shx"), zip = "C:/RBuildTools/3.4/bin/zip.exe")
        zip(file, c("extent.dbf","extent.prj","extent.shp","extent.shx"))
      }
      )}
  )
  
}

#combine the user interface and server to generate the shiny app
shinyApp(ui = ui, server = server)
