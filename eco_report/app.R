library(httr)
library(base64enc)
library(soilDB)
library(dplyr)
library(knitr)
library(kableExtra)
library(shinydashboard)

header<-dashboardHeader(
    title= span(tagList("Ecosite Report")), titleWidth=250)

sidebar<-dashboardSidebar(collapsed = TRUE, width = 250,
                           sidebarMenu(id="tabs",
                                       
                                       #Home Page Menu    
                                       menuItem("Home", tabName="Home", selected=TRUE, icon=icon("home"))
) )

body<-dashboardBody(

  #styling for the progress bar position
  tags$style(type="text/css", ".shiny-notification{position: fixed; top:33%;left:33%;right:33%;}"),
  
    tabItems(
        #Home tab
        tabItem(tabName="Home",
                verticalLayout(
                    box(fileInput("shpload", "Upload file"), width=12),
                    box(htmlOutput("report"), width=12),
                    box("This application was developed by John Hammerly and Richard Reid", width=12)
                )
        )
    )
)

ui <- dashboardPage(header, sidebar, body, title = "Ecological Report")

server <- function(input, output, session){
    
    output$report <- renderText({
        req(input$shpload)
        withProgress(message="Processing Data", value=0,{
    
    url <- "https://sdmdataaccess.sc.egov.usda.gov/Tabular/post.rest"
    setProgress(value = .25, message = "unzipping shapefile")
    shpfile <- unzip(isolate(input$shpload$datapath))
    # shpfile <- unzip("M:/Temp/Temp.zip")

    test <- base64encode(what = shpfile[grep(".shp$", shpfile)])
    test2 <- base64encode(what = shpfile[grep(".shx$", shpfile)])
    test3 <- base64encode(what = shpfile[grep(".prj$", shpfile)])
    test4 <- base64encode(what = shpfile[grep(".dbf$", shpfile)])
    setProgress(value = .5, message = "sending file to Soil Data Access")
    r <- POST(url, body = paste0('{"SERVICE":"aoi","REQUEST":"create","AOICOORDS":"shapefile,', test, ",", test2, ",", test3, ",", test4,'"}') , encode = "raw")
    
    aoiid <- paste0(unlist(content(r)))
    setProgress(value = .75, message = "sending query to Soil Data Access")
    querycode <- paste0("
SELECT DISTINCT
p.AoiPartName,
MapUnitSymbol AS 'Map unit symbol', 
MapUnitName AS 'Map unit name', 

compname +
  ' (' +
  CAST(comppct_r AS varchar) +
    '%)' AS 'Component name (percent)',
comppct_r,
(SELECT ' ' + ecoclassid + 
  '--' +
ecoclassname
FROM coecoclass p4
WHERE p4.cokey = p3.cokey AND ecoclassid like '%R%'
FOR XML PATH('') ) AS 'Ecological site',


SUM(pl.AreaAcres) OVER(PARTITION BY pl.AoiPartId, pl.MapUnitKey, p1.cokey, ecoclassid) AS 'MapUnit Acres in Part',

SUM(pl.AreaAcres) OVER(PARTITION BY pl.AoiPartId, pl.MapUnitKey, p1.cokey, ecoclassid) / p.AoiPartAcreage * 100 AS 'MapUnit % of AOI Part',
SUM(pl.AreaAcres) OVER(PARTITION BY pl.AoiPartId, pl.MapUnitKey, p1.cokey, ecoclassid) / a.AreaAcres * 100 AS 'MapUnit % of AOI',

p.AoiPartAcreage AS 'Total AOI Part Acres',
a.AreaAcres AS 'Total AOI Acres'



FROM SDA_Get_AoiSoilMapunitPolygon_By_AoiId(", aoiid, ") AS pl JOIN SDA_Get_AoiPart_By_AoiId(", aoiid, ") AS p ON p.AoiPartId = pl.AoiPartId JOIN SDA_Get_Aoi_By_AoiId(", aoiid, ") AS a ON a.AoiID = p.AoiId JOIN component AS p1 ON p1.mukey = pl.MapUnitKey
LEFT JOIN coecoclass AS p3 ON p3.cokey = p1.cokey AND ecoclassid like '%R%'")
    
    qtable <-SDA_query(querycode)
    setProgress(value = 1, message = "formatting data")   
    rslt <- qtable %>% arrange(AoiPartName, `Map unit symbol`, `Map unit name`, desc(comppct_r))
    
    rslt <- rslt[, -5]
    
    ntest <- kable(rslt) %>% kable_styling(bootstrap_options = c("striped", "hover")) %>% collapse_rows(columns = c(1:4, 6:10), valign = "top")
    return(ntest)         
    }

    )
    })
        
}

# Run the application 
shinyApp(ui = ui, server = server)
