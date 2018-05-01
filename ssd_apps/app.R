library(shiny)
library(shinydashboard)
library(soilDB)

header<-dashboardHeader(
  title="SSD Web Apps")

sidebar<-dashboardSidebar(disable = TRUE)

              
body<-dashboardBody(
  infoBoxOutput("s_lims"),
  infoBoxOutput("s_sda"),
  infoBoxOutput("s_sw"),
  box(
  includeHTML("home.html"),
width=12)
)
  
ui <- dashboardPage(header, sidebar, body)

server <- function(input, output){
  
  output$s_lims <- renderInfoBox({
    infoBox(
      "LIMS STATUS", if(is.null(parseWebReport("https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=get_projectmapunit_from_LIMS", ""))) paste0("Not Operational") else paste0("Operational"), color = if(is.null(parseWebReport("https://nasis.sc.egov.usda.gov/NasisReportsWebSite/limsreport.aspx?report_name=get_projectmapunit_from_LIMS", ""))) paste0("red") else paste0("green"), fill = TRUE, icon = icon("flask")
    )
    
  })
  
  output$s_sda <- renderInfoBox({
    infoBox(
      "SDA STATUS", if(is.null(get_mapunit_from_SDA(WHERE = "areasymbol = 'IA171' AND musym = '478G'"))) paste0("Not Operational") else paste0("Operational"), color = if(is.null(get_mapunit_from_SDA(WHERE = "areasymbol = 'IA171' AND musym = '478G'"))) paste0("red") else paste0("green"), fill = TRUE, icon = icon("database")
    )
    
  })
  
  output$s_sw <- renderInfoBox({
    infoBox(
      "SOIL WEB STATUS", if(is.null(fetchOSD("Marshall"))) paste0("Not Operational") else paste0("Operational"), color = if(is.null(fetchOSD("Marshall"))) paste0("red") else paste0("green"), fill = TRUE, icon = icon("map")
    )
    
  })
  
}

shinyApp(ui = ui, server = server)