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

#create a dashboard header

header <- dashboardHeader(title = span(
  tagList(
    img(src = "logo.png"),
    "Amoozemeter Measured Saturated Hydrualic Conductivity Processing App"
  )
), titleWidth = 750)

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
                                "Ksat Data",
                                tabName = "loaddata",
                                icon = icon("map-pin")
                              ),
                              
                              #Source Code Menu
                              menuItem(
                                "Source Code",
                                icon = icon("file-code-o"),
                                href = "https://github.com/ncss-tech/vitrusa/tree/master/r11_akp_app"
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
      titlePanel("Welcome to the Ksat App"),
      verticalLayout(
        infoBox(
          "About this App",
          "The Amoozemeter Measured Saturated Hydrualic Conductivity Processing App is designed to curate data gathered with amoozemeters and format it for upload into NASIS.",
          width = 12,
          icon = icon("users"),
          color = "blue"
        ),
        box(includeHTML("home.html"), width = 12),
        box("This application was developed by John Hammerly.", width =
              12)
      )
    ),
    
    #Analysis Report tab
    
    tabItem(
      tabName = "loaddata",
      class = "active",
      titlePanel("Processing Amoozemeter Data is as easy as 1 - 2 - 3"),
      verticalLayout(fluidRow(
        column(
          width = 3,
          tags$h3("1. Load"),
          box(
            p("Inputs marked with an asterisk (*) are required."),
            p("Click submit button to begin processing."),
            uiOutput("am_inputs"),
            uiOutput("am_upedonid"),
            uiOutput("am_sensor"),
            actionButton("submit", "Submit"),
            status = "primary",
            title = "Inputs",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12
          )
        ),
        column(
          width = 6,
          tags$h3("2. Review"),
          
          tabBox(
            title = "Input Data",
            tabPanel("Plot", plotlyOutput("iniplot")),
            tabPanel("Table", DT::dataTableOutput("ks_data")),
            width = 12
          ),
          tabBox(
            title = "Processed Data",
            tabPanel("Plot", plotlyOutput("finalplot")),
            tabPanel("Table", DT::dataTableOutput("exporttable")),
            width = 12
          )
        ),
        column(
          width = 3,
          tags$h3("3. Download"),
          box(
            uiOutput("downloadtext"),
            uiOutput("am_xlsfile"),
            status = "primary",
            title = "Download File",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12
          )
        ),
        
        box("This application was developed by John Hammerly.", width = 12)
      ))
    )
  ))

ui <-
  dashboardPage(header, sidebar, body, title = "Amoozemeter Measured Saturated Hydrualic Conductivity Processing App")

#create a function for the server
server <- function(input, output, session) {
  
  dataModal <- function(failed = FALSE) {
    modalDialog(title="Ksat Data Processing App Message:",
                
                if (failed)
                  div(tags$p("No file(s) selected for processing.  Please upload a dataset.", style = "color: red;")),
                easyClose=TRUE,
                footer = tagList(
                  modalButton("Try Again")
                )
    )
  }
  
  observeEvent(input$submit, {
    # Check that data object exists and is data frame.
    if (is.null(isolate(input$adatainput))) {
      showModal(dataModal(failed = TRUE))
    }
  })
  
  output$am_inputs <- renderUI({
    fileInput(
      "adatainput",
      "Ksat Data File(s)*",
      multiple = FALSE,
      accept = ".xlsx",
      buttonLabel = "Browse...",
      placeholder = "No file selected"
    )
    
  })
  

  
  output$am_upedonid <- renderUI({
    textInput("upedonid",
              "User Pedon ID*",
              value = "",
              placeholder = "2015IA101001")
    
  })
  
  output$am_sensor <- renderUI({
    selectInput(
      "sensor",
      "Test Method",
      selected = "",
      choices = c(
        "amoozemeter",
        "double ring",
        "single ring"
      ),
      multiple = FALSE
    )
    
  })
  
  k_data <- reactive({
    input$submit
    
    withProgress(message="Processing Data", value=1,{
      
      if (is.null(isolate(input$adatainput)))
        return(NULL)
      
      adata <- isolate(input$adatainput)
      
      try({
        sheet1<-read_xlsx(isolate(adata$datapath), "Amoozemeter Ksat Calc_1hor1loc", "B3", col_names = "date", col_types = "date")
        
        sheet1m<-read_xlsx(isolate(adata$datapath), "Amoozemeter Ksat Calc_1hor1loc", "G43", col_names = "k_mean", col_types = "numeric")
        
        sheet1sd<-read_xlsx(isolate(adata$datapath), "Amoozemeter Ksat Calc_1hor1loc", "G44", col_names = "k_sd", col_types = "numeric")
        
        sheet1horizon<-read_xlsx(isolate(adata$datapath), "Amoozemeter Ksat Calc_1hor1loc", "B9", col_names = "name", col_types = "text")
        
        sheet1data<-read_xlsx(isolate(adata$datapath), "Amoozemeter Ksat Calc_1hor1loc", "B2", col_names = "collector", col_types = "text")
        
        s1<-bind_cols(sheet1, sheet1data, sheet1horizon, sheet1m, sheet1sd)
        
      }, silent = TRUE)
      
      #SHEET 2
      
      try({sheet2<-read_xlsx(isolate(adata$datapath), "Amoozemeter Ksat Calc_1hor1loc", "B51", col_names = "date", col_types = "date")
      
      sheet2m<-read_xlsx(isolate(adata$datapath), "Amoozemeter Ksat Calc_1hor1loc", "G91", col_names = "k_mean", col_types = "numeric")
      
      sheet2sd<-read_xlsx(isolate(adata$datapath), "Amoozemeter Ksat Calc_1hor1loc", "G92", col_names = "k_sd", col_types = "numeric")
      
      sheet2horizon<-read_xlsx(isolate(adata$datapath), "Amoozemeter Ksat Calc_1hor1loc", "B57", col_names = "name", col_types = "text")
      
      sheet2data<-read_xlsx(isolate(adata$datapath), "Amoozemeter Ksat Calc_1hor1loc", "B50", col_names = "collector", col_types = "text")
      
      s2<-bind_cols(sheet2, sheet2data, sheet2horizon, sheet2m, sheet2sd)
      
      }, silent = TRUE)
      
      #SHEET 3
      
      try({sheet3<-read_xlsx(isolate(adata$datapath), "Amoozemeter Ksat Calc_1hor1loc", "B99", col_names = "date", col_types = "date")
      
      sheet3m<-read_xlsx(isolate(adata$datapath), "Amoozemeter Ksat Calc_1hor1loc", "G139", col_names = "k_mean", col_types = "numeric")
      
      sheet3sd<-read_xlsx(isolate(adata$datapath), "Amoozemeter Ksat Calc_1hor1loc", "G140", col_names = "k_sd", col_types = "numeric")
      
      sheet3horizon<-read_xlsx(isolate(adata$datapath), "Amoozemeter Ksat Calc_1hor1loc", "B105", col_names = "name", col_types = "text")
      
      sheet3data<-read_xlsx(isolate(adata$datapath), "Amoozemeter Ksat Calc_1hor1loc", "B98", col_names = "collector", col_types = "text")
      
      s3<-bind_cols(sheet3, sheet3data, sheet3horizon, sheet3m, sheet3sd)
      
      }, silent = TRUE)
      
      #SHEET 4
      
      try({sheet4<-read_xlsx(isolate(adata$datapath), "Amoozemeter Ksat Calc_1hor1loc", "B147", col_names = "date", col_types = "date")
      
      sheet4m<-read_xlsx(isolate(adata$datapath), "Amoozemeter Ksat Calc_1hor1loc", "G187", col_names = "k", col_types = "numeric")
      
      sheet4sd<-read_xlsx(isolate(adata$datapath), "Amoozemeter Ksat Calc_1hor1loc", "G188", col_names = "k", col_types = "numeric")
      
      sheet4horizon<-read_xlsx(isolate(adata$datapath), "Amoozemeter Ksat Calc_1hor1loc", "B153", col_names = "name", col_types = "text")
      
      sheet4data<-read_xlsx(isolate(adata$datapath), "Amoozemeter Ksat Calc_1hor1loc", "B146", col_names = "collector", col_types = "text")
      
      s4<-bind_cols(sheet4, sheet4data, sheet4horizon, sheet4m, sheet4sd)
      
      }, silent = TRUE)
      
      #SHEET 5
      
      try({sheet5<-read_xlsx(isolate(adata$datapath), "Amoozemeter Ksat Calc_1hor1loc", "B195", col_names = "date", col_types = "date")
      
      sheet5m<-read_xlsx(isolate(adata$datapath), "Amoozemeter Ksat Calc_1hor1loc", "G235", col_names = "k_mean", col_types = "numeric")
      
      sheet5sd<-read_xlsx(isolate(adata$datapath), "Amoozemeter Ksat Calc_1hor1loc", "G236", col_names = "k_sd", col_types = "numeric")
      
      sheet5horizon<-read_xlsx(isolate(adata$datapath), "Amoozemeter Ksat Calc_1hor1loc", "B201", col_names = "name", col_types = "text")
      
      sheet5data<-read_xlsx(isolate(adata$datapath), "Amoozemeter Ksat Calc_1hor1loc", "B194", col_names = "collector", col_types = "text")
      
      s5<-bind_cols(sheet5, sheet5data, sheet5horizon, sheet5m, sheet5sd)
      
      }, silent = TRUE)
      
      #COMBINE SHEETS
      
      
      try({
        all_data<-bind_rows(s1)
        all_data<-bind_rows(s1, s2)
        all_data<-bind_rows(s1, s2, s3)
        all_data<-bind_rows(s1, s2, s3, s4)
        all_data<-bind_rows(s1, s2, s3, s4, s5)}
        , silent = TRUE)
      
      all_data$date <- as.Date(all_data$date)
      
      return(all_data)
    })
  })
  
  output$ks_data <- DT::renderDataTable({
    ksat_data <- k_data()
    
  }, options = list(pageLength = 5, scrollX = "100%"))
  
  output$iniplot <- renderPlotly({
    if (input$submit == 0) {
      return()
    }
    
    else if (is.null(isolate(input$adatainput))) {
      return(NULL)
    }
    
    else
      ({
        
        withProgress(message="Processing Data", value=1,{
          
          all_data <- k_data()
          
          all_data$name <- factor(all_data$name, levels = all_data$name[order(all_data$name, decreasing = TRUE)])
          
          # Plot all data
          kplot <-ggplot(all_data, aes(y = k_mean, x = name)) +
            geom_point() +
            geom_linerange(ymin = all_data$k_mean-all_data$k_sd, ymax = all_data$k_mean+all_data$k_sd) +
            coord_flip() +
            ylim(min(all_data$k_mean-(2*all_data$k_sd)), max(all_data$k_mean+(2*all_data$k_sd))) +
            labs(x = "Horizons", y = "Mean Ksat (cm/hr)", title = "Input Ksat Data") +
            theme(plot.margin=unit(c(1,1,1,1),"cm"))
          ggplotly(kplot)
        })
      })
  })
  
  finalksat <-
    
    reactive({
      input$submit

      withProgress(message="Processing Data", value=1,{
        all_data <- k_data()
        all_data$k_mean_um <- round(all_data$k_mean*2.77777778, 4)
        all_data$k_sd_um <- round(all_data$k_sd*2.77777778, 2)
        
        all_data <- separate_rows(all_data, name)
        
        all_data <- filter(all_data, name != "and")
        
        all_data <- mutate(all_data, upedonid = isolate(input$upedonid))
        
        all_data <- mutate(all_data, sathydcondmethod = isolate(input$sensor))
        
        all_data <- mutate(all_data, k_mean = NULL)
        
        all_data <- mutate(all_data, k_sd = NULL)
        
        all_data <- mutate(all_data, usiteid = isolate(upedonid))
        
        all_data <- all_data[,c(8, 6, 3, 1, 2, 4, 5, 7)]
        
        names(all_data)[3]<-"hzname"
        names(all_data)[4]<-"testdate"
        names(all_data)[5]<-"datacollector"
        names(all_data)[6]<-"sathydcondmean"
        names(all_data)[7]<-"sathydcondstd"
        
        return(all_data)
      })
      
    })
  
  output$exporttable <- DT::renderDataTable({
    if (input$submit == 0)
      
      return()
    
    else if (is.null(isolate(input$adatainput)))
      return(NULL)
    
    shiny::validate(need(input$adatainput, message = "Please Upload a Dataset."))
    
    f_ksat_table <- finalksat()
    
    f_ksat_table
  }, options = list(pageLength = 5, scrollX = "100%"))
  
  output$file <- downloadHandler(
    filename = function() {
      paste0("HorizonKsat", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      
      horizonksat_table <- data.frame(finalksat())

      horizonksat_table$testdate <- as.character(horizonksat_table$testdate)
      
      # Create an empty workbook
      wb <- createWorkbook()
      
      # Create a sheet within the workbook
      sheet <- createSheet(wb, sheetName = "HorizonKsat")
      
      # Add monitoring data
      addDataFrame(
        horizonksat_table,
        sheet,
        startRow = 3,
        startColumn = 3,
        row.names = F
      )
      
      # Create a data.frame with the name and version of the workbook required for NASIS
      wbnamecell <- data.frame("HorizonKsat", "1.0")
      
      # Add the required NASIS data.frame to the workbook
      addDataFrame(
        wbnamecell,
        sheet,
        startRow = 1,
        startColumn = 1,
        col.names = F,
        row.names = F
      )
      
      # Save the workbook
      saveWorkbook(wb, file)
    }
  )
  
  output$am_xlsfile <- renderUI({
    if (input$submit == 0)
      
      return()
    
    downloadButton("file", "Download")
    
  })
  
  output$finalplot <- renderPlotly({
    if (input$submit == 0) {
      return()
    }
    
    else if (is.null(isolate(input$adatainput))) {
      return(NULL)
    }
    
    else
      ({
        
        withProgress(message="Processing Data", value=1,{
          
          all_data <- finalksat()
          
          all_data$hzname <- factor(all_data$hzname, levels = all_data$hzname[order(all_data$hzname, decreasing = TRUE)])
          
          # Plot all data
          fkplot <-ggplot(all_data, aes(y = sathydcondmean, x = hzname)) +
            geom_point() +
            geom_linerange(ymin = all_data$sathydcondmean-all_data$sathydcondstd, ymax = all_data$sathydcondmean+all_data$sathydcondstd) +
            coord_flip() +
            ylim(min(all_data$sathydcondmean-(2*all_data$sathydcondstd)), max(all_data$sathydcondmean+(2*all_data$sathydcondstd))) +
            labs(x = "Horizons", y = "Mean Ksat (micrometers/second)", title = "Processed Ksat Data") +
            theme(plot.margin=unit(c(1,1,1,1),"cm"))
          ggplotly(fkplot)
        })
      })
  })
  
  output$downloadtext <- renderUI({
    if (input$submit == 0)
      return()
    
    shiny::validate(
      need(input$adatainput, message = "No file(s) selected for processing.  Please Upload a Dataset.")
    )
    
    tags$p("Click  download to save processed file for NASIS upload -")
    
  })
  
}


#combine the user interface and server to generate the shiny app
shinyApp(ui = ui, server = server)