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

#create a dashboard header

header <- dashboardHeader(title = span(
  tagList(
    img(src = "logo.png"),
    "Amoozemeter Measured Saturated Hydraulic Conductivity Processing App"
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
          "The Amoozemeter Measured Saturated Hydraulic Conductivity Processing App is designed to curate data gathered with amoozemeters and format it for upload into NASIS.",
          width = 12,
          icon = icon("database"),
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
            uiOutput("sname"),
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
            tabPanel("Box Plot", plotlyOutput("boxplot")),
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
  dashboardPage(header, sidebar, body, title = "Amoozemeter Measured Saturated Hydraulic Conductivity Processing App")

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
  
  output$sname <- renderUI({
    radioButtons("sname",
              "Data Collection:",
              choiceNames = c("Single Horizon", "Many", "Field Sheet"),
              choiceValues = c("Amoozemeter Ksat Calc_1hor1loc", "Amoozemeter Ksat Calculation", "Field Sheet")
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
      
      fetchKsat <- function(xlsxpath, ksatsht, ksrange) {
        
        s_col <- paste(c(1:10), sep = ",")
        
        ds1 <-
          read_xlsx(
            xlsxpath,
            ksatsht,
            ksrange,
            col_names = s_col,
            col_types = "text"
          )
        
        user <- ds1[2, 2]
        date <- ds1[3, 2]
        permeameter <- ds1[3, 7]
        location <- ds1[4, 2]
        iniat <- ds1[4, 8]
        ssa <- ds1[5, 2]
        finat <- ds1[5, 8]
        series <- ds1[7, 2]
        smc <- ds1[7, 8]
        pn <- ds1[8, 2]
        hzname <- ds1[9, 2]
        hd <- ds1[12, 2]
        wl <- ds1[12, 8]
        dbtss <- ds1[13, 2]
        iniwl <- ds1[13, 8]
        finwl <- ds1[14, 8]
        dwd <- ds1[14, 2]
        chtts <- ds1[15, 2]
        ahr <- ds1[15, 8]
        ocu <- ds1[17, 4]
        mksat <- ds1[43, 7]
        sdksat <- ds1[44, 7]
        ksatclass <- ds1[45, 8]
        rmksat <- ds1[20, 10]
        rksatclass <- ds1[22, 10]
        
        ct <- ds1[24:42, 3]
        diw <- ds1[24:42, 1]
        et <- ds1[24:42, 4]
        ksat <- ds1[24:42, 7]
        oc <- ds1[24:42, 2]
        of <- ds1[24:42, 6]
        
        fds1 <- bind_cols(diw, oc, ct, et, of, ksat)
        
        fds1 <- drop_na(fds1, 1)
        
        colnames(fds1) <- c('diw',	'oc',	'ct',	'et',	'of',	'ksat')
        
        ids1 <-
          bind_cols(
            user,
            date,
            permeameter,
            location,
            iniat,
            ssa,
            finat,
            series,
            smc,
            pn,
            hzname,
            hd,
            wl,
            dbtss,
            iniwl,
            finwl,
            dwd,
            chtts,
            ahr,
            ocu,
            mksat,
            sdksat,
            ksatclass,
            rmksat,
            rksatclass
          )
        
        colnames(ids1) <-
          c(
            'user',
            'date',
            'permeameter',
            'location',
            'iniat',
            'ssa',
            'finat',
            'series',
            'smc',
            'pn',
            'hzname',
            'hd',
            'wl',
            'dbtss',
            'iniwl',
            'finwl',
            'dwd',
            'chtts',
            'ahr',
            'ocu',
            'mksat',
            'sdksat',
            'ksatclass',
            'rmksat',
            'rksatclass'
          )
        
        fds1<-mutate(fds1, j=1)
        
        ids1<-mutate(ids1, j=1)
        
        final <- inner_join(ids1, fds1, by="j")
        
        final$date <- as_date(as.numeric(final$date), origin = "1899-12-30")
        final$iniat <- as.numeric(final$iniat)
        final$finat <- as.numeric(final$finat)
        final$hd <- as.numeric(final$hd)
        final$wl <- as.numeric(final$wl)
        final$dbtss <- as.numeric(final$dbtss)
        final$iniwl <- as.numeric(final$iniwl)
        final$finwl <- as.numeric(final$finwl)
        final$dwd <- as.numeric(final$dwd)
        final$chtts <- as.numeric(final$chtts)
        final$ahr <- as.numeric(final$ahr)
        final$ocu <- as.numeric(final$ocu)
        final$mksat <- as.numeric(final$mksat)
        final$sdksat <- as.numeric(final$sdksat)
        final$rmksat <- as.numeric(final$rmksat)
        final$j <- NULL
        final$diw <- as.numeric(final$diw)
        final$oc <- as.numeric(final$oc)
        final$ct <- as_datetime(as.numeric(final$ct)*86400+1, origin = final$date)
        final$et <- as.numeric(final$et)
        final$of <- as.numeric(final$of)
        final$ksat <- as.numeric(final$ksat)
        final$permeameter <- replace_na(final$permeameter, "Unknown")
        final$pn <- replace_na(final$pn, "Unknown")
        final$repnum <- as.numeric(as.factor(final$pn))
        
        return(final)
      }

      s1<-fetchKsat(isolate(adata$datapath),isolate(input$sname),"A1:J48")
      s2<-fetchKsat(isolate(adata$datapath),isolate(input$sname),"A49:J96")
      s3<-fetchKsat(isolate(adata$datapath),isolate(input$sname),"A97:J144")
      s4<-fetchKsat(isolate(adata$datapath),isolate(input$sname),"A145:J192")
      s5<-fetchKsat(isolate(adata$datapath),isolate(input$sname),"A193:J240")
      
      all_data <- bind_rows(s1,s2,s3,s4,s5)
      
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
          
          # Plot all data
          kplot <-ggplot(all_data, aes(y = ksat, x = permeameter)) +
            facet_grid(hzname ~ .) +
            coord_flip() +
            geom_point() +
            ylim(min(all_data$mksat-(2*all_data$sdksat)), max(all_data$mksat+(2*all_data$sdksat))) +
            labs(x = "Permeameter #", y = "Ksat (cm/hr)", title = "Input Ksat Data") +
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
        all_data$rmksat <- round(all_data$rmksat*2.77777778, 4)
        # all_data$rsdksat <- round(all_data$rsdksat*2.77777778, 2)
        all_data$mksat <- round(all_data$mksat*2.77777778, 4)
        all_data$sdksat <- round(all_data$sdksat*2.77777778, 4)
        all_data$ksat <- round(all_data$ksat*2.77777778, 4)
        
        all_data <- separate_rows(all_data, hzname)
        
        all_data <- filter(all_data, hzname != "and")
        
        all_data <- mutate(all_data, upedonid = isolate(input$upedonid))
        
        all_data <- mutate(all_data, sathydcondmethod = isolate(input$sensor))
        
        all_data <- mutate(all_data, usiteid = isolate(upedonid))
        
        all_data <- mutate(all_data, steadystateflag = 1)
        
        # all_data <- mutate(all_data, oc = replace(oc, oc==20, 1))
        # 
        # all_data <- mutate(all_data, oc = replace(oc, oc==105, 2))
        
        g_all_data <- group_by(all_data, pn, hzname, permeameter)
        
        all_data <- mutate(g_all_data, ct = cumsum(et))
        
        all_data <- ungroup(all_data)
        
        all_data <- mutate(all_data, notes = paste0("Permeameter Number:  ", all_data$permeameter, "\n", "Location:  ", all_data$location, "\n", "Soil Survey Area:  ", ssa, "\n", "Initial Air Temperature (F):  ", all_data$iniat, "\n", "Final Air Temperature (F):  ", all_data$finat, "\n", "Series:  ", all_data$series, "\n", "Soil Moisture Content:  ", all_data$smc, "\n", "Pedon Number:  ", all_data$pn, "\n", "Actual water level in hole (cm):  ", all_data$wl, "\n", "Distance from Bottom of Bubble Tube to soil surface (cm):  ", all_data$dbtss, "\n", "Desired water depth in hole (cm):  ", all_data$dwd, "\n", "CHT tube setting (cm):  ", all_data$chtts, "\n", "Outflow chamber used:  ", all_data$ocu, "\n", "Mean Ksat class:  ", all_data$rksatclass))
        
        
        names(all_data)[1]<-"datacollector"
        names(all_data)[2]<-"testdate"
        names(all_data)[12]<-"boreholedepth"
        names(all_data)[15]<-"boreholewaterlevelinit"
        names(all_data)[16]<-"boreholewaterlevelfinal"
        names(all_data)[19]<-"boreholeradius"
        names(all_data)[21]<-"sathydcondrepmean"
        names(all_data)[22]<-"sathydcondrepstd"
        names(all_data)[23]<-"sathydcondclass"
        names(all_data)[24]<-"sathydcondmean"
        names(all_data)[26]<-"waterdrop"
        names(all_data)[27]<-"outflowchamberconvfact"
        names(all_data)[29]<-"deltatime"
        names(all_data)[31]<-"sathydcondmeasured"
  

        
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
      wbnamecell <- data.frame("HorizonKsat", "1.1")
      
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
          
          # all_data$hzname <- factor(all_data$hzname, levels = all_data$hzname[order(all_data$hzname, decreasing = TRUE)])
          
          # Plot all data
          fkplot <-ggplot(all_data, aes(y = sathydcondmeasured, x = ct, color = interaction(permeameter, pn,sep="-"))) +
            geom_line() +
            facet_grid(hzname ~ ., switch = "both") +
            ylim(min(all_data$sathydcondmeasured-(1.1*all_data$sathydcondmeasured)), max(all_data$sathydcondmeasured+(1.1*all_data$sathydcondmeasured))) +
            labs(x = "Elapsed Time \n (min)", y = "Ksat \n (micrometers/second)", title = "Processed Ksat Data") +
            theme(plot.margin=unit(c(1,1,1,1),"cm")) +
            guides(color=guide_legend(title="Perm # - Pedn #"))
          gp <- ggplotly(fkplot)
          gp[['x']][['layout']][['annotations']][[2]][['x']] <- -0.05
          layout(gp, margin = list(l=75))
        })
      })
  })
  
  output$boxplot <- renderPlotly({
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
          
          # Plot all data
          fkplot <-ggplot(all_data, aes(y = sathydcondmeasured, x = permeameter)) +
            geom_boxplot() +
            coord_flip() +
            facet_grid(hzname ~ ., switch = "both") +
            ylim(min(all_data$sathydcondmeasured-(1.1*all_data$sathydcondmeasured)), max(all_data$sathydcondmeasured+(1.1*all_data$sathydcondmeasured))) +
            labs(x = "Permeameter #", y = "Ksat \n (micrometers/second)", title = "Processed Ksat Data") +
            theme(plot.margin=unit(c(1,1,1,1),"cm")) +
            guides(color=guide_legend(title="Perm # - Pedn #"))
          gp <- ggplotly(fkplot)
          gp[['x']][['layout']][['annotations']][[2]][['x']] <- -0.05
          layout(gp, margin = list(l=75))
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