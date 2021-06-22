library(shiny)
library(shinydashboard)
library(plyr)
library(asdreader)
library(stringi)
library(cluster)
library(spectacles)
library(ggplot2)
library(readxl)
library(prospectr)
library(pls)
library(tidyverse)
library(shinyWidgets)
library(plotly)
library(RColorBrewer)
library(enpls)
library(chemometrics)
library(resemble)
library(DT)
library(aqp)
library(tactile)
library(reshape2)
library(signal)
library(rlang)
library(profvis)

# TODO: sanity check: build in validation so that on import the files are checked for the same number of rows.

# TODO: make inputs splice correction ranges dependent on the wl range selected
# TODO: add some hzname aggregation options via genhz logic - possibly a 'Plot Aggregated Spectra' tab
# aggregate spectra by taking the mean values across spectra within each hzname group
#d_ag <- aggregate_spectra(d, fun = mean, id = "hzname")
# TODO: test label_field for categorical or continuous and adjust color ramps in plot

## TODO: model columns should be numeric columns only otherwise the app will crash
## ISSUE: layer_seq is numeric but not useful - remove as well

#Define UI
#header================================

    header <- dashboardHeader(title = "Soil Spectroscopy")
    
    
#sidebar==============================  
    
    sidebar <-  dashboardSidebar(
                  sidebarMenu(id = "sidebarid",
                menuItem("About", 
                         tabName = "dashboard", 
                         icon = icon("dashboard")),
                menuItem("Load and Explore Data", 
                         tabName = "loaddata", 
                         icon = icon("table")),
        conditionalPanel('input.sidebarid == "loaddata"',
                      actionButton("example", 
                            "Load Example Spectra")),
                menuItem("Interactive Spectra Plot", 
                         tabName = "interactive", 
                         icon = icon("chart-area")),
                menuItem("Outlier Detection", 
                         tabName = "outlierdetection", 
                         icon = icon("glasses")),
                menuItem("Model", 
                         tabName = "model", 
                         icon = icon("code")),
                menuItem("Test Model", 
                         tabName = "testmodel", 
                         icon = icon("check"))
                  )
                )
    
#body============================================ 
    
    body <- dashboardBody(
          tabItems(
                tabItem(tabName = "dashboard",
             h2('About'),           
             p('This app was designed for exploring spectral data and building models from spectra of soil samples and associated laboratory data.  The app allows the user to load data, explore interactive plots, randomly split the data and build a Partial Least Squares Regression (PLSR) model from a calibration set and then test the model using a validation set.  Additional features include a tab for identifying outliers among the spectral data or among the lab data.'),
             p('The app includes example data on the Load and Explore Data tab which will load a small dataset of VNIR spectra and associated lab data for exploring the functionality of the app.'),
            h3('Instructions and workflow:'),
            h4('1) Load Data'), 
            p(' - Data can be loaded into the app via two csv files, one of spectral data (wavelength and spectral values) and another of the lab data values with a unique ID column in the first position. These files must be of equal record length. Lab data is visible in the Table tab and a basic plot of all spectra displayed in spectra-derived Munsell soil colors can be viewed in the Plot all spectra tab.'),
            h4('2) Data Prep'),
            p(' - For VNIR data, splicing at wavelength ranges of 750:1000 and 1830:1950 is performed when the data is loaded.  Further derivation and smoothing using Savitzky-Golay filtering can be applied to the spectral data.'),
            h4('3) Interactive Spectra Plots'), 
            p(' - This interactive plot of the spectra can be used to explore the variation related to the columns in the lab data.  Hovering over the plot will show the data values and identify the sample ID for each spectra. Clicking on the objects in the legend will control which spectra are shown.'),
            h4('4) Outlier Detection'),
            p(" - Here the user can generate two different outlier plots.  One for the similarity and Mahalanobis Distance among the spectra and another which uses the Monte-Carlo resampling method for the lab data. The user can set the amount of resampling repetitions, which will effect the accuracy of the detection, as well as the time needed to run the program. The user can choose the parameters to define outliers using a multiple of the sample's standard deviation and mean. Finally, the user can use the brush tool to click and choose which points to remove. The download button downloads a new .csv file with the data that was not removed. "),
            h4('5) Model'), 
            p(' - Data can be split on the model tab using the slider for the percentage of the split to be used.  The user selects the variable in the lab data they wish to model. After clicking the Run! button the app will display a principle components plot of Root Mean Squared Error of Prediction (RMSEP). This indicates the number of principle components which will yield the least amount of error and should be used for the most robust model.'),
            h4('6) Test Model'), 
            p(' - The user selects the appropriate number of components to use in the model, runs the model and an interactive plot of model performance is displayed.  Metrics from the model can be shown by clicking the View Statistics button.  Model output and associated split datasets can be downloaded as an .Rda file for later use.'),
               tags$hr(),
                       
            p(paste0('Version 1.1 -- ', Sys.Date(), 
                     ' - designed and developed by Kian Speck and Jay Skovlin.'))
                        ),
                
              tabItem(tabName = "loaddata",
                fluidRow(
                  box(
                    title = "Options",
                    collapsible = TRUE,
                    solidHeader = TRUE,
                    status  = "primary",
                    width = 12,
#Input: user chooses own files
            column(width = 4,
        fileInput("file", "Choose Spectra File", 
            accept = c("text/csv",
                  "text/comma-seperated-values,text/plain,
                  .csv"), 
            buttonLabel = "Browse", 
            placeholder = "Choose .CSV"),
        
        fileInput("file3", "Choose Lab Data File", 
             accept = c("text/csv",
                      "text/comma-seperated-values,text/plain,
                        .csv"),
              buttonLabel = "Browse", 
              placeholder = "Choose .CSV"),
        
        uiOutput("id_select"),
       
        selectInput(inputId = "wl_select",
                           label = "Wavelength range",
                           choices = c(
                    "VNIR: 350:2500" = paste0(350, ":", 2500), 
                    "MIR: 400:4000" = paste0(400, ":", 4000)),
                    selected = "VNIR: 350:2500"),
            
        #column(width = 6,
        selectInput("splice_ranges", 
                    "Splice correction ranges", 
                    choices = c("750:1000, 1830:1950" = "750:1000, 1830:1950"),
                    selected = "750:1000, 1830:1950"),
            ),
            #),

        column(width = 2,
        checkboxInput("header","Header", TRUE),
        
        radioButtons("sep", "Separator",
                     choices = c(Semicolon = ";",
                            Comma = ",",
                            Tab = "\t"),
                            selected = ","),

        radioButtons("disp", "Display",
                     choices = c(All = "all",
                            Head = "head"),
                            selected = "all"),
        
        
        ),
          column(width = 2,
                 
        uiOutput("checkbox"),
          ),
          #column(width = 4,
        # selectInput("splice_ranges", 
        #             "Splice correction ranges", 
        #             choices = "c(750, 1000), c(1830, 1950)",
        #             selected="c(750, 1000), c(1830, 1950)"),
          #)
                )),
        fluidRow(
        tabBox(
            width = 12,
            title = "View Data",
            tabPanel("Table", 
                   DT::dataTableOutput("rendered_file"),
                    tags$hr()
             ),
            tabPanel("Plot all spectra", 
                   plotOutput("test")
             )),
          box(
            width = 8,
            title = 'Savitzky-Golay smoothing filter options',
              column(width = 4,
                numericInput("SG_p", "filter order", 
                      width = "80px", value = 4, 
                      min = 1, max = 10),
              ),
              column(width = 4,
                numericInput("SG_n", "filter length",
                             width = "80px", 
                             value = 23, min = 11, 
                             max = 51, step = 2)
                ),
                numericInput("resampling_nm", 
                           "Spectra resampling for 
                           plotting (nm)",
                           value="20", min = 5, max = 20, 
                           step = 5)
      ),

       
          
       
    )
  ),
        
        tabItem(tabName = "interactive",
              fluidRow(
                  box(width = 12,
                    title = "Options",
                    collapsible = TRUE,
                    solidHeader = TRUE,
                    status  = "primary",
          column(width = 6,         
           radioButtons("radio","Preprocessing Function:",
                choices = list("Raw Spectra" = "Reflectance",
                              "Absorbance" = "Absorbance",
                              "Standard Normal Variate Transformation" = "SNV",
                              "Robust Normal Variate Transformation" = "RNV"),
                selected = "Reflectance"),
          
           numericInput("rnv_perc", 
                        "Percentile for RNV",
                        value="0.25", min = 0.05, max = 1, 
                        step = 0.05),
          ),
           column(width = 6,
           checkboxInput("soil_colors",
                         "Apply soil colors", TRUE),
           
          uiOutput("plotlabel1"),
           
           downloadButton("download_graph", "Download Graph",
                          class = "dlButton"),
                  ),
                  )
                  ),   
             fluidRow(
             box(width = 12,
                 status = "success",
               plotlyOutput("newdata"))
             )   
      ),
      
      tabItem(tabName = "outlierdetection",
              fluidRow(
                box(
                  title = "Options",
                  collapsible = TRUE,
                  solidHeader = TRUE,
                  status  = "primary",
             tags$head(tags$style(type="text/css", "
           #loadmessage {
             position: fixed;
             top: 0px;
            left: 0px;
             width: 100%;
             padding: 5px 0px 5px 0px;
             text-align: center;
             font-weight: bold;
             font-size: 100%;
             color: #000000;
             background-color: #99CCCC;
             z-index: 105;
           }
           ")),
                  
                  radioButtons("radio_od",
                               "Choose variable for
                          outlier detection",
                               choices = list(
                                 "Spectra" = "spectra_od",
                                 "Lab Data" = "data_od"),
                               selected = "spectra_od"),
                  
                  uiOutput("selectinput2"),
                  
                  uiOutput("od_par"),
                  
                  uiOutput("od_reps"),
                  
                  actionButton("run_od", "Run!"),
                  
                  downloadButton("download_od", 
                                 "Download Outlier-Removed
                                 Data",
                                 class = "dlButton")
          )),
           conditionalPanel(
              condition="$('html').hasClass('shiny-busy')",
             tags$div("Loading...",id="loadmessage")),
           fluidRow(
           box(
             width = 12,
             status = "success",
             plotOutput("result_od",
                      brush = brushOpts(
                        id = "plot1_brush"
                      )),
  
                  h4("Brushed points"),
                  verbatimTextOutput("brush_info"))
         
        )),

      tabItem(tabName = "model",
              fluidRow(
                box(
                  title = "Options",
                  collapsible = TRUE,
                  solidHeader = TRUE,
                  status  = "primary",
                sliderInput("sampleSize", 
                    "Percent data to use in train set", 
                          min = 0, max = 100,
                          value = 75,
                          step = 5, round = 0, post = '%'),
           
              numericInput("sampleSeed", "Sample seed", 
                        value = 4556),
           
              numericInput("components","Choose the Max # of 
                        Components", value = 8),
           
              uiOutput("selectinput"),
       
              radioButtons("choice", 
                           "Choose Spectral Preprocessing",
                           
             choices = c(Reflectance = "reflectance_m",
                         Absorbance = "absorbance_m"),
                         selected = "reflectance_m"),
       
            actionButton("run", "Run!")
          )),
          fluidRow(
          box(width = 12,
              status = "success",
            plotOutput("results"))
                
              )),

        tabItem(tabName = "testmodel",
            fluidRow(
              
              box(width = 12,
                title = "Options",
                collapsible = TRUE,
                solidHeader = TRUE,
                status  = "primary",
                  column(width = 6,
                         
             uiOutput("compon"),
             
             checkboxInput("do2", 
                           "Compare Testing Parameters"),
              ),
             column(width = 6,
             uiOutput("do2_compon"),
             
             actionButton("run_test", "Run!"),
             
             downloadButton("download_model", "Download Model",
                            class = "dlButton")
           )
             )),
           fluidRow(
           box(width = 12,
               status = "success",
             splitLayout(cellWidths = c("50%", "50%"),
                         plotlyOutput("result_test"),
                         plotlyOutput("plotgraph2")),
             
             splitLayout(cellWidths = c("50%", "50%"),
                         dataTableOutput("result_table"),
                         dataTableOutput("result_table_2"))
             
           )
         
            ))
            ))
    
ui <- dashboardPage(header, sidebar, body, skin = "green")

server <- function(input,output,session){
  options(shiny.maxRequestSize=30*1024^2)
#load data ================================================
  # load example data files on server
  inFile <- reactive({
    
    if(input$example == 1){
      inFile <- as.data.frame(read.csv(file = "lab_spectra_csv.csv",
                                       header = TRUE,
                                       sep = ",")) 
      } else {
      req(input$file)
      file_user <- read.csv(input$file$datapath)
      inFile <- as.data.frame(file_user)}
  }) %>%
    bindCache(input$file, input$example)
  
  inFile3 <- reactive({
    
    if(input$example == 1) {
      read.csv(file = "lab_data_csv.csv",
               header = TRUE,
               sep = ",")
      } else {
      req(input$file3)
      inFile3 <- read.csv(input$file3$datapath,
                          header = input$header,
                          sep = input$sep)}
  }) %>%
    bindCache(input$file3, input$example)
  
  
#labdata viewer=============================================
  output$checkbox <- renderUI({
    checkboxGroupInput(inputId = "select_var",
                       label = "Select variables",
                       choices = names(inFile3()),
                       selected = names(inFile3()[1:2]))
  })
  inFile3_sel <- reactive({
    req(input$select_var)
    inFile3_sel <- inFile3() %>% select(input$select_var)
  })
  
  output$id_select <- renderUI({
    selectInput(inputId = "select_id",
                label = "Select ID Column",
                choices = names(inFile3()))
  })
  
  id_sel <- reactive({
    req(input$select_id)
    id_sel <- inFile3() %>% select(input$select_id)
  }) %>%
    bindCache(input$select_id)
  
  output$rendered_file <- DT::renderDataTable({
    if(input$disp == "head") {
      head(inFile3_sel())
    }
    else {
      inFile3_sel()
    }
  })
#visualize===================================================
  wl_app <- reactive({
    eval(parse_expr(input$wl_select))
  }) %>%
    bindCache(input$wl_select)
  
  data_app <- reactive({
    dat <- inFile3()
    id_col <- as.character(id_sel())
  dat <- dat %>%
    dplyr::select(-id_col)
  
    })
  
  color_mun <- reactive({
    # create full range spectral DF 
    #wl <- 350:2500
    wl <- wl_app()
    nir <- inFile()
    id <- unlist(id_sel())
    dd <- SpectraDataFrame(wl= wl, nir = nir, id = id,
                           units ="nm", data = inFile3())
    
    #print(str(dd))
    
    # cut to specified wl range
    d <- cut(dd, wl= 380:730)
    
    # resample from 1nm to 20nm for plotting efficiency
    d <- apply_spectra(d, prospectr::resample, wl(d), seq(min(wl(d)), max(wl(d)), 10))
    
    # melt to long format 
    a <- melt_spectra(d)
    
    # factor
    a$id <- factor(a$id)
    
    # iterate over IDs and estimate a soil color
    # using the visible part of the spectra
    z <- split(a, a$id)
    
    #TODO: may need some error handling of NA colors
    z <- lapply(z, function(i) {
      # assumes 380--730nm at 10nm resolution
      sRGB <- spec2Munsell(i$nir, convert = FALSE)
      # pack into df
      res <- data.frame(
        id = i$id[1],
        colr = rgb(sRGB),
        stringsAsFactors = FALSE
      )
      #
      return(res)
    })
    
    z <- do.call('rbind', z)
    
    z1 <- z[, c('id', 'colr')]
    rownames(z1) <- 1:nrow(z1)
    
    # join colors back to the original data
    features(dd, key="id") <- z1
    
    # melt again for plot
    m1 <- melt_spectra(dd, attr="colr")
    
    m1
    
  }) %>%
    bindCache(input$soil_colors)
  
  output$test <- renderPlot({
    
    m2 <- color_mun()
    colr <- m2$colr
    
    test <- ggplot(m2)+
      geom_line(aes(x=wl,y=nir, group=as.factor(id), 
                    colour=id))+
      scale_color_manual(values = colr)+
      labs(x= "Wavelength (nm)", y = input$radio)+
      theme_bw()
    
    test + theme(legend.position = "none")
  })

  output$plotlabel1 <- renderUI({
    selectInput(inputId = "select_var3", 
                label = "Label field", 
                choices = names(inFile3()),
                selected = names(inFile3()[1]))
  })
  
  #reflectance=================================
  reflectance <- reactive({
    req(input$radio == 'Reflectance')
    req(!is.null(input$select_var3) || !is.na(input$select_var3))
    wl <- wl_app()
    nir <- inFile()
    id <- id_sel()
    s <- SpectraDataFrame(wl= wl, 
                          nir = nir, 
                          units ="nm",
                          id = id, 
                          data = inFile3())
    
    cols <- dput(names(features(s)[-1]))
    
    #derive splicings from input format
    sp_ranges <- trimws(input$splice_ranges)
    sp1 <- str_replace_all(sp_ranges, ',', '), c(')
    sp2 <- str_replace_all(sp1, ':', ', ')
    sp3 <- paste('list(c(', sp2, '))', sep="")
    
    raw <- spectacles::splice(s, 
              locations = eval(parse_expr(sp3)))
             #locations = list(c(750, 1000), c(1830, 1950)))
    
    # apply Savitzky-Golay smoothing filter
    raw <- apply_spectra(raw, signal::sgolayfilt, n = input$SG_n, p = input$SG_p)
    
    # resample from 1nm to 20nm for plotting efficiency
    raw <- apply_spectra(raw,
                prospectr::resample, wl(raw), 
                seq(min(wl(raw)), max(wl(raw)), input$resampling_nm))
    
    m <- melt_spectra(raw, attr = cols)
    
    }) %>%
    bindCache(input$radio)
  
#absorbance=========================================
  absorbance <- reactive({
    req(input$radio == 'Absorbance')
    req(!is.null(input$select_var3) || !is.na(input$select_var3))
    
    wl <- wl_app()
    nir <- log10(1/inFile())
    id <- id_sel()
    s <- SpectraDataFrame(wl= wl, 
                          nir = nir, 
                          units ="nm",
                          id = id, 
                          data = inFile3())
    
    cols <- dput(names(features(s)[-1]))
    
    #derive splicings from input format
    sp_ranges <- trimws(input$splice_ranges)
    sp1 <- str_replace_all(sp_ranges, ',', '), c(')
    sp2 <- str_replace_all(sp1, ':', ', ')
    sp3 <- paste('list(c(', sp2, '))', sep="")
    
    abs <- spectacles::splice(s, 
                locations = eval(parse_expr(sp3)))
                #locations = list(c(750, 1000), c(1830, 1950)))
    
    # apply Savitzky-Golay smoothing filter
    abs <- apply_spectra(abs, signal::sgolayfilt, n = input$SG_n, p = input$SG_p)
    
    # resample from 1nm to 20nm for plotting efficiency
    abs <- apply_spectra(abs, prospectr::resample, 
                wl(abs), seq(min(wl(abs)), max(wl(abs)), input$resampling_nm))
    
    m <- melt_spectra(abs, attr = cols)
    
  }) %>%
    bindCache(input$radio)
  
  # snv transformation==============================
  snv <- reactive({
    req(input$radio == 'SNV')
    req(!is.null(input$select_var3) || !is.na(input$select_var3))
    wl <- wl_app()
    nir <- inFile()
    id <- id_sel()
    s <- SpectraDataFrame(wl= wl, 
                          nir = nir, 
                          units ="nm",
                          id = id, 
                          data = inFile3())
    
    cols <- dput(names(features(s)[-1]))
    
    #derive splicings from input format
    sp_ranges <- trimws(input$splice_ranges)
    sp1 <- str_replace_all(sp_ranges, ',', '), c(')
    sp2 <- str_replace_all(sp1, ':', ', ')
    sp3 <- paste('list(c(', sp2, '))', sep="")
    
    raw <- spectacles::splice(s, 
                              locations = eval(parse_expr(sp3)))
                              #locations = list(c(750, 1000), c(1830, 1950)))
    
    # apply Savitzky-Golay smoothing filter
    raw <- apply_spectra(raw, signal::sgolayfilt, n = input$SG_n, p = input$SG_p)
    
    # resample from 1nm to 20nm for plotting efficiency
    raw <- apply_spectra(raw,
                         prospectr::resample, wl(raw), 
                         seq(min(wl(raw)), max(wl(raw)), input$resampling_nm))
    
    # snv transformation
    raw <- apply_spectra(raw, spectacles::snv)
    
    m <- melt_spectra(raw, attr = cols)
    
  }) %>%
    bindCache(input$radio)
  
  # rnv transformation==============================
  rnv <- reactive({
    req(input$radio == 'RNV')
    req(!is.null(input$select_var3) || !is.na(input$select_var3))
    wl <- wl_app()
    nir <- inFile()
    id <- id_sel()
    s <- SpectraDataFrame(wl= wl, 
                          nir = nir, 
                          units ="nm",
                          id = id, 
                          data = inFile3())
    
    cols <- dput(names(features(s)[-1]))
    
    #derive splicings from input format
    sp_ranges <- trimws(input$splice_ranges)
    sp1 <- str_replace_all(sp_ranges, ',', '), c(')
    sp2 <- str_replace_all(sp1, ':', ', ')
    sp3 <- paste('list(c(', sp2, '))', sep="")
    
    raw <- spectacles::splice(s, 
                              locations = eval(parse_expr(sp3)))
                              #locations = list(c(750, 1000), c(1830, 1950)))
    
    # apply Savitzky-Golay smoothing filter
    raw <- apply_spectra(raw, signal::sgolayfilt, n = input$SG_n, p = input$SG_p)
    
    # resample from 1nm to 20nm for plotting efficiency
    raw <- apply_spectra(raw,
                         prospectr::resample, wl(raw), 
                         seq(min(wl(raw)), max(wl(raw)), input$resampling_nm))
    
    # rnv transformation
    raw <- apply_spectra(raw, spectacles::rnv, r=input$rnv_perc)
    
    m <- melt_spectra(raw, attr = cols)
    
  }) %>%
    bindCache(input$radio)
  
  plot <- reactive({
    
    if(input$radio == 'Reflectance'){
      return(reflectance())}
    
    if(input$radio == 'Absorbance'){
      return(absorbance())}
    
    if(input$radio == 'SNV'){
      return(snv())}
    
    if(input$radio == 'RNV'){
      return(rnv())}
  })
  
  trigger <- reactive({
    paste(input$radio , input$soil_colors, input$select_id)
  })
re <- eventReactive(trigger(),{
  
  # recreate objects from color_mun
  m2 <- color_mun()
  colr <- m2$colr
  lab_name <- names(id_sel())
  
  x <- ggplot(plot(), aes( 
    text = paste(
    "Sample: ",as.factor(get(lab_name)) , "\n",
    "wl: ", wl, "\n",
    "nir: ", nir, "\n",
    paste0(input$select_var3, ": ", as.factor(get(input$select_var3))), "\n",
    sep = "")), 
    show.legend = TRUE)+
    geom_line(aes(x=wl,y=nir, 
                  group= as.factor(get(names(id_sel()))), 
                  colour= as.factor(get(input$select_var3))))+ 
    labs(x= "Wavelength (nm)", y = input$radio)+
    theme_grey()+
    theme(legend.position="bottom", 
          legend.title = element_blank())
  
  
    #TODO: work in better sequence of reactivity here
    if(input$soil_colors == FALSE){
      x <- x + scale_color_hue(l=50, c=80)
    } else {
      x <- x + scale_color_manual(values = colr)
    }
    x
    
})
  
  output$newdata <- renderPlotly({ 
    withProgress(message="Generating plot", 
                 detail="Please Wait", value=1, { 
      if(is.null(input$select_var3) || is.na(input$select_var3)){return()}
      ggplotly(re(), tooltip = "text")
    })
  })
   
  graph <- reactiveValues()
  observe({
    req(input$run)
    if(!is.null(re()))
      isolate(
        graph <<- re()
      )
  })
  output$download_graph <- downloadHandler(
    filename = function() {
      paste("shiny_spectra_graph_", Sys.Date(), ".tiff", 
            sep = "")
    },
    content = function(file) {
      ggsave(file, re(), height = 20, width = 20)
    }
  )
#model=========================================================
#choices###
  
  reflectance_m <- reactive({
    #req(input$file)
    req(input$choice == 'reflectance_m')
    
    #wl <- 350:2500
    wl <- wl_app()
    nir <- inFile()
    id <- id_sel()
    s <- SpectraDataFrame(wl= wl, 
                          nir = nir, 
                          units ="nm",
                          id = id, 
                          data = inFile3())
    
    #derive splicings from input format
    sp_ranges <- trimws(input$splice_ranges)
    sp1 <- str_replace_all(sp_ranges, ',', '), c(')
    sp2 <- str_replace_all(sp1, ':', ', ')
    sp3 <- paste('list(c(', sp2, '))', sep="")
    
    raw <- spectacles::splice(s,
        locations = eval(parse_expr(sp3)))
        #locations = list(c(750, 1000), c(1830, 1950)))
    
    # apply Savitzky-Golay smoothing filter
    raw <- apply_spectra(raw, sgolayfilt, n = input$SG_n, p = input$SG_p)
    
    spectras <- raw
  })
  
  absorbance_m <- reactive({
    #req(input$file)
    req(input$choice == 'absorbance_m')
    
    #wl <- 350:2500
    wl <- wl_app()
    nir <- log10(1/inFile())
    id <- id_sel()  
    s <- SpectraDataFrame(wl= wl, 
                          nir = nir, 
                          units ="nm",
                          id = id, 
                          data = inFile3())
    
    #derive splicings from input format
    sp_ranges <- trimws(input$splice_ranges)
    sp1 <- str_replace_all(sp_ranges, ',', '), c(')
    sp2 <- str_replace_all(sp1, ':', ', ')
    sp3 <- paste('list(c(', sp2, '))', sep="")
    
    abs <- spectacles::splice(s, 
            locations = eval(parse_expr(sp3)))
            #locations = list(c(750, 1000), c(1830, 1950)))
    
    # apply Savitzky-Golay smoothing filter
    abs <- apply_spectra(abs, sgolayfilt, n = input$SG_n, p = input$SG_p)
    
    spectras <- abs
    
  })
  
  choice <- reactive({
    if(input$choice == 'reflectance_m'){
      return(reflectance_m())}
    
    if(input$choice == 'absorbance_m'){
      return(absorbance_m())}
  })    
  
  output$selectinput <- renderUI({
    selectInput(inputId = "select_var2",
                label = "Select variable to Model",
                choices = names(Filter(is.numeric, inFile3()))) 
  })
  
#modeling process=================================================
 
  spectras_cal <- reactive({
    set.seed(input$sampleSeed)
    spectras_1 <- choice()
    spectras_2 <- spectras_1[, input$select_var2]
    names(spectras_2) <- 'var_col'
    spectras_2 <- subset(spectras_2, !is.na(var_col))
    idx <- sample(nrow(spectras_2),
                  nrow(spectras_2)*(input$sampleSize/100))
   
    spectras_cal <- spectras_2[idx, ]
  })
  
  spectras_val <- reactive({
    set.seed(input$sampleSeed)
    spectras_1 <- choice()
    spectras_2 <- spectras_1[, input$select_var2]
    names(spectras_2) <- 'var_col'
    spectras_2 <- subset(spectras_2, !is.na(var_col))
    idx <- sample(nrow(spectras_2),
                  nrow(spectras_2)*(input$sampleSize/100))
    spectras_val <- spectras_2[-idx, ]
  })
  
  model_user <- reactive({ 
    set.seed(input$sampleSeed)
    
  spectras_cal1 <- spectras_cal()
  spectras_val1 <- spectras_val()
  n_comp1 <- input$components
  
     model <- plsr(spectras_cal1$var_col ~ 
                     spectra(spectras_cal1), 
                   data=spectras_cal1, ncomp = n_comp1, 
                   validation = "LOO")
     
    # print(summary(model))
    # print(str(spectras_cal1))
     
     rmsep <- RMSEP(model, newdata= spectras_val1)
     
     r <- as.numeric(rmsep$val)
    
     comp <- (0:input$components)
     
     g <- data.frame(r,comp)
     
     h <- ggplot(g, aes(comp, r))+
       geom_line()+
       geom_point()+
       xlab("# of components")+
       ylab("RMSEP")+
       scale_x_continuous(n.breaks = max(comp))
     
     h
  })
  
  observeEvent(input$run,{
    # print(input$sampleSeed)
    # print(input$sampleSize)
    # print(input$components)
  }) 
 
#model_plot=====================================================
  
  re2 <- eventReactive(input$run, {
     
    return(model_user())
    
    })

    output$results <- renderPlot({
      
      re2()
      
    })
  
#test_model====================================================
    
    output$compon <- renderUI({
      req(input$components)
      numericInput("compon", "# of components",
                   value = 1, min = 1, max = input$components)
    })
    
    model <- reactive({
      
      spectras_cal1 <- spectras_cal()
      
      spectras_val1 <- spectras_val()
      n_comp1 <- input$components
      
      plsr(spectras_cal1$var_col ~ spectra(spectras_cal1), 
                    data=spectras_cal1, ncomp = n_comp1, 
                    validation = "LOO")
    })
    
    
    validation <- reactive({
      
      spectras_cal1 <- spectras_cal()
      spectras_val1 <- spectras_val()
      model <- model()
      n_comp <- input$compon 
      
      prediction <- predict(model, ncomp = n_comp, 
                            newdata = spectra(spectras_val1))
    
      training <- predict(model, ncomp = n_comp)
      
      prediction1 <- as.matrix(prediction)
      training1 <- as.matrix(training)
      x <- spectras_val1$var_col
      y <- prediction
      si <- lm(y~x)
      slope <- si[["coefficients"]][["x"]]
      intercept <- si[["coefficients"]][["(Intercept)"]]
      
    #TODO: add an option to compare plots side by side
      
  b <- ggplot()+
    geom_point(aes(x= spectras_cal1$var_col, y= training1))+
    geom_point(aes(x= spectras_val1$var_col, y= prediction1),
               color= "red",show.legend = FALSE ) +
    geom_abline()+
    geom_abline(slope = slope, intercept = intercept,
                color= 'red')+
    labs(x= paste0("Measured ", input$select_var2), y= paste0("Predicted ", input$select_var2),
         title= paste(input$select_var2, "model 1", sep=" "))+
        scale_x_continuous(limits= c(-3,45))+
        scale_y_continuous(limits = c(-3,45))+
        theme_classic()
  
      b + theme(legend.title = element_blank())
    })
    
    output$do2_compon <- renderUI({
      req(input$do2)
      numericInput(inputId = "comp_2","# of components",
                   value = 1, min = 1, max = input$components)
    })
    
    
    
    validation2 <- reactive({
      req(input$do2)
      req(input$comp_2)
      spectras_cal1 <- spectras_cal()
      spectras_val1 <- spectras_val()
      model <- model()
      n_comp <- input$comp_2
      
      prediction <- predict(model, ncomp = n_comp, 
                            newdata = spectra(spectras_val1))
      
      training <- predict(model, ncomp = n_comp)
      
      prediction1 <- as.matrix(prediction)
      training1 <- as.matrix(training)
      x <- spectras_val1$var_col
      y <- prediction
      si <- lm(y~x)
      slope <- si[["coefficients"]][["x"]]
      intercept <- si[["coefficients"]][["(Intercept)"]]
      
      b <- ggplot()+
        geom_point(aes(x= spectras_cal1$var_col,
                       y= training1))+
        geom_point(aes(x= spectras_val1$var_col, 
                       y= prediction1),
                   color= "red",show.legend = FALSE ) +
        geom_abline()+
        geom_abline(slope = slope, intercept = intercept,
                    color= 'red')+
        labs(x= paste0("Measured ", input$select_var2), y= paste0("Predicted ", input$select_var2),
             title= paste(input$select_var2, "model 2", sep=" "))+
        scale_x_continuous(limits= c(-3,45))+
        scale_y_continuous(limits = c(-3,45))+
        theme_classic()
      
      b + theme(legend.title = element_blank())
    })
    
    model_download <- reactiveValues()
    observe({
      req(input$run_test)
      if(!is.null(model()))
        isolate(
          model_download <<- model()
        )
    })
    
    ## download split datasets and model object
    output$download_model <- downloadHandler(
      filename = function() {
        paste("shiny_spectra_model_", Sys.Date(), ".rda", 
              sep = "")
      },
      content = function(file) {
        save(model_download, file = file)
        #save(list=c('spectras_cal()','spectras_val()','model2'), file = file)
        #save(spectras_cal1(), spectras_val1(), model2, file = file)
      }
    )
    
    table_t1 <- reactive({
      req(input$run_test)
      
      model <- model()
      spectras_val1 <- spectras_val()
      spectras_cal1 <- spectras_cal()
      n_comp <- input$compon
      
      #rmsep_train <- RMSEP(model, estimate = "train",
           #          newdata= spectras_val1, 
           #          n_comp = n_comp)
      rmsep_test <- RMSEP(model, estimate = "test",
                           newdata= spectras_val1, 
                           n_comp = n_comp)
      #r2_train <- R2(model, estimate = "train", 
           #          newdata = spectras_val1,
           #          ncomp = n_comp)
      r2_test <- R2(model, estimate = "test", 
                    newdata = spectras_val1,
                    ncomp = n_comp)
      
      stat_dat <- data.frame(Intercept= numeric(3),
                          Components= numeric(3),
                          row.names = c('RMSEP',
                                        'R2', 'RPD') )
    
      
      stat_dat[1,] <- round(rmsep_test[['val']], 3)
      stat_dat[2,] <- round(r2_test[['val']], 3)
      
      var.sd <- sd(spectras_cal1$var_col)
      #this is the component of test for RMSEP
      var.rmsep <- as.numeric(stat_dat[2,2])
      RPD_test <- var.sd/var.rmsep
      
      stat_dat[3,] <- round(RPD_test, 3)
      stat_dat
      #print(stat_dat)
    
    })
    
    table_t2 <- reactive({
      req(input$run_test)
      req(input$do2)
      req(input$comp_2)
      
      model <- model()
      spectras_val1 <- spectras_val()
      spectras_cal1 <- spectras_cal()
      n_comp <- input$comp_2
      
      rmsep_test <- RMSEP(model, estimate = "test",
                          newdata= spectras_val1, 
                          n_comp = n_comp)
     
      r2_test <- R2(model, estimate = "test", 
                    newdata = spectras_val1,
                    ncomp = n_comp)
      
      stat_dat <- data.frame(Intercept= numeric(3),
                             Components= numeric(3),
                             row.names = c('RMSEP',
                                           'R2', 'RPD') )
      
      
      stat_dat[1,] <- round(rmsep_test[['val']], 3)
      stat_dat[2,] <- round(r2_test[['val']], 3)
      
      var.sd <- sd(spectras_cal1$var_col)
      #this is the component of test for RMSEP
      var.rmsep <- as.numeric(stat_dat[2,2])
      RPD_test <- var.sd/var.rmsep
      
      stat_dat[3,] <- round(RPD_test, 3)
      stat_dat
      
    })
#test_model_plot================================================
    re3 <- eventReactive(input$run_test, {
      
      return(validation())
      
    })
    
    re3_2 <- eventReactive(input$run_test,{
    
      return(validation2())
  
    })
    
    output$result_test <- renderPlotly({
      
      ggplotly(re3(), tooltip = "text")
      
    })
    
    output$plotgraph2 <- renderPlotly({
      
      if(input$do2 == TRUE){
        ggplotly(re3_2(), tooltip = "text") }
      
      
    })
    
    output$result_table <- renderDataTable({
      
      datatable(table_t1(), rownames = TRUE)
      
    })
    
    output$result_table_2 <- renderDataTable({
      datatable(table_t2(), rownames = TRUE)
      
    })
#outlier_detection==========================================
    output$selectinput2 <- renderUI({
      if(input$radio_od == 'data_od'){
        return(
         selectInput(inputId = "select_odvar",
                          label = "Select lab variable 
                          for outlier detection",
                          choices = names(inFile3())))}
    })
    
    output$od_par <- renderUI({
      if(input$radio_od == 'data_od'){
      selectInput(inputId = "od_par",
                  label = "Define the SD and Mean Limits",
                  choices = 1:5)}
    })
    
    output$od_reps <- renderUI({
      if(input$radio_od == 'data_od'){
        return(
          sliderInput(inputId = "select_odreps",
                      label = "Select # of replications 
                          for outlier detection",
                      min = 10, max = 50,
                      value = 20,
                      step = 5, round = 0))}
          })
    
    data_od1 <- reactive({
      
      spectras_1 <- reflectance_m()
      spectras_2 <- spectras_1[, input$select_odvar]
      names(spectras_2) <- 'var_col'
      spectras_2 <- subset(spectras_2, !is.na(var_col))
      rep_t <- input$select_odreps
      
      
      od <- enpls.od(y = spectras_2$var_col, 
                     x = spectra(spectras_2), 
                     reptimes = rep_t)
      
      s <- od$error.sd
      n <- od$error.mean
      names2 <- id_sel()
      o <- data.frame(s,n)
      o$id <- ids(spectras_2)
      
      #o
      #print(o)
    })
    
    data_od2 <- reactive({
      
       #TODO: should maxcomp be set to input$components? should reptimes be an option in the UI?
      o <- data_od1()
      
      sd_par <- as.numeric(input$od_par)*sd(o$n)
      mean_par <- as.numeric(input$od_par)*sd(o$s)
      
      # TODO: include paste with data from the input$selection columns from UI
      v <- ggplot(data = o, aes(x = n, y = s))+
        geom_point()+
        geom_vline(xintercept = sd_par)+
        geom_hline(yintercept = mean_par)+
        scale_x_continuous(n.breaks = 6)+ xlab("Error Mean")+
        scale_y_continuous(n.breaks = 8)+ ylab("Error SD")+
        scale_fill_manual(values = "red")
      
      v
    })
    
    spectra_od1 <- reactive({
      spectras_1 <- reflectance_m()
      wl <- spectras_1@wl
      nir <- spectras_1@nir
      m_spec <- as.matrix(nir, wl)
      
      maxexplvar <- 0.99
      pcspectra_m <- pc_projection(Xr = m_spec,
                pc_selection = list("cumvar",
                                    maxexplvar),
                                    method = "pca",
                                    center = TRUE, 
                                    scale = FALSE)
      
      pcspectra_mCenter <- colMeans(pcspectra_m$scores)
      pcspectra_mCenter <- t(as.matrix(pcspectra_mCenter))
      
      mahD <- f_diss(Xr = pcspectra_m$scores,
                     Xu = pcspectra_mCenter,
                     diss_method = "mahalanobis",
                     center = FALSE, scale = FALSE)
      
      mahD1 <- as.data.frame(mahD)
      mahD1$Index <- 1:nrow(mahD1)
      mahD1$id <- ids(spectras_1)
      
      mahD1
    })
    spectra_od2 <- reactive({
      #spectras_1 <- reflectance_m()
      #wl <- spectras_1@wl
      #nir <- spectras_1@nir
      #m_spec <- as.matrix(nir, wl)
      
      #maxexplvar <- 0.99
      #pcspectra_m <- pc_projection(Xr = m_spec,
          #              pc_selection = list("cumvar",
          #                                  maxexplvar),
          #              method = "pca",
          #              center = TRUE, scale = FALSE)
      
     # pcspectra_mCenter <- colMeans(pcspectra_m$scores)
     # pcspectra_mCenter <- t(as.matrix(pcspectra_mCenter))
      
     # mahD <- f_diss(Xr = pcspectra_m$scores,
         #            Xu = pcspectra_mCenter,
         #            diss_method = "mahalanobis",
         #            center = FALSE, scale = FALSE)
      
     # mahD1 <- as.data.frame(mahD)
     # mahD1$Index <- 1:nrow(mahD1)
     # mahD1$id <- ids(spectras_1)
      
      mahD1 <- spectra_od1()
      
      mh <- ggplot(mahD1, aes(x = Index, y = Xu_1,
                              text = paste(
                                 "Sample: ", id,
                                 sep="")))+
      geom_point(color = 'black', fill = 'white', 
                 size = 2, stroke = 1, shape = 21)+
      geom_hline(yintercept = 3, color = 'red')+
                 labs(y = "Mahalanobis Distance")
      
      mh
    })   
    
    re4 <- eventReactive(input$run_od, {
      
      if(input$radio_od == 'data_od'){
        return(data_od2())}
      
      if(input$radio_od == 'spectra_od'){
        return(spectra_od2())}
    })
    
    output$result_od <- renderPlot({
      withProgress(message="Generating plot", detail="Please Wait", value=1, { 
        re4()
        #plotly_build(ggplotly(re4(), tooltip = "text"))
      })
    }) 
   
    lab_dat_br <- reactive({
      brushedPoints(data_od1(), input$plot1_brush)
    })
    
    spec_dat_br <- reactive({
      brushedPoints(spectra_od1(),
                    input$plot1_brush)
    })
    
    new_data_br_lab <- reactive({
      new_dat <- lab_dat_br()
      br_points <- as.numeric(rownames(new_dat))
      old_dat <- inFile3()
      new_lab_dat_br <- old_dat[-c(br_points),]
      
    })
    
    new_data_br_spec <- reactive({
      new_dat <- spec_dat_br()
      br_points <- as.numeric(new_dat$Index)
      old_dat <- inFile()
      new_spec_dat_br <- old_dat[-c(br_points),]
      
    })
    
    output$brush_info <- renderPrint({
 
      if(input$radio_od == 'data_od'){
        return(lab_dat_br())}
      
      if(input$radio_od == 'spectra_od'){
        return(spec_dat_br())}
      
    })
    
    
    download <- reactive({
      if(input$radio_od == 'data_od'){
        return(new_data_br_lab()) }
      
      if(input$radio_od == 'spectra_od'){
        return(new_data_br_spec()) }
    })
    
      output$download_od <- downloadHandler(
        
        filename = function() {
          paste("shiny_outlier_removed_", Sys.Date(), ".csv", 
                sep = "")
        },
        content = function(file) {
          write.csv(download(),
                    file, row.names = FALSE)
        })
      
}

shinyApp(ui = ui, server = server)

                      