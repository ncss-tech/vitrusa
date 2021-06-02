library(shiny)
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
library(lattice)

# DONE - to pull ID column from the first column in the lab data - no longer need file2 of ids
# TODO: sanity check: build in validation so that on import the files are checked for the same number of rows.
# TODO: sanity check: column picker for id column on the load data tab?
# TODO: make inputs for the wl bounds (low and high so this can be adjusted to VNIR, MIR or both!
# TODO: get interactive outliers plot working via plotly
# TODO: add some hzname aggregation options via genhz logic - possibly a 'Plot Aggregated Spectra' tab
# aggregate spectra by taking the mean values across spectra within each hzname group
#d_ag <- aggregate_spectra(d, fun = mean, id = "hzname")
# TODO: add SG and smoothing options
# TODO: test label_field for categorical or continuous and adjust color ramps in plot

## TODO: model columns should be numeric columns only otherwise the app will crash
## ISSUE: layer_seq is numeric but not useful - remove as well

#Define UI
ui <- navbarPage("Soil Spectroscopy",
                 
                 tabPanel("About",
                     titlePanel("Soil Spectroscopy App"),
                     p('This app was designed for exploring spectral data and building models from spectra of soil samples and associated laboratory data.  The app allows the user to load data, explore interactive plots, randomly split the data and build a Partial Least Squares Regression (PLSR) model from a calibration set and then test the model using a validation set.  Additional features include a tab for identifying outliers among the spectral data or among the lab data.'),
                      p('The app includes a demo mode on the Load and Explore Data tab which will load a small dataset of VNIR spectra and associated lab data for exploring the functionality of the app.'),
                     h3('Instructions and workflow:'),
                     h4('1) Load Data'), 
                     p(' - Data can be loaded into the app via two csv files, one of spectral data (wavelength and spectral values) and another of the lab data values with a unique ID column in the first position. These files must be of equal record length. Lab data is visible in the Table tab and a simple black and white plot of all spectra can be viewed in the Plot all spectra tab.'),
                     h4('2) Interactive Spectra Plots'), 
                     p(' - This interactive plot of the spectra can be used to explore the variation related to the columns in the lab data.  Hovering over the plot will show the data values and identify the sample ID for each spectra. Clicking on the objects in the legend will control which spectra are shown.'),
                     h4('3) Model'), 
                     p(' - Data can be split on the model tab using the slider for the percentage of the split to be used.  The user selects the variable in the lab data they wish to model. After clicking the Run! button the app will display a principle components plot of Root Mean Squared Error of Prediction (RMSEP). This indicates the number of principle components which will yield the least amount of error and should be used for the most robust model.'),
                     h4('4) Test Model'), 
                     p(' - The user selects the appropriate number of components to use in the model, runs the model and an interactive plot of model performance is displayed.  Metrics from the model can be shown by clicking the View Statistics button.  Model output and associated split datasets can be downloaded as an .Rda file for later use.'),
                     h4('5) Outlier Detection'),
                     p(' - Here the user can generate two different outlier plots.  One for the similarity and Mahalanobis Distance among the spectra and another of standard deviation versus mean error for the selected variable from the lab data.'),
                     tags$hr(),
                     p('Version 1.0 - designed and developed by Kian Speck and Jay Skovlin.')
                     ),
                 
              tabPanel("Load and Explore Data",
                          
#sidebar layout with input and output definitions
                sidebarLayout(
#sidebar panel for inputs
                sidebarPanel(
#Input: user chooses own files
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
        
        actionButton("example", "Load Example Spectra"),
       
        
        checkboxInput("header","Header", TRUE),
        
        radioButtons("sep", "Separator",
                     choices = c(Semicolon = ";",
                                 Comma = ",",
                                 Tab = "\t"),
                                 selected = ","),  # ADD wavelength range settings here.....
        radioButtons("disp", "Display",
                     choices = c(All = "all",
                                 Head = "head"),
                     selected = "all"),
        
        uiOutput("checkbox")
        
                ),

  mainPanel(
    tabsetPanel(
    id = "dataset",
    tabPanel("Table", DT::dataTableOutput("rendered_file")),
    tabPanel("Plot all spectra", plotOutput("test"),
             tags$hr(),
             )
  )
)
                ),
         tags$hr(),
                
         ),
        

  tabPanel("Interactive Spectra Plot",
           # sidebarLayout(
           #   sidebarPanel(
           fixedRow(
             column(3,
           radioButtons("radio","Preprocessing Function:",
                choices = list("Raw Spectra" = "Reflectance",
                              "Absorbance" = "Absorbance"),
                selected = "Reflectance"), 
           checkboxInput("soil_colors","Apply soil colors", TRUE),
           tags$hr(),
             ),
           column(9,
                   
                  uiOutput("plotlabel1"),
                  uiOutput("plotcolor"),
                  
                  
                  tags$hr(),
           )
           ),
           mainPanel(
             plotlyOutput("newdata", width = "850px",
                          height = "700px")
           )
      ),
          
tabPanel("Outlier Detection",
         fixedRow(
           column(3,
                  
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
                  
                  actionButton("run_od", "Run!"),
                  
                  tags$hr(),
           ),
           conditionalPanel(
             condition="$('html').hasClass('shiny-busy')",
             tags$div("Loading...",id="loadmessage")),
         ),
         mainPanel(
           plotlyOutput("result_od", width = "850px", 
                        height = "650px")
         )),
         
      tabPanel("Model",
               sidebarLayout(
                 sidebarPanel(
                   
                   sliderInput("sampleSize", 
                             "Percent data to use in train set", 
                              min = 0, max = 100,
                              value = 75,
                              step = 5, round = 0, post = '%'),
           
           numericInput("sampleSeed", "Sample seed", value = 4556),
           
           numericInput("components","Choose the Max # of 
                        Components", value = 8),
           
       uiOutput("selectinput"),
       
       radioButtons("choice", "Choose Spectral Preprocessing",
             choices = c(Reflectance = "reflectance_m",
                         Absorbance = "absorbance_m"),
                         selected = "reflectance_m"),
       
       actionButton("run", "Run!"),
       
       tags$hr(),
          ),
       
       mainPanel(
          plotOutput("results")
                 )
               )
             ),

    tabPanel("Test Model",
         sidebarLayout(
           sidebarPanel(
             
             numericInput("compon", "# of components",
                          value = 1),
             
             actionButton("run_test", "Run!"),
             
             actionButton("test_stats", "View Statistics"),
             
             downloadButton("download_model", "Download Model",
                            class = "dlButton"),
             
             tags$hr(),
           ),
           
           mainPanel(
             plotlyOutput("result_test"),
             dataTableOutput("result_table")
           )
         )
        )


      )

server <- function(input,output,session){
  options(shiny.maxRequestSize=30*1024^2)
#labdata viewer================================================
  inFile3 <- reactive({
    req(input$file3)
    read.csv(input$file3$datapath,
             header = input$header,
             sep = input$sep)
    
  })
  
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
  
  output$rendered_file <- DT::renderDataTable({
    if(input$disp == "head") {
      head(inFile3_sel())
    }
    else {
      inFile3_sel()
    }
  })
#visualize===================================================
  # load example data files on server
  inFile <- eventReactive(input$example, {
    read.csv(source("lab_spectra_csv.csv"),
             header = TRUE,
             sep = ",")
  })
   
  inFile3 <- eventReactive(input$example, {
   read.csv(source("lab_data_csv.csv"),
            header = TRUE,
            sep = ",")
  })
  
  inFile <- reactive({
    req(input$file)
    
    file_user <- read.csv(input$file$datapath)
    
    inFile <- as.data.frame(file_user)
    
  })
  
  color_mun <- reactive({
    # create full range spectral DF 
    wl <- 350:2500
    nir <- inFile()
    id <- unlist(inFile3()[1])
    dd <- SpectraDataFrame(wl= wl, nir = nir, id = id,
                           units ="nm", data = inFile3())
    
    
    
    # cut to specified wl range
    d <- cut(dd, wl= 380:730)
    
    # long format simpler to work with
    d1 <- melt_spectra(d)
    
    # "round" 1nm -> 10nm resolution
    d1$v10 <- round(d1$wl, -1)
    
    # aggregate to 10nm res via mean
    a <- aggregate(nir ~ id + v10, data = d1, FUN = mean)
    
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
    
    # convert color to sRGB
    #z$colr <- munsell2rgb(z$hue, z$value, z$chroma)
    # nice label
    #z$m <- sprintf('%s %s/%s', z$hue, z$value, z$chroma)
    
    z1 <- z[, c('id', 'colr')]
    rownames(z1) <- 1:nrow(z1)
    
    # join colors back to the original data
    features(dd, key="id") <- z1
    
    # melt again for plot
    d2 <- melt_spectra(dd, attr="colr")
    m1 <- d2
    m1
    
    #colr <- m1$colr
    
    #colr
  })
  output$test <- renderPlot({
    
    # wl <- 350:2500
    # nir <- inFile()
    # id <- inFile3()[1]
    # s1 <- SpectraDataFrame(wl= wl,
    #                        nir = nir,
    #                        id = id,
    #                        units ="nm",
    #                        data = inFile())
    # 
    # id_col <- names(features(s1, exclude_id = FALSE)[1])
    # m1 <- melt_spectra(s1)
    
    m2 <- color_mun()
    colr <- m2$colr
    
    test <- ggplot(m2)+
      geom_line(aes(x=wl,y=nir, group=as.factor(id), colour=id))+
      #geom_line(aes(x=wl,y=nir, group=as.factor(get(names(inFile3()[1])))), show.legend = FALSE)+
      scale_color_manual(values = colr)+
      #scale_color_binned()+
      #scale_color_hue(l=50, c=80)+
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
    req(input$file)
    req(input$file3)
    req(input$radio == 'Reflectance')
    req(!is.null(input$select_var3) || !is.na(input$select_var3))
    
    wl <- 350:2500
    nir <- inFile()
    id <- inFile3()[1]
    s <- SpectraDataFrame(wl= wl, 
                          nir = nir, 
                          units ="nm",
                          id = id, 
                          data = inFile3())
    
    cols <- dput(names(features(s)[-1]))
    
    raw <- spectacles::splice(s, 
             locations = list(c(750, 1000), c(1830, 1950)))
    
    m <- melt_spectra(raw, attr = cols)
    
    })
  
#absorbance=========================================
  absorbance <- reactive({
    req(input$file)
    req(input$file3)
    req(input$radio == 'Absorbance')
    req(!is.null(input$select_var3) || !is.na(input$select_var3))
    
    wl <- 350:2500
    nir <- log10(1/inFile())
    id <- inFile3()[1]
    s <- SpectraDataFrame(wl= wl, 
                          nir = nir, 
                          units ="nm",
                          id = id, 
                          data = inFile3())
    
    cols <- dput(names(features(s)[-1]))
    
    abs <- spectacles::splice(s, 
                locations = list(c(750, 1000), c(1830, 1950)))
    
    m <- melt_spectra(abs, attr = cols)
    
  })
  
  plot <- reactive({
    
    if(input$radio == 'Reflectance'){
      return(reflectance())}
    
    if(input$radio == 'Absorbance'){
      return(absorbance())}
  })
  
  trigger <- reactive({
    paste(input$radio , input$soil_colors)
  })
re <- eventReactive(trigger(),{
  
  # recreate objects from color_mun
  m2 <- color_mun()
  colr <- m2$colr
  
  x <- ggplot(plot())+
    #geom_line(aes(x=wl,y=nir, group=as.factor(Sample), colour=as.factor(get(input$select_var3))),
    geom_line(aes(x=wl,y=nir, group=as.factor(get(names(inFile3()[1]))), colour=as.factor(get(input$select_var3)), 
                  text = paste(
                    "Sample: ", as.factor(get(names(inFile3()[1]))), "\n",
                    "wl: ", wl, "\n",
                    "nir: ", nir, "\n",
                    "variable: ", paste0(input$select_var3, " --- ", as.factor(get(input$select_var3))), "\n",
                    sep = ""
                  )), show.legend = TRUE)+ 
    labs(x= "Wavelength (nm)", y = input$radio)+
    theme_grey()+
    theme(legend.position="bottom", legend.title = element_blank())
  
    #TODO: work in better sequence of reactivity here
    if(input$soil_colors == FALSE){
      x <- x + scale_color_hue(l=50, c=80)
    } else {
      #x <- x + scale_color_manual(values = color_mun())
      x <- x + scale_color_manual(values = colr)
    }
    x
    
})
  
  output$newdata <- renderPlotly({ 
    withProgress(message="Generating plot", detail="Please Wait", value=1, { 
      if(is.null(input$select_var3) || is.na(input$select_var3)){return()}
      plotly_build(ggplotly(re(), tooltip = "text"))
    })
  })
#model=========================================================
#choices###
  
  reflectance_m <- reactive({
    req(input$file)
    req(input$choice == 'reflectance_m')
    
    wl <- 350:2500
    nir <- inFile()
    id <- inFile3()[1]
    s <- SpectraDataFrame(wl= wl, 
                          nir = nir, 
                          units ="nm",
                          id = id, 
                          data = inFile3())
    
    raw <- spectacles::splice(s, 
       locations = list(c(750, 1000), c(1830, 1950)))
    
    spectras <- raw
  })
  
  absorbance_m <- reactive({
    req(input$file)
    req(input$choice == 'absorbance_m')
    
    wl <- 350:2500
    nir <- log10(1/inFile())
    id <- inFile3()[1]   
    s <- SpectraDataFrame(wl= wl, 
                          nir = nir, 
                          units ="nm",
                          id = id, 
                          data = inFile3())
    
    abs <- spectacles::splice(s, 
        locations = list(c(750, 1000), c(1830, 1950)))
    
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
              # JS - limited list to numeric vars only to avoid crashing the app!
  })
  
#modelling process=================================================
 
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
     print(str(spectras_cal1))
     
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
    print(input$sampleSeed)
    print(input$sampleSize)
    print(input$components)
  }) 
 
#model_plot=====================================================
  
  re2 <- eventReactive(input$run, {
     
    return(model_user())
    
    })

    output$results <- renderPlot({
      
      re2()
      
    })
  
#test_model====================================================
    
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
      #print(spectras_val1)
      
      model <- model()
      
      n_comp <- input$compon 
      
      prediction <- predict(model, ncomp = n_comp, 
                            newdata = spectra(spectras_val1))
      
      print(prediction)
      training <- predict(model, ncomp = n_comp)
      
      prediction1 <- as.matrix(prediction)
      training1 <- as.matrix(training)
      x <- spectras_val1$var_col
      y <- prediction
      si <- lm(y~x)
      slope <- si[["coefficients"]][["x"]]
      intercept <- si[["coefficients"]][["(Intercept)"]]
      
    
      
  b <- ggplot()+
    geom_point(aes(x= spectras_cal1$var_col, y= training1))+
    geom_point(aes(x= spectras_val1$var_col, y= prediction1),
               color= "red",show.legend = FALSE ) +
    geom_abline()+
    geom_abline(slope = slope, intercept = intercept,
                color= 'red')+
    labs(x= paste0("Measured ", input$select_var2), y= paste0("Predicted ", input$select_var2),
         title= "VNIR")+
        scale_x_continuous(limits= c(-3,45))+
        scale_y_continuous(limits = c(-3,45))+
        theme_classic()
  
      b + theme(legend.title = element_blank())
    })
    
    model2 <- reactiveValues()
    observe({
      req(input$run_test)
      if(!is.null(model()))
        isolate(
          model2 <<- model()
        )
    })
    
    ## download split datasets and model object
    output$download_model <- downloadHandler(
      filename = function() {
        paste("shiny_spectra_model_", Sys.Date(), ".rda", 
              sep = "")
      },
      content = function(file) {
        save(model2, file = file)
        #save(list=c('spectras_cal()','spectras_val()','model2'), file = file)
        #save(spectras_cal1(), spectras_val1(), model2, file = file)
      }
    )
    
    table_t1 <- reactive({
      req(input$test_stats)
      
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
      print(stat_dat)
    
    })
#test_model_plot================================================
    re3 <- eventReactive(input$run_test, {
      
      return(validation())
      
    })
    output$result_test <- renderPlotly({
      
      plotly_build(ggplotly(re3(), tooltip = "text"))
      
    })
    
   # te <- eventReactive(input$test_stats, {
      
    #  return(table_t1())
      
    #})
    
    output$result_table <- renderDataTable({
      
      datatable(table_t1(), rownames = TRUE)
      
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
    data_od2 <- reactive({
      
      spectras_1 <- reflectance_m()
      spectras_2 <- spectras_1[, input$select_odvar]
      names(spectras_2) <- 'var_col'
      spectras_2 <- subset(spectras_2, !is.na(var_col))
      
      od <- enpls.od(y = spectras_2$var_col, 
                     x = spectra(spectras_2), reptimes = 50, 
                     maxcomp = 5) #TODO: should maxcomp be set to input$components? should reptimes be an option in the UI?
      
      s <- od$error.sd
      n <- od$error.mean
      names2 <- inFile3()[1]
      o <- data.frame(s,n)
      o$id <- ids(spectras_2)
      
      # TODO: include paste with data from the input$selection columns from UI
      v <- ggplot(data = o, aes(x = n, y = s,
                                text = paste(
                                  #"ID: ", names2 ,"\n",
                                  "Sample: ", id ,"\n",
                                  "Error SD: ", s, "\n",
                                  "Error Mean: ", n, "\n",
                                  sep = "" 
                                ))) +
        geom_point()+
        geom_vline(xintercept = 2*sd(n))+
        geom_hline(yintercept = 2*sd(s))+
        scale_x_continuous(n.breaks = 6)+ xlab("Error Mean")+
        scale_y_continuous(n.breaks = 8)+ ylab("Error SD")+
        scale_fill_manual(values = "red",)
      
      v
    })
    
    spectra_od2 <- reactive({
      spectras_1 <- reflectance_m()
      wl <- spectras_1@wl
      nir <- spectras_1@nir
      m_spec <- as.matrix(nir, wl)
      
      maxexplvar <- 0.99
      pcspectra_m <- pc_projection(Xr = m_spec,
                        pc_selection = list("cumvar",
                                            maxexplvar),
                        method = "pca",
                        center = TRUE, scale = FALSE)
      
      pcspectra_mCenter <- colMeans(pcspectra_m$scores)
      pcspectra_mCenter <- t(as.matrix(pcspectra_mCenter))
      
      mahD <- f_diss(Xr = pcspectra_m$scores,
                     Xu = pcspectra_mCenter,
                     diss_method = "mahalanobis",
                     center = FALSE, scale = FALSE)
      
      mahD1 <- as.data.frame(mahD)
      mahD1$Index <- 1:nrow(mahD1)
      mahD1$id <- ids(spectras_1)

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
    
    output$result_od <- renderPlotly({
      withProgress(message="Generating plot", detail="Please Wait", value=1, { 
        plotly_build(ggplotly(re4(), tooltip = "text"))
      })
    }) 
    
}

shinyApp(ui = ui, server = server)

                        