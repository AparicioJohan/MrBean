
# pack <- c("shiny", "shinydashboard","plotly", "datasets", "SpATS", "lme4", "tools","readxl","shinyBS","purrr",
#            "GGally","ggpubr","corrplot","missMDA","FactoMineR","factoextra","psych","ggpmisc","lmerTest","car",
#            "data.table","shinytoastr","broom.mixed","shinyjs","DT"," ggvis","shinyalert","shinycssloaders","shinyWidgets",
#           "rintrojs","dplyr","shinydashboardPlus","robustbase")
# suppressWarnings(pacman::p_load(pack))

source("https://raw.githubusercontent.com/AparicioJohan/SpATS.plus/master/All_additional.R")     # SpATS PLUS


dts <- function(SUBSET,k,p.sub, data){   # SUBSET = input$subset ( TRUE / FALSE )  ; k =  input$varsubset (  variable for subset ) ; p.sub = input$levelessub
  if(SUBSET==TRUE) {
    if(k==""|p.sub=="" ) data <- data
    if(k!=""&p.sub!="") data <- subset(data,get(k)==p.sub)
    return(data)
  } else {
    return(data)}
}


# ----------- INICIO ------------------------
shinyServer(function(input, output, session){

  source("override.R", local = TRUE)


  observeEvent(input$file1,{   # warning Message
    tryCatch(
      {
        inFile <- input$file1
        Ext <- tools::file_ext(inFile$datapath)
        if(!Ext%in%c("csv","xls","xlsx","txt")) stop("File not allowed")
        toastr_info("Now check: Header, Missing Value Character, Cell Separation Character!",position =  "bottom-right")
        toastr_success("File loaded!",position =  "bottom-right")
      },
      error = function(e) {
        toastr_error(title = "Database error", conditionMessage(e),position =  "bottom-right")
      }
    )
  })


  observe({
    inFile <- input$file1
    Ext <- tools::file_ext(inFile$datapath)
    if (input$Id004==1) {
      dt <- read.table('means.txt', header=T,sep = ",")
    } else {
      if (is.null(inFile)) { dt <- data.frame() }
      else {
        if(Ext=="xlsx"|Ext=="xls") {
          if(input$miss=="Empty"){ P = "\" \""} else {P = "NA"}

          req(input$sheet!="")
          dt <- as.data.frame(read_excel(inFile$datapath,col_names = input$header,na = P, sheet = input$sheet))
        }else {
          dt <-  read.csv(inFile$datapath, header=input$header,sep=input$sep)
        }
      }

    }
    nums  <- unlist(lapply(dt, is.numeric))
    updateSelectInput(session, "variable", choices=names(dt), selected = "YDHPL")
    updateSelectInput(session, "column", choices=names(dt),selected = "col")
    updateSelectInput(session, "fila", choices=names(dt),selected = "row")
    updateSelectInput(session, "factor", choices=names(dt),selected = "rep")
    updateSelectInput(session, "genotipo", choices=names(dt),selected = "line")
    updateSelectInput(session, "show_fixed", choices=names(dt),selected = NULL)
    updateSelectInput(session, "show_random", choices=names(dt),selected = NULL)
    updateSelectInput(session, "variable2", choices=names(dt),selected = "YDHPL")
    updateSelectInput(session, "genotipo2", choices=names(dt),selected = "line")
    updateSelectInput(session, "show_fixed2", choices=names(dt),selected = NULL)
    updateSelectInput(session, "show_random2", choices=names(dt),selected = NULL)
    updateSelectInput(session, "interaction", choices=names(dt),selected = NULL)
    updateSelectInput(session, "covariate", choices=names(dt),selected = NULL)
    updateSelectInput(session, "Id088", choices=names(dt),selected = "YDHPL")
    updateSelectInput(session, "Id086", choices=names(dt),selected = NULL)
    updateSelectInput(session, "Id087", choices=names(dt),selected = NULL)
    updateSelectInput(session, "Id089", choices=names(dt),selected = NULL)
    updateSelectInput(session, "varsubset", choices=names(dt),selected = "NNNNN")

  })

  observe({
    dt <- data()
    k <- input$varsubset
    p.sub <- input$subset
    if (k!=""&p.sub==TRUE) {
      tmp <- as.factor(dt[,input$varsubset])
      updateSelectInput(session, "levelessub", choices= levels(tmp), selected = "NNNNN")
    }
  })

  # info cuando presiones botones

  observeEvent(input$action, {# Show a simple modal
    if (input$variable== "" | input$genotipo == "" | input$column == ""| input$fila == ""  ) {
      shinyalert(title = "Warning", text = "Please fill the fields", type = "error",confirmButtonCol = "#3c8dbc",closeOnClickOutside = TRUE)
    }
  })


  #--------------------------------- DATA -------------------------------------------



  # Pulling the list of variable for choice of variable x y and factor
  output$varx <- renderUI({
    selectInput("variablex", "Select the X variable", choices=names(dts(input$subset,input$varsubset,input$levelessub,data=data())), selected = input$column)
  })
  output$vary <- renderUI({
    selectInput("variabley", "Select the Y variable", choices=names(dts(input$subset,input$varsubset,input$levelessub,data=data())), selected = input$fila)
  })
  output$factor2 <- renderUI({
    selectInput("factor2", "Select a grouping variable", choices=names(dts(input$subset,input$varsubset,input$levelessub,data=data())),selected = input$rep)
  })
  # Pulling the list of variable for choice of variable y  for boxplot
  output$varresponse <- renderUI({
    selectInput("respuesta", "Select a continuous variable", choices=names(dts(input$subset,input$varsubset,input$levelessub,data=data())), selected = input$variable)  #varresponse in UI
  })
  output$box <- renderUI({
    selectInput("BOXPLOT", "Select a grouping variable", choices=names(dts(input$subset,input$varsubset,input$levelessub,data=data())),selected = input$column)  #varresponse in UI
  })

  # Para anadir subset a los datos
  observeEvent(!input$subset, toggle("varsubset",anim = TRUE,time = 1,animType = "fade"))
  observe(toggle("levelessub",anim = TRUE,time = 1,animType = "fade",condition = input$varsubset!=""&input$subset==TRUE))


  # When file loaded
  observeEvent(input$file1,
               shinyjs::show("when_file1",animType = "fade",anim = TRUE))

  observeEvent(input$file1,
               shinyjs::show("when_file2",animType = "fade",anim = TRUE))


  output$oshet <- renderUI({
    inFile <- input$file1
    Ext <- tools::file_ext(inFile$datapath)
    req(input$file1,Ext=="xlsx"|Ext=="xls")
    selectInput(inputId = "sheet",label =  "Sheet Excel", choices=excel_sheets(inFile$datapath))
  })

  #---------------------------------------------------------------------------------

  output$plot <- renderPlotly({
    if (input$actionplot==0)
      return()

    isolate({
      dt <- dts(input$subset,input$varsubset,input$levelessub,data=data())
      a <- min(dt[,input$variabley], na.rm = TRUE)
      b <- max(dt[,input$variabley], na.rm = TRUE)
      dt[,input$factor2] <- as.factor(dt[,input$factor2])
      gra <- ggplot(dt,aes_string(x=input$variablex, y=input$variabley,color=input$factor2)) +
        geom_point() + theme_bw()+ ylim(c(a,b)) + ggtitle("Scatterplot")
      ggplotly(gra)
    })

  })


  # For BOXPLOT
  output$boxplot <- renderPlotly({
    if (input$actionbox==0)
      return()

    isolate({
      dt <- dts(input$subset,input$varsubset,input$levelessub,data=data())
      dt[,input$BOXPLOT] <- as.factor(dt[,input$BOXPLOT])
      gra <- ggplot(dt,aes_string(x=input$BOXPLOT, y=input$respuesta,fill=input$BOXPLOT)) +
        geom_boxplot() + theme_bw() + ggtitle("Boxplot")
      ggplotly(gra)})
  })

  #------------------------------------- DATA ------------------------------------------
  ## Get the value of the dataset that is selected by user from the list of datasets
  # ------------------------------------------------------------------------------------

  data <- reactive({

    inFile <- input$file1
    Ext <- tools::file_ext(inFile$datapath)

    if (input$Id004==1) {

      dt <- read.table('means.txt', header=T,sep = ",")

    } else {

      if (is.null(inFile)) { dt <- data.frame()

      }  else {

        if(Ext=="xlsx"|Ext=="xls") {

          if(input$miss=="Empty"){ P = "\" \""} else {P = "NA"}

          req(input$sheet!="")
          dt <- as.data.frame(read_excel(inFile$datapath,col_names = input$header,na = P,sheet = input$sheet ))

        }  else {

          dt <-  read.csv(inFile$datapath, header=input$header,sep=input$sep)

        }
      }

    }

  })

  # SALIDA de los datos en DT TAble
  output$Rawdata <- DT::renderDataTable(
    DT::datatable({
      dts(input$subset,input$varsubset,input$levelessub,data=data())
    },
    option=list(pageLength=7,columnDefs = list(list(className = 'dt-center', targets = 0:ncol(dts(input$subset,input$varsubset,input$levelessub,data=data()))))),
    filter="top",
    selection="multiple"
    ))

  #----------------------------------------  MODELO  SPATS            ------------------------------------------


  observeEvent(!input$able, toggle("segcol",anim = TRUE,time = 1,animType = "fade"))
  observeEvent(!input$able, toggle("segrow",anim = TRUE,time = 1,animType = "fade"))


  output$segcol <- renderUI({

    validate(

      need(input$column != "", " Fill the area Column "),

      need(input$fila != "", " Fill the area Row ") )

    dt <- dts(input$subset,input$varsubset,input$levelessub,data=data())
    dt$col_f = factor( dt[,input$column])
    sliderInput("segcol", label = "Num of segments col", min = 1, max =  nlevels(dt$col_f)+30, value =  nlevels(dt$col_f) )
  })

  output$segrow <- renderUI({

    validate(

      need(input$column != "", " "),

      need(input$fila != "", " ") )

    dt <- dts(input$subset,input$varsubset,input$levelessub,data=data())
    dt$row_f = factor( dt[,input$fila])
    sliderInput("segrow", label = "Num of segments row", min = 1, max =  nlevels(dt$row_f)+30, value =  nlevels(dt$row_f) )
  })

  ##  SPATS ----------------------------------------------------------------------

  Modelo <- eventReactive(input$action, {
    if (isTRUE(!input$able)) {
      dt <- dts(input$subset,input$varsubset,input$levelessub,data=data())
      dt[ , input$genotipo] <- as.factor(dt[ , input$genotipo])
      dt$col =  dt[,input$column]
      dt$row = dt[,input$fila]
      dt$col_f = factor( dt[,input$column])
      dt$row_f = factor( dt[,input$fila])
      ncols =  length(unique(dt[,input$column]))
      nrows =  length(unique(dt[,input$fila]))
      for (i in 1:length(input$show_fixed)) {
        dt[, input$show_fixed[i]] <- as.factor(dt[, input$show_fixed[i]])
      }
      for (i in 1:length(input$show_random)) {
        dt[, input$show_random[i]] <- as.factor(dt[, input$show_random[i]])
      }
      if(is.null(input$show_fixed)&is.null(input$covariate)) Fijo <- as.formula(~NULL)
      else if(!is.null(input$show_fixed)&is.null(input$covariate)) Fijo <- as.formula(paste("", paste(input$show_fixed, collapse=" + "), sep=" ~ "))                        # as.formula(paste("", input$show_fixed, sep=" ~ "))
      else if(!is.null(input$show_fixed)&!is.null(input$covariate)) Fijo <- as.formula(paste("", paste(c(input$show_fixed,input$covariate), collapse=" + "), sep=" ~ "))
      else if(is.null(input$show_fixed)&!is.null(input$covariate)) Fijo <- as.formula(paste("",input$covariate,sep = " ~ "))

      if(is.null(input$show_random)) Random <- as.formula(~ col_f+row_f)
      else Random <-  as.formula(paste("" ,paste(c(input$show_random,"col_f","row_f"), collapse=" + "), sep=" ~ "))
      Modelo=try(SpATS(response=input$variable,
                       genotype=input$genotipo, genotype.as.random=input$res_ran,
                       fixed= Fijo,
                       spatial = ~ PSANOVA(col, row, nseg = c(ncols,nrows), degree=c(3,3),nest.div=2),
                       random = Random, data=dt,
                       control = list(tolerance=1e-03, monitoring=1)),silent = T)
      tryCatch(
        {
          if(class(Modelo)=="try-error") stop("Error in the components of model")
        },
        error = function(e) {
          toastr_error(title = "Warning:", conditionMessage(e),position =  "bottom-right",progressBar = TRUE)
        }
      )
      validate(need(class(Modelo)!="try-error", "Check the components"))

      Modelo
    } else {
      dt <- dts(input$subset,input$varsubset,input$levelessub,data=data())
      dt[ , input$genotipo] <- as.factor(dt[ , input$genotipo])
      dt$col =  dt[,input$column]
      dt$row = dt[,input$fila]
      dt$col_f = factor( dt[,input$column])
      dt$row_f = factor( dt[,input$fila])
      ncols = input$segcol
      nrows = input$segrow
      for (i in 1:length(input$show_fixed)) {
        dt[, input$show_fixed[i]] <- as.factor(dt[, input$show_fixed[i]])
      }
      for (i in 1:length(input$show_random)) {
        dt[, input$show_random[i]] <- as.factor(dt[, input$show_random[i]])
      }
      if(is.null(input$show_fixed)) Fijo <- as.formula(~NULL)
      else Fijo <- as.formula(paste("", paste(input$show_fixed, collapse=" + "), sep=" ~ "))
      if(is.null(input$show_random)) Random <- as.formula(~ col_f+row_f)
      else Random <-  as.formula(paste("" ,paste(c(input$show_random,"col_f","row_f"), collapse=" + "), sep=" ~ "))
      Modelo=try(SpATS(response=input$variable,
                       genotype=input$genotipo, genotype.as.random=input$res_ran,
                       fixed= Fijo,
                       spatial = ~ PSANOVA(col, row, nseg = c(ncols,nrows), degree=c(3,3),nest.div=2),
                       random = Random, data=dt,
                       control = list(tolerance=1e-03, monitoring=1)),silent = T)
      tryCatch(
        {
          if(class(Modelo)=="try-error") stop("Error in the components of model")
        },
        error = function(e) {
          toastr_error(title = "Warning:", conditionMessage(e),position =  "bottom-right",progressBar = TRUE)
        }
      )
      validate(need(class(Modelo)!="try-error", "Check the components"))
      Modelo}

  },ignoreNULL = FALSE)  #Cambio Reactive


  ## to output blups
  blup <- reactive({
    validate(
      need(input$variable != "", " "),
      need(input$genotipo != "", " "),
      need(input$column != "", " "),
      need(input$fila != "", " ") )

    BLUPS <- predict(Modelo(),  which = input$genotipo)[,c(input$genotipo,"predicted.values","standard.errors")]
    BLUPS <- arrange(BLUPS,desc(predicted.values))
    BLUPS <- data.frame(BLUPS[,1],round(BLUPS[,2],2) , round(BLUPS[,3],2))
    names(BLUPS) <- if(isTRUE(input$res_ran)) c("Line","BLUPs","Standard.errors") else  c("Line","BLUEs","Standard.errors")
    BLUPS

  })

  # SALIDA de los datos en DT TAble
  output$blups <- DT::renderDataTable(
    if (input$action==0) {return()}
    else {
      DT::datatable({
        blup()
      },
      option=list(pageLength=10,columnDefs = list(list(className = 'dt-center', targets = 0:ncol(blup())))),
      filter="top",
      selection="multiple"
      )} )

  # Download PLOT SPATIAL
  output$descargar <- downloadHandler(
    filename = function() {
      paste("plotSpATS", input$typefile, sep = ".")
    },
    content = function(file){
      if(input$typefile=="png") {
        png(file,width = input$png.wid ,height = input$png.hei)
        plot(Modelo(),cex.lab = 1.5, cex.main = 2, cex.axis = 1.5, axis.args = list(cex.axis = 1.2))
        dev.off()
      } else {
        pdf(file,width = input$pdf.wid , height = input$pdf.hei )
        plot(Modelo(),cex.lab = 1.5, cex.main = 2, cex.axis = 1.5, axis.args = list(cex.axis = 1.2))
        dev.off()
      }
    }
  )

  # BLups or BLUEs

  output$efectos <- renderText({
    if (isTRUE(input$res_ran)) paste("BLUPs")
    else paste("BLUEs")
  })


  # BLUPS with confidence interval

  output$plotblup2 <- renderPlotly({
    validate(
      need(input$variable != "", "Complete Spatial Model"),
      need(input$genotipo != "", " "),
      need(input$column != "", " "),
      need(input$fila != "", " ") )

    if (input$action==0) {return()}
    else {

      BLUPS <- predict(Modelo(),  which = input$genotipo)[,c(input$genotipo,"predicted.values","standard.errors")]
      names(BLUPS) <- if(isTRUE(input$res_ran)) c("Line","BLUPs","Standard.errors") else  c("Line","BLUEs","Standard.errors")
      BLUPS$blcenter <- BLUPS[,2]-mean(BLUPS[,2])
      BLUPS$Lu <- BLUPS$blcenter-1.645*BLUPS[,3]; BLUPS$Ls <- BLUPS$blcenter+1.645*BLUPS[,3]

      p <- ggplot(BLUPS,aes(x=reorder(Line, -blcenter),y=blcenter))+geom_point(size = 1) +
        geom_errorbar(aes(ymax = Ls, ymin = Lu))+ theme_bw() +
        theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+
        ylab(ifelse(isTRUE(input$res_ran),"BLUPs","BLUEs"))
      isolate(ggplotly(p)) }
  })

  # Histograma
  output$hist <- renderPlotly({
    validate(
      need(input$variable != "", " "),
      need(input$genotipo != "", " "),
      need(input$column != "", " "),
      need(input$fila != "", " ") )
    if (input$action==0) {return()}
    else {

      hi <- hist(blup()[,2],plot = FALSE)
      br <- hi$breaks
      k <- ggplot(blup(), aes_string(ifelse(isTRUE(input$res_ran),"BLUPs","BLUEs"))) +
        geom_histogram(breaks=c(br)) + theme_bw() +
        ggtitle(paste0("Histogram of ",ifelse(isTRUE(input$res_ran),"BLUPs","BLUEs") ) )
      isolate(ggplotly(k)) }
  })

  #-----------------------------------   PLOT  SUMMARY  BLUPS MODELO SPATS  ----------------------------------------------------------

  output$plot_spats <- renderPlot({
    if (input$action==0) {return()}
    else {

      validate(
        need(input$variable != "", "Fill the fields above"),

        need(input$genotipo != "", " "),

        need(input$column != "", " "),

        need(input$fila != "", " ") )

      isolate(plot(Modelo()))

    }
  })


  # for summary SPATS
  output$summary2 <- renderPrint({

    validate(
      need(input$variable != "", "Complete the areas above")
    )

    validate(
      need(input$variable != "", "Please select the response variable")
    )
    validate(
      need(input$genotipo != "", "Please select the Genotype")
    )
    validate(
      need(input$column != "", "Please select the Column")
    )

    validate(
      need(input$fila != "", "Please select the Row")
    )

    if (input$action==0) { return() }
    else { isolate(summary(Modelo())) }

  })




  # Componentes de varianza
  output$varcomp <- renderPlotly({

    validate(
      need(input$variable != "", " "),  need(input$genotipo != "", " "),
      need(input$column != "", " "),  need(input$fila != "", ""))

    if (input$action==0)
      return()

    isolate({
      VarE<- round(sqrt(Modelo()$psi[1]),2)
      va <-  round(sqrt(Modelo()$var.comp),2)
      Comp <- data.frame(Component=c(names(Modelo()$var.comp),"Residual") ,  Standard_deviation=c(va,VarE)  )

      # Extraer el orden
      v <- as.character(Comp[order(Comp$Standard_deviation,decreasing = FALSE),1])
      g1 <-  ggplot(Comp,aes(x =Component,Standard_deviation))+geom_bar(position="dodge", stat="identity")+xlab("")+
        ggtitle("Components") + theme_bw(base_size = 15) + theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5)) +
        scale_x_discrete(limits=v)+ylab("Standard Deviation")

      ggplotly(g1) })

  })



  #Spatial TREND
  output$trend <- renderPlotly({

    validate(
      need(input$variable != "", "Complete the areas")
    )

    validate(
      need(input$genotipo != "", "Please select the Genotype")
    )
    validate(
      need(input$column != "", "Please select the Column")
    )

    validate(
      need(input$fila != "", "Please select the Row")
    )



    if (input$spatial==0) return()
    else {
      isolate({
        COL     <- obtain.spatialtrend(Modelo())[[1]]
        ROW     <- obtain.spatialtrend(Modelo())[[2]]
        SPATIAL <- obtain.spatialtrend(Modelo())$fit
        p <- plot_ly(x=COL,y=ROW,  z =SPATIAL) %>% add_surface( ) %>%
          layout(
            title = "Spatial Trend",
            scene = list(
              xaxis = list(title = "Column"),
              yaxis = list(title = "Row"),
              zaxis = list(title = "Z"),
              camera=list(
                eye = list(x=-1.5, y=-1.5, z=1.2)
              )
            ))
      }) }


  })


  #--------------------------------------------------------------------------------------------------------------------

  output$downloadData <- downloadHandler(
    filename = function() {
      paste("BLUPS_", input$variable ,".csv", sep = "")
    },
    content = function(file) {
      BLUPS <- predict(Modelo(),  which = input$genotipo)[,c(input$genotipo,"predicted.values","standard.errors")]
      names(BLUPS) <- if(isTRUE(input$res_ran)) c("Line","BLUPs","Standard.errors") else  c("Line","BLUEs","Standard.errors")
      write.csv(BLUPS, file, row.names = FALSE)
    }
  )

  output$downloadData2 <- downloadHandler(
    filename = function() {
      paste("BLUPS_",input$variable, ".csv", sep = "")
    },
    content = function(file) {
      BLUPS <- predict(Modelo(),  which = input$genotipo)[,c(input$genotipo,"predicted.values","standard.errors")]
      names(BLUPS) <- if(isTRUE(input$res_ran)) c("Line","BLUPs","Standard.errors") else  c("Line","BLUEs","Standard.errors")
      write.csv(BLUPS, file, row.names = FALSE)
    }
  )

  output$distTable <- DT::renderDataTable(   # Modal BLUPs Spatial
    if (input$action==0) {
      validate(
        need(input$action !=0, "Please select the components and run the model")
      )
    }
    else {
      DT::datatable({
        blup()
      },
      option=list(pageLength=10,columnDefs = list(list(className = 'dt-center', targets = 0:ncol(blup())))),
      filter="top",
      selection="multiple"
      )} )

  #---------------------------------------    VALUE BOXES             ------------------------------------------------------------


  observeEvent(!input$inf, toggle("heritability",anim = TRUE,time = 1,animType = "fade"))
  observeEvent(!input$inf, toggle("maxline",anim = TRUE,time = 1,animType = "fade"))
  observeEvent(!input$inf, toggle("minline",anim = TRUE,time = 1,animType = "fade"))
  observeEvent(!input$inf, toggle("cv",anim = TRUE,time = 1,animType = "fade"))

  # VALUEBOX for HERITABILIY
  output$heritability <- renderValueBox({

    validate(
      need(input$variable != "", " "), need(input$genotipo != "", " "),
      need(input$res_ran == TRUE, " "), need(input$column != "", " "),  need(input$fila != "", "")
    )

    if (isTRUE(input$able)) {
      validate(
        need(input$segrow != "", " "),
        need(input$segcol != "", " ")
      )
    }

    H <- getHeritability(Modelo())
    valueBox(H,subtitle = "Heritability",
             icon=icon("pagelines"),
             color = "yellow")

  })

  observeEvent(input$res_ran,{   # warning Message heritability
    tryCatch(
      {
        aleatorio <- input$res_ran
        if(!isTRUE(aleatorio)) stop("Remember: Heritability can only be calculated when genotype is random")
      },
      error = function(e) {
        toastr_warning(title = "INFO:", conditionMessage(e),position =  "bottom-right")
      }
    )
  })


  # VALUEBOX for Max line WAS changed by residual standard deviation
  output$maxline <- renderValueBox({
    validate(
      need(input$variable != "", " "),  need(input$genotipo != "", " "),
      need(input$column != "", " "),  need(input$fila != "", "")
    )

    if (isTRUE(input$able)) {
      validate(
        need(input$segrow != "", " "),
        need(input$segcol != "", " ")
      )
    }
    a <- round(sqrt(Modelo()$psi[1]),2)
    valueBox(a,subtitle = "Residual SD",
             icon=icon("arrow-circle-down"),
             color = "green")

  })



  # VALUEBOX for Min line  WAS changed by R Square
  output$minline <- renderValueBox({
    validate(
      need(input$variable != "", " "),  need(input$genotipo != "", " "),
      need(input$column != "", " "),  need(input$fila != "", "")
    )
    if (isTRUE(input$able)) {
      validate(
        need(input$segrow != "", " "),
        need(input$segcol != "", " ")
      )
    }
    a <- R.square(Modelo())
    valueBox(a,subtitle = "R-Square",
             icon=icon("arrow-circle-up"),
             color = "maroon")

  })


  # VALUEBOX for coeficiente de variacion
  output$cv <- renderValueBox({
    validate(
      need(input$variable != "", " "), need(input$genotipo != "", " "),
      need(input$res_ran == TRUE, " "), need(input$column != "", " "),  need(input$fila != "", "")
    )
    if (isTRUE(input$able)) {
      validate(
        need(input$segrow != "", " "),
        need(input$segcol != "", " ")
      )
    }
    me <- mean(blup()[,2],na.rm = TRUE)
    sd <- sqrt(Modelo()$var.comp[input$genotipo])
    valueBox(round(sd/me,2),subtitle = "Coefficient of Variation",
             icon=icon(list(src="cv.png", width="80px"), lib="local"),
             color = "blue")

  })

  # -------------------------- VAlue box DESCRIPTIVES -----------------



  # VALUEBOX for MAX RESPONSE
  output$resp <- renderValueBox({
    validate(
      need(input$Id088 != "", " ")
    )
    dt <- dts(input$subset,input$varsubset,input$levelessub,data=data())
    validate(
      need(is.numeric(dt[ , input$Id088]) , "Select a continous variable")
    )

    valueBox(max( round(dt[ , input$Id088],2),na.rm = TRUE),
             subtitle = paste0("Maximum ",input$Id088),
             icon=icon(list(src="MAX.png", width="80px"), lib="local"),
             color = "blue")

  })


  # VALUEBOX for MIN RESPONSE
  output$min <- renderValueBox({
    validate(
      need(input$Id088 != "", " ")
    )
    dt <- dts(input$subset,input$varsubset,input$levelessub,data=data())
    validate(
      need(is.numeric(dt[ , input$Id088]) , " ")
    )
    valueBox(min( round(dt[ , input$Id088],2),na.rm = TRUE),
             subtitle = paste0("Minimum ",input$Id088),
             icon=icon(list(src="MIN.png", width="80px"), lib="local"),
             color = "yellow")

  })


  # VALUEBOX for MEAN RESPONSE
  output$mean <- renderValueBox({
    validate(
      need(input$Id088 != "", " ")
    )
    dt <- dts(input$subset,input$varsubset,input$levelessub,data=data())
    validate(
      need(is.numeric(dt[ , input$Id088]) , " ")
    )
    valueBox( round(mean(dt[ , input$Id088],na.rm = TRUE),2),
              subtitle = paste0("Mean ",input$Id088),
              icon=icon(list(src="MEAN.png", width="80px"), lib="local"),
              color = "green")

  })


  # VALUEBOX for MEdian RESPONSE
  output$median <- renderValueBox({
    validate(
      need(input$Id088 != "", " ")
    )
    dt <- dts(input$subset,input$varsubset,input$levelessub,data=data())
    validate(
      need(is.numeric(dt[ , input$Id088]) , " ")
    )
    valueBox( round(median(dt[ , input$Id088],na.rm = TRUE),2),
              subtitle = paste0("Median ",input$Id088),
              icon=icon(list(src="MEDIAN.png", width="80px"), lib="local"),
              color = "blue")

  })


  # VALUEBOX for NA
  output$na <- renderValueBox({
    validate(
      need(input$Id088 != "", " ")
    )
    dt <- dts(input$subset,input$varsubset,input$levelessub,data=data())
    y <- dt[, input$Id088]
    valueBox(dim(dt[is.na(y),])[1],
             subtitle = "Missing values",
             icon=icon("window-close"),
             color = "red")

  })

  # TOTAL OBS
  output$obs <- renderValueBox({
    validate(
      need(input$Id088 != "", " ")
    )
    dt <- dts(input$subset,input$varsubset,input$levelessub,data=data())
    y <- dt[, input$Id088]
    valueBox(length(na.omit(y)),
             subtitle = "Observations",
             icon=icon("check-square"),
             color = "yellow")

  })



  # Rango intercuatilico
  output$IQR <- renderValueBox({
    validate(
      need(input$Id088 != "", " ")
    )
    dt <- dts(input$subset,input$varsubset,input$levelessub,data=data())
    validate(
      need(is.numeric(dt[ , input$Id088]) , " ")
    )
    x <- dt[, input$Id088]
    a <-quantile(x, 3/4,na.rm = TRUE); b <- quantile(x, 1/4,na.rm = TRUE)
    Rango <- paste(round(a,2) , round(b,2),sep = " - ")
    valueBox("IQR",
             subtitle = Rango ,
             icon=icon(list(src="IQR.png", width="80px"), lib="local"),
             color = "blue")

  })



  # Standar deviation
  output$sd <- renderValueBox({
    validate(
      need(input$Id088 != "", " ")
    )
    dt <- dts(input$subset,input$varsubset,input$levelessub,data=data())
    validate(
      need(is.numeric(dt[ , input$Id088]) , " ")
    )
    x <- dt[, input$Id088]
    x <- round(sd(x,na.rm = TRUE),2)
    valueBox(x,
             subtitle = "Standard Deviation" ,
             icon=icon(list(src="SD.png", width="80px"), lib="local"),
             color = "blue")

  })



  #------------------------------------     Residuales Spats              ---------------------------------------

  output$plotati <- renderPlotly({
    validate(
      need(input$variable != "", "Select the response variable"), need(input$genotipo != "", " "),
      need(input$column != "", " "),  need(input$fila != "", "")
    )


    dt <- dts(input$subset,input$varsubset,input$levelessub,data=data())
    dt[ , input$genotipo] <- as.factor(dt[ , input$genotipo])
    dt$col =  dt[,input$column]
    dt$row = dt[,input$fila]

    var <- dt[,input$variable]

    validate(
      need(is.numeric(var), "Select a continous variable")
    )

    MAD <- MAD_method(var)
    IQR <- IQR_method(var)

    O <- list(MAD,IQR); names(O) <- c("MAD","IQR")


    dt$Classify <- NA
    dt$Classify[c(which(var<O[[input$out]][1]),which(var>O[[input$out]][2]))] <- "Outlier"
    dt$Classify[which(var>O[[input$out]][1]&var<O[[input$out]][2])] <- "Normal"

    filter(dt,!is.na(Classify)) %>%
      ggplot(aes_string(x = input$column,y = input$fila,color="Classify")) + geom_point( size = 2)+ theme_bw() -> q
    q <-  q + scale_color_manual(values=c("grey80", "red"))+ggtitle(paste("Method: ", input$out))
    ggplotly(q)

  })


  # Reactive RAW OUTLIERS
  dataOut <- reactive({
    validate(
      need(input$variable != "", " "), need(input$genotipo != "", " "),
      need(input$column != "", " "),  need(input$fila != "", "")
    )

    dt <- dts(input$subset,input$varsubset,input$levelessub,data=data())
    dt[ , input$genotipo] <- as.factor(dt[ , input$genotipo])
    dt$col =  dt[,input$column]
    dt$row = dt[,input$fila]

    var <- dt[,input$variable]
    MAD <- MAD_method(var)
    IQR <- IQR_method(var)

    O <- list(MAD,IQR); names(O) <- c("MAD","IQR")

    datos <- dt[c(which(var<O[[input$out]][1]),which(var>O[[input$out]][2])),]
    datos
  })


  # Datos para RAW outliers
  output$OUT <- DT::renderDataTable(
    DT::datatable({
      dataOut()
    },
    option=list(pageLength=5,columnDefs = list(list(className = 'dt-center', targets = 0:ncol(dataOut())))),
    filter="top",
    selection="multiple"
    ))



  # Dataframe withouth outliers

  dataclean <- reactive({
    validate(
      need(input$variable != "", " "), need(input$genotipo != "", " "),
      need(input$column != "", " "),  need(input$fila != "", "")
    )

    dt <- dts(input$subset,input$varsubset,input$levelessub,data=data())
    var <- dt[,input$variable]
    MAD <- MAD_method(var)
    IQR <- IQR_method(var)

    O <- list(MAD,IQR); names(O) <- c("MAD","IQR")
    x <- c(which(var<O[["MAD"]][1]),which(var>O[["MAD"]][2]))
    y <- c(which(var<O[["IQR"]][1]),which(var>O[["IQR"]][2]))

    VarE<- Modelo()$psi[1]
    Data <- data.frame(Index=1:length(residuals(Modelo())), Residuals=residuals(Modelo()))
    u <- +3*sqrt(VarE)
    l <- -3*sqrt(VarE)
    z <- which(abs(Data$Residuals)>=u)

    out <- data.frame(Out=c(x,y,z),Method=c(rep("MAD",length(x)),
                                            rep("IQR",length(y)),
                                            rep("Residuals",length(z))))

    out <- unique(out[input$Id059%in%out$Method,]$Out)   #out <-  Reduce(union, list(x, y, z))

    datos <- dt[-out,]
    datos
  })

  output$downloadout <- downloadHandler(
    filename = function() {
      paste("Data_Clean_", input$variable ,".csv", sep = "")
    },
    content = function(file) {
      datos <- data.frame(dataclean())
      write.csv(datos, file, row.names = FALSE)
    }
  )

  # Residuales

  output$plotati2 <- renderPlotly({
    validate(
      need(input$variable != "", " "), need(input$genotipo != "", " "),
      need(input$column != "", " "),  need(input$fila != "", "")
    )
    if (input$action==0)
      return()
    isolate({


      VarE<- Modelo()$psi[1]
      Data <- data.frame(Index=1:length(residuals(Modelo())), Residuals=residuals(Modelo()))
      u <- +3*sqrt(VarE)
      l <- -3*sqrt(VarE)
      Data$Classify <- NA
      Data$Classify[which(abs(Data$Residuals)>=u)] <- "Outlier"
      Data$Classify[which(abs(Data$Residuals)<u)  ] <- "Normal"
      filter(Data,!is.na(Classify)) %>%
        ggplot(aes(x=Index,y=Residuals,color=Classify))+geom_point(size=2,alpha = 0.3)+theme_bw()+
        scale_color_manual(values=c("grey80", "red"))+
        geom_hline(yintercept = u,color="red")+geom_hline(yintercept = l,color="red")+
        geom_hline(yintercept = 0,linetype="dashed")->k

      ggplotly(k)

    })
  })

  # Mapa outliers
  output$mapout <- renderPlotly({
    validate(
      need(input$variable != "", " "), need(input$genotipo != "", " "),
      need(input$column != "", " "),  need(input$fila != "", "")
    )
    if (input$action==0)
      return()
    isolate({

      dt <- dts(input$subset,input$varsubset,input$levelessub,data=data())
      dt$col =  dt[,input$column]
      dt$row = dt[,input$fila]


      VarE<- Modelo()$psi[1]
      Data <- data.frame(Index=1:length(residuals(Modelo())), Residuals=residuals(Modelo()),Col=dt$col,Row=dt$row)
      u <- +3*sqrt(VarE)
      l <- -3*sqrt(VarE)
      Data$Classify <- NA
      Data$Classify[which(abs(Data$Residuals)>=u)] <- "Outlier"
      Data$Classify[which(abs(Data$Residuals)<u)] <- "Normal"
      filter(Data,!is.na(Classify)) %>%
        ggplot(aes(x=Col,y=Row,color=Classify))+geom_point(size=2)+theme_bw()+
        scale_color_manual(values=c("grey80", "red"))->k

      ggplotly(k)
    })
  })

  # Residuals vs Fitted values
  output$fitted <- renderPlotly({
    validate(
      need(input$variable != "", " "), need(input$genotipo != "", " "),
      need(input$column != "", " "),  need(input$fila != "", "")
    )
    if (input$action==0)
      return()
    isolate({

      Fit<- fitted.values(Modelo())
      VarE<- Modelo()$psi[1]
      Data <- data.frame(Index=1:length(residuals(Modelo())), Residuals=residuals(Modelo()), Fitted.Values=Fit)
      u <- +3*sqrt(VarE)
      l <- -3*sqrt(VarE)
      Data$Classify <- NA
      Data$Classify[which(abs(Data$Residuals)>=u)] <- "Outlier"
      Data$Classify[which(abs(Data$Residuals)<u)] <- "Normal"
      filter(Data,!is.na(Classify)) %>%
        ggplot(aes(x=Fitted.Values,y=Residuals,color=Classify))+geom_point(size=2,alpha = 0.3)+theme_bw()+
        scale_color_manual(values=c("grey80", "red"))+xlab("Fitted Values")+
        geom_hline(yintercept = 0,linetype="dashed")->k

      ggplotly(k)
    })
  })


  # Reactive OUTLIERS
  dataOut2 <- reactive({
    validate(
      need(input$variable != "", " "), need(input$genotipo != "", " "),
      need(input$column != "", " "),  need(input$fila != "", "")
    )
    if (input$action==0)
      return()
    isolate({
      dt <- dts(input$subset,input$varsubset,input$levelessub,data=data())
      VarE<- Modelo()$psi[1]
      Data <- data.frame(Index=1:length(residuals(Modelo())), Residuals=residuals(Modelo()))
      u <- +3*sqrt(VarE)
      l <- -3*sqrt(VarE)
      p <- which(abs(Data$Residuals)>u)
      datos <-dt[p,]
      datos})
  })


  # Datos para outliers
  output$OUT2 <- DT::renderDataTable(
    if (input$action==0) return()
    else{
      DT::datatable({
        dataOut2()
      },
      option=list(pageLength=5,columnDefs = list(list(className = 'dt-center', targets = 0:ncol(dataOut2())))),
      filter="top",
      selection="multiple"
      )})


  output$qqplot <- renderPlotly({
    validate(
      need(input$variable != "", " "), need(input$genotipo != "", " "),
      need(input$column != "", " "),  need(input$fila != "", "")
    )
    if (input$action==0)
      return()
    isolate({

      Fit<- fitted.values(Modelo())
      VarE<- Modelo()$psi[1]
      Data <- data.frame(Index=1:length(residuals(Modelo())), Residuals=residuals(Modelo()), Fitted.Values=Fit)
      u <- +3*sqrt(VarE)
      l <- -3*sqrt(VarE)
      Data$Classify <- NA
      Data$Classify[which(abs(Data$Residuals)>=u)] <- "Outlier"
      Data$Classify[which(abs(Data$Residuals)<u)] <- "Normal"
      filter(Data,!is.na(Classify)) %>%
        ggqqplot(x="Residuals",fill="Classify",ggtheme=theme_bw())-> q
      ggplotly(q)
    })


  })



  # ----------------------------------- hipervinculo dentro ------------------------------------------

  # Jump from other tab inside Application
  observeEvent(input$toAwesome1, {
    updateTabItems(session = session, inputId = "tabs", selected = "data")
  })

  observeEvent(input$toAwesome2, {
    updateTabItems(session = session, inputId = "tabs", selected = "modelo")
  })


  observeEvent(input$toAwesome3, {
    updateTabItems(session = session, inputId = "tabs", selected = "mixed")
  })


  observeEvent(input$toAwesome4, {
    updateTabItems(session = session, inputId = "tabs", selected = "Comparison")
  })
  #------------------------------------     Modelo MIXTO              ---------------------------------------


  output$efectos2 <- renderText({
    if (isTRUE(input$res_ran2)) paste("BLUPs")
    else paste("BLUEs")
  })



  output$line <- renderText({
    if (isTRUE(input$res_ran2)) paste0("(1|",input$genotipo2,")")
    else paste0(input$genotipo2)
  })


  # InfoBOX for Formula
  output$form <- renderInfoBox({

    validate(
      need(input$variable2 != "", " "), need(input$genotipo2 != "", " ")
    )

    if (isTRUE(input$res_ran2)) {
      gen <- paste("(1|",input$genotipo2,")")
    } else{ gen <-  paste(input$genotipo2)}

    if (input$formula!="") {
      gen <-  paste0(gen," + ")
    } else { gen <- gen }

    infoBox(title = "Formula",value = paste0(input$variable2," ~ ", gen, input$formula),
            subtitle = "Check this",width = 3,icon = icon("code"),color = "blue",fill = TRUE)

  })

  ## Modelo alpha
  alpha <- eventReactive( input$run, {

    validate(
      need(input$variable2 != "", " "), need(input$genotipo2 != "", " ")
    )

    dt <- data.frame(dts(input$subset,input$varsubset,input$levelessub,data=data()))
    dt[ , input$genotipo2] <- as.factor(dt[ , input$genotipo2])

    #  Modelo  ####
    Response  <- dt[ ,input$variable2]
    Gen       <-as.factor(dt[ ,input$genotipo2] )

    if(input$res_ran2==TRUE){
      if (input$Id039==1) {                                # Alpha-lattice
        Replicate <-as.factor(dt[ ,input$Id086]  )
        Block  <-as.factor(dt[ ,input$Id087]     )
        Modelo = lmer(Response ~ (1|Gen) + Replicate + (1|Replicate:Block)) }
      else if (input$Id039==2)   {                         # Bloques
        Replicate <-as.factor(dt[ ,input$Id086]  )
        Modelo = lmer(Response ~ (1|Gen) + (1|Replicate)) }
      else if (input$Id039==3)  {                          # Free

        gen <- paste("(1|",input$genotipo2,")")

        if (input$formula!="") {
          gen <-  paste0(gen," + ")
        } else { gen <- gen }

        equation <- as.formula(paste0(input$variable2," ~ ", gen, input$formula))
        Modelo = try(lmer(formula = equation , data=dt ),silent = TRUE)

        tryCatch(
          {
            if(class(Modelo)=="try-error") stop("Error in formula")
          },
          error = function(e) {
            toastr_error(title = "Warning:", conditionMessage(e),position =  "bottom-right",progressBar = TRUE)
          }
        )
        validate(need(class(Modelo)!="try-error", "Check the formula"))
        Modelo     }
    }

    if(input$res_ran2==FALSE) {
      if (input$Id039==1){
        Replicate <-as.factor(dt[ ,input$Id086]  )
        Block  <-as.factor(dt[ ,input$Id087]     )
        Modelo = lmer(Response ~ Gen + Replicate + (1|Replicate:Block)); Modelo }
      else if (input$Id039==2)   {
        Replicate <-as.factor(dt[ ,input$Id086]  )
        Modelo = lmer(Response ~ Gen + (1|Replicate)); Modelo}
      else if (input$Id039==3)  {

        gen <-  paste(input$genotipo2)

        if (input$formula!="") {
          gen <-  paste0(gen," + ")
        } else { gen <- gen }

        equation <- as.formula(paste0(input$variable2," ~ ", gen, input$formula))
        Modelo = try(lmer(formula = equation , data=dt ),silent = TRUE)

        tryCatch(
          {
            if(class(Modelo)=="try-error") stop("Error in formula")
          },
          error = function(e) {
            toastr_error(title = "Warning:", conditionMessage(e),position =  "bottom-right",progressBar = TRUE)
          }
        )
        validate(need(class(Modelo)!="try-error", "Check the formula"))
        Modelo     }
    }

    Modelo


  },ignoreNULL = FALSE)



  # for summary alpha
  output$summaryalpha <- renderPrint({

    validate(
      need(input$variable2 != "", "Complete the areas above")
    )
    validate(
      need(input$genotipo2 != "", "Please select the Genotype")
    )

    if(input$Id039==1|input$Id039==2)  validate(need(input$Id086 != "", "Select the Replicate"))
    if(input$Id039==1) validate(need(input$Id087 != "", "Select Block "))

    validate(need(input$run!=0,"Run the model"))

    if (input$run==0) {return()}
    else {
      isolate( print( summary(alpha(),correlation = FALSE)))
    }
  })

  # Blups mixed model -----------------------


  ## to output blups Mixed
  blup_mix <-  eventReactive( input$run,{
    validate(
      need(input$variable2 != "", " "),
      need(input$genotipo2 != "", " "))

    if(input$Id039==1|input$Id039==2)  validate(need(input$Id086 != "", "Select the Replicate"))
    if(input$Id039==1) validate(need(input$Id087 != "", "Select Block "))



    if (input$run==0) {return()}
    else {
      isolate({
        if (input$res_ran2==TRUE) {
          if (input$Id039==3) {
            BLUPS <- ranef(alpha())[[input$genotipo2]]+fixef(alpha())[1]
            BLUPS <- data.frame(as.factor(row.names(BLUPS)),BLUPS[,1])
            colnames(BLUPS) <- c("Genotype","Effect")
            BLUPS <- arrange(BLUPS,desc(Effect))
            BLUPS <- data.frame(BLUPS[,1],round(BLUPS[,2],2))
            names(BLUPS) <- c("Line","BLUPs")
            d <- broom.mixed::augment(ranef(alpha()))
            d <- d[d$grp==input$genotipo2,c("level","std.error")]
            d <- data.frame(level=d[,1],std.error=round(d[,2],2))
            BLUPS <- merge(BLUPS,d,by.x="Line",by.y="level")
            BLUPS
          } else {
            BLUPS <- ranef(alpha())[["Gen"]]+fixef(alpha())[1]
            BLUPS <- data.frame(as.factor(row.names(BLUPS)),BLUPS[,1])
            colnames(BLUPS) <- c("Genotype","Effect")
            BLUPS <- arrange(BLUPS,desc(Effect))
            BLUPS <- data.frame(BLUPS[,1],round(BLUPS[,2],2))
            names(BLUPS) <- c("Line","BLUPs")
            d <- broom.mixed::augment(ranef(alpha()))
            d <- d[d$grp=="Gen",c("level","std.error")]
            d <- data.frame(level=d[,1],std.error=round(d[,2],2))
            BLUPS <- merge(BLUPS,d,by.x="Line",by.y="level")
            BLUPS
          }
        } else {
          if (input$Id039==3) {
            BLUES <- data.frame(ls_means(alpha(),input$genotipo2))
            BLUES <-  arrange(BLUES,desc(Estimate))
          }
          BLUES <-data.frame(ls_means(alpha(),"Gen"))
          BLUES <-  arrange(BLUES,desc(Estimate))
          BLUES %>%
            mutate_if(is.numeric, round, digits=2)
        }

      })}


  },ignoreNULL = FALSE)

  # SALIDA de los MIXED BLUPS en DT TAble
  output$blups_mixed <- DT::renderDataTable(
    if (input$run==0) {return()}
    else {

      isolate({
        DT::datatable({
          blup_mix()
        },
        option=list(pageLength=5,columnDefs = list(list(className = 'dt-center', targets = 0:ncol(blup_mix())))),
        filter="top",
        selection="multiple"
        )})
    })

  # DEscargar blups Mixed model

  output$desc_mixed <- downloadHandler(
    filename = function() {
      paste("Effect_Mix", input$variable2 ,".csv", sep = "")
    },
    content = function(file) {
      BLUPS <- blup_mix()
      write.csv(BLUPS, file, row.names = FALSE)
    }
  )

  # bLUPS


  output$plot_effects <- renderPlotly({
    if (input$run==0) {return()}
    else {

      validate(
        need(input$variable2 != "", "Fill the fields above"),

        need(input$genotipo2 != "", " "))

      if(input$Id039==1|input$Id039==2)  validate(need(input$Id086 != "", "Select the Replicate"))
      if(input$Id039==1) validate(need(input$Id087 != "", "Select Block "))

      if(input$res_ran2==FALSE){
        Me <- data.frame(ls_means(alpha(),"Gen"))
        v <- as.character(Me[order(Me$Estimate,decreasing = TRUE),2])
        g1 <-  ggplot(Me,aes(x =levels,Estimate))+geom_point(size = 1) +
          geom_errorbar(aes(ymax = upper, ymin = lower))+ theme_bw() +
          theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+
          scale_x_discrete(limits=v)
        isolate( ggplotly(g1))
      }
      else {

        if (input$Id039==3) {
          BLUPS <- ranef(alpha())[[input$genotipo2]]+fixef(alpha())[1]
          BLUPS <- data.frame(as.factor(row.names(BLUPS)),BLUPS[,1])
          colnames(BLUPS) <- c("Genotype","Effect")
          BLUPS <- arrange(BLUPS,desc(Effect))
          BLUPS <- data.frame(BLUPS[,1],round(BLUPS[,2],2)); colnames(BLUPS) <- c("Genotype","Effect")
          d <- broom.mixed::augment(ranef(alpha()))
          d <- d[d$grp==input$genotipo2,]
          BLUPS <- merge(BLUPS,d,by.x="Genotype",by.y="level")
          v <- as.character(BLUPS[order(BLUPS$Effect,decreasing = TRUE),1])
          p <- ggplot(BLUPS,aes(x=Genotype,y=Effect))+
            geom_errorbar(aes(ymax = ub+fixef(alpha())[1], ymin = lb+fixef(alpha())[1]))+ theme_bw()+
            theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+
            geom_point(size=1)+scale_x_discrete(limits=v)
          isolate( ggplotly(p) )
        } else {
          BLUPS <- ranef(alpha())[["Gen"]]+fixef(alpha())[1]
          BLUPS <- data.frame(as.factor(row.names(BLUPS)),BLUPS[,1])
          colnames(BLUPS) <- c("Genotype","Effect")
          BLUPS <- arrange(BLUPS,desc(Effect))
          BLUPS <- data.frame(BLUPS[,1],round(BLUPS[,2],2)); colnames(BLUPS) <- c("Genotype","Effect")
          d <- broom.mixed::augment(ranef(alpha()))
          d <- d[d$grp=="Gen",]
          BLUPS <- merge(BLUPS,d,by.x="Genotype",by.y="level")
          v <- as.character(BLUPS[order(BLUPS$Effect,decreasing = TRUE),1])
          p <- ggplot(BLUPS,aes(x=Genotype,y=Effect))+
            geom_errorbar(aes(ymax = ub+fixef(alpha())[1], ymin = lb+fixef(alpha())[1]))+ theme_bw()+
            theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+
            geom_point(size=1)+
            scale_x_discrete(limits=v)
          isolate( ggplotly(p) )
        }

      }

    }
  })

  # Residuals Mixed model qqplot

  output$Normality <- renderPlotly({
    if (input$run==0) {return()}
    else {

      validate(
        need(input$variable2 != "", "Fill the fields above"),

        need(input$genotipo2 != "", " "))

      if(input$Id039==1|input$Id039==2)  validate(need(input$Id086 != "", "Select the Replicate"))
      if(input$Id039==1) validate(need(input$Id087 != "", "Select Block "))

      Fit<- fitted.values(alpha())
      Modelo.vars   <- as.data.table(VarCorr(alpha()))
      VarE <- Modelo.vars[grp=='Residual','vcov']
      Data <- data.frame(Index=1:length(residuals(alpha())), Residuals=residuals(alpha()), Fitted.Values=Fit)
      u <- +3*sqrt(VarE)
      l <- -3*sqrt(VarE)
      Data$Classify <- NA
      Data$Classify[which(abs(Data$Residuals)>=u$vcov)] <- "Outlier"
      Data$Classify[which(abs(Data$Residuals)<u$vcov)] <- "Normal"
      filter(Data,!is.na(Classify)) %>%
        ggqqplot(x="Residuals",fill="Classify",ggtheme=theme_bw())-> q
      isolate(ggplotly(q))

    }
  })



  output$fitted2 <- renderPlotly({
    validate(
      need(input$variable2 != "", " "), need(input$genotipo2 != "", " ")  )

    if(input$Id039==1|input$Id039==2)  validate(need(input$Id086 != "", "Select the Replicate"))
    if(input$Id039==1) validate(need(input$Id087 != "", "Select Block "))

    Fit<- fitted.values(alpha())
    Modelo.vars   <- as.data.table(VarCorr(alpha()))
    VarE <- as.numeric(Modelo.vars[grp=='Residual','vcov'])
    Data <- data.frame(Index=1:length(residuals(alpha())), Residuals=residuals(alpha()), Fitted.Values=Fit)
    u <- +3*sqrt(VarE)
    l <- -3*sqrt(VarE)
    Data$Classify <- NA
    Data$Classify[which(abs(Data$Residuals)>=u)] <- "Outlier"
    Data$Classify[which(abs(Data$Residuals)<u)] <- "Normal"
    filter(Data,!is.na(Classify)) %>%
      ggplot(aes(x=Fitted.Values,y=Residuals,color=Classify))+geom_point(size=2,alpha = 0.3)+theme_bw()+
      scale_color_manual(values=c("grey80", "red"))+xlab("Fitted Values")+
      geom_hline(yintercept = u,color="red")+geom_hline(yintercept = l,color="red")+
      geom_hline(yintercept = 0,linetype="dashed")->k
    ggplotly(k)

  })



  output$plotati3 <- renderPlotly({

    validate(
      need(input$variable2 != "", " "), need(input$genotipo2 != "", " ")  )

    if(input$Id039==1|input$Id039==2)  validate(need(input$Id086 != "", "Select the Replicate"))
    if(input$Id039==1) validate(need(input$Id087 != "", "Select Block "))

    Fit<- fitted.values(alpha())
    Modelo.vars   <- as.data.table(VarCorr(alpha()))
    VarE <- as.numeric(Modelo.vars[grp=='Residual','vcov'])
    Data <- data.frame(Index=1:length(residuals(alpha())), Residuals=residuals(alpha()), Fitted.Values=Fit)
    u <- +3*sqrt(VarE)
    l <- -3*sqrt(VarE)
    Data$Classify <- NA
    Data$Classify[which(abs(Data$Residuals)>=u)] <- "Outlier"
    Data$Classify[which(abs(Data$Residuals)<u)] <- "Normal"

    p <- which(abs(Data$Residuals)>=u)
    tryCatch(
      {
        if(length(p)>=1) stop(paste("Outliers detected:",length(p),sep = " "))
      },
      error = function(e) {
        toastr_info(title = "Info:", conditionMessage(e),position =  "bottom-right",progressBar = TRUE)
      }
    )
    filter(Data,!is.na(Classify)) %>%
      ggplot(aes(x=Index,y=Residuals,color=Classify))+geom_point(size=2,alpha = 0.3)+theme_bw()+
      scale_color_manual(values=c("grey80", "red"))+
      geom_hline(yintercept = u,color="red")+geom_hline(yintercept = l,color="red")+
      geom_hline(yintercept = 0,linetype="dashed")->k
    ggplotly(k)
  })


  # Reactive OUTLIERS Mixed Model
  dataOut3 <- reactive({

    validate(
      need(input$variable2 != "", " "), need(input$genotipo2 != "", " ")  )

    if(input$Id039==1|input$Id039==2)  validate(need(input$Id086 != "", "Select the Replicate"))
    if(input$Id039==1) validate(need(input$Id087 != "", "Select Block "))


    if (input$run==0)
      return()
    isolate({
      dt <- dts(input$subset,input$varsubset,input$levelessub,data=data())
      Fit<- fitted.values(alpha())
      Modelo.vars   <- as.data.table(VarCorr(alpha()))
      VarE <- as.numeric(Modelo.vars[grp=='Residual','vcov'])
      Data <- data.frame(Index=1:length(residuals(alpha())), Residuals=residuals(alpha()), Fitted.Values=Fit)
      u <- +3*sqrt(VarE)
      l <- -3*sqrt(VarE)
      vect_res2 <- residuals(alpha())
      p <- as.numeric(names(vect_res2)[abs(vect_res2)>=u])
      datos <-dt[p,]
      datos})
  })


  # Datos para outliers Mixed Model
  output$OUT3 <- DT::renderDataTable(
    if (input$run==0) return()
    else{
      DT::datatable({
        dataOut3()
      },
      option=list(pageLength=5,columnDefs = list(list(className = 'dt-center', targets = 0:ncol(dataOut3())))),
      filter="top",
      selection="multiple"
      )})


  observeEvent(input$run ,{

    validate(
      need(input$variable2 != "", " "), need(input$genotipo2 != "", " ")  )

    if(input$Id039==1|input$Id039==2)  validate(need(input$Id086 != "", "Select the Replicate"))
    if(input$Id039==1) validate(need(input$Id087 != "", "Select Block "))

    Fit<- fitted.values(alpha())
    Modelo.vars   <- as.data.table(VarCorr(alpha()))
    VarE <- as.numeric(Modelo.vars[grp=='Residual','vcov'])
    Data <- data.frame(Index=1:length(residuals(alpha())), Residuals=residuals(alpha()), Fitted.Values=Fit)
    u <- +3*sqrt(VarE)
    l <- -3*sqrt(VarE)
    Data$Classify <- NA
    Data$Classify[which(abs(Data$Residuals)>=u)] <- "Outlier"
    Data$Classify[which(abs(Data$Residuals)<u)] <- "Normal"

    p <- which(abs(Data$Residuals)>=u)
    tryCatch(
      {
        if(length(p)>=1) stop(paste("Outliers detected:",length(p),sep = " "))
      },
      error = function(e) {
        toastr_info(title = "Info:", conditionMessage(e),position =  "bottom-right",progressBar = TRUE)
      }
    )

  })

  #-------------------------------------------



  # VALUEBOX MIXED MODEL
  output$rep <- renderValueBox({

    validate(
      need(input$Id086 != "", " ")
    )

    nreps <- nlevels(as.factor(dts(input$subset,input$varsubset,input$levelessub,data=data())[,input$Id086]))
    valueBox(nreps,subtitle = "Replicates",
             icon=icon("sync"),
             color = "yellow")

  })

  output$block <- renderValueBox({

    validate(
      need(input$Id087 != "", " ")
    )

    nreps <- nlevels(as.factor(dts(input$subset,input$varsubset,input$levelessub,data=data())[,input$Id087]))
    valueBox(nreps,subtitle = "Blocks",
             icon=icon("th"),
             color = "blue")

  })

  # --------------------------- MODAL MIXED MODEL --------------------------------------


  output$glance <- DT::renderDataTable({   # Modal BLUPs Spatial

    validate(
      need(input$variable2 != "", " "),
      need(input$genotipo2 != "", " "))

    if(input$Id039==1|input$Id039==2)  validate(need(input$Id086 != "", " "))
    if(input$Id039==1) validate(need(input$Id087 != "", " "))

    if (input$run==0) {return()}
    else{
      isolate({
        DT::datatable({
          round(data.frame(broom.mixed::glance(alpha())),2)
        },  options = list(paging = FALSE,searching = FALSE))
      })
    }
  })


  observeEvent(input$res_ran2,{   # warning Message heritability
    tryCatch(
      {
        aleatorio <- input$res_ran2
        if(!isTRUE(aleatorio)) stop("Remember: Heritability can only be calculated when genotype is random")
      },
      error = function(e) {
        toastr_warning(title = "INFO:", conditionMessage(e),position =  "bottom-right")
      }
    )
  })

  # VALUEBOX for HERITABILIY
  output$hcullis <- renderValueBox({

    validate(
      need(input$variable2 != "", " "), need(input$genotipo2 != "", " "),
      need(input$res_ran2 == TRUE, " ")
    )

    if(input$Id039==1|input$Id039==2)  validate(need(input$Id086 != "", " "))
    if(input$Id039==1) validate(need(input$Id087 != "", "  "))

    validate(need(input$run!=0," "))

    dat <- data.frame(dts(input$subset,input$varsubset,input$levelessub,data=data()))
    dat[ , input$genotipo2] <- as.factor(dat[ , input$genotipo2])
    Geno <-as.character(sort(unique(dat[,input$genotipo2][!is.na(dat[,input$variable2])])))
    H <- try(Heri.cullis(Model=alpha(), Nom.gen = ifelse(input$Id039==3,input$genotipo2,"Gen")),silent = TRUE)

    tryCatch(
      {

        if(!is.numeric(H)) stop("Check the model")
      },
      error = function(e) {
        toastr_error(title = "Error:", conditionMessage(e),position =  "bottom-right",progressBar = TRUE)
      }
    )

    validate(need(is.numeric(H),"Check the model"))


    H <- round(H,2)
    valueBox(H,subtitle = "Heritability",
             icon=icon("pagelines"),
             color = "light-blue")

  })




  # VALUEBOX for Variance residual
  output$varres <- renderValueBox({

    validate(
      need(input$variable2 != "", " "),
      need(input$genotipo2 != "", " "))

    if(input$Id039==1|input$Id039==2)  validate(need(input$Id086 != "", " "))
    if(input$Id039==1) validate(need(input$Id087 != "", " "))

    validate(need(input$run!=0," "))

    vc   <- as.data.table(VarCorr(alpha())) # extract estimated variance components (vc)
    vc.e <- sqrt(vc[grp=="Residual", vcov])
    v <- round(vc.e,2)
    valueBox(v,subtitle = "Residual SD",
             icon=icon(list(src="SD.png", width="80px"), lib="local"),
             color = "red")


  })



  # VALUEBOX for Variance Genotypic
  output$vargen <- renderValueBox({

    validate(
      need(input$variable2 != "", " "), need(input$genotipo2 != "", " "),
      need(input$res_ran2 == TRUE, " ")
    )

    if(input$Id039==1|input$Id039==2)  validate(need(input$Id086 != "", " "))
    if(input$Id039==1) validate(need(input$Id087 != "", "  "))

    validate(need(input$run!=0," "))

    vc   <- as.data.table(VarCorr(alpha())) # extract estimated variance components (vc)
    vc.e <- sqrt(vc[grp==ifelse(input$Id039==3,input$genotipo2,"Gen"), vcov])
    v <- round(vc.e,2)
    valueBox(v,subtitle = "Genotypic SD",
             icon=icon("pagelines"),
             color = "green")


  })

  # RANOVA  mixed model

  output$printranova<- renderPrint({

    validate(
      need(input$variable2 != "", "Complete the information"),
      need(input$genotipo2 != "", "Complete the information"))

    if(input$Id039==1|input$Id039==2)  validate(need(input$Id086 != "", "Complete the information"))
    if(input$Id039==1) validate(need(input$Id087 != "", "Complete the information"))


    validate(need(input$run!=0,"Run the model"))

    dt <- dts(input$subset,input$varsubset,input$levelessub,data=data())
    ranova(alpha())


  })


  output$anovamix <- DT::renderDataTable({
    validate(
      need(input$variable2 != "", " "),
      need(input$genotipo2 != "", " "))

    if(input$Id039==1|input$Id039==2)  validate(need(input$Id086 != "", " "))
    if(input$Id039==1) validate(need(input$Id087 != "", " "))

    nfixed <- nrow(anova(alpha(),ddf = "Kenward-Roger", type = 1))
    if (input$run==0) {return()}
    else if (nfixed<1) {validate(need(nfixed>1,"ANOVA is only calculated for fixed effects"))}
    else{
      isolate({
        k <- suppressWarnings(data.frame(tidy(anova(alpha(),ddf = "Kenward-Roger", type = 1))))
        DT::datatable({
          cbind(term=k[,1],round(k[,2:7],2))
        },  options = list(paging = FALSE,searching = FALSE))
      })
    }
  })



  #---------------------------------------    Comparison       -------------------------------------


  observe({
    inFile <- input$file2
    Ext <- tools::file_ext(inFile$datapath)
    if (is.null(inFile)) { dt <- data.frame() }
    else {
      if(Ext=="xlsx"|Ext=="xls") {
        if(input$miss=="Empty"){ P = "\" \""} else {P = "NA"}
        dt <- as.data.frame(read_excel(inFile$datapath,col_names = input$header2,na = P))
      }else {
        dt <-  read.csv(inFile$datapath, header=input$header2,sep=input$sep2)
      }
    }
  })



  observeEvent(input$file2,{   # warning Message
    tryCatch(
      {
        inFile <- input$file2
        Ext <- tools::file_ext(inFile$datapath)
        if(!Ext%in%c("csv","xls","xlsx","txt")) stop("File not allowed")
        toastr_info("Now check: Header, Missing Value Character, Cell Separation Character!",position =  "bottom-right")
        toastr_success("File loaded!",position =  "bottom-right")

      },
      error = function(e) {
        toastr_error(title = "Database error", conditionMessage(e),position =  "bottom-right")
      }
    )
  })




  data_comp <- reactive({
    inFile <- input$file2
    Ext <- tools::file_ext(inFile$datapath)

    validate(
      need(input$file2 != "", "Upload file")
    )

    if (is.null(inFile)) { dt <- data.frame() }
    else {
      if(Ext=="xlsx"|Ext=="xls") {
        if(input$miss=="Empty"){ P = "\" \""} else {P = "NA"}
        dt <- as.data.frame(read_excel(inFile$datapath,col_names = input$header2,na = P))
      }else {
        dt <-  read.csv(inFile$datapath, header=input$header2,sep=input$sep2)
      }
    }
  })


  output$correlation <- renderPlot({

    validate(

      need(!is.null(input$file2), " Load Data") )

    Base <- data_comp()
    mgc_mod = Base[,-1]
    myCors1 = corr.test(mgc_mod, use = 'pairwise.complete.obs', adjust = "fdr", alpha = 0.05)
    myCors1 = myCors1[c(1,4)]

    myCors1 = lapply(myCors1, function(x){
      x[upper.tri(x, T)] = NA
      x[upper.tri(x, T) & lower.tri(x,T)] = 1
      x  = as.data.frame(t(x))
      x[,'col'] = colnames(x)
      x  = reshape::melt(x, id='col')
      colnames(x) = c('col','row','value')
      x$name = round(x$value,2)
      x$col = factor(x$col, levels = colnames(mgc_mod))
      x$row = factor(x$row, levels = rev(colnames(mgc_mod)))
      x = na.omit(x)
    })

    myCors1 = merge(x = myCors1$r, y = myCors1$p, by = c('col','row'))
    myCors1 = myCors1[,c(1,2,4,5)]

    myCors1$signi = cut(x = myCors1$value.y, breaks = c(0, .0001, .001, .01, Inf), right = F, labels = c('***','**','*', 'ns'))
    myCors1[myCors1$col == myCors1$row, 'name.x'] = NA
    myCors1$label = paste(myCors1$name.x, myCors1$sign, sep='\n')
    myCors1[myCors1$col == myCors1$row, 'label'] = rep(1,length(names(mgc_mod)))
    textColors1 = ifelse(myCors1$name.x<0.6 & myCors1$name.x>=-0.7, 'black', 'white')
    textColors1[is.na(textColors1)] = 'white'
    p = ggplot(data = myCors1, aes(x=col, y=row, fill=name.x)) +
      geom_tile(color='gray') + theme_minimal(base_size=15) +
      geom_text(aes(x=col, y=row, label=label), color=textColors1, size=5) +
      scale_fill_gradient2(low='#db4437', mid='white', high='#4285f4') + labs(x=NULL, y=NULL) +
      theme(axis.text.x = element_text(angle = 40, hjust = 1), legend.position='none',
            panel.grid.minor.x = element_blank(), panel.grid.major = element_blank())
    p


  })


  output$pru <- DT::renderDataTable(
    DT::datatable({

      data_comp()
    },
    option=list(pageLength=5,columnDefs = list(list(className = 'dt-center', targets = 0:ncol(data_comp())))),
    filter="top",
    selection="multiple"
    ))

  # PCA --------------------------------------------------
  # Data Imputed

  data_Imput <- reactive({
    datos <- data_comp()
    if (isTRUE(anyNA(datos[,-1]))) {
      datos1 <- imputePCA(datos[,-1],ncp = 4 ,method = "EM")
      datos1 <- datos1$completeObs
    } else {
      datos1 <- datos[,-1]
    }

    rownames(datos1) <- datos[,1]
    datos1
  })


  output$biplot <- renderPlot({

    if(!input$compa)
      return()

    isolate({
      validate(
        need(!is.null(input$file2), " Load Data") )

      res.pca <- PCA(data_Imput(),  graph = FALSE)


      if(input$pca==1 ) fviz_screeplot(res.pca, addlabels = TRUE, ylim = c(0, 50))
      else if(input$pca==2 ) fviz_pca_biplot(res.pca, repel = FALSE)
      else if(input$pca==3 ) fviz_pca_var(res.pca, col.var="contrib",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE )
      else if(input$pca==4 ) fviz_pca_ind(res.pca, col.ind= "cos2"  ,gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = FALSE )


    })
  })

  # Download BIPLOT
  output$descargar_comp <- downloadHandler(
    filename = function() {
      paste("PCA", input$typefile_comp, sep = ".")
    },
    content = function(file){
      if(!input$compa)
        return()
      isolate({
        validate(
          need(!is.null(input$file2), " Load Data") )
        res.pca <- PCA(data_Imput(),  graph = FALSE)
        if(input$pca==1 )      bi <- fviz_screeplot(res.pca, addlabels = TRUE, ylim = c(0, 50))
        else if(input$pca==2 ) bi <- fviz_pca_biplot(res.pca, repel = FALSE)
        else if(input$pca==3 ) bi <- fviz_pca_var(res.pca, col.var="contrib",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE )
        else if(input$pca==4 ) bi <- fviz_pca_ind(res.pca, col.ind= "cos2"  ,gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = FALSE )
      })

      if(input$typefile_comp=="png") {
        png(file,width = input$png.wid_comp ,height = input$png.hei_comp)
        print(bi)
        dev.off()
      } else {
        pdf(file,width = input$pdf.wid_comp , height = input$pdf.hei_comp )
        print(bi)
        dev.off()
      }
    }
  )



  output$varx2 <- renderUI({
    selectInput("variablex2", "Select the X variable", choices=names(data_comp()[,-1]), selected = NULL)
  })
  output$vary2 <- renderUI({
    selectInput("variabley2", "Select the Y variable", choices=names(data_comp()[,-1]), selected = NULL)
  })

  output$pares <- renderPlot({
    validate(

      need(!is.null(input$file2), " Load Data") )

    if (input$corr==0)
      return()

    isolate({
      dt <- data_comp()
      a <- median( dt[,input$variablex2]  , na.rm = TRUE)
      b <- max( dt[,input$variabley2], na.rm = TRUE)
      gra <- ggplot(dt,aes_string(x=input$variablex2, y=input$variabley2)) +
        geom_point(size=3,alpha=0.5) + theme_bw(base_size = 15) + ggtitle("Comparison")+ stat_smooth(method = lm,formula = y~x)+
        stat_cor(method = "pearson",label.x = a,label.y = b,na.rm = T)
      gra
    })

  })

  #------------------ REPORT  ---------------------------


  output$downloadReport <- downloadHandler(
    filename = function() {
      paste('my-report', sep = '.', switch(
        input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
      ))
    },

    content = function(file) {
      src <- normalizePath('report.Rmd')

      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'report.Rmd', overwrite = TRUE)

      # Set up parameters to pass to Rmd document
      params_ <- list(genotipo = input$genotipo,
                      res_ran= input$res_ran,
                      format=input$format,
                      variable=input$variable,
                      subset=input$subset,
                      varsubset=input$varsubset,
                      levelessub =input$levelessub,
                      column =  input$column,
                      fila  = input$fila)

      library(rmarkdown)
      out <- withProgress(render('report.Rmd',
                                 params =  params_,
                                 switch(
                                   input$format,
                                   PDF = pdf_document(), HTML = html_document(), Word = word_document()
                                 )),
                          message = paste("Generating", input$format , "Report:"), value = 0, detail = "This may take a few minutes...")
      file.rename(out, file)
    }
  )

  #---------------------------------------------

  output$Rsession <-  renderPrint(
    print(sessionInfo())
  )

  #--------------------------------------------------------------------------------------------------------------------
  # GUIA

  observeEvent(input$btn,
               introjs(session,options = list("nextLabel"="Next",
                                               "prevLabel"="Back",
                                               "skipLabel"="Skip")))

}
)



