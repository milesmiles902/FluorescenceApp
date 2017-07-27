library(shiny)
library(RColorBrewer)
library(shinyFiles)

source("globals.R")

options(shiny.maxRequestSize = 100 * 1024 ^ 2)
col_func <- colorRampPalette(c('#1f77b4','#ff7f0e','#2ca02c','#d62728', '#9467bd', '#8c564b'))

shinyServer(function(input, output, session) {
  
  #Process data for all menus
  parallel.data <- reactive({
    validate(need((input$example1 != "none") ||
                    (is.null(input$file1) == FALSE), ""))
    
    if (is.null(input$file1) == FALSE) {
      if(input$table_row == "wave"){
          df <- load(file = input$file1$datapath, x = "wave")
      } else{
          df <- load(file = input$file1$datapath)
      }
    } else if (input$example1 == "parallel") {
        df <- load(file = "data/parallel.csv")
    } else {
      return(NULL)
    }
    
    if(input$wave == "wavenumber"){
      df@label$wavelength <- input$wave
    } 
    
    df@data$x <- df@data$x * as.numeric(input$time)
    return(df)
  })
  
  perpendicular.data <- reactive({
    validate(need((input$example2 != "none") ||
                    (is.null(input$file2) == FALSE), ""))

    if (is.null(input$file2) == FALSE) {
      if(input$table_row == "time"){
        df <- load(file = input$file2$datapath, x = "time")
      } else{
        df <- load(file = input$file2$datapath)
      }
    } else if (input$example2 == "perpendicular") {
      df <- load(file = "data/perpendicular.csv")
    } else {
      return(NULL)
    }
    
    if(input$wave == "wavenumber"){
      df@label$wavelength <- input$wave
    } 
    
    df@data$x <- df@data$x * as.numeric(input$time)
    return(df)
  })
  
  anisotropy.data <- reactive({
    par <- parallel.data()
    perp <- perpendicular.data()
    if (length(par@wavelength) == length(par@wavelength))  {
      df <- par
      df@data$spc <-
        (par@data$spc[,]-perp@data$spc[,])/(par@data$spc[,]+2*perp@data$spc[,])
    }
    return(df)
  })
  
  output$parallel <- renderPlot({
    xy.plot(xy.brush(parallel.data(), "parallel"))
  })
  
  output$parallel.x <- renderPlot({
    x.plot(x.brush(x.calc(parallel.data()), "parallel"))
  })
  
  output$parallel.y <- renderPlot({
    y.plot(y.brush(y.calc(parallel.data()), "parallel"))
  })
  
  output$perpendicular <- renderPlot({
    xy.plot(xy.brush(perpendicular.data(), "perpendicular"))
  })
  
  output$perpendicular.x <- renderPlot({
    x.plot(x.brush(x.calc(perpendicular.data()), "perpendicular"))
  })
  
  output$perpendicular.y <- renderPlot({
    y.plot(y.brush(y.calc(perpendicular.data()), "perpendicular"))
  })
  
  #Output anisotropy main plot
  output$anisotropy <- renderPlot({
    xy.plot(xy.brush(anisotropy.data(), "anisotropy"))
  })

  #Output anisotropy x plot
  output$anisotropy.x <- renderPlot({
    x.plot(x.brush(x.calc(anisotropy.data()), "anisotropy"))
  })
  
  #Output anisotropy y plot
  output$anisotropy.y <- renderPlot({
    y.plot(y.brush(y.calc(anisotropy.data()), "anisotropy"))
  })
  
  x.calc <- function(df){
    if (input$proj.style == "max") {
      df <- apply(df, 2, max)
      df@label$spc <- "Max Intensity"
    } else if (input$proj.style == "int") {
      df <- colSums(df, label.spc = "Integrated Intensity")
    } else {
      return(NULL)
    }
    return (df)
  }
  
  y.calc <- function(df){
    if (input$proj.style == "max") {
      df <- apply(df, 1, max)
      df@label$wavelength = "Max Intensity"
    } else if (input$proj.style == "int") {
      df <- rowSums(df, label.wavelength = "Integrated Intensity")
    }
    return (df)
  }
  
  xy.plot = function(df){
    par(mar = c(4.5, 5, 1.5, 7.5))
    filled.contour(
      x = df@wavelength,
      y = df@data$x,
      z = t(df@data$spc),
      plot.axes = {
        contour(
          x = df@wavelength,
          y = df@data$x,
          z = t(df@data$spc),
          nlevels = levels$numlevels,
          drawlabels = FALSE,
          axes = FALSE,
          frame.plot = FALSE,
          add = TRUE
        )
        
        axis(1)
        axis(2)
      },
      plot.title = {
        title(xlab = df@label$wavelength,
              ylab = df@label$x)
      },
      color = col_func
    )
  }
  
  x.plot = function(df){
    par(mar=c(4.0,3.7,1,7.5))
    plot(x = df@wavelength, y = df@data$spc, type = type$graph, xlab = df@label$wavelength, ylab = df@label$spc)
  }
  
  y.plot = function(df){
    par(mar=c(3.8,5,0.5,7.5))
    plot(df@data$spc, df@data$x, type = type$graph, xlab = df@label$wavelength, ylab = "")
  }
  
  xy.brush = function(df, name){
    if (!is.null(eval(parse(text=paste(name, c("_range$x"), sep=""))))) {
      df = brush(df, min(eval(parse(text=paste(name, c("_range$x"), sep="")))), max(eval(parse(text=paste(name, c("_range$x"), sep="")))), TRUE)
      df = brush(df, min(eval(parse(text=paste(name, c("_range$y"), sep="")))), max(eval(parse(text=paste(name, c("_range$y"), sep="")))), FALSE)
    }
    return(df)
  }
  
  x.brush = function(df, name){
    if (!is.null(eval(parse(text=paste(name, c("_range$x"), sep=""))))) {
      df = brush(df, min(eval(parse(text=paste(name, c("_range$x"), sep="")))), max(eval(parse(text=paste(name, c("_range$x"), sep="")))), TRUE)
    }
    return(df)
  }
  
  y.brush = function(df, name){
    if (!is.null(eval(parse(text=paste(name, c("_range$y"), sep=""))))) {
      df = brush(df, min(eval(parse(text=paste(name, c("_range$y"), sep="")))), max(eval(parse(text=paste(name, c("_range$y"), sep="")))), FALSE)
    }
    return(df)
  }
  
  #Brush for anisotropy tab main plot
  anisotropy_range <- reactiveValues(x = NULL, y = NULL)
  parallel_range <- reactiveValues(x = NULL, y = NULL)
  perpendicular_range <- reactiveValues(x = NULL, y = NULL)
  
  observeEvent(input$anisotropy_dblclick, {
    brush <- input$anisotropy_brush
    if (!is.null(brush)) {
      anisotropy_range$x <- c(brush$xmin, brush$xmax)
      anisotropy_range$y <- c(brush$ymin, brush$ymax)
    } else {
      anisotropy_range$x <- NULL
      anisotropy_range$y <- NULL
    }
  })
  observeEvent(input$perpendicular_dblclick, {
    brush <- input$perpendicular_brush
    if (!is.null(brush)) {
      perpendicular_range$x <- c(brush$xmin, brush$xmax)
      perpendicular_range$y <- c(brush$ymin, brush$ymax)
    } else {
      perpendicular_range$x <- NULL
      perpendicular_range$y <- NULL
    }
  })
  observeEvent(input$parallel_dblclick, {
    brush <- input$parallel_brush
    if (!is.null(brush)) {
      parallel_range$x <- c(brush$xmin, brush$xmax)
      parallel_range$y <- c(brush$ymin, brush$ymax)
    } else {
      parallel_range$x <- NULL
      parallel_range$y <- NULL
    }
  })
  
  levels <- reactiveValues(numlevels = NULL)
  observeEvent(input$levels, {
    if (!is.null(input$levels)) {
      levels$numlevels <- input$levels
    }else {
      levels$numlevels = 15
    }
  })
  
  type <- reactiveValues(graph = NULL)
  observeEvent(input$type, {
    if (!is.null(input$type)) {
      type$graph <- input$type
    }else {
      type$graph = "l"
    }
  })
  
  output$downloadEPS <- downloadHandler(
      filename = function(){
        paste(input$dataset, '.eps', sep='')
      }, content = function(file) {
          if(input$dataset == "main"){
            setEPS()
            postscript(file)
            print(xy.plot(xy.brush(eval(parse(text=paste(input$mytabs, c(".data()"), sep=""))), input$mytabs)))
            dev.off()
          } else if(input$dataset == "xplot"){
            setEPS()
            postscript(file)
            print(x.plot(x.brush(x.calc(eval(parse(text=paste(input$mytabs, c(".data()"), sep="")))), input$mytabs)))
            dev.off()
          } else if(input$dataset == "yplot"){
            setEPS()
            postscript(file)
            print(y.plot(y.brush(y.calc(eval(parse(text=paste(input$mytabs, c(".data()"), sep="")))), input$mytabs)))
            dev.off()
          } else{
            return(NULL)
          }
        })
  
})