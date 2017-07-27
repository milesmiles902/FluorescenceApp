library(shiny)
library(shinydashboard)
library(shinythemes)

shinyUI(dashboardPage(
  dashboardHeader(title = "Fluroescence App"),
  
  ##Side Bade Code: menuItem() + conditionalPanel() = New Menu
  dashboardSidebar(
    sidebarMenu(
      id = "mytabs",
        menuSubItem("Input Params", tabName = "inputparams", icon = icon("dashboard")),
        conditionalPanel(
          "input.mytabs == 'inputparams'",
          selectInput(
            'table_row',
            label = "Import Rows",
            selected = "time",
            choices = list(
              "time" = "time",
              "wavelength" = "wavelength"
            )
          ),
          selectInput(
            'table_col',
            label = "Import Columns",
            selected = "wavelength",
            choices = list(
              "time" = "time",
              "wavelength" = "wavelength"
            )
          ),
          selectInput(
            "wave",
            label = "spatial domain",
            selected = "wavelength",
            choices = list("wavenumbers" = "wavenumber",
                           "wavelength" = "wavelength")
          ),
          selectInput(
            "time",
            label = "temporal domain",
            selected = 1,
            choices = list(
              "femtoseconds (fs)" = 1*10^-3,
              "picoseconds (ps)" = 1,
              "nanoseconds (ns)" = 1*10^3,
              "microseconds (us)" = 1*10^6,
              "milliseconds (ms)" = 1*10^9,
              "seconds (s)" = 1*10^12
            )
          )
        ),
      menuItem(
        "Data Input - Anisotropy",
        tabName = "anisotropy",
        icon = icon("dashboard")
      ),
      conditionalPanel(
        "input.mytabs == 'anisotropy'",
        fileInput(
          'file1',
          label = 'upload a parallel file',
          accept = c('text/csv',
                     'text/comma-separated-values,text/plain',
                     '.csv')
        ),
        fileInput(
          'file2',
          label = 'upload a perpendicular file',
          accept = c('text/csv',
                     'text/comma-separated-values,text/plain',
                     '.csv')
        ),
        selectInput(
          'example1',
          label = "example: parallel dataset",
          selected = "none",
          choices = list(
            "none" = "none",
            "parallel" = "parallel"
          )
        ),
        selectInput(
          'example2',
          label = "example: perpendicular dataset",
          selected = "none",
          choices = list(
            "none" = "none",
            "perpendicular" = "perpendicular"
          )
        )),
      menuItem(
        "Analysis",
        tabName = "analysis",
        icon = icon("cloud-download")
      ),
      conditionalPanel(
        "input.mytabs == 'analysis'",
        numericInput("levels", "number of levels", 15, min = 1, max = 100),
        selectInput(
          "proj.style",
          label = "projection",
          selected = "max",
          choices = list("max" = "max",
                         "integrated" = "int")
        ),
        selectInput(
          "type",
          label = "graph type",
          selected = "lines",
          choices = list(
            "lines" = "l",
            "points" = "p",
            "lines and points" = "o"
          )
        )
      ),
      menuItem("Parallel data set", tabName = "parallel", icon = icon("arrows-h")),
      menuItem("Perpendicular data set", tabName = "perpendicular", icon = icon("arrows-v")),
      menuItem("About", tabName = "about", icon = icon("user-circle-o")),
      conditionalPanel("input.mytabs == input.mytabs",
                       selectInput("dataset", "Export a dataset:", 
                                   choices = c("main", "xplot", "yplot")),
          downloadButton('downloadEPS', "Save EPS"),
          tags$style(type='text/css', "#downloadEPS{ vertical-align: middle; height: 35px; width: 100%; font-size: 15px;; color: black})")
    ))
      ),
  
  ##Dashboard Body: tabItem corresponds to a specific menuItem
  dashboardBody(tabItems(
    tabItem(
      "anisotropy",
      box( width = "100%", fluidPage(fluidRow((includeMarkdown("markdown/anisotropy.md"))))),
      box(
        title = "Broadband Fluorescence Anisotropy",
        status = "warning",
        solidHeader = TRUE,
        width = "100%",
        height = "100%",
        fluidPage(
          fluidRow(column(7,
                          plotOutput("anisotropy",
                                     dblclick = "anisotropy_dblclick",
                                     brush = brushOpts(
                                       id = "anisotropy_brush",
                                       resetOnNew = TRUE
                                     )),
                          plotOutput("anisotropy.x")),
                   column(width = 5,
                          plotOutput("anisotropy.y")
                   )
                   ))
      )
    ),
    tabItem(
      "parallel",
      box( width = "100%", fluidPage(fluidRow((includeMarkdown("markdown/anisotropy.md"))))),
      box(
        title = "Broadband Fluorescence Anisotropy",
        status = "warning",
        solidHeader = TRUE,
        width = "100%",
        height = "100%",
        fluidPage(
          fluidRow(column(7,
                          plotOutput("parallel",
                                     dblclick = "plot1_dblclick",
                                     brush = brushOpts(
                                       id = "parallel_brush",
                                       resetOnNew = TRUE
                                     )),
                          plotOutput("parallel.x")),
                   column(width = 5,
                          plotOutput("parallel.y")
                   )
          ))
      )
    ),
    tabItem(
      "perpendicular",
      box( width = "100%", fluidPage(fluidRow((includeMarkdown("markdown/anisotropy.md"))))),
      box(
        title = "Broadband Fluorescence Anisotropy",
        status = "warning",
        solidHeader = TRUE,
        width = "100%",
        height = "100%",
        fluidPage(
          fluidRow(column(7,
                          plotOutput("perpendicular",
                                     dblclick = "perpendicular_dblclick",
                                     brush = brushOpts(
                                       id = "perpendicular_brush",
                                       resetOnNew = TRUE
                                     )),
                          plotOutput("perpendicular.x")),
                   column(width = 5,
                          plotOutput("perpendicular.y")
                   )
          ))
      )
    ),
    tabItem("about",
            includeMarkdown("markdown/about.md"))
  ))
)
)
