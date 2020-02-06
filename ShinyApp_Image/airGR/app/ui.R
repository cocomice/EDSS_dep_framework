# ui.R

# library(markdown)


navbarPage(
  title = div("airGRteaching",
    img(src = "fig/logo_airGR_square.svg", height = 350 / 9),
    img(src = "fig/logo_irstea_hydro_square.svg", height = 350 / 9),
    img(src = "fig/logo_irstea_square.svg", height = 350 / 9),
    style = "position:relative; top:-9px;"
  ),

  windowTitle = "airGRteaching",

  theme = switch(ShinyGR$theme,
    RStudio = "",
    Cyborg = "css/bootstrap.min_Cyborg.css",
    Cerulean = "css/bootstrap.min_Cerulean.css",
    Flatly = "css/bootstrap.min_Flatly.css",
    United = "css/bootstrap.min_United.css",
    Yeti = "css/bootstrap.min_Yeti.css"
  ),


  tabPanel(
    title = "Interface",
    icon = icon("bar-chart"),
    shinyjs::useShinyjs(), # set up shinyjs


    sidebarLayout(
      position = "left",


      sidebarPanel(
        width = 3,

        h4("Choose a dataset:"),
        fluidRow(
          column(width = 12, selectInput("Dataset", label = NULL, choices = ShinyGR$NamesObsBV))
        ),

        h4("Choose a model:"),
        fluidRow(
          column(width = 6, selectInput("HydroModel",
            label = "Hydrological model",
            choices = c("GR4J", "GR5J", "GR6J")
          )),
          column(width = 6, selectInput("SnowModel",
            label = "Snow model",
            choices = c("None", "CemaNeige")
          ))
        ),


        h4("Parameters values:"),
        conditionalPanel(
          condition = "input.HydroModel == 'GR4J' || input.HydroModel =='GR5J' || input.HydroModel =='GR6J'",
          sliderInput("X1",
            label = "X1  (production store capacity)",
            post = "  [mm]",
            min = 0,
            max = 2500,
            step = 10,
            value = 1250
          ),
          sliderInput("X2",
            label = "X2  (intercatchment exchange coeff.)",
            post = "  [mm/d]",
            min = -5,
            max = +5,
            step = 0.05,
            value = 0
          ),
          sliderInput("X3",
            label = "X3  (routing store capacity)",
            post = "  [mm]",
            min = 0,
            max = 1000,
            step = 5,
            value = 500
          ),
          sliderInput("X4",
            label = "X4  (unit hydrograph time constant)",
            post = "  [d]",
            min = 0.5,
            max = 10,
            step = 0.1,
            value = 5.2
          )
        ),
        conditionalPanel(
          condition = "input.HydroModel == 'GR5J' || input.HydroModel =='GR6J'",
          sliderInput("X5",
            label = "X5  (intercatchment exchange threshold)",
            post = "  [-]",
            min = -4,
            max = +4,
            step = 0.05,
            value = 0
          )
        ),
        conditionalPanel(
          condition = "input.HydroModel == 'GR6J'",
          sliderInput("X6",
            label = "X6  (coeff. for emptying exponential store)",
            post = "  [mm]",
            min = 0,
            max = 20,
            step = 0.5,
            value = 10
          )
        ),
        conditionalPanel(
          condition = "input.SnowModel == 'CemaNeige'",
          sliderInput("C1",
            label = "C1  (weighting coeff. for snow pack thermal state)",
            post = "  [-]",
            min = 0,
            max = 1,
            step = 0.01,
            value = 0.5
          ),
          sliderInput("C2",
            label = "C2 (degree-day melt coefficient)",
            post = "  [mm/°C/d]",
            min = 0,
            max = 10,
            step = 0.5,
            value = 5
          )
        ),

        h4("Automatic calibration:"),
        fluidRow(
          column(width = 6, selectInput("TypeCrit",
            label = "Objective function",
            choices = c(
              "NSE [Q]", "NSE [sqrt(Q)]", "NSE [1/Q]",
              "KGE [Q]", "KGE [sqrt(Q)]", "KGE [1/Q]"
            )
          )),
          column(width = 6, actionButton("CalButton",
            label = "Run", width = "100%",
            icon = icon("refresh"),
            style = ifelse(ShinyGR$theme != "Cerulean",
              "color:#ffffff; background-color:#A4C400; border-color:#A4C400; margin-top:25px; padding:6px;",
              "color:#565656; background-color:#ECF0F1; border-color:#DCDCDC; margin-top:25px; padding:6px;"
            )
          ))
        )
      ),


      mainPanel(
        width = 9,

        fluidRow(
          column(
            width = 2,
            selectInput("PlotType",
              label = "Choose a plot:",
              choices = c("Flow time series", "Model performance", "State variables", "Model diagram")
            )
          ),
          column(
            width = 4, offset = 1,
            sliderInput("Period",
              label = "Select the time window:",
              min = as.POSIXct(ShinyGR$SimPer[[1]][1L], tz = "UTC"),
              max = as.POSIXct(ShinyGR$SimPer[[1]][2L], tz = "UTC"),
              value = as.POSIXct(ShinyGR$SimPer[[1]], tz = "UTC"),
              timeFormat = "%F",
              timezone = "+0000",
              animate = FALSE
            )
          ),
          conditionalPanel(
            condition = "input.PlotType == 'Model diagram' & (input.HydroModel == 'GR4J' || input.HydroModel == 'GR5J' || input.HydroModel == 'GR6J')",
            column(
              width = 4, offset = 0,
              sliderInput("Event",
                label = "Select the target date:",
                min = as.POSIXct(ShinyGR$SimPer[[1]][1L], tz = "UTC"),
                max = as.POSIXct(ShinyGR$SimPer[[1]][2L], tz = "UTC"),
                value = as.POSIXct(ShinyGR$SimPer[[1]][1L], tz = "UTC"),
                timeFormat = "%F",
                timezone = "+0000",
                animate = animationOptions(interval = 500),
                step = 3600 * 24
              )
            )
          )
        ),

        fluidRow(
          conditionalPanel(
            condition = "input.PlotType == 'Model performance'",
            column(
              width = 09,
              plotOutput("stPlotMP", width = "100%", height = "900px")
            )
          ),
          conditionalPanel(
            condition = "input.PlotType == 'Flow time series'",
            column(
              width = 09,
              dygraphs::dygraphOutput("dyPlotTS", width = "100%", height = "400px")
            )
          ),
          conditionalPanel(
            condition = "input.PlotType == 'State variables'",
            column(
              width = 09,
              dygraphs::dygraphOutput("dyPlotSVs", width = "100%", height = "325px"),
              dygraphs::dygraphOutput("dyPlotSVq", width = "100%", height = "355px")
            )
          ),
          conditionalPanel(
            condition = "input.PlotType == 'Model diagram'",
            column(
              width = 05,
              dygraphs::dygraphOutput("dyPlotMDp", width = "100%", height = "190px"),
              dygraphs::dygraphOutput("dyPlotMDe", width = "100%", height = "215px"),
              dygraphs::dygraphOutput("dyPlotMDq", width = "100%", height = "235px")
            ),
            column(
              width = 04,
              plotOutput("stPlotMD", width = "100%", height = "665px")
            )
          ),
          column(
            width = 03,
            div(tableOutput("Criteria")), style = "font-size:90%",
            # conditionalPanel(condition = "input.PlotType == 'Flow time series' || input.PlotType == 'Model diagram'",
            radioButtons("ShowOldQsim",
              label = "Show previous simulation (Qold)",
              choices = c("No", "Yes"), inline = TRUE
            ), # ),
            downloadButton("DownloadTab",
              label = "Download sim. as csv",
              style = "color:#565656; background-color:#ECF0F1; border-color:#DCDCDC; width:170px; height:25px; font-size:95%; padding-top:2px; margin-top:20px;"
            ),
            # conditionalPanel(condition = "input.PlotType == 'Model performance' || input.PlotType == 'Flow time series' || input.PlotType == 'State variables'",
            downloadButton("DownloadPlot",
              label = "Download plot as png",
              style = "color:#565656; background-color:#ECF0F1; border-color:#DCDCDC; width:170px; height:25px; font-size:95%; padding-top:2px; margin-top:10px;"
            )
            # )
          )
        )
      )
    )
  ),
  tabPanel(
    title = "Functionalities", fluidRow(column(6, includeMarkdown("www/tab_fun.md"))),
    icon = icon("cog")
  ),
  tabPanel(
    title = "About", fluidRow(
      column(6, includeMarkdown("www/tab_about.md")),
      column(5, includeMarkdown("www/tab_authors.md"))
    ),
    icon = icon("navicon")
  )
)
