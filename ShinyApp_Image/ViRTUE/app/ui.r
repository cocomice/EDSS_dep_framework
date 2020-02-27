library(shiny)
library(leaflet)
library(ShinyDash)
library(shinyBS)
library(rsconnect)

row <- function(...) {
  tags$div(class="row", ...)
}

col <- function(width, ...) {
  tags$div(class=paste0("span", width), ...)
}

actionLink <- function(inputId, ...) {
  tags$a(href = "javascript:void",
         id = inputId,
         class = "action-button",
         ...)
}

shinyUI(fluidPage(
  # Header
  headerPanel("ViRTUE"),

  # Interactive map
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
  leafletMap("map", "100%", 400,
    initialTileLayer = "http://{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
    initialTileLayerAttribution = HTML('Maps by <a href="http://www.mapbox.com/">Mapbox</a>'),
    options = list(
      center = c(42.68, -72.05), #try to center around Northeast
      zoom = 6,
      maxBounds = list(list(17, -180), list(59, 180))
    )
  ),

  # Container
  tags$div(
    class = "container",
    tags$p(tags$br()),
    row(
      col(3, tags$br())
    ),
    row(
      col(3,
        conditionalPanel(
          condition = "output.markers",
          actionLink("clearMarkers", "Clear markers")
        ),
        h4("Choose Location"),
        checkboxInput("addMarkerOnClick", "Add marker on click", FALSE)
      ),
      col(8,
        htmlWidgetOutput(
          outputId = "desc",
          HTML(paste(""))
        )
      )
    )
  ),

  # Sidebar
  sidebarPanel(
    conditionalPanel(
      condition = "input.conditionedPanels=='wgen'",
  		actionButton(inputId = "alert_anchor","More Information"),
  		bsAlert("alert_anchor1"),
  		bsAlert("alert_anchor2"),

  		br(),

  		downloadButton("downloadData", "Download Plot"),
  		radioButtons("filetype1","Select the file type", choices = list("png", "pdf")),
      HTML(paste('<p style="text-align: center">&copy; 2014 Sarah V. Whateley</p>'))
    ),
    conditionalPanel(
      condition="input.conditionedPanels=='hydro'",

      actionButton(inputId= "info_hydro","More Information"),
  		bsAlert("info_hydro1"),
  		bsAlert("info_hydro2"),
  		bsAlert("info_hydro3"),

  		br(),

  		selectInput("input_type","Hydrologic Data:",c("Upload Historic Flows","Demo")),
      numericInput("basin", "Drainage area of gage (mi^2):", 94),
      uiOutput("ui"),
      downloadButton("downloadHydro", "Download Plot"),
      radioButtons("filetype2", "Select the file type",
                   choices = list("png", "pdf")),
      HTML(paste('<p style="text-align: center">&copy; 2014 Sarah V. Whateley</p>'))
    ),
    conditionalPanel(
      condition="input.conditionedPanels=='Performance'",

      actionButton(inputId= "info_sys","More Information"),
      bsAlert("info_sys1"),
      bsAlert("info_sys2"),
      bsAlert("info_sys3"),
      br(),
      numericInput("cc", "Reservoir Capacity (MG):", 22829),
      numericInput("rel", "Target Reliability:",min=0,max=1,step=0.01, 0.95),
      numericInput("da", "Drainage area of reservoir (mi^2):", 45.5),

    	selectInput("input_type2","Water Supply Demands:",c("Upload Water Supply Demands","Demo")),
    	uiOutput("ui2"),

    	br(),

      sliderInput("temperature","Temperature change from historic (Degrees C), no change=0:",
			min=0,max=5,value=0,step=0.5),
      sliderInput("precipitation","Precipitation change from historic (%), no change=1:",
			min=0.75,max=1.25,value=1,step=0.05),
      sliderInput("demand","Demand change (%), no change=0:",
      min=-1,max=1,value=0,step=.05),
      sliderInput("additional_storage","Additional Storage (%):",
			min=0,max=1,value=0,step=.1),
      sliderInput("storage_month","Storage Month:",
			min=1,max=12,value=4,step=1),
      sliderInput("additional_minimum_flow","Additional Minimum Flow (MGM):",
			min=0,max=300,value=0,step=5),

      checkboxInput("Standard", "Hedge", FALSE),
      bsAlert("info_sys4"),
      downloadButton("downloadSim", "Download Plot"),
  	  radioButtons("filetype3", "Select the file type", choices = list("png", "pdf")),
      HTML(paste('<p style="text-align: center">&copy; 2014 Sarah V. Whateley</p>'))
    ),
    conditionalPanel(
      condition="input.conditionedPanels=='plot_gcms'",

      bsAlert("info_gcm1"),
      downloadButton("downloadGCM", "Download Plot"),
      radioButtons("filetype4","Select the file type",choices=list("png","pdf")),
  	  HTML(paste('<p style="text-align: center">&copy; 2014 Sarah V. Whateley</p>'))
    ),
    conditionalPanel(
      condition="input.conditionedPanels=='summary'",

      bsAlert("info_summary1"),
      sliderInput("rel", "Reliability Threshold:", min=0, max=1, value=0.95, step=0.01),
      checkboxInput("Standard", "Hedge", FALSE),
      downloadButton("downloadSummary", "Download Plot"),
	    radioButtons("filetype5", "Select the file type", choices = list("png", "pdf")),
      HTML(paste('<p style="text-align: center">&copy; 2014 Sarah V. Whateley</p>'))
    ),
    conditionalPanel(
      condition = "input.conditionedPanels=='prob'",

      bsAlert("info_prob1"),
      sliderInput("rel", "Reliability Threshold:", min=0, max=1, value=0.95, step=0.01),
      checkboxInput("Standard", "Hedge", FALSE),
      downloadButton("downloadFinal", "Download Plot"),
      radioButtons("filetype6", "Select the file type", choices = list("png", "pdf")),
      HTML(paste('<p style="text-align: center">&copy; 2014 Sarah V. Whateley</p>'))
    )
  ),

  tags$hr(),

  # Tab Panels
  mainPanel(
    tabsetPanel(
      id="conditionedPanels",

      tabPanel("1. Choose Location", value="wgen",
               tableOutput("markers"),
               plotOutput("test"),
               plotOutput("plot_wgen")
      ),
      tabPanel("2. Generate Streamflow", value="hydro",
               plotOutput("plot_hydrograph")
      ),
      tabPanel("3. Water Supply Performance", value="Performance",
               plotOutput("plot", height = 700, width = 700)
      ),
      tabPanel("4. Climate Change Projections", value="plot_gcms",
               plotOutput("plot_gcms")
      ),
      tabPanel("5. Stress Test Results", value="summary",
               plotOutput("summary", height = 600, width = 700)
      ),
      tabPanel("6. Climate Risk", value = "prob",
               plotOutput("probability", height = 600, width = 700)
      )
    ),
    # Timer
    conditionalPanel("updateBusy() || $('html').hasClass('shiny-busy')",
      id="progressIndicator",
      "PATIENCE IS A VIRTUE",
      div(id="progress", includeHTML("timer.js"))
    ),
    tags$head(
      tags$style(type="text/css",
        '#progressIndicator {',
        '  position: fixed; top: 8px; right: 8px; width: 200px; height: 50px;',
        '  padding: 8px; border: 1px solid #CCC; border-radius: 8px;',
        '}'
      )
    )
  ),

  # Tooltips
  bsTooltip("test", "Click plot for more information.", "top"),
  bsPopover("test", "Historic Climate", "These figures show timeseries of historic precipitation and temperature at the location that you clicked on the map.", trigger="click", placement="right"),

  bsTooltip("plot_hydrograph", "Click plot for more information.", "top"),
  bsPopover("plot_hydrograph", "Model Performance", "The Nash Sutcliffe efficiency value is used to assess the predictive power of the hydrologic model. The value ranges from negative infinity to 1; a value greater than or equal to 0.3 indicates decent performance.", trigger="click", placement="left"),

  bsTooltip("plot", "Click plot for more information.", "top"),
  bsPopover("plot", "Water Supply Metrics", "The reliability metric is a measure of the percentage of time the system operates without failure.", trigger="click", placement="top"),
  bsTooltip("temperature", "Slide the scroll bar to see how an increase in temperature affects performance.", "right"),
  bsTooltip("precipitation", "Slide the scroll bar to see how a change in precipitation affects performance.", "right"),
  bsTooltip("demand", "Slide the scroll bar to see how a change in water supply demands affects performance.", "right"),
  bsTooltip("additional_storage", "Slide the scroll bar to see how performance changes with additional reservoir capacity.", "right"),
  bsTooltip("storage_month", "Look at storages for any month of the year.", "right"),
  bsTooltip("additional_minimum_flow", "Slide the scroll bar to see how performance changes with a minimum flow requirement.", "right"),

  bsTooltip("plot_gcms","Click plot for more information.", "top"),
  bsPopover("plot_gcms", "Climate Projections", "Distributions of precipitation (%) and temperature (Degrees C) changes are based on an ensemble of 39 GCM projections (SRES emission scenario A1B) from the World Climate Research Programme's (WCRP's) Coupled Model Intercomparison Projection Phase 3 (CMIP3) multi-model dataset.", trigger="click", placement="right"),

  bsTooltip("summary","Click plot for more information.", "top"),
  bsPopover("summary", "Stress Test Results", "The blue region illustrates where in climate space the system performs acceptably (i.e. reliability is above target). The red region is where the system performs inadequately. The black line illustrates the reliability target.", trigger="click", placement="right"),

  bsTooltip("probability","Click plot for more information.", "top"),
  bsPopover("probability", "Climate Risk", "The blue bar shows the fraction of climate projections that fall above the target reliability threshold and the red bar shows the fraction of climate projections that fall below the target reliability threshold.", trigger="click", placement="right")
))
