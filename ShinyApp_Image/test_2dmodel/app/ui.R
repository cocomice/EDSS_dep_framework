# ui.R

ui <- function(req) {
  basicPage(
    includeCSS("www/my_style.css"),

    ## ===== Language Switcher =====
    fluidRow(
      useShinyjs(),

      column(2,
        align = "center",
        br(), br(),
        radioButtons("lang_switch",
          label = NULL,
          choices = list("中文" = 1, "English" = 2),
          selected = 2, inline = T
        )
      ),

      column(10, uiOutput("mainHdr_ui"))
    ),

    sidebarLayout(
      position = c("left", "right"), fluid = T,

      ## ===== Panel: Simulation configuration =====
      sidebarPanel(
        width = 4,

        fluidRow(
          column(6, uiOutput("resetBT_ui")),
          column(6, uiOutput("runBT_ui"))
        ),

        hr(class = "innerBar"),

        # ==== __Irrigation Setting ====
        fluidRow(
          uiOutput("IrrSelct_ui"),
          uiOutput("irrSldr_ui")
        ),

        hr(class = "innerBar"),

        fluidRow(
          uiOutput("bdrySldr_ui")
        )
      ),

      ## ===== Panel: Visualization of results =====

      mainPanel(
        id = "main_panel", width = 8,

        # ==== __Leaflet Map ====
        fluidRow(
          column(
            12,
            leafletOutput("map"),
            absolutePanel(
              id = "leaflet-absPanel", fixed = F, draggable = F,
              top = 15, left = "auto", right = 150, bottom = "auto",
              width = 150, height = "auto",

              div(class = "my_div", uiOutput("map_switch_ui"))
            ),
            br()
          )
        ),

        # ==== __Tab: visualization of groundwater drawdown ====
        tabsetPanel(
          tabPanel(
            uiOutput("tab_a_ui"),

            fluidRow(
              column(12,
                align = "center",
                amChartsOutput("plot2"),
                br()
              )
            )
          ),

          # ==== __Tab: visualization of irrigation consumption ====

          tabPanel(
            uiOutput("tab_b_ui"),

            fluidRow(
              column(
                12,
                actionButton("disp_tab", label = uiOutput("modBT2_ui")),
                amChartsOutput(outputId = "plot3"),

                bsModal("modl_tab1",
                  uiOutput("dispHd_mod1_ui"),
                  "disp_tab",
                  size = "large", formattableOutput("table_statis")
                )
              )
            )
          )
        )
      )
    ), # end of sidebar layout

    hr(),
    uiOutput("bottomPanel_ui")
  )
} # end of "ui.R"
