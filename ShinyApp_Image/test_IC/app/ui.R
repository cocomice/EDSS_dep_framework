# ui.R

shinyUI(fluidPage(
  includeCSS("www/my_style.css"),
  useSweetAlert(),
  useShinyjs(),

  navbarPage(
    "EDSS",
    id = "mainNavBar",
    selected = "navHome",

    # define the theme of the platform UI
    theme = shinythemes::shinytheme("darkly"),

    # Home Tab -----------------------------------------------------------
    tabPanel(
      UI_Home_Tab$tab_str$tab_a[[idx_lang]],
      value = "navHome",

      fluidRow(
        h3(UI_Home_Tab$hdr_str$h_a[[idx_lang]]),
        br(),
        column(
          8,
          tags$div(
            class = "home",
            HTML(UI_Home_Tab$intro_str[[idx_lang]])
          )
        ),
        column(
          4,
          br(),
          img(src = "study_area.png", id = "studyArea")
        )
      ),

      h3(UI_Home_Tab$hdr_str$h_b[[idx_lang]]),

      tabsetPanel(
        id = "navMod",
        selected = NULL,

        tabPanel(
          title = UI_Home_Tab$tab_str$tab_b3[idx_lang],
          value = "mod_IC",

          fluidRow(
            style = "background-color:#337ab7;",
            column(
              6,
              tags$div(
                class = "taskItem",
                HTML(UI_Home_Tab$module_str$module_c[[idx_lang]])
              )
            )
          )
        ),
        tabPanel(
          title = UI_Home_Tab$tab_str$tab_b4[idx_lang],
          value = "mod_data",
          
          fluidRow(
            style = "background-color:#337ab7;",
            column(
              6,
              tags$div(
                class = "taskItem",
                HTML(UI_Home_Tab$module_str$module_d[[idx_lang]])
              )
            )
          )
        )
      ),
      br(),
      actionBttn(
        inputId = "btn_goTo",
        label = UI_Home_Tab$bt_str$bt_a[idx_lang],
        icon = icon("link"),
        style = "fill",
        color = "default", size = "sm", block = F, no_outline = F
      )
    ),
    

    # Irrication Calculator Tab ------------------------------------------------
    tabPanel(
      UI_IC_Tab$tab_str$tab_a[idx_lang],
      value = "navIC",

      # ---- __Option Panel ----
      sidebarPanel(
        h3(UI_IC_Tab$hd_str$h1[idx_lang]),
        br(),

        radioGroupButtons(
          inputId = "Id050",
          label = UI_IC_Tab$BT_str$radioBT_label_a[idx_lang],
          individual = T, justified = T, width = "100%", size = "sm",
          choices = UI_IC_Tab$BT_str$radioBT_crop[idx_lang, ],
          status = "primary",
          checkIcon = list(
            yes = icon("ok", lib = "glyphicon"),
            no = icon("remove", lib = "glyphicon")
          )
        ),

        hr(),

        conditionalPanel(
          "input.ICtabset == 'IC01tab'",

          radioGroupButtons(
            inputId = "soilType",
            label = UI_IC_Tab$BT_str$radioBT_label_d[idx_lang],
            individual = T, width = "100%", justified = T, size = "sm",
            choices = UI_IC_Tab$BT_str$radioBT_soil[[idx_lang]],
            selected = 1,
            status = "primary",
            checkIcon = list(
              yes = icon("ok", lib = "glyphicon"),
              no = icon("remove", lib = "glyphicon")
            )
          ),
          
          pickerInput(
            inputId = "IC_crop",
            label = UI_IC_Tab$BT_str$selector_crop_label[[idx_lang]],
            choices = UI_IC_Tab$BT_str$selector_cropChoice1[[idx_lang]],
            selected = crop_id_ini,
            options = list(
              style = "btn-danger"
            )
          ),
          
          dateInput("IC_date",
            label = UI_IC_Tab$BT_str$date_label[idx_lang],
            language = ifelse(idx_lang == 1, "zh-CN", "en"),
            value = paste(format(sys_date, "%Y"), substr(startdate_ini, 4, 5), substr(startdate_ini, 1, 2), sep = "-")
          ),

          hr(),
          actionBttn(
            inputId = "button_run_IC",
            label = UI_IC_Tab$BT_str$runIC_label[idx_lang],
            icon = icon("calculator"),
            style = "fill",
            color = "default", size = "sm", block = F, no_outline = F
          )
        ),

        conditionalPanel(
          "input.ICtabset == 'IC02tab'",

          prettyRadioButtons(
            inputId = "waterSrc",
            label = UI_IC_Tab$BT_str$radioBT_label_b[idx_lang],
            choices = UI_IC_Tab$BT_str$radioBT_water[[idx_lang]],
            selected = 1
          ),
          
          prettyRadioButtons(
            inputId = "irrSize",
            label = UI_IC_Tab$BT_str$radioBT_label_c[idx_lang],
            choices = list(
              ">30"  = 1, 
              "1~30" = 2, 
              "<1"   = 3
            ),
            selected = 1
          ),
          
          prettyRadioButtons(
            inputId = "irrMtd",
            label = UI_IC_Tab$BT_str$radioBT_label_e[idx_lang],
            choices = UI_IC_Tab$BT_str$radioBT_irrMTD1[[idx_lang]],
            selected = 1
          ),
          hr(),
          prettyCheckbox(
            inputId = "IC_ifDrip",
            label =  UI_IC_Tab$BT_str$checkBT_drip[idx_lang], 
            value = F,
            icon = icon("check")
          )
          
        )
      ),

      # ---- __Main Panel ----
      mainPanel(
        tabsetPanel(
          id = "ICtabset",
          tabPanel(UI_IC_Tab$tab_str$tab_c[idx_lang],
            value = "IC01tab",
            tags$div(
              style = "height:100px;",
              uiOutput("IC_results_ui")
            ),
            amChartsOutput("cropWD_timeseries") %>% withSpinner()
          ),

          tabPanel(UI_IC_Tab$tab_str$tab_d[idx_lang],
            value = "IC02tab",

            fluidRow(
              br(),
              DT::dataTableOutput("tabu_irrNorm")
            )
          )
        )
      )
    ),
    
    
    # Data Portal Tab ----------------------------------------------------------
    tabPanel(
      UI_DBPortal_Tab$tab_str$tab_a[idx_lang],
      value = "navData",
      
      dashboardPage(
        dashboardHeader(title = UI_DBPortal_Tab$hd_str$h1[idx_lang], disable = F),
        
        # ---- __Sidebar menu----
        dashboardSidebar(sidebarMenu(
          id = "tabs",
          
          menuItem(
            UI_DBPortal_Tab$menuItem_str$tab_b[idx_lang],
            tabName = "data_archive",
            startExpanded = TRUE,
            icon = icon("folder"),
            menuItem(UI_DBPortal_Tab$menuItem_str$tab_b1[idx_lang], tabName = "db_overview", icon = icon("folder")),
            menuItem(UI_DBPortal_Tab$menuItem_str$tab_b2[idx_lang], tabName = "meteo_data", icon = icon("folder"))
          )
        )),
        
        # ---- __Main panel ----
        dashboardBody(
          fluidRow(
            box(
              background = "black",
              title = NULL, solidHeader = T, collapsible = T, width = 12,
              leafletOutput("map_dashboard")
            )
          ),
          
          tabItems(
            
            tabItem(
              tabName = "db_overview",
              fluidRow(
                box(
                  width = 12,
                  background = "black",
                  h3(UI_DBPortal_Tab$hd_str$h3[idx_lang]),
                  br(),
                  
                  radioGroupButtons(
                    inputId = "db_rb_dataType",
                    label = UI_DBPortal_Tab$BT_str$radioBT_choiceData$label[idx_lang],
                    choices = UI_DBPortal_Tab$BT_str$radioBT_choiceData[[idx_lang]],
                    selected = 1,
                    individual = T
                  ),
                  
                  DT::dataTableOutput("tabu_dataMetaInfo"),
                  hr(),
   
                  column(
                    4,
                    actionBttn("btn_download2",
                               label = UI_DBPortal_Tab$BT_str$btDownload[idx_lang],
                               style = "fill",
                               color = "default", size = "sm", block = F, no_outline = F
                    )
                  )
                )
              )
            ),
            
            tabItem(
              tabName = "meteo_data",
              
              fluidRow(
                box(
                  width = 12,
                  background = "black",
                  amChartsOutput("meteo_ts") %>% withSpinner()
                )
              )
            )
          )
        )
      )
    )
  ),

  hr(),
  fluidRow(
    column(5, helpText(versionInfo[[idx_lang]]))
  )
)) # end of ui file