# server.R

shinyServer(function(input, output, session) {

  ## ==== Define reactive object for tracing the chagnes by users ====

  # ==== __Irrigation calculator ====
  RV_IC <- reactiveValues(
    factor_size = 1.12,
    factor_irr = 0.92,
    factor_wat = 1.05,
    cropWD = NULL,
    planDB = NULL
  )

  
  ## ==== Dynamic UI elements ====

  # ==== __General ====
  output$verInfo_ui <- renderUI({
    helpText(versionInfo[[idx_lang]])
  })

  # ==== __Irrigation Calculator ==== 
  output$IC_results_ui <- renderUI({
    tags$div( 
      DT::dataTableOutput("tabu_IC")
    )
  })

  ## ==== Shiny observers/reactive objects ====

  # ==== __Home page ====
  # ---- ____switcher: tab switcher ----
  observeEvent(input$btn_goTo, {
    if (input$navMod == "mod_IC") {
      updateTabsetPanel(session, "mainNavBar", selected = "navIC")
    }
  })
  
  ## ==== __Irrigation Calculator ====
  ## ---- ____btm: run Irrigation Calculator ----
  observeEvent(input$button_run_IC, {
    withProgress(message = ProgressBar$msg_main[idx_lang], value = 0, {

      # Step 1: Calculate total irrigatin requirement from crop mix (areas and req.)
      incProgress(0.1, detail = ProgressBar$msg_a[idx_lang])

      plantingDate <- input$IC_date
  
      crop_id <- as.double(input$IC_crop)
      soil_id <- as.double(input$soilType)

      climate <- extract_climData("guantao", "R_data/module_IC/meteoDB.rdb")
      loc <- data.frame(
        LAT = attr(climate, "lat"),
        LON = attr(climate, "lon"),
        ALT = attr(climate, "alt")
      )
      climate$RAD <- IC.monthly.radiation(climate, loc)
      climate$ET <- as.numeric(IC.monthly.PenmanFAO(climate, loc))

      soilParam <- read.table("R_data/module_IC/soiltypes.csv",
        header = TRUE,
        row.names = 1,
        sep = ","
      )

      ## Compute soil water balance
      plantingdate <- plantingDate
      cropparam <- cropParam[crop_id, ]
      soilparam <- soilParam[soil_id, ]

      incProgress(0.6, detail = ProgressBar$msg_b[idx_lang])

      # normal year
      results_daily_norm <- IC.monthly.soil_water_balance(cropparam, soilparam, climate, climate$PRCP,
        plantingdate,
        irrigation_threshold = 1,
        irrigation_method = "monthly"
      )

      df.monthly_norm <- data.day2month(results_daily_norm)


      # with 25 percentile of precipitation
      results_daily_wet <- IC.monthly.soil_water_balance(cropparam, soilparam, climate, climate$PRCP_75,
        plantingdate,
        irrigation_threshold = 1,
        irrigation_method = "monthly"
      )

      df.monthly_wet <- data.day2month(results_daily_wet)


      # with 75 percentile of precipitation
      results_daily_dry <- IC.monthly.soil_water_balance(cropparam, soilparam, climate, climate$PRCP_25,
        plantingdate,
        irrigation_threshold = 1,
        irrigation_method = "monthly"
      )

      incProgress(0.3, detail = ProgressBar$msg_c[idx_lang])
      df.monthly_dry <- data.day2month(results_daily_dry)

      RV_IC$cropWD <- data.frame(
        tsteps = seq(1, 12),
        wet_wd = df.monthly_wet$Water_Deficit,
        ave_wd = df.monthly_norm$Water_Deficit,
        dry_wd = df.monthly_dry$Water_Deficit
      )

      RV_IC$planDB <- data.frame(
        "v1" = crop_types[[idx_lang]][as.double(input$IC_crop)], # get crop name
        "v2" = input$IC_date,
        "v3" = round(sum(RV_IC$cropWD[, 2]) * 667 / 1000, 0),
        "v4" = round(sum(RV_IC$cropWD[, 3]) * 667 / 1000, 0),
        "v5" = round(sum(RV_IC$cropWD[, 4]) * 667 / 1000, 0)
      )
    })
  })


  ## ---- ____btm: radio button for water resource ----
  observeEvent(input$waterSrc, {
    idx <- as.integer(input$waterSrc)
    RV_IC$factor_wat <- irrNorm_param[[idx]]$factor_wat

    if (input$waterSrc == 1) {
      irrSizeChoice <- list(
        ">30"  = 1,
        "1~30" = 2,
        "<1"   = 3
      )
    } else {
      irrSizeChoice <- list(
        ">200"    = 1,
        "100~200" = 2,
        "<100"    = 3
      )
    }

    updatePrettyRadioButtons(session, "irrSize", choices = irrSizeChoice, selected = 1)

    if (input$waterSrc == 1) {
      irrMtdChoice <- UI_IC_Tab$BT_str$radioBT_irrMTD1[[idx_lang]]
    } else {
      irrMtdChoice <- UI_IC_Tab$BT_str$radioBT_irrMTD2[[idx_lang]]
    }

    updatePrettyRadioButtons(session, "irrMtd", choices = irrMtdChoice, selected = 1)
   })


  ## ---- ____btm: radio button for irrigation size ----
  observeEvent(input$irrSize, {
    idx_wat <- as.integer(input$waterSrc)
    idx <- as.integer(input$irrSize)

    RV_IC$factor_size <-irrNorm_param[[idx_wat]]$factor_size[[idx]]
  })


  ## ---- ____btm: radio button for irrigation method ----
  observeEvent(input$irrMtd, {
    idx_wat <- as.integer(input$waterSrc)
    idx <- as.integer(input$irrMtd)

    RV_IC$factor_irr <-irrNorm_param[[idx_wat]]$factor_irrMtd[[idx]]
  })

  
  ## ---- ____btm: radio button for changing crop category ----
  observeEvent(input$Id050, {

    if (input$Id050 == UI_IC_Tab$BT_str$radioBT_crop[idx_lang, 1]) {
      myChoice <- UI_IC_Tab$BT_str$selector_cropChoice1[[idx_lang]]
    }

    if (input$Id050 == UI_IC_Tab$BT_str$radioBT_crop[idx_lang, 2]) {
      myChoice <- UI_IC_Tab$BT_str$selector_cropChoice2[[idx_lang]]
    }

    if (input$Id050 == UI_IC_Tab$BT_str$radioBT_crop[idx_lang, 3]) {
      myChoice <- UI_IC_Tab$BT_str$selector_cropChoice3[[idx_lang]]
    }

    updatePickerInput(session,
      "IC_crop",
      choices = myChoice,
      selected = myChoice[[1]]
    )
    
    
    if (input$ICtabset == 'IC02tab'){
      if (input$Id050 == UI_IC_Tab$BT_str$radioBT_crop[idx_lang, 1]) {
        shinyjs::enable("waterSrc")
        shinyjs::enable("irrSize")
        shinyjs::enable("irrMtd")
        shinyjs::disable("IC_ifDrip")
        
      } else if (input$Id050 == UI_IC_Tab$BT_str$radioBT_crop[idx_lang, 3]) {
        
        shinyjs::disable("waterSrc")
        shinyjs::disable("irrSize")
        shinyjs::disable("irrMtd")
        shinyjs::enable("IC_ifDrip")
        
      } else {
        shinyjs::disable("waterSrc")
        shinyjs::disable("irrSize")
        shinyjs::disable("irrMtd")
        shinyjs::disable("IC_ifDrip")
      }
    } 
    
  })

  ## ---- ____btm: crop type switcher ----
  observe({
    crop_id <- as.integer(input$IC_crop)
    
    startdate <- as.character(cropParam$DATERANGESTART[crop_id])
    enddate <- as.character(cropParam$DATERANGEEND[crop_id])

    sltDate <- paste(format(sys_date, "%Y"), substr(startdate, 4, 5), substr(startdate, 1, 2), sep = "-")

    updateDateInput(session, "IC_date",
                    value = sltDate
    )
  })

  ## ==== Plots, Text and Tables ====

  # ==== __Irrigation Calculator =====
  # ---- ____chart: crop water demand ----
  output$cropWD_timeseries <- renderAmCharts({
    monStr <- UI_IC_Tab$plt_str$xlab[, idx_lang]
    colStr <- UI_IC_Tab$plt_str$bar_legend[idx_lang, ]

    if (is.null(RV_IC$cropWD)) {
      selected_tmp <- data.frame(matrix(0, nrow = 12, ncol = 3))
    } else {
      selected_tmp <- RV_IC$cropWD[, c(2, 3, 4)]
    }

    rownames(selected_tmp) <- monStr
    colnames(selected_tmp) <- colStr

    amBarplot(
      y = colStr, data = selected_tmp,
      theme = "dark",
      groups_color = c("#3B6F84", "#FCFCFC", "#A7C951"),
      ylab = UI_IC_Tab$plt_str$ylab[idx_lang],
      labelRotation = 90, precision = 1
    ) %>%
      setLegend(useMarkerColorForLabels = F) %>%
      setProperties(fontFamily = "sans-serif", fontSize = 14) %>%
      setChartCursor()
  })

  # ---- ____table: result table of selected crop ----
  output$tabu_IC <- DT::renderDataTable({
    if (is.null(RV_IC$planDB)) {
      db_tmp <- data.frame(
        "v1" = "NA",
        "v2" = "NA",
        "v3" = "NA",
        "v4" = "NA",
        "v5" = "NA"
      )
    } else {
      db_tmp <- RV_IC$planDB
    }

    datatable(
      db_tmp,
      rownames = F,
      colnames = UI_IC_Tab$table_str$IC_planTab[idx_lang, ],
      options = list(
        paging = F,
        ordering = F,
        info = F,
        searching = F
      ),
      style = "bootstrap"
    )
  })

  # ---- ____table: table of irrigation norm ----
  output$tabu_irrNorm <- DT::renderDataTable({

    if (input$Id050 == UI_IC_Tab$BT_str$radioBT_crop[idx_lang, 1]) {
      db_tmp <- read.table("R_data/module_IC/irr_norm_grain.csv", sep = ",", header = T, fileEncoding = "UTF-8")
      db_tmp[, 2] <- as.factor(db_tmp[, 2])
      cNames <- UI_IC_Tab$table_str$irrNorm1[idx_lang, ]
      db_tmp$base_quota <- round(db_tmp$base_quota * RV_IC$factor_size * RV_IC$factor_irr * RV_IC$factor_wat, 0)
    }

    if (input$Id050 == UI_IC_Tab$BT_str$radioBT_crop[idx_lang, 2]) {
      db_tmp <- read.table("R_data/module_IC/irr_norm_vegs.csv", sep = ",", header = T, fileEncoding = "UTF-8")
      cNames <- UI_IC_Tab$table_str$irrNorm2[idx_lang, ]
    }

    if (input$Id050 == UI_IC_Tab$BT_str$radioBT_crop[idx_lang, 3]) {
      db_tmp <- read.table("R_data/module_IC/irr_norm_fruit.csv", sep = ",", header = T, fileEncoding = "UTF-8")
      db_tmp[, 2] <- as.factor(db_tmp[, 2])
      cNames <- UI_IC_Tab$table_str$irrNorm3[idx_lang, ]

      if (input$IC_ifDrip==T){
        db_tmp$base_quota <- round(db_tmp$base_quota * 0.8)
      }
    }

    idx <- seq(idx_lang, nrow(db_tmp), 2)
    langJsonFile <- ifelse(idx_lang == 1, "www/lang_cn.json", "www/lang_en.json")

    datatable(
      db_tmp[idx, ],
      rownames = F,
      filter = "top",
      options = list(
        language = rjson::fromJSON(file = langJsonFile)
      ),
      colnames = cNames,
      style = "bootstrap"
    )
  })

  ## ==== End Shiny Session =====
  session$onSessionEnded(function() {
    stopApp()
  })
})
