# server.R

server <- function(input, output, session) {

  ## ==== Define reactive object for tracing the chagnes by users ====
  RV <- reactiveValues(
    z = numeric(length = default_nrYrs + 1), # hold drawdown time-series per selected point
    BS = data.BS, # hold baseline dataset
    handles = data.handles, # hold user specified inputs
    RES = NULL, # hold results
    int_loc = NULL, # hold location of selected drawdown point
    idx_loc = NULL, # hold the index of location w.r.t model domain
    idx_lang = 2, # language switcher
    mapData_tr = NULL, # hold transient results for rendering in leaflet
    mapData_ss = NULL # hold steady-state results for rendering in leaflet
  )

  ## ===== Dynamic UI elements =====

  # ==== __Web title ====
  output$mainHdr_ui <- renderUI({
    h2(UI_char_dict$titleWeb[RV$idx_lang], align = "center")
  })

  # ==== __Reset bottom ====
  output$resetBT_ui <- renderUI({
    actionButton("button_reset",
      width = "100px",
      label = UI_char_dict$BT_str$reset[RV$idx_lang],
      style = "color: #fff; background-color: #b51212; border-color: #750303"
    )
  })

  # ==== __Map switcher ====
  output$map_switch_ui <- renderUI({
    tagList(
      
      radioButtons("map_plot_switch",
                   label = NULL,
                   choices = UI_char_dict$BT_str$map_switch[[RV$idx_lang]],
                   selected = "tr", inline = F
      ),
      
      selectInput(
        "yr_slt_2dModel",
        UI_char_dict$BT_str$yr_seltor[RV$idx_lang],
        choices = seq(1, 10, 1),
        selected = 10,
        width = '100px'
      )
    )
  })
  
  
  # ==== __Update bottom ====
  output$runBT_ui <- renderUI({
    actionButton("button_run",
      width = "100px",
      label = UI_char_dict$BT_str$update[RV$idx_lang], class = "rightAlign",
      style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
    )
  })

  # ==== __Switcher for scenario specification method =====
  output$IrrSelct_ui <- renderUI({
    column(
      12,
      h4(strong(UI_char_dict$hd_str$dist[RV$idx_lang])),

      radioGroupButtons("input_switch",
        label = NULL,
        choices = UI_char_dict$BT_str$input_switch[[RV$idx_lang]],
        selected = 1, justified = T,
        checkIcon = list(yes = icon("ok", lib = "glyphicon"))
      ),

      conditionalPanel(
        condition = "input.input_switch == 1",

        tags$div(class = "notification",

          Server_char_dict$testInfo$note_info1[RV$idx_lang],
          tags$ol(
            tags$li(Server_char_dict$testInfo$note_info2[RV$idx_lang,1]),
            tags$li(Server_char_dict$testInfo$note_info2[RV$idx_lang,2])
          )
        ),

        # ==== ____Selector for irrigation district =====
        selectInput("select_district",
          label = UI_char_dict$selecter_dist[RV$idx_lang],
          choices = dist_choice[[RV$idx_lang]],
          selected = 8
        )
      ),

      conditionalPanel(
        condition = "input.input_switch == 2",

        tags$div(class="notification",

          Server_char_dict$testInfo$note_info1[RV$idx_lang],
          tags$ol(
            tags$li(Server_char_dict$testInfo$note_info3[RV$idx_lang,1]),
            tags$li(Server_char_dict$testInfo$note_info3[RV$idx_lang,2]),
            tags$li(Server_char_dict$testInfo$note_info3[RV$idx_lang,3])
          )
        ),

        # ==== ____Option for users' upload files ====
        fluidRow(
            style = "display:inline-block",
            column(
              8,

              fileInput("file1",
                label = NULL,
                accept = c(".xls"),
                buttonLabel = UI_char_dict$BT_str$upload$bt_label[RV$idx_lang],
                placeholder = UI_char_dict$BT_str$upload$bt_holder[RV$idx_lang]
              )
            ),

            column(4, downloadButton("downloadBT",
              UI_char_dict$BT_str$download[RV$idx_lang],
              class = "rightAlign"
            ))
          )
      )
    )
  })

  # ==== __Slider sets for irrigation setting ====
  output$irrSldr_ui <- renderUI({
    column(
      12,
      # ==== ____Slider for changing the irrigation area ====
      sliderInput("slider_area",
        label = HTML(UI_char_dict$slider_str$area_label[RV$idx_lang]),
        min = 0,
        max = 50,
        step = 0.1,
        value = 12.4
      ),

      # ==== ____Slider for changing irrigation water requirement ====
      sliderInput("slider_watReq",
        label = HTML(UI_char_dict$slider_str$area_watUse[RV$idx_lang]),
        min = 100,
        max = 1200,
        step = 1,
        value = 414
      ),

      # ==== ____Slider for changing surface irrigation percentage ====
      sliderInput("slider_SW_perc",
        label = HTML(UI_char_dict$slider_str$area_SWprec[RV$idx_lang]),
        min = 0,
        max = 100,
        step = 1,
        value = 18.0
      )
    )
  })

  # ==== __Slider sets for changing driving force ====
  output$bdrySldr_ui <- renderUI({
    column(
      12,
      h4(strong(UI_char_dict$hd_str$boundary[RV$idx_lang])),

      # ==== ____Slider for changing stream flow rate ====

      sliderInput("slider_riv",
        label = HTML(UI_char_dict$slider_str$riv_label[RV$idx_lang]),
        min = 10,
        max = 30,
        step = 0.1,
        value = 15.8
      ),

      # ==== ____Slider for changing boundary recharge ====

      sliderInput("slider_bdry",
        label = HTML(UI_char_dict$slider_str$bdry_label[RV$idx_lang]),
        min = 0.5,
        max = 5,
        step = 0.1,
        value = 1.5
      )
    )
  })

  # ==== __Modal for display table of water use ====
  output$dispHd_mod1_ui <- renderUI({
    h4(HTML(UI_char_dict$modal_lab$disp_mod1[RV$idx_lang]), align = "center")
  })
  
  output$modBT2_ui <- renderUI({
    UI_char_dict$BT_str$disp[RV$idx_lang]
  })
  
  # ==== __Tabs ====
  output$tab_a_ui <- renderUI({
    UI_char_dict$tab_str$tab_a[RV$idx_lang]
  })

  output$tab_b_ui <- renderUI({
    UI_char_dict$tab_str$tab_b[RV$idx_lang]
  })

  output$waterCmpBT_ui <- renderUI({
    UI_char_dict$BT_str$watCmp[RV$idx_lang]
  })

  # ==== __Version Info ====
  output$bottomPanel_ui <- renderUI({
    tagList(
      fluidRow(
        column(5, helpText(versionInfo[[RV$idx_lang]]))
      )
    )
  })

  ## ==== Shiny observers/reactive objects ====

  # ==== __switcher: language switch ====
  observeEvent(input$lang_switch, {
    if (input$lang_switch == 1) {
      RV$idx_lang <- 1
    } else {
      RV$idx_lang <- 2
    }

    RV$z <- numeric(length = data.BS$t_steps_default)
    RV$int_loc <- NULL
  })

  # ==== __switcher: configuration methods to enable corresonding options ====

  observeEvent(input$input_switch, {
    if (input$input_switch == 2) {
      shinyjs::disable("slider_area")
      shinyjs::disable("slider_watReq")
      shinyjs::disable("slider_SW_perc")
    } else {
      shinyjs::enable("slider_area")
      shinyjs::enable("slider_watReq")
      shinyjs::enable("slider_SW_perc")
    }
  })

  # ==== __slider: irrigation area ====

  observeEvent(input$slider_area, {
    curr_dist <- as.double(RV$handles$f_n_dist)
    RV$handles$f_area[curr_dist] <- input$slider_area * 1e4
  })

  # ==== __slider: water requirement ====
  observeEvent(input$slider_watReq, {
    curr_dist <- as.double(RV$handles$f_n_dist)
    RV$handles$f_irr_req[curr_dist] <- input$slider_watReq
  })

  # ==== __slider: surface water use ratio ====
  observeEvent(input$slider_SW_perc, {
    curr_dist <- as.double(RV$handles$f_n_dist)
    RV$handles$f_SW_perc[curr_dist] <- input$slider_SW_perc * 0.01
  })

  # ==== __slider: boundary flux ====
  observeEvent(input$slider_bdry, {
    RV$handles$f_bdry_mod <- input$slider_bdry * 1e8
  })

  # ==== __slider: river inflow ====
  observeEvent(input$slider_riv, {
    RV$handles$f_riv_mod <- input$slider_riv * 1e8
  })

  # ==== __btm: file upload ====

  observeEvent(input$file1, {
    inFile <- input$file1

    if (!is.null(inFile)) {
      userData <- fun_preprocess(inFile$datapath)

      for (i in 1:data.BS$n_dists) {

        # Update data handles
        RV$handles$f_SW_perc[i] <- userData$water_use_perc.SW[i]
        RV$handles$f_area[i] <- userData$irr_area[i]

      }

      updateSliderInput(session, "slider_riv",
        value = (1e-8) * RV$handles$f_riv_mod
      )

      updateSliderInput(session, "slider_bdry",
        value = (1e-8) * (RV$handles$f_bdry_mod)
      )

      sel_dist <- as.integer(input$select_district)

      updateSelectInput(session, "select_district",
        selected = sel_dist
      )

      # Update slider
      updateSliderInput(session, "slider_SW_perc",
                        value = 100 * RV$handles$f_SW_perc[sel_dist]
      )

      updateSliderInput(session, "slider_area",
                        min = 0,
                        max = 50,
                        step = 0.1,
                        value = RV$handles$f_area[sel_dist] / 1e4
      )
    }
  })

  # ==== __btm: update button to trigger Modflow execution ====

  observeEvent(input$button_run, {
    withProgress(message = Server_char_dict$progressBar$msg_main[RV$idx_lang], value = 0, {

      # Step 1: Calculate total irrigatin requirement from crop mix (areas and req.)
      incProgress(0.1, detail = Server_char_dict$progressBar$msg_a[RV$idx_lang])
      RV$RES <<- fun_calc_recharge(RV$BS, RV$handles)

      # Step 2: Call Modflow 2000 executable
      incProgress(0.1, detail = Server_char_dict$progressBar$msg_b[RV$idx_lang])
      fun_mfl_runner(RV$BS, RV$RES, RV$handles) # Run model

      # Step 3: Read final outputs
      incProgress(0.3, detail = Server_char_dict$progressBar$msg_c[RV$idx_lang])

      RV$RES <<- fun_RES_eval(RV$BS, RV$RES, RV$handles)

      # Step 4: Plot the spatial drawdown on the map
      incProgress(0.3, detail = Server_char_dict$progressBar$msg_d[RV$idx_lang])

      cc <- c("#a50027", "#f56c42", "#ffbea6", "#ffffff", "#6db7c4", "#4b5ea8")
      a <- seq(-14, 10, 4)
      tmp <- c(1:6)
      b <- matrix(c(a[1:6], a[2:7], tmp), ncol = 3)

      b[1, 1] <- -Inf
      b[6, 2] <- Inf

      pal <- colorNumeric(
        palette = cc,
        domain = tmp
      )

      rr_tr <- RV$RES$mat_dlevel
      rr_ss <- RV$RES$mat_dlevel_ss

      rr_tr <- raster(apply(rr_tr, 1, rev),
        xmn = 99.17817, xmx = 100.9907, ymn = 38.65716, ymx = 39.79317
      )
      rc_tr <- reclassify(rr_tr, b)

      rr_ss <- raster(apply(rr_ss, 1, rev),
        xmn = 99.17817, xmx = 100.9907, ymn = 38.65716, ymx = 39.79317
      )
      rc_ss <- reclassify(rr_ss, b)

      RV$mapData_tr <- rasterToPolygons(rc_tr, dissolve = T)
      RV$mapData_ss <- rasterToPolygons(rc_ss, dissolve = T)

      if (input$map_plot_switch == "tr") {
        pols <- RV$mapData_tr
      } else {
        pols <- RV$mapData_ss
      }

      RV$int_loc <- NULL

      leafletProxy("map") %>%
        clearGroup("dataMap") %>%
        clearGroup("selPts") %>%
        clearGroup("selectedFtr") %>%
        addPolygons(
          data = pols, fillColor = ~ pal(pols@data[[1]]),
          fillOpacity = 0.5, stroke = FALSE, group = "dataMap"
        )

      incProgress(0.3, detail = Server_char_dict$progressBar$msg_e[RV$idx_lang])
    })
  })

  # ==== __btm: reset button ====

  observeEvent(input$button_reset, {
    RV$handles <<- fun_reset_handles(RV$BS, default_dist)

    updateSliderInput(session, "slider_watReq",
      value = RV$handles$f_irr_req[RV$handles$f_n_dist]
    )
    
    updateSliderInput(session, "slider_SW_perc",
      value = 100 * RV$handles$f_SW_perc[RV$handles$f_n_dist]
    )

    updateSliderInput(session, "slider_area",
      min = 0,
      max = 50,
      value = RV$handles$f_area[RV$handles$f_n_dist] / 1e4
    )

    updateSliderInput(session, "slider_riv",
      value = (1e-8) * RV$handles$f_riv_mod
    )

    updateSliderInput(session, "slider_bdry",
      value = (1e-8) * (RV$handles$f_bdry_mod)
    )

    updateSelectInput(session, "select_district",
      selected = 8 # default_dist
    )

    updateSelectInput(session, "yr_slt_2dModel", selected = 10)

    updateRadioButtons(session, "map_plot_switch", selected = "tr")

    RV$int_loc <- NULL
    RV$mapData_ss <- NULL
    RV$mapData_tr <- NULL
    RV$RES$mat_dlevel_array <- NULL

    leafletProxy("map") %>%
      clearGroup("dataMap") %>%
      clearGroup("selPts")

  })

  # ==== __switcher: change year to display drawdown map ====
  observeEvent(input$yr_slt_2dModel, {

    if (!is.null(RV$RES$mat_dlevel_array)){
      withProgress(message = Server_char_dict$progressBar$msg_main[RV$idx_lang], value = 0, {

        incProgress(0.1, detail = Server_char_dict$progressBar$msg_d[RV$idx_lang])

        yr_selected <- as.integer( input$yr_slt_2dModel )
        mat_dlevel <- RV$RES$mat_dlevel_array[,,yr_selected]

        rr <- round(mat_dlevel, 4)

        cc <- c("#a50027", "#f56c42", "#ffbea6", "#ffffff", "#6db7c4", "#4b5ea8")
        a <- seq(-14, 10, 4)
        tmp <- c(1:6)
        b <- matrix(c(a[1:6], a[2:7], tmp), ncol = 3)

        b[1, 1] <- -Inf
        b[6, 2] <- Inf

        pal <- colorNumeric(
          palette = cc,
          domain = tmp
        )

        rr <- raster(apply(rr, 1, rev),
                     xmn = 99.17817, xmx = 100.9907, ymn = 38.65716, ymx = 39.79317
        )

        rc <- reclassify(rr, b)
        pols <- rasterToPolygons(rc, dissolve = T)

        # Leaflet output map
        leafletProxy("map") %>%
          clearGroup("dataMap") %>%
          clearGroup("selectedFtr") %>%
          addPolygons(
            data = pols, fillColor = ~ pal(pols@data[[1]]),
            fillOpacity = 0.5, stroke = FALSE, group = "dataMap"
          )
      })
    }
  })

  # ==== __selector: district selector ====
  observeEvent(input$select_district, {
    sel_dist <- as.double(input$select_district)

    RV$handles$f_n_dist <<- as.double(sel_dist)

    updateSliderInput(session, "slider_SW_perc",
      min = 0,
      max = 100,
      step = 1,
      value = 100 * RV$handles$f_SW_perc[sel_dist]
    )

    updateSliderInput(session, "slider_area",
      min  = 0,
      max  = 50,
      step = 0.1,
      value = RV$handles$f_area[sel_dist] / 1e4
    )

    updateSliderInput(session, "slider_watReq",
      min = 0,
      max = 1600,
      step = 1,
      value = RV$handles$f_irr_req[sel_dist]
    )

    # Highlight corresponding feature on the map
    idxDist <- which(RV$BS$shape_irr_area_pol$Id == sel_dist)

    leafletProxy("map") %>%
      clearGroup("selectedFtr") %>%
      addPolygons(
        data = RV$BS$shape_irr_area_pol[idxDist, 1],
        color = "#ffffff", fillColor = "#69a855", fillOpacity = 0.4,
        weight = 2, group = "selectedFtr"
      )
  })

  # ==== __click: mouse clicks on the map ====
  observeEvent(input$map_shape_click, {
    label_cn <- UI_char_dict$dist_name[RV$idx_lang, ]

    # Get long/lat value from the map click
    click <- input$map_shape_click
    clat  <- click$lat
    clng  <- click$lng

    # Convert coordinates into desired CRS system
    coordVal <- data.frame(lon = clng, lat = clat)

    coordinates(coordVal) <- c("lon", "lat")
    proj4string(coordVal) <- CRS("+init=epsg:4326")

    CRS.new <- CRS("+init=epsg:21417") # projection system: beijing 1954 gauss-kruger zone 34
    dd <- spTransform(coordVal, CRS.new)

    RV$int_loc <- matrix(c(dd@coords[1] - 17e6, dd@coords[2]), nrow = 1)

    # Choose the left-side closest point in the matrix
    idx_x <- max(which(floor(RV$BS$x - RV$int_loc[1]) >= 0)[1] - 1, 1)
    idx_y <- max(which(floor(RV$BS$y - RV$int_loc[2]) >= 0)[1] - 1, 1)

    RV$idx_loc <- c(idx_x, idx_y)

    if (is.null(RV$RES)) {
      RV$z <- numeric(length = data.BS$t_steps_default)
    } else {
      # Extract drawdown series for selected point
      RV$z <- RV$RES$mat_dlevel_array[idx_x, idx_y, ]
    }

    dist_selected <- RV$BS$mat_fields[idx_x, idx_y]
    dis_str <- c("N/A", label_cn)

    if (length(RV$RES$mat_dlevel) == 0) {
      labelTxt <- HTML(paste(
        sep = " ",
        dis_str[dist_selected + 1], "<br>",
        Server_char_dict$map$LblTxt[RV$idx_lang, 1], ": N/A", "m<br>",
        Server_char_dict$map$LblTxt[RV$idx_lang, 2], ": N/A", "m"
      ))
    } else {
      labelTxt <- HTML(paste(
        sep = " ",
        dis_str[dist_selected + 1], "<br>",
        Server_char_dict$map$LblTxt[RV$idx_lang, 1], ": ",
        round(-1 * RV$RES$mat_dlevel[idx_x, idx_y], 2), "m<br>",
        Server_char_dict$map$LblTxt[RV$idx_lang, 2], ": ",
        round(-1 * RV$RES$mat_dlevel_ss[idx_x, idx_y], 2), "m"
      ))
    }


    # Add a marker to the map
    leafletProxy("map") %>%
      clearGroup("selPts") %>%
      addMarkers(
        lng = clng, lat = clat,
        icon = mapIcon,
        label = labelTxt,
        group = "selPts"
      )
  })

  # ==== __switcher: changing steady state/trasient map ====
  observeEvent(input$map_plot_switch, {
    if (is.null(RV$mapData_tr) | is.null(RV$mapData_ss)) {
      return(NULL)
    }

    if (input$map_plot_switch == "tr") {
      pols <- RV$mapData_tr
    } else {
      pols <- RV$mapData_ss
    }

    cc <- c("#a50027", "#f56c42", "#ffbea6", "#ffffff", "#6db7c4", "#4b5ea8")
    tmp <- c(1:6)

    pal <- colorNumeric(
      palette = cc,
      domain = tmp
    )

    leafletProxy("map") %>%
      clearGroup("dataMap") %>%
      clearGroup("selectedFtr") %>%
      addPolygons(
        data = pols, fillColor = ~ pal(pols@data[[1]]),
        fillOpacity = 0.5, stroke = FALSE, group = "dataMap"
      )
  })


  # ==== __btm: download example file ====

  output$downloadBT <- downloadHandler(
    filename <- function() {
      paste("template", "xls", sep = ".")
    },

    content <- function(file) {
      if (RV$idx_lang == 1) {
        file.copy("R_data/template_cn.xls", file)
      } else {
        file.copy("R_data/template_en.xls", file)
      }
    }
  )

  ## ==== Plots, Text and Tables ====

  # ==== __Map ====

  output$map <- renderLeaflet({
    cc <- c("#a50027", "#f56c42", "#ffbea6", "#ffffff", "#6db7c4", "#4b5ea8")
    legendStr <- c("<-10", "[-10, -6)", "[-6, -2)", "[-2, 2)", "[2, 6)", ">6")

    leaflet() %>%
      addTiles() %>%
      addPolygons(
        data = RV$BS$shape_bdry_pol, color = "#ad1a51",
        opacity = 0.7, fill = F, weight = 2,
        group = Server_char_dict$map$group_bdy[RV$idx_lang]
      ) %>%
      addPolygons(
        data = RV$BS$shape_irr_area_pol, color = "#636363",
        opacity = 0.6, fill = TRUE, weight = 1
      ) %>%
      addPolylines(
        data = RV$BS$shape_irr_chanels, color = "#f3c062",
        opacity = 0.5, weight = 2,
        group = Server_char_dict$map$group_canals[RV$idx_lang]
      ) %>%
      addLegend("bottomright",
        colors = cc, className = "legendbox legend",
        title = Server_char_dict$map$legend[RV$idx_lang],
        opacity = 0.5, labels = legendStr
      ) %>%
      addScaleBar("bottomleft") %>%
      addEasyButton(easyButton(
        icon = "fa-globe", title = Server_char_dict$map$btReset[RV$idx_lang],
        onClick = JS("function(btn, map){ map.setView(new L.LatLng(39.18, 100.17), 8); }")
      )) %>%
      addLayersControl(
        overlayGroups = c(
          Server_char_dict$map$group_bdy[RV$idx_lang],
          Server_char_dict$map$group_canals[RV$idx_lang]
        ),
        options = layersControlOptions(collapsed = FALSE)
      )
  })

  # ==== __Chart: Drawdown curves ====

  output$plot2 <- renderAmCharts({
    isolate({
      no_yrs <- RV$handles$t_sim / 365
      t_steps <- RV$handles$t_steps
      t_steps_default <- data.BS$t_steps_default
    })

    step_len <- no_yrs / t_steps

    if (is.null(RV$RES) | is.null(RV$int_loc)) {
      RV$z <- numeric(length = t_steps_default)
    } else {
      # Extract drawdown series for selected point
      idx_x <- RV$idx_loc[1]
      idx_y <- RV$idx_loc[2]
      RV$z <- RV$RES$mat_dlevel_array[idx_x, idx_y, ]
    }

    t_axis <- seq(0, no_yrs, step_len)
    int_z_data <- c(0, RV$z[1:RV$handles$t_steps])

    dataIn <- data.frame(tsteps = t_axis, drawdown = int_z_data)

    amSerialChart(
      dataProvider = dataIn, theme = "light", categoryField = "tsteps",
      minMarginBottom = 50,
      fontFamily = Server_char_dict$plot$font_type[RV$idx_lang],
      fontSize = 14
    ) %>>%
      addGraph(
        valueField = "drawdown", precision = 3,
        negativeLineColor = "#B21212",
        lineColor = "#1485CC",
        lineThickness = 4,
        bullet = "round",
        bulletBorderThickness = 5,
        bulletBorderAlpha = 1,
        bulletColor = "#ffffff",
        bulletBorderColor = "#5b7ea3",
        bulletSize = 10
      ) %>>%
      addTitle(
        text = Server_char_dict$plot$drawdown_title[RV$idx_lang],
        size = 24
      ) %>>%
      addLabel(
        text = Server_char_dict$plot$drawdown_lab_x[RV$idx_lang],
        size = 16,
        x = "45%",
        y = "94.5%"
      ) %>>%
      setChartCursor(valueLineEnabled = FALSE, valueLineBalloonEnabled = TRUE)
  })

  # ==== __Chart: Barplots for irrigation consumption ====

  output$plot3 <- renderAmCharts({
    label_cn <- UI_char_dict$dist_name[RV$idx_lang, ]
    data1 <- RV$RES$irrigation_results

    if (is.null(data1)) {
      data_sim <- data.frame(
        SW_prior = RV$BS$irr$Q_SW_ref / 1e4,
        GW_prior = RV$BS$irr$Q_GW_ref / 1e4
      )
    } else {
      data_sim <- data.frame(round(data1[c(4, 6)] / 1e4, 2))
    }

    rownames(data_sim) <- label_cn
    colnames(data_sim) <- Server_char_dict$plot$bar_group[RV$idx_lang, ]

    amBarplot(
      y = Server_char_dict$plot$bar_group[RV$idx_lang, ],
      data = data_sim,
      groups_color = c("#BBD577", "#4B8DA8"),
      ylab = Server_char_dict$plot$bar_lab_y[RV$idx_lang],
      labelRotation = 90
    ) %>%
      setProperties(fontFamily = Server_char_dict$plot$font_type[RV$idx_lang], fontSize = 14) %>%
      setChartCursor()
  })

  # ==== __Table: Irrigation consumption ====

  output$table_statis <- renderFormattable({
    nr_dist <- length(RV$BS$irr$area_irr_ref)
    sign_formatter <- formatter("span",
      style = x ~ style(color = ifelse(x > 0, "green",
        ifelse(x < 0, "red", "black")
      ))
    )

    if (is.null(RV$RES$irrigation_results)) {
      data2 <- data.frame(
        Area = RV$BS$irr$area_irr_ref,
        Area_change = numeric(nr_dist),
        SW_usage = data.BS$irr$Q_SW_ref,
        SW_change = numeric(nr_dist),
        GW_usage = data.BS$irr$Q_GW_ref,
        GW_change = numeric(nr_dist)
      )
    } else {
      data2 <- RV$RES$irrigation_results[, -1]
    }

    data2 <- round(data2 / 1e4, 1)

    colnames(data2) <- Server_char_dict$plot$table_sta_colname[RV$idx_lang, ]
    rownames(data2) <- UI_char_dict$dist_name[RV$idx_lang, ]

    formattable(
      data2,
      list(
        area(col = Server_char_dict$plot$table_sta_colname[RV$idx_lang, c(3, 5)]) ~ normalize_bar("lightblue", 0.2),
        area(col = Server_char_dict$plot$table_sta_colname[RV$idx_lang, c(2, 4, 6)]) ~ sign_formatter
      )
    )
  })


  ## ==== Quit session whenver the website is cloded ====
  session$onSessionEnded(function() {
    stopApp()
  })
} # end of "server.R"
