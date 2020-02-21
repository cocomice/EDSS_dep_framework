# server.R

shinyServer(function(input, output, session) {



  ## --------------- List of input names

  getInputs <- reactive({
    inputList <- sort(names(reactiveValuesToList(input)))
    inputList <- inputList[!grepl("^dy", inputList)]
    inputList <- inputList[!grepl("^CalButton", inputList)]
    inputList <- c(inputList, "DownloadTab", "DownloadPlot")
    return(inputList)
  })



  ## --------------- Data preparation

  getPrep <- reactive({
    TMGR <- .TypeModelGR(input$HydroModel)
    PARAM <- c(input$X1, input$X2, input$X3, input$X4, input$X5, input$X6)[seq_len(TMGR$NbParam)]

    if (input$SnowModel == "CemaNeige") {
      PARAM <- c(PARAM, input$C1, input$C2)
    }
    if (input$Dataset == "Unnamed watershed") {
      ObsDF <- NULL
    } else {
      # ObsDF <- get(input$Dataset)
      ObsDF <- ShinyGR$ObsDF[[input$Dataset]]
    }
    PREP <- PrepGR(
      ObsDF = ObsDF,
      DatesR = ShinyGR$DatesR,
      Precip = ShinyGR$Precip, PotEvap = ShinyGR$PotEvap,
      Qobs = ShinyGR$Qobs, TempMean = ShinyGR$TempMean,
      ZInputs = ShinyGR$ZInputs[[input$Dataset]],
      HypsoData = ShinyGR$HypsoData[[input$Dataset]],
      NLayers = ShinyGR$NLayers[[input$Dataset]],
      HydroModel = input$HydroModel,
      CemaNeige = input$SnowModel == "CemaNeige"
    )


    ## old value: bad time zone management
    # WUPPER <- c(PREP$InputsModel$DatesR[1L], input$Period[1]-.TypeModelGR(PREP)$TimeLag)
    ## patch from Juan Camilo Peña <juancamilopec@gmail.com>
    # WUPPER <- c(format(PREP$InputsModel$DatesR[1L], format = "%Y-%m-%d", tz = "UTC"), format(input$Period[1]-.TypeModelGR(PREP)$TimeLag, format = "%Y-%m-%d", tz = "UTC"))
    ## new value
    WUPPER <- as.POSIXlt(c(as.character(PREP$InputsModel$DatesR[1L]), as.character(input$Period[1] - .TypeModelGR(PREP)$TimeLag)), tz = "UTC")
    if (WUPPER[2] < WUPPER[1]) {
      WUPPER[2] <- WUPPER[1]
    }

    ## Enable or disable automatic calibration (if there is Qobs or not)
    isQobs <- !all(is.na(PREP$Qobs[PREP$InputsModel$Dates >= input$Period[1] & PREP$InputsModel$Dates <= input$Period[2]]))
    # if ( isQobs | input$Period[1L] != input$Period[2L]) {
    #   shinyjs::enable("CalButton")
    # }
    if (!isQobs | input$Period[1L] == input$Period[2L]) {
      shinyjs::disable("CalButton")
    }

    return(list(TMGR = TMGR, PREP = PREP, WUPPER = WUPPER))
  })



  ## --------------- Calibration

  ## If the user calibrate the model
  CAL_click <- reactiveValues(valueButton = 0)


  ## Automatic calibration
  observeEvent(input$CalButton,
    {

      ## Desable all inputs during automatic calibration
      lapply(getInputs(), shinyjs::disable)
      shinyjs::disable("CalButton")

      ## Model calibration
      CAL_opt <- list(
        Crit = gsub(" .*", "", input$TypeCrit),
        Transfo = gsub("1", "inv", gsub("(\\D{3} \\[)(\\w{0,4})(\\W*Q\\W*\\])", "\\2", input$TypeCrit))
      )
      CAL <- CalGR(
        PrepGR = getPrep()$PREP, CalCrit = CAL_opt$Crit, transfo = CAL_opt$Transfo,
        WupPer = substr(getPrep()$WUPPER, 1, 10),
        CalPer = c(substr(input$Period[1], 1, 10), substr(input$Period[2], 1, 10)),
        verbose = FALSE
      )
      PARAM <- CAL$OutputsCalib$ParamFinalR

      updateSliderInput(session, inputId = "X1", value = PARAM[1L])
      updateSliderInput(session, inputId = "X2", value = PARAM[2L])
      updateSliderInput(session, inputId = "X3", value = PARAM[3L])
      updateSliderInput(session, inputId = "X4", value = PARAM[4L])
      if (getPrep()$TMGR$NbParam >= 5) {
        updateSliderInput(session, inputId = "X5", value = PARAM[5L])
      }
      if (getPrep()$TMGR$NbParam >= 6) {
        updateSliderInput(session, inputId = "X6", value = PARAM[6L])
      }
      if (input$SnowModel == "CemaNeige") {
        updateSliderInput(session, inputId = "C1", value = PARAM[length(PARAM) - 1])
        updateSliderInput(session, inputId = "C2", value = PARAM[length(PARAM)])
      }
      updateActionButton(session, inputId = "CalButton", label = "Model calibrated", icon = icon("check"))
      CAL_click$valueButton <- 1
      shinyjs::disable("CalButton")
      ## Enable calibration
      # if (input$Period[1L] != input$Period[2L]) {
      #   lapply(getInputs(), shinyjs::enable)
      #   shinyjs::enable("CalButton")
      # }
    },
    priority = +20
  )


  ## Manual calibration
  observeEvent(
    {
      input$Dataset
      input$HydroModel
      input$SnowModel
      input$X1
      input$X2
      input$X3
      input$X4
      input$X5
      input$X6
      input$C1
      input$C2
      input$TypeCrit
      input$Period
    },
    {
      CAL_click$valueButton <- CAL_click$valueButton - 1
      CAL_click$valueButton <- ifelse(CAL_click$valueButton < -1, -1, CAL_click$valueButton)
      if (CAL_click$valueButton < 0) {
        updateActionButton(session, inputId = "CalButton", label = "Run", icon = icon("refresh"))
        shinyjs::enable("CalButton")
      }

      ## Enable all inputs except automatic calibration
      if (input$Period[1L] != input$Period[2L]) {
        lapply(getInputs(), shinyjs::enable)
      }
    }
  )



  ## --------------- Simulation

  getSim <- reactive({
    PARAM <- c(input$X1, input$X2, input$X3, input$X4, input$X5, input$X6)[seq_len(getPrep()$TMGR$NbParam)]
    if (input$SnowModel == "CemaNeige") {
      PARAM <- c(PARAM, input$C1, input$C2)
    }

    ## Simulated flows computation
    SIM <- SimGR(
      PrepGR = getPrep()$PREP, Param = PARAM,
      WupPer = substr(getPrep()$WUPPER, 1, 10),
      SimPer = c(substr(input$Period[1], 1, 10), substr(input$Period[2], 1, 10)),
      verbose = FALSE
    )

    ## Criteria computation
    CRIT_opt <- list(
      Crit = c(rep("ErrorCrit_NSE", 3), rep("ErrorCrit_KGE", 3)),
      Transfo = rep(c("", "sqrt", "inv"), times = 2)
    )
    InputsCritMulti <- CreateInputsCrit(
      FUN_CRIT = CRIT_opt$Crit,
      InputsModel = getPrep()$PREP$InputsModel,
      RunOptions = SIM$OptionsSimul,
      Obs = replicate(n = 6, expr = SIM$Qobs, simplify = FALSE),
      VarObs = rep("Q", times = 6),
      transfo = CRIT_opt$Transfo,
      Weights = NULL
    )
    iCRIT <- ErrorCrit(InputsCrit = InputsCritMulti, OutputsModel = SIM$OutputsModel, verbose = FALSE)
    CRIT <- do.call("rbind", lapply(iCRIT, function(i) data.frame(CritName = NA, CritValue = i$CritValue))) ## patch: CritName = i$CritName to change with the future airGR
    CRIT$CritName <- c("NSE [Q]", "NSE [sqrt(Q)]", "NSE [1/Q]", "KGE [Q]", "KGE [sqrt(Q)]", "KGE [1/Q]") ## patch: to change with the future airGR
    # CRIT$CritName <- gsub("\\[", " [", CRIT$CritName) ## patch: to change with the future airGR
    CRIT <- rbind(CRIT, data.frame(
      CritName = "BIAS [Qsim/Qobs]",
      CritValue = ifelse(is.na(iCRIT[[which(CRIT$CritName == "KGE [Q]")]]$CritValue),
        NA,
        iCRIT[[which(CRIT$CritName == "KGE [Q]")]]$SubCritValues[3]
      )
    ))
    colnames(CRIT) <- c("Criterion", "Value")
    # CRIT_opt <- list(Crit    = c("ErrorCrit_NSE", "ErrorCrit_KGE"),
    #                  Transfo = c("NO", "sqrt", "inv"))
    # CRIT <- lapply(CRIT_opt$Crit, function(iCRIT) {
    #   Qtransfo <- lapply(CRIT_opt$Transfo, function(iTRSF) {
    #     iInputsCrit <- SIM$OptionsCrit
    #     iTRSF <- gsub("NO", "", iTRSF)
    #     iInputsCrit$transfo <- iTRSF
    #     iCRIT <- ErrorCrit(InputsCrit = iInputsCrit, OutputsModel = SIM$OutputsModel, FUN_CRIT = get(iCRIT), verbose = FALSE)
    #     iCRIT <- iCRIT[c("CritName", "CritValue")]
    #     return(iCRIT)
    #   })
    #   return(Qtransfo)
    # })
    # print(CRIT)
    # CRIT <- as.data.frame(matrix(na.omit(unlist(CRIT)), ncol = 2, byrow = TRUE), stringsAsFactors = FALSE)
    # colnames(CRIT) <- c("Criterion", "Value")
    # rownames(CRIT) <- NULL
    # CRIT$Value     <- as.numeric(CRIT$Value)
    # CRIT$Criterion <- gsub("\\[", " [", CRIT$Criterion)

    ## Recording past simulations
    ShinyGR.hist[[length(ShinyGR.hist) + 1]] <- list(
      Qsim = SIM$OutputsModel$Qsim,
      Param = PARAM,
      TypeModel = SIM$TypeModel,
      Crit = CRIT,
      Dataset = input$Dataset
    )

    ShinyGR.hist <- ShinyGR.hist[!(duplicated(sapply(ShinyGR.hist, function(x) sum(x$Param)), fromLast = TRUE) &
      duplicated(sapply(ShinyGR.hist, function(x) x$TypeModel), fromLast = TRUE))]
    ShinyGR.hist <- tail(ShinyGR.hist, n = 2)

    if (length(ShinyGR.hist) == 2 & is.null(names(ShinyGR.hist[[1]]))) {
      ShinyGR.hist[[1]] <- NULL
    }
    if (length(ShinyGR.hist) == 2) {
      if (ShinyGR.hist[[1]]$Dataset != ShinyGR.hist[[2]]$Dataset) { # reset Qold when new dataset
        ShinyGR.hist[[1]] <- NULL
      }
    }
    if (length(ShinyGR.hist) == 2 & !is.null(names(ShinyGR.hist[[1]]))) {
      isEqualSumQsim <- !identical(sum(ShinyGR.hist[[1]]$Crit$Value), sum(ShinyGR.hist[[2]]$Crit$Value))
      isEqualTypeModel <- ShinyGR.hist[[1]]$TypeModel == ShinyGR.hist[[2]]$TypeModel
      if (length(ShinyGR.hist[[1]]$Qsim) != length(ShinyGR.hist[[2]]$Qsim) |
        (isEqualSumQsim & isEqualTypeModel)) {
        OBSold <- getPrep()$PREP
        OBSold$TypeModel <- ShinyGR.hist[[1]]$TypeModel
        if (.TypeModelGR(OBSold)$CemaNeige & !.TypeModelGR(getPrep()$PREP)$CemaNeige | # present: No CemaNeige ; old: CemaNeige
          isEqualSumQsim & isEqualTypeModel) {
          if (input$Dataset == "Unnamed watershed") {
            ObsDF <- NULL
          } else {
            # ObsDF <- get(input$Dataset)
            ObsDF <- ShinyGR$ObsDF[[input$Dataset]]
          }
          OBSold <- PrepGR(
            ObsDF = ObsDF,
            Precip = ShinyGR$Precip, PotEvap = ShinyGR$PotEvap,
            Qobs = ShinyGR$Qobs, TempMean = ShinyGR$TempMean,
            ZInputs = ShinyGR$ZInputs[[input$Dataset]],
            HypsoData = ShinyGR$HypsoData[[input$Dataset]],
            NLayers = ShinyGR$NLayers[[input$Dataset]],
            HydroModel = input$HydroModel,
            CemaNeige = input$SnowModel == "CemaNeige"
          )
        }
        SIMold <- SimGR(
          PrepGR = OBSold,
          Param = ShinyGR.hist[[1]]$Param,
          WupPer = substr(getPrep()$WUPPER, 1, 10),
          SimPer = substr(c(input$Period[1], input$Period[2]), 1, 10),
          verbose = FALSE
        )
        # CRITold <- lapply(CRIT_opt$Crit, function(iCRIT) {
        #   SIM_transfo <- lapply(CRIT_opt$Transfo, function(iTRSF) {
        #     iTRSF <- gsub("NO", "", iTRSF)
        #     SIMold$OptionsCrit$transfo <- iTRSF
        #     iCRITold <- ErrorCrit(InputsCrit = SIMold$OptionsCrit, OutputsModel = SIMold$OutputsModel, FUN_CRIT = get(iCRIT), verbose = FALSE)
        #     iCRITold <- iCRITold[c("CritName", "CritValue")]
        #     return(iCRITold)
        #   })
        # })
        # CRITold <- as.data.frame(matrix(na.omit(unlist(CRITold)), ncol = 2, byrow = TRUE), stringsAsFactors = FALSE)
        # colnames(CRITold) <- c("Criterion", "Value")
        # rownames(CRITold) <- NULL
        # CRITold$Value     <- as.numeric(CRITold$Value)
        # CRITold$Criterion <- gsub("\\[", " [", CRITold$Criterion)
        InputsCritMultiold <- CreateInputsCrit(
          FUN_CRIT = CRIT_opt$Crit,
          InputsModel = OBSold$InputsModel,
          RunOptions = SIMold$OptionsSimul,
          Obs = replicate(n = 6, expr = SIMold$Qobs, simplify = FALSE),
          VarObs = rep("Q", times = 6),
          transfo = CRIT_opt$Transfo,
          Weights = NULL
        )
        # iCRITold <- ErrorCrit(InputsCrit = InputsCritMultiold, OutputsModel = SIMold$OutputsModel, verbose = FALSE)
        # CRITold <- do.call("rbind", lapply(iCRITold, function(i) data.frame(CritName = i$CritName, CritValue = i$CritValue)))
        iCRITold <- ErrorCrit(InputsCrit = InputsCritMulti, OutputsModel = SIMold$OutputsModel, verbose = FALSE)
        CRITold <- do.call("rbind", lapply(iCRITold, function(i) data.frame(CritName = NA, CritValue = i$CritValue))) ## patch: CritName = i$CritName to change with the future airGR
        CRITold$CritName <- c("NSE [Q]", "NSE [sqrt(Q)]", "NSE [1/Q]", "KGE [Q]", "KGE [sqrt(Q)]", "KGE [1/Q]") ## patch: to change with the future airGR
        # CRIT$CritName <- gsub("\\[", " [", CRIT$CritName) ## patch: to change with the future airGR
        # CRITold <- rbind(CRITold, data.frame(CritName = "BIAS [Qsim/Qobs]", CritValue = iCRITold[[which(CRITold$CritName == "KGE [Q]")]]$SubCritValues[3]))
        CRITold <- rbind(CRITold, data.frame(
          CritName = "BIAS [Qsim/Qobs]",
          CritValue = ifelse(is.na(iCRITold[[which(CRITold$CritName == "KGE [Q]")]]$CritValue),
            NA,
            iCRITold[[which(CRITold$CritName == "KGE [Q]")]]$SubCritValues[3]
          )
        ))
        colnames(CRITold) <- c("Criterion", "Value")

        ShinyGR.hist[[1]]$Crit <- CRITold
        ShinyGR.hist[[1]]$Qsim <- SIMold$OutputsModel$Qsim
      }
    }

    return(list(PARAM = PARAM, SIM = SIM, SIMold = ShinyGR.hist, Crit = CRIT))
  })



  ## --------------- Plot

  ## Choice
  getPlotType <- reactive({
    switch(input$PlotType,
      "Model performance" = 1,
      "Flow time series"  = 2,
      "State variables"   = 3,
      "Model diagram"     = 4
    )
  })


  ## Models available considering the plot type
  # observe({
  #   if (getPlotType() == 4) {
  #     updateSelectInput(session, inputId = "HydroModel", choice = c("GR4J", "GR5J", "GR6J"), selected = input$HydroModel)
  #     updateSelectInput(session, inputId = "SnowModel" , choice = c("None"))
  #   } else {
  #     updateSelectInput(session, inputId = "HydroModel", choice = c("GR4J", "GR5J", "GR6J"), selected = input$HydroModel)
  #     updateSelectInput(session, inputId = "SnowModel" , choice = c("None", "CemaNeige")   , selected = input$SnowModel)
  #   }
  # })


  ## Plots available considering the model type
  # observe({
  #   if (input$HydroModel == "GR6J") {
  #     updateSelectInput(session, inputId = "PlotType",
  #                       choice = c("Flow time series", "Model performance", "State variables"),
  #                       selected = input$PlotType)
  #   } else {
  #     updateSelectInput(session, inputId = "PlotType",
  #                       choice = c("Flow time series", "Model performance", "State variables", "Model diagram"),
  #                       selected = input$PlotType)
  #   }
  # })


  # Formated simulation results
  getData <- reactive({
    OutputsModel <- getSim()$SIM$OutputsModel
    IndPlot <- which(OutputsModel$DatesR >= input$Period[1L] & OutputsModel$DatesR <= input$Period[2L])
    OutputsModel2 <- sapply(OutputsModel[seq_len(which(names(OutputsModel) == "Qsim"))], function(x) x[IndPlot])
    OutputsModel2 <- c(OutputsModel2, Qobs = list(getSim()$SIM$Qobs[IndPlot]))

    if (length(OutputsModel2$DatesR) != 0) {
      data <- data.frame(
        DatesR = OutputsModel2$DatesR,
        precip. = OutputsModel2$Precip,
        PET = OutputsModel2$PotEvap,
        prod. = OutputsModel2$Prod,
        rout. = OutputsModel2$Rout,
        # exp.    = rep(NA, length(OutputsModel2$DatesR)),
        # 'exp. (+)'= rep(NA, length(OutputsModel2$DatesR)),
        # 'exp. (-)'= rep(NA, length(OutputsModel2$DatesR)),
        Qr = OutputsModel2$QR,
        Qd = OutputsModel2$QD,
        Qsim = OutputsModel2$Qsim,
        Qobs = OutputsModel2$Qobs,
        QsimOld = rep(NA, length(OutputsModel2$DatesR))
      )
      # QrExp   = rep(NA, length(OutputsModel2$DatesR)))

      if (length(ShinyGR.hist) == 2 & input$ShowOldQsim == "Yes") {
        data$QsimOld <- ShinyGR.hist[[1]]$Qsim[seq_len(nrow(data))]
      }
      if (input$HydroModel == "GR6J") {
        data$"exp." <- NULL
        data$"exp. (+)" <- ifelse(OutputsModel2$Exp >= 0, OutputsModel2$Exp, NA)
        data$"exp. (-)" <- ifelse(OutputsModel2$Exp < 0, OutputsModel2$Exp, NA)
        data$QrExp <- OutputsModel2$QRExp
      }

      return(list(OutputsModel = OutputsModel2, Tab = data))
    }
  })


  ## Period slider responds to changes in the selected/zoomed dateWindow
  observeEvent(
    {
      input$dyPlotTS_date_window
      input$dyPlotSVs_date_window
      input$dyPlotMDp_date_window
    },
    {
      if (!is.null(input$dyPlotTS_date_window) && getPlotType() == 2) {
        dateWindow <- as.POSIXct(strftime(input$dyPlotTS_date_window, "%Y-%m-%d %H:%M:%S"), tz = "UTC")
      }
      if (!is.null(input$dyPlotSVq_date_window) && getPlotType() == 3) {
        dateWindow <- as.POSIXct(strftime(input$dyPlotSVq_date_window, "%Y-%m-%d %H:%M:%S"), tz = "UTC")
      }
      if (!is.null(input$dyPlotMDp_date_window) && getPlotType() == 4) {
        dateWindow <- as.POSIXct(strftime(input$dyPlotMDp_date_window, "%Y-%m-%d %H:%M:%S"), tz = "UTC")
      }
      if (exists("dateWindow")) {
        # if (dateWindow[1L] == dateWindow[2L]) {
        #   if (dateWindow[1L] == as.POSIXct(ShinyGR$SimPer[2L], tz = "UTC")) {
        #     updateSliderInput(session, inputId = "Period",
        #                       value = dateWindow - c(1, 0) * .TypeModelGR(input$HydroModel)$TimeLag)
        #   } else {
        #     updateSliderInput(session, inputId = "Period",
        #                       value = dateWindow + c(0, 1) * .TypeModelGR(input$HydroModel)$TimeLag)
        #   }
        # } else {
        if (dateWindow[1L] != dateWindow[2L]) {
          updateSliderInput(session,
            inputId = "Period",
            value = dateWindow, ### + .TypeModelGR(input$HydroModel)$TimeLag,
            timeFormat = "%F", timezone = "+0000"
          )
        }
        # }
      }
    },
    priority = +100
  )


  # observe({
  #   if (getPlotType() == 1) {
  #     if (input$Period[1L] == input$Period[2L]) {
  #       if (input$Period[1L] == as.POSIXct(ShinyGR$SimPer[2L], tz = "UTC")) {
  #         updateSliderInput(session, inputId = "Period",
  #                           value = input$Period - c(1, 0) * .TypeModelGR(input$HydroModel)$TimeLag)
  #       } else {
  #         updateSliderInput(session, inputId = "Period",
  #                           value = input$Period + c(0, 1) * .TypeModelGR(input$HydroModel)$TimeLag)
  #       }
  #     }
  #   }
  # }, priority = +100)

  ## Disable all inputs if there is no data
  observe(
    {
      if (input$Period[1L] == input$Period[2L]) {
        inputs <- gsub("Period", "CalButton", getInputs())
        lapply(inputs, shinyjs::disable)
      }
    },
    priority = -100
  )


  ## Reset period slider responds to dygraphs to mouse clicks
  observeEvent(
    {
      input$dyPlotTS_click
    },
    {
      updateSliderInput(session,
        inputId = "Period",
        value = as.POSIXct(ShinyGR$SimPer[[input$Dataset]], tz = "UTC"),
        timeFormat = "%F", timezone = "+0000"
      )
    },
    priority = +10
  )
  observeEvent(
    {
      input$dyPlotSVs_click
    },
    {
      updateSliderInput(session,
        inputId = "Period",
        value = as.POSIXct(ShinyGR$SimPer[[input$Dataset]], tz = "UTC"),
        timeFormat = "%F", timezone = "+0000"
      )
    },
    priority = +10
  )
  observeEvent(
    {
      input$dyPlotSVq_click
    },
    {
      updateSliderInput(session,
        inputId = "Period",
        value = as.POSIXct(ShinyGR$SimPer[[input$Dataset]], tz = "UTC"),
        timeFormat = "%F", timezone = "+0000"
      )
    },
    priority = +10
  )
  observeEvent(
    {
      input$dyPlotMDp_click
    },
    {
      updateSliderInput(session,
        inputId = "Period",
        value = as.POSIXct(ShinyGR$SimPer[[input$Dataset]], tz = "UTC"),
        timeFormat = "%F", timezone = "+0000"
      )
    },
    priority = +10
  )
  observeEvent(
    {
      input$dyPlotMDe_click
    },
    {
      updateSliderInput(session,
        inputId = "Period",
        value = as.POSIXct(ShinyGR$SimPer[[input$Dataset]], tz = "UTC"),
        timeFormat = "%F", timezone = "+0000"
      )
    },
    priority = +10
  )
  observeEvent(
    {
      input$dyPlotMDq_click
    },
    {
      updateSliderInput(session,
        inputId = "Period",
        value = as.POSIXct(ShinyGR$SimPer[[input$Dataset]], tz = "UTC"),
        timeFormat = "%F", timezone = "+0000"
      )
    },
    priority = +10
  )


  ## Time window slider
  observeEvent(
    {
      input$Dataset
    },
    {
      updateSliderInput(session,
        inputId = "Period",
        min = as.POSIXct(ShinyGR$SimPer[[input$Dataset]][1L], tz = "UTC"),
        max = as.POSIXct(ShinyGR$SimPer[[input$Dataset]][2L], tz = "UTC"),
        value = as.POSIXct(ShinyGR$SimPer[[input$Dataset]], tz = "UTC"),
        timeFormat = "%F", timezone = "+0000"
      )
    }
  )


  ## Target date slider
  eventReactive(
    {
      input$Dataset
    },
    {
      updateSliderInput(session,
        inputId = "Event", label = "Select the target date:",
        min = as.POSIXct(ShinyGR$SimPer[[input$Dataset]][1L], tz = "UTC"), ## + .TypeModelGR(input$HydroModel)$TimeLag,
        max = as.POSIXct(ShinyGR$SimPer[[input$Dataset]][2L], tz = "UTC"),
        value = as.POSIXct(ShinyGR$SimPer[[input$Dataset]][1L], tz = "UTC"), +.TypeModelGR(input$HydroModel)$TimeLag,
        timeFormat = "%F", timezone = "+0000"
      )
    }
  )
  observe({
    updateSliderInput(session,
      inputId = "Event", label = "Select the target date:",
      min = input$Period[1L], ## + .TypeModelGR(input$HydroModel)$TimeLag,
      max = input$Period[2L],
      timeFormat = "%F", timezone = "+0000"
    )
  })


  ## Graphical parameters
  getPlotPar <- reactive({
    if (ShinyGR$theme == "Cyborg") {
      col_bg <- "black"
      col_fg <- "white"
      par(bg = col_bg, fg = col_fg, col.axis = col_fg, col.lab = col_fg)
    } else if (ShinyGR$theme == "Flatly") {
      col_bg <- "#2C3E50"
      col_fg <- "black"
      par(bg = col_bg, fg = col_fg, col.axis = col_bg, col.lab = col_bg)
    } else {
      col_bg <- "white"
      col_fg <- "black"
      par(bg = col_bg, fg = col_fg)
    }
    return(list(col_bg = col_bg, col_fg = col_fg, par = par(no.readonly = TRUE)))
  })


  ## Plot model performance
  output$stPlotMP <- renderPlot(
    {
      if (length(getSim()$SIM$OutputsModel$DatesR) < 2) {
        return(NULL)
      }
      OutputsModel <- getSim()$SIM$OutputsModel
      IndPlot <- which(OutputsModel$DatesR >= input$Period[1L] & OutputsModel$DatesR <= input$Period[2L])
      par(getPlotPar()$par)
      par(cex.axis = 1.2)
      if (input$SnowModel != "CemaNeige") {
        par(oma = c(20, 0, 0, 0))
      }
      plot(OutputsModel, Qobs = getSim()$SIM$Qobs, IndPeriod_Plot = IndPlot, cex.lab = 1.2, cex.axis = 1.4, cex.leg = 1.4)
    },
    bg = "transparent"
  )


  ## Plot flow time series
  output$dyPlotTS <- dygraphs::renderDygraph({
    if (length(getSim()$SIM$OutputsModel$DatesR) < 2) {
      return(NULL)
    }
    if (length(getSim()$SIMold) == 2 & input$ShowOldQsim == "Yes") {
      QsimOld <- getSim()$SIMold[[1]]$Qsim
    } else {
      QsimOld <- NULL
    }
    op <- getPlotPar()$par
    dg1 <- dyplot(getSim()$SIM,
      Qsup = QsimOld, Qsup.name = "Qold", RangeSelector = FALSE, LegendShow = "auto",
      col.Q = c(op$fg, "orangered", "grey"), col.Precip = c("#428BCA", "lightblue")
    )
    dg1 <- dygraphs::dyOptions(dg1,
      axisLineColor = op$fg, axisLabelColor = op$fg,
      retainDateWindow = FALSE, useDataTimezone = TRUE
    )
    dg1 <- dygraphs::dyLegend(dg1, show = "follow", width = 325)
  })


  ## Plot state variables stores
  output$dyPlotSVs <- dygraphs::renderDygraph({
    if (length(getSim()$SIM$OutputsModel$DatesR) < 2) {
      return(NULL)
    }
    # OutputsModel <- getSim()$SIM$OutputsModel
    # data <- data.frame(DatesR = OutputsModel$DatesR,
    #                    prod.  = OutputsModel$Prod,
    #                    rout.  = OutputsModel$Rout)
    data <- getData()$Tab[, c("DatesR", "prod.", "rout.", grep("^exp", colnames(getData()$Tab), value = TRUE))]
    data.xts <- xts::xts(data[, -1L], order.by = data$DatesR, tzone = "UTC")

    if (input$HydroModel == "GR6J") {
      colors <- c("#00008B", "#008B8B", "#10B510", "#FF0303")
    } else {
      colors <- c("#00008B", "#008B8B")
    }

    op <- getPlotPar()$par
    dg2 <- dygraphs::dygraph(data.xts, group = "state_var", ylab = "store [mm]")
    dg2 <- dygraphs::dyOptions(dg2,
      colors = colors,
      fillGraph = TRUE, fillAlpha = 0.3,
      drawXAxis = FALSE, axisLineColor = op$fg, axisLabelColor = op$fg,
      retainDateWindow = FALSE, useDataTimezone = TRUE
    )
    dg2 <- dygraphs::dyLegend(dg2, show = "always", width = 325)
    dg2 <- dygraphs::dyCrosshair(dg2, direction = "vertical")
  })


  ## Plot state variables Q
  output$dyPlotSVq <- dygraphs::renderDygraph({
    if (length(getSim()$SIM$OutputsModel$DatesR) < 2) {
      return(NULL)
    }
    # OutputsModel <- getSim()$SIM$OutputsModel
    # IndPlot <- which(OutputsModel$DatesR >= input$Period[1L] & OutputsModel$DatesR <= input$Period[2L])
    # OutputsModel2 <- sapply(OutputsModel[seq_len(which(names(OutputsModel) == "Qsim"))], function(x) x[IndPlot])
    # OutputsModel2 <- c(OutputsModel2, Qobs = list(getSim()$SIM$Qobs[IndPlot]))
    #
    # data <- data.frame(DatesR = OutputsModel2$DatesR,
    #                    Qr     = OutputsModel2$QR,
    #                    Qd     = OutputsModel2$QD,
    #                    Qsim   = OutputsModel2$Qsim,
    #                    Qobs   = OutputsModel2$Qobs)
    # if (input$HydroModel == "GR6J") {
    #   data$QrExp <- OutputsModel2$QRExp
    # } else {
    #   data$QrExp <- NA
    # }

    colSelec <- c("DatesR", "Qr", "Qd", grep("^QrExp", colnames(getData()$Tab), value = TRUE), "Qsim", "Qobs")
    if (length(getSim()$SIMold) == 2 & input$ShowOldQsim == "Yes") {
      colSelec <- c(colSelec, "QsimOld")
    }

    data <- getData()$Tab[, colSelec]
    data.xts <- xts::xts(data[, -1L], order.by = data$DatesR, tzone = "UTC")

    if (input$HydroModel == "GR6J") {
      names <- c("Qd", "Qr", "QrExp")
      colors <- c("#FFD700", "#EE6300", "brown")
    } else {
      names <- c("Qd", "Qr")
      colors <- c("#FFD700", "#EE6300")
    }

    op <- getPlotPar()$par
    dg3 <- dygraphs::dygraph(data.xts, group = "state_var", ylab = paste0("flow [mm/", getPrep()$TMGR$TimeUnit, "]"), main = " ")
    dg3 <- dygraphs::dyOptions(dg3,
      fillAlpha = 1.0,
      axisLineColor = op$fg, axisLabelColor = op$fg, titleHeight = 10,
      retainDateWindow = FALSE, useDataTimezone = TRUE
    )
    dg3 <- dygraphs::dyStackedRibbonGroup(dg3,
      name = names,
      color = colors, strokeBorderColor = "black"
    )
    dg3 <- dygraphs::dySeries(dg3, name = "Qobs", fillGraph = FALSE, drawPoints = TRUE, color = op$fg)
    dg3 <- dygraphs::dySeries(dg3, name = "Qsim", fillGraph = FALSE, color = "orangered")
    if (length(getSim()$SIMold) == 2 & input$ShowOldQsim == "Yes") {
      dg3 <- dygraphs::dySeries(dg3, name = "QsimOld", label = "Qold", fillGraph = FALSE, color = "grey", strokePattern = "dashed")
    }
    dg3 <- dygraphs::dyCrosshair(dg3, direction = "vertical")
    dg3 <- dygraphs::dyLegend(dg3, show = "always", width = 325)
  })


  ## Plot model diagram precipitation
  output$dyPlotMDp <- dygraphs::renderDygraph({
    if (length(getSim()$SIM$OutputsModel$DatesR) < 2) {
      return(NULL)
    }
    data <- data.frame(
      DatesR = getSim()$SIM$OutputsModel$DatesR,
      precip. = getSim()$SIM$OutputsModel$Precip
    )
    # data <- getData()$Tab[, c("DatesR", "precip.")]
    data.xts <- xts::xts(data[, -1L, drop = FALSE], order.by = data$DatesR, tzone = "UTC")

    dg4 <- dygraphs::dygraph(data.xts, group = "mod_diag", ylab = paste0("precip. [mm/", getPrep()$TMGR$TimeUnit, "]"))
    dg4 <- dygraphs::dyOptions(dg4,
      colors = "#428BCA", drawXAxis = FALSE,
      retainDateWindow = FALSE, useDataTimezone = TRUE
    )
    dg4 <- dygraphs::dyBarSeries(dg4, name = "precip.")
    dg4 <- dygraphs::dyAxis(dg4, name = "y", valueRange = c(max(data.xts[, "precip."], na.rm = TRUE), -1e-3))
    dg4 <- dygraphs::dyEvent(dg4, input$Event, color = "orangered")
    dg4 <- dygraphs::dyLegend(dg4, show = "onmouseover", width = 225)
    dg4 <- dygraphs::dyCrosshair(dg4, direction = "vertical")
  })


  ## Plot model diagram ETP
  output$dyPlotMDe <- dygraphs::renderDygraph({
    if (length(getSim()$SIM$OutputsModel$DatesR) < 2) {
      return(NULL)
    }
    # data <- data.frame(DatesR = getSim()$SIM$OutputsModel$DatesR,
    #                    PET    = getSim()$SIM$OutputsModel$PotEvap)
    data <- getData()$Tab[, c("DatesR", "PET")]
    data.xts <- xts::xts(data[, -1L, drop = FALSE], order.by = data$DatesR, tzone = "UTC")

    op <- getPlotPar()$par
    dg5 <- dygraphs::dygraph(data.xts, group = "mod_diag", ylab = paste0("PET [mm/", getPrep()$TMGR$TimeUnit, "]"), main = " ")
    dg5 <- dygraphs::dyOptions(dg5,
      colors = "#A4C400", drawPoints = TRUE,
      strokeWidth = 0, pointSize = 2, drawXAxis = FALSE,
      axisLineColor = op$fg, axisLabelColor = op$fg, titleHeight = 10,
      retainDateWindow = FALSE, useDataTimezone = TRUE
    )
    dg5 <- dygraphs::dyEvent(dg5, input$Event, color = "orangered")
    dg5 <- dygraphs::dyLegend(dg5, show = "onmouseover", width = 225)
    dg5 <- dygraphs::dyCrosshair(dg5, direction = "vertical")
  })


  ## Plot model diagram flow
  output$dyPlotMDq <- dygraphs::renderDygraph({
    if (length(getSim()$SIM$OutputsModel$DatesR) < 2) {
      return(NULL)
    }
    # if (length(getSim()$SIMold) == 2 & input$ShowOldQsim == "Yes") {
    #   QsimOld <- getSim()$SIMold[[1]]$Qsim
    # } else {
    #   QsimOld <- NA
    # }
    # OutputsModel <- getSim()$SIM$OutputsModel
    # IndPlot <- which(OutputsModel$DatesR >= input$Period[1L] & OutputsModel$DatesR <= input$Period[2L])
    # OutputsModel2 <- sapply(OutputsModel[seq_len(which(names(OutputsModel) == "Qsim"))], function(x) x[IndPlot])
    # OutputsModel2 <- c(OutputsModel2, Qobs = list(getSim()$SIM$Qobs[IndPlot]))
    # OutputsModel2$Qsim <- ifelse(format(OutputsModel2$DatesR, "%Y%m%d") > format(input$Event, "%Y%m%d"), NA, OutputsModel2$Qsim)
    # OutputsModel2$Qold <- ifelse(format(OutputsModel2$DatesR, "%Y%m%d") > format(input$Event, "%Y%m%d"), NA, QsimOld[IndPlot])
    #
    # data <- data.frame(DatesR  = OutputsModel2$DatesR,
    #                    Qobs    = OutputsModel2$Qobs,
    #                    Qsim    = OutputsModel2$Qsim,
    #                    QsimOld = OutputsModel2$Qold)
    data <- getData()$Tab[, c("DatesR", "Qobs", "Qsim", "QsimOld")]
    data$Qsim <- ifelse(format(data$DatesR, "%Y%m%d") > format(input$Event, "%Y%m%d"), NA, data$Qsim)
    data$QsimOld <- ifelse(format(data$DatesR, "%Y%m%d") > format(input$Event, "%Y%m%d"), NA, data$QsimOld)
    data.xts <- xts::xts(data[, -1L, drop = FALSE], order.by = data$DatesR, tzone = "UTC")

    op <- getPlotPar()$par
    dg6 <- dygraphs::dygraph(data.xts, group = "mod_diag", ylab = paste0("flow [mm/", getPrep()$TMGR$TimeUnit, "]"), main = " ")
    dg6 <- dygraphs::dyOptions(dg6,
      colors = c(op$fg, "orangered", "grey"), drawPoints = TRUE,
      axisLineColor = op$fg, axisLabelColor = op$fg, titleHeight = 10,
      retainDateWindow = FALSE, useDataTimezone = TRUE
    )
    dg6 <- dygraphs::dySeries(dg6, name = "Qsim", drawPoints = FALSE)
    dg6 <- dygraphs::dyEvent(dg6, input$Event, color = "orangered")
    dg6 <- dygraphs::dySeries(dg6, name = "QsimOld", label = "Qold", drawPoints = FALSE, strokePattern = "dashed")
    dg6 <- dygraphs::dyLegend(dg6, show = "onmouseover", width = 225)
    dg6 <- dygraphs::dyCrosshair(dg6, direction = "vertical")
  })


  ## Plot model diagram chart
  output$stPlotMD <- renderPlot(
    {
      if (length(getSim()$SIM$OutputsModel$DatesR) < 2) {
        return(NULL)
      }
      # OutputsModel <- getSim()$SIM$OutputsModel
      # IndPlot <- which(OutputsModel$DatesR >= input$Period[1L] & OutputsModel$DatesR <= input$Period[2L])
      # OutputsModel2 <- sapply(OutputsModel[seq_len(which(names(OutputsModel) == "Qsim"))], function(x) x[IndPlot])
      # OutputsModel2 <- c(OutputsModel2, Qobs = list(getSim()$SIM$Qobs[IndPlot]))

      # OutputsModel2 <- getData()$OutputsModel

      par(getPlotPar()$par)
      try(.DiagramGR(
        OutputsModel = getData()$OutputsModel, Param = getSim()$PARAM,
        SimPer = input$Period, EventDate = input$Event,
        HydroModel = input$HydroModel, CemaNeige = input$SnowModel == "CemaNeige"
      ),
      silent = TRUE
      )
    },
    bg = "transparent"
  )



  ## --------------- Criteria table

  output$Criteria <- renderTable(
    {

      ## Table created in order to choose order the criteria in the table output
      tabCrit_gauge <- data.frame(
        Criterion = c(
          "NSE [Q]", "NSE [sqrt(Q)]", "NSE [1/Q]",
          "KGE [Q]", "KGE [sqrt(Q)]", "KGE [1/Q]",
          "BIAS [Qsim/Qobs]"
        ),
        ID = 1:7, stringsAsFactors = FALSE
      )

      if (length(getSim()$SIMold) == 2 & input$ShowOldQsim == "Yes") {
        tabCrit_old <- getSim()$SIMold[[1]]$Crit$Value
        tabCrit_val <- cbind(getSim()$Crit, tabCrit_old)
        colnames(tabCrit_val) <- c(colnames(getSim()$Crit), "Qold")
        CellColHisto <- '<div style="color: #808080;"><span>9999</span></div>'
      } else {
        tabCrit_val <- getSim()$Crit
      }
      tabCrit_out <- merge(tabCrit_gauge, tabCrit_val, by = "Criterion", all.x = TRUE)
      tabCrit_out <- tabCrit_out[order(tabCrit_out$ID), ]
      tabCrit_out <- tabCrit_out[, !colnames(tabCrit_out) %in% "ID"]
      tabCrit_out[tabCrit_out <= -99.99] <- -99.99
      tabCrit_out[, seq_len(ncol(tabCrit_out))[-1]] <- sapply(seq_len(ncol(tabCrit_out))[-1], function(x) sprintf("%7.2f", tabCrit_out[, x]))
      tabCrit_out <- as.data.frame(tabCrit_out)
      tabCrit_out[tabCrit_out == " -99.99"] <- "< - 99.99"
      colnames(tabCrit_out) <- gsub("Value", "Qsim", colnames(tabCrit_out))

      ## Color the cell of the crietaia uses during the calibration
      if (CAL_click$valueButton >= 0) {
        CellColCalib <- '<div style="color: #FFFFFF; background-color: #A4C400; border: 5px solid #A4C400; position:relative; top: 0px; left: 5px; padding: 0px; margin: -5px -0px -8px -10px;">
<span>9999</span></div>'
        CellColCalib_id <- which(tabCrit_out$Criterion == input$TypeCrit)
        tabCrit_out[CellColCalib_id, 2] <- gsub("9999", tabCrit_out[CellColCalib_id, 2], CellColCalib)
      }
      if (input$ShowOldQsim == "Yes" & length(getSim()$SIMold) > 1) {
        tabCrit_out[, "Qold"] <- apply(tabCrit_out[, "Qold", drop = FALSE], 1, function(x) gsub("9999", x, CellColHisto))
      }

      return(tabCrit_out)
    },
    align = c("r"),
    sanitize.text.function = function(x) x
  )



  ## --------------- Download buttons

  ## Download simulation table
  output$DownloadTab <- downloadHandler(
    filename = function() {
      filename <- "TabSim"
      filename <- sprintf("airGR_%s_%s.csv", filename, gsub("(.*)( )(\\d{2})(:)(\\d{2})(:)(\\d{2})", "\\1_\\3h\\5m\\7s", Sys.time()))
    },
    content = function(file) {
      PREP <- getPrep()$PREP
      SIM <- getSim()$SIM
      if (input$SnowModel != "CemaNeige") {
        PrecipSim <- NA
        FracSolid <- NA
        TempMean <- NA
      } else {
        PrecipSol <- rowMeans(as.data.frame(PREP$InputsModel$LayerPrecip) * as.data.frame(PREP$InputsModel$LayerFracSolidPrecip), na.rm = TRUE)
        PrecipSim <- rowMeans(as.data.frame(PREP$InputsModel$LayerPrecip), na.rm = TRUE)
        FracSolid <- PrecipSol / PrecipSim
        FracSolid <- ifelse(is.na(FracSolid) & PrecipSol == 0 & PrecipSim == 0, 0, FracSolid)
        PrecipSim <- PrecipSim[SIM$OptionsSimul$IndPeriod_Run]
        FracSolid <- FracSolid[SIM$OptionsSimul$IndPeriod_Run]
        FracSolid <- round(FracSolid, digits = 3)
        TempMean <- rowMeans(as.data.frame(PREP$InputsModel$LayerTempMean), na.rm = TRUE)
        TempMean <- TempMean[SIM$OptionsSimul$IndPeriod_Run]
      }
      TabSim <- data.frame(
        Dates = SIM$OutputsModel$DatesR,
        PotEvap = SIM$OutputsModel$PotEvap,
        PrecipObs = SIM$OutputsModel$Precip,
        PrecipSim_CemaNeige = PrecipSim,
        PrecipFracSolid_CemaNeige = FracSolid,
        TempMeanSim_CemaNeige = TempMean,
        Qobs = SIM$OptionsCrit$Obs,
        Qsim = SIM$OutputsModel$Qsim
      )
      colnames(TabSim) <- sprintf("%s [%s]", colnames(TabSim), c("-", rep("mm", 3), "-", "°C", rep("mm", 2)))
      colnames(TabSim) <- ifelse(grepl("mm", colnames(TabSim)),
        gsub("mm", paste0("mm/", .TypeModelGR(PREP)$TimeUnit), colnames(TabSim)),
        colnames(TabSim)
      )
      write.table(TabSim, file = file, row.names = FALSE, sep = ";")
    }
  )


  ## Download plots
  output$DownloadPlot <- downloadHandler(
    filename = function() {
      filename <- switch(input$PlotType,
        "Model performance" = "PlotModelPerf",
        "Flow time series"  = "PlotFlowTimeSeries",
        "State variables"   = "PlotStateVar",
        "Model diagram"     = "PlotModelDiag"
      )
      filename <- sprintf("airGR_%s_%s.png", filename, gsub("(.*)( )(\\d{2})(:)(\\d{2})(:)(\\d{2})", "\\1_\\3h\\5m\\7s", Sys.time()))
    },
    content = function(file) {
      k <- 1.75
      ParamTitle <- c("X1", "X2", "X3", "X4", "X5", "X6")[seq_len(getPrep()$TMGR$NbParam)]
      ParamUnits <- c("mm", "mm/%s", "mm", "%s", "", "mm")[seq_len(getPrep()$TMGR$NbParam)]
      if (input$SnowModel == "CemaNeige") {
        ParamTitle <- c(ParamTitle, "C1", "C2")
        ParamUnits <- c(ParamUnits, "", "mm/°C/%s")
      }
      ParamTitle <- paste(ParamTitle, paste(getSim()$PARAM, sprintf(ParamUnits, getPrep()$TMGR$TimeUnit)), sep = " = ", collapse = ", ")
      ParamTitle <- gsub(" ,", ",", ParamTitle)
      PngTitle <- sprintf(
        "%s - %s/%s\n%s\n%s", input$Dataset,
        input$HydroModel, ifelse(input$SnowModel == "CemaNeige", "CemaNeige", "No snow model"),
        paste0(input$Period, collapse = " - "),
        ParamTitle
      )
      if (getPlotType() == 1) {
        png(filename = file, width = 1000 * k, height = ifelse(input$SnowModel != "CemaNeige", 700 * k, 1100 * k), pointsize = 14, res = 150)
        par(oma = c(0, 0, 4, 0))
        plot(getSim()$SIM)
        mtext(text = PngTitle, side = 3, outer = TRUE, cex = 0.8, line = 1.2)
        dev.off()
      }
      if (getPlotType() == 2) {
        png(filename = file, width = 1000 * k, height = 600 * k, pointsize = 14, res = 150)
        par(oma = c(0, 0, 4, 0))
        plot(getSim()$SIM, which = c("Precip", "Flows"))
        mtext(text = PngTitle, side = 3, outer = TRUE, cex = 0.8, line = 1.2)
        dev.off()
      }
      if (getPlotType() == 3) {
        png(filename = file, width = 1000 * k, height = 600 * k, pointsize = 14, res = 150)
        # OutputsModel <- getSim()$SIM$OutputsModel
        # IndPlot <- which(OutputsModel$DatesR >= input$Period[1L] & OutputsModel$DatesR <= input$Period[2L])
        # OutputsModel2 <- sapply(OutputsModel[seq_len(which(names(OutputsModel) == "Qsim"))], function(x) x[IndPlot])
        # OutputsModel2 <- c(OutputsModel2, Qobs = list(getSim()$SIM$Qobs[IndPlot]))
        #
        # data <- data.frame(DatesR = OutputsModel2$DatesR,
        #                    prod.  = OutputsModel2$Prod,
        #                    rout.  = OutputsModel2$Rout,
        #                    Qr     = OutputsModel2$QR,
        #                    Qd     = OutputsModel2$QD,
        #                    Qsim   = OutputsModel2$Qsim,
        #                    Qobs   = OutputsModel2$Qobs)
        # if (input$HydroModel == "GR6J") {
        #   data$QrExp <- OutputsModel2$QRExp
        # } else {
        #   data$QrExp <- 0
        # }
        data <- getData()$Tab[, c("DatesR", "prod.", "rout.", "Qr", "Qd", grep("^QrExp|exp", colnames(getData()$Tab), value = TRUE), "Qsim", "Qobs")]
        par(mfrow = c(2, 1), oma = c(3, 0, 4, 0))
        par(mar = c(0.6, 4.0, 0.0, 2.0), xaxt = "n", cex = 0.8)
        if (input$HydroModel != "GR6J") {
          plot(range(data$Dates), range(data$prod., data$rout.),
            type = "n", xlab = "", ylab = "store [mm]"
          )
        } else {
          data$exp. <- rowSums(data[, c("exp. (+)", "exp. (-)")], na.rm = TRUE)
          plot(range(data$Dates), range(data$prod., data$rout., data$rout., data$exp.),
            type = "n", xlab = "", ylab = "store [mm]"
          )
        }
        polygon(c(data$Dates, rev(range(data$Dates))), c(data$prod., rep(0, 2)), border = "darkblue", col = adjustcolor("darkblue", alpha.f = 0.30))
        polygon(c(data$Dates, rev(range(data$Dates))), c(data$rout., rep(0, 2)), border = "cyan4", col = adjustcolor("cyan4", alpha.f = 0.30))
        if (input$HydroModel == "GR6J") {
          minQrExp <- min(data$prod., data$rout., data$exp., 0)
          colQrExp <- ifelse(minQrExp > 0, "#10B510", "#FF0303")
          polygon(c(data$Dates, rev(range(data$Dates))), c(data$exp., rep(0, 2)), border = colQrExp, col = adjustcolor(colQrExp, alpha.f = 0.30))
        }
        if (input$HydroModel != "GR6J") {
          legend("topright",
            bty = "n", legend = c("prod.", "rout."), cex = 0.8,
            pt.bg = adjustcolor(c("darkblue", "cyan4"), alpha.f = 0.30),
            col = c("darkblue", "cyan4"),
            pch = 22
          )
        } else {
          legend("topright",
            bty = "n", legend = c("prod.", "rout.", "exp. (+)", "exp. (-)"), cex = 0.8,
            pt.bg = adjustcolor(c("darkblue", "cyan4", "#10B510", "#FF0303"), alpha.f = 0.30),
            col = c("darkblue", "cyan4", "#10B510", "#FF0303"),
            pch = 22
          )
        }
        par(mar = c(0.0, 4.0, 0.6, 2.0), xaxt = "s")
        plot(data$DatesR, data$Qobs, type = "n", xlab = "", ylab = paste0("flow [mm/", getPrep()$TMGR$TimeUnit, "]"))
        if (input$HydroModel != "GR6J") {
          polygon(c(data$Dates, rev(range(data$Dates))), c(data$Qr + data$Qd, rep(0, 2)), col = "#FFD700", border = NA)
          polygon(c(data$Dates, rev(range(data$Dates))), c(data$Qr, rep(0, 2)), col = "#EE6300", border = NA)
          legend("topright",
            bty = "n", legend = c("Qobs", "Qsim", "Qr", "Qd"), cex = 0.8,
            col = c(par("fg"), "orangered", "#FFD700", "#EE6300"),
            lwd = c(1, 1, NA, NA), pch = c(20, NA, 15, 15)
          )
        } else {
          polygon(c(data$Dates, rev(range(data$Dates))), c(data$QrExp + data$Qr + data$Qd, rep(0, 2)), col = "#FFD700", border = NA)
          polygon(c(data$Dates, rev(range(data$Dates))), c(data$QrExp + data$Qr, rep(0, 2)), col = "#EE6300", border = NA)
          polygon(c(data$Dates, rev(range(data$Dates))), c(data$QrExp, rep(0, 2)), col = "brown", border = NA)
          legend("topright",
            bty = "n", legend = c("Qobs", "Qsim", "Qd", "Qr", "QrExp"), cex = 0.8,
            col = c(par("fg"), "orangered", "#FFD700", "#EE6300", "brown"),
            lwd = c(1, 1, NA, NA, NA), pch = c(20, NA, 15, 15, 15)
          )
        }
        lines(data$DatesR, data$Qsim, lwd = 1, col = "orangered")
        lines(data$DatesR, data$Qobs, lwd = 1, col = par("fg"), type = "o", pch = 20, cex = 0.5)
        mtext(text = PngTitle, side = 3, outer = TRUE, cex = 0.8, line = 0.7)
        box()
        dev.off()
      }
      if (getPlotType() == 4) {
        isCN <- input$SnowModel == "CemaNeige"
        png(filename = file, width = 550 * k, height = ifelse(isCN, 1000, 900) * k, pointsize = 12, res = 150)
        PngTitle2 <- gsub(", C1", "\nC1", PngTitle)
        par(oma = c(0, 0, ifelse(isCN, 7, 6), 0))
        .DiagramGR(
          OutputsModel = getData()$OutputsModel, Param = getSim()$PARAM,
          SimPer = input$Period, EventDate = input$Event,
          HydroModel = input$HydroModel, CemaNeige = input$SnowModel == "CemaNeige"
        )
        mtext(text = PngTitle2, side = 3, outer = TRUE, cex = 1.2, line = ifelse(isCN, -0.15, 0.6))
        dev.off()
      }
    }
  )
})
