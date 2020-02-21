library(airGRteaching)

## data.frame of observed data of a low-land basin
data(L0123001, package = "airGR")
BV_L0123001 <- BasinObs[0001:6000, c("DatesR", "P", "E", "Qmm", "T")]
BI_L0123001 <- BasinInfo
## data.frame of observed data of a mountainous basin
data(L0123002, package = "airGR")
BV_L0123002 <- BasinObs[5000:9999, c("DatesR", "P", "E", "Qmm", "T")]
BI_L0123002 <- BasinInfo


ObsDF <- list("Low-land basin" = BV_L0123001, "Mountainous basin" = BV_L0123002)
ZInputs <- list(NULL, median(BI_L0123002$HypsoData))
HypsoData <- list(NULL, BI_L0123002$HypsoData)
NLayers <- list(5, 5)
SimPer <- list(c("1994-01-01", "1998-12-31"), c("2004-01-01", "2006-12-31"))
theme <- "United"

DatesR <- NULL
Precip <- NULL
PotEvap <- NULL
Qobs <- NULL
TempMean <- NULL

lenObsDF <- length(ObsDF)
NamesObsBV <- names(ObsDF)

ZInputs <- as.list(ZInputs)
if (length(ZInputs) == lenObsDF) {
  ZInputs <- as.list(ZInputs)
} else if (length(ZInputs) > lenObsDF) {
  ZInputs <- as.list(ZInputs)[seq_along(ObsDF)]
  warning("Too long 'ZInputs'. Only the first element(s) of 'ZInputs' argument used.")
} else if (length(ZInputs) < lenObsDF) {
  ZInputs <- as.list(rep(ZInputs, lenObsDF))[seq_along(ObsDF)]
  if (lenObsDF > 1) {
    warning("Not enough 'ZInputs' elements. Elements of the list recycled.")
  }
}

names(ZInputs) <- NamesObsBV


if (!is.list(HypsoData)) {
  HypsoData <- list(HypsoData)
}
if (length(HypsoData) == lenObsDF) {
  HypsoData <- as.list(HypsoData)
} else if (length(HypsoData) > lenObsDF) {
  HypsoData <- as.list(HypsoData)[seq_along(ObsDF)]
  warning("Too long 'HypsoData'. Only the first element(s) of 'HypsoData' argument used.")
} else if (length(HypsoData) < lenObsDF) {
  HypsoData <- as.list(rep(HypsoData, lenObsDF))[seq_along(ObsDF)]
  if (lenObsDF > 1) {
    warning("Not enough 'HypsoData' elements. Elements of the list recycled.")
  }
}
names(HypsoData) <- NamesObsBV

if (is.null(NLayers)) {
  NLayers <- vector(mode = "list", length = lenObsDF)
} else {
  if (length(NLayers) == lenObsDF) {
    NLayers <- as.list(NLayers)
  } else if (length(NLayers) > lenObsDF) {
    NLayers <- as.list(NLayers)[seq_along(ObsDF)]
    warning("Too long 'NLayers'. Only the first element(s) of 'NLayers' argument used.")
  } else if (length(NLayers) < lenObsDF) {
    NLayers <- as.list(rep(NLayers, lenObsDF))[seq_along(ObsDF)]
    if (lenObsDF > 1) {
      warning("Not enough 'NLayers' elements. Elements of the list recycled.")
    }
  }
}
names(NLayers) <- NamesObsBV

if (length(SimPer) > lenObsDF) {
  SimPer <- as.list(SimPer)[seq_along(ObsDF)]
  warning("Too long 'SimPer'. Only the first element(s) of 'SimPer' argument used.")
} else if (length(SimPer) < lenObsDF) {
  SimPer <- as.list(rep(SimPer, lenObsDF))[seq_along(ObsDF)]
  if (lenObsDF > 1) {
    warning("Not enough 'SimPer' elements. Elements of the list recycled.")
  }
}
names(SimPer) <- NamesObsBV


ShinyGR.hist <- list(list()) # list(Param = list(), TypeModel = lsit(), Crit = list(), Qsim = list())
ShinyGR <- list(
  ObsDF = ObsDF, NamesObsBV = NamesObsBV,
  DatesR = DatesR, Precip = Precip, PotEvap = PotEvap, Qobs = Qobs, TempMean = TempMean,
  ZInputs = ZInputs, HypsoData = HypsoData, NLayers = NLayers, SimPer = SimPer,
  theme = theme
)
