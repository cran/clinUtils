## ----optionsChunks, echo = FALSE, cache = FALSE-------------------------------

library(knitr)
tmpdir <- tempdir()

knitr::opts_chunk$set(
    message = FALSE,
    # stop document execution if error (not the default)
    error = FALSE, # stop-on-error
    fig.align = "center",
    fig.path = file.path(tmpdir, "./figures_vignette/"),
    echo = TRUE
)


## ----loadPackages-------------------------------------------------------------

library(clinUtils)

# packages required for the examples in the vignette
library(ggplot2)
library(pander)
library(htmltools)
library(plyr)


## ----loadData-----------------------------------------------------------------

pathExampleDatasets <- list.files(
    path = system.file("extdata", "cdiscpilot01", "SDTM", package = "clinUtils"), 
    pattern = "*.xpt", 
    full.names = TRUE
)

data <- loadDataADaMSDTM(files = pathExampleDatasets)
# A list is returned, each separated file is accessible via data$[fileName]
pander(head(data$DM, 2))
pander(head(data$LB, 2))
pander(head(data$AE, 2))

# Access labels for all variables
labelVars <- attr(data, "labelVars")
head(labelVars)
# Access label for a particular variable: 
labelVars["USUBJID"]


## ----exampleDataset-ADaM------------------------------------------------------

# load data
data(dataADaMCDISCP01)
dataADaM <- dataADaMCDISCP01
names(dataADaM)
pander(head(dataADaM$ADSL, 2))
pander(head(dataADaM$ADLBC, 2))
pander(head(dataADaM$ADAE, 2))

# and variable labels
labelVarsADaM <- attr(dataADaM, "labelVars")
head(labelVarsADaM)


## ----exampleDataset-SDTM------------------------------------------------------

# load data
data(dataSDTMCDISCP01)
dataSDTM <- dataSDTMCDISCP01
names(dataSDTM)
pander(head(dataSDTM$DM, 2))
pander(head(dataSDTM$LB, 2))
pander(head(dataSDTM$AE, 2))

# and variable labels
labelVarsSDTM <- attr(dataSDTM, "labelVars")
head(labelVarsSDTM)


## ----getLabelVar--------------------------------------------------------------

# variable label is extracted from 'labelVars'
getLabelVar(var = "AEDECOD", labelVars = labelVars)


## ----getLabelParamcd----------------------------------------------------------

# For ADaM dataset
getLabelParamcd(paramcd = "CHOL", data = dataADaM$ADLB)
getLabelParamcd(paramcd = "BILI", data = dataADaM$ADLB)

# For SDTM dataset
getLabelParamcd(paramcd = "CHOL", data = dataSDTM$LB, paramcdVar = "LBTESTCD", paramVar = "LBTEST")
getLabelParamcd(paramcd = "BILI", data = dataSDTM$LB, paramcdVar = "LBTESTCD", paramVar = "LBTEST")


## ----palette-show-------------------------------------------------------------

print(colorPaletteNRIND)
print(shapePaletteNRIND)


## ----palette-plot, fig.height = 8---------------------------------------------

plot(
    x = seq_along(colorPaletteNRIND),
    col = colorPaletteNRIND, 
    bg = colorPaletteNRIND, 
    pch = shapePaletteNRIND
)
text(
    x = seq_along(colorPaletteNRIND),
    labels = names(colorPaletteNRIND), pos = 3
)
title("Palette for CDISC normal reference range indicator")


## ----getPaletteCDISC----------------------------------------------------------

dataPlot <- subset(dataSDTM$LB, LBTEST == "Leukocytes")

colorPalette <- getPaletteCDISC(x = dataPlot$LBNRIND, var = "NRIND", type = "color")
print(colorPalette)
shapePalette <- getPaletteCDISC(x = dataPlot$LBNRIND, var = "NRIND", type = "shape")
print(shapePalette)

# visualize profile over time
gg <- ggplot(data = dataPlot) +
    geom_point(aes(x = LBDY, y = LBSTRESN, 
            color = LBNRIND, fill = LBNRIND, shape = LBNRIND)) +
    ggtitle("Evolution of Leukocytes actual value over time")
print(gg)

# use 'standard' symbols/colors
# ('limits' is only required if the categories are not already ordered in LBNRIND)
gg + 
    scale_color_manual(values = colorPalette, limits = names(colorPalette)) +
    scale_fill_manual(values = colorPalette, limits = names(colorPalette)) +
    scale_shape_manual(values = shapePalette, limits = names(colorPalette))


## ----palettes-----------------------------------------------------------------

dataPlot <- subset(dataADaM$ADLB, PARAMCD == "CHOL")

# extract palettes
colorPalette <- getColorPalette(x = dataPlot$USUBJID)
shapePalette <- getShapePalette(x = dataPlot$USUBJID)
linetypePalette <- getLinetypePalette(x = dataPlot$USUBJID)

# create the plot
ggplot(data = dataPlot, aes(x = ADY, y = CHG, color = USUBJID)) +
    geom_point(aes(shape = USUBJID)) +
    geom_line(aes(linetype = USUBJID, group = USUBJID)) +
    scale_color_manual(values = colorPalette) +
    scale_shape_manual(values = shapePalette) +
    scale_linetype_manual(values = linetypePalette) +
    labs(x = "Relative day", y = "Change from baseline",
        title = "Profile plot of cholesterol change from baseline") 


## ----roundHalfUp--------------------------------------------------------------
# round up
roundHalfUp(c(0.45, 0.55), 1)
# versus R default:
round(c(0.45, 0.55), 1)


## ----createDataAE-------------------------------------------------------------

dataTEAE <- subset(dataADaM$ADAE, SAFFL == "Y" & TRTEMFL == "Y")

# set column names to labels
labelVarsTEAE <- getLabelVar(
    var = colnames(dataTEAE), 
    labelVars = labelVarsADaM
)
colnamesTEAE <- setNames(names(labelVarsTEAE), labelVarsTEAE)

dataTEAE <- dataTEAE[order(dataTEAE$AESOC), ]


## ----getClinDT, eval = rmarkdown::pandoc_available()--------------------------

getClinDT(
    dataTEAE, 
    colnames = colnamesTEAE, 
    rowGroupVar = c("AESOC"),
    barVar = "AGE",
    barRange = c(0, 100),
    caption = "Listing of treatment-emergent adverse events on the safety analysis set"
)


## ----compareTables------------------------------------------------------------

# Build example dataset with treatment-emergent adverse events
# of multiple batches

varsListing <- c("USUBJID", "AESOC", "AEDECOD", "ASTDT", "AESEV", "AEOUT")
dataTEAEListing <- dataTEAE[, varsListing]

# simulate removal of observations in new batch
dataTEAENew <- dataTEAE[-sample.int(n = nrow(dataTEAEListing), size = 3), ]

# simulate addition of observations in new batch
dataTEAEOld <- dataTEAE[-sample.int(n = nrow(dataTEAEListing), size = 3), ]

# simulate change of observations		
dataTEAEOld[seq_len(2), "AESEV"] <- "SEVERE"

refVars <- c("USUBJID", "AESOC", "AEDECOD", "ASTDT")
tableComparison <- compareTables(
    newData = dataTEAENew, 
    oldData = dataTEAEOld, 
    referenceVars = refVars,
    changeableVars = setdiff(colnames(dataTEAEListing), refVars),
    # parameters passed to datatable
    colnames = setNames(names(labelVarsADaM), labelVarsADaM)
)


## ----compareTables-table-comparison-interactive, eval = rmarkdown::pandoc_available()----
tableComparison$`table-comparison-interactive`

## ----'lab-hist-static1', fig.cap='Barplot of chemistry measurements', fig.width=7, fig.height=4, results = 'asis', echo = FALSE----
cat("\n", paste(rep("#", titleLevel), collapse = ""), " Chemistry\n", sep = "")
print(xList[[1]])

## ----'lab-hist-static2', fig.cap='Barplot of hematology measurements', fig.width=3, fig.height=4, results = 'asis', echo = FALSE----
cat("\n", paste(rep("#", titleLevel), collapse = ""), " Hematology\n", sep = "")
print(xList[[2]])

## ----'lab-hist-static3', fig.cap='Barplot of urinalysis measurements', fig.width=5, fig.height=4, results = 'asis', echo = FALSE----
cat("\n", paste(rep("#", titleLevel), collapse = ""), " Urinalysis\n", sep = "")
print(xList[[3]])

## ----figure-static-knitPrintListPlots, out.width = "100%", warning = FALSE, results = "asis"----

dataLB <- subset(dataSDTM$LB, 
    LBTESTCD %in% c("ALB", "ALT", "CHOL", "HCT", "KETONES", "PH")
)
dataLB$ACTARM <- dataSDTM$DM$ACTARM[match(dataLB$USUBJID, dataSDTM$DM$USUBJID)]

# create plots:
listPlotsLB <- plyr::dlply(dataLB, "LBCAT", function(data)
      ggplot(data = data) +
          geom_histogram(aes(fill = LBNRIND, x = ACTARM), stat = "count", position = "dodge") +
          facet_wrap(~LBTEST) +
          theme(axis.text.x = element_text(angle = -45, hjust = 0))
)
# n2mfrow: extract default dimensions for a specified number of plots
figDim <- plyr::dlply(dataLB, "LBCAT", function(data) 
      n2mfrow(length(unique(data$LBTESTCD)))
)
knitPrintListPlots(
    plotsList = listPlotsLB, 
    generalLabel = "lab-hist-static",
    type = "ggplot2",
    # set caption for each figure
    fig.cap = paste("Barplot of", tolower(names(listPlotsLB)), "measurements"),
    # specify different dimensions
    fig.width = sapply(figDim, "[[", 1) * 2 + 1, # 3 in for each plot + 1 in for the legend
    fig.height = sapply(figDim, "[[", 2) * 2 + 2, # 3 in for each plot + 2 for x-axis labels
    # include title before each visualization
    titles = simpleCap(tolower(names(listPlotsLB))),
    titleLevel = 4
)


## ----figure-interactive-creation----------------------------------------------

library(plotly)
listPlotsInteractiveLB <- sapply(listPlotsLB, function(ggplot)
      ggplotly(ggplot) %>% partial_bundle()
    , simplify = FALSE)


## ----figure-interactive-tagList, warning = FALSE, results = "asis", eval = rmarkdown::pandoc_available()----

tagListArgs <- mapply(list,
    # section header
    lapply(names(listPlotsInteractiveLB), htmltools::h4),
    # interactive plots
    listPlotsInteractiveLB,
    SIMPLIFY = FALSE
)
tagListArgs <- unlist(tagListArgs, recursive = FALSE)
do.call(htmltools::tagList, tagListArgs)


## ----'lab-hist-interactive1', results = 'asis', echo = FALSE------------------
cat("\n", paste(rep("#", titleLevel), collapse = ""), " Chemistry\n", sep = "")
xList[[1]]

## ----'lab-hist-interactive2', results = 'asis', echo = FALSE------------------
cat("\n", paste(rep("#", titleLevel), collapse = ""), " Hematology\n", sep = "")
xList[[2]]

## ----'lab-hist-interactive3', results = 'asis', echo = FALSE------------------
cat("\n", paste(rep("#", titleLevel), collapse = ""), " Urinalysis\n", sep = "")
xList[[3]]

## ----figure-interactive-knitPrintListPlots, warning = FALSE, results = "asis", eval = rmarkdown::pandoc_available()----

knitPrintListPlots(
    plotsList = listPlotsInteractiveLB, 
    generalLabel = "lab-hist-interactive",
    type = "plotly",
    # include title before each visualization
    titles = simpleCap(tolower(names(listPlotsInteractiveLB))),
    titleLevel = 5
)


## ----'lab-listing-ft1', ft.align='center', results = 'asis', echo = FALSE-----
cat("\n", paste(rep("#", titleLevel), collapse = ""), " Chemistry\n", sep = "")
xList[[1]]

## ----'lab-listing-ft2', ft.align='right', results = 'asis', echo = FALSE------
cat("\n", paste(rep("#", titleLevel), collapse = ""), " Hematology\n", sep = "")
xList[[2]]

## ----'lab-listing-ft3', ft.align='left', results = 'asis', echo = FALSE-------
cat("\n", paste(rep("#", titleLevel), collapse = ""), " Urinalysis\n", sep = "")
xList[[3]]

## ----table-flextable-knitPrintListObjects, warning = FALSE, results = "asis"----

library(flextable)
listFtLB <- plyr::dlply(dataLB, "LBCAT", function(dataParcat){
      flextable::flextable(data = head(dataParcat))
    })

knitPrintListObjects(
    xList = listFtLB, 
    generalLabel = "lab-listing-ft",
    titles = simpleCap(tolower(names(listFtLB))),
    # different alignment for each table
    ft.align = c("center", "right", "left"),
    titleLevel = 4
)


## ----includeSessionInfo, echo = FALSE-----------------------------------------

pander(sessionInfo())


