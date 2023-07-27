#' Plot life status correction result
#'
#' @param Data Dataset (data.frame or data.table)
#'   The dataset must contain the columns:
#'   - `IdTree` (character)
#'   - `Year` (numeric)
#'   - `LifeStatus` (logical or character)
#'   - `LifeStatus_TreeDataCor` (logical or character)
#'
#' @param OnlyCorrected TRUE: plot only corrected trees, FALSE: plot all trees
#'   (logical)
#'
#' @param SeveralWindows TRUE: return each page in a new window (better
#'   visualisation in Rstudio), FALSE: return each page in the same window
#'   (needed to save all the pages) (logical)
#'
#' @return The plots of the initial diameter values and proposed corrections, by
#'   IdStem/IdTree
#'
#' @importFrom ggplot2 ggplot geom_point geom_line aes theme_minimal
#'    scale_colour_manual labs vars
#' @importFrom ggforce facet_wrap_paginate
#' @importFrom grDevices dev.new
#'
#' @export
#'
#' @examples
#'
#'\dontrun{
#' pdf("LifeStatusCorrectionPlots.pdf", width = 25, height = 10)
#' LifeStatusCorrectionPlot(Rslt, SeveralWindows = FALSE)
#' dev.off()
#'}
#'
LifeStatusCorrectionPlot <- function(
  Data,
  OnlyCorrected = FALSE,
  # CorCol = "LifeStatus_TreeDataCor",
  # InitialCol = "LifeStatus",
  # FileName = "LifeStatusCorrectionPlots.pdf"
  SeveralWindows = TRUE
){

  #### Arguments check ####

  # Data
  if(!inherits(Data, c("data.table", "data.frame")))
    stop("Data must be a data.frame or data.table")

  # IdStem or IdTree? ---------------------------------------------------------------------------------------
  # If no IdStem take IdTree
  if((!"IdStem" %in% names(Data) | all(is.na(Data$IdStem))) &
     ("IdTree" %in% names(Data) & any(!is.na(Data$IdTree))) ){ ID <- "IdTree"

  }else{ ID <- "IdStem"}

  if(!any(c("IdStem", "IdTree") %in% names(Data)) | (all(is.na(Data$IdStem)) &  all(is.na(Data$IdTree))) )
    stop("The 'IdStem' or 'IdTree' column is missing in your dataset")
  # ---------------------------------------------------------------------------------------------------------


  # Columns
  # IdTree, Year, LifeStatus, LifeStatus_TreeDataCor
  if(!all(c("Year", "LifeStatus", "LifeStatus_TreeDataCor") %in% names(Data)))
    stop("'Year', 'LifeStatus', 'LifeStatus_TreeDataCor' should be columns of Data")


  #### Function ####

  Data[,LifeStatus_TreeDataCor := as.character(LifeStatus_TreeDataCor)]
  Data[,LifeStatus := as.character(LifeStatus)]
  Data[is.na(LifeStatus_TreeDataCor), LifeStatus_TreeDataCor := "NA"]
  Data[is.na(LifeStatus), LifeStatus := "NA"]

  # Order IDs and times in ascending order ----------------------------------------------------------------------------
  Data <- Data[, Year := as.numeric(Year)]
  Data <- Data[order(get(ID), Year)]

  if(OnlyCorrected == TRUE){
    # Only corrected stems ----------------------------------------------------------------------------------------------
    IDCor <- Data[LifeStatus != LifeStatus_TreeDataCor, get(ID)] #  corrected stems

    DataCor <- Data[get(ID) %in% IDCor] #  corrected stems

  }else{
    DataCor <- Data
    IDCor <- Data[, get(ID)]
  }


  # Define nrow and ncol for the facet
  n <- length(unique(IDCor))
  if(n<3) { i = 1
  }else{ i = 3}


  # Plot --------------------------------------------------------------------------------------------------------------
  # pdf(FileName, width = 25, height = 10)

  if(SeveralWindows == TRUE)
  dev.new()

  for(p in 1:(ceiling(length(unique(IDCor))/9))){
    print(ggplot(DataCor) +
      aes(x = Year) +

      # Initial
      geom_point(aes(y = LifeStatus,
                     color = ifelse(LifeStatus != LifeStatus_TreeDataCor, 'Initial', 'Conserved')),
                 shape = "circle", size = 3.9) +

      geom_line(aes(y = LifeStatus, color = ifelse(LifeStatus != LifeStatus_TreeDataCor, 'Initial', 'Conserved'))) +


      # Corrected
      geom_line(aes(y = LifeStatus_TreeDataCor, color = ifelse(LifeStatus != LifeStatus_TreeDataCor, 'Corrected', 'Conserved'))) +
      geom_point(aes(y = LifeStatus_TreeDataCor,
                     color = ifelse(LifeStatus != LifeStatus_TreeDataCor | is.na(LifeStatus), 'Corrected', 'Conserved')),
                 shape = "circle", size = 3.9) +


      # Colours
      scale_colour_manual(name = "Status", values = c("Conserved" = "black",
                                                      "Initial" = "red",
                                                      "Corrected" = "forestgreen")) +
      theme_minimal() +

      # Titles
      labs(
        # title =  paste("ID: ",unique(DataCor[,get(ID)],""),
        x = "Year", y = "LifeStatus") +

      ggforce::facet_wrap_paginate(vars(get(ID)), scales = "free", ncol = min(n,3), nrow = i, page = p)

    )

    if(SeveralWindows == TRUE & p < ceiling(length(unique(IDCor))/9))
      dev.new()


  }
  # dev.off()


  # return(Pl)

}
