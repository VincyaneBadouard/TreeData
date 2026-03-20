#' Recruitment correction result
#'
#' @param Data Dataset (data.frame or data.table)
#'   The dataset must contain the columns:
#'   - `IdStem` (character)
#'   - `Year` (numeric)
#'   - `Diameter_TreeDataCor` (numeric)
#'
#' @param OnlyCorrected TRUE: plot only corrected stems, FALSE: plot all stems
#'   (logical)
#'
#' @param SeveralWindows TRUE: return each page in a new window (better
#'   visualisation in Rstudio), FALSE: return each page in the same window
#'   (needed to save all the pages) (logical)
#'
#' @param CorCol Diameter coorrected column name (character)
#'
#' @return The plots of the initial measured stem and proposed forgotten
#' recruits, by IdStem.
#'
#' @importFrom ggplot2 ggplot geom_point geom_line aes theme_minimal
#'   position_nudge scale_colour_manual labs vars
#' @importFrom ggforce facet_wrap_paginate
#' @importFrom ggrepel geom_text_repel
#' @importFrom grDevices dev.new
#'
#' @export
#'
#' @examples
#'
#'\dontrun{
#' pdf("RecruitmentCorrectionPlots_TestData.pdf", width = 25, height = 10)
#' RecruitmentCorrectionPlot(Rslt, OnlyCorrected = TRUE, SeveralWindows = FALSE)
#' dev.off()
#'}
#'
RecruitmentCorrectionPlot <- function(
    Data,
    OnlyCorrected = FALSE,
    CorCol = "Diameter_TreeDataCor",
    # InitialCol = "Diameter"
    SeveralWindows = TRUE
){

  #### Arguments check ####

  # Data ---------------------------------------------------------------------------------------------------------------
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

  # Columns --------------------------------------------------------------------------------------------------------------
  # IdStem, Year, Diameter, CorrectedRecruit, Diameter_TreeDataCor
  if(!"Year" %in% names(Data))
    stop(paste0("'Year' should be a column of Data"))

  if(!"CorrectedRecruit" %in% names(Data))
    stop(paste0("'CorrectedRecruit' should be a column of Data"))

  if(!CorCol %in% names(Data))
    stop(paste0(CorCol," should be a column of Data"))

  #### Function ####

  # Order IDs and times in ascending order ----------------------------------------------------------------------------
  Data <- Data[order(get(ID), Year)]

  if(OnlyCorrected == TRUE){
    # Only corrected stems ----------------------------------------------------------------------------------------------
    IDCor <- unique(Data[CorrectedRecruit==T, get(ID)]) #  corrected recruits IDstem/tree

    DataCor <- Data[get(ID) %in% IDCor] #  corrected recruits data

  }else{
    DataCor <- Data
    IDCor <- Data[, get(ID)]
  }


  # Define nrow and ncol for the facet
  n <- length(unique(IDCor))
  if(n<3) { i = 1
  }else{ i = 3}

  # Plot --------------------------------------------------------------------------------------------------------------

  if(SeveralWindows == TRUE)
    dev.new()

  for(p in seq_len(ceiling(length(unique(IDCor))/9))){
    print(ggplot(DataCor) +
            aes(x = Year) +

            # Corrected recruit trees
            geom_line(data = subset(DataCor, !is.na(get(CorCol))),
                      aes(y = get(CorCol)), colour = "black") +

            geom_line(data = subset(DataCor, !is.na(get(CorCol))),
                      aes(y = get(CorCol),
                          color = ifelse(CorrectedRecruit==T, 'Corrected recruit', 'Initial'))) +
            geom_point(data = subset(DataCor, !is.na(get(CorCol))),
                       aes(y = get(CorCol),
                           color = ifelse(CorrectedRecruit==T, 'Corrected recruit', 'Initial')),
                       shape = "circle", size = 3.9) +

            # Colours
            scale_colour_manual(name = "Status", values = c("Initial" = "black",
                                                            "Corrected recruit" = "#00AFBB")) +
            theme_minimal() +

            # Titles
            labs(
              # title =  paste("ID: ",unique(DataCor[, get(ID)]),""),
              x = "Year", y = "Diameter (cm)") +


            ggforce::facet_wrap_paginate(vars(get(ID), ScientificName),
                                         scales = "free",
                                         ncol = min(n,3), nrow = i, page = p)
    )

    if(SeveralWindows == TRUE & p < ceiling(length(unique(IDCor))/9))
      dev.new()
  }

}

