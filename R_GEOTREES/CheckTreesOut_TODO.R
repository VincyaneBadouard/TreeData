#' Check trees outside the subplot
#'
#' @param Data Dataset (data.frame or data.table)
#'
#' @details Check for trees **outside the subplot**
#'
#' @return The input dataset (data.table) with a new *Comment* column with error
#'   type informations.
#'
#' @export
#'
#' @examples
#' library(data.table)
#' data("TestData")
#'
#' Rslt <- CheckTreesOut(TestData)
#'
CheckTreesOut <- function(
  Data
){

  #### Arguments check ####

  # Data
  if (!inherits(Data, c("data.table", "data.frame")))
    stop("Data must be a data.frame or data.table")

  # IdStem or IdTree? ---------------------------------------------------------------------------------------
  # If no IdStem take IdTree
  if((!"IdStem" %in% names(Data) | all(is.na(Data$IdStem))) &
     ("IdTree" %in% names(Data) & any(!is.na(Data$IdTree))) ){
    ID <- "IdTree"
    Data[, IdTree := as.character(IdTree)]

  }else{ ID <- "IdStem"
  Data[, IdStem := as.character(IdStem)]
  }

  if(!any(c("IdStem", "IdTree") %in% names(Data)) | (all(is.na(Data$IdStem)) &  all(is.na(Data$IdTree))) )
    stop("The 'IdStem' or 'IdTree' column is missing in your dataset")
  # ---------------------------------------------------------------------------------------------------------

  Data[, Subplot := as.character(Subplot)]

  #### Function ####

  # In data.table
  setDT(Data)

  # TO DO
  # Check for trees outside the subplot (TODO) ---------------------------------------------------------------------
  # Check XY coordinates against plot dimensions
  # or Comparer PlotArea avec l'aire du MCP (Minimum Convex Polygon) des arbres a l'interieur de la parcelle.
  # Si aire du MCP > x% plotArea -> error

  return(Data)
}
