#' Detect Stem Number Incoherence
#'
#' @param Data Dataset (data.frame or data.table)
#'
#' @details Check the stem number coherence: if trees have not a stem n°1
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
#' Rslt <- DetectStemNbrIncoherence(TestData)
#'
DetectStemNbrIncoherence <- function(
    Data
){

  #### Arguments check ####

  # Data
  if (!inherits(Data, c("data.table", "data.frame")))
    stop("Data must be a data.frame or data.table")

  # IdTree and Stem.nb? ---------------------------------------------------------------------------------------

  if(!any(c("IdTree", "Stem.nb") %in% names(Data)) | (all(is.na(Data$Stem.nb)) &  all(is.na(Data$IdTree))) )
    stop("The 'IdTree' or 'Stem.nb' column is missing in your dataset")
  # ---------------------------------------------------------------------------------------------------------

  #### Function ####

  # In data.table
  setDT(Data)

  # Check the stem nbr coherence -----------------------------------------------

  stems <- Data[!is.na(Data$IdTree) & Data$Stem.nb!=1,]$IdTree # stem id >1
  stem1 <- Data[Data$IdTree %in% stems & Data$Stem.nb==1,]$IdTree

  Data[IdTree %in% stems[!stems %in% stem1],
       Comment := paste0(Comment, paste0("Trees without stem 1"), sep ="/")]

  return(Data)
}
