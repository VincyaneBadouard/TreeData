#' Detect and Remove Duplicated Rows
#'
#' @param Data Dataset (data.frame or data.table)
#'
#' @return The input dataset (data.table) without the duplicated rows.
#'
#' @export
#'
#' @examples
#' library(data.table)
#' data("TestData")
#'
#' Rslt <- DetectDuplicatedRows(TestData)
#'
DetectDuplicatedRows <- function(
  Data
){

  #### Arguments check ####

  # Data
  if (!inherits(Data, c("data.table", "data.frame")))
    stop("Data must be a data.frame or data.table")

  #### Function ####

  # In data.table
  setDT(Data)

  # Check duplicate rows ------------------------------------------------------------------------------------
  # if there are duplicate rows, delete them

  if(anyDuplicated(Data) != 0)
    Data <- unique(Data)

  return(Data)
}
