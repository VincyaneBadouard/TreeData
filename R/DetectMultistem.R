#' DetectMultistem
#'
#' @param Data Dataset (data.frame or data.table) with these columns:
#' - `XTreeUTM` and `YTreeUTM`
#' - `Genus` and `Species` (character)
#' - `Year` (numeric)
#'
#' @details Detect multistem in trees inventory data from common coordinates
#' and botanical name.
#'
#' @return The input dataset (data.table) with a new *Comment* column with
#' information on the detection of potential multistem.
#'
#' @import data.table
#' @export
#'
#' @examples
#' Rslt <- DetectMultistem(Data)
#'
DetectMultistem <- function(Data){

  setDT(Data)

  # Check duplicated coordinates in a census to detect multistems --------------
  Data[, ScientificName:= paste(Genus, Species, sep = "_")]
  Data[, Coord:= paste(XTreeUTM, YTreeUTM, sep = "_")]

  if(!"Comment" %in% names(Data)) Data[, Comment := ""]

  DuplicatedID <- Data[duplicated(Data[, list(Coord, ScientificName, Year)]), list(Coord, ScientificName, Year)]

  if(nrow(DuplicatedID) > 0){

    DuplicatedID[, IDYear := paste(Coord, ScientificName, Year, sep = "/")] # code to detect

    Data[, IDYear := paste(Coord, ScientificName, Year, sep = "/")] # code to detect

    Data[IDYear %in% DuplicatedID[, IDYear],
         Comment := paste0(Comment, paste0("Potential multistem (same coordinates and species)"), sep ="/")]

    warning("Potential multistem")

    Data[, IDYear := NULL]

  } else message("No potential multistem detected")

  return(Data)

}
