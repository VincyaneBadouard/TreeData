#' General Errors Detection
#'
#' @param Data Dataset (data.frame or data.table)
#'
#' @param Vars Variables to check for missing data (character)
#' (Default = c("Plot", "Subplot", "Year", "TreeFieldNum", "IdTree",
#' "IdStem", "Diameter", "POM", "HOM", "TreeHeight", "StemHeight","XTreeUTM",
#'  "YTreeUTM", "Family", "Genus", "Species", "VernName"))
#'
#' @param MeasVars Measurement variables to check for missing data or 0
#'  (character) (Default = c("Diameter", "HOM", "TreeHeight", "StemHeight"))
#'
#' @param PlotPolygon Plot polygon with a crs (sf)
#'
#' @details Detect errors
#'   - Remove **duplicated rows**
#'   - Check **missing value** in
#'      X-YTreeUTM/PlotArea/Plot/Subplot/Year/TreeFieldNum/
#'      IdTree/IdStem/Diameter/POM/HOM/Family/Genus/Species/VernName
#'   - Check **missing value** (NA/0) in the measurement variables: "Diameter",
#'      "HOM", "TreeHeight", "StemHeight"
#'   - Check of the **unique association of the IdTree with plot, subplot**
#'      **and TreeFieldNum** (at the site scale)
#'   - Check **duplicated IdTree/IdStem** in a census (at the site scale)
#'   - Check **invariant coordinates per IdTree/IdStem**
#'   - Check for trees **outside the plot**
#'   - Check **fix Plot and Subplot number** across censuses
#'      (not implemented yet)
#'
#'
#' @return The input dataset (data.table) with a new *Comment* column with error
#'   type informations.
#'
#' @importFrom stats na.omit
#'
#' @export
#'
#' @examples
#' library(data.table)
#' data("TestData")
#'
#' Rslt <- GeneralErrorsDetection(TestData)
#'
GeneralErrorsDetection <- function(
    Data,
    Vars = c("Plot", "Subplot", "Year", "TreeFieldNum", "IdTree", "IdStem",
             "Diameter", "POM", "HOM", "TreeHeight", "StemHeight",
             "XTreeUTM", "YTreeUTM", "Family", "Genus", "Species", "VernName"),
    MeasVars = c("Diameter", "HOM", "TreeHeight", "StemHeight"),
    PlotPolygon
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

  # Vars
  if(!inherits(Vars, "character"))
    stop("Vars must be a character")

  if(!inherits(MeasVars, "character"))
    stop("MeasVars must be a character")



  # PlotPolygon
  if (!inherits(PlotPolygon, "sf"))
    stop("PlotPolygon must be a sf")

  if(terra::crs(PlotPolygon) == "")
    stop("PlotPolygon must have a crs") # needeed

  # ---------------------------------------------------------------------------------------------------------

  Data[, Subplot := as.character(Subplot)]

  #### Function ####

  # In data.table
  setDT(Data)

  # Check duplicate rows -------------------------------------------------------
  # if there are duplicate rows, delete them
  Data <- DetectDuplicatedRows(Data)

  # Missing values -------------------------------------------------------------
  Data <- DetectMissingValues(Data, Vars, MeasVars)

  # Check of the unique association of the IdTree/IdStem with Plot-Subplot-TreeFieldNum, at the site scale -------------------
  Data <- DetectDiffTreeIDAssociation(Data)

  # Check duplicated IdTree/IdStem in a census ---------------------------------
  Data <- DetectDuplicatedStemID(Data)

  # Check the stem nbr coherence -----------------------------------------------
  Data <- DetectStemNbrIncoherence(Data)

  # Check invariant coordinates per IdTree/IdStem ------------------------------
  Data <- DetectStemVariantCoord(Data)

  # Check for trees outside the subplot ----------------------------------------
  Data <- DetectTreesOut(Data, PlotPolygon)

  # Check fix Plot and Subplot number across censuses (TODO, Eliot has) --------------------------------------------------------------
  # alerte quand le nombre de sous-parcelles/parcelles varie selon les années


  return(Data)
}
