#' Detect Missing Values
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
#' @details
#'   - Check **missing value** in
#'      X-YTreeUTM/PlotArea/Plot/Subplot/Year/TreeFieldNum/
#'      IdTree/IdStem/Diameter/POM/HOM/Family/Genus/Species/VernName
#'
#'   - Check **missing value** (NA/0) in the measurement variables: "Diameter",
#'      "HOM", "TreeHeight", "StemHeight"
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
#' Rslt <- DetectMissingValues(Data=TestData)
#'
DetectMissingValues <- function(
  Data,
  Vars = c("Plot", "Subplot", "Year", "TreeFieldNum", "IdTree", "IdStem",
            "Diameter", "POM", "HOM", "TreeHeight", "StemHeight",
            "XTreeUTM", "YTreeUTM", "Family", "Genus", "Species", "VernName"),
  MeasVars = c("Diameter", "HOM", "TreeHeight", "StemHeight")
){

  #### Arguments check ####

  # Data
  if(!inherits(Data, c("data.table", "data.frame")))
    stop("Data must be a data.frame or data.table")

  if(!inherits(Vars, "character"))
    stop("Vars must be a character")

  if(!inherits(MeasVars, "character"))
    stop("MeasVars must be a character")

  #### Function ####

  # In data.table
  setDT(Data)

  # Missing values ----------------------------------------------------------------------------------------------------
  # If the column exists, but have NA values

  # Check bota (?) : Family/Genus/Species/ScientificName/VernName

    # Vars <- c("Plot", "Subplot", "Year", "TreeFieldNum", "IdTree", "IdStem",
    #         "Diameter", "POM", "HOM", "TreeHeight", "StemHeight",
    #         "XTreeUTM", "YTreeUTM", "Family", "Genus", "Species", "VernName")

  valid_vars <- Vars[Vars %in% names(Data)] # only existing columns
  valid_vars <- valid_vars[!sapply(valid_vars, function(col) all(is.na(Data[[col]])))] # and not completely empty

  for (col in valid_vars) {
    Data <- GenerateComment(
      Data,
      condition = is.na(Data[[col]]),
      comment = paste0("Missing value in ", col)
    )
  }

  # Data[grepl("Missing value", Comment)] # to check


  # Measurement variables = 0 -----------------------------------------------------------------------------------------

  # MeasVars <- c("Diameter", "HOM", "TreeHeight", "StemHeight")

  valid_vars <- MeasVars[MeasVars %in% names(Data)] # only existing columns
  valid_vars <- valid_vars[!sapply(valid_vars, function(col) all(is.na(Data[[col]])))] # and not completely empty

  for (col in valid_vars) {
    Data <- GenerateComment(
      Data,
      condition = Data[,get(col) == 0],
      comment = paste0(col," cannot be 0")
    )
  }

  # Data[grepl("cannot be 0", Comment)] # to check

  return(Data)
}
