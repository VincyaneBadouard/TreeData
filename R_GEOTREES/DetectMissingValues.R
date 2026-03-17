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
#' Rslt <- DetectMissingValues(TestData)
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
  if (!inherits(Data, c("data.table", "data.frame")))
    stop("Data must be a data.frame or data.table")

  #### Function ####

  # In data.table
  setDT(Data)

  # Missing values ----------------------------------------------------------------------------------------------------
  # If the column exists, but have NA values

  # Check bota (?) : Family/Genus/Species/ScientificName/VernName

    # Vars <- c("Plot", "Subplot", "Year", "TreeFieldNum", "IdTree", "IdStem",
    #         "Diameter", "POM", "HOM", "TreeHeight", "StemHeight",
    #         "XTreeUTM", "YTreeUTM", "Family", "Genus", "Species", "VernName")

  for (v in 1:length(Vars)) {

    if(Vars[v] %in% names(Data)){ # If the column exists
      if(!all(is.na(Data[,get(Vars[v])]))){ # if the column is not completely empty

        Data <- GenerateComment(Data,
                                condition = is.na(Data[,get(Vars[v])]),
                                comment = paste0("Missing value in ", Vars[v]))

        # warning(paste0("Missing value in ", Vars[v]))

      } # not empty column
    } # column exists
  } # Vars loop

  # Data[grepl("Missing value", Comment)] # to check


  # Measurement variables = 0 -----------------------------------------------------------------------------------------

  # MeasVars <- c("Diameter", "HOM", "TreeHeight", "StemHeight")

  for (v in 1:length(MeasVars)) {
    if(MeasVars[v] %in% names(Data)){ # If the column exists

      Data <- GenerateComment(Data,
                              condition = Data[,get(MeasVars[v])] == 0,
                              comment = paste0(MeasVars[v]," cannot be 0"))

      # warning(paste0(MeasVars[v]," cannot be 0"))
    }
  }

  # Data[grepl("cannot be 0", Comment)] # to check

  return(Data)
}
