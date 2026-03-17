#' Flag botanical errors
#'
#' @param Data Dataset (data.frame or data.table)
#'   The dataset must contain the columns:
#'   - `IdTree` (character)
#'   - `Family` (character)
#'   - `Genus` (character)
#'   - `Species` (character)
#'   - `VernName` (character)
#'   - `ScientificName` (character)
#'
#' @return Fill the *Comment* column with error type informations.
#'
#'@details
#' - Check for special characters (typography)
#' - Check for family name in the Genus and Species columns (the suffix "aceae" is
#'     specific to the family name.
#' - Check if the family name does not end in 'aceae'.
#' - Check **invariant botanical informations per IdTree** (1 IdTree = 1 family,
#'     1 scientific and 1 vernacular name)
#'
#'@importFrom stats na.omit
#'
#' @export
#'
#' @examples
#' library(data.table)
#' data(TestData)
#'
#'# With The Plant List:
#' Rslt <- FlagBotanicalErrors(TestData)
#'
BotanicalCorrection <- function(
  Data
){

  #### Arguments check ####
  # Data
  if (!inherits(Data, c("data.table", "data.frame")))
    stop("Data must be a data.frame or data.table")

  #### Function ####

  setDT(Data) # data.frame to data.table

  Data[, IdTree := as.character(IdTree)]


  # Missing value ---------------------------------------------------------------------------------------------------------
  # Family, ScientificName/Genus, species, VernName

  Vars <- c("Family", "ScientificName", "Genus", "Species", "VernName")

  for (v in 1:length(Vars)) {

    if(Vars[v] %in% names(Data)){ # If the column exists

      Data <- GenerateComment(Data,
                              condition = is.na(Data[,get(Vars[v])]),
                              comment = paste0("Missing value in ", Vars[v]))
    }
  }

  # Data[Comment != ""] # to check

  # Comment :
  Data <- GenerateComment(Data,
                          condition = grepl("aceae", Data$Genus) | grepl("aceae", Data$Species),
                          comment = "Names ending in 'aceae' cannot be genus or species names")

  Data <- GenerateComment(Data,
                          condition = !grepl("aceae", Data$Family) & !is.na(Data$Family) & !grepl("Indet", Data$Family),
                          comment = "The family name does not end in 'aceae'")

  Data <- GenerateComment(Data,
                          condition = grepl('[[:punct:]]', Data$Genus), # TRUE if there are any special character
                          comment = "Special characters in the 'Genus'")

  Data <- GenerateComment(Data,
                          condition = grepl('[[:punct:]]', Data$Family), # TRUE if there are any special character
                          comment = "Special characters in the 'Family'")


  # Check invariant botanical informations per IdTree -------------------------------------------------------------------
  # Family, Genus, Species, Subspecies, VernName

  if(!"Subspecies" %in% names(Data)) Data[, Subspecies := NA_character_]
  if(!"VernName" %in% names(Data)) Data[, VernName := NA_character_]

  duplicated_ID <- CorresIDs <- vector("character")

  vars <- c("IdTree", "Family", "Genus", "Species", "Subspecies", "VernName")


  # For each site
  for (s in unique(na.omit(Data$Site))) {

    BotaIDCombination <- unique(
      Data[Data$Site == s, vars, with = FALSE]
    )

    CorresIDs <- BotaIDCombination[, IdTree] # .(IdTree)

    if(!identical(CorresIDs, unique(CorresIDs))){ # check if it's the same length, same ids -> 1 asso/ID

      duplicated_ID <- unique(CorresIDs[duplicated(CorresIDs)]) # identify the Idtree(s) having several bota combinations

      Data <- GenerateComment(Data,
                              condition =
                                Data[,Site] == s
                              & Data[,IdTree] %in% duplicated_ID,
                              comment = "Different botanical informations (Family, ScientificName, Subspecies, or VernName) for a same IdTree")
    }
  } # end site loop

  # unique(Data[IdTree %in% duplicated_ID,
  #             .(IdTree = sort(IdTree), Family, Genus, Species, Subspecies, VernName)]) # to check


  return(Data)

}
