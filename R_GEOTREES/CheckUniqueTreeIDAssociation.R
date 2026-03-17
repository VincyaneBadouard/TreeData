#' Check Unique Tree ID Association
#'
#' @param Data Dataset (data.frame or data.table)
#'
#' @details Check of the **unique association of the IdTree with plot, subplot**
#'      **and TreeFieldNum** (at the site scale)
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
#' Rslt <- CheckUniqueTreeIDAssociation(TestData)
#'
CheckUniqueTreeIDAssociation <- function(
  Data
){

  #### Arguments check ####

  # Data
  if (!inherits(Data, c("data.table", "data.frame")))
    stop("Data must be a data.frame or data.table")

  if(!"IdTree" %in% names(Data) | all(is.na(Data$IdTree)) )
    stop("The 'IdTree' column is missing in your dataset")
  # ---------------------------------------------------------------------------------------------------------

  Data[, Subplot := as.character(Subplot)]

  #### Function ####

  # In data.table
  setDT(Data)

  # Check of the unique association of the IdTree/IdStem with Plot-Subplot-TreeFieldNum, at the site scale -------------------

  duplicated_ID <- CorresIDs <- vector("character")

  # For each site
  for (s in unique(na.omit(Data$Site))) {

    correspondances <- na.omit(unique(
      Data[Data$Site == s, .(IdTree, Plot, Subplot, TreeFieldNum)]
    ))

    CorresIDs <- correspondances[, IdTree] # .(IdTree) all the Idtree's having a unique P-SubP-TreeFieldNum combination

    if(!identical(CorresIDs, unique(CorresIDs))){ # check if it's the same length, same ids -> 1 asso/ID

      duplicated_ID <- unique(CorresIDs[duplicated(CorresIDs)]) # identify the Idtree(s) having several P-SubP-TreeFieldNum combinations

      Data <- GenerateComment(Data,
                              condition =
                                Data[,Site] == s
                              & Data[,IdTree] %in% duplicated_ID,
                              comment = "Non-unique association of the IdTree with Plot, Subplot and TreeFieldNum")

      DuplicatedID <- unique(Data[IdTree %in% duplicated_ID,
                                  .(IdTree, Plot, Subplot, TreeFieldNum)])
      DuplicatedID <- DuplicatedID[order(IdTree)]

      b <- capture.output(DuplicatedID)
      c <- paste(b, "\n", sep = "")

      warning("Non-unique association of IdTree(s) with Plot, Subplot and TreeFieldNum:\n", c, "\n")

    }
  } # end site loop

  # unique(Data[IdTree %in% duplicated_ID,
  #             .(IdTree = sort(IdTree), Plot, Subplot, TreeFieldNum, Comment)]) # to check

  return(Data)
}
