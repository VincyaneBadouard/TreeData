#' Detect Stem Variant Coordinates
#'
#' @param Data Dataset (data.frame or data.table)
#'
#' @details Check invariant coordinates per IdTree/IdStem
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
#' Rslt <- DetectStemVariantCoord(TestData)
#'
DetectStemVariantCoord <- function(
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

  # Check invariant coordinates per IdTree/IdStem ---------------------------------------------------------------------

  duplicated_ID <- CorresIDs <- vector("character")

  # For each site
  for (s in unique(na.omit(Data$Site))) {

    CoordIDCombination <- na.omit(unique(
      Data[Data$Site == s, c(ID, "XTreeUTM", "YTreeUTM"), with = FALSE]
    ))

    CorresIDs <- CoordIDCombination[, get(ID)] # .(IdTree) all the Idtree's having a unique X-YTreeUTM) combination

    if(!identical(CorresIDs, unique(CorresIDs))){ # check if it's the same length, same ids -> 1 asso/ID

      duplicated_ID <- unique(CorresIDs[duplicated(CorresIDs)]) # identify the Idtree(s) having several P-SubP-TreeFieldNum combinations

      Data <- GenerateComment(Data,
                              condition =
                                Data[,Site] == s
                              & Data[,get(ID)] %in% duplicated_ID,
                              comment = paste0("Different coordinates (XTreeUTM, YTreeUTM) for a same '", ID,"'"))

      warning(paste0("Different coordinates (XTreeUTM, YTreeUTM) for a same '", ID,"' (",duplicated_ID,")"))

    }
  } # end site loop

  # unique(Data[IdTree %in% duplicated_ID,
  #             .(IdTree = sort(IdTree), XTreeUTM, YTreeUTM, Comment)]) # to check

  return(Data)
}
