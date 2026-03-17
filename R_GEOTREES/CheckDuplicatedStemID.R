#' Check Duplicated Stem ID
#'
#' @param Data Dataset (data.frame or data.table)
#'
#' @details Check **duplicated IdTree/IdStem** in a census (at the site scale)
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
#' Rslt <- CheckDuplicatedStemID(TestData)
#'
CheckDuplicatedStemID <- function(
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

  # Check duplicated IdTree/IdStem in a census ------------------------------------------------------------------------
  DuplicatedID <- Data[duplicated(Data[, list(get(ID), Year)]), list(get(ID), Year)]

  if(nrow(DuplicatedID) > 0){

    DuplicatedID[, IDYear := paste(V1, Year, sep = "/")] # code to detect

    Data[, IDYear := paste(get(ID), Year, sep = "/")] # code to detect

    Data <- GenerateComment(Data,
                            condition = Data$IDYear %in% DuplicatedID[, IDYear],
                            comment = paste0("Duplicated '", ID, "' in the census"))

    a <- Data[IDYear %in% DuplicatedID[, IDYear], .(Year, Plot, Subplot, TreeFieldNum, get(ID))]
    setnames(a, "V5", ID)
    a <- a[order(get(ID), Year)]
    b <- capture.output(a)
    c <- paste(b, "\n", sep = "")

    warning("Duplicated '", ID, "' in the census:\n", c, "\n")
    warning("If these duplicates are normal in your protocol (several measurements per year), you can leave your dataset like that,
the corrections taking into account only 1 measurement per year will consider the data to correct according to the 'KeepMeas' argument.
If these duplicates are abnormal according to your protocol, we advise you to treat them before applying the corrections.")

    Data[, IDYear := NULL]
  }

  # Data[grepl("Duplicated", Comment)] # to check

  return(Data)
}
