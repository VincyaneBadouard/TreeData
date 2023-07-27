#' RecruitmentCorrection
#'
#' @param Data  (data.frame or data.table)
#' The dataset should preferably contain the column of corrected diameters:
#' 'DBH_TreeDataCor', otherwise the function will take the column 'Diameter'
#'
#' @param KeepMeas In case of **multiple diameter measurements** in the same
#'   census year, on which to apply the correction:
#' Possible values: "MaxHOM", "MaxDate" (character).
#'   - "MaxHOM": apply the correction to the measurement taken at the
#'               **highest POM**
#'   - "MaxDate": apply the correction to the **most recent measurement** (same
#'                year but more recent date)
#'
#' @param MinDBH Minimum diameter of trees inventoried according to your
#'   protocol (in cm) (numeric, 1 value) (Default = 10 cm)
#'
#' @param PositiveGrowthThreshold A tree widening by more than x cm/year is
#'   considered abnormal (numeric, 1 value) (Default = 5 cm)
#'
#' @param InvariantColumns Vector with the names of the columns that are
#'   supposed to have always the same value for each measurement of the same
#'   tree (character)
#'
#' @param DetectOnly TRUE: Only detect errors, FALSE: detect and correct errors
#'   (logical)
#'
#' @details If the size of the tree has never changed, or if there is only one
#'   value the same value is kept for the added forgotten recruits. If the
#'   Diameter has not been corrected ('Diameter_TreeDataCor' column does not
#'   exist), the function will create it for the forgotten recruits. It is
#'   strongly recommended to correct the Diameter before correcting the recruits
#'
#' @return  Add rows for forgotten recruits with them estimated DBH in the
#'   'Diameter_TreeDataCor' column, create a 'CorrectedRecruit' col (logical)
#'   and fill the 'Comment' column : "This DBH is/was the 1st recorded for this
#'   tree, according to its annual growth and the census done for this plot, it
#'   should have been recruited earlier according to your protocol (MinDBH)."
#'
#' @importFrom data.table data.table rbindlist
#' @importFrom stats na.omit lm
#'
#' @export
#'
#' @examples
#' library(data.table)
#' data(TestData)
#' setnames(TestData, "Diameter", "Diameter_TreeDataCor")
#'
#' Rslt <- RecruitmentCorrection(TestData,
#'                               InvariantColumns = c("Site",
#'                                                    "Genus",
#'                                                    "Species",
#'                                                    "Family",
#'                                                    "ScientificName"))
#' IdCorr <- Rslt[CorrectedRecruit == TRUE, IdTree]
#' TreesCorr <- Rslt[IdTree %in% IdCorr]
#'
#' library(ggplot2)
#' ggplot(TreesCorr) +
#' aes(x = Year, y = Diameter_TreeDataCor) +
#'   geom_line(size = 0.5, colour = "#112446") +
#'   geom_point(shape = "circle", size = 1.5, mapping = aes(color = CorrectedRecruit)) +
#'   theme_minimal() +
#'   facet_wrap(vars(IdTree), scales = "free")
#'
RecruitmentCorrection <- function(
  Data,

  KeepMeas = c("MaxHOM", "MaxDate"),

  MinDBH = 10,
  PositiveGrowthThreshold = 5,
  InvariantColumns = c("Site",
                       "Genus_TreeDataCor",
                       "Species_TreeDataCor",
                       "Family_TreeDataCor",
                       "ScientificName_TreeDataCor"),
  DetectOnly = FALSE
){

  #### Arguments check ####
  # Data
  if (!inherits(Data, c("data.table", "data.frame")))
    stop("Data must be a data.frame or data.table")

  # IdStem or IdTree? ---------------------------------------------------------------------------------------
  # If no IdStem take IdTree
  if((!"IdStem" %in% names(Data) | all(is.na(Data$IdStem))) &
     ("IdTree" %in% names(Data) & any(!is.na(Data$IdTree))) ){ ID <- "IdTree"

  }else{ ID <- "IdStem"}

  if(!any(c("IdStem", "IdTree") %in% names(Data)) | (all(is.na(Data$IdStem)) &  all(is.na(Data$IdTree))) )
    stop("The 'IdStem' or 'IdTree' column is missing in your dataset")
  # ---------------------------------------------------------------------------------------------------------


  # MinDBH/PositiveGrowthThreshold (numeric, 1 value)
  if(!all(unlist(lapply(list(MinDBH, PositiveGrowthThreshold), length)) %in% 1) |
     !all(unlist(lapply(list(MinDBH, PositiveGrowthThreshold),
                        inherits, c("numeric", "integer")))))
    stop("The 'MinDBH' and 'PositiveGrowthThreshold'' arguments
         of the 'RecruitmentCorrection' function must be 1 numeric value each")

  if(DetectOnly %in% FALSE){
    # InvariantColumns
    if (!inherits(InvariantColumns, "character"))
      stop("'InvariantColumns' argument must be of character class")
  }

  # DetectOnly (logical)
  if(!all(unlist(lapply(list(DetectOnly),
                        inherits, "logical"))))
    stop("The 'DetectOnly' argument
         of the 'RecruitmentCorrection' function must be logicals")

  # Diameter_TreeDataCor column exists
  if(!"Diameter" %in% names(Data) & !"Diameter_TreeDataCor" %in% names(Data))
    stop("The 'Diameter' or the 'Diameter_TreeDataCor' (corrected Diameter)
           column does't exist in the dataset.")

  if(DetectOnly %in% FALSE){
    # Check if the InvariantColumns name exists in Data
    for(c in InvariantColumns){
      if (!c %in% names(Data)){ cc <- gsub("_TreeDataCor", "", c) # remove - Cor

      if (!cc %in% names(Data)){ # Col without - Cor exists?
        stop(paste("InvariantColumns argument must contain one or several column names (see help)."
                   ,cc,"is apparently not a dataset's column"))

      }else{ InvariantColumns[InvariantColumns == c] <- cc # If yes replace by the col name without cor
      warning("",c," column does't exist. ",cc," column is therefore considered as InvariantColumns instead of ",c,"")

      }
      } # if c doest exist
    } # end c loop
  }

  #### Function ####

  # data.frame to data.table
  setDT(Data)

  # Remove duplicated measurements per Year because different POM or Date -----------------------------------
  CompleteData <- copy(Data)
  Data <- UniqueMeasurement(Data, KeepMeas = KeepMeas, ID = ID)

  DuplicatedRows <- CompleteData[!Data, on = .NATURAL] # rows removed



  if(!"Comment" %in% names(Data)) Data[, Comment := ""]

  if(DetectOnly %in% FALSE){
    Data[, CorrectedRecruit := FALSE] # The initial rows are not corrected recruits
  }

  # Order IDs and times in ascending order ----------------------------------------------------------------------------
  Data <- Data[, Year := as.numeric(Year)]
  Data <- Data[order(get(ID), Year)]

  # IDs vector --------------------------------------------------------------------------------------------------------
  Ids <- as.vector(na.omit(unique(Data[!is.na(Year), get(ID)]))) # Tree Ids

  # Dataset with the rows without IDS ----------------------------------------------------------------------------------
  DataIDNa <- Data[is.na(get(ID))]

  # Dataset with the rows without Year --------------------------------------------------------------------------------
  DataYearNa <- Data[is.na(Year)]


  # Apply for all the trees
  # i = "100635"
  Data <- do.call(rbind, lapply(Ids, function(i) RecruitmentCorrectionByTree(
    Data[get(ID) %in% i & !is.na(Year)], # per ID, all censuses
    MinDBH = MinDBH,
    PositiveGrowthThreshold = PositiveGrowthThreshold,
    InvariantColumns = InvariantColumns,
    PlotCensuses = as.vector(na.omit( # rm NA
      unique(Data[Plot %in% unique(Data[get(ID) %in% i, Plot]),  Year]) # the censuses for the plot in which the tree is
    )),
    DetectOnly = DetectOnly
  )
  )) # do.call apply the 'rbind' to the lapply result

  # Re-put the rows duplicated, or without ID or Year -----------------------------------------------------------------
  Data <- rbindlist(list(Data, DuplicatedRows, DataIDNa, DataYearNa), use.names = TRUE, fill = TRUE)


  # Order IDs and times in ascending order ----------------------------------------------------------------------------
  Data <- Data[order(get(ID), Year)]

  return(Data)

}


#' RecruitmentCorrectionByTree
#'
#' @param DataTree A dataset corresponding to a single tree's (1 IdTree/Idstem)
#'   measurements (data.frame or data.table). The dataset should preferably
#'   contain the column of corrected diameters: 'Diameter_TreeDataCor', otherwise the function
#'   will take the column "Diameter"
#'
#' @param MinDBH Minimum diameter of trees inventoried according to your
#'   protocol (in cm) (numeric, 1 value)
#'
#' @param PositiveGrowthThreshold A tree widening by more than x
#'   cm/year is considered abnormal (numeric, 1 value)
#'
#' @param InvariantColumns Vector with the names of the columns that are
#'   supposed to have always the same value for each measurement of the same
#'   tree (character)
#'
#' @param PlotCensuses Census years for the plot in which the tree is (integer)
#'
#' @param DetectOnly TRUE: Only detect errors, FALSE: detect and correct errors
#'   (logical)
#'
#' @details If the size of the tree has never changed, or if there is only one
#'   value the same value is kept for the added forgotten recruits.
#'   If the Diameter has not been corrected (Diameter_TreeDataCor column does
#'   not exist), the function will create it for the forgotten recruits. It is
#'   strongly recommended to correct the Diameter before correcting the
#'   recruits.
#'
#' @return  Add rows for forgotten recruits with them estimated DBH in the
#'   'Diameter_TreeDataCor' column, create a 'CorrectedRecruit' column (logical)
#'    and fill the 'Comment' column : "This DBH is/was the 1st recorded for this
#'    tree, according to its annual growth and the census done for this plot, it
#'    should have been recruited earlier according to your protocol (MinDBH)."
#'
#' @importFrom data.table data.table rbindlist copy
#' @importFrom stats na.omit lm
#'
#' @export
#'
#' @examples
#' library(data.table)
#' DataTree <- data.table(IdTree = "a",
#'                        Year = seq(2000,2008, by = 2), # 2 years/census
#'                        Diameter_TreeDataCor  = as.numeric(c(13:17)), # 1cm/census(0.5 cm/year)
#'                        Site = "Imaginary forest"
#' )
#' # 1st DBH = 13 > MinDBH. In 1998 the tree was 12cm, 11cm in 1996 and 10cm in
#' 1994.
#' Rslt <- RecruitmentCorrectionByTree(DataTree,
#'                                     InvariantColumns = "Site",
#'                                     PlotCensuses = seq(1996,2016, by = 2))
#'
RecruitmentCorrectionByTree <- function(
  DataTree,
  MinDBH = 10,
  PositiveGrowthThreshold = 5, # je garde ?
  InvariantColumns = c("Site",
                       "GenusCor",
                       "SpeciesCor",
                       "FamilyCor",
                       "ScientificNameCor"),
  PlotCensuses,
  DetectOnly = FALSE
){

  #### Arguments check ####
  # DataTree
  if (!inherits(DataTree, c("data.table", "data.frame")))
    stop("DataTree must be a data.frame or data.table")

  # IdStem or IdTree? ---------------------------------------------------------------------------------------
  # If no IdStem take IdTree
  if((!"IdStem" %in% names(DataTree) | all(is.na(DataTree$IdStem))) &
     ("IdTree" %in% names(DataTree) & any(!is.na(DataTree$IdTree))) ){ ID <- "IdTree"

  }else{ ID <- "IdStem"}

  if(!any(c("IdStem", "IdTree") %in% names(DataTree)) | (all(is.na(DataTree$IdStem)) &  all(is.na(DataTree$IdTree))) )
    stop("The 'IdStem' or 'IdTree' column is missing in your dataset")
  # ---------------------------------------------------------------------------------------------------------


  # MinDBH/PositiveGrowthThreshold (numeric, 1 value)
  if(!all(unlist(lapply(list(MinDBH, PositiveGrowthThreshold), length)) %in% 1) |
     !all(unlist(lapply(list(MinDBH, PositiveGrowthThreshold),
                        inherits, c("numeric", "integer")))))
    stop("The 'MinDBH' and 'PositiveGrowthThreshold'' arguments
         of the 'RecruitmentCorrectionByTree' function must be 1 numeric value each")

  if(DetectOnly %in% FALSE){
    # InvariantColumns
    if (!inherits(InvariantColumns, "character"))
      stop("'InvariantColumns' argument must be of character class")
  }

  # PlotCensuses
  if (!inherits(PlotCensuses, c("numeric", "integer")))
    stop("'PlotCensuses' argument must be numeric or integer")

  # DetectOnly (logical)
  if(!all(unlist(lapply(list(DetectOnly),
                        inherits, "logical"))))
    stop("The 'DetectOnly' argument
         of the 'RecruitmentCorrectionByTree' function must be logicals")

  if(!"Diameter" %in% names(DataTree) & !"Diameter_TreeDataCor" %in% names(DataTree))
    stop("The 'Diameter' or the 'Diameter_TreeDataCor' (corrected Diameter)
           column does't exist in the dataset.")

  if(DetectOnly %in% FALSE){
    # Diameter_TreeDataCor column exists
    if(!"Diameter_TreeDataCor" %in% names(DataTree))
      warning("The 'Diameter_TreeDataCor' (corrected Diameter) column does't exist in the dataset.
         We advise to first correct the diameter measurements before correcting the recruitment")
  }

  # if there are several IdTrees
  if(length(unique(DataTree[, get(ID)])) != 1){
    stop("DataTree must correspond to only 1 same tree/stem so 1 same ",ID,"
    (the ",ID,"s: " ,paste0(unique(DataTree[, get(ID)]), collapse = "/"),"")
  }

  # if there are several plots for the same IdTree
  # if(length(as.vector(na.omit(unique(DataTree$Plot)))) != 1){
  #   stop(paste0("Tree ",unique(DataTree$IdTree)," has multiple plots: " ,paste0(unique(DataTree$Plot), collapse = "/")))
  # }

  if(DetectOnly %in% FALSE){
    # Check if the InvariantColumns name exists in DataTree
    for(c in InvariantColumns){
      if(!c %in% names(DataTree)){
        stop(paste("InvariantColumns argument must contain one or several column names (see help)."
                   ,c,"is apparently not a dataset's column"))
      }
    }
  }

  #### Function ####

  # print(unique(DataTree[, get(ID)])) # to debug

  # data.frame to data.table
  setDT(DataTree)

  # if there are no 'Diameter_TreeDataCor' col, create it from the Diameter col
  # (removes Diameter_TreeDataCor values that are not of order RecruitmentCorrection at the end)
  InitialDT <- copy(DataTree)
  if(!"Diameter_TreeDataCor" %in% names(DataTree))
    DataTree[, Diameter_TreeDataCor := Diameter]

  # Arrange year in ascending order
  PlotCensuses <- sort(PlotCensuses, decreasing = FALSE) # order plot census years
  DataTree <- DataTree[order(Year)] # order years in the data

  #### Compute annual diameter incrementation ####
  DBHCor <- DataTree[,Diameter_TreeDataCor]
  Year <- DataTree[,Year]

  if(!"Comment" %in% names(DataTree)) Data[, Comment := ""]

  # Initialisation
  cresc <- rep(0, length(DBHCor) - 1) # (cresc[1] corresponds to the 2nd DBH)

  if (sum(!is.na(DBHCor)) > 1) { # if there is at least 1 measurement

    cresc[which(!is.na(DBHCor))[-1] - 1] <- # 8 cresc for 9 dbh values ([-1]), shift all indices by 1 to the left (-1)
      diff(DBHCor[!is.na(DBHCor)]) / diff(Year[!is.na(DBHCor)]) # DBH difference between pairwise censuses / time difference between pairwise censuses

  }

  #### Detect the overgrown recruits ####

  RecruitYear <- min(DataTree[, Year], na.rm = TRUE) # recruitment year

  if(RecruitYear > min(PlotCensuses, na.rm = TRUE) & sum(!is.na(DataTree$Diameter_TreeDataCor)) > 0){ # if the 1st census of the plot is smaller than the 1st census of the tree, and there are measured DBH

    PrevCens <- PlotCensuses[which(PlotCensuses == RecruitYear)-1] # 1 census before the recruit year among the plot censuses

    FirstDBH <- DataTree[!is.na(Diameter_TreeDataCor), Diameter_TreeDataCor][1] # 1st measured DBH

    # if(DataTree$Diameter_TreeDataCor[1] > (MinDBH + (RecruitYear - PrevCens) * PositiveGrowthThreshold)){ # ah ben il detecte pas mes oublis pcq il considère l'erreur que si l'écart est superieur à la limite de croissance

    # Growth criteria
    if(length(cresc) > 0){ # if there are a growth
      Growth <- cresc[1] # 1st growth value
    }else{Growth <- 0} # if only 1 DBH value (no cresc)

    # Maybe here we should round up the Growth, or propose a limit.

    # Detection
    #### If the 1st DBH is larger than it would have been if at the previous census
    # it was at the minimum DBH
    if((FirstDBH > (MinDBH + (RecruitYear - PrevCens) * Growth)) & (Growth > 0)){ # ma proposition
      # and Growth > 0 je pense à ajouter

      DataTree <- GenerateComment(DataTree,
                                  condition = DataTree[, Year]  %in% RecruitYear,
                                  comment = "This DBH is/was the 1st recorded for this tree,
                                  according to its annual growth and the census done for this plot,
                                  it should have been recruited earlier according to your protocol (MinDBH).")

      if(DetectOnly %in% FALSE){

        MissingCens <- PlotCensuses[which(PlotCensuses < RecruitYear)] # the previous missing censuses

        #### Create new rows for the forgotten recruits ####
        if(length(MissingCens) > 0){
          if("Plot" %in% names(DataTree)){ # if we have the plot info
            NewRow <- data.table(ID = unique(DataTree[,get(ID)]), # the IdTree
                                 Year = NA, # the censuses to add
                                 Plot = unique(DataTree$Plot), # the unique plot of the tree
                                 CorrectedRecruit = TRUE, # there are corrected recruits
                                 stringsAsFactors = FALSE) # do not convert characters into factors
          }
          else{
            NewRow <- data.table(ID = unique(DataTree[,get(ID)]), # the IdTree
                                 Year = NA, # the censuses to add
                                 CorrectedRecruit = TRUE, # there are corrected recruits
                                 stringsAsFactors = FALSE) # do not convert characters into factors
          }

          setnames(NewRow, "ID", ID)

          if(length(InvariantColumns) > 0){ # if there are invariant columns

            NewRow[,(InvariantColumns) := NA] # empty the invariant columns for the added rows

            # Fill in the invariant columns in the added rows
            NewRow <- FillinInvariantColumns(NewRow = NewRow,
                                             InvariantColumns = InvariantColumns,
                                             DataTree = DataTree,
                                             IdTree = unique(DataTree[,get(ID)]))
          }

          # Multiply this new row the number of times as well as the number of absents
          NewRows <- do.call("rbind", replicate(n = length(MissingCens), NewRow, simplify = FALSE))
          NewRows[, Year := MissingCens]

          # Add these rows in the dataset
          DataTree <- rbindlist(list(DataTree, NewRows), use.names=TRUE, fill=TRUE)
          DataTree[, CorrectedRecruit := ifelse(is.na(CorrectedRecruit), FALSE, CorrectedRecruit)] # FALSE for the other rows

          DataTree <- DataTree[order(Year)] # order by Year

          #### Linear regression (DBH ~ Year) #### with all the DBH values
          coef <- stats::lm(
            DataTree[!is.na(Diameter_TreeDataCor), Diameter_TreeDataCor] ~ DataTree[!is.na(Diameter_TreeDataCor), Year])$coefficients

          if(is.na(coef[2]) | coef[2] %in% 0) { # if no slope
            ### if only 1 DBH value: replace all non-recruited DBH by this value (pas sure que ce soit une bonne idée)
            DataTree[Year < RecruitYear, ("Diameter_TreeDataCor") := unique(DataTree[!is.na(Diameter_TreeDataCor), Diameter_TreeDataCor])] # DBHCor := coef[1] (ça donne des pptés à la valeur)

          }else{

            # Estimate the recruits DBHCor with linear extrapolation
            RecruitsDBH <- coef[1] + DataTree[Year < RecruitYear, Year]*coef[2] # y = b + ax. Min entre ces DBH inférés et le 1er DBH

            # If estimated DBHCors are higher than the first measured DBHCors, (comment c possible ?)
            # these are replaced by first measured DBHCors.
            for(y in 1: length(RecruitsDBH)){
              RecruitsDBH[y] <- min(RecruitsDBH[y], FirstDBH)
            }
            DataTree[Year < RecruitYear, ("Diameter_TreeDataCor") := RecruitsDBH]
          }

          # UselessRows: added trees under the MinDBH
          UselessRows <- ((DataTree[, Diameter_TreeDataCor] < MinDBH) & (DataTree[, Year] %in% MissingCens))

          if(any(UselessRows)){
            DataTree <- DataTree[-which(UselessRows)] # remove them
          }


        } # end: missing censuses to add
      } # end: DetectOnly == FALSE
    } # end: overgrown recruit
  } # end: if the plot have previous censuses

  # If no 'Diameter_TreeDataCor' initially in the dataset
  if(!"Diameter_TreeDataCor" %in% names(InitialDT)){

    if(DetectOnly %in% TRUE) DataTree[, Diameter_TreeDataCor := NULL] # if detect only, remove Diameter_TreeDataCor if it didn't exist before

    if(DetectOnly %in% FALSE)
      DataTree[, Diameter_TreeDataCor := ifelse(CorrectedRecruit %in% FALSE, NA, Diameter_TreeDataCor)] # keep only recruitment correction
  }

  DataTree[is.na(Comment), ("Comment") := ""] # NAs come from NewRow

  return(DataTree)

}
