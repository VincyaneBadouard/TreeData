#' Full error processing
#'
#' @inheritParams GeneralErrorsDetection
#' @inheritParams DetectBotanicalErrors
#' @inheritParams StatusCorrection
#' @inheritParams TaperCorrection
#' @inheritParams DiameterCorrection
#' @inheritParams RecruitmentCorrection
#'
#' @details Detect errors or detect and correct errors:
#' - Check general errors (*GeneralErrorsDetection*)
#' - Check botanical identification (*DetectBotanicalErrors*)
#' - Check the life status evolution of the trees/stems (*StatusCorrection*)
#' - Apply a taper allometry on diameters measured at heights different from the
#'    default(*TaperCorrection*)
#' - Check diameter evolution of the trees (*DiameterCorrection*)
#' - Check tree/stem recruitment (*RecruitmentCorrection*)
#'
#' @seealso \link{GeneralErrorsDetection}, \link{DetectBotanicalErrors},
#'   \link{StatusCorrection}, \link{TaperCorrection}, \link{DiameterCorrection},
#'   \link{RecruitmentCorrection}
#'
#' @return The original dataset (data.table) with a *Comment* column containing
#'   information on the errors detected per row, the correction columns, and
#'   columns containing correction methods.
#'
#' @export
#'
#' @examples
#' data(TestData)
#' Rslt <- FullErrorProcessing(TestData, DetectOnly = TRUE)
#'
#'\dontrun{
#' WFO_Backbone <- file.choose()
#' load(WFO_Backbone)
#' Rslt_Test <- FullErrorProcessing(TestData, Source = "WFO", WFOData = WFO_Backbone)
#' Rslt_Panama <- FullErrorProcessing(PanamaFormated, Source = "WFO", WFOData = WFO_Backbone)
#' }
#'
FullErrorProcessing <- function(

  Data,

  DetectOnly = FALSE,

  # Botanical informations
  Source = NULL,
  WFOData = NULL,

  # Life status
  InvariantColumns = c("Site",
                       "Genus_TreeDataCor",
                       "Species_TreeDataCor",
                       "Family_TreeDataCor",
                       "ScientificName_TreeDataCor"),
  DeathConfirmation = 2,
  UseSize = FALSE,
  RemoveRBeforeAlive = FALSE,
  RemoveRAfterDeath = FALSE,

  # Taper
  DefaultHOM = 1.3,
  TaperParameter = function(DAB, HOM) 0.156 - 0.023 * log(DAB) - 0.021 * log(HOM),
  TaperFormula = function(DAB, HOM, TaperParameter, DefaultHOM) DAB / (exp(- TaperParameter*(HOM - DefaultHOM))),

  # Diameter
  KeepMeas = c("MaxHOM", "MaxDate"),
  MaxDBH = 500,
  PositiveGrowthThreshold = 5,
  NegativeGrowthThreshold = -2,

  Pioneers = NULL,
  PioneersGrowthThreshold = 7.5,

  WhatToCorrect = c("taper", "POM change", "punctual", "shift"),
  CorrectionType = c("individual", "phylogenetic hierarchical"),

  DBHRange = 10,
  MinIndividualNbr = 5,
  OtherCrit = NULL,
  Digits = 1L,

  DBHCorForDeadTrees = TRUE,

  coef = 0.9,

  # Recruitment
  MinDBH = 10
){

  #### Arguments check ####

  # Data
  if (!inherits(Data, c("data.table", "data.frame")))
    stop("Data must be a data.frame or data.table")

  # Source
  if(!is.null(Source)){
    if(Source != "WFO")
      stop("Source argument should be one of c('WFO', NULL)")
  }

  # WFOData
  if(!is.null(Source)){
    if(Source == "WFO" & is.null(WFOData))
      stop("You must provide the 'WFOData' argument,
          a database as a static copy of the World Flora Online (WFO) Taxonomic Backbone,
          when you choose Source = 'WFO'.")
  }

  # Plot column exists
  if (!"Plot" %in% names(Data)){
    stop("The column 'Plot' must be present in the dataset
    to add rows to the census where the plot was inventoried, where the tree was alive, but not recorded")
  }

  # InvariantColumns
  if (!inherits(InvariantColumns, "character"))
    stop("'InvariantColumns' argument must be of character class")

  # DeathConfirmation
  if (!inherits(DeathConfirmation, "numeric"))
    stop("'DeathConfirmation' argument must be numeric")

  # UseSize/DetectOnly/RemoveRBeforeAlive/RemoveRAfterDeath
  if (!all(unlist(lapply(list(UseSize, DetectOnly, RemoveRBeforeAlive, RemoveRAfterDeath),
                         inherits, "logical"))))
    stop("The 'UseSize', 'DetectOnly', 'RemoveRBeforeAlive' and 'RemoveRAfterDeath' arguments
         of the 'SatusCorrection' function must be logicals")

  # UseSize-Diameter
  if(UseSize %in% TRUE){ # if it is desired (TRUE) to use the presence of measurement to consider the tree alive
    if (!"Diameter" %in% names(Data)){
      stop("If you wish to use the size presence (UseSize=TRUE) as a witness of the living status of the tree,
           the 'Diameter' column must be present in the dataset")
    }
  }

  if("taper" %in% CorrectionType){
    # Check if the HOM column exists
    if(!"HOM" %in% names(Data)){
      stop("You have chosen to make a 'taper' correction,
       but you do not have the necessary 'HOM' column in your dataset")
    }

    # TaperParameter/TaperFormula (function)
    if(!all(unlist(lapply(list(TaperParameter, TaperFormula),
                          inherits, "function"))))
      stop("The 'TaperParameter' and 'TaperFormula' arguments must be functions")

  } # end: if taper

  # Diameter column exists
  if(!"Diameter" %in% names(Data))
    stop("The 'Diameter' column does't exist in the dataset")

  # DefaultHOM/Min-MaxDBH/Positive-Negative-PioneersGrowthThreshold/DBHRange/MinIndividualNbr (numeric, 1 value)
  if(!all(unlist(lapply(list(DefaultHOM, MaxDBH,
                             PositiveGrowthThreshold, NegativeGrowthThreshold, PioneersGrowthThreshold,
                             DBHRange, MinIndividualNbr),
                        length)) %in% 1) |
     !all(unlist(lapply(list(PositiveGrowthThreshold, NegativeGrowthThreshold, DefaultHOM, PioneersGrowthThreshold),
                        inherits, c("numeric", "integer")))))
    stop("The 'PositiveGrowthThreshold', 'NegativeGrowthThreshold', 'PioneersGrowthThreshold' and 'DefaultHOM' arguments
         must be 1 numeric value each")

  # Pioneers (characters vector)
  if(!inherits(Pioneers, "character") & !is.null(Pioneers))
    stop("'Pioneers' argument must be a characters vector, or NULL")

  # WhatToCorrect
  if(!any(c("POM change","punctual", "shift") %in% WhatToCorrect))
    stop("The 'WhatToCorrect' argument value must be among 'POM change', 'punctual' and 'shift'")

  # CorrectionType
  if(!any(c("taper","linear", "quadratic", "individual", 'phylogenetic hierarchical') %in% CorrectionType))
    stop("The 'CorrectionType' argument value must be among
         'taper', 'linear', quadratic', 'individual' and 'phylogenetic hierarchical'")

  # Digits
  if(!inherits(Digits, "integer") & Digits != as.integer(Digits))  {
    warning(paste0("The 'Digits' argument must be an integer. Value entered (", Digits, ")  coerced to ", as.integer(Digits), "."))
    Digits <- as.integer(Digits)
  }

  # Taper before if 'HOM' and no taper correction asked
  if(any(!is.na(Data$HOM)) & !"taper" %in% CorrectionType) # HOM exists?
    message("You have the 'HOM' information in your dataset.
            We advise you to correct your diameters also with the 'taper' correction (CorrectionType = 'taper')")

  # If 'POM' 'POM change' correction is advised
  if((all(is.na(Data$HOM)) | !"HOM" %in% names(Data)) &
     any(!is.na(Data$POM)) & !any(WhatToCorrect %in% "POM change")) # POM exists?
    message("You have the 'POM' information in your dataset.
            We advise you to correct your diameters also from the 'POM change' ('WhatToCorrect' argument)")

  # 'POM change' correction needs 'POM' or 'HOM' values
  if(!any(c("POM", "HOM") %in% names(Data)) | (all(is.na(Data$POM)) &  all(is.na(Data$HOM))) )
    stop("You have chosen to make a 'POM change' correction,
       but you do not have the necessary 'POM' or HOM' column in your dataset or they are empty")

  #### General errors detection ####
  Data <- GeneralErrorsDetection(Data)

  #### Botanical informations ####

  Data <- DetectBotanicalErrors(Data = Data)

  #### Life status ####

  Data <- StatusCorrection(Data,
                           InvariantColumns = InvariantColumns,
                           DeathConfirmation = DeathConfirmation,
                           UseSize = UseSize,
                           DetectOnly = DetectOnly,
                           RemoveRBeforeAlive = RemoveRBeforeAlive,
                           RemoveRAfterDeath = RemoveRAfterDeath)

  #### Taper ####
  if("taper" %in% WhatToCorrect & "HOM" %in% names(Data) & any(!is.na(Data$HOM))){

    Data <- TaperCorrection(Data,
                            DefaultHOM = DefaultHOM,

                            TaperParameter = TaperParameter,
                            TaperFormula = TaperFormula,

                            DetectOnly = DetectOnly)
  }

  #### Diameter ####

  if(any(c("linear", "quadratic", "individual", 'phylogenetic hierarchical') %in% CorrectionType) |
     any(c("POM change", "punctual", "shift") %in% WhatToCorrect)){

    Data <- DiameterCorrection(Data,

                               KeepMeas = KeepMeas,

                               DefaultHOM = DefaultHOM,
                               MaxDBH = MaxDBH,
                               PositiveGrowthThreshold = PositiveGrowthThreshold,
                               NegativeGrowthThreshold = NegativeGrowthThreshold,

                               Pioneers = Pioneers,
                               PioneersGrowthThreshold = PioneersGrowthThreshold,

                               WhatToCorrect = WhatToCorrect,
                               CorrectionType = CorrectionType,

                               DBHRange = DBHRange,
                               MinIndividualNbr = MinIndividualNbr,
                               OtherCrit = OtherCrit,
                               Digits = Digits,

                               DBHCorForDeadTrees = DBHCorForDeadTrees,

                               coef = coef,

                               DetectOnly = DetectOnly)
  }

  #### Recruitment ####

  Data <- RecruitmentCorrection(Data,
                                KeepMeas = KeepMeas,
                                MinDBH = MinDBH,
                                PositiveGrowthThreshold = PositiveGrowthThreshold,
                                DetectOnly = DetectOnly
  )

  return(Data)

}
