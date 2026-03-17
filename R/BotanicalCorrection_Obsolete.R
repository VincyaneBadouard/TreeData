#' Botanical Correction
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
#'
#'@param Source (character) To correct and standardise, you can choose between:
#'  - "WFO": *World Flora Online* (http://www.worldfloraonline.org/) (long time
#'           but based on the 2022 taxonomy)
#'  - NULL: if only error detection (DetectOnly = TRUE)
#'
#' @param WFOData To be filled in if the argument `Source` = "WFO". Data set
#'   with the static copy of the *World Flora Online* (WFO) Taxonomic Backbone
#'   data (from http://www.worldfloraonline.org/downloadData.) (data.frame or
#'   data.table)
#'
#' @param DetectOnly TRUE: Only detect errors, FALSE: detect and correct errors
#'   (Default: FALSE) (logical)
#'
#' @return Fill the *Comment* column with error type informations. If
#'   *DetectOnly* = FALSE, add columns:
#'   - `Family_TreeDataCor` (character): corrected Family name
#'   - `FamilyCorSource` (character): source of the Family correction
#'   - `Genus_TreeDataCor` (character): corrected Genus name
#'   - `Species_TreeDataCor` (character): corrected Species name
#'   - `BotanicalCorrectionSource` (character): source of the Genus and Species
#'       correction
#'   - `ScientificName_TreeDataCor` (character): corrected Scientific name
#'   - `VernName_TreeDataCor` (character): completed if information available at
#'       `IdTree` level.
#'
#'@details
#' - Check for special characters (typography)
#' - Check for family name in the Genus and Species columns (the suffix "aceae" is
#'     specific to the family name.
#' - Check if the family name does not end in 'aceae'.
#' - Correct spelling of botanical names (*WorldFlora*)
#' - Family & Scientific names match (*BIOMASS::getTaxonomy or WorldFlora*)
#' - Update the scientific botanical names with the current phylogenetic
#'     classification
#' - Check **invariant botanical informations per IdTree** (1 IdTree = 1 family,
#'     1 scientific and 1 vernacular name)
#'
#'@importFrom BIOMASS getTaxonomy
#'@importFrom WorldFlora WFO.match
#'@importFrom stats na.omit
#'
#' @export
#'
#' @examples
#'\dontrun{
#' library(data.table)
#' data(TestData)
#'
#'
#'# With World Flora Online:
#' WFO_Backbone <- file.choose()
#' load(WFO_Backbone)
#'
#' RsltWFO <- BotanicalCorrection(TestData, Source = "WFO", WFOData = WFO_Backbone)
#'
#' ScfcCor <- unique(RsltWFO[ScientificNameCor != ScientificName,
#'                 list(ScientificName, ScientificNameCor,
#'                 Family, FamilyCor,
#'                 Genus, GenusCor,
#'                 Species, SpeciesCor, Subspecies,
#'                 BotanicalCorrectionSource, Comment)
#'                 ])
#'
#' FamCor <- unique(RsltWFO[FamilyCor != Family,
#'                 list(ScientificName, ScientificNameCor,
#'                 Family, FamilyCor,
#'                 Genus, GenusCor,
#'                 Species, SpeciesCor, Subspecies,
#'                 BotanicalCorrectionSource, Comment)
#'                 ])
#'}
#'
BotanicalCorrection <- function(
  Data,
  Source = NULL,
  WFOData = NULL,
  DetectOnly = FALSE
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


  # DetectOnly (logical)
  if(!inherits(DetectOnly, "logical"))
    stop("The 'DetectOnly' argument must be a logical")


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

  if(DetectOnly %in% FALSE){

    # Corrected columns initialisation --------------------------------------------------------------------------------------
    Data[, GenusCor := Genus]
    Data[, SpeciesCor := Species]

    # No family name in the genus and species columns -----------------------------------------------------------------------

    ## Columns split if there is multiple information -----------------------------------------------------------------------
    # For Genus: split at punctuation then at upper case, and Create GenspFamily
    Data[, c("GenusCor", "GenspFamily") := tstrsplit(Genus, '[[:punct:]]')]
    Data[, c("GenusCor", "GenspFamily") := tstrsplit(Genus, "(?<=.)(?=[[:upper:]])", perl = TRUE)]

    ## Detection of the suffix "aceae" in the genus column (it is specific to the family name)
    # if there is -aceae in  GenusCor and not in GenspFamily, swap values between GenusCor and GenspFamily
    Data[grep("aceae", GenusCor),  c("GenusCor", "GenspFamily")] <- Data[grep("aceae", GenusCor), c("GenspFamily", "GenusCor")]

    # For species: split at space or underscore, and create Subspecies
    Data[, c("SpeciesCor", "Subspecies") := tstrsplit(Species, '\\[[:blank:]] |\\_')] # \\ devant une des possibilités. Le manque d'espace après le barre du "ou" (|) est important, le résultat n'est pas le même sinon
    # Detection of the suffix "aceae" in the species column (it is specific to the family name)
    Data[grep("aceae", SpeciesCor), `:=`(GenspFamily = ifelse(grep("aceae", SpeciesCor), SpeciesCor, GenspFamily),
                                         SpeciesCor = NA_character_)]

    Data[!grep("aceae", GenspFamily), GenspFamily := NA_character_]

    # Remove special characters only for Genus (because in Species we want to keep them): ---------------------------------
    # remove : !"#$%&’()*+,-./:;<=>?@[]^_`{|}~
    Data[, GenusCor := gsub("[[:punct:]]", "", Data$GenusCor)]

    Data[, ScientificNameCor := paste(GenusCor, SpeciesCor)]

  } # end DetectOnly = FALSE


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

  if(DetectOnly %in% FALSE){

    # if(Source == "TPL"){
    #
    #   # Correct spelling error & standardise botanical names ----------------------------------------------------------------
    #
    #   # TPL correction with Taxonstand package
    #   TPLCor <- suppressWarnings(Taxonstand::TPL(splist = unique(Data$ScientificNameCor),
    #                                              corr = TRUE, diffchar = 20, max.distance = 1)
    #   ) # diffchar: maximum difference of characters nbr between input and output
    #   # with Genus and species marche pas bien pcq décale genre et sp quand on unique())
    #
    #   setDT(TPLCor) # df to dt
    #
    #   # Take only corrected names. Columns: New.Genus, New.Species, Typo. Not Family because it is outdated.
    #   TPLCor <- TPLCor[New.Genus != Genus | New.Species != Species,]
    #   TPLCor <- TPLCor[, list(Taxonomic.status, Typo, Taxon, New.Genus, New.Species)]
    #   TPLCor[, BotanicalCorrectionSource := "The Plant List"] # create the Source
    #
    #
    #   # Join the corrected Genus and Species, by original 'ScientificNameCor'
    #   Data <- merge(Data, TPLCor, by.x = "ScientificNameCor", by.y = "Taxon", all.x = TRUE)
    #
    #   # Update correction columns
    #   Data[, GenusCor := ifelse(!is.na(New.Genus), New.Genus, GenusCor)]
    #   Data[, SpeciesCor := ifelse(!is.na(New.Species), New.Species, SpeciesCor)]
    #
    #
    #   # Comment:
    #   ## if "Synonym" :
    #   Data <- GenerateComment(Data,
    #                           condition = Data$Taxonomic.status == "Synonym",
    #                           comment = "'ScientificName' is a synonym of the accepted botanical name")
    #   ## if Typo == TRUE :
    #   Data <- GenerateComment(Data,
    #                           condition = Data$Typo == TRUE,
    #                           comment = "Spelling error in the 'ScientificName'")
    #
    #   # Remove columns that have become useless
    #   Data[, c("Taxonomic.status", "Typo", "New.Genus", "New.Species") := NULL]
    #
    #
    #   # Family & Scientific names match -------------------------------------------------------------------------------------
    #   # Retrieve Family names by Genus
    #   # (*BIOMASS::getTaxonomy*) with APG III family
    #
    #   FamilyData <-
    #     setDT( # as data.table
    #       BIOMASS::getTaxonomy(unique(Data$GenusCor), findOrder = FALSE)
    #     )
    #
    #   setnames(FamilyData, "family", "FamilyCor") # rename columns
    #
    #
    #   # Join Family table and the dataset
    #   Data <- merge(Data, FamilyData, by.x = "GenusCor", by.y = "inputGenus",  all.x = TRUE, sort = FALSE)
    #
    # } # end if "TPL"

    if(Source == "WFO"){

      # Prepare WFO database
      setDT(WFOData) # in data.table
      WFOData[is.na(WFOData), ] <- "" # WFO.match doesn't take NA but ""

      # Prepare Data (replace risky characters)
      Data[, ScientificNameCor := gsub(" NA", "", ScientificNameCor)]
      Data[, ScientificNameCor := gsub("Indet", "", ScientificNameCor)]
      Data[, ScientificNameCor := gsub("indet", "", ScientificNameCor)]

      # "Plants of the World Online" correction (more actual than WFO): ---------------------------------------------------
      Data <- GenerateComment(Data,
                              condition = Data$ScientificNameCor %in% c("Tetragastris panamensis",
                                                                        "Protium picramnioides",
                                                                        "Tetragastris stevensonii"),
                              comment = "'ScientificName' is a synonym of the accepted botanical name")

      Data[ScientificNameCor %in% c("Tetragastris panamensis",
                                    "Protium picramnioides",
                                    "Tetragastris stevensonii"),
           `:=` (ScientificNameCor = "Protium stevensonii",
                 FamilyCor = "Burseraceae",
                 BotanicalCorrectionSource = "Plants of the World Online")]

      # Genus not found but I don't know why: -----------------------------------------------------------------------------
      # Data[GenusCor == "Tovomita",
      #      FamilyCor := "Clusiaceae"]

      # Special taxa vector
      Specialtaxa <- c("Protium stevensonii")

      WFmatch <- WorldFlora::WFO.match(spec.data = unique(Data[!ScientificNameCor %in% Specialtaxa,
                                                               ScientificNameCor]), # data to correct
                                       WFO.data = WFOData, # WFO data
                                       no.dates = TRUE, # to speed
                                       First.dist = TRUE,
                                       Fuzzy.one = TRUE,
                                       Fuzzy.force = FALSE,
                                       Fuzzy.shortest = TRUE,
                                       # squish = TRUE, # don't remove whitespace
                                       spec.name.nonumber = TRUE, # if nbr take only the genus
                                       spec.name.sub = FALSE, # don't remove " sp"
                                       verbose = FALSE)
      setDT(WFmatch) # in data.table

      WFmatch <- WFmatch[, list(taxonomicStatus, Old.status, spec.name.ORIG, scientificName, family)] # columns of interest
      WFmatch <- WFmatch[taxonomicStatus == "ACCEPTED",] # Only "ACCEPTED"
      WFmatch[, taxonomicStatus := NULL] # remove the column


      # Remove multiple matches case (but keep the family and prefer not synonym)
      # WFmatch[spec.name.ORIG %in% c(WFmatch[duplicated(WFmatch[, spec.name.ORIG]), spec.name.ORIG]), Old.status := ""] # not necessary with fromLast = TRUE
      WFmatch <- WFmatch[!duplicated(WFmatch[, spec.name.ORIG], fromLast = TRUE)] # the first is a synonym, the last can be the original name

      # Create the correction source
      WFmatch[, BotaCorSource := "World Flora Online"]


      # Join the corrected Genus and Species, by original 'ScientificNameCor'
      Data <- merge(Data, WFmatch, by.x = "ScientificNameCor", by.y = "spec.name.ORIG", all.x = TRUE)

      Data[, ScientificNameCor := NULL] # remove previous column before create the new one

      # Deal with "Plants of the World Online" correction
      Data[, FamilyCor := ifelse(is.na(FamilyCor), family, FamilyCor)]
      Data[, BotanicalCorrectionSource := ifelse(is.na(BotanicalCorrectionSource), BotaCorSource, BotanicalCorrectionSource)]
      Data[, c("family", "BotaCorSource") := NULL]

      setnames(Data, "scientificName", "ScientificNameCor") # rename columns

      # For Genus not detected by WFO by already corrected the family
      Data[is.na(ScientificNameCor) & !is.na(FamilyCor), ScientificNameCor := paste(GenusCor, SpeciesCor)]

      # No output species name if only genus in input
      Data[is.na(SpeciesCor) | grepl("Indet", SpeciesCor)| grepl("indet", SpeciesCor)| grepl("[0-9]", SpeciesCor),
           ScientificNameCor := sub(" .*", "", ScientificNameCor) ]

      # But we want to keep species name with number
      Data[grepl("[0-9]", SpeciesCor),
           ScientificNameCor := paste(ScientificNameCor, SpeciesCor)]


      # if "Synonym" :
      Data <- GenerateComment(Data,
                              condition = Data$Old.status %in% "SYNONYM",
                              comment = "'ScientificName' is a synonym of the accepted botanical name")
      Data[, Old.status := NULL] # remove the column

      # Create GenusCor and SpeciesCor
      Data[, c("GenusCor", "SpeciesCor") := tstrsplit(ScientificNameCor, " ", fixed = TRUE)] # fixed = T : match split exactly

    } # end if WFO

    # IN COMMON -------------------------------------------------------------------------------------------------------------

    # Generate a comment if the family name is incorrect --------------------------------------------------------------------
    Data <- GenerateComment(Data,
                            condition = !Data$Family %in% Data$FamilyCor,
                            comment = "The 'Family' name is incorrect")

    # if(Source == "TPL") FamCorSource <- "APG III family"
    if(Source == "WFO") FamCorSource <- "World Flora Online"
    Data[!is.na(FamilyCor), FamilyCorSource := FamCorSource] # create the Source

    # If no Family corr because no genus, previously with -aceae, take this name put in GenspFamily -------------------------
    Data[is.na(FamilyCor) & !is.na(GenspFamily), `:=`(FamilyCor = GenspFamily,
                                                      FamilyCorSource = "Found in the 'Genus' or 'Species' column")]

    Data[, GenspFamily := NULL] # remove obsolete column

    # Homogenise unique botanical info (same Family, Genus, Species, Vernacular name) by IdTree if NA -----------------------

    Data[, VernNameCor := VernName]

    BotaCols <- c("FamilyCor", "GenusCor", "SpeciesCor", "VernNameCor", "FamilyCorSource", "BotanicalCorrectionSource")

    for(j in BotaCols){
      Data[,  (j) := ifelse(is.na(get(j)) & length(na.omit(unique(get(j)))) == 1,
                            na.omit(unique(get(j))), get(j))
           , keyby = IdTree]
    }

    # If no correction keep input botanical values: "Family", "Genus", "Species", "ScientificName" ------------------------
    # (en fait c'est pas une bonne idée)
    # NotCorVar <- c("Family", "Genus", "Species", "ScientificName")
    #
    # for(i in NotCorVar){
    #   j <- paste0(i, "Cor")
    #
    #   Data[is.na(get(j)), c(j, "BotanicalCorrectionSource") := list(get(i), "Taxon not found")]
    #
    # }
    Data[is.na(FamilyCor) & is.na(BotanicalCorrectionSource), BotanicalCorrectionSource := "Taxon not found"]

    # Reform ScientificNameCor ------------------------------------------------------------------------------------------
    # If "NA NA" -> NA_character_

    Data[, ScientificNameCor := paste(GenusCor, SpeciesCor)]

    Data[, ScientificNameCor := ifelse(ScientificNameCor == "NA NA", NA_character_, ScientificNameCor)]

    Data[, ScientificNameCor := gsub(" NA", "", ScientificNameCor)] # remove NA after Genus in ScientificNameCor


  } # end DetectOnly = FALSE


  # Check invariant botanical informations per IdTree -------------------------------------------------------------------
  # Family, Genus, Species, Subspecies, VernName

  if(!"Subspecies" %in% names(Data)) Data[, Subspecies := NA_character_]

  duplicated_ID <- CorresIDs <- vector("character")

  if(DetectOnly %in% FALSE) vars <- c("IdTree", "FamilyCor", "GenusCor", "SpeciesCor", "Subspecies", "VernNameCor")
  if(DetectOnly %in% TRUE) vars <- c("IdTree", "Family", "Genus", "Species", "Subspecies", "VernName")


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

  if(DetectOnly %in% FALSE){
    # Rename correction columns
    Corcol <- c("FamilyCor", "GenusCor", "SpeciesCor", "ScientificNameCor", "VernNameCor")
    setnames(Data, Corcol, gsub("Cor", "_TreeDataCor", Corcol))
  }

  return(Data)

}
