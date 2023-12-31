#' Compute Colleagues Growth Mean
#'
#' @description Internal function of *PhylogeneticHierarchicalCorrection()*
#'
#' @param Colleagues 'Colleagues' are the individuals other than the one whose
#'   diameter is to be corrected. Their diameter is included in the 'DBHRange'
#'   around the diameter to be corrected. They are either of the same species,
#'   the same genus, the same family or the same stand, depending on what the
#'   inventory proposes and the minimum selection criteria of the user
#'   ('*MinIndividualNbr*' argument of *PhylogeneticHierarchicalCorrection()*).
#'   (data.table)
#'   The dataset must contain the column: 'IdTree' (character)
#'
#' @param Data (data.table)
#'   The dataset must contain the columns:
#'   - 'IdTree' (character)
#'   - 'Diameter' (numeric)
#'   - 'Year' (numeric)
#'
#' @param EstDBH Estimated value of the diameter to correct (cm)
#'
#' @param PositiveGrowthThreshold in cm/year : a tree
#'   widening by more than x cm/year is considered abnormal (numeric, 1 value)
#'
#' @param NegativeGrowthThreshold in cm/census : The possible
#'   positive measurement error (+n) cannot be corrected until the growth
#'   appears abnormal, but a negative measurement error can be allowed until -n
#'   (a tree does not decrease). Thus the positive measurement error (+n) is
#'   "compensated". (numeric, 1 value)
#'
#' @param DBHRange DBH range in cm to take into account to select other trees in
#'   the dataset to apply "phylogenetic hierarchical" correction (Default: 10
#'   cm) (numeric, 1 value)
#'
#' @return The average growth of 'colleagues' in the DBH range (numeric)
#'
#' @export
#'
#' @examples
#' data(TestData)
#'
#' EstDBH <- 17 # Estimated DBH (cm)
#' DBHRange = 10 # DBH range (cm)
#' # Find colleagues
#' Colleagues <- TestData[
#' IdTree != "100658" & # colleagues, not the tree to correct
#' ScientificName == "Licania membranacea" & # same species as the tree to be corrected
#' (Diameter > (EstDBH - DBHRange/2) & Diameter < (EstDBH + DBHRange/2))] # Diameter or DBHCor ?
#'
#' ColleaguesCrescMean <- ComputeColleaguesGrowthMean(Colleagues = Colleagues, Data = TestData,
#'                                                    EstDBH = EstDBH,
#'                                                    PositiveGrowthThreshold = 5,
#'                                                    NegativeGrowthThreshold = -2,
#'                                                    DBHRange = DBHRange)
#'
ComputeColleaguesGrowthMean <- function(
  Colleagues,
  Data,
  EstDBH,
  PositiveGrowthThreshold,
  NegativeGrowthThreshold,
  DBHRange
){
  # Work start --------------------------------------------------------------------------------------------------------------
  # Find their cresc at these DBHs and compute the average of these crescs

  # Collect their IdTree ----------------------------------------------------------------------------------------------------
  ColleaguesId <- unique(Colleagues[, IdTree])

  # DBH seq of the Colleagues -----------------------------------------------------------------------------------------------
  ColleaguesSeq <- Data[IdTree %in% ColleaguesId]

  # Order IdTrees and times in ascending order
  ColleaguesSeq <- ColleaguesSeq[order(IdTree, Year)]

  # Compute cresc for each Colleague ----------------------------------------------------------------------------------------
  # i = "100747"
  for(i in ColleaguesId){ # i : each Colleagues ind

    # i = "100747"
    ColleaguesCresc <- ComputeIncrementation( # matrix (Rows: ind, Col: cresc)
      Var = ColleaguesSeq[IdTree %in% i, Diameter], # Diameter or DBHCor ?
      Type = "annual",
      Time = ColleaguesSeq[IdTree %in% i, Year]
    )

    ColleaguesSeq[IdTree == i, Cresc := c(NA, ColleaguesCresc)] # crescs in the Colleagues table
  }

  # Keep only rows with DBH in DBHRange -------------------------------------------------------------------------------------
  Colleagues <- ColleaguesSeq[Diameter > (EstDBH - DBHRange/2) & Diameter < (EstDBH + DBHRange/2)] # Diameter or DBHCor ?

  # Compute mean(Colleaguescresc) -------------------------------------------------------------------------------------------
  ColleaguesCrescMean <- mean(
    Colleagues[Cresc < PositiveGrowthThreshold | Cresc > NegativeGrowthThreshold]$Cresc # filter abnormal growth
  )

  return(ColleaguesCrescMean)

}
