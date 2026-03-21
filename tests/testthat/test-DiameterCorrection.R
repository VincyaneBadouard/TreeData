test_that("DiameterCorrection", {

  # Import data ---------------------------------------------------------------------------------------------------------------------
  library(testthat)
  library(TreeData)
  library(data.table)
  data(TestData)

  # Remove other errors types (non-unique idTree, missing Year)
  TestData <- TestData[, HOM := NULL]
  TestData <- TestData[!IdTree %in% c("100898", "101686")]

  # Create test data ----------------------------------------------------------------------------------------------------------------
  MatrixData <- as.matrix(TestData)
  NoDBHData <- TestData[, !c("Diameter")]
  HOMData <- copy(TestData[IdTree == "100658"])
  HOMData[, HOM := 1.3] # data with HOM
  POMData <- copy(TestData[IdTree == "100658"])
  POMData[, POM := as.factor(1)] # data with POM, without HOM


  # Check the function argument -----------------------------------------------------------------------------------------------------

  expect_error(DiameterCorrection(MatrixData),
               regexp = "Data must be a data.frame or data.table")

  expect_error(DiameterCorrection(NoDBHData),
               regexp = "column does't exist in the dataset")

  expect_error(DiameterCorrection(TestData, DefaultHOM = "a",
                                  MaxDBH = c(1,2),
                                  PositiveGrowthThreshold = c("a", "b"),
                                  NegativeGrowthThreshold = "-2",
                                  PioneersGrowthThreshold = F,
                                  DBHRange = "10",
                                  MinIndividualNbr = "5"),
               regexp = "The 'PositiveGrowthThreshold', 'NegativeGrowthThreshold', 'PioneersGrowthThreshold' and 'DefaultHOM' arguments
         of the 'DiameterCorrection' function must be 1 numeric value each")

  expect_error(DiameterCorrection(TestData, Pioneers = T),
               regexp = "'Pioneers' argument must be a characters vector, or NULL")

  expect_error(DiameterCorrection(TestData, WhatToCorrect = "diameter"),
               regexp = "The 'WhatToCorrect' argument value must be among 'POM change', 'punctual' and 'shift'")

  expect_error(DiameterCorrection(TestData, CorrectionType = "best"),
               regexp = "The 'CorrectionType' argument value must be among
         'quadratic', 'linear', 'individual' and 'phylogenetic hierarchical'")

  expect_warning(DiameterCorrection(TestData, Digits = 1.2),
                 regexp = "The 'Digits' argument must be an integer")

  expect_error(DiameterCorrection(TestData, DetectOnly = "TRUE"),
               regexp = "The 'DetectOnly' argument must be a logical")

  expect_message(DiameterCorrection(POMData, CorrectionType = "linear", WhatToCorrect = "punctual"),
                 regexp = "You have the 'POM' information in your dataset")



  # Check the function work ---------------------------------------------------------------------------------------------------------

  ## Detect Only --------------------------------------------------------------------------------------------------------------------
  Rslt <- DiameterCorrection(
    TestData,
    CorrectionType = "phylogenetic hierarchical",
    DetectOnly = TRUE)

  # No correction, only comments
  expect_true(!"Diameter_TreeDataCor" %in% names(Rslt) & "Comment" %in% names(Rslt))

  ## Correction --------------------------------------------------------------------------------------------------------------------
  # options(warn = 2) # trace warning
  # options(warn = 0) # when debug is over

  Rslt <- DiameterCorrection(
    TestData,

    PositiveGrowthThreshold = 5,
    NegativeGrowthThreshold = -2,

    Pioneers = c("Cecropia","Pourouma"),
    PioneersGrowthThreshold = 7.5,

    WhatToCorrect = c("POM change", "punctual", "shift"),
    CorrectionType = c("quadratic", "linear", "phylogenetic hierarchical"),

    DBHRange = 10,
    MinIndividualNbr = 1,
    Digits = 2L,

    DetectOnly = FALSE)

  # Growth > 5 cm DBH/year & < -2 cm DBH/census

  # Comment and Methode if correction
  expect_true(all(!is.na(Rslt[Diameter_TreeDataCor != round(Diameter, digits = 2L), DiameterCorrectionMeth]))) # method when the DBH has been corrected

  # expect_true(all(Rslt[Diameter_TreeDataCor != round(Diameter, digits = 2L), Comment] != "")) # comment when the DBH has been corrected



})

