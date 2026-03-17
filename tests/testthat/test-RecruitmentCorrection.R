test_that("RecruitmentCorrection", {

  # Import data
  library(data.table)
  data(TestData)

  # Create test data
  MatrixData <- as.matrix(TestData)
  NoDBHData <- TestData[, !c("Diameter")]
  NoDBHCorData <- TestData

  setnames(TestData, "Diameter", "Diameter_TreeDataCor")


  # Check the function argument
  expect_error(RecruitmentCorrection(MatrixData),
               regexp = "Data must be a data.frame or data.table")

  expect_error(RecruitmentCorrection(TestData, MinDBH = c(5, 10, 20), PositiveGrowthThreshold = TRUE),
               regexp = "The 'MinDBH' and 'PositiveGrowthThreshold'' arguments
         of the 'RecruitmentCorrection' function must be 1 numeric value each")

  expect_error(RecruitmentCorrection(TestData, InvariantColumns = c(1:3)),
               regexp = "'InvariantColumns' argument must be of character class")

  expect_error(RecruitmentCorrection(TestData,
                                     DetectOnly = "no"),
               regexp = "The 'DetectOnly' argument
         of the 'RecruitmentCorrection' function must be logical")

  expect_error(RecruitmentCorrection(NoDBHData),
               regexp = "column doesn't exist in the dataset.")


  expect_error(RecruitmentCorrection(TestData, InvariantColumns = "a"),
               regexp = "InvariantColumns argument must contain one or several column names")



  # Check the function works
  Rslt <- RecruitmentCorrection(TestData,
                                InvariantColumns = c("Site",
                                                     "Genus",
                                                     "Species",
                                                     "Family",
                                                     "ScientificName"))

})
