test_that("GeneralErrorsDetection", {

  # Load packages
  library(testthat)
  library(data.table)
  library(sf)

  # Import data
  data(TestData)

  # Create test data
  MatrixData <- as.matrix(TestData)
  MatrixData <- as.matrix(TestData)
  NoDBHData <- TestData[, !c("Diameter")]
  NoPlotData <- TestData[, !c("Plot")]

  # Create a plot polygon
  PlotPolygon <- st_as_sf(st_sfc(st_polygon(list(
  rbind(c(1, 5), c(2, 2), c(4, 1), c(4, 4), c(1, 5))))))
  st_crs(PlotPolygon) <- 4326


  # Check the function argument
  expect_error(GeneralErrorsDetection(MatrixData, PlotPolygon),
               regexp = "Data must be a data.frame or data.table")


  # Check the function work
  TestData[, IdStem := NULL]
  Rslt <- suppressWarnings(GeneralErrorsDetection(TestData, PlotPolygon))

  ## Remove *duplicated rows*
  expect_true(anyDuplicated(TestData)!= 0 & anyDuplicated(Rslt) == 0)

  ## Check *missing value* in
  # (X-YTreeUTM/PlotArea/Plot/Subplot/Year/TreeFieldNum/IdTree/Diameter/POM/HOM/Family/Genus/Species/VernName)
  Vars <- c("Plot", "Subplot", "Year", "TreeFieldNum", "IdTree", "IdStem",
            "Diameter", "POM", "HOM", "TreeHeight", "StemHeight",
            "XTreeUTM", "YTreeUTM", "Family", "Genus", "Species", "VernName")
  # v =1
  for (v in 1:length(Vars)) {
    if(Vars[v] %in% names(Rslt)){ # If the column exists
      if(!all(is.na(Rslt[,get(Vars[v])]))){ # if the column is not completely empty

        MissingVal <- is.na(Rslt[,get(Vars[v])]) # any(MissingVal)

        expect_true(all(grepl("Missing value", Rslt[MissingVal, Comment])))

      } # not empty column
    } # column exists
  } # Vars loop


  ## Check *missing value* (NA/0) in the measurement variables
  Vars <- c("Diameter", "HOM", "TreeHeight", "StemHeight")
  # v = 1
  for (v in 1:length(Vars)) {

    if(Vars[v] %in% names(Rslt)){ # If the column exists

      NullVal <- Rslt[,get(Vars[v])] %in% 0 # any(NullVal) # which(is.na(NullVal))

      expect_true(all(grepl("cannot be 0", Rslt[NullVal, Comment])))

    }
  }


  ## Check of the *unique association of the idTree with plot, TreeFieldNum subplot* (at the site scale)

  duplicated_ID <- CorresIDs <- vector("character")
  # For each site
  for (s in unique(na.omit(Rslt$Site))) {

    correspondances <- na.omit(unique(
      Rslt[Rslt$Site == s, .(IdTree, Plot, Subplot, TreeFieldNum)]
    ))

    CorresIDs <- correspondances[, IdTree] # .(IdTree) all the Idtree's having a unique P-SubP-TreeFieldNum combination

    if(!identical(CorresIDs, unique(CorresIDs))){ # check if it's the same length, same ids -> 1 asso/ID

      duplicated_ID <- unique(CorresIDs[duplicated(CorresIDs)]) # identify the Idtree(s) having several P-SubP-TreeFieldNum combinations

      NnUniqdIdTree <- (Rslt[,Site] == s
                        & Rslt[,IdTree] %in% duplicated_ID)

      expect_true(all(grepl("Non-unique association", Rslt[NnUniqdIdTree, Comment]))) # Rslt[NnUniqdIdTree]

    }
  } # end site loop

  ## Check *duplicated IdTree* in a census (at the site scale)

  DuplicatedID <- Rslt[duplicated(Rslt[, list(IdTree, Year)]), list(IdTree, Year)]

  if(nrow(DuplicatedID) > 0){

    DuplicatedID[, IDYear := paste(IdTree, Year, sep = "/")] # code to detect

    Rslt[, IDYear := paste(IdTree, Year, sep = "/")] # code to detect

    DuplIdTree <- Rslt$IDYear %in% DuplicatedID[, IDYear] # any(DuplIdTree)


    expect_true(all(grepl("Duplicated", Rslt[DuplIdTree, Comment])))
    # Rslt[DuplIdTree]

  }


  ## Check *invariant coordinates per IdTree*
  duplicated_ID <- CorresIDs <- vector("character")

  # For each site
  for (s in unique(na.omit(Rslt$Site))) {

    CoordIDCombination <- na.omit(unique(
      Rslt[Rslt$Site == s, c("IdTree", "XTreeUTM", "YTreeUTM"), with = FALSE]
    ))

    CorresIDs <- CoordIDCombination[, IdTree] # .(IdTree) all the Idtree's having a unique X-YTreeUTM) combination

    if(!identical(CorresIDs, unique(CorresIDs))){ # check if it's the same length, same ids -> 1 asso/ID

      duplicated_ID <- unique(CorresIDs[duplicated(CorresIDs)]) # identify the Idtree(s) having several P-SubP-TreeFieldNum combinations

      expect_true(all(grepl("Different coordinates", Rslt[IdTree %in% duplicated_ID, Comment])))

    }
  } # end site loop

  ## Check for trees *outside the subplot* A FAIRE

  ## Check *fix Plot and Subplot number* A FAIRE

})

# Remove *duplicated rows*
# Check *missing value* in X-YTreeUTM/PlotArea/Plot/Subplot/Year/TreeFieldNum/IdTree/Diameter/POM/HOM/Family/Genus/Species/VernName
# Check *missing value* (NA/0) in the measurement variables
# Check *duplicated TreeFieldNum* in plot-subplot association in a census (at the site scale)
# Check of the *unique association of the idTree with plot, subplot, TreeFieldNum and coordinates* (at the site scale)
# Check *duplicated idTree* in a census (at the site scale)
# Check for trees *outside the subplot*
# Check *invariant coordinates per IdTree*
# Check *fix Plot and Subplot number*
