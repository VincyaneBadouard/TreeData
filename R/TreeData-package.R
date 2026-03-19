#' TreeData-package
#'
#' Forest Inventories Harmonization & Correction
#'
#' @name TreeData
#' @docType package
#'
#' @section TreeData functions:
#' RequiredFormat
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
### quiets concerns of R CMD check "no visible binding for global variables"
utils::globalVariables(c("Site", "Plot", "Subplot", "PlotArea", "SubplotArea", "PlotSubNum",
                         "XTreeUTM", "YTreeUTM", "XTreePlot", "YTreePlot", "XTreeSubplot", "YTreeSubplot",

                         "CensusYear", "Date", "CensusDate", "Month", "Day", "IdCensus",
                         "Year", "Time", "DateOriginal", "CensusDateOriginal", "SitYearID",

                         "IdTree", "IdStem", "StemFieldNum", "TreeFieldNum", "NewIdTree",

                         "Circ", "Diameter", "MinDBH", "TreeHeight",
                         "POM", "HOM",


                         "Family", "VernName", "Genus", "Species", "Subspecies", "ScientificName",


                         "LifeStatus", "CommercialSp", "LifeStatusOriginal", "CommercialSpOriginal",



                         "BD", "BCirc", "BPOM", "BHOM",

                         ".", ".N", ".SD", ".NATURAL", "..ColumnsToReturn",

                         "IDYear", "V1", "MaxHOM", "MaxDate", "POMChange",

                         "LifeStatusCor", 'LifeStatus_TreeDataCor', "Comment",
                         "TaperCorDBH", "TaperDBH_TreeDataCor",
                         "CorrectedRecruit",
                         "DBHCor", "Diameter_TreeDataCor", "DiameterCorrectionMeth", "Cresc",
                         "HOMCor", "POMCor",
                         "GenusCor", "SpeciesCor", "ScientificNameCor", "FamilyCor", "VernNameCor",
                         "New.Genus", "New.Species", "Taxonomic.status", "Typo", "family",
                         "Taxon", "GenspFamily", "taxonomicStatus", "spec.name", "scientificName",
                         "Old.status", "spec.name.ORIG", "BotaCorSource",
                         "BotanicalCorrectionSource", "FamilyCorSource"
))

#' @importFrom dplyr bind_rows
#' @importFrom ggplot2 geom_sf
#' @importFrom ggplot2 scale_color_manual
#' @importFrom ggplot2 theme_classic
#' @importFrom ggplot2 labs

## usethis namespace: end
NULL
