#' Detect trees outside the subplot
#'
#' @param Data Dataset (data.frame or data.table)
#'
#' @param PlotDimensions Dataset with the plot dimensions
#'  (minimum and maximum X, Y coordinates) (data.frame or data.table)
#'
#' @param PlotPolygon (shapefile)
#'
#' @details Check for trees **outside the subplot**
#'
#' @return The input dataset (data.table) with a new *Comment* column with error
#'   type informations.
#'
#' @export
#'
#' @examples
#' library(data.table)
#' data("TestData")
#' PlotDim <-
#' PlotPol <- st_read("TestPlotPolygon.shp")
#' Rslt <- DetectTreesOut(TestData, PlotDimensions = PlotDim, PlotPolygon = PlotPol)
#'
DetectTreesOut <- function(
  Data,
  PlotDimensions = NULL,
  PlotPolygon = NULL
){

  #### Arguments check ####

  # Data
  if (!inherits(Data, c("data.table", "data.frame")))
    stop("Data must be a data.frame or data.table")

  # PlotDimensions
  if (!inherits(PlotDimensions, c("data.table", "data.frame")))
    stop("PlotDimensions must be a data.frame or data.table")

  # PlotPolygon
  if (!inherits(PlotPolygon, "sf"))
    stop("PlotPolygon must be a sf")

  if(terra::crs(PlotPolygon) == "")
    stop("PlotPolygon must have a crs") # needeed?

  # ---------------------------------------------------------------------------------------------------------

  Data[, Subplot := as.character(Subplot)]

  #### Function ####

  # In data.table
  setDT(Data)

  # TO DO
  # Check for trees outside the subplot ----------------------------------------
  ## Check XY coordinates against plot dimensions ------------------------------

  PlotDimensions$X
  PlotDimensions$Y

  Data %>%

  ## Check XY coordinates on the polygon ---------------------------------------

  Data_sf <- Data %>%
    filter(!is.na(XTreeUTM)) %>%
    filter(!is.na(YTreeUTM)) %>%
    st_as_sf(coords = c("XTreeUTM", "YTreeUTM")) %>%
    st_set_crs(st_crs(PlotPolygon) # check the crs !!

  st_contains(Data_sf, PlotPolygon) # if trees are within the plot polygon
  # st_intersection(Data_sf, PlotPolygon) # Identifies if trees and plot polygon geometry share any space

  return(Data)
}
