#' Detect trees outside the plot
#'
#' @param Data Dataset (data.frame or data.table)
#'
#' @param PlotPolygon Plot polygon with a crs (sf)
#'
#' @details Check for trees **outside the plot**
#'
#' @return The input dataset (data.table) with a new *Comment* column with error
#'   type informations.
#'
#' @import sf
#' @importFrom ggplot2 ggplot scale_color_manual theme_classic labs
#' @importFrom ggplot2 geom_sf
#' @importFrom dplyr bind_rows
#' @importFrom terra crs
#'
#' @export
#'
#' @examples
#' library(data.table)
#' library(sf)
#'
#' PlotPolygon <- st_as_sf(st_sfc(st_polygon(list(
#' rbind(c(1, 5), c(2, 2), c(4, 1), c(4, 4), c(1, 5))))))
#' st_crs(PlotPolygon) <- 4326
#'
#' Data <- data.frame(IdTree = c('A', 'B', 'C', 'D'),
#' XTreeUTM = c(1.5, 2.5, 3, 3),
#' YTreeUTM = c(1.5, 2, 2.5, 3.5)
#' )
#' Rslt <- DetectTreesOut(Data, PlotPolygon)

DetectTreesOut <- function(
    Data,
    PlotPolygon
){

  #### Arguments check ####

  # Data
  if (!inherits(Data, c("data.table", "data.frame")))
    stop("Data must be a data.frame or data.table")

  # PlotPolygon
  if (!inherits(PlotPolygon, "sf"))
    stop("PlotPolygon must be a sf")

  if(terra::crs(PlotPolygon) == "")
    stop("PlotPolygon must have a crs") # needeed

  # ---------------------------------------------------------------------------------------------------------

  #### Function ####

  # In data.table
  setDT(Data)

  PlotPolygon <- st_union(PlotPolygon) # 1 polygon only

  # Filter only located trees
  Data_loc <- Data[!is.na(XTreeUTM) & !is.na(YTreeUTM)]
  Data_unloc <- Data[is.na(XTreeUTM) | is.na(YTreeUTM)]

  # Convert to sf
  Data_sf <- Data_loc[, st_as_sf(.SD, coords = c("XTreeUTM", "YTreeUTM"))
  ]
  st_crs(Data_sf) <- st_crs(PlotPolygon) # set CRS

  # Identifies if trees are in the plot polygon --------------------------------
  intersections <- st_intersects(Data_sf, PlotPolygon, sparse = FALSE)[,1]
  Data_loc[, InPlot := intersections]

  # Bind located and unlocated data as initialy
  Data <- dplyr::bind_rows(Data_loc, Data_unloc)

  # Flag errors
  Data <- GenerateComment(Data,
                          condition =
                            Data[,!InPlot],
                          comment = "Tree outside the plot")

  if(!all(Data_loc$InPlot))
    warning(sum(!Data_loc$InPlot), " tree(s) is/are outside the plot")

  # Filter + reconvert to sf
  Data_sf <- Data_loc[, st_as_sf(.SD, coords = c("XTreeUTM", "YTreeUTM"))]
  st_crs(Data_sf) <- st_crs(PlotPolygon)

  # Plot the result -----------------------------------------------------------
  print(
    ggplot() +
      ggplot2::geom_sf(data = Data_sf[Data_sf$InPlot,], size = 1, aes(col= "TRUE")) +
      ggplot2::geom_sf(data = Data_sf[!Data_sf$InPlot,], size = 1, aes(col= "FALSE")) +

      geom_sf(data = sf::st_cast(PlotPolygon, "LINESTRING")) +
      scale_color_manual(values = c("TRUE" = "grey",
                                    "FALSE"= "red")) +
      theme_classic() +
      labs(title = "Trees in/out the plot", col = "Trees in the plot")
  )

  return(Data)
}
