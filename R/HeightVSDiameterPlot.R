#' HeightVSDiameterPlot
#'
#' @param Data Dataset (data.frame or data.table)
#' @param HeightCol Height column name (character)
#' @param DiameterCol Diameter column name (character)
#'
#' @return Plot the height - diameter relationship of the stand.
#'
#' @importFrom ggplot2 ggplot aes theme_minimal geom_point labs
#'
#' @export
#'
#' @examples
#' HeightVSDiameterPlot(Data, "TreeHeight", "Diameter_TreeDataCor")

HeightVSDiameterPlot <- function(
    Data,
    HeightCol,
    DiameterCol
){

  #### Arguments check ####

  # Data ---------------------------------------------------------------------------------------------------------------
  if(!inherits(Data, c("data.table", "data.frame")))
    stop("Data must be a data.frame or data.table")

  # Columns --------------------------------------------------------------------------------------------------------------
  if(!HeightCol %in% names(Data))
    stop("Value prodive at the 'HeightCol' argument should be a column of Data")

  if(!DiameterCol %in% names(Data))
    stop("Value prodive at the 'DiameterCol' argument should be a column of Data")


  #### Function ####

  print(
    ggplot(Data, aes(x=get(DiameterCol), y=get(HeightCol))) +
      theme_minimal() +
      geom_point() +
      labs(title='Height - Diameter relationship of the stand',
           y= 'Tree height (m)', x= 'Tree diameter (cm)')
  )

}
