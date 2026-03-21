#' DistributionPlot
#'
#' @param Data Dataset (data.frame or data.table)
#' @param InitialCol Initial column name (character)
#' @param CorCol Corrected column name (character)
#'
#' @return The plots of the initial and/or corrected values distribution of the
#' stand.
#'
#' @importFrom ggplot2 ggplot aes theme_minimal geom_histogram labs
#'
#' @export
#'
#' @examples
#' DistributionPlot(Data, InitialCol = "Diameter",
#'                        CorCol = "Diameter_TreeDataCor")

DistributionPlot <- function(
    Data,
    InitialCol = NULL,
    CorCol = NULL
){

  #### Arguments check ####

  # Data ---------------------------------------------------------------------------------------------------------------
  if(!inherits(Data, c("data.table", "data.frame")))
    stop("Data must be a data.frame or data.table")

  # Columns --------------------------------------------------------------------------------------------------------------
  if(!CorCol %in% names(Data))
    stop("Value prodive at the 'CorCol' argument should be a column of Data")

  if(!InitialCol %in% names(Data))
    stop("Value prodive at the 'InitialCol' argument should be a column of Data")


  #### Function ####

  if(!is.null(InitialCol)){
    print(
      ggplot(Data, aes(x=get(InitialCol))) +
        theme_minimal() +
        geom_histogram(binwidth=1, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
        labs(title=paste0(InitialCol,' distribution of the stand'),
             y= 'Number of individuals', x= InitialCol)
    )
  }

  if(!is.null(CorCol)){
    print(
      ggplot(Data, aes(x=get(CorCol))) +
        theme_minimal() +
        geom_histogram(binwidth=1, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
        labs(title=paste0(CorCol,' distribution of the stand'),
             y= 'Number of individuals', x= CorCol)
    )
  }

}
