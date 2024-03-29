% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RecruitmentCorrection.R
\name{RecruitmentCorrection}
\alias{RecruitmentCorrection}
\title{RecruitmentCorrection}
\usage{
RecruitmentCorrection(
  Data,
  KeepMeas = c("MaxHOM", "MaxDate"),
  MinDBH = 10,
  PositiveGrowthThreshold = 5,
  InvariantColumns = c("Site", "Genus_TreeDataCor", "Species_TreeDataCor",
    "Family_TreeDataCor", "ScientificName_TreeDataCor"),
  DetectOnly = FALSE
)
}
\arguments{
\item{Data}{(data.frame or data.table)
The dataset should preferably contain the column of corrected diameters:
'DBH_TreeDataCor', otherwise the function will take the column 'Diameter'}

\item{KeepMeas}{In case of \strong{multiple diameter measurements} in the same
census year, on which to apply the correction:
Possible values: "MaxHOM", "MaxDate" (character).
\itemize{
\item "MaxHOM": apply the correction to the measurement taken at the
\strong{highest POM}
\item "MaxDate": apply the correction to the \strong{most recent measurement} (same
year but more recent date)
}}

\item{MinDBH}{Minimum diameter of trees inventoried according to your
protocol (in cm) (numeric, 1 value) (Default = 10 cm)}

\item{PositiveGrowthThreshold}{A tree widening by more than x cm/year is
considered abnormal (numeric, 1 value) (Default = 5 cm)}

\item{InvariantColumns}{Vector with the names of the columns that are
supposed to have always the same value for each measurement of the same
tree (character)}

\item{DetectOnly}{TRUE: Only detect errors, FALSE: detect and correct errors
(logical)}
}
\value{
Add rows for forgotten recruits with them estimated DBH in the
'Diameter_TreeDataCor' column, create a 'CorrectedRecruit' col (logical)
and fill the 'Comment' column : "This DBH is/was the 1st recorded for this
tree, according to its annual growth and the census done for this plot, it
should have been recruited earlier according to your protocol (MinDBH)."
}
\description{
RecruitmentCorrection
}
\details{
If the size of the tree has never changed, or if there is only one
value, no recruits are added. If the Diameter has not been corrected
('Diameter_TreeDataCor' column does not exist), the function will create it
for the forgotten recruits. It is strongly recommended to correct the
Diameter before correcting the recruits
}
\examples{
library(data.table)
data(TestData)
setnames(TestData, "Diameter", "Diameter_TreeDataCor")

Rslt <- RecruitmentCorrection(TestData,
                              InvariantColumns = c("Site",
                                                   "Genus",
                                                   "Species",
                                                   "Family",
                                                   "ScientificName"))
IdCorr <- Rslt[CorrectedRecruit == TRUE, IdTree]
TreesCorr <- Rslt[IdTree \%in\% IdCorr]

library(ggplot2)
ggplot(TreesCorr) +
aes(x = Year, y = Diameter_TreeDataCor) +
  geom_line(size = 0.5, colour = "#112446") +
  geom_point(shape = "circle", size = 1.5, mapping = aes(color = CorrectedRecruit)) +
  theme_minimal() +
  facet_wrap(vars(IdTree), scales = "free")

}
