\name{Data_Matrix_Of_Ratios}
\alias{Data_Matrix_Of_Ratios}
\docType{data}
\title{
Dataset of a matrix of ratios for a usage example of the package eVenn
}
\description{
A matrix of 4 columns * 10493 lines filled with ratios or NA. NA means that the item (line) does not belong to the list (column), there is a ratio if it belongs.
}
\usage{data("Data_Matrix_Of_Ratios")}
\format{
  The format is:
 num [1:10493, 1:4] NA 1.94 NA NA NA ...
 - attr(*, "dimnames")=List of 2
  ..$ : chr [1:10493] "7896736" "7896742" "7896754" "7896798" ...
  ..$ : chr [1:4] "List_1" "List_2" "List_3" "List_4"
}
\references{
\url{http://blog.mrbioinfo.com/}
}
\examples{
#   library(eVenn)
#   YNdisplay = TRUE	# Allows commentaries and display of the main steps of the process
#   data(Data_Matrix_Of_Ratios)
#   evenn(matLists=Data_Matrix_Of_Ratios, display=YNdisplay, CompName="Matrix_Of_Ratios")
}
\keyword{datasets}
