\name{Data_Binary_Matrix}
\alias{Data_Binary_Matrix}
\docType{data}
\title{
Dataset of a binary matrix for a usage example of the package eVenn
}
\description{
A matrix of 4 columns * 10493 lines filled with 0 and 1. 0 means that the item (line) does not belong to the list (column), 1 if it belongs.
}
\usage{data("Data_Binary_Matrix")}
\format{
  The format is:
 int [1:10493, 1:4] 0 1 0 0 0 0 0 0 0 0 ...
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
#   data(Data_Binary_Matrix)
#   evenn(matLists=Data_Binary_Matrix, display=YNdisplay, CompName="Binary_Matrix")
}
\keyword{datasets}
