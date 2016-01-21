\name{eVenn-package}
\alias{eVenn-package}
\alias{eVenn}
\docType{package}
\title{
A Powerful Tool to Quickly Compare Huge Lists and Draw Venn Diagrams
}
\description{
Compare lists (from 2 to infinite) and plot the results in a Venn diagram if (N<=4) with regulation details. It allows to produce a complete annotated file, merging the annotations of the compared lists. It is also possible to compute an overlaps table to show the overlaps proportions of all the couples of lists and draw proportional Venn diagrams.
}
\details{
\tabular{ll}{
Package: \tab eVenn\cr
Type: \tab Package\cr
Version: \tab 2.2.3\cr
Date: \tab 2016-01-21\cr
License: \tab GPL\cr
LazyLoad: \tab yes\cr
}
}
\author{
Author & Maintainer: Nicolas Cagnard <nicolas.cagnard@gmail.com>
}
\references{
\url{http://blog.mrbioinfo.com/}
}
\keyword{ package }
\examples{
library(eVenn)
YNdisplay = TRUE	# Allows commentaries and display of the main steps of the process

#	Matrix of binary data
   data(Data_Binary_Matrix)
   evenn(matLists=Data_Binary_Matrix, display=YNdisplay, CompName="Binary_Matrix")

#	Matrix of folds
#   data(Data_Matrix_Of_Folds)
#   evenn(matLists=Data_Matrix_Of_Folds, display=YNdisplay, CompName="Matrix_Of_Folds")

#	Matrix of ratios
#   data(Data_Matrix_Of_Ratios)
#   evenn(matLists=Data_Matrix_Of_Ratios, display=YNdisplay, CompName="Matrix_Of_Ratios")


#	List of 2, 3 or 4 matrix w/wo modulations and w/wo profils data

   data(Data_Lists)

#   evenn(matLists=Data_Lists[c("List_1", "List_2", "List_3", "List_4")], annot=TRUE,
#    display=YNdisplay, CompName="Lists_4")

#   evenn(matLists=Data_Lists[c("List_1", "List_2", "List_3", "List_4")], ud=TRUE,
#    annot=TRUE, display=YNdisplay, CompName="Lists_4_UD")

#   evenn(matLists=Data_Lists[c("List_1", "List_2", "List_3", "List_4", "DataMoy")],
#    ud=TRUE, annot=TRUE, Profils=TRUE, display=YNdisplay, CompName="Lists_4_UD_Profils")
}
