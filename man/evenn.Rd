\name{evenn}
\alias{evenn}
\title{
A Powerful Tool to Quickly Compare Huge Lists and Draw Venn Diagrams
}
\description{
Compare lists (from 2 to infinite) and plot the results in a Venn diagram if (N<=4) with regulation details. It allows to produce a complete annotated file, merging the annotations of the compared lists. It is also possible to compute an overlaps table to show the overlaps proportions of all the couples of lists and draw proportional Venn diagrams.
}
\usage{
evenn(annot=FALSE, matLists="", pathRes="", pathLists="", ud=FALSE, prop=FALSE, 
	 noms="", overlaps=FALSE, f=0, display=FALSE, couleurs="", VennBar=FALSE, 
	 CompName="", transp=0.5, Solid=TRUE, Profils=FALSE, OnlyVariable=FALSE,
	 colBlack=FALSE, Ptest=FALSE)
}
\arguments{
  \item{annot}{
If the compared files got more than one column (not only IDs).
}
  \item{matLists}{
Matrix, list of matrix or list of lists to be compared.
}
  \item{pathRes}{
Path used to save the results. The working directory is used by default.
}
  \item{pathLists}{
Path of the data folder where are placed the files to be compared. The files must be text tabulated or csv format. 
}
  \item{ud}{
If the source files comport a ratio or fold column the details of up and down regulated genes will be shown.
}
  \item{prop}{
If the surfaces of the disks should be proportional to the size of the lists.
}
  \item{noms}{
Custom short names for each list.
}
  \item{overlaps}{
Compute the proportions of the overlaps for each couple of lists in two ways (A in B and B in A). A heatmap is created if at least three lists are processed.
}
  \item{f}{
Set a minimum overlap to produce a filtered overlaps matrix. Default f=0 is set to no filter.
}
  \item{display}{
Print commentaries during the process.
}
  \item{couleurs}{
Vector of custom colours to be used in the graphics. Default colours: c("blue", "red", "green", "orange")
}
  \item{VennBar}{
Represent the zones of a venn diagram with a bars chart even for more than 4 lists. 
}
  \item{CompName}{
Custom name for the main folder. 
}
  \item{transp}{
Percentage of transparency of the disks.
}
  \item{Solid}{
No transparancy, disks are filled with solid colours.
}
  \item{Profils}{
This option is valid only if the modulations (UD) are available (ie ratio column or each list) and a DataMoy.txt file containing the means of each group for all the items. The items will be separated by modulations profiles.
}
  \item{OnlyVariable}{
evenn expect a variable in (matList) and produce a binary matrix. (=> BinaryMatrixOut = evenn(matList=DataIn))
}
  \item{colBlack}{
No color, only Black on a white background.
}
  \item{Ptest}{
Proportional test (Fisher's exact) of the non null areas.
}
}
\value{
All the results are saved in a folder names with date and hour.
The comparison binary matrix is saved as a tabulated text file "venn_matrix.txt".
If annot is True, the resulting file (same as venn matrix with annotation given by the source lists) is saved as a text file "venn_annot.txt".
The venn diagram is saved in a png file "venn_diagram.png".
}
\references{
\url{http://blog.mrbioinfo.com/}
}
\author{
Author & Maintainer: Nicolas Cagnard <nicolas.cagnard@gmail.com>
}
\examples{
library(eVenn)
YNdisplay = TRUE	# Allows commentaries and display of the main steps of the process

#	Matrix of binary data
#   data(Data_Binary_Matrix)
#   evenn(matLists=Data_Binary_Matrix, display=YNdisplay, CompName="Binary_Matrix")

#	Matrix of folds
#   data(Data_Matrix_Of_Folds)
#   evenn(matLists=Data_Matrix_Of_Folds[,c(1,2)], display=YNdisplay, CompName="Matrix_Of_Folds")

#	Matrix of ratios
#   data(Data_Matrix_Of_Ratios)
#   evenn(matLists=Data_Matrix_Of_Ratios, display=YNdisplay, CompName="Matrix_Of_Ratios")


#	List of 2, 3 or 4 matrix w/wo modulations and w/wo profils data

#   data(Data_Lists)

#   evenn(matLists=Data_Lists[c("List_1", "List_2", "List_3", "List_4")], annot=TRUE,
#    display=YNdisplay, CompName="Lists_4")

#   evenn(matLists=Data_Lists[c("List_1", "List_2", "List_3", "List_4")], ud=TRUE,
#    annot=TRUE, display=YNdisplay, CompName="Lists_4_UD")

#   evenn(matLists=Data_Lists[c("List_1", "List_2", "List_3", "List_4", "DataMoy")],
#    ud=TRUE, annot=TRUE, Profils=TRUE, display=YNdisplay, CompName="Lists_4_UD_Profils")
}
