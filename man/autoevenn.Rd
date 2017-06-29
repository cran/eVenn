\name{autoevenn}
\alias{autoevenn}
\title{
Automates the process of several eVenn
}
\description{
This function creates a list of folders where are placed sources files for venn diagrams and call the eVenn function for each one. 
}
\usage{
autoevenn(FolderPath="", pathRes="", annot=TRUE, ud=TRUE, prop=FALSE, overlaps=FALSE,
display=FALSE, couleurs="", VennBar=FALSE, transp=0.5, filled=TRUE, Profils=FALSE,
ColorTxt="", colBlack=FALSE, Ptest=FALSE, tUD=NULL, tUDp=NULL, tnoUD=NULL,
Gtype="png", lw=1, NutShell=TRUE, VennClust=TRUE, OnlyVenn=FALSE)
}
\arguments{
  \item{FolderPath}{
Path where are placed the source data folders.
}
  \item{pathRes}{
Path used to save the results. The working directory is used by default.
}
  \item{annot}{
If the compared files got more than one column (not only IDs).
}
  \item{ud}{
If the source files comport a ratio or fold column the details of up and down regulated genes will be shown.
}
  \item{prop}{
If the surfaces of the disks should be proportional to the size of the lists.
}
  \item{overlaps}{
Compute the proportions of the overlaps for each couple of lists in two ways (A in B and B in A). A heatmap is created if at least three lists are processed.
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
  \item{transp}{
Percentage of transparency of the disks.
}
  \item{filled}{
No transparancy, disks are filled with solid colours.
}
  \item{Profils}{
This option is valid only if the modulations (UD) are available (ie ratio column or each list) and a DataMoy.txt file containing the means of each group for all the items. The items will be separated by modulations profiles.
}
  \item{colBlack}{
No color, only Black on a white background.
}
  \item{ColorTxt}{
Custom color of the counts.
}
  \item{Ptest}{
Proportional test (Fisher's exact) of the non null areas.
}
  \item{tUD}{
Font size of the count and profile names of specific zones.
}
  \item{tUDp}{
Font size of the count and profile names of common zones.
}
  \item{tnoUD}{
Font size of the total count of a zone.
}
  \item{Gtype}{
File format of the graphic (png or svg).
}
  \item{lw}{
Line width relative to the default (default=1). 2 is twice as wide.
}
  \item{NutShell}{
All the Venn zones and UD profiles in a nutshell (count table).
}
  \item{VennClust}{
Creates two clusterings (using Spearman correlation similarity measure and average or Ward.D2 linkage algorithm) to model links between the lists.
}
  \item{OnlyVenn}{
Draw only the venn diagram without any text outside (remove the total and lists names).
}
}
\references{
\url{http://blog.mrbioinfo.com/}
}
\author{
Author & Maintainer: Nicolas Cagnard <nicolas.cagnard@gmail.com>
}
