\name{autoevenn}
\alias{autoevenn}
\title{
Automates the process of several eVenn
}
\description{
This function creates a list of folders where are placed sources files for venn diagrams and call the eVenn function for each one. 
}
\usage{
autoevenn(FolderPath = "", annot = TRUE, ud = TRUE, VennBar = FALSE,
Solid = TRUE, Profils = TRUE, prop = FALSE, display = FALSE, Ptest=FALSE,
tUD=NULL, tUDp=NULL, tnoUD=NULL, ColorTxt="", Gtype="png")
}
\arguments{
  \item{FolderPath}{
Path where are placed the source data folders.
}
  \item{annot}{
If the compared files got more than one column (not only IDs).
}
  \item{ud}{
If the source files comport a ratio or fold column the details of up and down regulated genes will be shown.
}
  \item{VennBar}{
Represent the zones of a venn diagram with a bars chart even for more than 4 lists.
}
  \item{Solid}{
No transparancy, disks are filled with solid colours.
}
  \item{Profils}{
This option is valid only if the modulations (UD) are available (ie ratio column or each list) and a DataMoy.txt file containing the means of each group for all the items. The items will be separated by modulations profiles.
}
  \item{prop}{
If the surfaces of the disks should be proportional to the size of the lists.
}
  \item{display}{
Print commentaries during the process.
}
  \item{Ptest}{
Proportional test (Fisher's exact) of the non null areas.
}
  \item{tnoUD}{
Font size of the total count of a zone.
}
  \item{tUDp}{
Font size of the count and profile names of common zones.
}
  \item{tUD}{
Font size of the count and profile names of specific zones.
}
  \item{ColorTxt}{
Custom color of the counts.
}
  \item{Gtype}{
File format of the graphic (png or svg).
}
}
\references{
\url{http://blog.mrbioinfo.com/}
}
\author{
Author & Maintainer: Nicolas Cagnard <nicolas.cagnard@gmail.com>
}
