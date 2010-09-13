\name{evenn}
\alias{evenn}
\title{
eVenn: A powerful to compare huge lists almost instantaneously and draw Venn diagrams.
}
\description{
Compute a matrix combination of lists (from 2 to infinite) and plot the results in a Venn diagram if (N<=4). It is possible to produce a complete annotated file, merging the annotations of the compared lists.
}
\usage{
evenn(annot = FALSE, path_res = "", path_lists = "", res = "", ud = FALSE)
}
\arguments{
  \item{annot}{
If the lists to be compared are files with identifiers (used for comparisons) and data (annotations or whatever). The identifiers data (Gene name and extended name) must be the 2nd and 3rd columns.
}
  \item{path_res}{
Path used to save the results. The working directory is used by default.
}
  \item{path_lists}{
Path used to find the lists to be compared. There is no default. The files must be text tabulated or csv format. 
}
  \item{res}{
Binary matrix showing which identifier belong to which lists. Columns are compared lists, rows are identifiers.
}
  \item{ud}{
If the details of up and down regulated genes should be shown. Then the source files must comport a "ratios" column.
}
}
\value{
All the results are saved in a folder names with date and hour.
The comparison bunary matrix is saved as a csv file "venn_matrix.csv".
If annot is True, the resulting file (same as venn matrix with annotation given by the source lists) is saved as a text file "venn_annot.txt".
The venn diagram is saved in a pdf file "venn_diagram.pdf".
The venn diagram with up/down informations is saved in a pdf file "venn_diagram_ud.pdf".
}
\references{
\url{http://blog.mrbioinfo.com/}
}
\author{
Author & Maintainer: Nicolas Cagnard <nicolas.cagnard@gmail.com>
}
\examples{
#exemple with 2 lists
data(res2)
evenn(annot=FALSE, path_res="", path_lists="test", res=res2, ud=TRUE)

#exemple with 3 lists
data(res3)
evenn(annot=FALSE, path_res="", path_lists="test", res=res3, ud=TRUE)

#exemple with 4 lists
data(res4)
evenn(annot=FALSE, path_res="", path_lists="test", res=res4, ud=TRUE)
}
