\name{evenn}
\alias{evenn}
\title{
eVenn: A powerful tool made to compare huge lists almost instantaneously a plot Venn diagrams.
}
\description{
Compute a matrix combination of lists (from 2 to infinite) and plot the results in a Venn diagram if (N<=4). It is possible to produce a complete annotated file, merging the annotations of the compared lists.
}
\usage{
evenn(annot, path_res, path_lists, res = "")
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
Binary matrix showing of which identifier belong to which lists. Columns are lists, rows are identifiers.
}
}
\value{
All the results are saved in a folder names with date and hour.
The comparison bunary matrix is saved as a csv file "venn_matrix.csv".
If annot is True, the resulting file (same as venn matrix with annotation given by the source lists) is saved as a text file "venn_annot.txt".
The venn diagram is saved in a pdf file "venn_diagram.pdf".
}
\references{
http://blog.mrbioinfo.com/
}
\author{
Author & Maintainer: Nicolas Cagnard <nicolas.cagnard@gmail.com>
}
\examples{
#exemple with 2 lists
data(res2)
evenn(annot=F, path_res="", path_lists="test", res=res2)

#exemple with 3 lists
data(res3)
evenn(annot=F, path_res="", path_lists="test", res=res3)

#exemple with 4 lists
data(res4)
evenn(annot=F, path_res="", path_lists="test", res=res4)
}
