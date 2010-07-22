\name{eVenn-package}
\alias{eVenn-package}
\alias{eVenn}
\docType{package}
\title{
eVenn: A powerful tool to compare huge lists almost instantaneously by Venn diagrams.
}
\description{
Compute a matrix combination of lists (from 2 to infinite) and plot the results in a Venn diagram if (N<=4). It is possible to produce a complete annotated file, merging the annotations of the compared lists.
}
\details{
\tabular{ll}{
Package: \tab eVenn\cr
Type: \tab Package\cr
Version: \tab 1.21\cr
Date: \tab 2010-07-22\cr
License: \tab GPL\cr
LazyLoad: \tab yes\cr
}
The lists to be compared must be alone in a folder (path_lists). The lists can be just identifiers lists or be lists of genes with annotation columns and data columns. The option "annot" (T/F) allow to produces a result "counts" file with a venn diagram matrix (0/1) and the corresponding annotations and data.
The first column of the data files must be the unic identifiers, it is used by eVenn to detect common or specific genes.

v1.21: Minor bugs reported have been corrected.
v1.2: eVenn can separate up and down regulated genes.

v1.1: No other library called.
			Lists types can be mixed, (columns numbers, file types)
			The total number of unic genes/IDs is printed in the graph.
			New column "Total lists" in the results files indicating the nbr of lists in which each ID is found.
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
evenn(annot=FALSE, path_res="", path_lists="test", res=res2, ud=TRUE)

#exemple with 3 lists
data(res3)
evenn(annot=FALSE, path_res="", path_lists="test", res=res3, ud=TRUE)

#exemple with 4 lists
data(res4)
evenn(annot=FALSE, path_res="", path_lists="test", res=res4, ud=TRUE)
}
