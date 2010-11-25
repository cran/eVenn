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
Version: \tab 1.24.1\cr
Date: \tab 2010-11-24\cr
License: \tab GPL\cr
LazyLoad: \tab yes\cr
}
The lists to be compared must be alone in a folder (path_lists). The lists can be just identifiers lists or be lists of genes with annotation columns and data columns. The option "annot" (T/F) allow to produces a result "counts" file with a venn diagram matrix (0/1) and the corresponding annotations and data.
The first column of the data files must be the unic identifiers, it is used by eVenn to detect common or specific genes.

v1.24.1: Corrected bug in the processing of the 1 column files or just ID + 1 column 
v1.24: Add the man() function for quick help or quick start and allow to custom the short names of the lists instead of "A", "B", "C", "D".
v1.23: Bugs in the ud graph corrected.
v1.22: Pb with the lists of 0 genes with the ud=TRUE corrected.
v1.21: Minor bugs reported have been corrected.
v1.2: eVenn can separate up and down regulated genes.

v1.1: No other library called.
			Lists types can be mixed, (columns numbers, file types)
			The total number of unic genes/IDs is printed in the graph.
			New column "Total lists" in the results files indicating the nbr of lists in which each ID is found.
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
evenn(path_lists="test", res=res2, ud=TRUE)

#exemple with 3 lists
data(res3)
evenn(path_lists="test", res=res3, ud=TRUE)

#exemple with 4 lists
data(res4)
evenn(path_lists="test", res=res4, ud=TRUE)
}
