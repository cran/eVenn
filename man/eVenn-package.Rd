\name{eVenn-package}
\alias{eVenn-package}
\alias{eVenn}
\docType{package}
\title{
eVenn: A powerful tool made to compare huge lists almost instantaneously a plot Venn diagrams.
}
\description{
Compute a matrix combination of lists (from 2 to infinite) and plot the results in a Venn diagram if (N<=4). It is possible to produce a complete annotated file, merging the annotations of the compared lists.
}
\details{
\tabular{ll}{
Package: \tab eVenn\cr
Type: \tab Package\cr
Version: \tab 1.01\cr
Date: \tab 2010-06-14\cr
License: \tab GPL\cr
Depends: \tab plotrix\cr
LazyLoad: \tab yes\cr
}
The lists to be compared must be alone in a folder (path_lists). The lists can be just identifiers lists or be lists of genes with annotation columns and data columns. The option "annot" (T/F) allow to produces a result "counts" file with a venn diagram matrix (0/1) and the corresponding annotations and data.
The first column of the data files must be the unic identifiers, it is used by eVenn to detect common or specific genes.
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
