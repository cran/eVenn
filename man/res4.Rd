\name{res4}
\alias{res4}
\docType{data}
\title{
Four lists example dataset
}
\description{
Dataset for an example of a venn diagram with 4 lists.
}
\usage{data(res4)}
\format{
  The format is:
 num [1:3831, 1:4] 1 1 1 1 1 1 0 0 0 1 ...
 - attr(*, "dimnames")=List of 2
  ..$ : chr [1:3831] "1010050" "1010079" "1010131" "1010132" ...
  ..$ : chr [1:4] "liste1" "liste2" "liste3" "liste4"
}
\examples{
#exemple with 4 lists
data(res4)
evenn(path_lists="test", res=res4, ud=TRUE)
}
\keyword{datasets}
