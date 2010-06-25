\name{res3}
\alias{res3}
\docType{data}
\title{
Three lists example dataset
}
\description{
Dataset for an example of a venn diagram with 3 lists.
}
\usage{data(res3)}
\format{
  The format is:
 num [1:3415, 1:3] 1 1 1 1 1 1 1 1 0 0 ...
 - attr(*, "dimnames")=List of 2
  ..$ : chr [1:3415] "1010050" "1010079" "1010131" "1010132" ...
  ..$ : chr [1:3] "liste1" "liste2" "liste3"
}
\examples{
#exemple with 3 lists
data(res3)
evenn(annot=F, path_res="", path_lists="test", res=res3)
}
\keyword{datasets}
