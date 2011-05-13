\name{res2}
\alias{res2}
\docType{data}
\title{
	Two lists example dataset
}
\description{
Dataset for an example of a venn diagram with 2 lists.
}
\usage{data(res2)}
\format{
  The format is:
 num [1:3405, 1:2] 1 1 1 1 1 1 1 1 0 0 ...
 - attr(*, "dimnames")=List of 2
  ..$ : chr [1:3405] "1010050" "1010079" "1010131" "1010132" ...
  ..$ : chr [1:2] "liste1" "liste2"
}
\examples{
#exemple with 2 lists
data(res2)
evenn(path_lists="test", res=res2, ud=TRUE)
}
\keyword{datasets}
