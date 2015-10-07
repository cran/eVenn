\name{Data_Lists}
\alias{Data_Lists}
\docType{data}
\title{
Dataset of a list of 4 matrix of expression data with ratio columns and a matrix of means for a usage example of the package eVenn
}
\description{
List of 4 matrix (List_1, List_2, List_3, List_4) with IDs, annotations and a column of ratios and a matrix of means (DataMoy).
}
\usage{data("Data_Lists")}
\format{
  The format is:
List of 5
 $ DataMoy: num [1:33793, 1:6] 3.54 3.79 177.21 182.66 3.89 ...
  ..- attr(*, "dimnames")=List of 2
  .. ..$ : chr [1:33793] "17210850" "17210852" "17210855" "17210869" ...
  .. ..$ : chr [1:6] "Group_1" "Group_2" "Group_3" "Group_4" ...
 $ List_1 : chr [1:3845, 1:15] "Pcmtd1 " "---" "Cspp1 " "Tmem14a " ...
  ..- attr(*, "dimnames")=List of 2
  .. ..$ : chr [1:3845] "17210984" "17210996" "17211090" "17211396" ...
  .. ..$ : chr [1:15] "Gene" "Name" "pval" "Group_5_sample_1" ...
 $ List_2 : chr [1:2445, 1:15] "Rb1cc1 " "Pcmtd1 " "Cspp1 " "4932411L15  // 4932411L15 " ...
  ..- attr(*, "dimnames")=List of 2
  .. ..$ : chr [1:2445] "17210912" "17210984" "17211090" "17211194" ...
  .. ..$ : chr [1:15] "Gene" "Name" "pval" "Group_6_sample_1" ...
 $ List_3 : chr [1:2525, 1:15] "Terf1 " "Tfap2d " "Efhc1 " "Cfc1 " ...
  ..- attr(*, "dimnames")=List of 2
  .. ..$ : chr [1:2525] "17211243" "17211335" "17211382" "17211739" ...
  .. ..$ : chr [1:15] "Gene" "Name" "pval" "Group_2_sample_1" ...
 $ List_4 : chr [1:1558, 1:15] "Terf1 " "Khdc1c  // Khdc1c " "Khdrbs2 " "Cfc1 " ...
  ..- attr(*, "dimnames")=List of 2
  .. ..$ : chr [1:1558] "17211243" "17211423" "17211548" "17211739" ...
  .. ..$ : chr [1:15] "Gene" "Name" "pval" "Group_3_sample_1" ...
}
\references{
\url{http://blog.mrbioinfo.com/}
}
\examples{
library(eVenn)

#	List of 2, 3 or 4 matrix w/wo modulations and w/wo profils data
#   data(Data_Lists)
#   YNdisplay = TRUE # Allows commentaries and display of the main steps of the process
   
#   evenn(matLists=Data_Lists[c("List_1", "List_2", "List_3", "List_4")], annot=TRUE,
#   display=YNdisplay, CompName="Lists_4")
   
#   evenn(matLists=Data_Lists[c("List_1", "List_2", "List_3", "List_4")], ud=TRUE,
#      annot=TRUE, display=YNdisplay, CompName="Lists_4_UD")
   
#   evenn(matLists=Data_Lists[c("List_1", "List_2", "List_3", "List_4", "DataMoy")],
#      ud=TRUE, annot=TRUE, Profils=TRUE, display=YNdisplay, CompName="Lists_4_UD_Profils")
}
\keyword{datasets}
