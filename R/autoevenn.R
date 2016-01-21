autoevenn <-function(FolderPath="", annot=TRUE, ud=TRUE, VennBar=FALSE, Solid=TRUE, Profils=TRUE, prop=FALSE, display=FALSE, Ptest=FALSE, tUD=NULL, tUDp=NULL, tnoUD=NULL, ColorTxt="", Gtype="png")
{
	if(FolderPath=="")
	{
		write(paste("FolderPath is empty, it must contain the directory where are placed the Folders > Lists" , sep=""), file="")
		flush.console()
		FolderPath = choose.dir() 
		break
	}
	
	Venns = list.dirs(FolderPath, full.names=TRUE)
	
	FolderDest = paste(getwd(), "/Venn.diagrams/", sep="")
	dir.create(file.path(FolderDest), showWarnings = FALSE)
	FolderDest = paste(getwd(), "/Venn.diagrams/", basename(FolderPath), sep="")
	dir.create(file.path(FolderDest), showWarnings = FALSE)
	
	Venns = Venns[Venns!=FolderPath]  # filtre le repertoire principal de la liste
	write(paste(length(Venns), " Venn", if(length(Venns)>1){paste("s", sep="")}, sep=""), file="")
	for(V in 1:length(Venns))
	{
		write(paste(V, " / ", length(Venns), ": ", basename(Venns[V]), sep=""), file="")
		evenn(annot=annot, ud=ud, VennBar=VennBar, pathRes=FolderDest, pathLists=Venns[V], display=display, Solid=Solid, Profils=Profils, prop=prop, Ptest=Ptest, , tUD=tUD, tUDp=tUDp, tnoUD=tnoUD, ColorTxt=ColorTxt, Gtype=Gtype)
	}
}
