autoevenn <-
function(FolderPath="", annot=TRUE, ud=TRUE, VennBar=FALSE, Solid=TRUE, Profils=TRUE, prop=FALSE, display=FALSE)
{
	os<-Sys.info()["sysname"]
	if((os!="Windows")&(FolderPath==""))
	{
		#require(tcltk)
		write(paste("Choose the directory where are placed the Folders > Lists" , sep=""), file="")
		flush.console()
		FolderPath = tk_choose.dir()	
	}
	if((os=="Windows")&(FolderPath==""))
	{
		write(paste("Choose the directory where are placed the Folders > Lists" , sep=""), file="")
		flush.console()
		FolderPath = choose.dir() 
	}
	
	Venns = list.dirs(FolderPath, full.names=TRUE)
	
	FolderDest = paste(getwd(), "/Venn.diagrams/", basename(FolderPath), sep="")
	dir.create(file.path(FolderDest), showWarnings = FALSE)
	
	Venns = Venns[Venns!=FolderPath]  # filtre le repertoire principal de la liste
	write(paste(length(Venns), " autOeVenn", sep=""), file="")
	for(V in 1:length(Venns))
	{
		write(paste(V, " / ", length(Venns), ": ", basename(Venns[V]), sep=""), file="")
		evenn(annot=annot, ud=ud, VennBar=VennBar, pathRes=FolderDest, pathLists=Venns[V], display=display, Solid=Solid, Profils=Profils, prop=prop)
	}
}
