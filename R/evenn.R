evenn <-
function(annot=FALSE, matLists="", pathRes="", pathLists="", ud=FALSE, prop=FALSE, noms="", overlaps=FALSE, f=0, display=FALSE, couleurs="", VennBar=FALSE, CompName="", transp=0.5, Solid=TRUE, Profils=FALSE, OnlyVariable=FALSE, colBlack=FALSE)
{  
	if(OnlyVariable)
	{
		FilesOut=FALSE
		display=FALSE
	}else{
		FilesOut=TRUE
	}
	
	if(is.character(matLists))	# Source = lecture de fichiers
	{
		if(pathLists == "")	# path source non renseigne
		{
			if(Sys.info()["sysname"]!="Windows")	# Linux
			{
				# require(tcltk)
				write(paste("Choose the directory where are placed the lists" , sep=""), file="")
				flush.console()
				pathLists = tk_choose.dir()
			}else{
				write(paste("Choose the directory where are placed the lists" , sep=""), file="")
				flush.console()
				pathLists = choose.dir()  
			}  
		}
		listes = list.files(path = pathLists, full.names = TRUE)
		if(length(listes)>1)
		{
			matLists = list()
			for(L in 1:length(listes))	matLists[[length(matLists)+1]] = as.matrix(read.table(listes[L], header=TRUE, row.names=1, sep="\t"))
			names(matLists) = paste(substr(basename(listes), 0, (nchar(basename(listes))-4)), sep="")
			if(sum(grepl("DataMoy", names(matLists)))==0)	Profils=FALSE	
		}else{
			matLists = as.matrix(read.table(listes[1], header=TRUE, row.names=1, sep="\t"))
			Profils=FALSE
		}		
	}
	
	if(display&FilesOut){
		write("        ,.-.,                                                                         ", file="")
		write("      .`     `.                                                                       ", file="")
		write("     /.-., ,.-.`            *       *                                 ****     ****   ", file="")
		write("   .`    .`.    `.     ***   *     *   ***    ****   ****    *     * *    *   *    *  ", file="")
		write("  / `.  /   `.  / `  *     *  *   *  *     * *    * *    *    *   *      *        *   ", file="")
		write(" |    ',_____,'    | ******   *   *  ******  *    * *    *     * *      *        *    ", file="")
		write(" `.     `   /     /  *         * *   *       *    * *    *     * *    *        *      ", file="")
		write("   ',    '_'    ,'    *****     *     *****  *    * *    *      *    ****** * ******* ", file="")
		write("     `'-'` `'-'`                                                                      ", file="")
		#write("\n\t[Run man.evenn() for quick help]\n", file="")
		flush.console()  
	}                                       
	options(warn=-1)
	res=""
	#	if((Sys.info()["sysname"]!="Windows")&(path_lists==""))  require(tcltk)
	
	########################################################################################################
	########################################################################################################
	#
	# Fonctions
	
	BarVenn<-function(resTmp, path, ud)
	{
		path = paste(path, "/BarVenn", sep="")
		dir.create(path)
		#noms = toupper(letters[1:(grep("total", colnames(resTmp), ignore.case=TRUE)-1)])  # Remlace le nom des listes par A, B, C, D, ...
		nmstmp = colnames(resTmp)
		#noms=paste(rep(toupper(letters), ceiling((grep("total", nmstmp, ignore.case=TRUE)-1)/length(letters))),
		#  c(rep("", length(letters)), rep(1:floor((grep("total", nmstmp, ignore.case=TRUE)-1)/length(letters)), each=length(letters))), COLLAPSE=NULL, sep="")[1:(ncol(resTmp)-1)]
		
		noms = toupper(letters[1:(grep("total", nmstmp, ignore.case=TRUE)-1)])
		colnames(resTmp)[1:(grep("total", nmstmp, ignore.case=TRUE)-1)] = noms  # Remplace les noms par les A, B, C, D, ...
		
		tableau = matrix(0, ncol=5, nrow=0)                                         # Creation du tableau des effectifs des zones et profils
		colnames(tableau) = c("zone", "profile", "N", "tot", "size")
		
		for(N in 1:length(noms))                                                    # Nbre de listes mlises en jeu
		{
			zones = combn(noms, N)                                                    # Nbre de zones impliquant N listes de noms
			for(Z in 1:ncol(zones))                                                   # Parcours des zones pour completer le tableau:
			{
				selection = (apply(as.matrix(resTmp[,c(zones[1:N,Z])]), 1, function(x) sum(as.numeric(x)))==N)&(resTmp[,grep("total", colnames(resTmp), ignore.case=TRUE)]==N)
				if(ud)
				{
					cols = seq(1, length(noms), by=1)[duplicated(c(zones[1:N,Z], noms))[(length(zones[1:N,Z])+1):(length(zones[1:N,Z])+length(noms))]]+(1+length(noms)) # Postion des colonnes de ratio
					
					if(sum(selection)!=0)
					{
						ratiosTmp = resTmp[selection,cols] # Recupere les colonnes de ratios correpondant aux listes de la zone
						if(sum(selection)==1)
						{
							ratiosTmp = t(as.matrix(ratiosTmp))
							colnames(ratiosTmp) = zones[,Z]
						}
					}
				}
				if(sum(selection)>0) # Si la zone n'est pas vide
				{
					if(ud)
					{
						UDtmp = ratiosTmp                                                   # Cree une matrice de taille identique
						UDtmp[ratiosTmp>1]="U"                                              # Remplace les ratios par le profil
						UDtmp[ratiosTmp<1]="D"
						UDtmp[ratiosTmp==1]="n"
						UDtmp = table(apply(as.matrix(UDtmp), 1, function(x) paste(x, sep="", collapse=",")))  # Collapse les colonnes/listes pour creer le profil complet
						tableau = rbind(tableau, cbind(rep(paste(zones[1:N,Z], sep="", collapse=","), length(UDtmp)),  # Repete le nom de zone pour toutes les lignes de profil
										as.matrix(names(UDtmp)),             # Profil
										as.matrix(UDtmp),                    # Effectif du profil
										rep(sum(UDtmp), length(UDtmp)),      # Effectif total de la zone
										rep(N, length(UDtmp))))              # Nbre de listes = type de zone
					}else{
						tableau = rbind(tableau, c(paste(zones[1:N,Z],sep="", collapse=""), # Repete le nom de zone pour toutes les lignes de profil
										"",                                  # Profil
										"",                                  # Effectif du profil
										sum(apply(as.matrix(resTmp[,c(zones[1:N,Z])]), 1, function(x) sum(as.numeric(x)))==N),        # Effectif total de la zone
										N))                                  # Nbre de listes = type de zone
					}
				}else{                                                                  # Si la zone est vide
					if(ud)
					{
						tableau = rbind(tableau, cbind(paste(zones[1:N,Z], sep="", collapse=","),# Nom de zone
										"",                                  # Profil
										"",                                  # Effectif du profil
										0,                                   # Effectif total de la zone
										N))                                  # Nbre de listes = type de zone
					}else{
						tableau = rbind(tableau, c(paste(zones[1:N,Z],sep="", collapse=","),# Nom de zone
										"",                                  # Profil
										"",                                  # Effectif du profil
										0,                                   # Effectif total de la zone
										N))                                  # Nbre de listes = type de zone
					}
				}
			}
		}
		
		# Ajoute une colonne code de groupe en concatenant les lettres des listes
		resTmpN = resTmp[,1:(grep("total", colnames(resTmp), ignore.case=TRUE)-1)]      # extrait la matrice binaire sans la colonne total
		resTmpN = cbind(resTmpN, resTmp[, grep("total", colnames(resTmp), ignore.case=TRUE)], apply(resTmpN, 1, function(x) paste(toupper(letters[1:length(x)][x==1]), sep="", collapse="")))  # Collapse les lettres correspondant aux 1 et ajoute a resTmp
		colnames(resTmpN)[1:grep("total", colnames(resTmp), ignore.case=TRUE)] = nmstmp
		colnames(resTmpN)[ncol(resTmpN)] = "GroupName"
		resTmpNp = cbind(rownames(resTmpN), resTmpN)
		colnames(resTmpNp)[1] = "Name"    
		if(ud)
		{
			resTmpNp = cbind(resTmpN, resTmp[,(grep("total", colnames(resTmp), ignore.case=TRUE)+1):ncol(resTmp)])     # Ajoute des ratios
			colnames(resTmpNp)[(grep("GroupName", colnames(resTmpNp))+1):ncol(resTmpNp)] = paste("Ratio_", colnames(resTmpNp)[2:(grep("total", colnames(resTmpNp), ignore.case=TRUE)-1)], sep="")
			resTmpNp[is.na(resTmpNp)]=""
		}            
		write.table(resTmpNp, file=paste(path, "/BarVenn.txt", sep=""), sep="\t", row.names = FALSE, quote=FALSE)
		
		FactCorr = 400/max(as.numeric(tableau[,"tot"]))                             # Facteur de correction pour un max = 500
		
		for(S in 1:max(as.numeric(tableau[,"size"])))
		{
			tabTmp = tableau[tableau[,"size"]==S, ]                                  # Extraction des datas des zones du meme type = impliquant S listes
			if(is.null(nrow(tabTmp)))  tabTmp = t(as.matrix(tabTmp))                 # Correction du bug de dimension si il y a 1 seul profile
			nn=1                                                                    # Initialise le compteur de chiffre
			while((max(as.numeric(tabTmp[,"tot"]))/(10**nn))>1){nn = nn + 1}        # Estime la taille en chiffres du total
			Csize = 30*max(max(nchar(tabTmp[,"zone"])), 2*nn)                        # Ajuste la largeur des barres en fonction du nbre de caracteres
			if(Csize<400){Csize=400}                                                  #       avec min = 400
			sizeVU = round(max(as.numeric(tabTmp[,"tot"]))*FactCorr)+60*length(noms) # Hauteur du graph <0 avec min = 1000
			if(sizeVU<1000){sizeVU=1000}
			
			sizeVD = 0
			if(sum(tabTmp[,"profile"]!="")) sizeVD = max(table(tabTmp[tabTmp[,"profile"]!="","zone"]))*50  # Hauteur du graph <0 avec min = 400
			if(sizeVD<500){sizeVD=500}
			if(!ud){sizeVD=50}
			sizeH = length(table(tabTmp[,"zone"]))*Csize                             # Largeur du graph
			if(sizeH<1200){sizeH=1200}                                            #       avec min = 1200
			decal=0                                                                  # Initialisation de la position des barres
			
			png(filename = paste(path, "/BarVenn_", S, ".png", sep=""), width=30, height=20, units='cm', pointsize=15, bg="white", res=300)
			plot(c(0,sizeH), c(-sizeVD,sizeVU), type = "n", xlab="", ylab="", axes=FALSE, asp = 1)
			
			for(G in names(table(tabTmp[,"zone"])))                                  # Parcours zone par zone
			{
				tots = max(as.numeric(tabTmp[tabTmp[,"zone"]==G,"tot"]))              # Effectif de la zone
				FCtots = tots*FactCorr                                                # Hauteur de la barre = maximum de la zone * FactCorr
				polygon(c(decal,decal,(decal+Csize), (decal+Csize)),c(0,FCtots, FCtots,0),col=240, density=10)
				segments(x0=decal, y0=0, x1=decal, y1=FCtots)                         # Position de la barre= decal
				segments(x0=decal, y0=FCtots, x1=(decal+Csize), y1=FCtots)            # Largeur de la barre = Csize
				segments(x0=(decal+Csize), y0=FCtots, x1=(decal+Csize), y1=0)
				
				text(x=(decal+Csize/2), y=(FCtots+50), labels=paste(G, sep=""), cex=1)    # Nom de la zone 20 au dessus de la barre
				text(x=(decal+Csize/2), y=(FCtots+50), labels=paste(tots, sep=""), cex=1, pos=1, offset=0.8) # Effectif tot 20 sous le haut de la barre
				
				if(ud)
				{
					profilTmp = tabTmp[(tabTmp[,"zone"]==G),]                           # Extraction des datas de la zone courante = G
					decaly=-20                                                          # Initialisation de la position des UD -50 pour eviter chevauchement avec nom G
					if(is.null(nrow(profilTmp)))  profilTmp = t(as.matrix(profilTmp))   # Correction du bug de dimension si il y a 1 seul profile
					for(P in 1:nrow(profilTmp))
					{                                                                   # Parcours des profils UD
						text(x=(decal+Csize/2), y=decaly, labels=paste(profilTmp[P,"profile"], profilTmp[P,"N"], sep=" "), cex=0.7, pos=1, offset=P)
						#decaly = decaly - 40                                              # Decalage vertical pour separer les profils
					}
				}                                                                     # Decalage Horizontal pour separer les barres
				decal = decal+Csize
			}
			
			decal = decal+(Csize/2)                                                   # Decalage Horizontal pour separer les zones de meme type S
			segments(x0=0, y0=-60, x1=(decal-Csize/2), y1=-60)                        # Barre horizontale ~0
			
			text(x=sizeH/2, y=sizeVU, labels=paste("Zones of ", S, " lists (", sum(as.numeric(tabTmp[,"tot"])!=0), " / ", length(names(table(tabTmp[,"zone"]))), ")", sep=""), cex=1.2)   # Titre
			
			for(L in 1:length(noms))  # Affichage des correspondances des noms de lites
			{
				text(x=sizeH/2, y=sizeVU, labels=paste(noms[L], " - ", nmstmp[L], sep=""), cex=0.9, pos=1, offset=L*0.9)
			}
			dev.off()
		}
	}
	
	cercle<-function(x, y, rayon, int=NA, out=NA, lty, lwd)
	{
		xylim <- par("usr")
		plotdim <- par("pin")
		ymult <- (xylim[4] - xylim[3])/(xylim[2] - xylim[1]) * plotdim[1]/plotdim[2]
		angle.inc <- 2 * pi/2000
		angles <- seq(0, 2 * pi - angle.inc, by = angle.inc)
		
		for (I in 1:length(x))
		{
			dens = NULL
			if(is.na(int[I])) dens=0
			xv <- cos(angles) * rayon[I] + x[I]
			yv <- sin(angles) * rayon[I] * ymult + y[I]
			polygon(xv, yv, border = out[I], col=int[I], lty, lwd, density=dens)
		}
	}
	
	cmbn<-function(x, n)                                                          # Fonction combinaison de n dans x
	{
		if(n<=length(x))
		{
			zones = expand.grid(rep(list(x), n))                                      # Matrice des arrangements
			zones = zones[, seq(n, 1, by=-1)]                                         # Inverse l'ordre des colonnes pour obtenir l'ordre logique
			zones = cbind(zones, apply(zones, 1, function(x) paste(x[order(x)], sep="", collapse="")))
			colnames(zones)[ncol(zones)]="nom"
			t = table(zones[,ncol(zones)])                                            # Nbre d'occurence de chaque combinaison
			t = names(t)[t==max(t)]                                                   # Selectionne les combinaisons OK qui ont un score max
			zones = as.matrix(zones[!duplicated(zones[, ncol(zones)]), 1:(ncol(zones)-1)])
			return(zones)
		}else{return(NULL)}
	}
	
	tolog2<-function(dataset)
	{
		data_log2 = cbind(as.matrix(apply(dataset, 2, function(x) apply(as.matrix(x), 2, function(y) log2(y)))))
		rownames(data_log2) = rownames(dataset)
		return(data_log2)
	}
	
	overlapp<-function(res, path, f)
	{
		write("Computing overlaps ", file="")
		flush.console()
		
		overlapp_table = matrix(1, ncol=(ncol(res)-1), nrow=(ncol(res)-1))
		rownames(overlapp_table) = colnames(res)[1:(ncol(res)-1)]
		colnames(overlapp_table) = colnames(res)[1:(ncol(res)-1)]
		comps = cmbn(x=colnames(overlapp_table), n=2)
		overlapp_table_n = overlapp_table
		
		if((ncol(res)-1)>2)
		{
			for(K in 1:nrow(comps))
			{
				tmp = res[,c(comps[K,])] #extrait les deux colonnes en question
				tmp = cbind(tmp, (tmp[,1] + tmp[,2]))
				comm = nrow(tmp[tmp[,3]==2,])
				if(!is.null(comm))
				{
					overlapp_table_n[comps[K,1],comps[K,2]] = comm
					overlapp_table_n[comps[K,2],comps[K,1]] = comm
					overlapp_table[comps[K,1],comps[K,2]] = comm / sum(tmp[,1])
					overlapp_table[comps[K,2],comps[K,1]] = comm / sum(tmp[,2])
				}else{
					overlapp_table_n[comps[K,1],comps[K,2]] = 0
					overlapp_table_n[comps[K,2],comps[K,1]] = 0
					overlapp_table[comps[K,1],comps[K,2]] = 0
					overlapp_table[comps[K,2],comps[K,1]] = 0
				}
				write(paste(K, " / ", ncol(comps), sep=""), file="")
				flush.console()
			}
			
			png(filename = paste(path, "/HeatOverlaps.png", sep=""), width=1200, height=1000, units = "px", pointsize = 10, bg = "white")
			heatmap(overlapp_table, margins = c(50,50), cellnote=overlapp_table, notecol="black")
			dev.off()
			
			
		}else{
			comm = nrow(res[res[,3]==2,])
			overlapp_table[comps[1],comps[2]] = comm / sum(res[,1])
			overlapp_table[comps[2],comps[1]] = comm / sum(res[,2])
		}
		overlapp_tablep=cbind(rownames(overlapp_table), overlapp_table)
		write.table(overlapp_tablep, row.names = TRUE, file = paste(path, "/overlaps_table.txt", sep=""), sep="\t", quote=FALSE)
		overlapp_table_np=cbind(rownames(overlapp_table_n), overlapp_table_n)
		write.table(overlapp_table_np, row.names = TRUE, file = paste(path, "/overlaps_table_n.txt", sep=""), sep="\t", quote=FALSE)
		
		# filtrage
		if(f!=0)
		{
			overlapp_f = rep(0, ncol(overlapp_table))
			names(overlapp_f) = rownames(overlapp_table)
			for(L in 2:ncol(overlapp_table))
			{
				overlapp_f[names(overlapp_table[1:(L-1),L])[overlapp_table[1:(L-1),L]>=f]] = 1
			}
			overlapp_table_f = overlapp_table[overlapp_f==0,overlapp_f==0]
			overlapp_tablef = cbind(rownames(overlapp_table), overlapp_table)
			write.table(overlapp_table, row.names = FALSE, file = paste(path, "/overlaps_table_f(", f, ").txt", sep=""), sep="\t", quote=FALSE)
		}
	}
	
	compte<-function(x) #compte les types de profils up/down
	{
		t = x
		t[x<1] = "D"
		t[x>=1] = "U"
		#corrige bug si 1 seul res
		if(!(class(t)=="character"))
		{
			if(ncol(t)==2)  profils = apply(t, 1, function(x) paste(x[1], x[2], sep=""))             #matrice des profils
			if(ncol(t)==3)  profils = apply(t, 1, function(x) paste(x[1], x[2], x[3], sep=""))       #matrice des profils
			if(ncol(t)==4)  profils = apply(t, 1, function(x) paste(x[1], x[2], x[3], x[4], sep="")) #matrice des profils
		}
		if((class(t)=="character"))
		{
			if(length(t)==2)  profils = paste(t[1], t[2], sep="")                     #matrice des profils
			if(length(t)==3)  profils = paste(t[1], t[2], t[3], sep="")               #matrice des profils
			if(length(t)==4)  profils = paste(t[1], t[2], t[3], t[4], sep="")         #matrice des profils
		}
		return(table(profils))
	}
	
	format_label<-function(n, m, nom, x, y, type=0, dtitre=1.6, dprofils, noms, couleurs, dh, dv)
	{
		m = m[order(seq(length(m), 1, by=-1))]
    # Compense l'usage d'offset qui ne centre pas la chaine de caratcteres
		compensOffset = max(nchar(paste(m, sep="", collapse=NULL)))*0.8 + max(nchar(paste(names(m), sep="", collapse=NULL)))
		text(x=x, y=y, labels=n, cex=dtitre, col="black") # Effectif tot
		if(n!=0)
		{
			y=y-dv*0.5
			if(length(m)!=0)
			{
				nl = length(strsplit(nom, ",")[[1]])
				for(I in 1:length(m))
				{
					for(J in 1:nl)
					{
						couleur = couleurs[grep(strsplit(nom, ",")[[1]][J], noms)]            
						text(x=x, y=(y-I*dv), labels=substr(rownames(m)[I],J,J), cex=dprofils, col=couleur, pos=4, offset=(J-1-(compensOffset/2))*dh) # Profil en couleurs
					}
					text(x=x, y=(y-I*dv), labels=m[I], cex=dprofils, col="black", pos=4, offset=(J-(compensOffset/2))) # Effectif du profil     
				}
			}else{
				text(x=x, y=y, labels=paste(0, sep=""), cex=dprofils, col="black")
			}
		}
	}
	
	#test des formats des listes
	# liste=listes[1]; type="Res"
	test_list<-function(liste, type, matLists, noms, path, affich=TRUE)
	{
		if(!is.list(matLists))
		{
			ext = substr(basename(liste), (nchar(basename(liste))-2), nchar(basename(liste)))
			nomListe = substr(basename(liste), 0, (nchar(basename(liste))-4))
			data_t=""
			if(exists("data_t"))  rm("data_t")
			if(ext == "txt") data_t = read.table(file=liste, header=TRUE, sep="\t")
			if(ext == "tsv") data_t = read.table(file=liste, header=TRUE, sep="\t")
			if(ext == "csv") data_t = try(read.table(file=liste, header=TRUE, sep=","), silent=TRUE)
			if((ext == "csv")&(class(data_t)=="try-error")) data_t = try(read.table(file=liste, header=TRUE, sep=";"), silent=TRUE)
			if((ext!="csv")&(ext!="txt"))
			{
				if(display) write("The file format is not supported (must be txt/tab or csv/,;)", file="")
				flush.console()
				break;
			}
		}else{
			data_t = matLists[[seq(1, length(noms), by=1)[noms==liste]]]
			colnames(data_t) = names(matLists[[seq(1, length(noms), by=1)[noms==liste]]])
			nomListe = liste
		}
		
		data_t=DuplicateTest(filename=liste, fich=data_t, resPath=path, nomlignes=FALSE, affich=affich)
		
		if(ncol(data_t)<3)                                                          #juste les ID
		{
			rownames(data_t) = data_t[,1]                                           #ID = 1ere colonne
		}
		if(ncol(data_t)>=3)                                                         #ID et une seule colonne
		{
			rownames(data_t) = data_t[,1]                                           #ID = 1ere colonne
			data_t = data_t[,2:ncol(data_t)]
		}
		data_t = as.matrix(data_t)
		return(data_t)
	}
	
	# filename=liste; fich=""; resPath=path; nomlignes=FALSE
	DuplicateTest<-function(filename="", fich="", resPath, nomlignes=TRUE, affich=TRUE)
	{
		if(fich=="")  # Adresse en variable
		{
			TempListe = read.table(file=filename, header=TRUE, sep="\t")
		}else{  # fichier deja lu en variable
			TempListe = fich
		}
		if(sum(duplicated(TempListe[,1]))==0) # Pas de duplicats de rownames
		{
			if(nomlignes)
			{
				rownames(TempListe) = TempListe[,1]
				TempListe = TempListe[,2:ncol(TempListe)]
			}
		}else{  # Duplicats => gestion et affichage
			if(affich)  write(paste(sum(duplicated(TempListe[,1])), " duplicated row names in ", basename(filename), sep=""), file="")
			TempDuplicates = TempListe[duplicated(TempListe[,1]),]  # Recupere les diplicats
			write.table(TempDuplicates, file=paste(resPath, "/FDup_", basename(filename), sep=""), sep="\t")  # Filtered Duplicated rown names
			TempListe = TempListe[!duplicated(TempListe[,1]),] # Filtrage des duplicats
			
			if(nomlignes)
			{
				rownames(TempListe) = TempListe[,1]
				TempListe = TempListe[,2:ncol(TempListe)]
			}
		}
		return(TempListe)
	}
	
	graph_2<-function(path, listeA, listeB, nA, nB, nAB, tot_ugenes, noms, ud, nAu="", nAd="", nBu="", nBd="", nABud="", couleurs, couleursIn, Solid)
	{
		png(filename=paste(path, "/venn_diagram.png", sep=""), width=30, height=15, bg="white", units='cm', res=300)
		plot.new()
		par(mar=c(0,0,0,0))
		sizeGx=100
		sizeGy=sizeGx/2
		plot.window(xlim=c(0, sizeGx), ylim=c(0, sizeGy), asp=1)
		
		xcercles = c((12*sizeGx)/30, (18*sizeGx)/30)
		ycercles = c(sizeGy/2, sizeGy/2)
		rcercles = c(sizeGy/3, sizeGy/3)
		if(Solid)
		{
			cercle(xcercles, ycercles, rcercles, out=NA, int=couleursIn, lty=1, lwd=1)
			colTxt = "white"
		}
		if(!Solid)
		{
			cercle(xcercles, ycercles, rcercles, out=couleurs, lty=1, lwd=1)
			colTxt = "black"
		}
		tUD=1.5
		tnoUD=2.2
		if(ud)
		{
			text(x=(10*sizeGx)/30, y=(17*sizeGy)/30, labels=nA,                       cex=tUD, col=couleurs[1])
			text(x=(10*sizeGx)/30, y=(17*sizeGy)/30, labels=paste("U ", nAu, sep=""), cex=tUD, col=couleurs[1], pos=1, offset=1)
			text(x=(10*sizeGx)/30, y=(17*sizeGy)/30, labels=paste("D ", nAd, sep=""), cex=tUD, col=couleurs[1], pos=1, offset=2)
			text(x=(20*sizeGx)/30, y=(17*sizeGy)/30, labels=nB,                       cex=tUD, col=couleurs[2])
			text(x=(20*sizeGx)/30, y=(17*sizeGy)/30, labels=paste("U ", nBu, sep=""), cex=tUD, col=couleurs[2], pos=1, offset=1)
			text(x=(20*sizeGx)/30, y=(17*sizeGy)/30, labels=paste("D ", nBd, sep=""), cex=tUD, col=couleurs[2], pos=1, offset=2)
			format_label(n=nAB, m=nABud, nom=paste(noms[1], noms[2], sep=","), x=sizeGx/2, y=(17*sizeGy)/30, dtitre=tUD, dprofil=tUD, noms=noms, couleurs=couleurs, dh=0.9, dv=2)
		}else{
			text(x=(11*sizeGx)/30, y=ycercles[1], labels=nA,  cex=tnoUD, col=colTxt)
			text(x=(19*sizeGx)/30, y=ycercles[2], labels=nB,  cex=tnoUD, col=colTxt)
			text(x=sizeGx/2,    y=sizeGy/2,    labels=nAB, cex=tnoUD, col=colTxt)
		}
		#titres
		tTitre = 2 
		text(x=sizeGx/6, y=(7*sizeGy)/10, labels=paste("Total: ", tot_ugenes, sep=""), cex=tTitre, col="black")
		text(x=sizeGx/2, y=(9*sizeGy)/10, labels=listeA,                               cex=tTitre, col=couleurs[1])
		text(x=sizeGx/2, y=sizeGy/10,     labels=listeB,                               cex=tTitre, col=couleurs[2])
		dev.off()
	}
	
	graph_3<-function(path, listeA, listeB, listeC, nA, nB, nC, nAB, nAC, nBC, nABC, tot_ugenes, noms, ud, nAu="", nAd="", nBu="", nBd="", nCu="", nCd="", nABud="", nACud="", nBCud="", nABCud="", couleurs, couleursIn, Solid)
	{
		png(filename=paste(path, "/venn_diagram.png", sep=""), width=25, height=25, bg="white", units='cm', res=300)
		plot.new()
		par(mar=c(0,0,0,0))
		sizeG=100
		plot.window(xlim=c(0, sizeG), ylim=c(0, sizeG), asp=1)
		
		xcercles = c((15*sizeG)/30, (11*sizeG)/30, (19*sizeG)/30)  # A, B, C
		ycercles = c(((((7.5*sizeG)/30)*sqrt(3))/2)+(10*sizeG/30), (10*sizeG)/30, (10*sizeG)/30)
		rcercles = rep((7.5*sizeG)/30, 3)
		if(Solid)
		{
			cercle(xcercles, ycercles, rcercles, out=NA, int=couleursIn, lty=1, lwd=1)
			colTxt = "white"
			colGreen = "darkgreen"
		}
		if(!Solid)
		{
			cercle(xcercles, ycercles, rcercles, out=couleurs, lty=1, lwd=1)
			colTxt = "black"
			colGreen = "green"
		}
		
		if(ud)
		{
			tUD=1.5
			tUDp = 1.3
			text(x=xcercles[1],                 y=(ycercles[1]+rcercles[1]/2), labels=nA,               cex=tUD, col="black")
			text(x=xcercles[1],                 y=(ycercles[1]+rcercles[1]/2), labels=paste("U ", nAu), cex=tUD, col=couleurs[1], pos=1, offset=1)
			text(x=xcercles[1],                 y=(ycercles[1]+rcercles[1]/2), labels=paste("D ", nAd), cex=tUD, col=couleurs[1], pos=1, offset=2)
			text(x=(xcercles[2]-rcercles[2]/2), y=ycercles[2],              labels=nB,               cex=tUD, col="black")
			text(x=(xcercles[2]-rcercles[2]/2), y=ycercles[2],              labels=paste("U ", nBu), cex=tUD, col=couleurs[2], pos=1, offset=1)
			text(x=(xcercles[2]-rcercles[2]/2), y=ycercles[2],              labels=paste("D ", nBd), cex=tUD, col=couleurs[2], pos=1, offset=2)
			text(x=(xcercles[3]+rcercles[3]/2), y=ycercles[3],              labels=nC,               cex=tUD, col="black")
			text(x=(xcercles[3]+rcercles[3]/2), y=ycercles[3],              labels=paste("U ", nCu), cex=tUD, col=colGreen, pos=1, offset=1)
			text(x=(xcercles[3]+rcercles[3]/2), y=ycercles[3],              labels=paste("D ", nCd), cex=tUD, col=colGreen, pos=1, offset=2)
			
			y = (xcercles[1]+(xcercles[3]+rcercles[3]/2))/2
			
			format_label(n=nAB,  m=nABud,  nom=paste(noms[1], noms[2], sep=","),          x=(10.5*sizeG)/30, y=(16*sizeG)/30, dtitre=tUD, dprofil=tUDp, noms=noms, couleurs=couleurs, dh=0.9, dv=2.3)
			format_label(n=nAC,  m=nACud,  nom=paste(noms[1], noms[3], sep=","),          x=(19.5*sizeG)/30, y=(16*sizeG)/30, dtitre=tUD, dprofil=tUDp, noms=noms, couleurs=couleurs, dh=0.9, dv=2.3)
			format_label(n=nBC,  m=nBCud,  nom=paste(noms[2], noms[3], sep=","),          x=(15*sizeG)/30, y=(8.5*sizeG)/30, dtitre=tUD, dprofil=tUDp, noms=noms, couleurs=couleurs, dh=0.9, dv=2.3)
			format_label(n=nABC, m=nABCud, nom=paste(noms[1], noms[2], noms[3], sep=","), x=(15*sizeG)/30,   y=(15*sizeG)/30, dtitre=tUD, dprofil=tUDp, noms=noms, couleurs=couleurs, dh=0.9, dv=2)
		}else{
			tnoUD=2.2
			text(x=xcercles[1],                                y=(ycercles[1]+rcercles[1]/2),               labels=nA, cex=tnoUD, col=colTxt)
			text(x=(xcercles[2]-rcercles[2]/2),                y=ycercles[2],                               labels=nB, cex=tnoUD, col=colTxt)
			text(x=(xcercles[3]+rcercles[3]/2),                y=ycercles[3],                               labels=nC, cex=tnoUD, col=colTxt)
			text(x=(xcercles[1]-(2*rcercles/3)),               y=(ycercles[2]+(2*rcercles[1]/3)),           labels=nAB, cex=tnoUD, col=colTxt)
			text(x=(xcercles[1]+(2*rcercles/3)),               y=(ycercles[2]+(2*rcercles[1]/3)),           labels=nAC, cex=tnoUD, col=colTxt)
			text(x=(xcercles[2]+(xcercles[3]-xcercles[2])/2),  y=(ycercles[2]-rcercles[1]/3),               labels=nBC, cex=tnoUD, col=colTxt)
			text(x=xcercles[1],                                y=mean(ycercles),                            labels=nABC, cex=tnoUD, col=colTxt)
		}
		
		#titres
		tTitre=1.7
		text(x=(sizeG/2), y=sizeG, labels=listeA, cex=tTitre, col=couleurs[1], pos=1, offset=1.7)
		text(x=(sizeG/2), y=sizeG, labels=listeB, cex=tTitre, col=couleurs[2], pos=1, offset=3.4)
		text(x=(sizeG/2), y=sizeG, labels=listeC, cex=tTitre, col=couleurs[3], pos=1, offset=5.2)
		
		text(x=(xcercles[3]+rcercles[3]), y=ycercles[1], labels=paste("Total: ", tot_ugenes, collapse="\n"), cex=tTitre, col="black")
		dev.off()
	}
	
	graph_4<-function(path, listeA, listeB, listeC, listeD, nA, nB, nC, nD, nAB, nAC, nBD, nCD, nAD, nBC, nABC, nBCD, nACD, nABD, nABCD, tot_ugenes, noms, ud, nAu="", nAd="", nBu="", nBd="", nCu="", nCd="", nDu="", nDd="", nABud="", nACud="", nBCud="", nBDud="", nCDud="", nADud="", nABCud="", nBCDud="", nABDud="", nACDud="", nABCDud="", couleurs, couleursIn, Solid)
	{
		tnoUD=2.1
		png(filename=paste(path, "/venn_diagram.png", sep=""), width=30, height=30, bg="white", units='cm', res=300)
		plot.new()
		par(mar=c(0,0,0,0))
		sizeG=100
		plot.window(xlim=c(0, sizeG), ylim=c(0, sizeG), asp=1)
		
		xcercles = c((11*sizeG)/30, (7*sizeG)/30, (15*sizeG)/30, (11*sizeG)/30, (24*sizeG)/30, (26*sizeG)/30, (26*sizeG)/30, (26*sizeG)/30)  # A, B, C, D,    A, B   C, D
		ycercles = c((15*sizeG)/30, (11*sizeG)/30, (11*sizeG)/30, (7*sizeG)/30, (19*sizeG)/30, (19*sizeG)/30, (10*sizeG)/30, (8*sizeG)/30)
		rcercles = c((7*sizeG)/30, (7*sizeG)/30, (7*sizeG)/30, (7*sizeG)/30, sizeG/8, sizeG/8, sizeG/8, sizeG/8)
		
		xcercles[c(1, 2, 3, 4)] = xcercles[c(1, 2, 3, 4)]
		xcercles[c(5, 6)] = xcercles[c(5, 6)]
		xcercles[c(7, 8)] = xcercles[c(7, 8)]
		
		ycercles[c(1, 2, 3, 4)] = ycercles[c(1, 2, 3, 4)]
		ycercles[c(5, 6)] = ycercles[c(5, 6)]
		ycercles[c(7, 8)] = ycercles[c(7, 8)]
		
		if(Solid)
		{
			cercle(xcercles, ycercles, rcercles, out=NA, int=c(couleursIn, couleursIn[c(1,4,2,3)]), lty=1, lwd=1)
			colTxt = "white"
			coulGroups = c("darkblue", "darkred", "darkgreen", "brown")
			couleursTxt = c(couleurs[1:3], "yellow")
		}
		if(!Solid)
		{
			cercle(xcercles, ycercles, rcercles, out=c(couleurs[1], couleurs[2], couleurs[3], couleurs[4], couleurs[1], couleurs[4], couleurs[2], couleurs[3]), lty=1, lwd=1)
			colTxt = "black"
			couleursTxt=couleurs
			coulGroups=couleurs
		}
		if(ud)
		{
			tUD=1.4
			tUDp=0.8
			text(x=(11*sizeG)/30, y=(19*sizeG)/30, labels=paste(nA, "\n", "U ", nAu, "\nD ", nAd, sep=""), cex=tUD, col=coulGroups[1])
			text(x=(3*sizeG)/30,  y=(11*sizeG)/30, labels=paste(nB, "\nU ", nBu, "\nD ", nBd, sep=""), cex=tUD, col=coulGroups[2])
			text(x=(19*sizeG)/30, y=(11*sizeG)/30, labels=paste(nC, "\nU ", nCu, "\nD ", nCd, sep=""), cex=tUD, col=coulGroups[3])
			text(x=(11*sizeG)/30, y=(3*sizeG)/30,  labels=paste(nD, "\nU ", nDu, "\nD ", nDd, sep=""), cex=tUD, col=coulGroups[4])
			
			format_label(n=nAB,   m=nABud,   nom=paste(noms[1], ",", noms[2], sep=""),                             x=(7*sizeG)/30,  y=(17*sizeG)/30, dtitre=tUD, dprofil=tUDp, noms=noms, couleurs=couleursTxt, dh=0.7, dv=2)
			format_label(n=nAC,   m=nACud,   nom=paste(noms[1], ",", noms[3], sep=""),                             x=(15*sizeG)/30, y=(17*sizeG)/30, dtitre=tUD, dprofil=tUDp, noms=noms, couleurs=couleursTxt, dh=0.7, dv=2)
			format_label(n=nBD,   m=nBDud,   nom=paste(noms[2], ",", noms[4], sep=""),                             x=(7*sizeG)/30,  y=(8*sizeG)/30, dtitre=tUD, dprofil=tUDp, noms=noms, couleurs=couleursTxt, dh=0.7, dv=2)
			format_label(n=nCD,   m=nCDud,   nom=paste(noms[3], ",", noms[4], sep=""),                             x=(15*sizeG)/30, y=(8*sizeG)/30, dtitre=tUD, dprofil=tUDp, noms=noms, couleurs=couleursTxt, dh=0.7, dv=2)
			
			format_label(n=nAD,   m=nADud,   nom=paste(noms[1], ",", noms[4], sep=""),                             x=(25*sizeG)/30, y=(20*sizeG)/30, dtitre=tUD, dprofil=tUDp, noms=noms, couleurs=couleursTxt, dh=0.7, dv=2)
			format_label(n=nBC,   m=nBCud,   nom=paste(noms[2], ",", noms[3], sep=""),                             x=(26*sizeG)/30, y=(10*sizeG)/30, dtitre=tUD, dprofil=tUDp, noms=noms, couleurs=couleursTxt, dh=0.7, dv=2)
			
			format_label(n=nABC,  m=nABCud,  nom=paste(noms[1], ",", noms[2], ",", noms[3], sep=""),               x=(11*sizeG)/30, y=(16*sizeG)/30, dtitre=tUD, dprofil=tUDp, noms=noms, couleurs=couleursTxt, dh=0.7, dv=1)
			format_label(n=nABD,  m=nABDud,  nom=paste(noms[1], ",", noms[2], ",", noms[4], sep=""),               x=(15*sizeG)/30, y=(12*sizeG)/30, dtitre=tUD, dprofil=tUDp, noms=noms, couleurs=couleursTxt, dh=0.7, dv=1)
			format_label(n=nACD,  m=nACDud,  nom=paste(noms[1], ",", noms[3], ",", noms[4], sep=""),               x=(7*sizeG)/30,  y=(12*sizeG)/30, dtitre=tUD, dprofil=tUDp, noms=noms, couleurs=couleursTxt, dh=0.7, dv=1)
			format_label(n=nBCD,  m=nBCDud,  nom=paste(noms[2], ",", noms[3], ",", noms[4], sep=""),               x=(11*sizeG)/30, y=(8*sizeG)/30, dtitre=tUD, dprofil=tUDp, noms=noms, couleurs=couleursTxt, dh=0.7, dv=1)
			
			format_label(n=nABCD, m=nABCDud, nom=paste(noms[1], ",", noms[2], ",", noms[3], ",", noms[4], sep=""), x=(11*sizeG)/30, y=(13*sizeG)/30, dtitre=tUD, dprofil=tUDp, noms=noms, couleurs=couleursTxt, dh=0.7, dv=1)
		}else{
			text(x=(11*sizeG)/30, y=(19*sizeG)/30, labels=nA, cex=tnoUD, col=colTxt)
			text(x=(3*sizeG)/30,  y=(11*sizeG)/30, labels=nB, cex=tnoUD, col=colTxt)
			text(x=(19*sizeG)/30, y=(11*sizeG)/30, labels=nC, cex=tnoUD, col=colTxt)
			text(x=(11*sizeG)/30, y=(3*sizeG)/30,  labels=nD, cex=tnoUD, col=colTxt)
			
			text(x=(7*sizeG)/30,  y=(15*sizeG)/30, labels=nAB, cex=tnoUD, col=colTxt)
			text(x=(15*sizeG)/30, y=(15*sizeG)/30, labels=nAC, cex=tnoUD, col=colTxt)
			text(x=(7*sizeG)/30,  y=(7*sizeG)/30,  labels=nBD, cex=tnoUD, col=colTxt)
			text(x=(15*sizeG)/30, y=(7*sizeG)/30,  labels=nCD, cex=tnoUD, col=colTxt)
			
			text(x=(25*sizeG)/30, y=(19*sizeG)/30, labels=nAD, cex=tnoUD, col=colTxt)      
			text(x=(26*sizeG)/30, y=(9*sizeG)/30,  labels=nBC, cex=tnoUD, col=colTxt)
			
			text(x=(11*sizeG)/30, y=(15*sizeG)/30, labels=nABC, cex=tnoUD, col=colTxt)
			text(x=(15*sizeG)/30, y=(11*sizeG)/30, labels=nABD, cex=tnoUD, col=colTxt)
			text(x=(7*sizeG)/30,  y=(11*sizeG)/30, labels=nACD, cex=tnoUD, col=colTxt)      
			text(x=(11*sizeG)/30, y=(7*sizeG)/30,  labels=nBCD, cex=tnoUD, col=colTxt)
			
			text(x=(11*sizeG)/30, y=(11*sizeG)/30, labels=nABCD, cex=tnoUD, col=colTxt)
		}
		#titres
		tTitres=1.7
		text(x=sizeG/2, y=(29*sizeG)/30, labels=listeA, cex=tTitres, col=couleurs[1], pos=1, offset=1.5)
		text(x=sizeG/2, y=(29*sizeG)/30, labels=listeB, cex=tTitres, col=couleurs[2], pos=1, offset=3)
		text(x=sizeG/2, y=(29*sizeG)/30, labels=listeC, cex=tTitres, col=couleurs[3], pos=1, offset=4.5)
		text(x=sizeG/2, y=(29*sizeG)/30, labels=listeD, cex=tTitres, col=couleurs[4], pos=1, offset=6)
		
		text(x=(25*sizeG)/30, y=(3*sizeG)/30,  labels=paste("Total: ", tot_ugenes, collapse="\n"), cex=tTitres, col="black")
		dev.off()
	}
	
	graph_prop_2<-function(path, res, nA, nB, nAB, tot_ugenes, noms, couleurs, tlog, colBlack)
	{
		if(!colBlack)  
		{
			colWhite="white"
			coultxt=couleurs
		}else{
			colWhite="black"
			coultxt = rep("black", length(couleurs))
		}
		
		n = apply(res[,1:(ncol(res)-1)], 2, function(x) sum(as.numeric(x)))
		
		# Normalize a 50 le max
		if(tlog)
		{
			nr = round(log2(n))
			nrA = round(log2(nA))
			nrB = round(log2(nB))
			nrAB = round(log2(nAB))
		}else{    
			nr = round(n/max(n, nA, nB)*50)
			nrA = round(nA/max(n, nA, nB)*50)
			nrB = round(nB/max(n, nA, nB)*50)
			nrAB = round(nAB/max(n, nA, nB)*50)
		}
		#write(paste("n:",n, ", nA:", nA, ", nB:", nB, ", nAB:", nAB, sep=""), file="")
		#write(paste("nr:",nr, ", nrA:", nrA, ", nrB:", nrB, ", nrAB:", nrAB, sep=""), file="")
		
		#calculs des rayons pour que la surface des cercles reflete la taille des listes
		if(is.infinite(nr[1])){rAtot=0}else{rAtot=sqrt(nr[1]/pi)}
		if(is.infinite(nr[2])){rBtot=0}else{rBtot=sqrt(nr[2]/pi)}
		if(is.infinite(nrA)){rA=0}else{rA=sqrt(nrA/pi)}
		if(is.infinite(nrB)){rB=0}else{rB=sqrt(nrB/pi)}
		if(is.infinite(nrAB)){rAB=0}else{rAB=sqrt(nrAB/pi)}
		
		# coordonnees des centres
		xAtot = (rAtot) # 10% de decalage vers la droite pour le premier cercle
		xBtot = (2*rAtot+2*rBtot+2*rAB) # 10% d'expension pour le dernier
		xAB = (xAtot+rAtot) + (xBtot-rBtot-(xAtot+rAtot))/2
		xA = rA
		xB = xBtot + rBtot - rB
		
		yAtot = max(rAtot, rBtot, rAB)*1.1
		yBtot = yAtot
		yAB = yAtot
		yA = yAtot
		yB = yAtot
		
		xCircles = c(xAtot, xBtot, xAB)
		xSpeCircles = c(xA, xB)
		yCircles = c(yAtot, yBtot, yAB)
		ySpeCircles = c(yA, yB)
		rCircles = c(rAtot, rBtot, rAB)
		rSpeCircles = c(rA, rB)
		colorCircles = c("blue", "red", "black")
		colorSpeCircles = c("blue", "red")
		
		xmax = (2*rAtot + 2*rBtot+2*rAB) *1.2  # decal de 10% de rAtot pour avoir le meme decalage qu'a gauche
		xmin = 0
		ymax = max((yAtot+rAtot), (yBtot+rBtot), (yAB+rAB))*1.1
		ymin = 0
		
		if(!tlog)  png(filename = paste(path, "/venn_diagram_prop", if(tlog) paste("_tlog2", sep=""), ".png", sep=""), width=xmax*100, height=ymax*90, pointsize = 6, bg = "white")
		if(tlog)  png(filename = paste(path, "/venn_diagram_prop", if(tlog) paste("_tlog2", sep=""), ".png", sep=""), width=xmax*100, height=ymax*140, pointsize = 5, bg = "white")
		
		plot.new()
		plot.window(c(0, xmax), c(ymin, ymax+abs(ymin)), asp=1)
		segments(x0=c(xAtot, xBtot),
				y0=c(ySpeCircles, ySpeCircles),
				x1=c(xAB, xAB),
				y1=c(yCircles, yCircles),
				col=c("blue", "red"))
		
		symbols(x=xCircles, y=yCircles, circles=rCircles, main = "PropCircles", fg=colorCircles, bg="white", add=TRUE, inches=FALSE)
		symbols(x=xSpeCircles, y=ySpeCircles, circles=rSpeCircles, main = "PropCircles", fg="white", bg=colorSpeCircles, add=TRUE, inches=FALSE)
		
		# N groupes
		taille = 7
		text(x=xA, y=yA, labels=paste(nA), cex=taille, col=colWhite)
		text(x=xB, y=yB, labels=paste(nB), cex=taille, col=colWhite)
		text(x=xAB, y=yAB, labels=paste(nAB), col="black", cex=taille)
		
		#titres
		taille = 6    
		text(x=xAB, y=ymin, labels=paste(colnames(res)[1], " (", n[1], ")", sep=""), cex=taille, col=couleurs[1])
		text(x=xAB, y=ymax, labels=paste(colnames(res)[2], " (", n[2], ")", sep=""), cex=taille, col=couleurs[2])
		
		dev.off()
	}
	
	graph_prop_3<-function(path, res, nA, nB, nC, nAB, nAC, nBC, nABC, tot_ugenes, noms, couleurs, tlog, colBlack)
	{
		if(!colBlack)  
		{
			colWhite="white"
			coultxt=couleurs
		}else{
			colWhite="black"
			coultxt = rep("black", length(couleurs))
		}
		
		n = apply(res[,1:(ncol(res)-1)], 2, function(x) sum(as.numeric(x)))
		
		if(tlog)
		{
			nr = round(log2(n))
			nrA = round(log2(nA))
			nrB = round(log2(nB))
			nrC = round(log2(nC))
			nrAB = round(log2(nAB))
			nrAC = round(log2(nAC))
			nrBC = round(log2(nBC))
			nrABC = round(log2(nABC))
		}else{    
			nr = round(n/max(n, nA, nB, nC)*50)
			nrA = round(nA/max(n, nA, nB, nC)*50)
			nrB = round(nB/max(n, nA, nB, nC)*50)
			nrC = round(nC/max(n, nA, nB, nC)*50)
			nrAB = round(nAB/max(n, nA, nB, nC)*50)
			nrAC = round(nAC/max(n, nA, nB, nC)*50)
			nrBC = round(nBC/max(n, nA, nB, nC)*50)
			nrABC = round(nABC/max(n, nA, nB, nC)*50)
		}
		
		#calculs des rayons pour que la surface des cercles reflete la taille des listes
		if(is.infinite(nr[1])){rAtot=0}else{rAtot=sqrt(nr[1]/pi)}
		if(is.infinite(nr[2])){rBtot=0}else{rBtot=sqrt(nr[2]/pi)}
		if(is.infinite(nr[3])){rCtot=0}else{rCtot=sqrt(nr[3]/pi)}
		if(is.infinite(nrA)){rA=0}else{rA=sqrt(nrA/pi)}
		if(is.infinite(nrB)){rB=0}else{rB=sqrt(nrB/pi)}
		if(is.infinite(nrC)){rC=0}else{rC=sqrt(nrC/pi)}
		if(is.infinite(nrAB)){rAB=0}else{rAB=sqrt(nrAB/pi)}
		if(is.infinite(nrBC)){rBC=0}else{rBC=sqrt(nrBC/pi)}
		if(is.infinite(nrAC)){rAC=0}else{rAC=sqrt(nrAC/pi)}    
		if(is.infinite(nrABC)){rABC=0}else{rABC=sqrt(nrABC/pi)}
		
		# coordonnees des 3 centres
		xBtot = rBtot
		xCtot = 2*max(rBtot, rCtot) + 2*rBC + rCtot
		xAtot = (xBtot+xCtot)/2
		
		yAtot = 2*(max(rBtot, rCtot)+rABC+max(rAC, rAB))+rAtot
		yBtot = max(rBtot, rCtot)
		yCtot =  max(rBtot, rCtot)
		
		#ajustement triangle equilateral
		#AB=AC <?> BC
		AB = sqrt((xAtot-xBtot)^2+(yAtot-yBtot)^2)
		BC = sqrt((xBtot-xCtot)^2+(yBtot-yCtot)^2)
		
		#Calcul de H qui sera fixe pour le calcul de la taille des cotes
		a = acos(BC/(2*AB))
		H = tan(a)*(BC/2)
		#Calcul de la taille des cotes
		cote = (2*H)/sqrt(3)
		
		#Le point B est fixe: xBtot et yBtot sont ok, yA est ok puisque AH est la ref.
		xCtot = xBtot + cote #yBtot est ok
		xAtot = xBtot + cote/2
		
		xA = xAtot
		xB = rBtot - (rBtot - rB)*cos(pi/6)
		xC = xCtot + (rCtot - rC)*cos(pi/6)
		xAB = (xAtot + xBtot)/2
		xAC = (xAtot + xCtot)/2
		xBC = (xBtot + xCtot)/2
		xABC = (xAtot+xBtot+xCtot)/3
		
		yA = yAtot + rAtot - rA
		yB = yBtot - (rBtot - rB)*sin(pi/6)
		yC = yCtot - (rCtot - rC)*sin(pi/6)
		yAB = (yAtot + yBtot)/2
		yAC = (yAtot + yCtot)/2
		yBC = (yBtot + yCtot)/2
		yABC = (yAtot+yBtot+yCtot)/3
		
		xCircles = c(xAtot, xBtot, xCtot, xAB, xAC, xBC, xABC)
		xSpeCircles = c(xA, xB, xC)
		yCircles = c(yAtot, yBtot, yCtot, yAB, yAC, yBC, yABC)
		ySpeCircles = c(yA, yB, yC)
		
		#decal x
		yCircles = 0.8*yCircles
		ySpeCircles = 0.8*ySpeCircles
		yAtot = 0.8*yAtot
		yBtot = 0.8*yBtot
		yCtot = 0.8*yCtot
		yA = 0.8*yA
		yB = 0.8*yB
		yC = 0.8*yC
		yAB = 0.8*yAB
		yAC = 0.8*yAC
		yBC = 0.8*yBC
		yABC = 0.8*yABC
		
		rCircles = c(rAtot, rBtot, rCtot, rAB, rAC, rBC, rABC)
		rSpeCircles = c(rA, rB, rC)
		colorCircles = c(couleurs[1], couleurs[2], couleurs[3], "black", "black", "black", "black")
		colorSpeCircles = c(couleurs[1], couleurs[2], couleurs[3])
		
		png(filename = paste(path, "/venn_diagram_prop", if(tlog) paste("_tlog2", sep=""), ".png", sep=""), width=30, height=30, bg="white", units='cm', res=300)
		plot.new()
		par(mar=c(0,0,0,0))
		xmin = -max(rBtot, rCtot)
		xmax = (xCtot + rCtot)
		ymin = 0
		ymax = (yAtot + rAtot)
		plot.window(c(xmin, xmax), c(ymin, ymax), asp=1)
		segments(x0=c(xAtot, xBtot, xAtot, xCtot, xBtot, xCtot, xAtot, xBtot, xCtot),
				y0=c(yAtot, yBtot, yAtot, yCtot, yBtot, yCtot, yAtot, yBtot, yCtot),
				x1=c(xAB, xAB, xAC, xAC, xBC, xBC, xABC, xABC, xABC),
				y1=c(yAB, yAB, yAC, yAC, yBC, yBC, yABC, yABC, yABC),
				col=c(couleurs[1], couleurs[2], couleurs[1], couleurs[3], couleurs[2], couleurs[3], couleurs[1], couleurs[2], couleurs[3]))
		#ajouter les 3 segments pour le cercle central
		symbols(x=xCircles, y=yCircles, circles=rCircles, main = "PropCircles", fg=colorCircles, bg="white", add=TRUE, inches=FALSE)
		symbols(x=xSpeCircles, y=ySpeCircles, circles=rSpeCircles, main = "PropCircles", fg=colorSpeCircles, bg=colorSpeCircles, add=TRUE, inches=FALSE)
		
		taille=1.5
		#effectifs
		text(x=xAtot, y=(yAtot-rAtot)+((yA-rA)-(yAtot-rAtot))/2, labels=paste(n[1]-nA), cex=taille, col=coultxt[1], font=1)
		text(x=xA, y=yA, labels=paste(nA), cex=taille, col=colWhite, font=2)
		
		text(x=xBtot+(rB*cos(pi/6)), y=yBtot+(rB*sin(pi/6)), labels=paste(n[2]-nB), cex=taille, col=coultxt[2], font=1)
		text(x=xB, y=yB, labels=paste(nB), cex=taille, col=colWhite, font=2)
		
		text(x=xCtot-(rC*cos(pi/6)), y=yCtot+(rC*sin(pi/6)), labels=paste(n[3]-nC), cex=taille, col=coultxt[3], font=1)
		text(x=xC, y=yC, labels=paste(nC), cex=taille, col=colWhite, font=2)
		
		text(x=xAB, y=yAB, labels=paste(nAB), cex=taille, col="black", font=1)
		text(x=xAC, y=yAC, labels=paste(nAC), cex=taille, col="black", font=1)
		text(x=xBC, y=yBC, labels=paste(nBC), cex=taille, col="black", font=1)
		text(x=xABC, y=yABC, labels=paste(nABC), cex=taille, col="black", font=1)
		
		text(x=0.1, y=(yAB+yAtot)/2, labels=paste("Total: ", tot_ugenes, collapse="\n"), cex=taille, col="black", font=2)
		
		#titres
		taille=1.4
		text(x=xAtot, y=ymax*1.2, labels=paste(colnames(res)[1], " (", n[1], ")", sep=""), cex=taille, col=couleurs[1], pos=1, offset=0)
		text(x=xAtot, y=ymax*1.2, labels=paste(colnames(res)[2], " (", n[2], ")", sep=""), cex=taille, col=couleurs[2], pos=1, offset=1.2)
		text(x=xAtot, y=ymax*1.2, labels=paste(colnames(res)[3], " (", n[3], ")", sep=""), cex=taille, col=couleurs[3], pos=1, offset=2.4)
		dev.off()
	}
	
	graph_prop_4<-function(path, res, nA, nB, nC, nD, nAB, nAC, nBD, nCD, nAD, nBC, nABC, nBCD, nACD, nABD, nABCD, tot_ugenes, noms, couleurs, tlog, colBlack)
	{
		if(!colBlack)  
		{
			colWhite="white"
			coultxt=couleurs
		}else{
			colWhite="black"
			coultxt = rep("black", length(couleurs))
		}
		
		n = apply(res[,1:(ncol(res)-1)], 2, function(x) sum(as.numeric(x)))
		
		if(tlog)
		{
			nr = round(log2(n))
			nrA = round(log2(nA))
			nrB = round(log2(nB))
			nrC = round(log2(nC))
			nrD = round(log2(nD))
			nrAB = round(log2(nAB))
			nrAC = round(log2(nAC))
			nrAD = round(log2(nAD))
			nrBC = round(log2(nBC))
			nrBD = round(log2(nBD))
			nrCD = round(log2(nCD))
			nrABC = round(log2(nABC))
			nrBCD = round(log2(nBCD))
			nrACD = round(log2(nACD))
			nrABD = round(log2(nABD))
			nrABCD = round(log2(nABCD))
		}else{    
			nr = round(n/max(n, nA, nB, nC, nD)*50)
			nrA = round(nA/max(n, nA, nB, nC, nD)*50)
			nrB = round(nB/max(n, nA, nB, nC, nD)*50)
			nrC = round(nC/max(n, nA, nB, nC, nD)*50)
			nrD = round(nD/max(n, nA, nB, nC, nD)*50)
			nrAB = round(nAB/max(n, nA, nB, nC)*50)
			nrAC = round(nAC/max(n, nA, nB, nC)*50)
			nrAD = round(nAD/max(n, nA, nB, nC)*50)
			nrBC = round(nBC/max(n, nA, nB, nC)*50)
			nrBD = round(nBD/max(n, nA, nB, nC)*50)
			nrCD = round(nCD/max(n, nA, nB, nC)*50)
			nrABC = round(nABC/max(n, nA, nB, nC)*50)
			nrBCD = round(nBCD/max(n, nA, nB, nC)*50)
			nrACD = round(nACD/max(n, nA, nB, nC)*50)
			nrABD = round(nABD/max(n, nA, nB, nC)*50)
			nrABCD = round(nABCD/max(n, nA, nB, nC)*50)
		}
		
		#calculs des rayons pour que la surface des cercles reflete la taille des listes
		if(is.infinite(nr[1])){rAtot=0}else{rAtot=sqrt(nr[1]/pi)}
		if(is.infinite(nr[2])){rBtot=0}else{rBtot=sqrt(nr[2]/pi)}
		if(is.infinite(nr[3])){rCtot=0}else{rCtot=sqrt(nr[3]/pi)}
		if(is.infinite(nr[4])){rDtot=0}else{rDtot=sqrt(nr[4]/pi)}
		if(is.infinite(nrA)){rA=0}else{rA=sqrt(nrA/pi)}
		if(is.infinite(nrB)){rB=0}else{rB=sqrt(nrB/pi)}
		if(is.infinite(nrC)){rC=0}else{rC=sqrt(nrC/pi)}
		if(is.infinite(nrD)){rD=0}else{rD=sqrt(nrD/pi)}
		if(is.infinite(nrAB)){rAB=0}else{rAB=sqrt(nrAB/pi)}
		if(is.infinite(nrBC)){rBC=0}else{rBC=sqrt(nrBC/pi)}
		if(is.infinite(nrAC)){rAC=0}else{rAC=sqrt(nrAC/pi)}    
		if(is.infinite(nrAD)){rAD=0}else{rAD=sqrt(nrAD/pi)}
		if(is.infinite(nrBD)){rBD=0}else{rBD=sqrt(nrBD/pi)}
		if(is.infinite(nrCD)){rCD=0}else{rCD=sqrt(nrCD/pi)}
		if(is.infinite(nrABC)){rABC=0}else{rABC=sqrt(nrABC/pi)}
		if(is.infinite(nrABD)){rABD=0}else{rABD=sqrt(nrABD/pi)}
		if(is.infinite(nrBCD)){rBCD=0}else{rBCD=sqrt(nrBCD/pi)}
		if(is.infinite(nrACD)){rACD=0}else{rACD=sqrt(nrACD/pi)}
		if(is.infinite(nrABCD)){rABCD=0}else{rABCD=sqrt(nrABCD/pi)}
		
		calc_coord<-function(rAtot, rBtot, rCtot, rDtot, rA, rB, rC, rD, rAB, rAC, rAD, rBC, rBD, rCD, rABC, rABD, rACD, rBCD, rABCD, expy, expx)
		{
			yCtot = max(rCtot, rCD, rDtot)*expy
			yDtot = yCtot
			yCD = yCtot
			yAtot = (yCtot + max(rCtot, rCD, rDtot) + max(rACD, rBD, rBCD) + 2*max(rAC, rABCD, rBD) + max(rABC, rAC, rABD) + max(rAtot, rBtot, rAB))*expy
			yBtot = yAtot
			yAB = yAtot
			yAC = (yAtot + yCtot)/2
			yABCD = yAC
			yBD = yAC
			yACD = (yCtot + yAC)/2
			yBC = yACD
			yBCD = yACD
			yABC = (yAtot + yAC)/2
			yAD = yABC
			yABD = yABC
			
			xAtot = max(rAtot, rAC, rCtot)*expy
			xCtot = xAtot
			xAC = xAtot
			xBtot = (xAtot + rAtot + max(rABC, rACD) + 2*max(rAB, rAD, rABCD, rBD, rCD) + max(rABD, rBCD) + max(rBtot, rBD, rDtot))*expx
			xBD = xBtot
			xDtot = xBtot
			xAB = (xAtot + xBtot)/2
			xAD = xAB
			xABCD = xAB
			xBC = xAB
			xCD = xAB
			xABC = (xAtot + xAB)/2
			xACD = xABC
			xABD = (xAB + xBtot)/2
			xBCD = xABD
			
			#spe
			xA = xAtot - (rAtot-rA)*cos(pi/4)
			yA = yAtot + (rAtot-rA)*sin(pi/4)
			xB = xBtot + (rBtot-rB)*cos(pi/4)
			yB = yBtot + (rBtot-rB)*sin(pi/4)
			xC = xCtot - (rCtot-rC)*cos(pi/4)
			yC = yCtot - (rCtot-rC)*sin(pi/4)
			xD = xDtot + (rDtot-rD)*cos(pi/4)
			yD = yDtot - (rDtot-rD)*sin(pi/4)
			
			xCircles = c(xAtot, xBtot, xCtot, xDtot, xAB, xAC, xAD, xBC, xBD, xCD, xABC, xABD, xACD, xBCD, xABCD, xA, xB, xC, xD)
			yCircles = c(yAtot, yBtot, yCtot, yDtot, yAB, yAC, yAD, yBC, yBD, yCD, yABC, yABD, yACD, yBCD, yABCD, yA, yB, yC, yD)
			rCircles = c(rAtot, rBtot, rCtot, rDtot, rAB, rAC, rAD, rBC, rBD, rCD, rABC, rABD, rACD, rBCD, rABCD, rA, rB, rC, rD)
			colorCircles = c(couleurs[1], couleurs[2], couleurs[3], couleurs[4], "black", "black", "black", "black", "black", "black", "black", "black", "black", "black", "black", "blue", "red", "green", "orange")
			data_graph=cbind(xCircles, yCircles, rCircles, colorCircles)
			colnames(data_graph) = c("xCircles", "yCircles", "rCircles", "colorCircles")
			rownames(data_graph) = c("Atot", "Btot", "Ctot", "Dtot", "AB", "AC", "AD", "BC", "BD", "CD", "ABC", "ABD", "ACD", "BCD", "ABCD", "A", "B", "C", "D")
			
			#ajustement, taille fenetre
			
			
			return(data_graph)
		}
		
		expx = 1
		expy = 1
		data_graph = calc_coord(rAtot, rBtot, rCtot, rDtot, rA, rB, rC, rD, rAB, rAC, rAD, rBC, rBD, rCD, rABC, rABD, rACD, rBCD, rABCD, expy, expx)
		#calcul de l'ajustement
		adjust<-function(data_graph)
		{
			xAll = as.matrix(as.numeric(data_graph[,"xCircles"]))
			rownames(xAll) = rownames(data_graph)
			yAll = as.matrix(as.numeric(data_graph[,"yCircles"]))
			rownames(yAll) = rownames(data_graph)
			rAll = as.matrix(as.numeric(data_graph[,"rCircles"]))
			rownames(rAll) = rownames(data_graph)
			
			# en x
			dC_CD = abs(sqrt((xAll["Ctot",]-xAll["CD",])^2) - (rAll["Ctot",]+rAll["CD",])) #C-CD
			dCD_D = abs(sqrt((xAll["Dtot",]-xAll["CD",])^2) - (rAll["Dtot",]+rAll["CD",])) #CD-D
			dA_AB = abs(sqrt((xAll["Atot",]-xAll["AB",])^2) - (rAll["Atot",]+rAll["AB",])) #A-AB
			dAB_B = abs(sqrt((xAll["Btot",]-xAll["AB",])^2) - (rAll["Btot",]+rAll["AB",])) #AB-B
			#ACD-BC, BC-BCD, AC-ABCD, ABCD-BD, ABC-AD, AD-ABD
			
			# en y
			dA_AC = abs(sqrt((yAll["Atot",]-yAll["AC",])^2) - (rAll["Atot",]+rAll["AC",])) #A-AC
			dAC_C = abs(sqrt((yAll["Ctot",]-yAll["AC",])^2) - (rAll["Ctot",]+rAll["AC",])) #AC-C
			dAB_AD = abs(sqrt((yAll["AB",]-yAll["AD",])^2) - (rAll["AB",]+rAll["AD",])) #AB-AD
			dBC_CD = abs(sqrt((yAll["BC",]-yAll["CD",])^2) - (rAll["BC",]+rAll["CD",])) #BC-CD
			dB_BD = abs(sqrt((yAll["Btot",]-yAll["BD",])^2) - (rAll["Btot",]+rAll["BD",])) #B-BD
			dBD_D = abs(sqrt((yAll["Dtot",]-yAll["BD",])^2) - (rAll["Dtot",]+rAll["BD",])) #BD-D
			#ABC-ACD, AD-ABCD, ABCD-BC, ABD-BCD
			
			# en x et y
			dC_ACD = abs(sqrt((xAll["Ctot",]-xAll["ACD",])^2+(yAll["Ctot",]-yAll["ACD",])^2)-(rAll["Ctot",]+rAll["ACD",])) #C-ACD
			dABD_B = abs(sqrt((xAll["Btot",]-xAll["ABD",])^2+(yAll["Btot",]-yAll["ABD",])^2)-(rAll["Btot",]+rAll["ABD",])) #ABD-B
			dA_ABC = abs(sqrt((xAll["Atot",]-xAll["ABC",])^2+(yAll["Atot",]-yAll["ABC",])^2)-(rAll["Atot",]+rAll["ABC",])) #A-ABC
			dBCD_D = abs(sqrt((xAll["Dtot",]-xAll["BCD",])^2+(yAll["Dtot",]-yAll["BCD",])^2)-(rAll["Dtot",]+rAll["BCD",])) #BCD-D
			#ACD-ABCD, ABCD-ABD, ABD-ABCD, ABCD-BCD
			
			#ajustement en x
			dx = max(dC_CD, dCD_D, dA_AB, dAB_B, dC_ACD/2, dABD_B/2, dA_ABC/2, dBCD_D/2)
			dy = max(dA_AC, dAC_C, dAB_AD, dBC_CD, dB_BD, dBD_D, dC_ACD/2, dABD_B/2, dA_ABC/2, dBCD_D/2)
			res = c(dx, dy)
			return(res)
		}
		
		#si ajustement expx, expy
		d = adjust(data_graph)
		data_graph = calc_coord(rAtot, rBtot, rCtot, rDtot, rA, rB, rC, rD, rAB, rAC, rAD, rBC, rBD, rCD, rABC, rABD, rACD, rBCD, rABCD, expy=(1+(expy*d[2]/ymax)*1.2), expx=(1+(expx*(d[1]/xmax))*1.2))
		
		xmin = (min(as.numeric(data_graph[,"xCircles"])) - max(as.numeric(data_graph[,"rCircles"])))
		xmax = (max(as.numeric(data_graph[,"xCircles"])) + max(as.numeric(data_graph[,"rCircles"])))
		ymin = (min(as.numeric(data_graph[,"yCircles"])) - max(as.numeric(data_graph[,"rCircles"])))
		ymax = (max(as.numeric(data_graph[,"yCircles"])) + max(as.numeric(data_graph[,"rCircles"])))*1.2
		
		png(filename = paste(path, "/venn_diagram_prop", if(tlog) paste("_tlog2", sep=""), ".png", sep=""), width=30, height=30, bg="white", units='cm', res=300)
		plot.new()
		par(mar=c(0,0,0,0))
		plot.window(c(xmin, xmax), c(ymin, ymax), asp=1)
		
		#couleurs
		#bleu
		segments(
				x0=as.numeric(c(data_graph["Atot", "xCircles"], data_graph["Atot", "xCircles"], data_graph["Atot", "xCircles"], data_graph["Atot", "xCircles"], data_graph["Atot", "xCircles"], data_graph["Atot", "xCircles"], data_graph["Atot", "xCircles"])),
				y0=as.numeric(c(data_graph["Atot", "yCircles"], data_graph["Atot", "yCircles"], data_graph["Atot", "yCircles"], data_graph["Atot", "yCircles"], data_graph["Atot", "yCircles"], data_graph["Atot", "yCircles"], data_graph["Atot", "yCircles"])),
				x1=as.numeric(c(data_graph["AD", "xCircles"], data_graph["AC", "xCircles"], data_graph["AB", "xCircles"], data_graph["ABD", "xCircles"], data_graph["ABC", "xCircles"], data_graph["ACD", "xCircles"], data_graph["ABCD", "xCircles"])),
				y1=as.numeric(c(data_graph["AD", "yCircles"], data_graph["AC", "yCircles"], data_graph["AB", "yCircles"], data_graph["ABD", "yCircles"], data_graph["ABC", "yCircles"], data_graph["ACD", "yCircles"], data_graph["ABCD", "yCircles"])), col="blue")
		#rouge
		segments(
				x0=as.numeric(c(data_graph["Btot", "xCircles"], data_graph["Btot", "xCircles"], data_graph["Btot", "xCircles"], data_graph["Btot", "xCircles"], data_graph["Btot", "xCircles"], data_graph["Btot", "xCircles"], data_graph["Btot", "xCircles"])),
				y0=as.numeric(c(data_graph["Btot", "yCircles"], data_graph["Btot", "yCircles"], data_graph["Btot", "yCircles"], data_graph["Btot", "yCircles"], data_graph["Btot", "yCircles"], data_graph["Btot", "yCircles"], data_graph["Btot", "yCircles"])),
				x1=as.numeric(c(data_graph["BC", "xCircles"], data_graph["BD", "xCircles"], data_graph["AB", "xCircles"], data_graph["ABD", "xCircles"], data_graph["ABC", "xCircles"], data_graph["BCD", "xCircles"], data_graph["ABCD", "xCircles"])),
				y1=as.numeric(c(data_graph["BC", "yCircles"], data_graph["BD", "yCircles"], data_graph["AB", "yCircles"], data_graph["ABD", "yCircles"], data_graph["ABC", "yCircles"], data_graph["BCD", "yCircles"], data_graph["ABCD", "yCircles"])), col="red")
		#vert
		segments(
				x0=as.numeric(c(data_graph["Ctot", "xCircles"], data_graph["Ctot", "xCircles"], data_graph["Ctot", "xCircles"], data_graph["Ctot", "xCircles"], data_graph["Ctot", "xCircles"], data_graph["Ctot", "xCircles"], data_graph["Ctot", "xCircles"])),
				y0=as.numeric(c(data_graph["Ctot", "yCircles"], data_graph["Ctot", "yCircles"], data_graph["Ctot", "yCircles"], data_graph["Ctot", "yCircles"], data_graph["Ctot", "yCircles"], data_graph["Ctot", "yCircles"], data_graph["Ctot", "yCircles"])),
				x1=as.numeric(c(data_graph["BC", "xCircles"], data_graph["CD", "xCircles"], data_graph["AC", "xCircles"], data_graph["ABC", "xCircles"], data_graph["ACD", "xCircles"], data_graph["BCD", "xCircles"], data_graph["ABCD", "xCircles"])),
				y1=as.numeric(c(data_graph["BC", "yCircles"], data_graph["CD", "yCircles"], data_graph["AC", "yCircles"], data_graph["ABC", "yCircles"], data_graph["ACD", "yCircles"], data_graph["BCD", "yCircles"], data_graph["ABCD", "yCircles"])), col="green")
		#orange
		segments(
				x0=as.numeric(c(data_graph["Dtot", "xCircles"], data_graph["Dtot", "xCircles"], data_graph["Dtot", "xCircles"], data_graph["Dtot", "xCircles"], data_graph["Dtot", "xCircles"], data_graph["Dtot", "xCircles"], data_graph["Dtot", "xCircles"])),
				y0=as.numeric(c(data_graph["Dtot", "yCircles"], data_graph["Dtot", "yCircles"], data_graph["Dtot", "yCircles"], data_graph["Dtot", "yCircles"], data_graph["Dtot", "yCircles"], data_graph["Dtot", "yCircles"], data_graph["Dtot", "yCircles"])),
				x1=as.numeric(c(data_graph["CD", "xCircles"], data_graph["AD", "xCircles"], data_graph["BD", "xCircles"], data_graph["ABD", "xCircles"], data_graph["ACD", "xCircles"], data_graph["BCD", "xCircles"], data_graph["ABCD", "xCircles"])),
				y1=as.numeric(c(data_graph["CD", "yCircles"],data_graph["AD", "yCircles"], data_graph["BD", "yCircles"], data_graph["ABD", "yCircles"], data_graph["ACD", "yCircles"], data_graph["BCD", "yCircles"], data_graph["ABCD", "yCircles"])), col="orange")
		
		symbols(x=as.numeric(data_graph[1:15,"xCircles"]), y=as.numeric(data_graph[1:15,"yCircles"]), circles=as.numeric(data_graph[1:15,"rCircles"]), main = "PropCircles", fg=data_graph[1:15,"colorCircles"], bg="white", add=TRUE, inches=FALSE)
		symbols(x=as.numeric(data_graph[16:19,"xCircles"]), y=as.numeric(data_graph[16:19,"yCircles"]), circles=as.numeric(data_graph[16:19,"rCircles"]), main = "PropCircles", fg=data_graph[16:19,"colorCircles"], bg=data_graph[16:19,"colorCircles"], add=TRUE, inches=FALSE)
		
		#titres
		taille = 1.4
		text(x=xmax/2, y=ymax, labels=paste(colnames(res)[1], " (", n[1], ")", sep=""), cex=taille, col=couleurs[1], pos=1, offset=0)
		text(x=xmax/2, y=ymax, labels=paste(colnames(res)[2], " (", n[2], ")", sep=""), cex=taille, col=couleurs[2], pos=1, offset=1.2)
		text(x=xmax/2, y=ymax, labels=paste(colnames(res)[3], " (", n[3], ")", sep=""), cex=taille, col=couleurs[3], pos=1, offset=2.4)
		text(x=xmax/2, y=ymax, labels=paste(colnames(res)[4], " (", n[4], ")", sep=""), cex=taille, col=couleurs[4], pos=1, offset=3.6)
		
		xAll = as.matrix(as.numeric(data_graph[,"xCircles"]))
		rownames(xAll) = rownames(data_graph)
		yAll = as.matrix(as.numeric(data_graph[,"yCircles"]))
		rownames(yAll) = rownames(data_graph)
		rAll = as.matrix(as.numeric(data_graph[,"rCircles"]))
		rownames(rAll) = rownames(data_graph)
		
		xAll = as.matrix(as.numeric(data_graph[,"xCircles"]))
		rownames(xAll) = rownames(data_graph)
		yAll = as.matrix(as.numeric(data_graph[,"yCircles"]))
		rownames(yAll) = rownames(data_graph)
		rAll = as.matrix(as.numeric(data_graph[,"rCircles"]))
		rownames(rAll) = rownames(data_graph)
		
		#effectifs
		text(x=(xAll["Atot",]+(2*rAll["A",]/3)), y=(yAll["Atot",]-(2*rAll["A",]/3)), labels=paste(n[1]-nA), cex=taille, col=coultxt[1], font=1)
		text(x=xAll["A",], y=yAll["A",], labels=paste(nA), cex=taille, col=colWhite, font=2)
		text(x=(xAll["Btot",]-(2*rAll["B",]/3)), y=(yAll["Btot",]-(2*rAll["B",]/3)), labels=paste(n[2]-nB), cex=taille, col=coultxt[2], font=1)
		text(x=xAll["B",], y=yAll["B",], labels=paste(nB), cex=taille, col=colWhite, font=2)
		text(x=(xAll["Ctot",]+(2*rAll["C",]/3)), y=(yAll["Ctot",]+(2*rAll["C",]/3)), labels=paste(n[3]-nC), cex=taille, col=coultxt[3], font=1)
		text(x=xAll["C",], y=yAll["C",], labels=paste(nC), cex=taille, col=colWhite, font=2)
		text(x=(xAll["Dtot",]-(2*rAll["D",]/3)), y=(yAll["Dtot",]+(2*rAll["D",]/3)), labels=paste(n[4]-nD), cex=taille, col=coultxt[4], font=1)
		text(x=xAll["D",], y=yAll["D",], labels=paste(nD), cex=taille, col=colWhite, font=2)
		
		text(x=(xmin+xAll["A",])/2, y=(yAll["Atot",]+yAll["AC",])/2, labels=paste("Total: ", tot_ugenes, collapse="\n"), cex=taille, col="black", font=2)
		
		text(x=c(xAll["AB",], xAll["AC",], xAll["AD",], xAll["BC",], xAll["BD",], x=xAll["CD",],xAll["ABC",], xAll["ABD",],xAll["ACD",], xAll["BCD",],xAll["ABCD",]),
				y=c(yAll["AB",], yAll["AC",],yAll["AD",], yAll["BC",], yAll["BD",], yAll["CD",], yAll["ABC",], yAll["ABD",], yAll["ACD",], yAll["BCD",], yAll["ABCD",]),
				labels=c(paste(nAB),paste(nAC),paste(nAD),paste(nBC), paste(nBD), paste(nCD), paste(nABC), paste(nABD), paste(nACD),paste(nBCD),paste(nABCD)),
				cex=taille, col="black", font=1)
		
		dev.off()
	}
	
	##################################################################################################################################################
	##################################################################################################################################################
	##################################################################################################################################################
	##################################################################################################################################################
	
	if(FilesOut)
	{
		if(pathRes == "")
		{
			if(!file.exists(paste(getwd(), "/Venn.diagrams/", sep=""))) dir.create(paste(getwd(), "/Venn.diagrams/", sep=""))
			pathRes = paste(getwd(), "/Venn.diagrams/", sep="")
			if(display) write(paste("Default results path is: \n\t", pathRes, sep=""), file="")
			flush.console()
		}
		
		if((CompName=="")&(pathLists!="")) CompName = basename(pathLists)
		if(CompName!="")  path = paste(pathRes, "/Venn_", CompName, sep="")
		if(CompName=="")  path = paste(pathRes, "/Venn_", format(Sys.time(), "(%H-%M-%S)_%a_%d_%b_%Y"), sep="")
		dir.create(path)
		if(display) write(paste("The results will be placed here: \n\t", path, sep=""), file="")
		flush.console()
	}
	
	if(is.list(matLists)) # Si listes de listes ou matrices
	{
		if(Profils)	# Test la presence du fichier DataMoy.txt
		{
			# Teste la presence du DataMoy ET nbre de cols = nbre listes (ET noms cols = noms listes)
			if(sum(grepl("DataMoy", names(matLists)))!=0)
			{				
				DataMoy = matLists[[seq(1, length(matLists), by=1)[grepl("DataMoy", names(matLists))]]]	#	Extraction du DataMoy.txt
				matLists = matLists[seq(1, length(matLists), by=1)[!grepl("DataMoy", names(matLists))]]	#	suppression de matLists			    
				
				if(display)	write(paste("     => DataMoy and lists are OK." , sep=""), file="")
				flush.console()
			}else{
				if(display) 
					write(paste("     => DataMoy file not found, the profiles wil not be analysed." , sep=""), file="")
				flush.console()
				Profils = FALSE
			}
		}
		
		if(display) write(paste("     => Source = ", length(matLists), " listes" , sep=""), file="")
		flush.console()
		listes = matLists
		noms = names(matLists)
		nlistes = length(noms)
	}else{ # matLists = une matrice
		if((min(matLists[!is.na(matLists)])==0)&(max(matLists[!is.na(matLists)])==1))  # Matrice binaire
		{
			if(display) write(paste("     => Source = binary matrix" , sep=""), file="")
			flush.console()
			matLists[is.na(matLists)]=""
			res = cbind(matLists, apply(matLists, 1, function(x) sum(as.numeric(x))))
			colnames(res)[ncol(matLists)+1] = "Total"
			ud = FALSE
			nlistes = ncol(matLists)
		}else{
			if(sum(!is.na(as.numeric(matLists)))!=0)  # Liste numerique, folds ou ratios
			{
				if(min(matLists[!is.na(matLists)])>0)  # Matrice de ratios
				{
					if(display)	write(paste("     => Source = matrix of ratios" , sep=""), file="")
					flush.console()
					# Ajoute la matrice binaire
					MatBin = matLists
					MatBin[is.na(matLists)]=0
					MatBin[!is.na(matLists)]=1
					matLists[is.na(matLists)]=""
					res = cbind(MatBin, apply(MatBin, 1, function(x) sum(x)), matLists)
					colnames(res)[ncol(MatBin)+1] = "Total"
					colnames(res)[(ncol(MatBin)+2):ncol(res)] = paste("ratios_", colnames(MatBin), sep="")
					nlistes = ncol(matLists)
				}else{
					if(min(matLists[!is.na(matLists)])<0)  # Matrice de folds
					{
						if(display) write(paste("     => Source = matrix of folds" , sep=""), file="")
						flush.console()
						
						TempListeRatio = matLists
						for(y in 1:ncol(matLists))	#	transforme les folds en ratios
							for(x in 1:nrow(matLists))
								if(!is.na(matLists[x, y])&(matLists[x, y]<0)) TempListeRatio[x, y] = -1/matLists[x, y]
						colnames(TempListeRatio) = paste("ratios_", colnames(TempListeRatio), sep="")
						
						# Ajoute la matrice binaire
						MatBin = TempListeRatio
						MatBin[is.na(TempListeRatio)]=0
						MatBin[!is.na(TempListeRatio)]=1
						matLists[is.na(matLists)]=""
						res = cbind(MatBin, apply(MatBin, 1, function(x) sum(x)), TempListeRatio)
						colnames(res)[ncol(MatBin)+1] = "Total"
						colnames(res)[(ncol(MatBin)+2):ncol(res)] = paste("ratios_", colnames(MatBin), sep="")
						#data_all = res # Ajoute les ratios a la matrice bin
						#data_all[is.na(data_all)]=""
						nlistes = ncol(matLists)
					}
				}
			}else{  # Liste d'IDs
				ud=FALSE
				listes = matLists 
				noms = names(matLists)  
			}
		}
	}
	
	if(!is.matrix(res))	# Si res est vide: ce n est pas un exemple => lecture de fichiers
	{
		# Traitement 1ere liste, amorce les variables
		if(!is.list(matLists)) data_t = test_list(liste=listes[1], type="Res", matLists, noms, path)
		if(is.list(matLists)) data_t = matLists[[1]]
		if(length(noms)==1)  noms=c("A", "B", "C", "D")[1:length(listes)]
		if((length(noms)>1)&(length(noms)!=length(listes)))
		{
			write(paste("Only ", length(noms), " names for ", length(listes), " lists.\nThe default names ", c("A", "B", "C", "D")[1:length(listes)], " will be used.", sep=""), file="")
			flush.console()
		}
		
		if(length(couleurs)==1)
		{
			couleurs=c("blue", "red", "green", "orange")[1:length(listes)]
			couleursIn = c(rgb(0, 0, 1, transp), rgb(1, 0, 0, transp), rgb(0, 1, 0, transp), rgb(1, 0.6470588, 0, transp))[1:length(listes)]
		}
		
		if((length(couleurs)>1)&(length(couleurs)!=length(listes)))
		{
			write(paste("Only ", length(couleurs), " colours for ", length(listes), " lists.\nThe default colours ", c("blue", "red", "green", "orange")[1:length(listes)]  , " will be used.", sep=""), file="")
			flush.console()
		}
		
		if(is.null(length(listes)))
		{
			write("The directory is empty.", file="")
			flush.console()
			break
		}
		
		nomListe = noms[1]
		Cud=TRUE
		if(sum(is.na(as.numeric(data_t[,grepl("atio", colnames(data_t))])))!=0)
		{
			write(paste("\t !!!  The \"ratio\" column of the ", nomListe, " list is not numeric.", sep=""), file="")
			flush.console()
			Cud=FALSE
		}
		if(ud&(sum(grepl("atio", colnames(data_t)))==0))
		{
			write(paste("\t !!! No \"ratio\" column detected in ", nomListe, sep=""), file="")
			flush.console()
			Cud=FALSE
		}
		
		if(ncol(data_t)>1)  data_t = rownames(data_t)
		
		res = matrix(1, ncol=1, nrow=length(data_t))
		rownames(res) = data_t
		if(!is.list(matLists)) noms_listes = paste(substr(basename(listes[1]), 0, (nchar(basename(listes[1]))-4)), "_(", length(data_t), ")", sep="")
		if(is.list(matLists)) noms_listes = noms
		
		if(!ud) Cud=FALSE
		for(i in 2:length(listes))
		{
			if(!is.list(matLists)) data_t = test_list(liste=listes[i], type="Res", matLists, noms, path)
			if(is.list(matLists)) data_t = matLists[[i]]
			
			nomListe = noms[i]
			
			if(Cud&(sum(grepl("atio", colnames(data_t)))!=0))  # teste la presence de la col ratio et le type de data
			{
				if(!is.null(dim(data_t[,grepl("atio", colnames(data_t))])))
				{
					write(paste("\t !!!  The \"ratio\" column is not unic !", sep=""), file="")
					flush.console()
					Cud=FALSE
				}
				if(sum(is.na(as.numeric(data_t[,grepl("atio", colnames(data_t))])))!=0)
				{
					write(paste("\t !!!  The \"ratio\" column of the ", nomListe, " list is not numeric.", sep=""), file="")
					flush.console()
					Cud=FALSE
				}
				
			}
			if(ud&(sum(grepl("atio", colnames(data_t)))==0))
			{
				write(paste("\t !!! No \"ratio\" column detected in ", nomListe, sep=""), file="")
				flush.console()
				Cud=FALSE
			}
			
			if(ncol(data_t)>1)  data_t = rownames(data_t)
			
			#id a ajouter a res ~ new sans les communs
			dupli_new = duplicated(c(rownames(res), data_t))
			dupli_new = dupli_new[(nrow(res)+1):length(dupli_new)]
			temp_new = matrix(1, ncol=1, nrow=length(dupli_new))
			temp_new[dupli_new] = 0
			rownames(temp_new) = data_t
			
			#id a ajouter a new ~ old sans les communs
			old = duplicated(c(data_t, rownames(res)))
			old = old[(length(data_t)+1):length(old)]
			temp_old = matrix(1, ncol=1, nrow=length(old))
			temp_old[old] = 0
			rownames(temp_old) = rownames(res)
			
			#completion de la matrice res
			if(sum(temp_new)!=0)
			{
				res = rbind(res, matrix(0, ncol=ncol(res), nrow=sum(temp_new)))
				rownames(res)[(nrow(res)-sum(temp_new)+1):nrow(res)] = rownames(temp_new)[temp_new==1]
				res = res[order(rownames(res)),]
			}
			
			ajout = rbind(matrix(0, ncol=1, nrow=sum(temp_old)), matrix(1, ncol=1, nrow=length(data_t)))
			rownames(ajout) = c(rownames(temp_old)[temp_old==1], data_t)
			ajout = ajout[order(rownames(ajout)),]
			
			if(!is.list(matLists)) noms_listes = c(noms_listes, paste(substr(basename(listes[i]), 0, (nchar(basename(listes[i]))-4)), "_(", length(data_t), ")", sep=""))
			if(is.list(matLists)) noms_listes = noms
			res = cbind(res, as.matrix(ajout))
		}
		colnames(res) = noms_listes
		
		if(ud&!Cud)
		{
			write(paste("\t !!!  The modulations will not be analyzed.", sep=""), file="")
			flush.console()
			ud=FALSE
		}
		
		#colonne somme: nbre de listes pour chaque id
		res = cbind(res, apply(res, 1, function(x) sum(x)))
		colnames(res)[ncol(res)] = "Total_lists"
		if(annot)
		{
			#ajout des datas de chaque liste
			#res = matrice des appartenances
			data_all = res
			for(M in 1:length(listes)) #liste par liste
			{
				#lecture du fichier
				if(!is.list(matLists)) data_t = test_list(liste=listes[M], type="Annot", matLists, noms, path, affich=FALSE)
				if(is.list(matLists)) data_t = matLists[[M]]
				#ajoute une colonne vide entre les annots de chaque liste
				data_all = cbind(data_all, matrix("", ncol=1, nrow=nrow(data_all)))
				if(!is.null(ncol(data_t)))
				{
					data_all = data_all[order(rownames(data_all)),]  #classe tous les IDs
					data_all = data_all[order(data_all[,M], decreasing = TRUE),]  #regroupe en tete les IDs classes de la liste en cours
					data_t = data_t[order(rownames(data_t)),] #classe les IDs de la liste en cours
					data_all = cbind(data_all, rbind(data_t, matrix("", ncol=ncol(data_t), nrow=nrow(data_all)-nrow(data_t))))
				}
			}
			
			#ajoute une colonne de profil d'expression resume UD
			concat<-function(x)
			{
				rescat = NULL
				for(O in 1:length(x))
				{
					rescat = paste(rescat, x[O], sep="")
				}
				return(rescat)
			}
			
			if(ud)
			{
				# 1- recupere les colonnes ratios => dans l'ordre
				profils = data_all[,grepl("atio", colnames(data_all))]
				
				# 2- codage des modulations
				UDprofils = matrix("", ncol=ncol(profils), nrow=nrow(profils))
				for(P in 1:ncol(profils))
				{
					UDprofils[as.numeric(profils[,P])<1, P] = "D"
					UDprofils[as.numeric(profils[,P])>1, P] = "U"
				}
				UDprofils[UDprofils==""] = "n"
				UDp = as.matrix(apply(UDprofils, 1, function(x) concat(x)))
				data_all = cbind(data_all[,1:length(listes)], UDp, data_all[,(length(listes)+1):ncol(data_all)])
				colnames(data_all)[length(listes)+1] = "Profils"
			}
			if(FilesOut)
			{
				if(sum(grepl("Profils", colnames(data_all)))==1)  data_all=data_all[order(data_all[,"Profils"], decreasing=TRUE),]
				for(L in 1:length(listes))  data_all=data_all[order(data_all[,L], decreasing=TRUE),]
				
				dataAlltmp = cbind(rownames(data_all), data_all)
				write.table(dataAlltmp, file = paste(path, "/VennAnnot.txt", sep=""), sep="\t", row.names = FALSE, quote=FALSE)
			}
		}else{
			if(!FilesOut)  ud=FALSE  # si Files Out et Annot=FALSE, pas de ud (qui est essentielement une optio graphique)
			if(!annot&ud)
			{
				data_all = res
				for(M in 1:length(listes)) #liste par liste
				{
					#lecture du fichier
					if(!is.list(matLists)) data_t = test_list(liste=listes[M], type="Annot", matLists, noms, path)
					if(is.list(matLists)) data_t = test_list(liste=noms[M], type="Annot", matLists, noms, path)
					if(sum(grepl("atio", colnames(data_t)))==1)
					{
						data_all = data_all[order(rownames(data_all)),]
						data_all = data_all[order(data_all[,M], decreasing = TRUE),]
						data_t = data_t[order(rownames(data_t)),]
						data_all = cbind(data_all, rbind(as.matrix(data_t[,grepl("atio", colnames(data_t))]), matrix(NA, ncol=1, nrow=nrow(data_all)-nrow(data_t))))
						#colnames(data_all)[ncol(data_all)] = "ratios"
					}else{
						if(!is.list(matLists)) print(paste("\"ratio\" column not found in ", basename(listes[M]), " file.", sep=""))
						if(is.list(matLists)) print(paste("\"ratio\" column not found in ", noms[M], " file.", sep=""))
					}
				}
			}
		}
	}else{
		data_all = res
		res = res[,1:grep("total", colnames(res), ignore.case=TRUE)]
		nlistes = grep("total", colnames(res), ignore.case=TRUE)-1  # Calcul le nbre de listes
		if(is.null(names(res)))
		{
			noms=c("A", "B", "C", "D")[1:nlistes]
		}else{
			noms = names(res)
		}
		if((noms!="")&(length(noms)!=nlistes))
		{
			write(paste("Only ", length(noms), " names for ", nlistes, " lists.\nThe default names ", c("A", "B", "C", "D")[1:nlistes], " will be used.", sep=""), file="")
			flush.console()
		}
		noms = toupper(noms)
		if(length(couleurs)==1)
		{
			couleurs=c("blue", "red", "green", "orange")[1:nlistes]
			couleursIn = c(rgb(0, 0, 1, transp), rgb(1, 0, 0, transp), rgb(0, 1, 0, transp), rgb(1, 0.6470588, 0, transp))[1:nlistes]
		}
		if((couleurs!="")&(length(couleurs)!=nlistes))
		{
			write(paste("Only ", length(couleurs), " colours for ", nlistes, " lists.\nThe default colours ", c("blue", "red", "green", "orange")[1:nlistes]  , " will be used.", sep=""), file="")
			flush.console()
		}
	}  
	
	if(FilesOut)
	{
		resTmp = cbind(rownames(res), res)
		write.table(resTmp, file = paste(path, "/VennMatrixBin.txt", sep=""), sep="\t", row.names = FALSE, quote=FALSE)
	}
	tot_ugenes = nrow(res)  #nbre de genes ou id uniques
	
	if(ud)
	{
		#matrice numerique des appartenances (res) et les ratios
		nliste = (grep("total", colnames(res), ignore.case=TRUE)-1)
		data_all = data_all[order(rownames(data_all)),]
		res = res[order(rownames(res)),]
		
		# ajouter seulement les ratios au dela de la colonne total
		data_r = cbind(res[,1:(nliste+1)], data_all[,grep("atio", colnames(data_all))[grep("atio", colnames(data_all))>ncol(res)]])        
		data_rt = matrix(0, ncol=0, nrow=nrow(data_r))
		data_rt = cbind(data_rt, apply(data_r, 2, function(x) as.matrix(as.numeric(x))))
		colnames(data_rt) = colnames(data_r)
		data_r = data_rt
		data_r[is.na(data_r)] = ""
		
		if(Profils)
		{
			# Selection des DataMoy et pour les profils connus
			DataMoy = DataMoy[duplicated(c(rownames(data_all), rownames(DataMoy)))[(nrow(data_all)+1):(nrow(data_all)+nrow(DataMoy))],]
			DataMoy = DataMoy[order(rownames(DataMoy)),]
			data_all = data_all[order(rownames(data_all)),]
			
			DataMoyLog2 = tolog2(DataMoy)
			
			ProfilsPath = paste(path, "/Profiles/", sep="")
			if(!file.exists(ProfilsPath)) dir.create(ProfilsPath)
			
			for(TotLists in names(table(data_all[,"Total_lists"])))
			{
				#write(paste("#     C_", TotLists, sep=""), file="")
				data_f = data_all[data_all[,"Total_lists"]==TotLists,]
				DataTmpLog2 = DataMoyLog2[data_all[,"Total_lists"]==TotLists,]
				png(filename = paste(path, "/Profiles_C", TotLists, ".png", sep=""), height=2000, width=2000, pointsize = 18, bg = "white")
				
				#if(length(names(table(data_f[,"Profils"])))<=6) old.par <- par(mfrow=c(length(names(table(data_f[,"Profils"]))), 1))
				if(length(names(table(data_f[,"Profils"])))<=12)  old.par <- par(mfrow=c((ceiling(length(names(table(data_f[,"Profils"])))/2)), 2))
				if(length(names(table(data_f[,"Profils"])))>12)  old.par <- par(mfrow=c((ceiling(length(names(table(data_f[,"Profils"])))/3)), 3))
				
				for(P in names(table(data_f[,"Profils"])))
				{
					data_p = data_f[data_f[,"Profils"]==P,]
					DataTmp_p = DataTmpLog2[data_f[,"Profils"]==P,]
					#write(paste("#      ", P, " (", sum(data_f[,"Profils"]==P), ")", sep=""), file="")
					write.table(cbind(DataTmp_p, rownames(data_p), data_p), file=paste(ProfilsPath, "/Profiles_", P, ".txt", sep=""), sep="\t", , quote=FALSE)
					
					coul="black"
					if(grepl("U", P)&(!grepl("D", P)))  coul="red"
					if(grepl("D", P)&(!grepl("U", P)))  coul="green"
					
					x = t(DataTmp_p)
					if(sum(data_f[,"Profils"]==P)==1)
					{ 
						y=matrix(rep(seq(1, ncol(DataTmpLog2), by=1), sum(data_f[,"Profils"]==P)), ncol=ncol(DataTmpLog2), byrow=TRUE)
						plot(y, x, type="l", lty=1, axes=FALSE, col=coul, xlab="", ylab="Log2Moy", main=paste(P, " (", sum(data_f[,"Profils"]==P), ")", sep=""))
					}else{
						y=t(matrix(rep(seq(1, ncol(DataTmpLog2), by=1), sum(data_f[,"Profils"]==P)), ncol=ncol(DataTmpLog2), byrow=TRUE))
						matplot(y, x, type="l", lty=1, axes=FALSE, col=coul, xlab="", ylab="Log2Moy", main=paste(P, " (", sum(data_f[,"Profils"]==P), ")", sep=""))
					}
					axis(1, 1:ncol(DataTmpLog2), colnames(DataTmpLog2))
					axis(2)
				}
				par(old.par)
				dev.off()
			}
		}
	}
	
	nomTot = colnames(res)[grep("total", colnames(res), ignore.case=TRUE)]
	
	#graphs
	if(FilesOut)
	{
		if(grep("total", colnames(res), ignore.case=TRUE)==3)
		{
			nA = nrow(res[(res[,nomTot]==1)&(res[,1]==1),])
			if(is.null(nA)) nA=1
			nB = nrow(res[(res[,nomTot]==1)&(res[,2]==1),])
			if(is.null(nB)) nB=1
			
			nAB = nrow(res[(res[,nomTot]==2)&(res[,1]==1)&(res[,2]==1),])
			if(is.null(nAB)) nAB=1
			
			listeA = colnames(res)[1]
			listeB = colnames(res)[2]
			
			if(ud)
			{
				nAud = data_r[(data_r[,nomTot]==1)&(data_r[,1]==1),4]
				nAu = length(nAud[nAud>1])
				nAd = length(nAud[nAud<1])
				nBud = data_r[(data_r[,nomTot]==1)&(data_r[,2]==1),5]
				nBu = length(nBud[nBud>1])
				nBd = length(nBud[nBud<1])
				
				nABud = data_r[(data_r[,nomTot]==2)&(data_r[,1]==1)&(data_r[,2]==1),4:5]
				nABud = compte(nABud)
				
				graph_2(path, listeA, listeB, nA, nB, nAB, tot_ugenes, noms, ud, nAu, nAd, nBu, nBd, nABud, couleurs, couleursIn, Solid)
			}else{
				graph_2(path, listeA, listeB, nA, nB, nAB, tot_ugenes, noms, ud, couleurs=couleurs, couleursIn=couleursIn, Solid=Solid)
			}
			if(prop)
			{
				if(max(nA, nB, nAB)/min(nA, nB, nAB)>10) # Si le ratio max/min>10 => log2
				{
					graph_prop_2(path, res[,1:grep("total", colnames(res), ignore.case=TRUE)], nA, nB, nAB, tot_ugenes, noms, couleurs=couleurs, tlog=TRUE, colBlack=colBlack)
					graph_prop_2(path, res[,1:grep("total", colnames(res), ignore.case=TRUE)], nA, nB, nAB, tot_ugenes, noms, couleurs=couleurs, tlog=FALSE, colBlack=colBlack)
				}else{
					graph_prop_2(path, res[,1:grep("total", colnames(res), ignore.case=TRUE)], nA, nB, nAB, tot_ugenes, noms, couleurs=couleurs, tlog=FALSE, colBlack=colBlack)
				}
			}
		}
		
		if(grep("total", colnames(res), ignore.case=TRUE)==4)
		{
			nA = nrow(res[(res[,nomTot]==1)&(res[,1]==1),])
			if(is.null(nA)) nA=1
			nB = nrow(res[(res[,nomTot]==1)&(res[,2]==1),])
			if(is.null(nB)) nB=1
			nC = nrow(res[(res[,nomTot]==1)&(res[,3]==1),])
			if(is.null(nC)) nnCA=1
			
			nAB = nrow(res[(res[,nomTot]==2)&(res[,1]==1)&(res[,2]==1),])
			if(is.null(nAB)) nAB=1
			nAC = nrow(res[(res[,nomTot]==2)&(res[,1]==1)&(res[,3]==1),])
			if(is.null(nAC)) nAC=1
			nBC = nrow(res[(res[,nomTot]==2)&(res[,2]==1)&(res[,3]==1),])
			if(is.null(nBC)) nBC=1
			
			nABC = nrow(res[res[,nomTot]==3,])
			if(is.null(nABC)) nABC=1
			
			listeA = colnames(res)[1]
			listeB = colnames(res)[2]
			listeC = colnames(res)[3]
			
			if(ud)
			{
				nAud = data_r[(data_r[,nomTot]==1)&(data_r[,1]==1),5]
				nAu = length(nAud[nAud>1])
				nAd = length(nAud[nAud<1])
				nBud = data_r[(data_r[,nomTot]==1)&(data_r[,2]==1),6]
				nBu = length(nBud[nBud>1])
				nBd = length(nBud[nBud<1])
				nCud = data_r[(data_r[,nomTot]==1)&(data_r[,3]==1),7]
				nCu = length(nCud[nCud>1])
				nCd = length(nCud[nCud<1])
				
				nABud = data_r[(data_r[,nomTot]==2)&(data_r[,1]==1)&(data_r[,2]==1),5:6]
				nABud = compte(nABud)
				nACud = data_r[(data_r[,nomTot]==2)&(data_r[,1]==1)&(data_r[,3]==1),c(5,7)]
				nACud = compte(nACud)
				nBCud = data_r[(data_r[,nomTot]==2)&(data_r[,2]==1)&(data_r[,3]==1),6:7]
				nBCud = compte(nBCud)
				
				nABCud = data_r[data_r[,nomTot]==3,5:7]
				nABCud = compte(nABCud)
				
				graph_3(path, listeA, listeB, listeC, nA, nB, nC, nAB, nAC, nBC, nABC, tot_ugenes, noms, ud, nAu, nAd, nBu, nBd, nCu, nCd, nABud, nACud, nBCud, nABCud, couleurs, couleursIn=couleursIn, Solid=Solid)
			}else{
				graph_3(path, listeA, listeB, listeC, nA, nB, nC, nAB, nAC, nBC, nABC, tot_ugenes, noms, ud, couleurs=couleurs, couleursIn=couleursIn, Solid=Solid)
			}
			if(prop)
			{ 
				if(max(nA, nB, nC, nAB, nAC, nBC, nABC)/min(nA, nB, nC, nAB, nAC, nBC, nABC)>10) # Si le ratio max/min>10 => log2
				{
					graph_prop_3(path, res[,1:grep("total", colnames(res), ignore.case=TRUE)], nA, nB, nC, nAB, nAC, nBC, nABC, tot_ugenes, noms, couleurs=couleurs, tlog=TRUE, colBlack=colBlack)
					graph_prop_3(path, res[,1:grep("total", colnames(res), ignore.case=TRUE)], nA, nB, nC, nAB, nAC, nBC, nABC, tot_ugenes, noms, couleurs=couleurs, tlog=FALSE, colBlack=colBlack)
				}else{
					graph_prop_3(path, res[,1:grep("total", colnames(res), ignore.case=TRUE)], nA, nB, nC, nAB, nAC, nBC, nABC, tot_ugenes, noms, couleurs=couleurs, tlog=FALSE, colBlack=colBlack)
				}
			}
		}
		
		if(grep("total", colnames(res), ignore.case=TRUE)==5)
		{
			nA = nrow(res[(res[,nomTot]==1)&(res[,1]==1),])
			if(is.null(nA)) nA=1
			nB = nrow(res[(res[,nomTot]==1)&(res[,2]==1),])
			if(is.null(nB)) nB=1
			nC = nrow(res[(res[,nomTot]==1)&(res[,3]==1),])
			if(is.null(nC)) nC=1
			nD = nrow(res[(res[,nomTot]==1)&(res[,4]==1),])
			if(is.null(nD)) nD=1
			
			nAB = nrow(res[(res[,nomTot]==2)&(res[,1]==1)&(res[,2]==1),])
			if(is.null(nAB)) nAB=1
			nAC = nrow(res[(res[,nomTot]==2)&(res[,1]==1)&(res[,3]==1),])
			if(is.null(nAC)) nAC=1
			nBD = nrow(res[(res[,nomTot]==2)&(res[,2]==1)&(res[,4]==1),])
			if(is.null(nBD)) nBD=1
			nCD = nrow(res[(res[,nomTot]==2)&(res[,3]==1)&(res[,4]==1),])
			if(is.null(nCD)) nCD=1
			nAD = nrow(res[(res[,nomTot]==2)&(res[,1]==1)&(res[,4]==1),])
			if(is.null(nAD)) nAD=1
			nBC = nrow(res[(res[,nomTot]==2)&(res[,2]==1)&(res[,3]==1),])
			if(is.null(nBC)) nBC=1
			
			nABC = nrow(res[(res[,nomTot]==3)&(res[,1]==1)&(res[,2]==1)&(res[,3]==1),])
			if(is.null(nABC)) nABC=1
			nBCD = nrow(res[(res[,nomTot]==3)&(res[,2]==1)&(res[,3]==1)&(res[,4]==1),])
			if(is.null(nBCD)) nBCD=1
			nACD = nrow(res[(res[,nomTot]==3)&(res[,1]==1)&(res[,3]==1)&(res[,4]==1),])
			if(is.null(nACD)) nACD=1
			nABD = nrow(res[(res[,nomTot]==3)&(res[,1]==1)&(res[,2]==1)&(res[,4]==1),])
			if(is.null(nABD)) nABD=1
			
			nABCD = nrow(res[res[,nomTot]==4,])
			if(is.null(nABCD)) nABCD=1
			
			listeA = colnames(res)[1]
			listeB = colnames(res)[2]
			listeC = colnames(res)[3]
			listeD = colnames(res)[4]
			
			if(ud)
			{
				nAud = as.numeric(data_r[(data_r[,nomTot]==1)&(data_r[,1]==1),6])
				nAu = length(nAud[nAud>1])
				nAd = length(nAud[nAud<1])
				nBud = as.numeric(data_r[(data_r[,nomTot]==1)&(data_r[,2]==1),7])
				nBu = length(nBud[nBud>1])
				nBd = length(nBud[nBud<1])
				nCud = as.numeric(data_r[(data_r[,nomTot]==1)&(data_r[,3]==1),8])
				nCu = length(nCud[nCud>1])
				nCd = length(nCud[nCud<1])
				nDud = as.numeric(data_r[(data_r[,nomTot]==1)&(data_r[,4]==1),9])
				nDu = length(nDud[nDud>1])
				nDd = length(nDud[nDud<1])
				
				nABud = data_r[(data_r[,nomTot]==2)&(data_r[,1]==1)&(data_r[,2]==1),6:7]
				nABud = compte(nABud)
				nACud = data_r[(data_r[,nomTot]==2)&(data_r[,1]==1)&(data_r[,3]==1),c(6,8)]
				nACud = compte(nACud)
				nBCud = data_r[(data_r[,nomTot]==2)&(data_r[,2]==1)&(data_r[,3]==1),7:8]
				nBCud = compte(nBCud)
				nBDud = data_r[(data_r[,nomTot]==2)&(data_r[,2]==1)&(data_r[,4]==1),c(7,9)]
				nBDud = compte(nBDud)
				nCDud = data_r[(data_r[,nomTot]==2)&(data_r[,3]==1)&(data_r[,4]==1),8:9]
				nCDud = compte(nCDud)
				nADud = data_r[(data_r[,nomTot]==2)&(data_r[,1]==1)&(data_r[,4]==1),c(6,9)]
				nADud = compte(nADud)
				
				nABCud = data_r[(data_r[,nomTot]==3)&(data_r[,1]==1)&(data_r[,2]==1)&(data_r[,3]==1),6:8]
				nABCud = compte(nABCud)
				nBCDud = data_r[(data_r[,nomTot]==3)&(data_r[,2]==1)&(data_r[,3]==1)&(data_r[,4]==1),7:9]
				nBCDud = compte(nBCDud)
				nACDud = data_r[(data_r[,nomTot]==3)&(data_r[,1]==1)&(data_r[,3]==1)&(data_r[,4]==1),c(6,8,9)]
				nACDud = compte(nACDud)
				nABDud = data_r[(data_r[,nomTot]==3)&(data_r[,1]==1)&(data_r[,2]==1)&(data_r[,4]==1),c(6,7,9)]
				nABDud = compte(nABDud)
				
				nABCDud = data_r[data_r[,nomTot]==4,6:9]
				nABCDud = compte(nABCDud)
				
				graph_4(path, listeA, listeB, listeC, listeD, nA, nB, nC, nD, nAB, nAC, nBD, nCD, nAD, nBC, nABC, nBCD, nACD, nABD, nABCD, tot_ugenes, noms, ud, nAu, nAd, nBu, nBd, nCu, nCd, nDu, nDd, nABud, nACud, nBCud, nBDud, nCDud, nADud, nABCud, nBCDud, nABDud, nACDud, nABCDud, couleurs, couleursIn=couleursIn, Solid=Solid)
			}else{
				graph_4(path, listeA, listeB, listeC, listeD, nA, nB, nC, nD, nAB, nAC, nBD, nCD, nAD, nBC, nABC, nBCD, nACD, nABD, nABCD, tot_ugenes, noms, ud, couleurs=couleurs, couleursIn=couleursIn, Solid=Solid)
			}
			if(prop) 
			{
				if(max(nA, nB, nC, nD, nAB, nAC, nBD, nCD, nAD, nBC, nABC, nBCD, nACD, nABD, nABCD)/min(nA, nB, nC, nD, nAB, nAC, nBD, nCD, nAD, nBC, nABC, nBCD, nACD, nABD, nABCD)>10) # Si le ratio max/min>10 => log2
				{
					graph_prop_4(path, res[,1:grep("total", colnames(res), ignore.case=TRUE)], nA, nB, nC, nD, nAB, nAC, nBD, nCD, nAD, nBC, nABC, nBCD, nACD, nABD, nABCD, tot_ugenes, noms, couleurs=couleurs, tlog=TRUE, colBlack=colBlack)
					graph_prop_4(path, res[,1:grep("total", colnames(res), ignore.case=TRUE)], nA, nB, nC, nD, nAB, nAC, nBD, nCD, nAD, nBC, nABC, nBCD, nACD, nABD, nABCD, tot_ugenes, noms, couleurs=couleurs, tlog=FALSE, colBlack=colBlack)
				}else{
					graph_prop_4(path, res[,1:grep("total", colnames(res), ignore.case=TRUE)], nA, nB, nC, nD, nAB, nAC, nBD, nCD, nAD, nBC, nABC, nBCD, nACD, nABD, nABCD, tot_ugenes, noms, couleurs=couleurs, tlog=FALSE, colBlack=colBlack)
				}
			}
		}
		if(overlaps)  overlapp(res, path, f)
	}else{
		if(!annot)  return(res)
		if(annot)  return(data_all)
	}
	
	if(ud)
	{
		ratios_all = data_all[,grep("atio", colnames(data_all))[grep("atio", colnames(data_all))>grep(nomTot, colnames(data_all))]] # Recupere les ratios                
		ratios_all = matrix(as.numeric(ratios_all), nrow=nrow(res), ncol=nliste)    # Transforme les ratios en numeric et NA
		# colnames(ratios_all) = colnames(data_all)[grep("atio", colnames(data_all))[grep("atio", colnames(data_all))>grep(nomTot, colnames(data_all))]]
		resTmp = cbind(res[,1:(nliste+1)], ratios_all)                              # Ajoute la matrice binaire + ratios
	}else{
		resTmp = res
	}
	# Si plus de 4 listes => VennBar=TRUE
	if(VennBar)  BarVenn(resTmp=resTmp, path=path, ud=ud)
	
	if(OnlyVariable) return(data_all)
}
