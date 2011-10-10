#annot=TRUE; path_res=""; path_lists=""; res=""; ud=TRUE; prop=FALSE; noms=""; overlaps=FALSE; f=0; Tk=FALSE; aa=FALSE

evenn <-
function(annot=FALSE, path_res="", path_lists="", res="", ud=FALSE, prop=FALSE, noms="", overlaps=FALSE, f=0, Tk=FALSE, aa=FALSE)
{  
  if(aa){
  write("        ,.-.,                                                                                    ", file="")
  write("      .`     `.                                                                                  ", file="")
  write("     /.-., ,.-.`            *       *                                 ****      **       *****   ", file="")
  write("   .`    .`.    `.     ***   *     *   ***    ****   ****    *     * *    *   *  *      *     *  ", file="")
  write("  / `.  /   `.  / `  *     *  *   *  *     * *    * *    *    *   *      *       *           *   ", file="")
  write(" |    ',_____,'    | ******   *   *  ******  *    * *    *     * *      *        *           *   ", file="")
  write(" `.     `   /     /  *         * *   *       *    * *    *     * *    *          *      *     *  ", file="")
  write("   ',    '_'    ,'    *****     *     *****  *    * *    *      *    ****** * ******* *  *****   ", file="")
  write("     `'-'` `'-'`                                                                                 ", file="")
  write("\n\t[Run man.evenn() for quick help]\n", file="")
  }else{
    write("\n\teVenn v2.1.3 (2011-10-10)\n", file="")
    write("\t[Run man.evenn() for quick help]\n", file="")
  }
  flush.console()                                         
  options(warn=-1)
  if((Sys.info()["sysname"]!="Windows")&(path_lists==""))  require(tcltk)
  
  ########################################################################################################
  ########################################################################################################
  #
  # Fonctions
  
  cmbn<-function(x, n)
  {
    res = matrix(0, ncol=0, nrow=n)
    for(I in 1:(length(x)-1))
    {
      for(J in (I+1):length(x))
      {
         res = cbind(res, matrix(0, ncol=1, nrow=n))
         res[1,ncol(res)] = x[I]
         res[2,ncol(res)] = x[J]
      }
    }
    return(res)
  }
  
  overlapp<-function(res, path, f)
  {
    write("Computing overlaps ", file="")
    flush.console()
    
    overlapp_table = matrix(1, ncol=(ncol(res)-1), nrow=(ncol(res)-1))
    rownames(overlapp_table) = colnames(res)[1:(ncol(res)-1)]
    colnames(overlapp_table) = colnames(res)[1:(ncol(res)-1)]
    comps = cmbn(x=colnames(overlapp_table), 2)
    overlapp_table_n = overlapp_table
    
    if((ncol(res)-1)>2)
    {
      for(K in 1:ncol(comps))
      {
         tmp = res[,c(comps[,K])] #extrait les deux colonnes en question
         tmp = cbind(tmp, (tmp[,1] + tmp[,2]))
         comm = nrow(tmp[tmp[,3]==2,])
         if(!is.null(comm))
         {
            overlapp_table_n[comps[1,K],comps[2,K]] = comm
            overlapp_table_n[comps[2,K],comps[1,K]] = comm
            overlapp_table[comps[1,K],comps[2,K]] = comm / sum(tmp[,1])
            overlapp_table[comps[2,K],comps[1,K]] = comm / sum(tmp[,2])
         }else{
            overlapp_table_n[comps[1,K],comps[2,K]] = 0
            overlapp_table_n[comps[2,K],comps[1,K]] = 0
            overlapp_table[comps[1,K],comps[2,K]] = 0
            overlapp_table[comps[2,K],comps[1,K]] = 0
         }
         write(paste(K, " / ", ncol(comps), sep=""), file="")
         flush.console()
      }

      #ajoute des nbres avant les noms
      rownames(overlapp_table) = paste(seq(1, ncol(overlapp_table), by=1), colnames(overlapp_table))
      colnames(overlapp_table) = seq(1, ncol(overlapp_table), by=1)
      rownames(overlapp_table_n) = rownames(overlapp_table)
      colnames(overlapp_table_n) = colnames(overlapp_table)
      png(filename = paste(path, "/HeatOverlaps.png", sep=""), width=(1000+10*nrow(overlapp_table)), height=(500+5*nrow(overlapp_table)), units = "px", pointsize = 10, bg = "white")
      heatmap(overlapp_table)
      dev.off()
    }else{
       comm = nrow(res[res[,3]==2,])
       overlapp_table[comps[1],comps[2]] = comm / sum(res[,1])
       overlapp_table[comps[2],comps[1]] = comm / sum(res[,2])
    }
    write.csv2(overlapp_table, row.names = TRUE, file = paste(path, "/overlaps_table.csv", sep=""))
    write.csv2(overlapp_table_n, row.names = TRUE, file = paste(path, "/overlaps_table_n.csv", sep=""))
    
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
      write.csv2(overlapp_table, row.names = TRUE, file = paste(path, "/overlaps_table_f(", f, ").csv", sep=""))
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
      if(ncol(t)==2)  profils = apply(t, 1, function(x) paste(x[1], x[2], sep="")) #matrice des profils
      if(ncol(t)==3)  profils = apply(t, 1, function(x) paste(x[1], x[2], x[3], sep="")) #matrice des profils
      if(ncol(t)==4)  profils = apply(t, 1, function(x) paste(x[1], x[2], x[3], x[4], sep="")) #matrice des profils
    }
    if((class(t)=="character"))
    {
      if(length(t)==2)  profils = paste(t[1], t[2], sep="") #matrice des profils
      if(length(t)==3)  profils = paste(t[1], t[2], t[3], sep="") #matrice des profils
      if(length(t)==4)  profils = paste(t[1], t[2], t[3], t[4], sep="") #matrice des profils
    }
    return(table(profils))
  }
  
  format_label<-function(n, m, nom, x, y, t, type=0, noms, dv)
  {
     dh = 0.2*(t/0.5) #decalage horizontal
     d=4
     if(dim(m)!=0)
     {
       m = as.matrix(m) #details effectifs/groupes
       text(x=x, y=(y+0.05), labels=paste(n, sep=""), cex=t, col="black"); x=x-0.3  #ref de localisation: total
       nl = length(strsplit(nom, ",")[[1]])
       x = x-dh

       for(I in 1:nrow(m))
       {
          dec=0
          for(J in 1:nl)
          {
            if(strsplit(nom, ",")[[1]][J] == noms[1])  couleur = "blue"
            if(strsplit(nom, ",")[[1]][J] == noms[2])  couleur = "red"
            if(strsplit(nom, ",")[[1]][J] == noms[3])  couleur = "green"
            if(strsplit(nom, ",")[[1]][J] == noms[4])  couleur = "orange"
            text(x=x+dec, y=(y-I*dv), labels=paste(substr(rownames(m)[I],J,J), sep=""), cex=t, col=couleur)
            dec = dec + dh*nchar(substr(rownames(m)[I],J,J))
          }
          text(x=x+dec+(t/d)*((nchar(paste(": ", m[I, 1], sep=""))/2)/2), y=(y-I*dv), labels=paste(": ", m[I, 1], sep=""), cex=t, col="black")
        }
     }else{
        text(x=(x+0.1), y=(y-0.4), labels=paste(0, sep=""), cex=t, col="black")
     }
  }
  
  #test des formats des listes
  test_list<-function(liste, ud, type)
  {
    ext = substr(basename(liste), (nchar(basename(liste))-2), nchar(basename(liste)))
    data_t=""
    if(exists("data_t"))  rm("data_t")
    if(ext == "txt") data_t = read.table(file=liste, header=TRUE, sep="\t")
    if(ext == "tsv") data_t = read.table(file=liste, header=TRUE, sep="\t")
    if(ext == "csv") data_t = try(read.table(file=liste, header=TRUE, sep=","), silent=TRUE)
    if((ext == "csv")&(class(data_t)=="try-error")) data_t = try(read.table(file=liste, header=TRUE, sep=";"), silent=TRUE)
    if((ext!="csv")&(ext!="txt")) 
    {
      write("The file format is not supported (must be txt/tab or csv/,;)", file="")
      flush.console()
      break;
    }
    if(ncol(data_t)<3) #juste les ID
    {
        rownames(data_t) = data_t[,1] #ID = 1ere colonne
    }         
    if(ncol(data_t)>=3) #ID et une seule colonne
    {
        rownames(data_t) = data_t[,1] #ID = 1ere colonne
        data_t = data_t[,2:ncol(data_t)]
    }
    data_t = as.matrix(data_t)
    
    if(ud & (type == "Res"))  
    {
      #test la presence de la colonne ratios
      if(length(colnames(data_t)[colnames(data_t)=="ratios"])==0)  write(paste("\n\t !!!  The list ", substr(basename(liste), 0, (nchar(basename(liste))-4)), " do not have \"ratios\" column.\n\t !!! Its modulations will not be analyzed.\n", sep=""), file="")
      #teste le type de data
      if(length(colnames(data_t)[colnames(data_t)=="ratios"])!=0)
      {
         if(is.na(as.numeric(data_t[2,colnames(data_t)=="ratios"]))) write(paste("\n\t !!!  The \"ratios\" column of the ", substr(basename(liste), 0, (nchar(basename(liste))-4)), " list is not numeric.\n\t !!! Its modulations will not be analyzed.\n", sep=""), file="")
      }
      flush.console()
    }
    return(data_t)
  }
  
  cercle<-function(x, y, rayon, int=NA, out, lty, lwd)
  {
    xylim <- par("usr")
    plotdim <- par("pin")
    ymult <- (xylim[4] - xylim[3])/(xylim[2] - xylim[1]) * plotdim[1]/plotdim[2]
    angle.inc <- 2 * pi/2000
    angles <- seq(0, 2 * pi - angle.inc, by = angle.inc)
  
    for (I in 1:length(x))
    {
      dens = NULL
      if(int[I]=="") dens=0
      xv <- cos(angles) * rayon[I] + x[I]
      yv <- sin(angles) * rayon[I] * ymult + y[I]
      polygon(xv, yv, border = out[I], col=int[I], lty, lwd, density=dens)
    }
  }
  
  graph_2<-function(path, listeA, listeB, nA, nB, nAB, tot_ugenes, noms, ud, nAu="", nAd="", nBu="", nBd="", nABud="")
  {
    dd=1; t=1.4
    png(filename = paste(path, "/venn_diagram.png", sep=""), width = 800*dd, height = 500*dd, pointsize = 15, bg = "white")
    plot.window(c(0, 25*dd), c(0, 20*dd))
    plot(x=1:(25*dd), y=1:(25*dd), type="n", axes=FALSE, xlab="",ylab="")

    xcercles = c(10*dd, 16*dd)
    ycercles = c(13*dd, 13*dd)
    rcercles = c(5*dd, 5*dd)
    cercle(xcercles, ycercles, rcercles, out=c("blue", "red"), int=c("", ""), lty=1, lwd=1)

    if(ud)
    {
      text(x=(8*dd), y=(15*dd), labels=nA, cex=t, col="blue")
      text(x=(8*dd), y=(13*dd), labels=paste("U:", nAu, sep=""), cex=t, col="blue")
      text(x=(8*dd), y=(11*dd), labels=paste("D:", nAd, sep=""), cex=t, col="blue")
      text(x=(18*dd), y=(15*dd), labels=nB, cex=t, col="red")
      text(x=(18*dd), y=(13*dd), labels=paste("U:", nBu, sep=""), cex=t, col="red")
      text(x=(18*dd), y=(11*dd), labels=paste("D:", nBd, sep=""), cex=t, col="red")
  
      format_label(n=nAB, m=nABud, nom=paste(noms[1], noms[2], sep=","), x=(13*dd), y=(14*dd), t, type=2, noms, dv=t)
    }else{
      text(x=(8*dd), y=(13*dd), labels=nA, cex=t, col="blue")
      text(x=(18*dd), y=(13*dd), labels=nB, cex=t, col="red")
      text(x=(13*dd), y=(13*dd), labels=nAB, cex=t, col="black")  
      text(x=(2.5*dd), y=(15*dd), labels=paste("Total unique\ngenes: ", tot_ugenes, sep=""), cex=t, col="black")
    }
    #titres
    text(x=(13*dd), y=(1*dd), labels=listeA, cex=t, col="blue")
    text(x=(13*dd), y=(25*dd), labels=listeB, cex=t, col="red")
    dev.off()
  }
  
  graph_3<-function(path, listeA, listeB, listeC, nA, nB, nC, nAB, nAC, nBC, nABC, tot_ugenes, noms, ud, nAu="", nAd="", nBu="", nBd="", nCu="", nCd="", nABud="", nACud="", nBCud="", nABCud="")
  {
    dd=1; t=1.2
    png(filename = paste(path, "/venn_diagram.png", sep=""), width = 800*dd, height = 800*dd, pointsize = 15, bg = "white")
    plot.window(c(0, 25*dd), c(0, 25*dd))
    plot(x=1:(25*dd), y=1:(25*dd), type="n", axes=FALSE, xlab="",ylab="")

    xcercles = c(12*dd, 9*dd, 15*dd)
    ycercles = c(13*dd, 7*dd, 7*dd)
    rcercles = c(6*dd, 6*dd, 6*dd)
    cercle(xcercles, ycercles, rcercles, out=c("blue", "red", "green"), int=c("", "", ""), lty=1, lwd=1)

    if(ud)
    {
      text(x=(12*dd), y=(17*dd), labels=nA, cex=t, col="black")
      text(x=(12*dd), y=(16*dd), labels=paste("U:", nAu), cex=t, col="blue")
      text(x=(12*dd), y=(15*dd), labels=paste("D:", nAd), cex=t, col="blue")      
      text(x=(7*dd), y=(7*dd), labels=nB, cex=t, col="black")
      text(x=(7*dd), y=(6*dd), labels=paste("U:", nBu), cex=t, col="red")
      text(x=(7*dd), y=(5*dd), labels=paste("D:", nBd), cex=t, col="red")      
      text(x=(17*dd), y=(7*dd), labels=nC, cex=t, col="black")
      text(x=(17*dd), y=(6*dd), labels=paste("U:", nCu), cex=t, col="green")
      text(x=(17*dd), y=(5*dd), labels=paste("D:", nCd), cex=t, col="green")
      
      format_label(n=nAB, m=nABud, nom=paste(noms[1], noms[2], sep=","), x=(8*dd), y=(13*dd), t, type=3, noms, dv=(t/2))
      format_label(n=nAC, m=nACud, nom=paste(noms[1], noms[3], sep=","), x=(15.5*dd), y=(13*dd), t, type=3, noms, dv=(t/2))
      format_label(n=nBC, m=nBCud, nom=paste(noms[2], noms[3], sep=","), x=(12*dd), y=(6*dd), t, type=3, noms, dv=(t/2))  
      format_label(n=nABC, m=nABCud, nom=paste(noms[1], noms[2], noms[3], sep=","), x=(12*dd), y=(12*dd), t, type=3, noms, dv=(t/2))
    }else{
      text(x=(12*dd), y=(16*dd), labels=nA, cex=t, col="black")
      text(x=(7*dd), y=(6*dd), labels=nB, cex=t, col="black")
      text(x=(17*dd), y=(6*dd), labels=nC, cex=t, col="black")  
      text(x=(8.5*dd), y=(11*dd), labels=nAB, cex=t, col="black")
      text(x=(15.5*dd), y=(11*dd), labels=nAC, cex=t, col="black")
      text(x=(12*dd), y=(4*dd), labels=nBC, cex=t, col="black")  
      text(x=(12*dd), y=(9*dd), labels=nABC, cex=t, col="black")  
      text(x=(22*dd), y=(15*dd), labels=paste("Total unique\ngenes: ", tot_ugenes, sep=""), cex=t, col="black")
    }
    text(x=(22*dd), y=(15*dd), labels=paste("Total unique\ngenes: ", tot_ugenes, sep=""), cex=t, col="black")

    #titres
    text(x=(12*dd), y=(23*dd), labels=listeA, cex=t, col="blue")
    text(x=(12*dd), y=(22*dd), labels=listeB, cex=t, col="red")
    text(x=(12*dd), y=(21*dd), labels=listeC, cex=t, col="green")
    dev.off()
  }
  
  graph_4<-function(path, listeA, listeB, listeC, listeD, nA, nB, nC, nD, nAB, nAC, nBD, nCD, nAD, nBC, nABC, nBCD, nACD, nABD, nABCD, tot_ugenes, noms, ud, nAu="", nAd="", nBu="", nBd="", nCu="", nCd="", nDu="", nDd="", nABud="", nACud="", nBCud="", nBDud="", nCDud="", nADud="", nABCud="", nBCDud="", nABDud="", nACDud="", nABCDud="")
  {
    dd=1.3; t=1.1
    png(filename = paste(path, "/venn_diagram.png", sep=""), width = 800*dd, height = 800*dd, pointsize = 15, bg = "white")
    plot.window(c(0, 25*dd), c(0, 25*dd))
    plot(x=1:(25*dd), y=1:(25*dd), type="n", axes=FALSE, xlab="",ylab="")
    
    xcercles = c(9*dd, 12*dd, 6*dd, 9*dd, 22*dd, 22*dd, 8.5*dd, 9.5*dd)
    ycercles = c(12*dd, 9*dd, 9*dd, 6*dd, 9.5*dd, 8.5*dd, 22*dd, 22*dd)
    rcercles = c(5*dd, 5*dd, 5*dd, 5*dd, 3*dd, 3*dd, 3*dd, 3*dd)
    cercle(xcercles, ycercles, rcercles, out=c("blue", "red", "green", "orange", "blue", "orange", "red", "green"), int=c("", "", "", "", "", "", "", ""), lty=1, lwd=1)

    if(ud)
    {
      text(x=(9*dd), y=(15.5*dd), labels=paste(noms[1], "\n", "U:", nAu, "\nD:", nAd, sep=""), cex=t, col="blue")
      text(x=(3*dd), y=(9*dd), labels=paste(noms[2], "\nU:", nBu, "\nD:", nBd, sep=""), cex=t, col="red")
      text(x=(15*dd), y=(9*dd), labels=paste(noms[3], "\nU:", nCu, "\nD:", nCd, sep=""), cex=t, col="green")
      text(x=(9*dd), y=(3*dd), labels=paste(noms[4], "\nU:", nDu, "\nD:", nDd, sep=""), cex=t, col="orange")
  
      format_label(n=nAB, m=nABud, nom=paste(noms[1], ",", noms[2], sep=""), x=(6*dd), y=(14*dd), t, type=4, noms, dv=t/2)
      format_label(n=nAC, m=nACud, nom=paste(noms[1], ",", noms[3], sep=""), x=(12*dd), y=(14*dd), t, type=4, noms, dv=t/2)
      format_label(n=nBD, m=nBDud, nom=paste(noms[2], ",", noms[4], sep=""), x=(6*dd), y=(7*dd), t, type=4, noms, dv=t/2)
      format_label(n=nCD, m=nCDud, nom=paste(noms[3], ",", noms[4], sep=""), x=(12*dd), y=(7*dd), t, type=4, noms, dv=t/2)
      format_label(n=nAD, m=nADud, nom=paste(noms[1], ",", noms[4], sep=""), x=(22*dd), y=(10*dd), t, type=4, noms, dv=t/2)
      format_label(n=nBC, m=nBCud, nom=paste(noms[2], ",", noms[3], sep=""), x=(9*dd), y=(23*dd), t, type=4, noms, dv=t/2)
      format_label(n=nABC, m=nABCud, nom=paste(noms[1], ",", noms[2], ",", noms[3], sep=""), x=(9*dd), y=(13*dd), t, type=4, noms, dv=t/2)
      format_label(n=nBCD, m=nBCDud, nom=paste(noms[2], ",", noms[3], ",", noms[4], sep=""), x=(9*dd), y=(6.5*dd), t, type=4, noms, dv=t/2)
      format_label(n=nABD, m=nABDud, nom=paste(noms[1], ",", noms[2], ",", noms[4], sep=""), x=(6.2*dd), y=(10*dd), t, type=4, noms, dv=t/2)
      format_label(n=nACD, m=nACDud, nom=paste(noms[1], ",", noms[3], ",", noms[4], sep=""), x=(11.8*dd), y=(10*dd), t, type=4, noms, dv=t/2)
      format_label(n=nABCD, m=nABCDud, nom=paste(noms[1], ",", noms[2], ",", noms[3], ",", noms[4], sep=""), x=(9*dd), y=(11*dd), t, type=4, noms, dv=t/2)
    }else{
      text(x=(9*dd), y=(16*dd), labels=nA, cex=t, col="blue")
      text(x=(3*dd), y=(9*dd), labels=nB, cex=t, col="red")
      text(x=(15*dd), y=(9*dd), labels=nC, cex=t, col="green")
      text(x=(9*dd), y=(3*dd), labels=nD, cex=t, col="orange")  
      text(x=(6*dd), y=(12*dd), labels=nAB, cex=t, col="black")
      text(x=(12*dd), y=(12*dd), labels=nAC, cex=t, col="black")
      text(x=(6*dd), y=(6*dd), labels=nBD, cex=t, col="black")
      text(x=(12*dd), y=(6*dd), labels=nCD, cex=t, col="black")  
      text(x=(22*dd), y=(9*dd), labels=nAD, cex=t, col="black")
      text(x=(9*dd), y=(22*dd), labels=nBC, cex=t, col="black")
      text(x=(9*dd), y=(12*dd), labels=nABC, cex=t, col="black")  
      text(x=(9*dd), y=(6*dd), labels=nBCD, cex=t, col="black")
      text(x=(6.2*dd), y=(9*dd), labels=nABD, cex=t, col="black")
      text(x=(11.8*dd), y=(9*dd), labels=nACD, cex=t, col="black")
      text(x=(9*dd), y=(9*dd), labels=nABCD, cex=t, col="black")
    }
    text(x=(18*dd), y=(18*dd), labels=paste("Total unique\ngenes: ", tot_ugenes, sep=""), cex=t, col="black")

    #titres
    text(x=(18*dd), y=(24*dd), labels=listeA, cex=t, col="blue")
    text(x=(18*dd), y=(23*dd), labels=listeB, cex=t, col="red")
    text(x=(18*dd), y=(22*dd), labels=listeC, cex=t, col="green")
    text(x=(18*dd), y=(21*dd), labels=listeD, cex=t, col="orange")

    dev.off()
  }
  
  graph_prop_2<-function(path, res, nA, nB, nAB, tot_ugenes, noms)
  {
    n = c(NULL)
    for(N in 1:(ncol(res)-1))
    {
      xtmp = unlist(strsplit(colnames(res)[N], "_"))[length(unlist(strsplit(colnames(res)[N], "_")))]
      n = c(n, as.numeric(substr(xtmp, 2, (nchar(xtmp)-1))))
    }
    
    #calculs des rayons pour que la surface des cercles reflete la taille des listes
    rAtot = sqrt(n[1]/pi)
    rBtot = sqrt(n[2]/pi)
  
    rA = sqrt(nA/pi)
    rB = sqrt(nB/pi)
    rAB = sqrt(nAB/pi) 
    
    # coordonnees des centres
    xAtot = rAtot
    xBtot = (2*max(rAtot, rBtot) + 2*rAB + rBtot)*1.2
    xAB = (xAtot+xBtot)/2
    xA = rA
    xB = xBtot + rBtot - rB
    
    yAtot = max(rAtot, rBtot)
    yBtot = max(rAtot, rBtot)
    yAB = max(rAtot, rBtot)
    yA = max(rAtot, rBtot)
    yB = max(rAtot, rBtot)
    
    xCircles = c(xAtot, xBtot, xAB)
    xSpeCircles = c(xA, xB)
    yCircles = c(yAtot, yBtot, yAB)
    ySpeCircles = c(yA, yB)
    rCircles = c(rAtot, rBtot, rAB)
    rSpeCircles = c(rA, rB)
    colorCircles = c("blue", "red", "black")
    colorSpeCircles = c("blue", "red")
    
    xmin = 0
    xmax = (xBtot + rBtot)
    ymin = 0
    ymax = 2*max(rAtot, rBtot)
    
    yCircles = 1.5*yCircles
    ySpeCircles = 1.5*ySpeCircles
    
    dd=1; t=1.5
    png(filename = paste(path, "/venn_diagram_prop.png", sep=""), width = 800*dd, height = 600*dd, pointsize = 15, bg = "white")
    plot.window(c(0, (max(xmax, ymax)*1.1)*dd), c(0, (max(xmax, ymax)*1.1)*dd))
    plot(x=1:((max(xmax, ymax)*1.1)*dd), y=1:((max(xmax, ymax)*1.1)*dd), type="n", axes=FALSE, xlab="",ylab="")
    segments(x0=c(xAtot, xBtot), 
             y0=c(yAtot, yBtot), 
             x1=c(xAB, xAB), 
             y1=c(yAB, yAB), 
             col=c("blue", "red"))
    symbols(x=xCircles, y=yCircles, circles=rCircles, main = "PropCircles", fg=colorCircles, bg="white", add=TRUE, inches=FALSE)
    symbols(x=xSpeCircles, y=ySpeCircles, circles=rSpeCircles, main = "PropCircles", fg=colorSpeCircles, bg=colorSpeCircles, add=TRUE, inches=FALSE)
    
    cercle(xCircles, yCircles, rCircles, out=colorCircles, int=rep("", length(rCircles)),lty=1,lwd=1)
    
    #titres
    taille = 12
    text(x=xAtot, y=(yAtot-rAtot-0.2*taille), labels=paste(n[1]-nA), ps=taille, col="blue", font=1)
    text(x=xAtot, y=(yAtot-rAtot-0.1*taille), labels=paste(nA), ps=taille, col="blue", font=2)
    
    text(x=xBtot, y=(yBtot-rBtot-0.2*taille), labels=paste(n[2]-nB), ps=taille, col="red", font=1)
    text(x=xBtot, y=(yBtot-rBtot-0.1*taille), labels=paste(nB), ps=taille, col="red", font=2)
    
    text(x=xAB, y=yAB, labels=paste(nAB), ps=12, col="black", font=1)
    
    #titres
    ex=2
    text(x=xAB, y=(yCircles+0.2*taille), labels=colnames(res)[1], ps=(taille*ex), col="blue", font=2)
    text(x=xAB, y=(yCircles+0.1*taille), labels=colnames(res)[2], ps=(taille*ex), col="red", font=2)
    
    dev.off()
  }
  
  graph_prop_3<-function(path, res, nA, nB, nC, nAB, nAC, nBC, nABC, tot_ugenes, noms)
  {  
    n = c(NULL)
    for(N in 1:(ncol(res)-1))
    {
      xtmp = unlist(strsplit(colnames(res)[N], "_"))[length(unlist(strsplit(colnames(res)[N], "_")))]
      n = c(n, as.numeric(substr(xtmp, 2, (nchar(xtmp)-1))))
    }
    
    #calculs des rayons pour que la surface des cercles reflete la taille des listes
    rAtot = sqrt(n[1]/pi)
    rBtot = sqrt(n[2]/pi)
    rCtot = sqrt(n[3]/pi)
    rA = sqrt(nA/pi)
    rB = sqrt(nB/pi)
    rC = sqrt(nC/pi)
    rAB = sqrt(nAB/pi)
    rBC = sqrt(nBC/pi)
    rAC = sqrt(nAC/pi)
    rABC = sqrt(nABC/pi)
    
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
    rCircles = c(rAtot, rBtot, rCtot, rAB, rAC, rBC, rABC)
    rSpeCircles = c(rA, rB, rC)
    colorCircles = c("blue", "red", "green", "black", "black", "black", "black")
    colorSpeCircles = c("blue", "red", "green")
    
    pdf(file = paste(path, "/venn_diagram_prop.pdf", sep=""), width=10, height=10)
    plot.new()
    xmin = -max(rBtot, rCtot)
    xmax = (xCtot + rCtot)
    ymin = 0
    ymax = (yAtot + rAtot)*1.2
    plot.window(c(xmin, xmax), c(ymin, ymax), asp=1)
    segments(x0=c(xAtot, xBtot, xAtot, xCtot, xBtot, xCtot, xAtot, xBtot, xCtot), 
             y0=c(yAtot, yBtot, yAtot, yCtot, yBtot, yCtot, yAtot, yBtot, yCtot), 
             x1=c(xAB, xAB, xAC, xAC, xBC, xBC, xABC, xABC, xABC), 
             y1=c(yAB, yAB, yAC, yAC, yBC, yBC, yABC, yABC, yABC), 
             col=c("blue", "red", "blue", "green", "red", "green", "blue", "red", "green"))  
  #ajouter les 3 segments pour le cercle central  
    symbols(x=xCircles, y=yCircles, circles=rCircles, main = "PropCircles", fg=colorCircles, bg="white", add=TRUE, inches=FALSE)
    symbols(x=xSpeCircles, y=ySpeCircles, circles=rSpeCircles, main = "PropCircles", fg=colorSpeCircles, bg=colorSpeCircles, add=TRUE, inches=FALSE)
    
    taille = 12
    ex=2
    #effectifs
    text(x=xAtot, y=(yAtot+rAtot+0.1*taille), labels=paste(n[1]-nA), ps=taille, col="blue", font=1)
    text(x=xAtot, y=(yAtot+rAtot+0.2*taille), labels=paste(nA), ps=taille, col="blue", font=2)
    
    text(x=xBtot, y=(yBtot+rBtot+0.1*taille), labels=paste(n[2]-nB), ps=taille, col="red", font=1)
    text(x=xBtot, y=(yBtot+rBtot+0.2*taille), labels=paste(nB), ps=taille, col="red", font=2)
    
    text(x=xCtot, y=(yCtot+rCtot+0.1*taille), labels=paste(n[3]-nC), ps=taille, col="green", font=1)
    text(x=xCtot, y=(yCtot+rCtot+0.2*taille), labels=paste(nC), ps=taille, col="green", font=2)
    
    text(x=xAB, y=yAB, labels=paste(nAB), ps=12, col="black", font=1)
    text(x=xAC, y=yAC, labels=paste(nAC), ps=taille, col="black", font=1)
    text(x=xBC, y=yBC, labels=paste(nBC), ps=taille, col="black", font=1)
    text(x=xABC, y=yABC, labels=paste(nABC), ps=taille, col="black", font=1)
  
    text(x=0.1, y=(yAB+yAtot)/2, labels=paste("Total unique \ngenes: ", tot_ugenes, sep=""), ps=(taille*ex), col="black", font=2)
  
    #titres
    text(x=xAtot, y=ymax, labels=colnames(res)[1], ps=(taille*ex), col="blue", font=2)
    text(x=xAtot, y=(ymax-0.15*taille), labels=colnames(res)[2], ps=(taille*ex), col="red", font=2)
    text(x=xAtot, y=(ymax-0.30*taille), labels=colnames(res)[3], ps=(taille*ex), col="green", font=2)
    dev.off()
  }
 
  graph_prop_4<-function(path, res, nA, nB, nC, nD, nAB, nAC, nBD, nCD, nAD, nBC, nABC, nBCD, nACD, nABD, nABCD, tot_ugenes, noms)
  {  
    n = c(NULL)
    for(N in 1:(ncol(res)-1))
    {
      xtmp = unlist(strsplit(colnames(res)[N], "_"))[length(unlist(strsplit(colnames(res)[N], "_")))]
      n = c(n, as.numeric(substr(xtmp, 2, (nchar(xtmp)-1))))
    }
    
    #calculs des rayons pour que la surface des cercles reflete la taille des listes
    rAtot = sqrt(n[1]/pi)
    rBtot = sqrt(n[2]/pi)
    rCtot = sqrt(n[3]/pi)
    rDtot = sqrt(n[4]/pi)
    rA = sqrt(nA/pi)
    rB = sqrt(nB/pi)
    rC = sqrt(nC/pi)
    rD = sqrt(nD/pi)
    rAB = sqrt(nAB/pi)
    rBC = sqrt(nBC/pi)
    rAC = sqrt(nAC/pi)
    rAD = sqrt(nAD/pi)
    rBD = sqrt(nBD/pi)
    rCD = sqrt(nCD/pi)
    rABC = sqrt(nABC/pi)
    rABD = sqrt(nABD/pi)
    rBCD = sqrt(nBCD/pi)
    rACD = sqrt(nACD/pi)
    rABCD = sqrt(nABCD/pi) 
  
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
      colorCircles = c("blue", "red", "green", "orange", "black", "black", "black", "black", "black", "black", "black", "black", "black", "black", "black", "blue", "red", "green", "orange")
      data_graph=cbind(xCircles, yCircles, rCircles, colorCircles)
      colnames(data_graph) = c("xCircles", "yCircles", "rCircles", "colorCircles")
      rownames(data_graph) = c("Atot", "Btot", "Ctot", "Dtot", "AB", "AC", "AD", "BC", "BD", "CD", "ABC", "ABD", "ACD", "BCD", "ABCD", "A", "B", "C", "D")
      return(data_graph)
    }
    
    expx = 1.2
    expy = 1.2
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
    
    xmin = (min(as.numeric(data_graph[,"xCircles"])) - max(as.numeric(data_graph[,"rCircles"])))
    xmax = (max(as.numeric(data_graph[,"xCircles"])) + max(as.numeric(data_graph[,"rCircles"])))
    ymin = (min(as.numeric(data_graph[,"yCircles"])) - max(as.numeric(data_graph[,"rCircles"])))
    ymax = (max(as.numeric(data_graph[,"yCircles"])) + max(as.numeric(data_graph[,"rCircles"])))*1.2
    
    #si ajustement expx, expy
    d = adjust(data_graph)
    data_graph = calc_coord(rAtot, rBtot, rCtot, rDtot, rA, rB, rC, rD, rAB, rAC, rAD, rBC, rBD, rCD, rABC, rABD, rACD, rBCD, rABCD, expy=(1+(expy*d[2]/ymax)*1.2), expx=(1+(expx*(d[1]/xmax))*1.2))
  
    pdf(file = paste(path, "/venn_diagram_prop.pdf", sep=""), width=10, height=10)
    plot.new()  
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
    taille = 12
    ex=2
    #titres
    text(x=xmax/2, y=(ymax-0.15*taille), labels=colnames(res)[1], ps=(taille*ex), col="blue", font=2)
    text(x=xmax/2, y=(ymax-0.35*taille), labels=colnames(res)[2], ps=(taille*ex), col="red", font=2)
    text(x=xmax/2, y=(ymax-0.55*taille), labels=colnames(res)[3], ps=(taille*ex), col="green", font=2)
    text(x=xmax/2, y=(ymax-0.75*taille), labels=colnames(res)[4], ps=(taille*ex), col="orange", font=2)
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
    text(x=xAll["Atot",], y=(yAll["Atot",]+rAll["Atot",]+0.25*taille), labels=paste(n[1]-nA), ps=taille, col="blue", font=1)
    text(x=xAll["Atot",], y=(yAll["Atot",]+rAll["Atot",]+0.1*taille), labels=paste(nA), ps=taille, col="blue", font=2)  
    text(x=xAll["Btot",], y=(yAll["Btot",]+rAll["Btot",]+0.25*taille), labels=paste(n[2]-nB), ps=taille, col="red", font=1)
    text(x=xAll["Btot",], y=(yAll["Btot",]+rAll["Btot",]+0.1*taille), labels=paste(nB), ps=taille, col="red", font=2)
    text(x=xAll["Ctot",], y=(yAll["Ctot",]-rAll["Ctot",]-0.1*taille), labels=paste(n[3]-nC), ps=taille, col="green", font=1)
    text(x=xAll["Ctot",], y=(yAll["Ctot",]-rAll["Ctot",]-0.25*taille), labels=paste(nC), ps=taille, col="green", font=2)
    text(x=xAll["Dtot",], y=(yAll["Dtot",]-rAll["Dtot",]-0.1*taille), labels=paste(n[4]-nD), ps=taille, col="orange", font=1)
    text(x=xAll["Dtot",], y=(yAll["Dtot",]-rAll["Dtot",]-0.25*taille), labels=paste(nD), ps=taille, col="orange", font=2)
    
    text(x=(xmin+xAll["Atot",])/2, y=(yAll["Atot",]+yAll["AC",])/2, labels=paste("Total unique\ngenes: ", tot_ugenes, sep=""), ps=taille, col="black", font=2)
    
    text(x=c(xAll["AB",], xAll["AC",], xAll["AD",], xAll["BC",], xAll["BD",], x=xAll["CD",],xAll["ABC",], xAll["ABD",],xAll["ACD",], xAll["BCD",],xAll["ABCD",]), 
      y=c(yAll["AB",], yAll["AC",],yAll["AD",], yAll["BC",], yAll["BD",], yAll["CD",], yAll["ABC",], yAll["ABD",], yAll["ACD",], yAll["BCD",], yAll["ABCD",]), 
      labels=c(paste(nAB),paste(nAC),paste(nAD),paste(nBC), paste(nBD), paste(nCD), paste(nABC), paste(nABD), paste(nACD),paste(nBCD),paste(nABCD)), 
      ps=taille, col="black", font=1)
    
    dev.off()
  }
  
  ########################################################################################################
  ########################################################################################################  

  if(path_res == "")
  {
    if(!file.exists(paste(getwd(), "/Venn.diagrams/", sep=""))) dir.create(paste(getwd(), "/Venn.diagrams/", sep=""))
    path_res = paste(getwd(), "/Venn.diagrams/", sep="")
    write(paste("The results path have not been entered, the default results path is: \n\t", path_res, sep=""), file="")
    flush.console()
  }
  path = paste(path_res, "/Venn_", format(Sys.time(), "(%H-%M-%S)_%a_%d_%b_%Y"), sep="")
  dir.create(path)
  write(paste("The results will be placed here: \n\t", path, sep=""), file="")
  flush.console()
  
  os<-Sys.info()["sysname"]
  if((path_lists == "")&(os!="Windows")&(!is.matrix(res))& !Tk)
  {
     write(paste("You have to enter a path_lists" , sep=""), file="")
     break;
  }
  if((path_lists == "")&(os!="Windows")&(!is.matrix(res))& Tk)
  {
     write(paste("You have to enter a path_lists" , sep=""), file="")
     flush.console()
     path_lists = tk_choose.dir()
  }  
  if(!is.matrix(res)&(path_lists == "")&(os=="Windows"))
  {
     write(paste("Choose the directory where are placed the lists" , sep=""), file="")
     flush.console()
     path_lists = choose.dir()
  } 
  
  if(noms=="")  noms=c("A", "B", "C", "D") 
  noms = toupper(noms)

  if(!is.matrix(res))
  {
    listes = list.files(path = path_lists, full.names = TRUE)
    if(is.null(length(listes)))
    {
      write("The directory is empty.", file="")
      break    
    }
    data_t = test_list(liste=listes[1], ud, type="Res")
   
    if(ncol(data_t)>=1)  data_t = rownames(data_t)
    res = matrix(1, ncol=1, nrow=length(data_t))
    rownames(res) = data_t
    noms_listes = paste(substr(basename(listes[1]), 0, (nchar(basename(listes[1]))-4)), "_(", length(data_t), ")", sep="")
    
    for(i in 2:length(listes))
    {   
      data_t = test_list(listes[i], ud, type="Res")
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
      
      noms_listes = c(noms_listes, paste(substr(basename(listes[i]), 0, (nchar(basename(listes[i]))-4)), "_(", length(data_t), ")", sep=""))
      res = cbind(res, as.matrix(ajout))  
    }
    colnames(res) = noms_listes

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
        data_t = test_list(listes[M], ud, type="Annot")
        #ajoute une colonne vide entre les annots de chaque liste
        data_all = cbind(data_all, matrix("", ncol=1, nrow=nrow(data_all)))
        if(ncol(data_t)>1)
        {
          data_all = data_all[order(rownames(data_all)),]  #classe tous les IDs
          data_all = data_all[order(data_all[,M], decreasing = TRUE),]  #regroupe en tête les IDs classes de la liste en cours
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
        profils = data_all[,colnames(data_all) == "ratios"]
        
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
      write.table(data_all, file = paste(path, "/venn_annot.txt", sep=""), sep="\t")
    }else{
      if(!annot&ud)
      {
        data_all = res
        for(M in 1:length(listes)) #liste par liste
        {
          #lecture du fichier
          data_t = test_list(listes[M], ud, type="Annot")
          if(length(colnames(data_t)[colnames(data_t)=="ratios"])==1)
          {
             data_all = data_all[order(rownames(data_all)),]
             data_all = data_all[order(data_all[,M], decreasing = TRUE),]
             data_t = data_t[order(rownames(data_t)),]
             data_all = cbind(data_all, rbind(as.matrix(data_t[,"ratios"]), matrix(NA, ncol=1, nrow=nrow(data_all)-nrow(data_t))))
             colnames(data_all)[ncol(data_all)] = "ratios"
          }else{
            print(paste("\"ratios\" column not found in the ", basename(listes[M]), " file.", sep=""))
          }
        }
      }
    }
  }else{
     data_all = res
  }
  write.csv2(res, row.names = TRUE, file = paste(path, "/venn_matrix.csv", sep=""))
  tot_ugenes = nrow(res)  #nbre de genes ou id uniques
  
  if(ud)  
  {
    #matrice numerique des appartenances (res) et les ratios
    c = seq(1, ncol(res), by=1)
    nliste = c[colnames(res)=="Total_lists"]-1
    data_all = data_all[order(rownames(data_all)),]
    res = res[order(rownames(res)),]
    data_r = cbind(res[,1:(nliste+1)], data_all[,colnames(data_all)=="ratios"])
    data_rt = matrix(0, ncol=0, nrow=nrow(data_r))
    data_rt = cbind(data_rt, apply(data_r, 2, function(x) as.matrix(as.numeric(x))))
    colnames(data_rt) = colnames(data_r)
    data_r = data_rt
    data_r[is.na(data_r)] = ""
  }
  
  #graphs
  if(colnames(res)[3]=="Total_lists")
  {
     nA = nrow(res[(res[,"Total_lists"]==1)&(res[,1]==1),])
     if(is.null(nA)) nA=1
     nB = nrow(res[(res[,"Total_lists"]==1)&(res[,2]==1),])
     if(is.null(nB)) nB=1
     
     nAB = nrow(res[(res[,"Total_lists"]==2)&(res[,1]==1)&(res[,2]==1),])
     if(is.null(nAB)) nAB=1
     
     listeA = colnames(res)[1]
     listeB = colnames(res)[2]

     if(ud)
     {
        nAud = data_r[(data_r[,"Total_lists"]==1)&(data_r[,1]==1),4]
        nAu = length(nAud[nAud>=1])
        nAd = length(nAud[nAud<1])
        nBud = data_r[(data_r[,"Total_lists"]==1)&(data_r[,2]==1),5]
        nBu = length(nBud[nBud>=1])
        nBd = length(nBud[nBud<1])
        
        nABud = data_r[(data_r[,"Total_lists"]==2)&(data_r[,1]==1)&(data_r[,2]==1),4:5]
        nABud = compte(nABud)
        
        graph_2(path, listeA, listeB, nA, nB, nAB, tot_ugenes, noms, ud, nAu, nAd, nBu, nBd, nABud)
     }else{
        graph_2(path, listeA, listeB, nA, nB, nAB, tot_ugenes, noms, ud)
     }
     if(prop) graph_prop_2(path, res, nA, nB, nAB, tot_ugenes, noms)
  }
  
  if((ncol(res)>=4)&(colnames(res)[4]=="Total_lists"))
  {
     nA = nrow(res[(res[,"Total_lists"]==1)&(res[,1]==1),])
     if(is.null(nA)) nA=1
     nB = nrow(res[(res[,"Total_lists"]==1)&(res[,2]==1),])
     if(is.null(nB)) nB=1
     nC = nrow(res[(res[,"Total_lists"]==1)&(res[,3]==1),])
     if(is.null(nC)) nnCA=1
     
     nAB = nrow(res[(res[,"Total_lists"]==2)&(res[,1]==1)&(res[,2]==1),])
     if(is.null(nAB)) nAB=1
     nAC = nrow(res[(res[,"Total_lists"]==2)&(res[,1]==1)&(res[,3]==1),])
     if(is.null(nAC)) nAC=1
     nBC = nrow(res[(res[,"Total_lists"]==2)&(res[,2]==1)&(res[,3]==1),])
     if(is.null(nBC)) nBC=1
     
     nABC = nrow(res[res[,"Total_lists"]==3,])
     if(is.null(nABC)) nABC=1
     
     listeA = colnames(res)[1]
     listeB = colnames(res)[2]
     listeC = colnames(res)[3]

     if(ud)
     {
        nAud = data_r[(data_r[,"Total_lists"]==1)&(data_r[,1]==1),5]
        nAu = length(nAud[nAud>=1])
        nAd = length(nAud[nAud<1])
        nBud = data_r[(data_r[,"Total_lists"]==1)&(data_r[,2]==1),6]
        nBu = length(nBud[nBud>=1])
        nBd = length(nBud[nBud<1])
        nCud = data_r[(data_r[,"Total_lists"]==1)&(data_r[,3]==1),7]
        nCu = length(nCud[nCud>=1])
        nCd = length(nCud[nCud<1])
        
        nABud = data_r[(data_r[,"Total_lists"]==2)&(data_r[,1]==1)&(data_r[,2]==1),5:6]
        nABud = compte(nABud)
        nACud = data_r[(data_r[,"Total_lists"]==2)&(data_r[,1]==1)&(data_r[,3]==1),c(5,7)]
        nACud = compte(nACud)
        nBCud = data_r[(data_r[,"Total_lists"]==2)&(data_r[,2]==1)&(data_r[,3]==1),6:7]
        nBCud = compte(nBCud)
        
        nABCud = data_r[data_r[,"Total_lists"]==3,5:7]
        nABCud = compte(nABCud)
        
        graph_3(path, listeA, listeB, listeC, nA, nB, nC, nAB, nAC, nBC, nABC, tot_ugenes, noms, ud, nAu, nAd, nBu, nBd, nCu, nCd, nABud, nACud, nBCud, nABCud)
     }else{
        graph_3(path, listeA, listeB, listeC, nA, nB, nC, nAB, nAC, nBC, nABC, tot_ugenes, noms, ud)
     }
     if(prop) graph_prop_3(path, res, nA, nB, nC, nAB, nAC, nBC, nABC, tot_ugenes, noms)
  }
  
  if((ncol(res)>=5)&(colnames(res)[5]=="Total_lists"))
  {
     nA = nrow(res[(res[,"Total_lists"]==1)&(res[,1]==1),])
     if(is.null(nA)) nA=1
     nB = nrow(res[(res[,"Total_lists"]==1)&(res[,2]==1),])
     if(is.null(nB)) nB=1
     nC = nrow(res[(res[,"Total_lists"]==1)&(res[,3]==1),])
     if(is.null(nC)) nC=1
     nD = nrow(res[(res[,"Total_lists"]==1)&(res[,4]==1),])
     if(is.null(nD)) nD=1    
     
     nAB = nrow(res[(res[,"Total_lists"]==2)&(res[,1]==1)&(res[,2]==1),])
     if(is.null(nAB)) nAB=1
     nAC = nrow(res[(res[,"Total_lists"]==2)&(res[,1]==1)&(res[,3]==1),])
     if(is.null(nAC)) nAC=1
     nBD = nrow(res[(res[,"Total_lists"]==2)&(res[,2]==1)&(res[,4]==1),])
     if(is.null(nBD)) nBD=1
     nCD = nrow(res[(res[,"Total_lists"]==2)&(res[,3]==1)&(res[,4]==1),])
     if(is.null(nCD)) nCD=1
     nAD = nrow(res[(res[,"Total_lists"]==2)&(res[,1]==1)&(res[,4]==1),])
     if(is.null(nAD)) nAD=1
     nBC = nrow(res[(res[,"Total_lists"]==2)&(res[,2]==1)&(res[,3]==1),])
     if(is.null(nBC)) nBC=1
         
     nABC = nrow(res[(res[,"Total_lists"]==3)&(res[,1]==1)&(res[,2]==1)&(res[,3]==1),])
     if(is.null(nABC)) nABC=1
     nBCD = nrow(res[(res[,"Total_lists"]==3)&(res[,2]==1)&(res[,3]==1)&(res[,4]==1),])
     if(is.null(nBCD)) nBCD=1
     nACD = nrow(res[(res[,"Total_lists"]==3)&(res[,1]==1)&(res[,3]==1)&(res[,4]==1),])
     if(is.null(nACD)) nACD=1
     nABD = nrow(res[(res[,"Total_lists"]==3)&(res[,1]==1)&(res[,2]==1)&(res[,4]==1),])
     if(is.null(nABD)) nABD=1
     
     nABCD = nrow(res[res[,"Total_lists"]==4,])
     if(is.null(nABCD)) nABCD=1
     
     listeA = colnames(res)[1]
     listeB = colnames(res)[2]
     listeC = colnames(res)[3]
     listeD = colnames(res)[4]
     
     if(ud)
     {
        nAud = as.numeric(data_r[(data_r[,"Total_lists"]==1)&(data_r[,1]==1),6])
        nAu = length(nAud[nAud>=1])
        nAd = length(nAud[nAud<1])      
        nBud = as.numeric(data_r[(data_r[,"Total_lists"]==1)&(data_r[,2]==1),7])
        nBu = length(nBud[nBud>=1])
        nBd = length(nBud[nBud<1])       
        nCud = as.numeric(data_r[(data_r[,"Total_lists"]==1)&(data_r[,3]==1),8])
        nCu = length(nCud[nCud>=1])
        nCd = length(nCud[nCud<1])       
        nDud = as.numeric(data_r[(data_r[,"Total_lists"]==1)&(data_r[,4]==1),9])
        nDu = length(nDud[nDud>=1])
        nDd = length(nDud[nDud<1])
        
        nABud = data_r[(data_r[,"Total_lists"]==2)&(data_r[,1]==1)&(data_r[,2]==1),6:7]
        nABud = compte(nABud)        
        nACud = data_r[(data_r[,"Total_lists"]==2)&(data_r[,1]==1)&(data_r[,3]==1),c(6,8)]
        nACud = compte(nACud)   
        nBCud = data_r[(data_r[,"Total_lists"]==2)&(data_r[,2]==1)&(data_r[,3]==1),7:8]
        nBCud = compte(nBCud)
        nBDud = data_r[(data_r[,"Total_lists"]==2)&(data_r[,2]==1)&(data_r[,4]==1),c(7,9)]
        nBDud = compte(nBDud)
        nCDud = data_r[(data_r[,"Total_lists"]==2)&(data_r[,3]==1)&(data_r[,4]==1),8:9]
        nCDud = compte(nCDud)
        nADud = data_r[(data_r[,"Total_lists"]==2)&(data_r[,1]==1)&(data_r[,4]==1),c(6,9)]
        nADud = compte(nADud)   
        
        nABCud = data_r[(data_r[,"Total_lists"]==3)&(data_r[,1]==1)&(data_r[,2]==1)&(data_r[,3]==1),6:8]
        nABCud = compte(nABCud)
        nBCDud = data_r[(data_r[,"Total_lists"]==3)&(data_r[,2]==1)&(data_r[,3]==1)&(data_r[,4]==1),7:9]
        nBCDud = compte(nBCDud)
        nACDud = data_r[(data_r[,"Total_lists"]==3)&(data_r[,1]==1)&(data_r[,3]==1)&(data_r[,4]==1),c(6,8,9)]
        nACDud = compte(nACDud)
        nABDud = data_r[(data_r[,"Total_lists"]==3)&(data_r[,1]==1)&(data_r[,2]==1)&(data_r[,4]==1),c(6,7,9)]
        nABDud = compte(nABDud)  
        
        nABCDud = data_r[data_r[,"Total_lists"]==4,6:9]
        nABCDud = compte(nABCDud)

        graph_4(path, listeA, listeB, listeC, listeD, nA, nB, nC, nD, nAB, nAC, nBD, nCD, nAD, nBC, nABC, nBCD, nACD, nABD, nABCD, tot_ugenes, noms, ud, nAu, nAd, nBu, nBd, nCu, nCd, nDu, nDd, nABud, nACud, nBCud, nBDud, nCDud, nADud, nABCud, nBCDud, nABDud, nACDud, nABCDud)
     }else{
        graph_4(path, listeA, listeB, listeC, listeD, nA, nB, nC, nD, nAB, nAC, nBD, nCD, nAD, nBC, nABC, nBCD, nACD, nABD, nABCD, tot_ugenes, noms, ud)
     }
     if(prop) graph_prop_4(path, res, nA, nB, nC, nD, nAB, nAC, nBD, nCD, nAD, nBC, nABC, nBCD, nACD, nABD, nABCD, tot_ugenes, noms)
  }
  if(overlaps)  overlapp(res, path, f)
}
