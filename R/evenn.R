evenn <-
function(annot=FALSE, path_res="", path_lists="", res="", ud=FALSE, noms="")
{  
  write("\t#############################################################################", file="")
  write("\t#                                                                           #", file="")
  write("\t#                                eVenn (v1.24)                              #", file="")
  write("\t#                                                                           #", file="")
  write("\t#############################################################################\n", file="")
  write("\t[Run man() for quick help]\n", file="")
  
  
  ########################################################################################################
  ########################################################################################################
  #
  # Fonctions
      
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
  
  format_label<-function(n, m, nom, x, y, t, type=0, noms)
  {
     dv = 0.25*(t/0.5) #decalage vertical
     dh = 0.2*(t/0.5) #decalage horizontal
     d=4
     if(dim(m)!=0)
     {
       m = as.matrix(m) #details effectifs/groupes
       if(type!=4)  text(x=x, y=(y+0.05), labels=paste(n, sep=""), cex=t*(1.2), col="black"); x=x-0.3  #ref de localisation: total
       nl = length(strsplit(nom, ",")[[1]])
       if(nl==2) x = x-0.1*dh
       if(nl==3) x = x-0.1*dh
       if(nl==4) x = x-0.2*dh
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
        text(x=(x+0.1), y=(y-0.4), labels=paste(0, sep=""), cex=t*(1.2), col="black")
     }
  }
  
  #test des formats des listes
  test_list<-function(liste)
  {
    ext = substr(basename(liste), (nchar(basename(liste))-2), nchar(basename(liste)))
    data_t=""
    if(exists("data_t"))  rm("data_t")
    if(ext == "txt") data_t = read.table(file=liste, header=TRUE, sep="\t")
    if(ext == "csv") data_t = try(read.table(file=liste, header=TRUE, sep=","), silent=TRUE)
    if((ext == "csv")&(class(data_t)=="try-error")) data_t = try(read.table(file=liste, header=TRUE, sep=";"), silent=TRUE)
    if((ext!="csv")&(ext!="txt")) 
    {
      write("The file format is not supported (must be txt/tab or csv/,;)", file="")
      break;
    }
    if(ncol(data_t)>1) #plusieurs colonnes
    {
        rownames(data_t) = data_t[,1] #ID = 1ere colonne
        data_t = data_t[,2:ncol(data_t)]
    }
    data_t = as.matrix(data_t)
    return(data_t)
  }
  
  #graph 2
  graph_2<-function(path, listeA, listeB, nA, nB, nAB, tot_ugenes, noms)
  {
    dd=4.8; t=1
    pdf(file = paste(path, "/venn_diagram.pdf", sep=""))
    plot.window(c(0, 20), c(0, 15))
    plot(x=1:20, y=1:20,type="n", axes=FALSE, xlab="",ylab="")
    symbols(8, 7.5, circle=(1*dd), add=TRUE, inches=TRUE, fg="blue")
    symbols(12, 7.5, circle=(1*dd), add=TRUE, inches=TRUE, fg="red")
    
    text(x=6.5, y=7.5, labels=nA, cex=t, col="black")
    text(x=13.5, y=7.5, labels=nB, cex=t, col="black")
    
    text(x=10, y=7.5, labels=nAB, cex=t, col="black")
    
    text(x=3.5, y=11, labels=paste("Unics: ", tot_ugenes, sep=""), cex=(1.1*t), col="black")
    
    #titres
    text(x=10, y=1, labels=listeA, cex=(1.1*t), col="blue")
    text(x=10, y=13.5, labels=listeB, cex=(1.1*t), col="red")
    dev.off()
  }
  
  #graph 2 ud
  graph_2ud<-function(path, listeA, listeB, nA, nB, nAB, tot_ugenes, nAu, nAd, nBu, nBd, nABud, noms)
  {
    dd=4.8; t=1
    pdf(file = paste(path, "/venn_diagram_ud.pdf", sep=""))
    plot.window(c(0, 20), c(0, 15))
    plot(x=1:20, y=1:20,type="n", axes=FALSE, xlab="",ylab="")
    symbols(8, 7.5, circle=(1*dd), add=TRUE, inches=TRUE, fg="blue")
    symbols(12, 7.5, circle=(1*dd), add=TRUE, inches=TRUE, fg="red")
    
    text(x=6.5, y=8.5, labels=paste(nA, sep=""), cex=t*(1.2), col="blue")
    text(x=6.5, y=7.75, labels=paste("U:", nAu, sep=""), cex=t*(1.2), col="blue")
    text(x=6.5, y=7, labels=paste("D:", nAd, sep=""), cex=t*(1.2), col="blue")
    text(x=13.5, y=8.5, labels=paste(nB, sep=""), cex=t*(1.2), col="red")
    text(x=13.5, y=7.75, labels=paste("U:", nBu, sep=""), cex=t*(1.2), col="red")
    text(x=13.5, y=7, labels=paste("D:", nBd, sep=""), cex=t*(1.2), col="red")
                                
    format_label(n=nAB, m=nABud, nom=paste(noms[1], noms[2], sep=","), x=10, y=8.5, t, type=2, noms)
    
    text(x=3.5, y=11, labels=paste("Unics: ", tot_ugenes, sep=""), cex=(1.1*t), col="black")
    
    #titres
    text(x=10, y=1, labels=listeA, cex=(1.1*t), col="blue")
    text(x=10, y=13.5, labels=listeB, cex=(1.1*t), col="red")
    dev.off()
  }
  
  #graph 3
  graph_3<-function(path, listeA, listeB, listeC, nA, nB, nC, nAB, nAC, nBC, nABC, tot_ugenes, noms)
  {
    dd=5; t=1.1
    pdf(file = paste(path, "/venn_diagram.pdf", sep=""))
    plot.window(c(0, 20), c(0, 15))
    plot(x=1:20, y=1:20,type="n", axes=FALSE, xlab="",ylab="")
     
    symbols(10, 13, circle=(1*dd), add=TRUE, inches=TRUE, fg="blue")
    symbols(7.5, 8.5, circle=(1*dd), add=TRUE, inches=TRUE, fg="red")
    symbols(12.5, 8.5, circle=(1*dd), add=TRUE, inches=TRUE, fg="green")
    
    text(x=10, y=14, labels=nA, cex=t, col="black")
    text(x=6.5, y=8, labels=nB, cex=t, col="black")
    text(x=13.5, y=8, labels=nC, cex=t, col="black")
    
    text(x=8, y=11, labels=nAB, cex=t, col="black")
    text(x=12, y=11, labels=nAC, cex=t, col="black")
    text(x=10, y=7.5, labels=nBC, cex=t, col="black")
    
    text(x=10, y=10, labels=nABC, cex=t, col="black")
    
    text(x=2.5, y=14, labels=paste("Unics: ", tot_ugenes, sep=""), cex=(1.1*t), col="black")
    
    #titres
    text(x=10, y=18, labels=listeA, cex=(1.1*t), col="blue")
    text(x=10, y=2, labels=listeB, cex=(1.1*t), col="red")
    text(x=10, y=3.5, labels=listeC, cex=(1.1*t), col="green")
    dev.off()
  }
  
  #graph 3 ud
  graph_3ud<-function(path, listeA, listeB, listeC, nA, nB, nC, nAB, nAC, nBC, nABC, tot_ugenes, nAu, nAd, nBu, nBd, nCu, nCd, nABud, nACud, nBCud, nABCud, noms)
  {
    dd=6; t=0.7
    pdf(file = paste(path, "/venn_diagram_ud.pdf", sep=""))    
    plot(x=1:20, y=1:20,type="n", axes=FALSE, xlab="",ylab="")
     
    symbols(10, 12.5, circle=(1*dd), add=TRUE, inches=TRUE, fg="blue")
    symbols(8, 8.5, circle=(1*dd), add=TRUE, inches=TRUE, fg="red")
    symbols(12, 8.5, circle=(1*dd), add=TRUE, inches=TRUE, fg="green")
    
    text(x=10, y=14.5, labels=paste(nA), cex=t*(1.2), col="blue")
    text(x=10, y=14, labels=paste("U:", nAu), cex=t*(1.2), col="blue")
    text(x=10, y=13.5, labels=paste("D:", nAd), cex=t*(1.2), col="blue")
        
    text(x=6.5, y=8.5, labels=paste(nB), cex=t*(1.2), col="red")  
    text(x=6.5, y=8, labels=paste("U:", nBu), cex=t*(1.2), col="red")  
    text(x=6.5, y=7.5, labels=paste("D:", nBd), cex=t*(1.2), col="red")  
      
    text(x=13.5, y=8.5, labels=paste(nC), cex=t*(1.2), col="green")
    text(x=13.5, y=8, labels=paste("U:", nCu), cex=t*(1.2), col="green")
    text(x=13.5, y=7.5, labels=paste("D:", nCd), cex=t*(1.2), col="green")
    
    format_label(n=nAB, m=nABud, nom=paste(noms[1], noms[2], sep=","), x=8, y=11.5, t, type=3, noms)
    format_label(n=nAC, m=nACud, nom=paste(noms[1], noms[3], sep=","), x=12, y=11.5, t, type=3, noms)
    format_label(n=nBC, m=nBCud, nom=paste(noms[2], noms[3], sep=","), x=10, y=7.5, t, type=3, noms)
  
    format_label(n=nABC, m=nABCud, nom=paste(noms[1], noms[2], noms[3], sep=","), x=10, y=11, t, type=3, noms)
    
    text(x=2.5, y=14, labels=paste("Unics: ", tot_ugenes, sep=""), cex=(1.1*t), col="black")
    
    #titres
    text(x=10, y=18, labels=listeA, cex=(1.1*t), col="blue")
    text(x=10, y=2, labels=listeB, cex=(1.1*t), col="red")
    text(x=10, y=3, labels=listeC, cex=(1.1*t), col="green")
    dev.off()
  }
  
  #graph 4
  graph_4<-function(path, listeA, listeB, listeC, listeD, nA, nB, nC, nD, nAB, nAC, nBD, nCD, nAD, nBC, nABC, nBCD, nACD, nABD, nABCD, tot_ugenes, noms)
  {
    dd=6; t=1
    pdf(file = paste(path, "/venn_diagram.pdf", sep=""), width=10, height=10)
    plot.window(c(0, 20), c(0, 20))
    plot(x=1:25, y=1:25,type="n", axes=FALSE, xlab="",ylab="")
    symbols(10, 9, circle=(1*dd), add=TRUE, inches=TRUE, fg="orange")
    symbols(8.5, 7.5, circle=(1*dd), add=TRUE, inches=TRUE, fg="blue")
    symbols(11.5, 7.5, circle=(1*dd), add=TRUE, inches=TRUE, fg="red")
    symbols(10, 6, circle=(1*dd), add=TRUE, inches=TRUE, fg="green")
    
    symbols(9, 16, circle=(0.3*dd), add=TRUE, inches=TRUE, fg="blue")
    symbols(11, 16, circle=(0.3*dd), add=TRUE, inches=TRUE, fg="red")
    symbols(18.5, 8.5, circle=(0.3*dd), add=TRUE, inches=TRUE, fg="orange")
    symbols(18.5, 6.5, circle=(0.3*dd), add=TRUE, inches=TRUE, fg="green")
    
    text(x=10, y=11, labels=nA, cex=t, col="orange")
    text(x=6.5, y=7.5, labels=nB, cex=t, col="blue")
    text(x=13.5, y=7.5, labels=nC, cex=t, col="red")
    text(x=10, y=3.75, labels=nD, cex=t, col="green")
    
    text(x=8, y=9.5, labels=nAB, cex=t, col="black")
    text(x=11.5, y=9.5, labels=nAC, cex=t, col="black")
    text(x=8, y=5.5, labels=nBD, cex=t, col="black")
    text(x=11.5, y=5.5, labels=nCD, cex=t, col="black")
    
    text(x=18.5, y=7.5, labels=nAD, cex=(1.3*t), col="black")    
    text(x=10, y=16, labels=nBC, cex=(1.3*t), col="black")
    
    text(x=10, y=9.5, labels=nABC, cex=t, col="black")
    text(x=10, y=5.5, labels=nBCD, cex=t, col="black")
    text(x=8, y=7.5, labels=nABD, cex=t, col="black")
    text(x=12, y=7.5, labels=nACD, cex=t, col="black")
    
    text(x=10, y=7.5, labels=nABCD, cex=(1.3*t), col="black")
    
    text(x=18.5, y=16, labels=paste("Unics: ", tot_ugenes, sep=""), cex=(1.3*t), col="black")
    
    #titres
    text(x=12.5, y=24.5, labels=listeA, cex=(1.3*t), col="orange")
    text(x=12.5, y=23.5, labels=listeB, cex=(1.3*t), col="blue")
    text(x=12.5, y=22.5, labels=listeC, cex=(1.3*t), col="red")
    text(x=12.5, y=21.5, labels=listeD, cex=(1.3*t), col="green")
    
    dev.off()
  }
  
  #graph 4 ud
  graph_4ud<-function(path, listeA, listeB, listeC, listeD, nA, nB, nC, nD, nAB, nAC, nBD, nCD, nAD, nBC, nABC, nBCD, nACD, nABD, nABCD, tot_ugenes, nAu, nAd, nBu, nBd, nCu, nCd, nDu, nDd, nABud, nACud, nBCud, nBDud, nCDud, nADud, nABCud, nBCDud, nABDud, nACDud, nABCDud, noms)
  {
    dd=8; t=0.7; d=6.5
    pdf(file = paste(path, "/venn_diagram_ud.pdf", sep=""), width=10, height=10)
    plot.window(c(0, 20), c(0, 20))
    plot(x=1:20, y=1:20,type="n", axes=FALSE, xlab="",ylab="")
    symbols(8, 9.4, circle=(1*dd), add=TRUE, inches=TRUE, fg="blue")  #A
    symbols(6.8, 8.4, circle=(1*dd), add=TRUE, inches=TRUE, fg="red") #B
    symbols(9.2, 8.4, circle=(1*dd), add=TRUE, inches=TRUE, fg="green") #C
    symbols(8, 7.4, circle=(1*dd), add=TRUE, inches=TRUE, fg="orange")  #D

    #AC
    symbols(16.8, 15.6, circle=(0.3*dd), add=TRUE, inches=TRUE, fg="red")
    symbols(17.6, 15.6, circle=(0.3*dd), add=TRUE, inches=TRUE, fg="green")
    #BD
    symbols(18, 6.8, circle=(0.3*dd), add=TRUE, inches=TRUE, fg="blue")
    symbols(18, 6, circle=(0.3*dd), add=TRUE, inches=TRUE, fg="orange")

    #ABCD
    symbols(3, 18, circle=(0.3*dd), add=TRUE, inches=TRUE, fg="red")
    symbols(3.4, 18, circle=(0.3*dd), add=TRUE, inches=TRUE, fg="green")
    symbols(3.2, 18.2, circle=(0.3*dd), add=TRUE, inches=TRUE, fg="blue")
    symbols(3.2, 17.8, circle=(0.3*dd), add=TRUE, inches=TRUE, fg="orange")

    #A
    text(x=8, y=11.4, labels=paste(noms[1], "\n", nA, sep=""), cex=t, col="blue")
    text(x=6.8, y=16.8, labels=paste(noms[1], "\n", "U:", nAu, "\nD:", nAd, sep=""), cex=t*1.2, col="blue")
    #B
    text(x=5.2, y=8.4, labels=paste(noms[2], "\n", nB, sep=""), cex=t, col="red")
    text(x=1.6, y=9, labels=paste(noms[2], "\nU:", nBu, "\nD:", nBd, sep=""), cex=t*1.2, col="red")
    #C
    text(x=10.8, y=8.4, labels=paste(noms[3], "\n", nC, sep=""), cex=t*(1), col="green")
    text(x=14.4, y=8.4, labels=paste(noms[3], "\nU:", nCu, "\nD:", nCd, sep=""), cex=t*1.2, col="green")
    #D
    text(x=8, y=5.6, labels=paste(noms[4], "\n", nD, sep=""), cex=t, col="orange")
    text(x=6.8, y=1.4, labels=paste(noms[4], "\nU:", nDu, "\nD:", nDd, sep=""), cex=t*1.2, col="orange")

    #AB
    text(x=6.4, y=10, labels=paste(noms[1], ",", noms[2], "\n", nAB, sep=""), cex=t, col="black")
    pos=3.5 - (t/d)*((nchar(paste(noms[2], sep=""))-nchar(noms[1])/2)/2)
    text(x=pos, y=13.2, labels=paste(noms[1]), cex=t*1.2, col="blue")
    pos = pos + (t/d)*(nchar(paste(noms[1], noms[2], sep="")))
    text(x=pos, y=13.2, labels=paste(noms[2]), cex=t*1.2, col="red")
    format_label(n=nAB, m=nABud, nom=paste(noms[1], ",", noms[2], sep=""), x=3.5, y=13.2, t, type=4, noms)

    #AC
    text(x=9.6, y=10, labels=paste(noms[1], ",", noms[3], "\n", nAC, sep=""), cex=t, col="black")
    pos = 11.5 - (t/d)*((nchar(paste(noms[3], sep=""))-nchar(noms[1])/2)/2)
    text(x=pos, y=13.2, labels=paste(noms[1]), cex=t*1.2, col="blue")
    pos = pos + (t/d)*(nchar(paste(noms[1], noms[3], sep="")))
    text(x=pos, y=13.2, labels=paste(noms[3]), cex=t*1.2, col="green")
    format_label(n=nAC, m=nACud, nom=paste(noms[1], ",", noms[3], sep=""), x=11.5, y=13.2, t, type=4, noms)

    #BD
    text(x=6.4, y=6.8, labels=paste(noms[2], ",", noms[4], "\n", nBD, sep=""), cex=t, col="black")
    pos = 3.5  - (t/d)*((nchar(paste(noms[4], sep=""))-nchar(noms[2])/2)/2)
    text(x=pos, y=3.2, labels=paste(noms[2]), cex=t*1.2, col="red")
    pos = pos + (t/d)*(nchar(paste(noms[2], noms[4], sep="")))
    text(x=pos, y=3.2, labels=paste(noms[4]), cex=t*1.2, col="orange")
    format_label(n=nBD, m=nBDud, nom=paste(noms[2], ",", noms[4], nBD, sep=""), x=3.5, y=3.2, t, type=4, noms)

    #CD
    text(x=9.6, y=6.8, labels=paste(noms[3], ",", noms[4], "\n", nCD, sep=""), cex=t, col="black")
    pos = 11.9 - (t/d)*((nchar(paste(noms[4], sep=""))-nchar(noms[3])/2)/2)
    text(x=pos, y=3.2, labels=paste(noms[3]), cex=t*1.2, col="green")
    pos = pos + (t/d)*(nchar(paste(noms[3], noms[4], sep="")))
    text(x=pos, y=3.2, labels=paste(noms[4]), cex=t*1.2, col="orange")
    format_label(n=nCD, m=nCDud, nom=paste(noms[3], ",", noms[4], sep=""), x=11.9, y=3.2, t, type=4, noms)

    #AD
    pos = 17.9 - (t/d)*((nchar(paste(noms[4], sep=""))-nchar(noms[1])/2)/2)
    text(x=pos, y=7.6, labels=paste(noms[1]), cex=t*1.2, col="blue")
    pos = pos + (t/d)*(nchar(paste(noms[1], noms[4], sep="")))
    text(x=pos, y=7.6, labels=paste(noms[4]), cex=t*1.2, col="orange")
    text(x=18, y=7.2, labels=paste(nAD), cex=t, col="black")
    format_label(n=nAD, m=nADud, nom=paste(noms[1], ",", noms[4], sep=""), x=17.9, y=6.8, t, type=4, noms)

    #BC
    pos = 17.1 - (t/d)*((nchar(paste(noms[2], sep=""))-nchar(noms[3])/2)/2)
    text(x=pos, y=17.2, labels=paste(noms[2]), cex=t*1.2, col="red")
    pos = pos + (t/d)*(nchar(paste(noms[2], noms[3], sep="")))
    text(x=pos, y=17.2, labels=paste(noms[3]), cex=t*1.2, col="green")
    text(x=17.2, y=16.8, labels=paste(nBC), cex=t, col="black")
    format_label(n=nBC, m=nBCud, nom=paste(noms[2], ",", noms[3], sep=""), x=17.1, y=16.4, t, type=4, noms)

    #ABC
    text(x=8, y=10.2, labels=paste(noms[1], ",", noms[2], ",", noms[3], "\n", nABC, sep=""), cex=t, col="black")
    pos = 8.1 - (t/d)*((nchar(paste(noms[2], noms[3], sep=""))-nchar(noms[1])/2)/2)
    text(x=pos, y=15.2, labels=paste(noms[1]), cex=t*1.2, col="blue")
    pos = pos + (t/d)*(nchar(paste(noms[1], noms[2], sep="")))
    text(x=pos, y=15.2, labels=paste(noms[2]), cex=t*1.2, col="red")
    pos = pos + (t/d)*(nchar(paste(noms[2], noms[3], sep="")))
    text(x=pos, y=15.2, labels=paste(noms[3]), cex=t*1.2, col="green")
    format_label(n=nABC, m=nABCud, nom=paste(noms[1], ",", noms[2], ",", noms[3], sep=""), x=8.1, y=15.2, t, type=4, noms)

    #BCD
    text(x=8, y=6.6, labels=paste(noms[2], ",", noms[3], ",", noms[4], "\n", nBCD, sep=""), cex=t, col="black")
    pos = 9.4 - (t/d)*((nchar(paste(noms[3], noms[4], sep=""))-nchar(noms[2])/2)/2)
    text(x=pos, y=4, labels=paste(noms[2]), cex=t*1.2, col="red")
    pos = pos + (t/d)*(nchar(paste(noms[2], noms[3], sep="")))
    text(x=pos, y=4, labels=paste(noms[3]), cex=t*1.2, col="green")
    pos = pos + (t/d)*(nchar(paste(noms[3], noms[4], sep="")))
    text(x=pos, y=4, labels=paste(noms[4]), cex=t*1.2, col="orange")
    format_label(n=nBCD, m=nBCDud, nom=paste(noms[2], ",", noms[3], ",", noms[4], sep=""), x=9.4, y=4, t, type=4, noms)

    #ACD
    text(x=9.6, y=8.2, labels=paste(noms[1], ",", noms[3], ",", noms[4], "\n", nACD, sep=""), cex=t, col="black")
    pos = 12.6 - (t/d)*((nchar(paste(noms[3], noms[4], sep=""))-nchar(noms[1])/2)/2)
    text(x=pos, y=8.8, labels=paste(noms[1]), cex=t*1.2, col="blue")
    pos = pos + (t/d)*(nchar(paste(noms[1], noms[3], sep="")))
    text(x=pos, y=8.8, labels=paste(noms[3]), cex=t*1.2, col="green")
    pos = pos + (t/d)*(nchar(paste(noms[3], noms[4], sep="")))
    text(x=pos, y=8.8, labels=paste(noms[4]), cex=t*1.2, col="orange")
    format_label(n=nACD, m=nACDud, nom=paste(noms[1], ",", noms[3], ",", noms[4], sep=""), x=12.6, y=8.8, t, type=4, noms)

    #ABD
    text(x=6.4, y=8.2, labels=paste(noms[1], ",", noms[2], ",", noms[4], "\n", nABD, sep=""), cex=t, col="black")
    pos = 3.1 - (t/d)*((nchar(paste(noms[2], noms[4], sep=""))-nchar(noms[1])/2)/2)
    text(x=pos, y=8.8, labels=paste(noms[1]), cex=t*1.2, col="blue")
    pos = pos + (t/d)*(nchar(paste(noms[1], noms[2], sep="")))
    text(x=pos, y=8.8, labels=paste(noms[2]), cex=t*1.2, col="red")
    pos = pos + (t/d)*(nchar(paste(noms[2], noms[4], sep="")))
    text(x=pos, y=8.8, labels=paste(noms[4]), cex=t*1.2, col="orange")
    format_label(n=nABD, m=nABDud, nom=paste(noms[1], ",", noms[2], ",", noms[4], sep=""), x=3.1, y=8.8, t, type=4, noms)

    #ABCD
    text(x=8, y=8.5, labels=paste(noms[1], ",", noms[2], ",", noms[3], ",", noms[4], "\n", nABCD, sep=""), cex=t*1.2, col="black")
    pos = 2.8 - (t/d)*((nchar(paste(noms[2], noms[3], noms[4], sep=""))-nchar(noms[1])/2)/2)
    text(x=pos, y=19.6, labels=paste(noms[1]), cex=t*1.2, col="blue")
    pos = pos + (t/d)*(nchar(paste(noms[1], noms[2], sep="")))
    text(x=pos, y=19.6, labels=paste(noms[2]), cex=t*1.2, col="red")
    pos = pos + (t/d)*(nchar(paste(noms[2], noms[3], sep="")))
    text(x=pos, y=19.6, labels=paste(noms[3]), cex=t*1.2, col="green")
    pos = pos + (t/d)*(nchar(paste(noms[3], noms[4], sep="")))
    text(x=pos, y=19.6, labels=paste(noms[4]), cex=t*1.2, col="orange")
    format_label(n=nABCD, m=nABCDud, nom=paste(noms[1], ",", noms[2], ",", noms[3], ",", noms[4], sep=""), x=2.8, y=19.6, t, type=4, noms)

    text(x=4, y=14.4, labels=paste("Unics: ", tot_ugenes, sep=""), cex=(1.3*t), col="black")

    #titres
    text(x=10, y=20.4, labels=paste(noms[1], ":", listeA, sep=""), cex=(1.3*t), col="blue")
    text(x=10, y=19.8, labels=paste(noms[2], ":", listeB, sep=""), cex=(1.3*t), col="red")
    text(x=10, y=19.2, labels=paste(noms[3], ":", listeC, sep=""), cex=(1.3*t), col="green")
    text(x=10, y=18.6, labels=paste(noms[4], ":", listeD, sep=""), cex=(1.3*t), col="orange")

    dev.off()
  }
  
  ########################################################################################################
  ########################################################################################################  
  options(warn=-1)
 
  if(path_res == "")
  {
    if(!file.exists(paste(getwd(), "/Venn.diagrams/", sep=""))) dir.create(paste(getwd(), "/Venn.diagrams/", sep=""))
    path_res = paste(getwd(), "/Venn.diagrams/", sep="")
    write(paste("The results path have not been entered, the default results path is: \n\t", path_res, sep=""), file="")
  }
  path = paste(path_res, "/Venn_", format(Sys.time(), "(%H-%M-%S)_%a_%d_%b_%Y"), sep="")
  dir.create(path)
  write(paste("The results will be placed here: \n\t", path, sep=""), file="")
  
  os<-Sys.info()["sysname"]
  if((path_lists == "")&(os!="Windows")&(!is.matrix(res)))
  {
     write(paste("You have to enter a path_lists" , sep=""), file="")
     break;
  } 
  if(!is.matrix(res))  if((path_lists == "")&(os=="Windows"))
  {
     write(paste("Choose the directory where are placed the lists" , sep=""), file="")
     path_lists = choose.dir()
  } 
  
  if(noms=="")  noms=c("A", "B", "C", "D") 
  noms = toupper(noms)

  if(!is.matrix(res))
  {
    listes = list.files(path = path_lists, full.names = TRUE)    
    data_t = test_list(liste=listes[1])
    if(ncol(data_t)>1)  data_t = rownames(data_t)
    res = matrix(1, ncol=1, nrow=length(data_t))
    rownames(res) = data_t
    noms_listes = substr(basename(listes[1]), 0, (nchar(basename(listes[1]))-4))
    
    for(i in 2:length(listes))
    {   
      data_t = test_list(listes[i])
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
      res = rbind(res, matrix(0, ncol=ncol(res), nrow=sum(temp_new)))
      rownames(res)[(nrow(res)-sum(temp_new)+1):nrow(res)] = rownames(temp_new)[temp_new==1]
      res = res[order(rownames(res)),]
      
      ajout = rbind(matrix(0, ncol=1, nrow=sum(temp_old)), matrix(1, ncol=1, nrow=length(data_t)))
      rownames(ajout) = c(rownames(temp_old)[temp_old==1], data_t)
      ajout = ajout[order(rownames(ajout)),]
      
      noms_listes = c(noms_listes, substr(basename(listes[i]), 0, (nchar(basename(listes[i]))-4)))
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
        data_t = test_list(listes[M])
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
      write.table(data_all, file = paste(path, "/venn_annot.txt", sep=""), sep="\t")
    }else{
      if(!annot&ud)
      {
        data_all = res
        for(M in 1:length(listes)) #liste par liste
        {
          #lecture du fichier
          data_t = test_list(listes[M])
          if(length(colnames(data_t)[colnames(data_t)=="ratios"])==1)
          {
             data_all = data_all[order(rownames(data_all)),]
             data_all = data_all[order(data_all[,M], decreasing = TRUE),]
             data_t = data_t[order(rownames(data_t)),]
             data_all = cbind(data_all, rbind(as.matrix(data_t[,"ratios"]), matrix(NA, ncol=1, nrow=nrow(data_all)-nrow(data_t))))
             colnames(data_all)[ncol(data_all)] = "ratios"
          }else{
            print(paste("La liste ", basename(listes[M]), " ne comporte pas de colonne \"ratios\"", sep=""))
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
     
     graph_2(path, listeA, listeB, nA, nB, nAB, tot_ugenes, noms)
     
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
        
        graph_2ud(path, listeA, listeB, nA, nB, nAB, tot_ugenes, nAu, nAd, nBu, nBd, nABud, noms)
     }
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
     
     graph_3(path, listeA, listeB, listeC, nA, nB, nC, nAB, nAC, nBC, nABC, tot_ugenes, noms)
     
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
        
        graph_3ud(path, listeA, listeB, listeC, nA, nB, nC, nAB, nAC, nBC, nABC, tot_ugenes, nAu, nAd, nBu, nBd, nCu, nCd, nABud, nACud, nBCud, nABCud, noms)
     }
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
     
     graph_4(path, listeA, listeB, listeC, listeD, nA, nB, nC, nD, nAB, nAC, nBD, nCD, nAD, nBC, nABC, nBCD, nACD, nABD, nABCD, tot_ugenes, noms)
     
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
        
        graph_4ud(path, listeA, listeB, listeC, listeD, nA, nB, nC, nD, nAB, nAC, nBD, nCD, nAD, nBC, nABC, nBCD, nACD, nABD, nABCD, tot_ugenes, nAu, nAd, nBu, nBd, nCu, nCd, nDu, nDd, nABud, nACud, nBCud, nBDud, nCDud, nADud, nABCud, nBCDud, nABDud, nACDud, nABCDud, noms)
     }
  }
}

