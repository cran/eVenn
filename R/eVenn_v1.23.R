#v1.2  - details des up et down
#      - Les colonnes ratios doit etre nommee "ratios"
#      - Les "ud" des profils sont en couleurs, spécifiques des listes
#
#V1.1: - Version independante d'autres packages (plotrix)
#      - Compatible avec les listes d'une seule colonne d'identifiants
#      - Compatible avec des listes annotees et listes seulement IDs

#evenn(annot=TRUE, path_res="", path_lists="", res="", ud=TRUE)

#annot=TRUE; path_res=""; path_lists=""; res=""; ud=TRUE

evenn <-function(annot=FALSE, path_res="", path_lists="", res="", ud=FALSE)
{  
  write("\t#############################################################################\n")
  write("\t#                                                                           #\n")
  write("\t#                                eVenn (v1.23)                              #\n")
  write("\t#                                                                           #\n")
  write("\t#############################################################################\n")
  
  
  ########################################################################################################
  ########################################################################################################
  #
  # Fonctions
      
  compte<-function(x) #compte les types de profils up/down
  {
    t = x
    t[x<1] = "d"
    t[x>=1] = "u"
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
  
  format_label<-function(n, m, nom, x, y, t, type=0)
  {
     if(dim(m)!=0)
     {
       dv = 0.25*(t/0.5) #decalage vertical
       dh = 0.13*(t/0.5) #decalage horizontal
       m = as.matrix(m) #details effectifs/groupes
       if(type!=4)  text(x=x, y=(y+0.05), labels=paste(n, sep=""), cex=t*(1.2), col="black")  #ref de localisation: total
       for(I in 1:nrow(m))
       {
          for(J in 1:nchar(rownames(m)[I]))
          {
            if(substr(nom,J,J) == "A")  couleur = "blue"
            if(substr(nom,J,J) == "B")  couleur = "red"
            if(substr(nom,J,J) == "C")  couleur = "green"
            if(substr(nom,J,J) == "D")  couleur = "orange"
            text(x=(x-0.7+(J-1*(nchar(nom)/2))*dh), y=(y-(I*dv)), labels=paste(substr(rownames(m)[I],J,J), sep=""), cex=t, col=couleur)
          }
          text(x=(x-0.6+(J-1*(nchar(nom)/2)+2+(nchar(paste(": ", m[I, 1], sep=""))-3)/2)*dh), y=(y-(I*dv)), labels=paste(": ", m[I, 1], sep=""), cex=t, col="black")
        }
     }else{
        text(x=x, y=(y+0.05), labels=paste(0, sep=""), cex=t*(1.2), col="black")
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
  graph_2<-function(path, listeA, listeB, nA, nB, nAB, tot_ugenes)
  {
    dx=5; dy=5; dd=4.8; t=1
    pdf(file = paste(path, "/venn_diagram.pdf", sep=""))
    plot.window(c(0, 20), c(0, 15))
    plot(x=1:(4*dx), y=1:(4*dy),type="n", axes=FALSE, xlab="",ylab="")
    symbols((1.6*dx), (1.5*dy), circle=(1*dd), add=TRUE, inches=TRUE, fg="blue")
    symbols((2.4*dx), (1.5*dy), circle=(1*dd), add=TRUE, inches=TRUE, fg="red")
    
    text(x=(1.3*dx), y=(1.5*dy), labels=nA, cex=t, col="black")
    text(x=(2.7*dx), y=(1.5*dy), labels=nB, cex=t, col="black")
    
    text(x=(2*dx), y=(1.5*dy), labels=nAB, cex=t, col="black")
    
    text(x=(0.7*dx), y=(2.2*dy), labels=paste("Unics: ", tot_ugenes, sep=""), cex=(1.1*t), col="black")
    
    #titres
    text(x=(2*dx), y=(0.2*dy), labels=listeA, cex=(1.1*t), col="blue")
    text(x=(2*dx), y=(2.7*dy), labels=listeB, cex=(1.1*t), col="red")
    dev.off()
  }
  
  #graph 2 ud
  graph_2ud<-function(path, listeA, listeB, nA, nB, nAB, tot_ugenes, nAu, nAd, nBu, nBd, nABud)
  {
    dx=5; dy=5; dd=4.8; t=1
    pdf(file = paste(path, "/venn_diagram_ud.pdf", sep=""))
    plot.window(c(0, 20), c(0, 15))
    plot(x=1:(4*dx), y=1:(4*dy),type="n", axes=FALSE, xlab="",ylab="")
    symbols((1.6*dx), (1.5*dy), circle=(1*dd), add=TRUE, inches=TRUE, fg="blue")
    symbols((2.4*dx), (1.5*dy), circle=(1*dd), add=TRUE, inches=TRUE, fg="red")
    
    text(x=(1.3*dx), y=(1.7*dy), labels=paste(nA, sep=""), cex=t*(1.2), col="blue")
    text(x=(1.3*dx), y=(1.55*dy), labels=paste("u:", nAu, sep=""), cex=t*(1.2), col="blue")
    text(x=(1.3*dx), y=(1.4*dy), labels=paste("d:", nAd, sep=""), cex=t*(1.2), col="blue")
    text(x=(2.7*dx), y=(1.7*dy), labels=paste(nB, sep=""), cex=t*(1.2), col="red")
    text(x=(2.7*dx), y=(1.55*dy), labels=paste("u:", nBu, sep=""), cex=t*(1.2), col="red")
    text(x=(2.7*dx), y=(1.4*dy), labels=paste("d:", nBd, sep=""), cex=t*(1.2), col="red")
                                
    format_label(n=nAB, m=nABud, nom="AB", x=(2*dx), y=(1.7*dy), t, type=2)
    
    text(x=(0.7*dx), y=(2.2*dy), labels=paste("Unics: ", tot_ugenes, sep=""), cex=(1.1*t), col="black")
    
    #titres
    text(x=(2*dx), y=(0.2*dy), labels=listeA, cex=(1.1*t), col="blue")
    text(x=(2*dx), y=(2.7*dy), labels=listeB, cex=(1.1*t), col="red")
    dev.off()
  }
  
  #graph 3
  graph_3<-function(path, listeA, listeB, listeC, nA, nB, nC, nAB, nAC, nBC, nABC, tot_ugenes)
  {
    dx=5; dy=5; dd=5; t=1.1
    pdf(file = paste(path, "/venn_diagram.pdf", sep=""))
    plot.window(c(0, 20), c(0, 15))
    plot(x=1:(4*dx), y=1:(4*dy),type="n", axes=FALSE, xlab="",ylab="")
     
    symbols((2*dx), (2.6*dy), circle=(1*dd), add=TRUE, inches=TRUE, fg="blue")
    symbols((1.5*dx), (1.7*dy), circle=(1*dd), add=TRUE, inches=TRUE, fg="red")
    symbols((2.5*dx), (1.7*dy), circle=(1*dd), add=TRUE, inches=TRUE, fg="green")
    
    text(x=(2*dx), y=(2.8*dy), labels=nA, cex=t, col="black")
    text(x=(1.3*dx), y=(1.6*dy), labels=nB, cex=t, col="black")
    text(x=(2.7*dx), y=(1.6*dy), labels=nC, cex=t, col="black")
    
    text(x=(1.6*dx), y=(2.2*dy), labels=nAB, cex=t, col="black")
    text(x=(2.4*dx), y=(2.2*dy), labels=nAC, cex=t, col="black")
    text(x=(2*dx), y=(1.5*dy), labels=nBC, cex=t, col="black")
    
    text(x=(2*dx), y=(2*dy), labels=nABC, cex=t, col="black")
    
    text(x=(0.5*dx), y=(2.8*dy), labels=paste("Unics: ", tot_ugenes, sep=""), cex=(1.1*t), col="black")
    
    #titres
    text(x=(2*dx), y=(3.6*dy), labels=listeA, cex=(1.1*t), col="blue")
    text(x=(2*dx), y=(0.4*dy), labels=listeB, cex=(1.1*t), col="red")
    text(x=(2*dx), y=(0.7*dy), labels=listeC, cex=(1.1*t), col="green")
    dev.off()
  }
  
  #graph 3 ud
  graph_3ud<-function(path, listeA, listeB, listeC, nA, nB, nC, nAB, nAC, nBC, nABC, tot_ugenes, nAu, nAd, nBu, nBd, nCu, nCd, nABud, nACud, nBCud, nABCud)
  {
    dx=5; dy=5; dd=6; t=0.7
    pdf(file = paste(path, "/venn_diagram_ud.pdf", sep=""))    
    plot(x=1:(4*dx), y=1:(4*dy),type="n", axes=FALSE, xlab="",ylab="")
     
    symbols((2*dx), (2.5*dy), circle=(1*dd), add=TRUE, inches=TRUE, fg="blue")
    symbols((1.6*dx), (1.7*dy), circle=(1*dd), add=TRUE, inches=TRUE, fg="red")
    symbols((2.4*dx), (1.7*dy), circle=(1*dd), add=TRUE, inches=TRUE, fg="green")
    
    text(x=(2*dx), y=(2.9*dy), labels=paste(nA), cex=t*(1.2), col="blue")
    text(x=(2*dx), y=(2.8*dy), labels=paste("u:", nAu), cex=t*(1.2), col="blue")
    text(x=(2*dx), y=(2.7*dy), labels=paste("d:", nAd), cex=t*(1.2), col="blue")
        
    text(x=(1.3*dx), y=(1.7*dy), labels=paste(nB), cex=t*(1.2), col="red")  
    text(x=(1.3*dx), y=(1.6*dy), labels=paste("u:", nBu), cex=t*(1.2), col="red")  
    text(x=(1.3*dx), y=(1.5*dy), labels=paste("d:", nBd), cex=t*(1.2), col="red")  
      
    text(x=(2.7*dx), y=(1.7*dy), labels=paste(nC), cex=t*(1.2), col="green")
    text(x=(2.7*dx), y=(1.6*dy), labels=paste("u:", nCu), cex=t*(1.2), col="green")
    text(x=(2.7*dx), y=(1.5*dy), labels=paste("d:", nCd), cex=t*(1.2), col="green")
    
    format_label(n=nAB, m=nABud, nom="AB", x=(1.6*dx), y=(2.3*dy), t, type=3)
    format_label(n=nAC, m=nACud, nom="AC", x=(2.4*dx), y=(2.3*dy), t, type=3)
    format_label(n=nBC, m=nBCud, nom="BC", x=(2*dx), y=(1.5*dy), t, type=3)
  
    format_label(n=nABC, m=nABCud, nom="ABC", x=(2*dx), y=(2.2*dy), t, type=3)
    
    text(x=(0.5*dx), y=(2.8*dy), labels=paste("Unics: ", tot_ugenes, sep=""), cex=(1.1*t), col="black")
    
    #titres
    text(x=(2*dx), y=(3.6*dy), labels=listeA, cex=(1.1*t), col="blue")
    text(x=(2*dx), y=(0.4*dy), labels=listeB, cex=(1.1*t), col="red")
    text(x=(2*dx), y=(0.6*dy), labels=listeC, cex=(1.1*t), col="green")
    dev.off()
  }
  
  #graph 4
  graph_4<-function(path, listeA, listeB, listeC, listeD, nA, nB, nC, nD, nAB, nAC, nBD, nCD, nAD, nBC, nABC, nBCD, nACD, nABD, nABCD, tot_ugenes)
  {
    dx=4; dy=4; dd=6; t=1
    pdf(file = paste(path, "/venn_diagram.pdf", sep=""), width=10, height=10)
    plot.window(c(0, 20), c(0, 20))
    plot(x=1:(5*dx), y=1:(5*dy),type="n", axes=FALSE, xlab="",ylab="")
    symbols((2*dx), (1.8*dy), circle=(1*dd), add=TRUE, inches=TRUE, fg="orange")
    symbols((1.7*dx), (1.5*dy), circle=(1*dd), add=TRUE, inches=TRUE, fg="blue")
    symbols((2.3*dx), (1.5*dy), circle=(1*dd), add=TRUE, inches=TRUE, fg="red")
    symbols((2*dx), (1.2*dy), circle=(1*dd), add=TRUE, inches=TRUE, fg="green")
    
    symbols((1.8*dx), (3.2*dy), circle=(0.3*dd), add=TRUE, inches=TRUE, fg="blue")
    symbols((2.2*dx), (3.2*dy), circle=(0.3*dd), add=TRUE, inches=TRUE, fg="red")
    symbols((3.7*dx), (1.7*dy), circle=(0.3*dd), add=TRUE, inches=TRUE, fg="orange")
    symbols((3.7*dx), (1.3*dy), circle=(0.3*dd), add=TRUE, inches=TRUE, fg="green")
    
    text(x=(2*dx), y=(2.2*dy), labels=nA, cex=t, col="black")
    text(x=(1.3*dx), y=(1.5*dy), labels=nB, cex=t, col="black")
    text(x=(2.7*dx), y=(1.5*dy), labels=nC, cex=t, col="black")
    text(x=(2*dx), y=(0.75*dy), labels=nD, cex=t, col="black")
    
    text(x=(1.6*dx), y=(1.9*dy), labels=nAB, cex=t, col="black")
    text(x=(2.3*dx), y=(1.9*dy), labels=nAC, cex=t, col="black")
    text(x=(1.6*dx), y=(1.1*dy), labels=nBD, cex=t, col="black")
    text(x=(2.3*dx), y=(1.1*dy), labels=nCD, cex=t, col="black")
    
    text(x=(3.7*dx), y=(1.5*dy), labels=nAD, cex=t, col="black")    
    text(x=(2*dx), y=(3.2*dy), labels=nBC, cex=t, col="black")
    
    text(x=(2*dx), y=(1.9*dy), labels=nABC, cex=t, col="black")
    text(x=(2*dx), y=(1.1*dy), labels=nBCD, cex=t, col="black")
    text(x=(1.6*dx), y=(1.5*dy), labels=nACD, cex=t, col="black")
    text(x=(2.4*dx), y=(1.5*dy), labels=nABD, cex=t, col="black")
    
    text(x=(2*dx), y=(1.5*dy), labels=nABCD, cex=(1.3*t), col="black")
    
    text(x=(3.7*dx), y=(3.2*dy), labels=paste("Unics: ", tot_ugenes, sep=""), cex=t, col="black")
    
    #titres
    text(x=(2.5*dx), y=(4.9*dy), labels=listeA, cex=(1.3*t), col="orange")
    text(x=(2.5*dx), y=(4.7*dy), labels=listeB, cex=(1.3*t), col="blue")
    text(x=(2.5*dx), y=(4.5*dy), labels=listeC, cex=(1.3*t), col="red")
    text(x=(2.5*dx), y=(4.3*dy), labels=listeD, cex=(1.3*t), col="green")
    
    dev.off()
  }
  
  #graph 4 ud
  graph_4ud<-function(path, listeA, listeB, listeC, listeD, nA, nB, nC, nD, nAB, nAC, nBD, nCD, nAD, nBC, nABC, nBCD, nACD, nABD, nABCD, tot_ugenes, nAu, nAd, nBu, nBd, nCu, nCd, nDu, nDd, nABud, nACud, nBCud, nBDud, nCDud, nADud, nABCud, nBCDud, nABDud, nACDud, nABCDud)
  {
    dx=4; dy=4; dd=8; t=1
    pdf(file = paste(path, "/venn_diagram.pdf", sep=""), width=10, height=10)
    plot.window(c(0, 20), c(0, 20))
    plot(x=1:(5*dx), y=1:(5*dy),type="n", axes=FALSE, xlab="",ylab="")
    symbols((2*dx), (2.35*dy), circle=(1*dd), add=TRUE, inches=TRUE, fg="blue")  #A
    symbols((1.7*dx), (2.1*dy), circle=(1*dd), add=TRUE, inches=TRUE, fg="red") #B
    symbols((2.3*dx), (2.1*dy), circle=(1*dd), add=TRUE, inches=TRUE, fg="green") #C
    symbols((2*dx), (1.85*dy), circle=(1*dd), add=TRUE, inches=TRUE, fg="orange")  #D

    #AC
    symbols((4.2*dx), (3.9*dy), circle=(0.3*dd), add=TRUE, inches=TRUE, fg="red")
    symbols((4.4*dx), (3.9*dy), circle=(0.3*dd), add=TRUE, inches=TRUE, fg="green")
    #BD
    symbols((4.5*dx), (1.7*dy), circle=(0.3*dd), add=TRUE, inches=TRUE, fg="blue")
    symbols((4.5*dx), (1.5*dy), circle=(0.3*dd), add=TRUE, inches=TRUE, fg="orange")

    #ABCD
    symbols((0.75*dx), (4.5*dy), circle=(0.3*dd), add=TRUE, inches=TRUE, fg="red")
    symbols((0.85*dx), (4.5*dy), circle=(0.3*dd), add=TRUE, inches=TRUE, fg="green")
    symbols((0.8*dx), (4.55*dy), circle=(0.3*dd), add=TRUE, inches=TRUE, fg="blue")
    symbols((0.8*dx), (4.45*dy), circle=(0.3*dd), add=TRUE, inches=TRUE, fg="orange")

    #A
    text(x=(2*dx), y=(2.85*dy), labels=paste("A"), cex=t, col="blue")
    text(x=(2*dx), y=(2.75*dy), labels=paste(nA), cex=t, col="blue")
    text(x=(1.7*dx), y=(4.2*dy), labels=paste("A"), cex=t*1.2, col="blue")
    text(x=(1.7*dx), y=(4.1*dy), labels=paste("u:", nAu), cex=t, col="blue")
    text(x=(1.7*dx), y=(4*dy), labels=paste("d:", nAd), cex=t, col="blue")
    #B
    text(x=(1.3*dx), y=(2.1*dy), labels=paste("B\n", nB, sep=""), cex=t, col="red")
    text(x=(0.4*dx), y=(2.25*dy), labels=paste("B"), cex=t*1.2, col="red")
    text(x=(0.4*dx), y=(2.1*dy), labels=paste("u:", nBu), cex=t, col="red")
    text(x=(0.4*dx), y=(2*dy), labels=paste("d:", nBd), cex=t, col="red")
    #C
    text(x=(2.7*dx), y=(2.1*dy), labels=paste("C\n", nC, sep=""), cex=t*(1), col="green")
    text(x=(3.6*dx), y=(2.25*dy), labels=paste("C"), cex=t*1.2, col="green")
    text(x=(3.6*dx), y=(2.1*dy), labels=paste("u:", nCu), cex=t, col="green")
    text(x=(3.6*dx), y=(2*dy), labels=paste("d:", nCd), cex=t, col="green")
    #D
    text(x=(2*dx), y=(1.5*dy), labels=paste("D"), cex=t, col="orange")
    text(x=(2*dx), y=(1.4*dy), labels=paste(nD), cex=t, col="orange")
    text(x=(1.7*dx), y=(0.35*dy), labels=paste("D"), cex=t*1.2, col="orange")
    text(x=(1.7*dx), y=(0.25*dy), labels=paste("u:", nDu), cex=t, col="orange")
    text(x=(1.7*dx), y=(0.15*dy), labels=paste("d:", nDd), cex=t, col="orange")

    #AB
    text(x=(1.6*dx), y=(2.5*dy), labels=paste("AB"), cex=t, col="black")
    text(x=(1.6*dx), y=(2.4*dy), labels=paste(nAB), cex=t, col="black")
    text(x=(0.96*dx), y=(3.3*dy), labels=paste("A"), cex=t*1.2, col="blue")
    text(x=(1.04*dx), y=(3.3*dy), labels=paste("B"), cex=t*1.2, col="red")
    format_label(n=nAB, m=nABud, nom="AB", x=(1*dx), y=(3.3*dy), t, type=4)

    #AC
    text(x=(2.4*dx), y=(2.5*dy), labels=paste("AC"), cex=t, col="black")
    text(x=(2.4*dx), y=(2.4*dy), labels=paste(nAC), cex=t, col="black")
    text(x=(2.86*dx), y=(3.3*dy), labels=paste("A"), cex=t*1.2, col="blue")
    text(x=(2.94*dx), y=(3.3*dy), labels=paste("C"), cex=t*1.2, col="green")
    format_label(n=nAC, m=nACud, nom="AC", x=(2.9*dx), y=(3.3*dy), t, type=4)

    #BD
    text(x=(1.6*dx), y=(1.8*dy), labels=paste("BD"), cex=t, col="black")
    text(x=(1.6*dx), y=(1.7*dy), labels=paste(nBD), cex=t, col="black")
    text(x=(0.96*dx), y=(0.8*dy), labels=paste("B"), cex=t*1.2, col="red")
    text(x=(1.04*dx), y=(0.8*dy), labels=paste("D"), cex=t*1.2, col="orange")
    format_label(n=nBD, m=nBDud, nom="BD", x=(1*dx), y=(0.8*dy), t, type=4)

    #CD
    text(x=(2.4*dx), y=(1.8*dy), labels=paste("CD"), cex=t, col="black")
    text(x=(2.4*dx), y=(1.7*dy), labels=paste(nCD), cex=t, col="black")
    text(x=(2.86*dx), y=(0.8*dy), labels=paste("C"), cex=t*1.2, col="green")
    text(x=(2.94*dx), y=(0.8*dy), labels=paste("D"), cex=t*1.2, col="orange")
    format_label(n=nCD, m=nCDud, nom="CD", x=(2.9*dx), y=(0.8*dy), t, type=4)

    #AD
    text(x=(4.46*dx), y=(1.9*dy), labels=paste("A"), cex=t*1.2, col="blue")
    text(x=(4.54*dx), y=(1.9*dy), labels=paste("D"), cex=t*1.2, col="orange")
    text(x=(4.5*dx), y=(1.8*dy), labels=paste(nAD), cex=t, col="black")
    format_label(n=nAD, m=nADud, nom="AD", x=(4.5*dx), y=(1.7*dy), t, type=4)

    #BC
    text(x=(4.26*dx), y=(4.3*dy), labels=paste("B"), cex=t*1.2, col="red")
    text(x=(4.34*dx), y=(4.3*dy), labels=paste("C"), cex=t*1.2, col="green")
    text(x=(4.3*dx), y=(4.2*dy), labels=paste(nBC), cex=t, col="black")
    format_label(n=nBC, m=nBCud, nom="BC", x=(4.3*dx), y=(4.1*dy), t, type=4)

    #ABC
    text(x=(2*dx), y=(2.55*dy), labels=paste("ABC"), cex=t, col="black")
    text(x=(2*dx), y=(2.45*dy), labels=paste(nABC), cex=t, col="black")
    text(x=(2.22*dx), y=(3.8*dy), labels=paste("A"), cex=t*1.2, col="blue")
    text(x=(2.3*dx), y=(3.8*dy), labels=paste("B"), cex=t*1.2, col="red")
    text(x=(2.38*dx), y=(3.8*dy), labels=paste("C"), cex=t*1.2, col="green")
    format_label(n=nABC, m=nABCud, nom="ABC", x=(2.3*dx), y=(3.8*dy), t, type=4)

    #BCD
    text(x=(2*dx), y=(1.75*dy), labels=paste("BCD"), cex=t, col="black")
    text(x=(2*dx), y=(1.65*dy), labels=paste(nBCD), cex=t, col="black")
    text(x=(2.22*dx), y=(1*dy), labels=paste("B"), cex=t*1.2, col="red")
    text(x=(2.3*dx), y=(1*dy), labels=paste("C"), cex=t*1.2, col="green")
    text(x=(2.38*dx), y=(1*dy), labels=paste("D"), cex=t*1.2, col="orange")
    format_label(n=nBCD, m=nBCDud, nom="BCD", x=(2.3*dx), y=(1*dy), t, type=4)

    #ACD
    text(x=(2.4*dx), y=(2.15*dy), labels=paste("ACD"), cex=t, col="black")
    text(x=(2.4*dx), y=(2.05*dy), labels=paste(nACD), cex=t, col="black")
    text(x=(3.02*dx), y=(2.2*dy), labels=paste("A"), cex=t*1.2, col="blue")
    text(x=(3.1*dx), y=(2.2*dy), labels=paste("C"), cex=t*1.2, col="green")
    text(x=(3.18*dx), y=(2.2*dy), labels=paste("D"), cex=t*1.2, col="orange")
    format_label(n=nACD, m=nACDud, nom="ACD", x=(3.1*dx), y=(2.2*dy), t, type=4)

    #ABD
    text(x=(1.6*dx), y=(2.15*dy), labels=paste("ABD"), cex=t, col="black")
    text(x=(1.6*dx), y=(2.05*dy), labels=paste(nABD), cex=t, col="black")
    text(x=(0.77*dx), y=(2.2*dy), labels=paste("A"), cex=t*1.2, col="blue")
    text(x=(0.85*dx), y=(2.2*dy), labels=paste("B"), cex=t*1.2, col="red")
    text(x=(0.94*dx), y=(2.2*dy), labels=paste("D"), cex=t*1.2, col="orange")
    format_label(n=nABD, m=nABDud, nom="ABD", x=(0.9*dx), y=(2.2*dy), t, type=4)

    #ABCD
    text(x=(0.68*dx), y=(4.9*dy), labels=paste("A"), cex=t*1.2, col="blue")
    text(x=(0.76*dx), y=(4.9*dy), labels=paste("B"), cex=t*1.2, col="red")
    text(x=(0.84*dx), y=(4.9*dy), labels=paste("C"), cex=t*1.2, col="green")
    text(x=(0.92*dx), y=(4.9*dy), labels=paste("D"), cex=t*1.2, col="orange")
    text(x=(2*dx), y=(2.1*dy), labels=paste("ABCD"), cex=t*1.2, col="black")
    text(x=(2*dx), y=(2*dy), labels=paste(nABCD), cex=t, col="black")
    format_label(n=nABCD, m=nABCDud, nom="ABCD", x=(0.8*dx), y=(4.9*dy), t, type=4)

    text(x=(3.7*dx), y=(3.2*dy), labels=paste("Unics: ", tot_ugenes, sep=""), cex=t, col="black")

    #titres
    text(x=(2.5*dx), y=(5.1*dy), labels=paste("A: ", listeA, sep=""), cex=(1.3*t), col="blue")
    text(x=(2.5*dx), y=(4.95*dy), labels=paste("B: ", listeB, sep=""), cex=(1.3*t), col="red")
    text(x=(2.5*dx), y=(4.80*dy), labels=paste("C: ", listeC, sep=""), cex=(1.3*t), col="green")
    text(x=(2.5*dx), y=(4.65*dy), labels=paste("D: ", listeD, sep=""), cex=(1.3*t), col="orange")

    dev.off()
  }
  
  ########################################################################################################
  ########################################################################################################  

 
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
     
     graph_2(path, listeA, listeB, nA, nB, nAB, tot_ugenes)
     
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
        
        graph_2ud(path, listeA, listeB, nA, nB, nAB, tot_ugenes, nAu, nAd, nBu, nBd, nABud)
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
     
     graph_3(path, listeA, listeB, listeC, nA, nB, nC, nAB, nAC, nBC, nABC, tot_ugenes)
     
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
        
        graph_3ud(path, listeA, listeB, listeC, nA, nB, nC, nAB, nAC, nBC, nABC, tot_ugenes, nAu, nAd, nBu, nBd, nCu, nCd, nABud, nACud, nBCud, nABCud)
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
     
     graph_4(path, listeA, listeB, listeC, listeD, nA, nB, nC, nD, nAB, nAC, nBD, nCD, nAD, nBC, nABC, nBCD, nACD, nABD, nABCD, tot_ugenes)
     
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
        
        graph_4ud(path, listeA, listeB, listeC, listeD, nA, nB, nC, nD, nAB, nAC, nBD, nCD, nAD, nBC, nABC, nBCD, nACD, nABD, nABCD, tot_ugenes, nAu, nAd, nBu, nBd, nCu, nCd, nDu, nDd, nABud, nACud, nBCud, nBDud, nCDud, nADud, nABCud, nBCDud, nABDud, nACDud, nABCDud)
     }
  }
}
