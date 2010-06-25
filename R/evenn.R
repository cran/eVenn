evenn <-
function(annot=FALSE, path_res="", path_lists="", res="")
{  
  write("\t#############################################################################\n")
  write("\t#                                                                           #\n")
  write("\t#                                eVenn (v1.01                               #\n")
  write("\t#                                                                           #\n")
  write("\t#############################################################################\n")
  
  ################################################################################
  #graph 2
  graph_2<-function(path, listeA, listeB, nA, nB, nAB)
  {
    library(plotrix)
    dx=5; dy=5; dd=4.8; t=1
    pdf(file = paste(path, "/venn_diagram.pdf", sep=""))
    plot(x=1:(4*dx), y=1:(4*dy),type="n", axes=FALSE, xlab="",ylab="")
    
    draw.circle((1.5*dx),(1.5*dy),(1*dd),border="blue",lty=1,lwd=1)
    draw.circle((2.5*dx),(1.5*dy),(1*dd),border="red",lty=1,lwd=1)
    
    text(x=(1.1*dx), y=(1.5*dy), labels=nA, cex=t, col="black")
    text(x=(2.9*dx), y=(1.5*dy), labels=nB, cex=t, col="black")
    
    text(x=(2*dx), y=(1.5*dy), labels=nAB, cex=t, col="black")
    
    #titres
    text(x=(2*dx), y=(0.2*dy), labels=listeA, cex=(1.1*t), col="blue")
    text(x=(2*dx), y=(2.7*dy), labels=listeB, cex=(1.1*t), col="red")
    dev.off()
  }
  
  #graph 3
  graph_3<-function(path, listeA, listeB, listeC, nA, nB, nC, nAB, nAC, nBC, nABC)
  {
    library(plotrix)
    dx=5; dy=5; dd=4.8; t=1
    pdf(file = paste(path, "/venn_diagram.pdf", sep=""))
    plot(x=1:(4*dx), y=1:(4*dy),type="n", axes=FALSE, xlab="",ylab="")
     
    draw.circle((2*dx),(2.6*dy),(1*dd),border="green",lty=1,lwd=1)
    draw.circle((1.5*dx),(1.6*dy),(1*dd),border="blue",lty=1,lwd=1)
    draw.circle((2.5*dx),(1.6*dy),(1*dd),border="red",lty=1,lwd=1)
    
    text(x=(2*dx), y=(3.1*dy), labels=nA, cex=t, col="black")
    text(x=(1*dx), y=(1.4*dy), labels=nB, cex=t, col="black")
    text(x=(3*dx), y=(1.4*dy), labels=nC, cex=t, col="black")
    
    text(x=(1.4*dx), y=(2.3*dy), labels=nAB, cex=t, col="black")
    text(x=(2.6*dx), y=(2.3*dy), labels=nAC, cex=t, col="black")
    text(x=(2*dx), y=(1.1*dy), labels=nBC, cex=t, col="black")
    
    text(x=(2*dx), y=(2*dy), labels=nABC, cex=t, col="black")
    
    #titres
    text(x=(2*dx), y=(3.9*dy), labels=listeA, cex=(1.1*t), col="green")
    text(x=(2*dx), y=(0.2*dy), labels=listeB, cex=(1.1*t), col="blue")
    text(x=(2*dx), y=(0.4*dy), labels=listeC, cex=(1.1*t), col="red")
    dev.off()
  }
  
  #graph 4
  graph_4<-function(path, listeA, listeB, listeC, listeD, nA, nB, nC, nD, nAB, nAC, nBD, nCD, nAD, nBC, nABC, nBCD, nACD, nABD, nABCD)
  {
    library(plotrix)
    dx=4; dy=4; dd=5; t=0.6
    pdf(file = paste(path, "/venn_diagram.pdf", sep=""))
    plot(x=1:(5*dx), y=1:(5*dy),type="n", axes=FALSE, xlab="",ylab="")
    draw.circle((2*dx),(2.5*dy),(1*dd),border="yellow",lty=1,lwd=1)
    draw.circle((1.5*dx),(2*dy),(1*dd),border="blue",lty=1,lwd=1)
    draw.circle((2.5*dx),(2*dy),(1*dd),border="red",lty=1,lwd=1)
    draw.circle((2*dx),(1.5*dy),(1*dd),border="green",lty=1,lwd=1)
    
    draw.circle((1*dx),(4.5*dy),(0.3*dd),border="blue",lty=1,lwd=1)
    draw.circle((1.2*dx),(4.5*dy),(0.3*dd),border="red",lty=1,lwd=1)
    draw.circle((4.5*dx),(2.1*dy),(0.3*dd),border="yellow",lty=1,lwd=1)
    draw.circle((4.5*dx),(1.9*dy),(0.3*dd),border="green",lty=1,lwd=1)
    
    text(x=(2*dx), y=(3.6*dy), labels=nA, cex=t, col="black")
    text(x=(0.5*dx), y=(2*dy), labels=nB, cex=t, col="black")
    text(x=(3.5*dx), y=(2*dy), labels=nC, cex=t, col="black")
    text(x=(2*dx), y=(0.4*dy), labels=nD, cex=t, col="black")
    
    text(x=(1.2*dx), y=(2.9*dy), labels=nAB, cex=t, col="black")
    text(x=(2.8*dx), y=(2.9*dy), labels=nAC, cex=t, col="black")
    text(x=(1.2*dx), y=(1.1*dy), labels=nBD, cex=t, col="black")
    text(x=(2.8*dx), y=(1.1*dy), labels=nCD, cex=t, col="black")
    
    text(x=(1.1*dx), y=(4.5*dy), labels=nAD, cex=t, col="black")
    text(x=(4.5*dx), y=(2*dy), labels=nBC, cex=t, col="black")
    
    text(x=(2*dx), y=(3.05*dy), labels=nABC, cex=t, col="black")
    text(x=(2*dx), y=(0.95*dy), labels=nBCD, cex=t, col="black")
    text(x=(2.95*dx), y=(2*dy), labels=nACD, cex=t, col="black")
    text(x=(1.05*dx), y=(2*dy), labels=nABD, cex=t, col="black")
    
    text(x=(2*dx), y=(2*dy), labels=nABCD, cex=t, col="black")
    
    #titres
    text(x=(3.5*dx), y=(4.8*dy), labels=listeA, cex=(1.3*t), col="yellow")
    text(x=(3.5*dx), y=(4.6*dy), labels=listeB, cex=(1.3*t), col="blue")
    text(x=(3.5*dx), y=(4.4*dy), labels=listeC, cex=(1.3*t), col="red")
    text(x=(3.5*dx), y=(4.2*dy), labels=listeD, cex=(1.3*t), col="green")
    
    dev.off()
  }
  ################################################################################
  
  #test des formats des listes
  test_list<-function(liste)
  {
    ext = substr(basename(liste), (nchar(basename(liste))-2), nchar(basename(liste)))
    data_t=""
    if(exists("data_t"))  rm("data_t")
    if(ext == "txt") data_t = read.table(file=liste, header=TRUE, row.names=1, sep="\t")
    if(ext == "csv") data_t = try(read.table(file=liste, header=TRUE, row.names=1, sep=","), silent=TRUE)
    if((ext == "csv")&(class(data_t)=="try-error")) data_t = try(read.table(file=liste, header=TRUE, row.names=1, sep=";"), silent=TRUE)
    if((ext!="csv")&(ext!="txt")) 
    {
      write("The file format is not supported (must be txt/tab or csv/,;)", file="")
      break;
    }
    data_t = as.matrix(data_t)
    return(data_t)
  }

  
  if(path_res == "")
  {
    path_res = getwd()
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
  if((path_lists == "")&(os=="Windows"))
  {
     write(paste("Choose the directory where are placed the lists" , sep=""), file="")
     path_lists = choose.dir()
  } 
   

  if(!is.matrix(res))
  {
    listes = list.files(path = path_lists, full.names = TRUE)    
    data_t = test_list(liste=listes[1])
    data_t = rownames(data_t)
    res = matrix(1, ncol=1, nrow=length(data_t))
    rownames(res) = data_t
    noms_listes = substr(basename(listes[1]), 0, (nchar(basename(listes[1]))-4))
    
    for(i in 2:length(listes))
    {   
      data_t = test_list(listes[i])
      data_t = rownames(data_t)
    
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
      
      #competion de la matrice res
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

    #ajout des datas de chaque liste
    #res = matrice des appartenances
    noms = matrix(0, ncol=2, nrow=nrow(res))#matrice des noms/genes
    #on ne sait pas combien de colonnes d'info en plus des noms/genes
    data_all = res
    for(M in 1:length(listes)) #liste par liste
    {
      #lecture du fichier
      data_t = test_list(listes[M])
      data_liste = matrix("", ncol=(ncol(data_t)-2), nrow=0)
      for(N in 1:nrow(res)) #parcours l'ensembe des sondes
      {                       
        if(res[N, substr(basename(listes[M]), 0, (nchar(basename(listes[M]))-4))] == 1)  #si la sonde appartient a la liste en cours
        {
           #on recupere ses datas dans le bon fichier
           data_liste = rbind(data_liste, t(as.matrix(data_t[rownames(data_t)==rownames(res)[N], 3:ncol(data_t)])))
           noms[N,] = data_t[rownames(data_t)==rownames(res)[N], 1:2]
        }else{  #ajoute une ligne vide
           data_liste = rbind(data_liste, matrix("", ncol=ncol(data_liste), nrow=1))
        }     
      }
      data_all = cbind(data_all, matrix("", ncol=1, nrow=nrow(data_all)), data_liste)        
    }
    #ajoute la liste complete des noms
    colnames(noms) = colnames(data_t)[1:2]
    data_all = cbind(data_all[,1:ncol(res)], noms, data_all[,(ncol(res)+1):ncol(data_all)])
    write.table(data_all, file = paste(path, "/venn_annot.txt", sep=""), sep="\t")
  }else{
    noms_listes = colnames(res)
  }
  write.csv2(res, row.names = TRUE, file = paste(path, "/venn_matrix.csv", sep=""))
  
  #graphs
  if(length(noms_listes)==2)
  {
     #calcul des sommes
     res = cbind(res, matrix(apply(res, 1, function(x) sum(x))))
     colnames(res)[ncol(res)] = "sum"
     nA = nrow(res[(res[,"sum"]==1)&(res[,1]==1),])
     nB = nrow(res[(res[,"sum"]==1)&(res[,2]==1),])
     nAB = nrow(res[(res[,"sum"]==2)&(res[,1]==1)&(res[,2]==1),])
     
     listeA = noms_listes[1]
     listeB = noms_listes[2]
     
     graph_2(path, listeA, listeB, nA, nB, nAB)
  }
  
  if(length(noms_listes)==3)
  {
     #calcul des sommes
     res = cbind(res, matrix(apply(res, 1, function(x) sum(x))))
     colnames(res)[ncol(res)] = "sum"
     nA = nrow(res[(res[,"sum"]==1)&(res[,1]==1),])
     nB = nrow(res[(res[,"sum"]==1)&(res[,2]==1),])
     nC = nrow(res[(res[,"sum"]==1)&(res[,3]==1),])
     nAB = nrow(res[(res[,"sum"]==2)&(res[,1]==1)&(res[,2]==1),])
     nAC = nrow(res[(res[,"sum"]==2)&(res[,1]==1)&(res[,3]==1),])
     nBC = nrow(res[(res[,"sum"]==2)&(res[,2]==1)&(res[,3]==1),])
     nABC = nrow(res[res[,"sum"]==3,])
     
     listeA = noms_listes[1]
     listeB = noms_listes[2]
     listeC = noms_listes[3]
     
     graph_3(path, listeA, listeB, listeC, nA, nB, nC, nAB, nAC, nBC, nABC)
  }
  
  if(length(noms_listes)==4)
  {
     #calcul des sommes
     res = cbind(res, matrix(apply(res, 1, function(x) sum(x))))
     colnames(res)[ncol(res)] = "sum"
     nA = nrow(res[(res[,"sum"]==1)&(res[,1]==1),])
     nB = nrow(res[(res[,"sum"]==1)&(res[,2]==1),])
     nC = nrow(res[(res[,"sum"]==1)&(res[,3]==1),])
     nD = nrow(res[(res[,"sum"]==1)&(res[,4]==1),])
     
     nAB = nrow(res[(res[,"sum"]==2)&(res[,1]==1)&(res[,2]==1),])
     nAC = nrow(res[(res[,"sum"]==2)&(res[,1]==1)&(res[,3]==1),])
     nBD = nrow(res[(res[,"sum"]==2)&(res[,2]==1)&(res[,4]==1),])
     nCD = nrow(res[(res[,"sum"]==2)&(res[,3]==1)&(res[,4]==1),])
     nAD = nrow(res[(res[,"sum"]==2)&(res[,1]==1)&(res[,4]==1),])
     nBC = nrow(res[(res[,"sum"]==2)&(res[,2]==1)&(res[,3]==1),])
     
     nABC = nrow(res[(res[,"sum"]==3)&(res[,1]==1)&(res[,2]==1)&(res[,3]==1),])
     nBCD = nrow(res[(res[,"sum"]==3)&(res[,2]==1)&(res[,3]==1)&(res[,4]==1),])
     nACD = nrow(res[(res[,"sum"]==3)&(res[,1]==1)&(res[,3]==1)&(res[,4]==1),])
     nABD = nrow(res[(res[,"sum"]==3)&(res[,1]==1)&(res[,2]==1)&(res[,4]==1),])
     
     nABCD = nrow(res[res[,"sum"]==4,])
     
     listeA = noms_listes[1]
     listeB = noms_listes[2]
     listeC = noms_listes[3]
     listeD = noms_listes[4]
     
     try(graph_4(path, listeA, listeB, listeC, listeD, nA, nB, nC, nD, nAB, nAC, nBD, nCD, nAD, nBC, nABC, nBCD, nACD, nABD, nABCD), silent=TRUE)
  }
}

