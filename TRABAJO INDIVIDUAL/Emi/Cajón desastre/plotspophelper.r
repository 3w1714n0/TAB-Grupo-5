#Clean workspace
rm(list= ls())

#Load pophelper
if (!require("pophelper")) {
  install.packages("pophelper", dependencies = TRUE)
  library("pophelper")
}

#Set the working directory
setwd("C:/Users/enava/OneDrive - Universitat Autònoma de Barcelona/PRACTICAS/Replica Paper/5. Structure/admixture.Q")

#File is located in the current working directory
files <- c("alberaDef.3.Q", "alberaDef.4.Q", "alberaDef.5.Q", "alberaDef.6.Q")

list<-readQ(files=files)

#Basic usage for summarise the Admixture run
tr1 <- tabulateQ(qlist=list); tr1
sr1 <- summariseQ(tr1); sr1

#Load gridExtra
if (!require("gridExtra")) {
  install.packages("gridExtra", dependencies = TRUE)
  library("gridExtra")
}

#Load the group  and ID information from a txt file made with awk
indTable <- read.table("C:/Users/enava/OneDrive/PRACTICAS/Replica Paper/5. Structure/grupos.txt", col.names = c("Pop", "ID"))

#Take only the group information
onelabset <- indTable[,1,drop=FALSE]

####Palettes for the plot (NOT NEEDED)####

clist <- list(
  "shiny"=c("#1D72F5","#DF0101","#77CE61", "#FF9326","#A945FF","#0089B2","#FDF060","#FFA6B2","#BFF217","#60D5FD","#CC1577","#F2B950","#7FB21D","#EC496F","#326397","#B26314","#027368","#A4A4A4","#610B5E"),
  "strong"=c("#11A4C8","#63C2C5","#1D4F9F","#0C516D","#2A2771","#396D35","#80C342","#725DA8","#B62025","#ED2224","#ED1943","#ED3995","#7E277C","#F7EC16","#F8941E","#8C2A1C","#808080"),
  "oceanfive"=c("#00A0B0", "#6A4A3C", "#CC333F", "#EB6841", "#EDC951"),
  "keeled"=c("#48B098", "#91CB62", "#FFEE3B", "#FB9013", "#FF3C28"),
  "vintage"=c("#400F13", "#027368", "#A3BF3F", "#F2B950", "#D93A2B"),
  "muted"=c("#46BDDD","#82DDCE","#F5F06A","#F5CC6A","#F57E6A"),
  "teal"=c("#CFF09E","#A8DBA8","#79BD9A","#3B8686","#0B486B"),
  "merry"=c("#5BC0EB","#FDE74C","#9BC53D","#E55934","#FA7921"),
  "funky"=c("#A6CEE3", "#3F8EAA", "#79C360", "#E52829", "#FDB762","#ED8F47","#9471B4"),
  "retro"=c("#01948E","#A9C4E2","#E23560","#01A7B3","#FDA963","#323665","#EC687D"),
  "cb_paired"=c("#A6CEE3","#1F78B4","#B2DF8A","#33A02C","#FB9A99","#E31A1C","#FDBF6F","#FF7F00","#CAB2D6","#6A3D9A","#FFFF99","#B15928"),
  "cb_set3"=c("#8DD3C7","#FFFFB3","#BEBADA","#FB8072","#80B1D3","#FDB462","#B3DE69","#FCCDE5","#D9D9D9","#BC80BD","#CCEBC5","#FFED6F"),
  "morris"=c("#4D94CC","#34648A","#8B658A","#9ACD32","#CC95CC","#9ACD32","#8B3A39","#CD6601","#CC5C5B","#8A4500"),
  "wong"=c("#000000","#E69F00","#56B4E9","#009E73","#F0E442","#006699","#D55E00","#CC79A7"),
  "krzywinski"=c("#006E82","#8214A0","#005AC8","#00A0FA","#FA78FA","#14D2DC","#AA0A3C","#FA7850","#0AB45A","#F0F032","#A0FA82","#FAE6BE"))

#Add length of palettes
lengths <- sapply(clist,length)
names(clist) <- paste0(names(clist),"_",lengths)

par(mar=c(0.2,6,0.2,0))
par(mfrow=c(length(clist),1))

#Plot all the palettes
for(i in 1:length(clist))
{
  {barplot(rep(1,max(lengths)),col=c(clist[[i]],rep("white",max(lengths)-length(clist[[i]]))),axes=F,border=F)
    text(x=-0.1,y=0.5,adj=1,label=names(clist)[i],xpd=T,cex=1.2)}
}

####Plot the Admixture result####

#AlignK: switch the clusters colors for make them match 
list <- alignK(list[1:4])

#Plot and save it in the working directory
plotQ(list[1:4],imgoutput="join",showindlab=F,grplab=onelabset,
      ordergrp=T,showtitle=T,showsubtitle=T, titlelab= "", subtitlelab = "",
      height=1.6,indlabspacer=-1, showdiv = T, divsize = 1, divtype = 1,barbordersize=0,
      sppos="left",splabcol="black",spbgcol="#A4A4A4",splab=c("K3","K4","K5","K6"),clustercol=clist$shiny,
      outputfilename="admixtureAlbera_4",imgtype="png",
      exportpath=getwd())
