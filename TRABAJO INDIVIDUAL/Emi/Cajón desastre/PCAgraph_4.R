### Albera PCA graphics ###

setwd("C:/Users/enava/OneDrive - Universitat Autònoma de Barcelona/PRACTICAS/Replica Paper/4. PCA")

library(readxl) #import from Excel
library(dplyr)  #to manipulate tables


#Pruning SNPs in strong LD 

system("plink --file snpAlb-QC --cow --indep-pairwise 50 5 0.3")

system("plink --file snpAlb-QC --cow --extract plink.prune.in --recode --out snpAlbPruned")

#PCA in PLINK

system("plink --file snpAlbPruned --cow --pca 5 header --out pca_results_Pl")


#Importing data to represent PCA

eigenvPl <- read.table("pca_results_Pl.eigenvec", header=TRUE)
groups4 <- read_excel("InfoSamplesAlbera_BP_Group.xlsx")

str(eigenvPl)
str(groups4)


#Eliminating FID from PCA eigenvector file

eigenvPl <- select(eigenvPl, -FID)

#Eliminating .cel from IID in PCA eigenvector file

library(stringr)
eigenvPl$IID <- str_sub(eigenvPl$IID, end=-5)
dim(eigenvPl)

#Comparing the two ID
setdiff(groups4$IID, eigenvPl$IID) 

#Merging the two files

dataGraphic4 <- merge(groups4, eigenvPl, by = "IID") 
str(dataGraphic4)

#Making plots for 4 groups

library(ggplot2)

ggplot(data = dataGraphic4, mapping = aes(x = PC1, y = PC2, shape = Group, color = Group)) + geom_point() +
geom_hline(yintercept = 0, linetype="dotted") + geom_vline(xintercept = 0, linetype="dotted") + theme_test() +
  theme(legend.title = element_blank()) +
  theme(legend.position = c(0.9,0.9),
        legend.direction = "vertical") 

ggplot(data = dataGraphic4, mapping = aes(x = PC1, y = PC3, shape = Group, color = Group)) + geom_point() +
geom_hline(yintercept = 0, linetype="dotted") + geom_vline(xintercept = 0, linetype="dotted") + theme_test() +
  theme(legend.title = element_blank()) +
  theme(legend.position = c(0.05,0.92),
        legend.direction = "vertical") 

ggplot(data = dataGraphic4, mapping = aes(x = PC2, y = PC3, shape = Group, color = Group)) + geom_point() +
geom_hline(yintercept = 0, linetype="dotted") + geom_vline(xintercept = 0, linetype="dotted") + theme_test() +
  theme(legend.title = element_blank()) +
  theme(legend.position = c(0.05,0.92),
        legend.direction = "vertical") 