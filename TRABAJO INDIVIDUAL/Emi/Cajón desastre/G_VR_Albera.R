#Clean workspace
rm(list= ls())

#Set the working directory
setwd("C:/Users/enava/OneDrive - Universitat Autònoma de Barcelona/PRACTICAS/Replica Paper/6. Van Raden")

#Additive matrix in PLINK ####

#Reading Albera1

system("plink --file snpAlbera1c_r --cow --allow-extra-chr -autosome --no-fid --no-parents --no-sex --allow-no-sex --no-pheno --hwe 1e-50 --recode --out snpAlb1rP")

#Reading Albera2 without Bruna Pirineus

system("plink --file snpAlbera2c_r_nBP --cow --allow-extra-chr -autosome --no-fid --no-parents --no-sex --allow-no-sex --no-pheno --hwe 1e-50 --recode --out snpAlb2rP")

#Merging both files

system("plink --file snpAlb1rP --cow --merge snpAlb2rP.ped snpAlb2rP.map --recode --out snpAlbT-r_nBP")

#Additive coefficients (M = gene dosage: 0, 1, 2)

system("plink --file snpAlbT-r_nBP --cow --recode A --out MAlb")

#Reading Plink M file into R ####

#Load data.table, to read tables, more flexible
if (!require("data.table")) {
  install.packages("data.table", dependencies = TRUE)
  library("data.table")
}

MAlb <- fread("MAlb.raw")
str(MAlb)

#Vector of animal's IID

animal <- MAlb$IID

#Eliminating .cel from animal

#Load stringr
if (!require("stringr")) {
  install.packages("stringr", dependencies = TRUE)
  library("stringr")
}
animal <- str_sub(animal, end=-5)

#Removing first 6 columns to get M

#Load dplyr, to manipulate tables
if (!require("dplyr")) {
  install.packages("dplyr", dependencies = TRUE)
  library("dplyr")
}
MAlb <- select(MAlb, -FID, -IID, -PAT, -MAT, -SEX, -PHENOTYPE)

#Extracting SNP names

SNP <- colnames(MAlb)
length(SNP)

#Converting the dataframe into a matrix

MAlb <- as.matrix(MAlb)

#Renaming rows (animal) and columns (SNP)

dimnames(MAlb) <- list(animal, SNP)
str(MAlb)

#Computing VanRaden G matrix####

#Load stringr
if (!require("AGHmatrix")) {
  install.packages("AGHmatrix", dependencies = TRUE)
  library("AGHmatrix")
}

#Generate the Gmatrix by the VanRaden method and the correct parameters
GVR_Albera <- Gmatrix(SNPmatrix=MAlb, missingValue="NA", maf=0.01, method="VanRaden")

#Some check statistics ####
dim(GVR_Albera)

GVR_Albera[1:8,1:8]

det(GVR_Albera)

#Self relationship

selfrel <- diag(GVR_Albera)
length(selfrel)
mean(selfrel)
sd(selfrel)
hist(selfrel)

#Some summary statistics

valuesG <- GVR_Albera[upper.tri(GVR_Albera, diag = TRUE)]

length(valuesG)  #(781*780)/2 + 781
mean(valuesG)
sd(valuesG)
length(which(valuesG<0))
length(which(valuesG>=0))
hist(valuesG, breaks=100)


#Find the relationship between two given animals####

#Create the function
relationship<-function(Gmatrix, ID1, ID2){ #matrix and IDs as arguments
  
  #IDs as chr not as int
  ID1 <- as.character(ID1)
  ID2 <- as.character(ID2)
  
  
  #Find the kinship between the animals
  kinship <- Gmatrix[ID1, ID2]
  
  #Strait kinship warning messages
  if (kinship >= 0.45 & kinship < 0.55) {
  
      warning("This two animals can be descendent and parent or siblings")
    
  } else if (kinship >= 0.2 & kinship < 0.4){
    
    warning("This two animals can be half-siblings")
    
  } else if (kinship > 0.8 & kinship < 1) {
    
    warning("This two IDs can come from the same animal sample")
    
  } else if (kinship > 1) {
    
    warning("This two IDs can be of two animal with consanguinity")
    
  } else if (kinship < 0.2) {
    
    warning("This two animals can present negative genotypic correlation")
    
  }
  
  return(kinship)
  
}

#Run it 
relationship(GVR_Albera, 80, 4284)

#Van Raden Matrix Heatmap ####

#Load the group  and ID information from a txt file made with awk
indTable <- read.table("C:/Users/enava/OneDrive/PRACTICAS/Replica Paper/5. Structure/grupos.txt", col.names = c("Pop", "ID"))

#Change the full population name to an abbreviation
largePop <- c("Cat-Albera", "Cat-notAlbera", "Fra-Albera", "Fra-notAlbera")
shortPop <- c("CA", "CN", "FA", "FN")

for (i in 1:length(largePop)){
 
   indTable[indTable == largePop[i]] <- shortPop[i]
   
   }

#Combine the ID with the population 
for (i in 1:length(indTable$Pop)){
  PopID<-paste0(indTable$Pop,indTable$ID)
}

#Change the row and column names to the PopID
rownames(GVR_Albera) =colnames(GVR_Albera) <- PopID

#Order by population
GVR_Albera <- GVR_Albera[order(rownames(GVR_Albera)), order(colnames(GVR_Albera))]


library(RColorBrewer)#for create a color palette

#Assign the groups and the color
my_group <- as.numeric(as.factor(substr(rownames(GVR_Albera), 1 , 2)))
colSide <- brewer.pal(4, "Set3")[my_group]

#Plot
heatmap(GVR_Albera, Colv = NA, Rowv = NA, scale="column", col = colorRampPalette(brewer.pal(3, "BuGn"))(26),
        RowSideColors=colSide, ColSideColors = colSide)

#Population statistics####

#Select the Catalonia and France Albera individuals
CatAlb<-GVR_Albera[1:372,1:372]
FrAlb<-GVR_Albera[622:705,622:705]

#Select the Albera individuals in two steps, eliminating the Cat-notAlbera individuals
Alb<-GVR_Albera[1:705, 1:705]
Alb<-Alb[-(373:621),-(373:621)]

#CatAlbera's statistics####
dim(CatAlb)

CatAlb[1:8,1:8]

det(CatAlb)

#Self relationship

CAselfrel <- diag(CatAlb)
length(CAselfrel)
mean(CAselfrel)
sd(CAselfrel)
hist(CAselfrel)

#Some summary statistics

CAvaluesG <- CatAlb[upper.tri(CatAlb, diag = TRUE)]

length(CAvaluesG)  #(372*371)/2 + 372
mean(CAvaluesG)
sd(CAvaluesG)
length(which(CAvaluesG<0))
length(which(CAvaluesG>=0))
hist(CAvaluesG, breaks=100)

#FraAlbera's statistics####
dim(FrAlb)

FrAlb[1:8,1:8]

det(FrAlb)

#Self relationship

FAselfrel <- diag(FrAlb)
length(FAselfrel)
mean(FAselfrel)
sd(FAselfrel)
hist(FAselfrel)

#Some summary statistics

FAvaluesG <- FrAlb[upper.tri(FrAlb, diag = TRUE)]

length(FAvaluesG)  #(372*371)/2 + 372
mean(FAvaluesG)
sd(FAvaluesG)
length(which(FAvaluesG<0))
length(which(FAvaluesG>=0))
hist(FAvaluesG, breaks=100)

#Albera's statistics####
dim(Alb)

Alb[1:8,1:8]

det(Alb)

#Self relationship

Aselfrel <- diag(Alb)
length(Aselfrel)
mean(Aselfrel)
sd(Aselfrel)
hist(Aselfrel)

#Some summary statistics

AvaluesG <- Alb[upper.tri(Alb, diag = TRUE)]

length(AvaluesG)  #(372*371)/2 + 372
mean(AvaluesG)
sd(AvaluesG)
length(which(AvaluesG<0))
length(which(AvaluesG>=0))
hist(AvaluesG, breaks=100)
