#Clean workspace
rm(list= ls())

#Set the working directory
setwd("C:/Users/enava/OneDrive - Universitat Autònoma de Barcelona/CUARTO/1 CUATRI/Plantas/LAB")

library(readxl) #import from Excel
library(ggpubr)
library(ggplot2)

#Importing data to make the plot
info <- read_xlsx("Dataset.xlsx",col_names = c("Mutante","Longitud", "Condición", "Gen"))

#placa2 <- read_xlsx("Dataset.xlsx")

info <- as.data.frame(info)
head(info)

my_comparisons <- list( c("hfr1_W", "hfr1_W+FR"), c("pif7_W", "pif7_W+FR"))
plot <- ggplot(info, mapping = aes(x = Mutante, y = Longitud, fill = Condición, ymax= 2.5)) + 
  labs(y = "Longitud hipocotilo normalizada sobre wt (mm)") +
  theme_light() + 
  geom_boxplot(notch = TRUE, notchwidth = 0.75) +
  scale_fill_manual(values = c("#88CCEE", "#6699CC")) +
  stat_compare_means(label = "p.signif", comparisons = my_comparisons, method = "t.test")


plot

ggsave(filename = "plot.png", plot = plot)
