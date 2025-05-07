
install.packages("meta")

library(meta)

getwd()
setwd("C:/Users/AB889933/Desktop/Meta_ale")

author   <- c("Liu-Ambrose", "Cavalcante","Maki")
year     <- c(2010, 2020, 2012)
N_TRT   <- c(52,23,75)
MU_TRT <- c(0.3,0.01,-0.1)
SD_TRT  <- c(0.2,0.04,0.3)
N_CON   <- c(49,22,75)
MU_CON <- c(0.2,0,-0.2)
SD_CON  <- c(0.2,0.04,0.4)

GS <- data.frame(author, year, N_TRT, MU_TRT, SD_TRT, N_CON, MU_CON, SD_CON)

# to delete 
rm(author, year, N_TRT, MU_TRT, SD_TRT, N_CON, MU_CON, SD_CON)                

MD  <- GS$MU_TRT - GS$MU_CON
MD

seMD <- sqrt(GS$SD_TRT^2/GS$N_TRT + GS$SD_CON^2/GS$N_CON)
seMD

MD
round(MD - 1.96 * seMD, 2)
round(MD + 1.96 * seMD, 2)            

res_GS <- metacont(n.e = N_TRT, mean.e = MU_TRT, sd.e = SD_TRT,
                     n.c = N_CON, mean.c = MU_CON, sd.c = SD_CON,sm = "SMD",
                     studlab = paste(author, "et al.", year), data = GS)
res_GS
summary(res_GS)

# Quantifying heterogeneity 

metainf(res_GS)

# Forest plot 

forest(res_GS)

forest_GS <- forest(res_GS, layout = "RevMan5", common = FALSE,
                      label.left = "Favours control", col.label.left = "red",
                      label.right = "Favours experimental", col.label.right = "green",
                      prediction = FALSE, xlim = c(-0.3,0.8))

# Il test per l'eterogeneità rifiuta che c'è omogeneità ma dato che l'indice I^2 
# è del 70% si può ignorare la presenza di eterogeneità 

funnel_GS <- funnel(res_GS)

# trim-and-fill method for estimating end adjusting for the number and outcomes 
tf_GS <- trimfill(res_GS)
tf_GS
funnel(tf_GS)
forest(tf_GS)

# Posso interpretare i risultati con il modello ad effetti fissi 

