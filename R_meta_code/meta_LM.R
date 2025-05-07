
install.packages("meta")

library(meta)

setwd("C:/Users/AB889933/Desktop/Meta_ale")

author   <- c("Zhu", "Suzuki", "Sungkart", "Maki")
year     <- c(2018, 2012, 2017,2012)
N_TRT   <- c(29,25,33,75)
MU_TRT <- c(2.4,3.8,10.1,3.1)
SD_TRT  <- c(5.1,6.2,7.4,5.9)
N_CON   <- c(31,25,33,75)
MU_CON <- c(0,0.6,6.5,2.8)
SD_CON  <- c(5.1,6.3,8.9,5.6)

LM <- data.frame(author, year, N_TRT, MU_TRT, SD_TRT, N_CON, MU_CON, SD_CON)

# to delete 

rm(author, year, N_TRT, MU_TRT, SD_TRT, N_CON, MU_CON, SD_CON)                

MD  <- LM$MU_TRT - LM$MU_CON
MD

seMD <- sqrt(LM$SD_TRT^2/LM$N_TRT + LM$SD_CON^2/LM$N_CON)
seMD

MD
round(MD - 1.96 * seMD, 2)
round(MD + 1.96 * seMD, 2)            

# Metacont <- calculation of common and random effects estimates for meta-analyses 
# with continuous outcome data; inverse variance weightin is used for pooling. 
# Method used to estimate the standardised mean difference is Hedges

res_LM <- metacont(n.e = N_TRT, mean.e = MU_TRT, sd.e = SD_TRT,
                    n.c = N_CON, mean.c = MU_CON, sd.c = SD_CON,sm = "SMD",
                    studlab = paste(author, "et al.", year), data = LM)
res_LM
summary(res_LM)

# Quantifying heterogeneity 

metainf(res_LM)

# Forest plot 

forest_LM <- forest(res_LM, layout = "RevMan5", random = FALSE,
                      label.left = "Favours control", col.label.left = "red",
                      label.right = "Favours experimental", col.label.right = "green",
                      prediction = FALSE, xlim= c(-0.5,1))


funnel_LM <- funnel(res_LM)

# Posso usare modello ad effetti fissi perchè il test accetta l'ipotesi di omogeineità
# e l'indice I^2 è del 27%

# trim-and-fill method for estimating end adjusting for the number and outcomes 
tf_LM <- trimfill(res_LM)
tf_LM
funnel(tf_LM)
forest(tf_LM)


# Analisi di influenza: tolgo Maki 


author   <- c("Zhu", "Suzuki", "Sungkart")
year     <- c(2018, 2012, 2017)
N_TRT   <- c(29,25,33)
MU_TRT <- c(2.4,3.8,10.1)
SD_TRT  <- c(5.1,6.2,7.4)
N_CON   <- c(31,25,33)
MU_CON <- c(0,0.6,6.5)
SD_CON  <- c(5.1,6.3,8.9)

LM_NoM <- data.frame(author, year, N_TRT, MU_TRT, SD_TRT, N_CON, MU_CON, SD_CON)

# to delete 

rm(author, year, N_TRT, MU_TRT, SD_TRT, N_CON, MU_CON, SD_CON)                

MD  <- LM_NoM$MU_TRT - LM_NoM$MU_CON
MD

seMD <- sqrt(LM_NoM$SD_TRT^2/LM_NoM$N_TRT + LM_NoM$SD_CON^2/LM_NoM$N_CON)
seMD

MD
round(MD - 1.96 * seMD, 2)
round(MD + 1.96 * seMD, 2)            

# Metacont <- calculation of common and random effects estimates for meta-analyses 
# with continuous outcome data; inverse variance weightin is used for pooling. 
# Method used to estimate the standardised mean difference is Hedges

res_LM_NoM <- metacont(n.e = N_TRT, mean.e = MU_TRT, sd.e = SD_TRT,
                   n.c = N_CON, mean.c = MU_CON, sd.c = SD_CON,sm = "SMD",
                   studlab = paste(author, "et al.", year), data = LM_NoM)
res_LM_NoM
summary(res_LM_NoM)

# Forest plot 

forest_LM <- forest(res_LM_NoM, layout = "RevMan5", random = FALSE,
                    label.left = "Favours control", col.label.left = "red",
                    label.right = "Favours experimental", col.label.right = "green",
                    prediction = FALSE, xlim= c(-0.5,1))

