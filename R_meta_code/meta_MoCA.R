
install.packages("meta")

library(meta)

setwd("C:/Users/AB889933/Desktop/Meta_ale")

author   <- c("Zhu", "Choi", "Yoon")
year     <- c(2018, 2019, 2017)
N_TRT   <- c(29, 30, 14)
MU_TRT <- c(1.8,2.1,6)
SD_TRT  <- c(2.5,4.5,2.6)
N_CON   <- c(31,30,7)
MU_CON <- c(1.8,0.1,-0.6)
SD_CON  <- c(2.6,3.5,3.0)

MoCA <- data.frame(author, year, N_TRT, MU_TRT, SD_TRT, N_CON, MU_CON, SD_CON)

rm(author, year, N_TRT, MU_TRT, SD_TRT, N_CON, MU_CON, SD_CON)                

MD  <- MoCA$MU_TRT - MoCA$MU_CON
MD

seMD <- sqrt(MoCA$SD_TRT^2/MoCA$N_TRT + MoCA$SD_CON^2/MoCA$N_CON)
seMD

MD
round(MD - 1.96 * seMD, 2)
round(MD + 1.96 * seMD, 2)            

# Metacont <- calculation of common and random effects estimates for meta-analyses 
# with continuous outcome data; inverse variance weightin is used for pooling. 
# Method used to estimate the standardised mean difference is Hedges

res_MoCA <- metacont(n.e = N_TRT, mean.e = MU_TRT, sd.e = SD_TRT,
                    n.c = N_CON, mean.c = MU_CON, sd.c = SD_CON,sm = "SMD",
                    studlab = paste(author, "et al.", year), data = MoCA)
res_MoCA
summary(res_MoCA)

# Quantifying heterogeneity: 90% 

metainf(res_MoCA)

# Forest plot 

forest(res_MoCA)

# I have heterogeneity so i use random effect model but i keep both the options 
# common and random effects


forest_MoCA <- forest(res_MoCA, layout = "RevMan5", common = FALSE,
                      label.left = "Favours control", col.label.left = "red",
                      label.right = "Favours experimental", col.label.right = "green",
                      prediction = FALSE, xlim= c(-1,3))

funnel_MoCA <- funnel(res_MoCA)


# trim-and-fill method for estimating end adjusting for the number and outcomes 
tf_MoCA <- trimfill(res_MoCA)
tf_MoCA
funnel(tf_MoCA)
forest(tf_MoCA)

# Non creo una meta-regressione perchè ho solo tre studi e quindi non faccio utilizzo
# variabili di moderazione

MoCA$MCI <- c("MCI", "noMCI","MCI")
MoCA$controllo <- c("passive", "active","passive")  


m1 <- metacont(N_TRT,MU_TRT,SD_TRT,N_CON,MU_CON,SD_CON,
               data = MoCA, sm="SMD")

mu2_MCI <- update(m1, subgroup = MCI, tau.common = TRUE, common = FALSE, method.random.ci = "HK")
metareg(mu2_MCI)
bubble(metareg(mu2_MCI), col.line = "blue")

mu2_controllo <- update(m1, subgroup = controllo, tau.common = TRUE, common = FALSE, method.random.ci = "HK")
metareg(mu2_controllo)
bubble(metareg(mu2_controllo), col.line = "blue")



