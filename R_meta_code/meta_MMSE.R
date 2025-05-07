

install.packages("meta")

library(meta)

setwd("C:\Users\AB889933\Desktop\Meta_ale")


author   <- c("Martínez-Velilla","Yoon","Suzuki", "Lam", "Williamson", "Muscari", "Venturelli")
year     <- c(2019, 2017, 2012, 2014, 2009, 2010, 2010)
N_TRT   <- c(185,14, 25, 96,50,60,15)
MU_TRT <- c(2.1, 4.4,0.3,0.3, 0.7, -0.2, 0.7)
SD_TRT  <- c(2.8,1.8,3.3,3.3,6.1,2.3,1.4)
N_CON   <- c(185,7,25,169,52,60,15)
MU_CON <- c(0.3, -1.2, -1.4, 0.2, 1.0, -1.2,-4.6)
SD_CON  <- c(2.4,1.6,3.3,3.4,6.3,2.4,1.2)

MMSE <- data.frame(author, year, N_TRT, MU_TRT, SD_TRT, N_CON, MU_CON, SD_CON)


rm(author, year, N_TRT, MU_TRT, SD_TRT, N_CON, MU_CON, SD_CON)                


str(MMSE)
head(MMSE, 2)
names(MMSE)
View(MMSE)               

# let's compute the mean difference with confidence interval 

MD  <- MMSE$MU_TRT - MMSE$MU_CON
MD

seMD <- sqrt(MMSE$SD_TRT^2/MMSE$N_TRT + MMSE$SD_CON^2/MMSE$N_CON)
seMD

MD
round(MD - 1.96 * seMD, 2)
round(MD + 1.96 * seMD, 2) 

# run the metanalysis

res_MMSE <- metacont(n.e = N_TRT, mean.e = MU_TRT, sd.e = SD_TRT,
                     n.c = N_CON, mean.c = MU_CON, sd.c = SD_CON, sm = "SMD",
                     studlab = paste(author, "et al.", year), data = MMSE)
res_MMSE
summary(res_MMSE)
# Test di Egger per vedere se ho asimmetria nel funnel plot
metabias(res_MMSE, k.min=5)
metabias(res_MMSE, k.min=5, plotit=TRUE)

# Test di Begg & Mazumdar per vedere se ho asimmetria nel funnel plot
metabias(res_MMSE, method.bias ="Begg",k.min=5)

# Quantifying heterogeneity:I^2 = 94% c'è eterogeneità 

metainf(res_MMSE, pooled="random")

# Forest plot 

forest_MMSE <- forest(res_MMSE, layout = "RevMan5", common = FALSE,
       label.left = "Favours control", col.label.left = "red",
       label.right = "Favours experimental", col.label.right = "green",
       prediction = FALSE, xlim = c(-1,5))

# Funnel plot

funnel_MMSE <- funnel(res_MMSE, common = FALSE,level=0.95, contour=c(0.9, 0.95, 0.99))$col.contour
legend(2, 0.05, c("0.1 > p > 0.05", "0.05 > p > 0.01", "< 0.01"), fill = funnel_MMSE)

funnel_MMSE <- funnel(res_MMSE, studlab=TRUE, cex.studlab = 0.5)


# trim-and-fill method for estimating end adjusting for the number and outcomes 
tf_MMSE <- trimfill(res_MMSE)
tf_MMSE
funnel(tf_MMSE, studlab=TRUE, cex.studlab = 0.5)
forest(tf_MMSE)

metabias(tf_MMSE, k.min=5)
metabias(res_MMSE, method.bias ="Begg",k.min=5)

# perform meta-regression to assess the influence of moderator variables
# to make a meta-regression add some grouping variables: 
# Variabili di moderazione.
# gender: se la % di donne è più alta women, altrimenti men.
# exstrenght: se si usa o no la forza all'interno del programma (Tai Chi "no")

MMSE$MCI <- c("noMCI", "MCI", "MCI", "noMCI","noMCI","noMCI","noMCI")
MMSE$control <- c("passive", "passive", "active", "active", "active", "active", "active")  
MMSE$region <- c("Europe", "nonEurope", "nonEurope", "nonEurope", "nonEurope","Europe", "Europe")
MMSE$FU <- c(0,3,12,12,12,12,3)
MMSE$gender <- c("female", "female", "male","female","female", "male","female")


m1 <- metacont(N_TRT,MU_TRT,SD_TRT,N_CON,MU_CON,SD_CON,
               data = MMSE, sm="SMD")

metareg(m1, ~ MCI + control + region + FU + gender)

m2 <- metareg(m1, ~ FU)
m2 <- metareg(m1, ~ MCI)
bubble(m2)

m1


m2 <- update(m1, method.random.ci ="HK")

m2 <- update(m1, method.tau ="SJ",method.random.ci ="HK")
m2 <- update(m1, method.tau = "SJ")
metareg(m2, ~ MCI + control + region + FU + gender)





mu2_MCI <- update(m1, subgroup = MCI, tau.common = TRUE, common = FALSE, method.random.ci = "HK")
metareg(mu2_MCI)

mu2_controllo <- update(m1, subgroup = controllo, tau.common = TRUE, common = FALSE, method.random.ci = "HK")
metareg(mu2_controllo)

forest(mu2)
# Generate bubble plot 

bubble(metareg(mu2_MCI), col.line = "blue")


bubble(metareg(mu2_controllo), col.line = "blue")

# Use Hartung-Knapp method with stratyfing

mu3 <- update(mu2, method.random.ci = "HK")
mu3

forest(mu3)

# Hartung-Knapp without stratyfing  

mu4 <- update(res_MMSE, method.random.ci = "HK")
mu4
forest(mu4)
# do same meta-regressions using formula notation

metareg(m1, ~ MCI + controllo)


# Do meta-regression using REML method and print intermediate
# results for iterative estimation algorithm; furthermore print
# results with three digits.


# ANALISI DI INFLUENZA 

author   <- c("Martínez-Velilla","Suzuki", "Lam", "Williamson", "Muscari", "Venturelli")
year     <- c(2019, 2012, 2014, 2009, 2010, 2010)
N_TRT   <- c(185, 25, 96,50,60,15)
MU_TRT <- c(2.1,0.3,0.3, 0.7, -0.2, 0.7)
SD_TRT  <- c(2.8,3.3,3.3,6.1,2.3,1.4)
N_CON   <- c(185,25,169,52,60,15)
MU_CON <- c(0.3, -1.4, 0.2, 1.0, -1.2,-4.6)
SD_CON  <- c(2.4,3.3,3.4,6.3,2.4,1.2)

MMSE_noYoon <- data.frame(author, year, N_TRT, MU_TRT, SD_TRT, N_CON, MU_CON, SD_CON)


rm(author, year, N_TRT, MU_TRT, SD_TRT, N_CON, MU_CON, SD_CON)                


str(MMSE_noYoon)
head(MMSE_noYoon, 2)
names(MMSE_noYoon)
View(MMSE_noYoon)               

# let's compute the mean difference with confidence interval 

MD  <- MMSE_noYoon$MU_TRT - MMSE_noYoon$MU_CON
MD

seMD <- sqrt(MMSE_noYoon$SD_TRT^2/MMSE_noYoon$N_TRT + MMSE_noYoon$SD_CON^2/MMSE_noYoon$N_CON)
seMD

MD
round(MD - 1.96 * seMD, 2)
round(MD + 1.96 * seMD, 2) 

# run the metanalysis

res_MMSE_noYoon <- metacont(n.e = N_TRT, mean.e = MU_TRT, sd.e = SD_TRT,
                     n.c = N_CON, mean.c = MU_CON, sd.c = SD_CON, sm = "SMD",
                     studlab = paste(author, "et al.", year), data = MMSE_noYoon)
res_MMSE_noYoon
summary(res_MMSE_noYoon)


# Forest plot 

forest_MMSE <- forest(res_MMSE_noYoon, layout = "RevMan5", common = FALSE,
                      label.left = "Favours control", col.label.left = "red",
                      label.right = "Favours experimental", col.label.right = "green",
                      prediction = FALSE, xlim = c(-1,5))


# Analisi di influenza: No Yoon e no Venturelli 


author   <- c("Martínez-Velilla","Suzuki", "Lam", "Williamson", "Muscari")
year     <- c(2019, 2012, 2014, 2009, 2010)
N_TRT   <- c(185, 25, 96,50,60)
MU_TRT <- c(2.1,0.3,0.3, 0.7, -0.2)
SD_TRT  <- c(2.8,3.3,3.3,6.1,2.3)
N_CON   <- c(185,25,169,52,60)
MU_CON <- c(0.3, -1.4, 0.2, 1.0, -1.2)
SD_CON  <- c(2.4,3.3,3.4,6.3,2.4)

MMSE_noYV <- data.frame(author, year, N_TRT, MU_TRT, SD_TRT, N_CON, MU_CON, SD_CON)


rm(author, year, N_TRT, MU_TRT, SD_TRT, N_CON, MU_CON, SD_CON)                


str(MMSE_noYV)
head(MMSE_noYV, 2)
names(MMSE_noYV)
View(MMSE_noYV)               

# let's compute the mean difference with confidence interval 

MD  <- MMSE_noYV$MU_TRT - MMSE_noYV$MU_CON
MD

seMD <- sqrt(MMSE_noYV$SD_TRT^2/MMSE_noYV$N_TRT + MMSE_noYV$SD_CON^2/MMSE_noYV$N_CON)
seMD

MD
round(MD - 1.96 * seMD, 2)
round(MD + 1.96 * seMD, 2) 

# run the metanalysis

res_MMSE_noYV <- metacont(n.e = N_TRT, mean.e = MU_TRT, sd.e = SD_TRT,
                            n.c = N_CON, mean.c = MU_CON, sd.c = SD_CON, sm = "SMD",
                            studlab = paste(author, "et al.", year), data = MMSE_noYV)
res_MMSE_noYV
summary(res_MMSE_noYV)


# Forest plot 

forest_MMSE <- forest(res_MMSE_noYV, layout = "RevMan5", common = FALSE,
                      label.left = "Favours control", col.label.left = "red",
                      label.right = "Favours experimental", col.label.right = "green",
                      prediction = FALSE, xlim = c(-1,1.5))



# Analisi di influenza: No Yoon e no Venturelli no Suzuki


author   <- c("Martínez-Velilla", "Lam", "Williamson", "Muscari")
year     <- c(2019, 2014, 2009, 2010)
N_TRT   <- c(185, 96,50,60)
MU_TRT <- c(2.1,0.3, 0.7, -0.2)
SD_TRT  <- c(2.8,3.3,6.1,2.3)
N_CON   <- c(185,169,52,60)
MU_CON <- c(0.3, 0.2, 1.0, -1.2)
SD_CON  <- c(2.4,3.4,6.3,2.4)

MMSE_noYVS <- data.frame(author, year, N_TRT, MU_TRT, SD_TRT, N_CON, MU_CON, SD_CON)


rm(author, year, N_TRT, MU_TRT, SD_TRT, N_CON, MU_CON, SD_CON)                


str(MMSE_noYVS)
head(MMSE_noYVS, 2)
names(MMSE_noYVS)
View(MMSE_noYVS)               

# let's compute the mean difference with confidence interval 

MD  <- MMSE_noYVS$MU_TRT - MMSE_noYVS$MU_CON
MD

seMD <- sqrt(MMSE_noYVS$SD_TRT^2/MMSE_noYVS$N_TRT + MMSE_noYVS$SD_CON^2/MMSE_noYVS$N_CON)
seMD

MD
round(MD - 1.96 * seMD, 2)
round(MD + 1.96 * seMD, 2) 

# run the metanalysis

res_MMSE_noYVS <- metacont(n.e = N_TRT, mean.e = MU_TRT, sd.e = SD_TRT,
                          n.c = N_CON, mean.c = MU_CON, sd.c = SD_CON, sm = "SMD",
                          studlab = paste(author, "et al.", year), data = MMSE_noYVS)
res_MMSE_noYVS
summary(res_MMSE_noYVS)


# Forest plot 

forest_MMSE <- forest(res_MMSE_noYVS, layout = "RevMan5", common = FALSE,
                      label.left = "Favours control", col.label.left = "red",
                      label.right = "Favours experimental", col.label.right = "green",
                      prediction = FALSE, xlim = c(-1,1.5))



# Analisi di influenza: No Yoon e no Venturelli no Martinez 


author   <- c("Suzuki", "Lam", "Williamson", "Muscari")
year     <- c(2012, 2014, 2009, 2010)
N_TRT   <- c( 25, 96,50,60)
MU_TRT <- c(0.3,0.3, 0.7, -0.2)
SD_TRT  <- c(3.3,3.3,6.1,2.3)
N_CON   <- c(25,169,52,60)
MU_CON <- c(-1.4, 0.2, 1.0, -1.2)
SD_CON  <- c(3.3,3.4,6.3,2.4)

MMSE_noYVM <- data.frame(author, year, N_TRT, MU_TRT, SD_TRT, N_CON, MU_CON, SD_CON)


rm(author, year, N_TRT, MU_TRT, SD_TRT, N_CON, MU_CON, SD_CON)                


str(MMSE_noYVM)
head(MMSE_noYVM, 2)
names(MMSE_noYVM)
View(MMSE_noYVM)               

# let's compute the mean difference with confidence interval 

MD  <- MMSE_noYVM$MU_TRT - MMSE_noYVM$MU_CON
MD

seMD <- sqrt(MMSE_noYVM$SD_TRT^2/MMSE_noYVM$N_TRT + MMSE_noYVM$SD_CON^2/MMSE_noYVM$N_CON)
seMD

MD
round(MD - 1.96 * seMD, 2)
round(MD + 1.96 * seMD, 2) 

# run the metanalysis

res_MMSE_noYVM <- metacont(n.e = N_TRT, mean.e = MU_TRT, sd.e = SD_TRT,
                          n.c = N_CON, mean.c = MU_CON, sd.c = SD_CON, sm = "SMD",
                          studlab = paste(author, "et al.", year), data = MMSE_noYVM)
res_MMSE_noYVM
summary(res_MMSE_noYVM)


# Forest plot 

forest_MMSE <- forest(res_MMSE_noYVM, layout = "RevMan5", common = FALSE,
                      label.left = "Favours control", col.label.left = "red",
                      label.right = "Favours experimental", col.label.right = "green",
                      prediction = FALSE, xlim = c(-1,1.5))




# Analisi di influenza: No Yoon e no Venturelli no Martinez no Suzuki 


author   <- c("Lam", "Williamson", "Muscari")
year     <- c(2014, 2009, 2010)
N_TRT   <- c(96,50,60)
MU_TRT <- c(0.3, 0.7, -0.2)
SD_TRT  <- c(3.3,6.1,2.3)
N_CON   <- c(169,52,60)
MU_CON <- c(0.2, 1.0, -1.2)
SD_CON  <- c(3.4,6.3,2.4)

MMSE_noYVMS <- data.frame(author, year, N_TRT, MU_TRT, SD_TRT, N_CON, MU_CON, SD_CON)


rm(author, year, N_TRT, MU_TRT, SD_TRT, N_CON, MU_CON, SD_CON)                


str(MMSE_noYVMS)
head(MMSE_noYVMS, 2)
names(MMSE_noYVMS)
View(MMSE_noYVMS)               

# let's compute the mean difference with confidence interval 

MD  <- MMSE_noYVMS$MU_TRT - MMSE_noYVMS$MU_CON
MD

seMD <- sqrt(MMSE_noYVMS$SD_TRT^2/MMSE_noYVMS$N_TRT + MMSE_noYVMS$SD_CON^2/MMSE_noYVMS$N_CON)
seMD

MD
round(MD - 1.96 * seMD, 2)
round(MD + 1.96 * seMD, 2) 

# run the metanalysis

res_MMSE_noYVMS <- metacont(n.e = N_TRT, mean.e = MU_TRT, sd.e = SD_TRT,
                           n.c = N_CON, mean.c = MU_CON, sd.c = SD_CON, sm = "SMD",
                           studlab = paste(author, "et al.", year), data = MMSE_noYVMS)
res_MMSE_noYVMS
summary(res_MMSE_noYVMS)


# Forest plot 

forest_MMSE <- forest(res_MMSE_noYVMS, layout = "RevMan5", common = FALSE,
                      label.left = "Favours control", col.label.left = "red",
                      label.right = "Favours experimental", col.label.right = "green",
                      prediction = FALSE, xlim = c(-1,1.5))



# Analisi di influenza: No Yoon e no Venturelli no Martinez no Lam


author   <- c("Suzuki", "Williamson", "Muscari")
year     <- c(2012, 2009, 2010)
N_TRT   <- c( 25,50,60)
MU_TRT <- c(0.3,0.7, -0.2)
SD_TRT  <- c(3.3,6.1,2.3)
N_CON   <- c(25,52,60)
MU_CON <- c(-1.4, 1.0, -1.2)
SD_CON  <- c(3.3,6.3,2.4)

MMSE_noYVML <- data.frame(author, year, N_TRT, MU_TRT, SD_TRT, N_CON, MU_CON, SD_CON)


rm(author, year, N_TRT, MU_TRT, SD_TRT, N_CON, MU_CON, SD_CON)                


str(MMSE_noYVML)
head(MMSE_noYVML, 2)
names(MMSE_noYVML)
View(MMSE_noYVML)               

# let's compute the mean difference with confidence interval 

MD  <- MMSE_noYVML$MU_TRT - MMSE_noYVML$MU_CON
MD

seMD <- sqrt(MMSE_noYVML$SD_TRT^2/MMSE_noYVML$N_TRT + MMSE_noYVML$SD_CON^2/MMSE_noYVML$N_CON)
seMD

MD
round(MD - 1.96 * seMD, 2)
round(MD + 1.96 * seMD, 2) 

# run the metanalysis

res_MMSE_noYVML <- metacont(n.e = N_TRT, mean.e = MU_TRT, sd.e = SD_TRT,
                           n.c = N_CON, mean.c = MU_CON, sd.c = SD_CON, sm = "SMD",
                           studlab = paste(author, "et al.", year), data = MMSE_noYVML)
res_MMSE_noYVML
summary(res_MMSE_noYVML)


# Forest plot 

forest_MMSE <- forest(res_MMSE_noYVML, layout = "RevMan5", common = FALSE,
                      label.left = "Favours control", col.label.left = "red",
                      label.right = "Favours experimental", col.label.right = "green",
                      prediction = FALSE, xlim = c(-1,1.5))










