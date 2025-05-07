
install.packages("meta")

library(meta)

setwd("C:/Users/AB889933/Desktop/Meta_ale")

author   <- c("Martínez-Velilla", "Zhu", "Maki", "Cassilhas")
year     <- c(2019, 2018, 2012, 2007)
N_TRT   <- c(185, 29, 75, 20)
MU_TRT <- c(-1.3,-2.1,-0.5,-0.8)
SD_TRT  <- c(2.1,5.9,3.0,0.5)
N_CON   <- c(185,31,75,23)
MU_CON <- c(0.7,-3.3,0,0)
SD_CON  <- c(1.7,6.1, 3.0, 0.7)

GDS <- data.frame(author, year, N_TRT, MU_TRT, SD_TRT, N_CON, MU_CON, SD_CON)

# to delete 
rm(author, year, N_TRT, MU_TRT, SD_TRT, N_CON, MU_CON, SD_CON)                
              
# First, let's compute the mean difference with confidence interval for the
# separate studies, just to get acquainted with the data

MD  <- GDS$MU_TRT - GDS$MU_CON
MD

seMD <- sqrt(GDS$SD_TRT^2/GDS$N_TRT + GDS$SD_CON^2/GDS$N_CON)
seMD

MD
round(MD - 1.96 * seMD, 2)
round(MD + 1.96 * seMD, 2)            

# Metacont <- calculation of common and random effects estimates for meta-analyses 
# with continuous outcome data; inverse variance weightin is used for pooling. 
# Method used to estimate the standardised mean difference is Hedges

res_GDS <- metacont(n.e = N_TRT, mean.e = MU_TRT, sd.e = SD_TRT,
                     n.c = N_CON, mean.c = MU_CON, sd.c = SD_CON, sm = "SMD",
                     studlab = paste(author, "et al.", year), data = GDS)
res_GDS
summary(res_GDS)

# Quantifying heterogeneity 

metainf(res_GDS)

# Forest plot 

forest(res_GDS)

# I have heterogeneity so i use random effect model but i keep both the options 
# common and random effects

forest_GDS <- forest(res_GDS, layout = "RevMan5", common = FALSE,
                      label.right = "Favours control", col.label.right = "red",
                      label.left = "Favours experimental", col.label.left = "green",
                      prediction = FALSE, xlim = c(-2,1))

funnel_GDS <- funnel(res_GDS)


# trim-and-fill method for estimating end adjusting for the number and outcomes 
tf_GDS <- trimfill(res_GDS)
tf_GDS
funnel(tf_GDS)
forest(tf_GDS)

# Variabili di moderazione: 

GDS$controllo <- c("passive", "passive", "active", "active")
GDS$intervento <- c("exercise", "different", "different", "exercise")  

m1 <- metacont(N_TRT,MU_TRT,SD_TRT,N_CON,MU_CON,SD_CON,
               data = GDS, sm="SMD")

mu2 <- update(m1, subgroup = controllo, tau.common = TRUE, common = FALSE)
metareg(mu2)

forest(mu2)

# se stratifico per la variabile di stratificazione "controllo: attivo o passivo" 
# non viene più significativa la differenza media 

# Generate bubble plot 

bubble(metareg(mu2))

# do same meta-regressions using formula notation

metareg(m1, ~ controllo + intervento)

# Do meta-regression using REML method and print intermediate
# results for iterative estimation algorithm; furthermore print
# results with three digits.

metareg(m1, region, method.tau = "REML",
        control = list(verbose = TRUE), digits = 3)

# Use Hartung-Knapp method

mu3 <- update(mu2, method.random.ci = "HK")
mu3

forest(mu3)

# Hartung-Knapp without stratyfing for controllo 

mu4 <- update(res_GDS, method.random.ci = "HK")
mu4

forest(mu4)



