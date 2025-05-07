
# To perform a meta-analysis in R, we need to access functions from the "meta"
# package. If this package hasn't been installed yet, you need to do so first:

install.packages("meta")

library(meta)

# To get some info on the package, ask for the help-file:

help(meta)

# We need some data first, this can be done two different ways: read the data
# into R or create the data in R. 

getwd()
setwd("C:/Users/AB889933/Desktop/Meta_ale")

author   <- c("Martínez-Velilla", "Cavalcante", "Yoon", "Hauer")
year     <- c(2019, 2020, 2017, 2017)
N_TRT   <- c(185, 23, 14, 14)
MU_TRT <- c(2.4, 1.1, 2.7, 2.0)
SD_TRT  <- c(2.1,0.3,1.6,2.7)
N_CON   <- c(185,22,7,14)
MU_CON <- c(0.2,0.1,0.4,0.3)
SD_CON  <- c(2.1,0.3,1.0,2.4)

SPPB <- data.frame(author, year, N_TRT, MU_TRT, SD_TRT, N_CON, MU_CON, SD_CON)

# to delete 
rm(author, year, N_TRT, MU_TRT, SD_TRT, N_CON, MU_CON, SD_CON)                
                
# explore the data first

str(SPPB)
head(SPPB, 2)
names(SPPB)
View(SPPB)               
                
# First, let's compute the mean difference with confidence interval for the
# separate studies, just to get acquainted with the data

MD  <- SPPB$MU_TRT - SPPB$MU_CON
MD

seMD <- sqrt(SPPB$SD_TRT^2/SPPB$N_TRT + SPPB$SD_CON^2/SPPB$N_CON)
seMD

MD
round(MD - 1.96 * seMD, 2)
round(MD + 1.96 * seMD, 2)            

# With these data available, we can run the meta analysis.Use metacont()
# for continuous outcome measures, and metabin() for binary outcome measures

?metacont

# Metacont <- calculation of common and random effects estimates for meta-analyses 
# with continuous outcome data; inverse variance weightin is used for pooling. 
# Method used to estimate the standardised mean difference is Hedges

res_SPPB <- metacont(n.e = N_TRT, mean.e = MU_TRT, sd.e = SD_TRT,
                n.c = N_CON, mean.c = MU_CON, sd.c = SD_CON,sm = "SMD",
                studlab = paste(author, "et al.", year), data = SPPB)
res_SPPB
summary(res_SPPB)

# Quantifying heterogeneity 

metainf(res_SPPB)

# Forest plot 

forest(res_SPPB)

# I have heterogeneity so i use random effect model but i keep both the options 
# common and random effects

forest_SPPB <- forest(res_SPPB, layout = "RevMan5", common = FALSE,
                      label.left = "Favours control", col.label.left = "red",
                      label.right = "Favours experimental", col.label.right = "green",
                      prediction = FALSE, xlim = c(-0.5, 4))


# FUNNEL PLOT: graphical tool used in meta-analyses to visualize the precision 
# of effect estimates from individual studies included. The plot is particulary 
# useful for detecting asymmetries and potential biases

funnel_SPPB <- funnel(res_SPPB)

# Some tests are available for small study effects. Note that N = 11 is low!

# Say there was significant evidence of a small study effect, what could we do?

?trimfill

# trim-and-fill method for estimating end adjusting for the number and outcomes 
tf_SPPB <- trimfill(res_SPPB)
tf_SPPB
funnel(tf_SPPB)
forest(tf_SPPB)

# perform meta-regression to assess the influence of moderator variables
# to make a meta-regression add some grouping variables: 

SPPB$age <- c("very_old", "old", "old", "very_old")
SPPB$region <- c("Europe", "Europe", "Non_EU", "Non_EU")  


m1 <- metacont(N_TRT,MU_TRT,SD_TRT,N_CON,MU_CON,SD_CON,
               data = SPPB, sm="SMD")

mu2 <- update(m1, subgroup = region, tau.common = TRUE, common = FALSE)
metareg(mu2)

# Generate bubble plot 

bubble(metareg(mu2))

# do same meta-regressions using formula notation

metareg(m1, ~ region + age)

# Do meta-regression using REML method and print intermediate
# results for iterative estimation algorithm; furthermore print
# results with three digits.

metareg(m1, region, method.tau = "REML",
        control = list(verbose = TRUE), digits = 3)

# Use Hartung-Knapp method

mu3 <- update(mu2, method.random.ci = "HK")
mu3

forest(mu3)

# Hartung-Knapp without stratyfing for regione and age 

mu4 <- update(res_SPPB, method.random.ci = "HK")
mu4

forest(mu4)



