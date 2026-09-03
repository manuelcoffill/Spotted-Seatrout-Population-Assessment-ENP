#########################################
#########################################
#########################################
### ENP creel filtered data for spotted seatrout index standardization
### Manuel Coffill-Rivera
### started 9/2/26
#########################################
#########################################
#########################################


########### load libraries
library(here)
library(readxl)
library(ggplot2)
library(dplyr)
library(writexl)
library(mgcv)
library(MuMIn)
library(gratia)
library(car)

########### upload filtered dataset 
spt <- read_excel(
  here("ENPCreelSpottedSeatroutFilteredForIndex.xlsx"),
  guess_max = Inf
)
# need to do this so it doesnt assume that cols with no values in the first
# however many rows make it think that the whole column is blank throughout

nrow(spt)
names(spt)


######## CAN come back here and make plots for all of the candidate variables






################################################################################
################################################################################
################################################################################
### quick look at CPUE
hist(spt$CPUE)
table(spt$CPUE)

ggplot(spt, aes(x = factor(Year), y = CPUE)) +
  geom_boxplot() +
  labs(
    x = "Year",
    y = "SPT CPUE"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5)
  )




################################################################################
################################################################################
################################################################################

### FULL MODEL WILL BE:
### SPT CPUE = Year + Season + FishingPartyComp + Area
### where SPT CPUE = SPT total catch / (hours fished * number of anglers)



#### lets make sure categorical variables (all of them) are treated as factors
spt$Year <- as.factor(spt$Year)
spt$Season <- as.factor(spt$Season)
spt$FPC <- as.factor(spt$FPC)
spt$areaFishedBinned <- as.factor(spt$areaFishedBinned)

levels(spt$Year)
levels(spt$Season)
levels(spt$FPC)
levels(spt$areaFishedBinned)


####### CONSIDER checking for correlation
####### A good option is Cramér's V, which measures the strength of association
####### between categorical variables:


################################################################################
################################################################################
################################################################################


########## chekcing annual proportion positives for SPT
presence_by_year <- spt %>%
  group_by(Year) %>%
  summarise(
    proportion_present = mean(SPTPresence == "Present", na.rm = TRUE),
    .groups = "drop"
  )

ggplot(presence_by_year, aes(x = factor(Year), y = proportion_present)) +
  geom_col() +
  scale_y_continuous(
    limits = c(0, 1),
    labels = scales::percent
  ) +
  labs(
    x = "Year",
    y = "Proportion of Interviews with SPT Present"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5)
  )

########## MAJOR DECISION POINT
######### will consider single models (NOT DELTA) first !!!!!!
######### JUSTIFICATION: SPT annual proportion positive stays above 30% thru timeseries



################################################################################
################################################################################
################################################################################
### log and 4th root transformations
spt$CPUElog <- log(spt$CPUE + 1)
spt$CPUE4th <- (spt$CPUE)^(1/4)

summary(spt$CPUE)
summary(spt$CPUElog)
summary(spt$CPUE4th)

### FULL MODEL WILL BE:
### SPT CPUE = Year + Season + FishingPartyComp + Area
### where SPT CPUE = SPT total catch / (hours fished * number of anglers)




################## test all error distributions with all transformations
# no transformation gaussian
gauss <- gam(CPUE ~ (Year) + (Season) + (FPC) + (areaFishedBinned),
    family=gaussian, data=spt, method="REML")

# log gaussian
loggauss <- gam(CPUElog ~ (Year) + (Season) + (FPC) + (areaFishedBinned),
                family=gaussian, data=spt, method="REML")

# 4th root gaussian
fourthgauss <- gam(CPUE4th ~ (Year) + (Season) + (FPC) + (areaFishedBinned),
                  family=gaussian, data=spt, method="REML")

# tweedie
tweed <- gam(CPUE ~ (Year) + (Season) + (FPC) + (areaFishedBinned),
                family=tw(link = "log"), data=spt, method="REML")

# poisson
poiss <- gam(CPUE ~ (Year) + (Season) + (FPC) + (areaFishedBinned),
             family=poisson, data=spt, method="REML")

# negative binomial
negbin <- gam(CPUE ~ (Year) + (Season) + (FPC) + (areaFishedBinned),
             family=nb(), data=spt, method="REML")

# negative binomial log
lognegbin <- gam(CPUElog ~ (Year) + (Season) + (FPC) + (areaFishedBinned),
              family=nb(), data=spt, method="REML")

# negative binomial 4th
fourthnegbin <- gam(CPUE4th ~ (Year) + (Season) + (FPC) + (areaFishedBinned),
              family=nb(), data=spt, method="REML")









AIC(gauss,loggauss,fourthgauss, tweed, poiss,negbin,
    lognegbin,fourthnegbin)
#### only approrpaite for same scales!!! So cannot use to compare between
#### untransformed, log-transformed, and 4th root transformations



######## Diagnostic plots
op <- par(mfrow = c(2, 2), ask = FALSE) 
gam.check(gauss)
gam.check(loggauss)
gam.check(fourthgauss)
gam.check(tweed) # relatively best
gam.check(poiss)
gam.check(negbin)
gam.check(lognegbin)
gam.check(fourthnegbin)
# WHO look best

par(op)


qq.gam(tweed)
appraise(tweed)

# qq looks best for tweed, but NOT GREAT :(



####### check deviance epxlained
summary(gauss)$dev.expl
summary(loggauss)$dev.expl
summary(fourthgauss)$dev.expl
summary(tweed)$dev.expl
summary(poiss)$dev.expl
summary(negbin)$dev.expl
summary(lognegbin)$dev.expl
summary(fourthnegbin)$dev.expl
#### ALL TERRIBLE :(



# DHARMA
library(DHARMa)

# check all variations
resgauss <- simulateResiduals(fittedModel = gauss)
plot(resgauss)

resloggauss <- simulateResiduals(fittedModel = loggauss)
plot(resloggauss)

res4thgauss <- simulateResiduals(fittedModel = fourthgauss)
plot(res4thgauss)

restweed <- simulateResiduals(fittedModel = tweed)
plot(restweed)

respoiss <- simulateResiduals(fittedModel = poiss)
plot(respoiss)

resnegbin <- simulateResiduals(fittedModel = negbin)
plot(resnegbin)

reslognegbin <- simulateResiduals(fittedModel = lognegbin)
plot(reslognegbin)

res4thnegbin <- simulateResiduals(fittedModel = fourthnegbin)
plot(res4thnegbin)

# Tweed is comparatively the best by far

#######################################################
library(performance)
library(tweedie)
compare_performance(gauss,loggauss,fourthgauss, tweed,poiss,negbin,
                    lognegbin,fourthnegbin, rank = TRUE)
# also innaprpriate because veral metrics are not comparable between different scales
# different repsonse trnasofmraitons have likelihoods in different sclaes,
# so AIC, log-likelihood are not comparable 





################################################################################
### based on QQplots (via gam.check), simulated residuals (via DHARMA), 
### and deviance explained, Tweedie seems to be best.
### primarily based on dharma resids

testUniformity(tweed)
testDispersion(tweed)
testZeroInflation(tweed)



############### VIF and concurvity
library(car)
vif(tweed)


dev.off()
library(gratia)
draw(concrvity(tweed))
draw(concrvity(tweed, pairwise = TRUE))
concurvity(tweed, full=TRUE)



###############################################################################
################################################################################
################################################################################
# GAM multi model inference
### Multimodel inference ###
options(na.action = "na.fail")

fullg <- gam(CPUE ~ (Year) + (Season) + (FPC) + (areaFishedBinned),
             family=tw(link = "log"), data=spt, method="REML")


######################### 
ddb<- dredge(fullg, extra=c("adjR^2","R^2","deviance", F = function(x)summary(x)$fstatistic[[1]]))
head(ddb)

#deviance explained 
mod0Sif<-gam(CPUE ~ 1, family=tw(link = "log"),data=spt)
(ddb$Deviance.explained<-((deviance(mod0Sif)-ddb$deviance)/deviance(mod0Sif))*100 )
head(ddb)
# consider saving these
write_xlsx(ddb, here("SPT_MuMIn.xlsx"))

bestmodelg <-get.models(ddb,1)[[1]]
(anova.gam(bestmodelg))

par(mfrow = c(1,1))
par(mar = c(3,5,6,4))
plot(ddb, labAsExpr = TRUE, main = "")





