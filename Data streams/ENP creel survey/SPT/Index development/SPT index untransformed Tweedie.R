#########################################
#########################################
#########################################
### ENP creel spotted seatrout index standardization
### Manuel Coffill-Rivera
### started 9/3/26
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
library(DHARMa)

########### upload filtered dataset 
spt <- read_excel(
  here("ENPCreelSpottedSeatroutFilteredForIndex.xlsx"),
  guess_max = Inf
)
# need to do this so it doesnt assume that cols with no values in the first
# however many rows make it think that the whole column is blank throughout

#final sample size
nrow(spt)




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
# tweedie
tweed <- gam(CPUE ~ (Year) + (Season) + (FPC) + (areaFishedBinned),
             family=tw(link = "log"), data=spt, method="REML")

summary(tweed)
anova(tweed)

######## Diagnostic plots
gam.check(tweed)
qq.gam(tweed)

restweed <- simulateResiduals(fittedModel = tweed)
plot(restweed)


################################################################################
################################################################################
################################################################################
##### visualizations

#### first determine predictor levels with highest sample sizes to predict with
table(spt$Year) #1997
table(spt$Season) #dry
table(spt$FPC) # skilled
table(spt$areaFishedBinned) #3-5


# rename model to match code below
mod <- tweed

####### Year
#######################################
table(spt$Year)
as.numeric(unique(spt$Year))


grid.res <- 34 # estimation grid resolution - in my case, number of years in the analysis
grid.bin <- data.frame(
  "Year"=c("1991","1992","1993","1994","1995","1996","1997",
           "1998","1999","2000","2001","2002","2003","2004",
           "2005","2006","2007","2008","2009","2010","2011",
           "2012","2013","2014","2015","2016","2017","2018",
           "2019","2020","2021","2022","2023","2024"
           ),
  "Season"="Dry",
  "FPC"="Skilled",
  "areaFishedBinned"="3-5")
grid.bin
grid.bin$Year <- as.factor(grid.bin$Year)
grid.bin$Season <- as.factor(grid.bin$Season)
grid.bin$FPC <- as.factor(grid.bin$FPC)
grid.bin$areaFishedBinned <- as.factor(grid.bin$areaFishedBinned)

pred.bin <- predict(mod, grid.bin, type="response", se=T)
niter <- 10000  # number of replicates
pred.bin.boot <- matrix(NA,nrow=grid.res,ncol=niter)
for(i in 1:grid.res){
  pred.bin.boot[i,] <- rnorm(niter, pred.bin$fit[i], pred.bin$se.fit[i])}
pred.mean <- apply(pred.bin.boot,1,mean)
pred.95CI <- apply(pred.bin.boot,1,quantile,probs=c(0.025,0.975)) # to get 95% CIs
pred.sd <- apply(pred.bin.boot,1,sd)

years <- c("1991","1992","1993","1994","1995","1996","1997",
           "1998","1999","2000","2001","2002","2003","2004",
           "2005","2006","2007","2008","2009","2010","2011",
           "2012","2013","2014","2015","2016","2017","2018",
           "2019","2020","2021","2022","2023","2024"
)
binomial <- rbind(years, pred.mean, pred.95CI,pred.sd)
binomial

redd <- adjustcolor("#9AD93CFF", alpha.f=0.25) 


plot(binomial[1,], binomial[2,], type="p",mgp=c(2,0.7,0), ylim=c(0,1),xlab="Year",lwd=2,cex.lab=1.4,ylab="",boxwex=0.01,cex.axis=1.1, main="", xaxt = "n")
polygon(c(rev(binomial[1,1:34]), binomial[1,1:34]), c(rev(binomial[3,1:34]), binomial[4,1:34]), col = redd, alpha = 0.5, border = NA)
points(binomial[1,],binomial[2,], type="p",cex=1.5,pch=19, col="#9AD93CFF")
#points(binomial[1,1:5],binomial[2,1:5], type="l",col="#9AD93CFF")
#points(binomial[1,7:21],binomial[2,7:21], type="l",col="#9AD93CFF")
#points(binomial[1,22:23],binomial[2,22:23], type="l",col="#9AD93CFF")

#lines(binomial[1,1:5], binomial[3,1:5], lty=1,col="#9AD93CFF")
#lines(binomial[1,1:5], binomial[3,1:5], lty=1,col="#9AD93CFF")
#lines(binomial[1,7:21], binomial[3,7:21], lty=1,col="#9AD93CFF")
#lines(binomial[1,22:23], binomial[3,22:23], lty=1,col="#9AD93CFF")
#lines(binomial[1,1:5], binomial[4,1:5], lty=1,col="#9AD93CFF")
#lines(binomial[1,7:21], binomial[4,7:21], lty=1,col="#9AD93CFF")
#lines(binomial[1,22:23], binomial[4,22:23], lty=1,col="#9AD93CFF")
axis(1, at = seq(1992,2024,4))

#### all the predicted values and associated error + CVs
binomial


#### save year data for ggplot
binomiall <- t(binomial)
binomiall <- as.data.frame(binomiall)

index <- binomiall
index <- as.data.frame(index)
index[] <- lapply(index, function(x) as.numeric(as.character(x)))


## calculate index CV
index$CV <- index$pred.sd / index$pred.mean


library(writexl)
write_xlsx(index,
           here("SPTENPIndexYearPreds.xlsx"))



############## END !!!!


############# CAN ALSO LOOK AT PARTIAL EFFECTS OF THE OTHER PREDICTORS!!!!!!!!!!











