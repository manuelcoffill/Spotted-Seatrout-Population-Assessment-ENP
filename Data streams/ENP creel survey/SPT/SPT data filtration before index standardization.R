#########################################
#########################################
#########################################
### ENP creel data filtration for spotted seatrout index standardization
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


########### upload dataset and flags dataset
spt <- read_excel(
  here("ENPCreelSpottedSeatrout.xlsx"),
  guess_max = Inf
)
# need to do this so it doesnt assume that cols with no values in the first
# however many rows make it think that the whole column is blank throughout

nrow(spt)
names(spt)


# starting interview total and annual sample size
nrow(spt)
table(spt$Year)

# save starting n just to compare as filtration occurs
startingn <- nrow(spt)

################################################################################
################################################################################
################################################################################

#### let's start exploring to see how candidate variables look
colnames(spt)
### will try to do so in order of most relevant for index standardization

## lets look at amount of missing values by col
sapply(spt, function(x) sum(is.na(x)))


## can use function below to remove rows with NAs in major cols of interest
# spt <- spt %>%
#   filter(
#     !is.na(UniqueInterview),
#     !is.na(interviewLocation)
#   )
# ignoring the above for now.






##### hours fished
summary(spt$hoursFished)

ggplot(spt, aes(x = hoursFished)) +
  geom_histogram(
    bins = 30,
    na.rm = TRUE
  ) +
  labs(
    x = "Hours Fished",
    y = "Count"
  ) +
  theme_bw()

spt <- spt %>%
  filter(hoursFished <= 20)

startingn - nrow(spt)
# only 22 samples had over 20hrs fished

ggplot(spt, aes(x = hoursFished)) +
  geom_histogram(
    bins = 30,
    na.rm = TRUE
  ) +
  labs(
    x = "Hours Fished",
    y = "Count"
  ) +
  theme_bw()

table(spt$hoursFished)

#### DECISION POINT: REMOVE UNDER 2 HRS AND OVER 10 HRS FISHED
#### jUSTIFICATION: those ranges don't seem reflective to me of a "normal fishing trip
####                + sample sizes in those ranges are smaller.
####                + i've never seen charters sell trips below 2 hrs

spt <- spt %>%
  filter(hoursFished >= 2.0, hoursFished <= 10.0)
startingn - nrow(spt)
# removed a considerable number of interviews~ ~14k
nrow(spt)

ggplot(spt, aes(x = hoursFished)) +
  geom_histogram(
    bins = 30,
    na.rm = TRUE
  ) +
  labs(
    x = "Hours Fished",
    y = "Count"
  ) +
  theme_bw()

#### CONSIDER: making upper limit at 8. but would remove >10k interviews.


#### will keep 2-10 for now :)





################################################################################
################################################################################
################################################################################
colnames(spt)

######## FISHING PARTY COMPOSITION

table(spt$fishingPartyComposition)
## MUST REMOVE NO FISHING + NAs
spt <- spt %>%
  filter(
    !is.na(fishingPartyComposition),
    fishingPartyComposition != "no fishing"
  )

nrow(spt)
# removed a bunch of interviews

table(spt$fishingPartyComposition)

# making a new col to be used in index standardization
spt <- spt %>%
  mutate(
    FPC = if_else(
      fishingPartyComposition == "skilled",
      "Skilled",
      "Other"
    )
  )

table(spt$FPC)

# visualization of annual counts
ggplot(spt, aes(x = factor(Year), fill = FPC)) +
  geom_bar(position = "dodge") +
  labs(
    x = "Year",
    y = "Frequency",
    fill = "Fishing Party Composition"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    legend.position = "inside",
    legend.position.inside = c(0.98, 0.98),
    legend.justification = c("right", "top")
  )

##### NOTE will definitely need to make 1991 starting year
#####      and probably crop out 2025


################################################################################
################################################################################
################################################################################
colnames(spt)


################################## YEAR


ggplot(spt, aes(x = factor(Year))) +
  geom_bar() +
  labs(
    x = "Year",
    y = "Number of interviews"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5)
  )


############# DECISION POINT: removing pre 1991 and 2025
spt <- spt %>%
  filter(Year >= 1991, Year <= 2024)

ggplot(spt, aes(x = factor(Year))) +
  geom_bar() +
  labs(
    x = "Year",
    y = "Number of interviews"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5)
  )

nrow(spt)
startingn - nrow(spt)
# have removed almost 75k interviews at this point





################################################################################
################################################################################
################################################################################


############ MONTH / SEASON
table(spt$Month)

ggplot(spt, aes(x = factor(Month))) +
  geom_bar() +
  facet_wrap(~ Year) +
  labs(
    x = "Month",
    y = "Frequency"
  ) +
  theme_bw()

####### NOTE keep in mind that there are no samples for months 4 & 5 in 2020


###### DECISION POINT turning month into variable that reflects rainfall seasons
######                as done in Carlson and Osborne indices using ENP creel data

### dry = dec-may
### wet = jun-nov

spt <- spt %>%
  mutate(
    Season = case_when(
      Month %in% c(1:5, 12) ~ "Dry",
      Month %in% 6:11 ~ "Wet"
    )
  )

table(spt$Season)

ggplot(spt, aes(x = Season)) +
  geom_bar() +
  facet_wrap(~ Year) +
  labs(
    x = "Season",
    y = "Count"
  ) +
  theme_bw()






################################################################################
################################################################################
################################################################################
colnames(spt)


################################ area fished
table(spt$areaFished)
## probably need to gorup these. look at Fig.1 in goliath goruper index paper
nrow(spt)

# remove NAs
spt <- spt %>%
  filter(!is.na(areaFished))
nrow(spt)


## lets start by grouping same nmumber
spt <- spt %>%
  mutate(
    areaFishedRevised = as.integer(gsub("[^0-9]", "", areaFished))
  )
table(spt$areaFishedRevised)

ggplot(spt, aes(x = factor(areaFishedRevised))) +
  geom_bar() +
  facet_wrap(~ Year) +
  labs(
    x = "Area Fished",
    y = "Frequency"
  ) +
  theme_bw()
## look at this alongside the area maps

## COMMENTS
## 6 is constantly high
## 2 is constantly low

### DECISION POINT binning 1-2, 3-5, and keeping 6.
### JUSTIFICATION sample sizes and size of areas

spt <- spt %>%
  mutate(
    areaFishedBinned = case_when(
      areaFishedRevised %in% c(1, 2) ~ "1-2",
      areaFishedRevised %in% 3:5 ~ "3-5",
      areaFishedRevised == 6 ~ "6"
    )
  )

ggplot(spt, aes(x = factor(areaFishedBinned))) +
  geom_bar() +
  facet_wrap(~ Year) +
  labs(
    x = "Area Fished",
    y = "Frequency"
  ) +
  theme_bw()
# look fairly balanced more or less



################################################################################
################################################################################
################################################################################
colnames(spt)

########### preferred species

table(spt$scientificName_pref)

print(
  spt %>%
    count(scientificName_pref, sort = TRUE),
  n = Inf
)
# consider binning into assemblage that broadly uses similar habitat types.
# such as tarpon, snook, red drum, ladyfish, jack crevalle
# these are the spp identified in cluster analysis for the MRIP CPUE index in
# most recent FWC stock assessment of spotted seatrout
# decent justification, i suppose
# can have three categories, Unidentified, SPT assemblage, and other

# CONSIDER that if we end up using this, there are a couple hundred NAs to remove!
spt <- spt %>%
  filter(!is.na(scientificName_pref))

print(
  spt %>%
    count(scientificName_pref, sort = TRUE),
  n = Inf
)

#### make binned col for preference
spt <- spt %>%
  mutate(
    PreferredSpp = case_when(
      scientificName_pref == "Unidentified species" ~ "Unidentified SPP",
      
      scientificName_pref %in% c(
        "Centropomus undecimalis",
        "Megalops atlanticus",
        "Sciaenops ocellatus",
        "Elops saurus",
        "Caranx hippos"
      ) ~ "SPT cluster",
      
      scientificName_pref == "Cynoscion nebulosus" ~ "SPT",
      
      TRUE ~ "Other"
    )
  )

# chekc it worked right
spt %>%
  count(PreferredSpp, sort = TRUE)

spt %>%
  count(scientificName_pref, PreferredSpp, sort = TRUE)

# now look at yearly breakdowns
ggplot(spt, aes(x = PreferredSpp)) +
  geom_bar() +
  facet_wrap(~ Year) +
  labs(
    x = "Preferred Species",
    y = "Frequency"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5)
  )

### OBSERVATIONS: interesting how seatrout dissapears towards end of time series!
###               Highly unbalanced! Don't feel it would be good to keep
###

### DECISION POINT will NOT consider this variable in index development



################################################################################
################################################################################
################################################################################
################################################################################
################################################################################


#### CHECK numpeople
table(spt$numPeople)
## lets remove those trips with numpeople not so representative and low sample sizes

##### DECISION POINT: will keep under 6 people since higher vlaues have very low n
nrow(spt)

spt <- spt %>%
  filter(numPeople <= 6)

nrow(spt)
# removes ~ 200 interviews






################################################################################
################################################################################
################################################################################
################################################################################



### FULL MODEL WILL BE:
### SPT CPUE = Year + Season + FishingPartyComp + Area
### where SPT CPUE = SPT total catch / (hours fished * number of anglers)



##### ADD CPUE col
spt <- spt %>%
  mutate(
    CPUE = TotalCatch / (hoursFished * numPeople)
  )
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

nrow(spt)
########### DECISION POINT: removing really high CPUE outliers
###########                will remove those above 10

########## CONSIDER if this is a reasonable cutoff....

spt <- spt %>%
  filter(CPUE <= 10)

nrow(spt)
# only removed less than 100 interviews

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
################################################################################



##### CONSIDERED the use of interviewe location as a predictor variable, but
##### its probably very correlated with area fished, especially since I binned
##### area fished into three groups




######### SAVE DATASET!
nrow(spt)
write_xlsx(spt, here("Index development","ENPCreelSpottedSeatroutFilteredForIndex.xlsx"))




########### MOVIING ON TO INDEX DEVELOPMENT :))))


