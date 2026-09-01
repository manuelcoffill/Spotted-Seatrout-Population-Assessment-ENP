#########################################
#########################################
#########################################
### ENP creel data exploration
### Manuel Coffill-Rivera
### started 9/1/26
#########################################
#########################################
#########################################


########### load libraries
library(here)
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(writexl)



########### upload dataset and flags dataset
dat <- read_csv(
  here("EVER_creel_rec_catch.csv")
)

flags <- read_csv(
  here("EVER_creel_rec_catch_flags.csv")
)



########## data structure
head(dat)
colnames(dat)


### making column for unique record (species)
dat <- dat %>%
  mutate(
    UniqueRecord = paste(
      interviewLocation,
      eventDate,
      interviewNumber,
      scientificName_catch,
      sep = "_"
    )
  )

### making a column for unique interview
dat <- dat %>%
  mutate(
    UniqueInterview = paste(
      interviewLocation,
      eventDate,
      interviewNumber,
      sep = "_"
    )
  )


### count of unique records per unique interview
records_per_interview <- dat %>%
  group_by(UniqueInterview) %>%
  summarise(
    n_unique_records = n_distinct(UniqueRecord),
    .groups = "drop"
  )
records_per_interview

### count of unique records and interviews
dat %>%
  summarise(
    unique_interview = n_distinct(UniqueInterview),
    unique_records = n_distinct(UniqueRecord),

  )



########################### adding flags to dat
flags <- flags %>%
  mutate(
    UniqueRecord = paste(
      interviewLocation,
      eventDate,
      interviewNumber,
      scientificName_catch,
      sep = "_"
    )
  )

# checking max number of flags to a unique record
summary(dat$flags)

# create numbered flags
flags_wide <- flags %>%
  group_by(UniqueRecord) %>%
  mutate(flag_number = row_number()) %>%
  ungroup()

# convert to separate columns
flags_wide <- flags_wide %>%
  select(UniqueRecord, flag_number, flagDescription) %>%
  pivot_wider(
    names_from = flag_number,
    values_from = flagDescription,
    names_prefix = "Flag_"
  )

# join to dat
dat <- dat %>%
  left_join(flags_wide, by = "UniqueRecord")

# chekcing to make sure original dat$flags matches imported flags :)
dat %>%
  mutate(
    calculated_flag_count = rowSums(
      !is.na(select(., starts_with("Flag_")))
    )
  ) %>%
  select(UniqueRecord, flags, calculated_flag_count)


### separating date in new cols
dat <- dat %>%
  mutate(
    Year = year(eventDate),
    Month = month(eventDate),
    Day = day(eventDate)
  )



############################# data ready for visualizations and summary stats :)




###############################################################################
###############################################################################

###### interview breakdown
interviews_by_month <- dat %>%
  distinct(UniqueInterview, Year, Month) %>%
  count(Year, Month, name = "n_interviews")

ggplot(interviews_by_month, aes(x = Month, y = n_interviews)) +
  geom_col() +
  facet_wrap(~ Year) +
  scale_x_continuous(breaks = 1:12) +
  labs(
    x = "Month",
    y = "Number of interviews",
    title = "Number of interviews by year and month"
  ) +
  theme_bw()

ggsave("ENPCreelInterviews.png", width = 10, height = 8, dpi = 1000)

# NOTE very low interviews during Jan 2019 due to hurricane
# NOTE very low interviews during april and may 2002 (COVID)
# VERY surprised with the effort consistency across the time series!

###############################################################################
###############################################################################

#### making dataset specifically for spotted seatrout

# NOTE there are separate rows for disposition (harvested VS released). so trips
# that had SPT both harvested and released will have two rows. individualCount
# column tells you how many individuals were subject to each disposition

cynoscion_catch <- dat %>%
  filter(scientificName_catch == "Cynoscion nebulosus") %>%
  group_by(UniqueInterview) %>%
  summarise(
    Harvested = sum(individualCount[disposition == "harvested"], na.rm = TRUE),
    Released = sum(individualCount[disposition == "released"], na.rm = TRUE),
    TotalCatch = Harvested + Released,
    .groups = "drop"
  )

## merging back with important variables form dat
# first make a df of unique interviews
unique_interviews <- dat %>%
  select(
    UniqueInterview,
    interviewLocation,
    eventDate,
    interviewNumber,
    Year,
    Month,
    Day,
    numPeople,
    hoursFished,
    areaFished,
    interviewTime,
    hoursTrip,
    fishingPartyComposition,
    scientificName_pref,
    commonName_pref,
    originTrip,
    anglerResidence,
    interviewer,
    dayOfWeek
  ) %>%
  distinct(UniqueInterview, .keep_all = TRUE)

# verify count matches uniqueinterviews in dat
nrow(unique_interviews)
n_distinct(unique_interviews$UniqueInterview)

# now merge spt data with unique interview data
spt <- unique_interviews %>%
  left_join(cynoscion_catch, by = "UniqueInterview")

spt <- cynoscion_interviews %>%
  mutate(
    Harvested = coalesce(Harvested, 0),
    Released = coalesce(Released, 0),
    TotalCatch = coalesce(TotalCatch, 0)
  )

## also add the number of unique records (species). could be informative
spt <- spt %>%
  left_join(
    records_per_interview,
    by = "UniqueInterview"
  )
# rename column so its more intuitive
spt <- spt %>%
  rename(
    NumberOfSpeciesRecordedInInterview = n_unique_records
  )

spt <- spt %>%
  mutate(
    SPTPresence = ifelse(TotalCatch > 0, "Present", "Absent")
  )

### save spt dataset
write_xlsx(
  spt,
  "ENPCreelSpottedSeatrout.xlsx"
)



################################################################################
################################################################################
################################################################################

############################ exploration of spt data ###########################
##### NOTE keep in mind that these are before ant data filtration


### visualization of spt presence absence

## presence/absence
table(spt$SPTPresence)

# stacked bar plot for pres abs
ggplot(spt, aes(x = Year, fill = SPTPresence)) +
  geom_bar(col="black") +
  scale_fill_viridis_d(option = "D",
                       begin = 0.1,
                       end = 0.8) +
  labs(
    x = "Year",
    y = "Number of interviews",
    fill = "C. nebulosus",
    title = "Spotted seatrout presence/absence by year"
  ) +
  theme_bw() +
  theme(
    legend.position = c(0.85, 0.85),
    legend.background = element_rect(fill = "white", color = "black"),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )

ggsave(
  here("SPT", "SPT_presence_absence_by_year.png"),
  width = 10,
  height = 8,
  dpi = 1000
)


### annual proportion positive
annual_presence <- spt %>%
  group_by(Year) %>%
  summarise(
    n_interviews = n(),
    n_positive = sum(TotalCatch > 0, na.rm = TRUE),
    proportion_positive = n_positive / n_interviews,
    .groups = "drop"
  )
# plot
ggplot(annual_presence, aes(x = Year, y = proportion_positive)) +
  geom_line(size=2) +
  geom_point(size=3) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, 1)
  ) +
  labs(
    x = "Year",
    y = "Proportion positive",
    title = "Annual proportion of interviews positive for spotted seatrout"
  ) +
  theme_bw()

ggsave(
  here("SPT", "SPT_proportion_interviews_positive_by_year.png"),
  width = 10,
  height = 8,
  dpi = 1000
)



####################################
# NOTE I noticed that the following variables start being recorded in 1991. CONFIRM!
# numPeople,
# hoursFished,
# areaFished,
# interviewTime,
# hoursTrip,
# fishingPartyComposition,
# scientificName_pref,
# commonName_pref,
# originTrip,
# interviewer

# if so, that could determine the start year for an index standardization in
# order to consider how all those variables affect catch, harvest, & release

cols_to_check <- c(
  "numPeople",
  "hoursFished",
  "areaFished",
  "interviewTime",
  "hoursTrip",
  "fishingPartyComposition",
  "scientificName_pref",
  "commonName_pref",
  "originTrip",
  "interviewer"
)

completeness_by_year <- spt %>%
  group_by(Year) %>%
  summarise(
    across(
      all_of(cols_to_check),
      ~ sum(!is.na(.)),
      .names = "{.col}_filled"
    ),
    n_interviews = n(),
    .groups = "drop"
  )

completeness_by_year <- spt %>%
  group_by(Year) %>%
  summarise(
    across(
      all_of(cols_to_check),
      ~ mean(!is.na(.)),
      .names = "{.col}"
    ),
    n_interviews = n(),
    .groups = "drop"
  )

completeness_by_year <- spt %>%
  group_by(Year) %>%
  summarise(
    across(
      all_of(cols_to_check),
      ~ mean(!is.na(.)) * 100
    ),
    n_interviews = n(),
    .groups = "drop"
  )

View(completeness_by_year)



completeness_long <- completeness_by_year %>%
  pivot_longer(
    cols = all_of(cols_to_check),
    names_to = "Variable",
    values_to = "PercentFilled"
  )

ggplot(completeness_long, aes(x = Year, y = Variable, fill = PercentFilled)) +
  geom_tile() +
  scale_fill_viridis_c(
    limits = c(0, 100),
    name = "% filled"
  ) +
  labs(
    x = "Year",
    y = NULL,
    title = "Completeness of interview variables by year"
  ) +
  theme_bw()
#### confirms that various vars start being recorded in 1991.
### if we want to consider all these varuables in index standardization, 1991
### would be a good start year. I SUPPOSSE this is only true if we want to 
### consider target species. at the least could be a sensitivity of the index
### e.g., full time series vs 1991 start year 


###############################################################################
###############################################################################
# check target species!!!!

species_counts <- spt %>%
  count(scientificName_pref, sort = TRUE)
species_counts

top10_species <- spt %>%
  count(scientificName_pref, sort = TRUE) %>%
  slice_head(n = 10)

ggplot(top10_species, 
       aes(x = reorder(scientificName_pref, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(
    x = NULL,
    y = "Number of interviews",
    title = "Top 10 preferred species"
  ) +
  theme_bw()

ggplot(top10_species, 
       aes(x = reorder(scientificName_pref, n), y = n)) +
  geom_col() +
  geom_text(
    aes(label = n),
    hjust = -0.2,
    size = 4
  ) +
  coord_flip() +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.1))
  ) +
  labs(
    x = NULL,
    y = "Number of interviews",
    title = "Top 10 preferred species"
  ) +
  theme_bw()


#### proportion of interview where SPT is preferred
# option 1: including NA and unidentified spp
spt %>%
  summarise(
    n_interviews = n(),
    n_preferred_spt = sum(scientificName_pref == "Cynoscion nebulosus", na.rm = TRUE),
    proportion_preferred_spt = n_preferred_spt / n_interviews
  )

# option 2: excluding NA and unidentified spp
spt %>%
  filter(
    !is.na(scientificName_pref),
    scientificName_pref != "Unidentified species"
  ) %>%
  summarise(
    n_interviews = n(),
    n_preferred_spt = sum(scientificName_pref == "Cynoscion nebulosus"),
    proportion_preferred_spt = n_preferred_spt / n_interviews
  )
# about 20% when a spp is listed as preferred!



#### plots of the above
annual_preference <- spt %>%
  group_by(Year) %>%
  summarise(
    # Method 1: all interviews
    n_interviews_all = n(),
    n_spt_all = sum(
      scientificName_pref == "Cynoscion nebulosus",
      na.rm = TRUE
    ),
    prop_spt_all = n_spt_all / n_interviews_all,
    
    # Method 2: exclude NA and Unidentified species
    n_interviews_identified = sum(
      !is.na(scientificName_pref) &
        scientificName_pref != "Unidentified species"
    ),
    n_spt_identified = sum(
      scientificName_pref == "Cynoscion nebulosus",
      na.rm = TRUE
    ),
    prop_spt_identified = n_spt_identified / n_interviews_identified,
    
    .groups = "drop"
  )


annual_preference_long <- annual_preference %>%
  select(
    Year,
    prop_spt_all,
    prop_spt_identified
  ) %>%
  pivot_longer(
    cols = c(prop_spt_all, prop_spt_identified),
    names_to = "Method",
    values_to = "Proportion"
  ) %>%
  mutate(
    Method = recode(
      Method,
      prop_spt_all = "All interviews",
      prop_spt_identified = "Excluding NA and Unidentified"
    )
  )

ggplot(
  annual_preference_long,
  aes(x = Year, y = Proportion, linetype = Method)
) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, 1)
  ) +
  labs(
    x = "Year",
    y = "Proportion preferring C. nebulosus",
    linetype = "Method",
    title = "Annual proportion of interviews preferring spotted seatrout"
  ) +
  theme_bw()


ggplot(
  annual_preference,
  aes(x = Year, y = prop_spt_identified)
) +
  geom_line(linewidth = 2) +
  geom_point(size = 3) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, 1)
  ) +
  labs(
    x = "Year",
    y = "Proportion preferring C. nebulosus",
    title = "Annual proportion of interviews preferring spotted seatrout"
  ) +
  theme_bw()
## interesting downward trend!!! worth mentioning 
## also worth considering how overall preference dynamics have changed over time
## e.g., has preference shifted due to population trends? 

ggsave(
  here("SPT", "SPT_proportion_interviews_preferred_by_year_excludingNAandUnIDspp.png"),
  width = 10,
  height = 8,
  dpi = 1000
)



###############################################################################
###############################################################################
###############################################################################
######## temporal trends of top preferred spp
top10_pref <- spt %>%
  filter(
    !is.na(scientificName_pref),
    scientificName_pref != "Unidentified species"
  ) %>%
  count(scientificName_pref, sort = TRUE) %>%
  slice_head(n = 10) %>%
  pull(scientificName_pref)

annual_top10 <- spt %>%
  filter(
    !is.na(scientificName_pref),
    scientificName_pref != "Unidentified species"
  ) %>%
  group_by(Year) %>%
  mutate(
    n_identified = n()
  ) %>%
  filter(scientificName_pref %in% top10_pref) %>%
  count(Year, scientificName_pref, n_identified, name = "n") %>%
  mutate(
    proportion = n / n_identified
  ) %>%
  ungroup()

ggplot(
  annual_top10,
  aes(x = Year, y = proportion, color = scientificName_pref)
) +
  geom_line(linewidth = 2) +
  geom_point(size = 3) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, 1)
  ) +
  labs(
    x = "Year",
    y = "Proportion of interviews",
    color = "Preferred species",
    title = "Annual proportion of top 10 preferred species"
  ) +
  theme_bw() +
  theme(
    legend.position = c(0.98, 0.98),
    legend.justification = c(1, 1),
    legend.background = element_rect(
      fill = "white",
      color = "black"
    )
  )

ggsave(
  here("SPT", "Top10_proportion_interviews_preferred_by_year_excludingNAandUnIDspp.png"),
  width = 10,
  height = 8,
  dpi = 1000
)

# top 5
top5_pref <- spt %>%
  filter(
    !is.na(scientificName_pref),
    scientificName_pref != "Unidentified species"
  ) %>%
  count(scientificName_pref, sort = TRUE) %>%
  slice_head(n = 5) %>%
  pull(scientificName_pref)

annual_top5 <- spt %>%
  filter(
    !is.na(scientificName_pref),
    scientificName_pref != "Unidentified species"
  ) %>%
  group_by(Year) %>%
  mutate(
    n_identified = n()
  ) %>%
  filter(scientificName_pref %in% top5_pref) %>%
  count(Year, scientificName_pref, n_identified, name = "n") %>%
  mutate(
    proportion = n / n_identified
  ) %>%
  ungroup()

ggplot(
  annual_top5,
  aes(x = Year, y = proportion, color = scientificName_pref)
) +
  geom_line(linewidth = 2) +
  geom_point(size = 3) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, 1)
  ) +
  labs(
    x = "Year",
    y = "Proportion of interviews",
    color = "Preferred species",
    title = "Annual proportion of top 5 preferred species"
  ) +
  theme_bw() +
  theme(
    legend.position = c(0.98, 0.98),
    legend.justification = c(1, 1),
    legend.background = element_rect(
      fill = "white",
      color = "black"
    )
  )

ggsave(
  here("SPT", "Top5_proportion_interviews_preferred_by_year_excludingNAandUnIDspp.png"),
  width = 10,
  height = 8,
  dpi = 1000
)

### NOTE interesting trends!
### tarpon and snook have increased in recent years
### red drum displays downard trend in most recent years
### the most pronounced IMO is SPT as is has it consistently dispalys a downward
### trend across the time series
### the other species show more oscillation patterns snook and rdm
### tarpon seems to be a consistent upward trend

### CONSIDER MAKING THESE PLOTS with a truncated time series starting in 1991


################################################################################
################################################################################
################################################################################

#### nominal indices in mean harvest, releases, and total catch of spt with SE

annual_catch <- spt %>%
  group_by(Year) %>%
  summarise(
    n = n(),
    
    mean_harvested = mean(Harvested, na.rm = TRUE),
    se_harvested = sd(Harvested, na.rm = TRUE) / sqrt(sum(!is.na(Harvested))),
    
    mean_released = mean(Released, na.rm = TRUE),
    se_released = sd(Released, na.rm = TRUE) / sqrt(sum(!is.na(Released))),
    
   
     mean_total_catch = mean(TotalCatch, na.rm = TRUE),
    se_total_catch = sd(TotalCatch, na.rm = TRUE) / sqrt(sum(!is.na(TotalCatch))),
    
    .groups = "drop"
  )

ggplot(annual_catch, aes(x = Year)) +
  
  geom_line(
    aes(y = mean_harvested, color = "Harvested"),
    linewidth = 2
  ) +
  geom_point(
    aes(y = mean_harvested, color = "Harvested"),
    size = 3
  ) +
  geom_errorbar(
    aes(
      ymin = mean_harvested - se_harvested,
      ymax = mean_harvested + se_harvested,
      color = "Harvested"
    ),
    width = 1
  ) +
  
  geom_line(
    aes(y = mean_released, color = "Released"),
    linewidth = 2
  ) +
  geom_point(
    aes(y = mean_released, color = "Released"),
    size = 3
  ) +
  geom_errorbar(
    aes(
      ymin = mean_released - se_released,
      ymax = mean_released + se_released,
      color = "Released"
    ),
    width = 1
  ) +
  
  geom_line(
    aes(y = mean_total_catch, color = "Total catch"),
    linewidth = 2
  ) +
  geom_point(
    aes(y = mean_total_catch, color = "Total catch"),
    size = 3
  ) +
  geom_errorbar(
    aes(
      ymin = mean_total_catch - se_total_catch,
      ymax = mean_total_catch + se_total_catch,
      color = "Total catch"
    ),
    width = 1
  ) +
  
  labs(
    x = "Year",
    y = "Mean catch per interview",
    color = "Catch category",
    title = "Annual nominal indices of spotted seatrout catch with +/- 1 SE"
  ) +
  
  theme_bw() +
  theme(
    legend.position = c(0.98, 0.98),
    legend.justification = c(1, 1),
    legend.background = element_rect(
      fill = "white",
      color = "black"
    )
  )

ggsave(
  here("SPT", "SPT_nominal_indices_by_year_withSE.png"),
  width = 10,
  height = 8,
  dpi = 1000
)



################################################################################
################################################################################
################################################################################





