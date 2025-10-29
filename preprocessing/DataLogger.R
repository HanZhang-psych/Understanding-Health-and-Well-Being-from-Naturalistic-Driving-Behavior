######## GPS Data ######## 
# Clear environment
rm(list = ls())

# load libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(ggdist)
library(psych)
library(easystats)
library(lmerTest)
library(ggcorrplot)

# set working directory to file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# function to remove outliers based on z-score
outlier_z <- function(data, var, by = NULL, thresh = 3.5) {
  var <- enquo(var)
  
  data <- data %>%
    group_by(across(all_of(by))) %>%  # group by 'by' variables
    mutate(
      z = scale(!!var)[, 1],
      !!quo_name(var) := case_when(
        z > thresh | z < -thresh ~ NA,
        TRUE ~ !!var
      )
    ) %>%
    ungroup() %>%
    select(-z)
  
  return(data)
}


# load data from csv
GPS_raw = read.csv(file = '../../Data Files/CSV/GPS_DataLogger_Final.csv')
TOURMO_raw = read.csv(file = '../../Data Files/CSV/GPS_Tourmo_Final.csv')

########## Making GPS and Tourmo Data Consistent ##########
# add source info
gps = GPS_raw %>% mutate(device='GPS')
tourmo = TOURMO_raw %>% mutate(device='Tourmo')

# rename columns
lookup <- c(Site = "SITE", Year = "YEAR", Month="MONTH",FractionOfMonth="FRACTION_OF_MONTH",
            DaysDriving="DAYS_DRIVING", Miles_n="MILES", LeftTurnCount="NUM_LEFT_TURNS",
            RightTurnCount="NUM_RIGHT_TURNS", RightToLeftTurnRatio_n = "RIGHT_LEFT_RATIO",
            Trips="TRIPS", TripChains="TRIP_CHAINS",TripsAtNight="NUM_TRIPS_NIGHT",
            TripsInDay="NUM_TRIPS_DAY",TripsAMPeak="NUM_TRIPS_IN_AM_PEAK",TripsPMPeak="NUM_TRIPS_IN_PM_PEAK",
            TripsVgt60="TRIPS_HIGH_SPEED_ROADS",TripsLt15Miles="NUM_TRIPS_15_MILES",TripsLt25Miles="NUM_TRIPS_25_MILES",
            MilesPerTrip_n="MILES_PER_TRIP", TripMinutes_n="TOTAL_TRIP_MINUTES",MinutesPerTrip_n="MINUTES_PER_TRIP",
            MinutesPerChain_n="MINUTES_PER_CHAIN",MilesPerChain_n="MILES_PER_CHAIN",PercentTripsAtNight_n="PERC_TRIPS_NIGHT",
            PercentTripsInDay_n="PERC_TRIPS_DAY",PercentTripsAMPeak_n="PERC_TRIPS_AT_AM_PEAK", PercentTripsPMPeak_n="PERC_TRIPS_AT_PM_PEAK",
            PercentTripsVgt60_n="PERC_TRIPS_HIGH_SPEED_ROADS",PercentDistLt15Miles_n="PERC_TRIPS_15_MILES",
            PercentDistLt25Miles_n="PERC_TRIPS_25_MILES", SpeedGt80mphCount="SPEEDING_EVENTS")
tourmo = tourmo %>% rename(all_of(lookup))

# check differences in column names
setdiff(names(gps), names(tourmo)) # 3 deacc events from gps
setdiff(names(tourmo), names(gps)) # 2 deacc events from tourmo
common_cols = intersect(names(gps), names(tourmo))

# checking differences in data types
# all the pct vars are different
cbind(sapply(gps[,common_cols], class), sapply(tourmo[,common_cols], class))

# fix data types by stripping % sign and convert to numeric
tourmo = tourmo %>% 
  mutate(across(starts_with("Percent"), ~ as.numeric(gsub("%", "", .))))
gps = gps %>% 
  mutate(across(starts_with("Percent"), ~ as.numeric(.)))
# check again
cbind(sapply(gps[,common_cols], class), sapply(tourmo[,common_cols], class))

# combine data while ignoring different columns
driving_data = bind_rows(gps, tourmo) %>% arrange(XID, Site, Year, Month)

######### Check Device Differences #########

# # compute mean group by XID and device for all common columns
# agg_by_device = driving_data %>% 
#   filter(FractionOfMonth == 1) %>% # only full month data
#   select(all_of(common_cols)) %>%
#   group_by(XID, Site, device) %>%
#   summarise(across(everything(), mean, na.rm = TRUE), .groups = 'drop') 
# 
# # select only those who have both device present
# agg_by_device = agg_by_device %>% 
#   filter(n() == 2, .by=c(XID, Site))

# plot differences
# agg_by_device %>% 
#   gather(key, value, DaysDriving:SpeedGt80mphCount) %>%
#   ggplot(aes(x = device, y = value, fill = device)) +
#   geom_boxplot() +
#   labs(title = "Miles Driven by Device", x = "Device", y = "Miles Driven") +
#   theme_minimal() +
#   facet_wrap(~ key, scales = "free_y") 


# conduct paired t-test for each variable in common_cols
# t_test_results = list()
# for (col in common_cols) {
#   if (!col %in% c('XID','Site','Year','FractionOfMonth','device')) {
#     test_data = agg_by_device %>% 
#       select(XID, device, all_of(col)) %>%
#       pivot_wider(names_from = device, values_from = all_of(col)) 
#     t_test_results[[col]] = t.test(test_data$GPS, test_data$Tourmo, paired = TRUE)
#   }
# }

# ok, decided not to include Tourmo data since it's hard to know if any difference is due to device or time
driving_data = gps %>% arrange(XID, Site, Year, Month)

############## Matching Monthly Driving Data with Study Interval ##############
visits = read.csv(file = '../../Data Files/CSV/VISITS VALUEs.csv') %>% 
  mutate(VISITDATE = as.Date(VISITDATE, format = "%d-%b-%y")) %>%
  pivot_wider(values_from = VISITDATE, names_from = INTERVAL, names_prefix = "v")

# create new data column in the same format
# set date as the last day of the month
driving_data = driving_data %>% 
  mutate(Date = clock::date_build(Year, Month, 31, invalid = "previous"))

# for each row in driving_data, check if Date falls in between any of the visit intervals for that subject
datafiles = c()
for (i in unique(driving_data$XID)) {
  subject_data = driving_data %>% filter(XID == i)
  subject_visit_data = visits %>% filter(XID == i)
  
  subject_data = subject_data %>% mutate(Interval = 
                          case_when(Date >= subject_visit_data$v0 & Date < subject_visit_data$v1 ~ 0,
                                    Date >= subject_visit_data$v1 & Date < subject_visit_data$v2 ~ 1,
                                    Date >= subject_visit_data$v2 & Date < subject_visit_data$v3 ~ 2,
                                    Date >= subject_visit_data$v3 & Date < subject_visit_data$v4 ~ 3,
                                    Date >= subject_visit_data$v4 & Date < subject_visit_data$v5 ~ 4,
                                    Date >= subject_visit_data$v5 ~ 5,
                                    .default = NA)
  )
  
  # add another column to indicate the nth year since first month of driving data 
  subject_data$year_since = as.numeric(subject_data$Date - min(subject_data$Date)) %/% 365
  # add another column to indicate the nth month since the first month of driving data
  subject_data$month_since = as.numeric(subject_data$Date - min(subject_data$Date)) %/% 30
  datafiles = append(datafiles, list(subject_data))
  }

# combine all subject data into one data frame
driving_data = bind_rows(datafiles) %>% drop_na(Interval)

######### Data Cleaning #########
# n of subjects who have driving data available for each interval
driving_data %>% select(XID, Interval) %>% distinct() %>% count(Interval)
                                                               
# n of monthly driving data available for each interval
driving_data %>% count(Interval) %>% mutate(Percent = n / sum(n) * 100)

n_raw = nrow(driving_data)

# select the baseline interval (0) and the first follow-up interval (2)
driving_data = driving_data %>% 
  filter(Interval %in% c(0, 1, 2)) 

# for each subject and each interval, there must be at least 3 months of driving data
driving_data = driving_data %>% 
  filter(FractionOfMonth == 1) %>%
  filter(n() >= 3, .by = c(XID, Interval))

n_after = nrow(driving_data)

# percent of data removed
(n_raw - n_after) / n_raw * 100

## inspect the distribution of each measure and see if outlier removal is warranted

### Miles_n ###
## 0 mile but > 0 trips? remove for now
driving_data = driving_data %>% filter(Miles_n > 0 & Trips > 0)
# adjust for partial data
driving_data$Miles_n = driving_data$Miles_n / driving_data$FractionOfMonth 
# outlier removal
hist(driving_data$Miles_n)
driving_data = outlier_z(driving_data, Miles_n, by=c('Site', 'Interval'))
hist(driving_data$Miles_n)

### Left Turn Count ###
# adjust for partial data
driving_data$LeftTurnCount = driving_data$LeftTurnCount / driving_data$FractionOfMonth
# outlier removal
hist(driving_data$LeftTurnCount)
driving_data = outlier_z(driving_data, LeftTurnCount, by=c('Site', 'Interval'))
hist(driving_data$LeftTurnCount)

### Right Turn Count ###
# adjust for partial data
driving_data$RightTurnCount = driving_data$RightTurnCount / driving_data$FractionOfMonth
# outlier removal
hist(driving_data$RightTurnCount)
driving_data = outlier_z(driving_data, RightTurnCount, by=c('Site', 'Interval'))
hist(driving_data$RightTurnCount)

### Trips ###
# adjust for partial data
driving_data$Trips = driving_data$Trips / driving_data$FractionOfMonth
# outlier removal
hist(driving_data$Trips)
driving_data = outlier_z(driving_data, Trips, by=c('Site', 'Interval'))
hist(driving_data$Trips)

### Trip Chains ###
# adjust for partial data
driving_data$TripChains = driving_data$TripChains / driving_data$FractionOfMonth
# outlier removal
hist(driving_data$TripChains)
driving_data = outlier_z(driving_data, TripChains, by=c('Site', 'Interval'))
hist(driving_data$TripChains)

### Trips At Night ###
# adjust for partial data
driving_data$TripsAtNight = driving_data$TripsAtNight / driving_data$FractionOfMonth
# outlier removal
hist(driving_data$TripsAtNight)
driving_data = outlier_z(driving_data, TripsAtNight, by=c('Site', 'Interval'))
hist(driving_data$TripsAtNight)

### Trips In Day ###
# adjust for partial data
driving_data$TripsInDay = driving_data$TripsInDay / driving_data$FractionOfMonth
# outlier removal
hist(driving_data$TripsInDay)
driving_data = outlier_z(driving_data, TripsInDay, by=c('Site', 'Interval'))
hist(driving_data$TripsInDay)

### Trips AM Peak ###
# adjust for partial data
driving_data$TripsAMPeak = driving_data$TripsAMPeak / driving_data$FractionOfMonth
# outlier removal
hist(driving_data$TripsAMPeak)
driving_data = outlier_z(driving_data, TripsAMPeak, by=c('Site', 'Interval'))
hist(driving_data$TripsAMPeak)

### Trips VGT 60 mph ###
# adjust for partial data
driving_data$TripsVgt60 = driving_data$TripsVgt60 / driving_data$FractionOfMonth
# outlier removal
hist(driving_data$TripsVgt60)
driving_data = outlier_z(driving_data, TripsVgt60, by=c('Site', 'Interval'))
hist(driving_data$TripsVgt60)

### Trips LT 15 Miles ###
# adjust for partial data
driving_data$TripsLt15Miles = driving_data$TripsLt15Miles / driving_data$FractionOfMonth
# outlier removal
hist(driving_data$TripsLt15Miles)
driving_data = outlier_z(driving_data, TripsLt15Miles, by=c('Site', 'Interval'))
hist(driving_data$TripsLt15Miles)

### Trips LT 25 Miles ###
# adjust for partial data
driving_data$TripsLt25Miles = driving_data$TripsLt25Miles / driving_data$FractionOfMonth
# outlier removal
hist(driving_data$TripsLt25Miles)
driving_data = outlier_z(driving_data, TripsLt25Miles, by=c('Site', 'Interval'))
hist(driving_data$TripsLt25Miles)

### Trip Minutes ###
# adjust for partial data
driving_data$TripMinutes_n = driving_data$TripMinutes_n / driving_data$FractionOfMonth
# outlier removal
hist(driving_data$TripMinutes_n)
driving_data = outlier_z(driving_data, TripMinutes_n, by=c('Site', 'Interval'))
hist(driving_data$TripMinutes_n)

### Speeding Event Count ###
# adjust for partial data
driving_data$SpeedGt80mphCount = driving_data$SpeedGt80mphCount / driving_data$FractionOfMonth
# outlier removal
hist(driving_data$SpeedGt80mphCount)
driving_data = outlier_z(driving_data, SpeedGt80mphCount, by=c('Site', 'Interval'))
hist(driving_data$SpeedGt80mphCount)

### !!!Below are derived variables
### Average Speed ###
driving_data = driving_data %>% mutate(Average_speed =  60*Miles_n/TripMinutes_n) %>%
  mutate(Average_speed=case_when(TripMinutes_n == 0 ~ NA, .default=Average_speed)) # avoid division by zero
hist(driving_data$Average_speed)
driving_data = outlier_z(driving_data, Average_speed, by=c('Site', 'Interval'))
hist(driving_data$Average_speed)

### Miles Per Trip ###
driving_data = driving_data %>% mutate(MilesPerTrip_n = Miles_n / Trips) %>%
  mutate(MilesPerTrip_n=case_when(Trips == 0 ~ NA, .default=MilesPerTrip_n)) # avoid division by zero
hist(driving_data$MilesPerTrip_n)
driving_data = outlier_z(driving_data, MilesPerTrip_n, by=c('Site', 'Interval'))
hist(driving_data$MilesPerTrip_n)

### Minutes Per Trip ###
driving_data = driving_data %>% mutate(MinutesPerTrip_n = TripMinutes_n / Trips) %>%
  mutate(MinutesPerTrip_n=case_when(Trips == 0 ~ NA, .default=MinutesPerTrip_n)) # avoid division by zero
hist(driving_data$MinutesPerTrip_n)
driving_data = outlier_z(driving_data, MinutesPerTrip_n, by=c('Site', 'Interval'))
hist(driving_data$MinutesPerTrip_n)

### Minutes Per Chain ###
# note: this corrects the original data where 0 was used for inf values
driving_data = driving_data %>% 
  mutate(MinutesPerChain_n = TripMinutes_n / TripChains) %>%
  mutate(MinutesPerChain_n=case_when(TripChains == 0 ~ NA, .default=MinutesPerChain_n)) # avoid division by zero)
hist(driving_data$MinutesPerChain_n)
driving_data = outlier_z(driving_data, MinutesPerChain_n, by=c('Site', 'Interval'))
hist(driving_data$MinutesPerChain_n)

### Miles Per Chain ###
# same as above
driving_data = driving_data %>% 
  mutate(MilesPerChain_n = Miles_n / TripChains) %>%
  mutate(MilesPerChain_n=case_when(TripChains == 0 ~ NA, .default=MilesPerChain_n)) # avoid division by zero)
hist(driving_data$MilesPerChain_n)
driving_data = outlier_z(driving_data, MilesPerChain_n, by=c('Site', 'Interval'))
hist(driving_data$MilesPerChain_n)

### Percent Trips At Night ###
driving_data = driving_data %>% mutate(PercentTripsAtNight_n = (TripsAtNight / Trips) * 100) %>%
  mutate(PercentTripsAtNight_n=case_when(Trips == 0 ~ NA, .default=PercentTripsAtNight_n)) # avoid division by zero
hist(driving_data$PercentTripsAtNight_n)
driving_data = outlier_z(driving_data, PercentTripsAtNight_n, by=c('Site', 'Interval'))
hist(driving_data$PercentTripsAtNight_n)

### Percent Trips In Day ###
driving_data = driving_data %>% mutate(PercentTripsInDay_n = (TripsInDay / Trips) * 100) %>%
  mutate(PercentTripsInDay_n=case_when(Trips == 0 ~ NA, .default=PercentTripsInDay_n)) # avoid division by zero
hist(driving_data$PercentTripsInDay_n)
driving_data = outlier_z(driving_data, PercentTripsInDay_n, by=c('Site', 'Interval'))
hist(driving_data$PercentTripsInDay_n)

### Percent Trips AM Peak ###
driving_data = driving_data %>% mutate(PercentTripsAMPeak_n = (TripsAMPeak / Trips) * 100) %>%
  mutate(PercentTripsAMPeak_n=case_when(Trips == 0 ~ NA, .default=PercentTripsAMPeak_n)) # avoid division by zero
hist(driving_data$PercentTripsAMPeak_n)
driving_data = outlier_z(driving_data, PercentTripsAMPeak_n, by=c('Site', 'Interval'))
hist(driving_data$PercentTripsAMPeak_n)

### Percent Trips PM Peak ###
driving_data = driving_data %>% mutate(PercentTripsPMPeak_n = (TripsPMPeak / Trips) * 100) %>%
  mutate(PercentTripsPMPeak_n=case_when(Trips == 0 ~ NA, .default=PercentTripsPMPeak_n)) # avoid division by zero
hist(driving_data$PercentTripsPMPeak_n)
driving_data = outlier_z(driving_data, PercentTripsPMPeak_n, by=c('Site', 'Interval'))
hist(driving_data$PercentTripsPMPeak_n)

### Percent Trips VGT 60 mph ###
driving_data = driving_data %>% mutate(PercentTripsVgt60_n = (TripsVgt60 / Trips) * 100) %>%
  mutate(PercentTripsVgt60_n=case_when(Trips == 0 ~ NA, .default=PercentTripsVgt60_n)) # avoid division by zero
hist(driving_data$PercentTripsVgt60_n)
driving_data = outlier_z(driving_data, PercentTripsVgt60_n, by=c('Site', 'Interval'))
hist(driving_data$PercentTripsVgt60_n)

### Percent Dist LT 15 Miles ###
driving_data = driving_data %>% mutate(PercentDistLt15Miles_n = (TripsLt15Miles / Trips) * 100) %>%
  mutate(PercentDistLt15Miles_n=case_when(Trips == 0 ~ NA, .default=PercentDistLt15Miles_n)) # avoid division by zero
hist(driving_data$PercentDistLt15Miles_n)
driving_data = outlier_z(driving_data, PercentDistLt15Miles_n, by=c('Site', 'Interval'))
hist(driving_data$PercentDistLt15Miles_n)

### Percent Dist LT 25 Miles ###
driving_data = driving_data %>% mutate(PercentDistLt25Miles_n = (TripsLt25Miles / Trips) * 100) %>%
  mutate(PercentDistLt25Miles_n=case_when(Trips == 0 ~ NA, .default=PercentDistLt25Miles_n)) # avoid division by zero
hist(driving_data$PercentDistLt25Miles_n)
driving_data = outlier_z(driving_data, PercentDistLt25Miles_n, by=c('Site', 'Interval'))
hist(driving_data$PercentDistLt25Miles_n)

### Speeding per 1000 miles ###
driving_data = driving_data %>% mutate(SpeedingPer1000Miles = SpeedGt80mphCount * 1000 / Miles_n) %>%
  mutate(SpeedingPer1000Miles=case_when(Miles_n == 0 ~ NA, .default=SpeedingPer1000Miles)) # avoid division by zero
hist(driving_data$SpeedingPer1000Miles)
driving_data = outlier_z(driving_data, SpeedingPer1000Miles, by=c('Site', 'Interval'))
hist(driving_data$SpeedingPer1000Miles)

### Deceleration (>0.4G) per 1000 miles ###
driving_data = driving_data %>% mutate(DecelerationPer1000Miles = DecelCntLtN4pt0Mps2*1000 / Miles_n) %>%
  mutate(DecelerationPer1000Miles=case_when(Miles_n == 0 ~ NA, .default=DecelerationPer1000Miles)) # avoid division by zero
hist(driving_data$DecelerationPer1000Miles)
driving_data = outlier_z(driving_data, DecelerationPer1000Miles, by=c('Site', 'Interval'))
hist(driving_data$DecelerationPer1000Miles)

######## Saving Data ########
# cleaned monthly driving data
write.csv(driving_data, '../data/cleaned_monthly_driving_data.csv', row.names = F)
