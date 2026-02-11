#Class Activity & HW2

#Chapter 3 Tutorial----

#Only load once (never have to do this again)
install.packages(c("dplyr", "lubridate")) 

#Run every time 
library(dplyr)
library(lubridate)

#Path to open up the stream gauge csv, did not fit into activity02
streamH = read.csv("/cloud/project/activtiy02/stream_gauge.csv")
siteInfo = read.csv("/cloud/project/activtiy02/site_info.csv")

#Start to clean data
streamH$dateF = ymd_hm(streamH$datetime, tz="America/New_York")
streamH$year = year(streamH$dateF)


peaceH <- streamH %>% 
  filter(siteID == 2295637)

plot(peaceH$dateF, peaceH$gheight.ft, type="b", pch=19, xlab="Date", ylab="Stage Height (ft)")

#Joining two tables (Answer to Prompt 1)----
#I am joining the site info and stream heights into a new data frame floods, the type of join in this instance does not make a difference in this data merge.
floods <- full_join(streamH, siteInfo, by="siteID") 
floods$names[floods$names == " WITHLACOOCHEE RIVER AT US 301 AT TRILBY"] = "WITHLACOOCHEE RIVER AT US 301 AT TRILBY"

height.ave = floods %>%
  group_by(names) %>%
  summarise(mean.height = mean(gheight.ft))
height.ave

floods$day = yday(floods$dateF)

height.day <- floods %>% # data frame with pipe
  group_by(names, day) %>% # group data frame by unique names and doy
  summarise(mean.height = mean(gheight.ft), max.height = max(gheight.ft)) # next summarize using mean and max

max.cat <- floods %>% #filter floods
  group_by(names) %>% # group by name
  filter(gheight.ft >= major.ft) #observations with height more than or equal to the major flood height

#Parsing date for Floods Data Frame (Answer to Prompt 2)----

floods$dateF = ymd_hm(streamH$datetime, tz="America/New_York")
floods$year = year(floods$dateF)

#Earliest Date Each Stream reached Flood Height (Answer to Prompt 3)----
flood_cat = floods %>%
  filter(gheight.ft >= flood.ft) %>%
  group_by(names) %>%
  summarise(min_date = min(dateF))

#Start of HW2 ----
# Question 1: Make a separate plot of the stream stage data for each river ----

fish_creek = floods %>%
  filter(names == "FISHEATING CREEK AT PALMDALE")
plot(fish_creek$dateF, fish_creek$gheight.ft, type="b", pch=19, xlab="Date", ylab="Stage Height (ft)", main="Fisheating Creek at Palmdale")

peace_river = floods %>%
  filter(names == "PEACE RIVER AT US 17 AT ZOLFO SPRINGS")
plot(peace_river$dateF, peace_river$gheight.ft, type="b", pch=19, xlab="Date", ylab="Stage Height (ft)", main="Peace River at US 17 at Zolfo Springs")

sf_river = floods %>%
  filter(names == "SANTA FE RIVER NEAR FORT WHITE")
plot(sf_river$dateF, sf_river$gheight.ft, type="b", pch=19, xlab="Date", ylab="Stage Height (ft)", main="Santa Fe River Near Fort White")

white_river = floods %>%
  filter(names == "WITHLACOOCHEE RIVER AT US 301 AT TRILBY")
plot(white_river$dateF, white_river$gheight.ft, type="b", pch=19, xlab="Date", ylab="Stage Height (ft)", main="Withlacoochee River at US 301 at Trilby")

#Question 2 - What was the earliest date of occurrence for each flood category in each river? ----

flood_action = floods %>%
  filter(gheight.ft >= action.ft) %>%
  group_by(names) %>%
  summarise(min_date = min(dateF))

flood_cat = floods %>%
  filter(gheight.ft >= flood.ft) %>%
  group_by(names) %>%
  summarise(min_date = min(dateF))

flood_mod = floods %>%
  filter(gheight.ft >= moderate.ft) %>%
  group_by(names) %>%
  summarise(min_date = min(dateF))

flood_maj = floods %>%
  filter(gheight.ft >= major.ft) %>%
  group_by(names) %>%
  summarise(min_date = min(dateF))

#Question 3 - Which river had the highest stream stage above its listed height in the major flood category? ----

highest_flood = floods %>%
  filter(gheight.ft >= major.ft) %>%
  group_by(names) %>%
  summarise(rel_max_height = max(gheight.ft - major.ft))

#Question 4 ----




