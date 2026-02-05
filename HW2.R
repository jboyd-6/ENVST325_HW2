#Class Activity & HW2

#Chapter 3 Tutorial

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


peaceH = streamH %>%
  filter(siteID == 2295637)

plot(peaceH$dateF, peaceH$gheight.ft, type="b", pch=19, xlab="Date", ylab="Stage Height (ft)")

#joining two tables
# join site info and stream heights into a new data frame floods
floods <- full_join(streamH, siteInfo, by="siteID") 

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
  

