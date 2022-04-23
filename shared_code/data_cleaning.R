# Data Cleaning File
library(tidyverse)
library(lubridate)

# load the data 
dt = read.csv("hurrican703.csv")

# clean the original data 
df_temp = dt %>%
  mutate( # Factoring the Nature and Months Variable 
    Nature = factor(Nature),
    Month = factor(Month, levels = c(month.name)),
  ) %>%  # Seperating the day and time to usable info
  tidyr::separate(
    time, c('Date', 'Time'), 
                       sep = ' ', extra = 'merge') %>% 
  tidyr::separate(
    Date, c('year_num', 'month_num', 'day'), 
                       sep = '-', extra = 'merge') %>% 
  tidyr::separate(
    Time, c('hour', 'min', 'sec'), 
                       sep =':', extra = 'merge')  %>%
  select(ID, Nature, Latitude, Longitude, Wind.kt, 
         Season, Month, day, hour, min) %>% # selecting relevent columns
  filter(min == "00") %>% 
  filter(hour %in% c("00", "06", "12", "18")) %>% # only want observations on every 6 hours 
  mutate(
    hour = as.numeric(hour),
    day = as.numeric(day)
  )

 # find the id's with limited observations
temp = table(df_temp$ID) %>% as.data.frame()

# eliminate anyone that has less or equal to 7
id_get_rid <- temp %>% arrange(Freq) %>% filter(Freq < 7) %>% pull(Var1)
df_rm <- df_temp %>% filter(!(ID %in% c(id_get_rid))) 

# figuring out the percentage we are removing 
(dim(dt)[1]-dim(df_rm)[1]) / dim(dt)[1]

save(df_rm, file = "df_rm.RData")


