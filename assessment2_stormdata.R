library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(forcats)

wd <- getwd()

url_storm <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"

download.file(url_storm, destfile = paste0(wd, "/StormData.csv.bz2"))

stormdata <- read_csv("StormData.csv.bz2", 
                      col_types = list(PROPDMGEXP = col_character(), CROPDMGEXP = col_character()))

head(stormdata)
tail(stormdata)
str(stormdata)
glimpse(stormdata)


summary(as.factor(stormdata$EVTYPE))
summary(as.numeric(stormdata$FATALITIES))
summary(as.numeric(stormdata$INJURIES))
summary(as.numeric(stormdata$PROPDMG))
summary(as.factor(stormdata$PROPDMGEXP))
summary(as.numeric(stormdata$CROPDMG))
summary(as.factor(stormdata$CROPDMGEXP))

stormdata_evtype <- stormdata %>% 
    select(EVTYPE, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP) %>% 
    mutate(PROPDMGEXP_scale = if_else(is.na(PROPDMGEXP), 1,
                              if_else(PROPDMGEXP == "K", 10^3,
                              if_else(PROPDMGEXP %in% c("m", "M"), 10^6,
                              if_else(PROPDMGEXP == "B", 10^9,
                              if_else(PROPDMGEXP == "1", 10^1,
                              if_else(PROPDMGEXP %in% c("2","H","h"), 10^2,
                              if_else(PROPDMGEXP == "3", 10^3,
                              if_else(PROPDMGEXP == "4", 10^4,
                              if_else(PROPDMGEXP == "5", 10^5,
                              if_else(PROPDMGEXP == "6", 10^6,
                              if_else(PROPDMGEXP == "7", 10^7,        
                              if_else(PROPDMGEXP == "8", 10^8,
                              if_else(PROPDMGEXP == "9", 10^9, 1))))))))))))),
           CROPDMGEXP_scale = if_else(is.na(CROPDMGEXP), 1,
                              if_else(CROPDMGEXP %in% c("K", "k"), 10^3,
                              if_else(CROPDMGEXP %in% c("m", "M"), 10^6,
                              if_else(CROPDMGEXP == "B", 10^9,
                              if_else(CROPDMGEXP == "2", 10^2, 1))))),
           EVTYPE = as.factor(EVTYPE))


stormdata_evtype %>% 
    group_by(EVTYPE) %>% 
    summarise(SUM_FATALITIES = sum(FATALITIES),
              SUM_INJURIES   = sum(INJURIES),
              Total_CASUALTIES = SUM_FATALITIES + SUM_INJURIES) %>% 
    arrange(desc(Total_CASUALTIES))


stormdata_evtype %>% 
  group_by(EVTYPE) %>% 
  summarise(SUM_FATALITIES = sum(FATALITIES),
            SUM_INJURIES   = sum(INJURIES),
            Total_CASUALTIES = SUM_FATALITIES + SUM_INJURIES) %>% 
  arrange(desc(Total_CASUALTIES)) %>% 
  top_n(10) %>% 
  gather(TYPE, Total_CASUALTIES, -EVTYPE, -Total_CASUALTIES) %>% 
  mutate(EVTYPE = fct_reorder(as.factor(EVTYPE),Total_CASUALTIES),
         TYPE = factor(TYPE, levels = c("SUM_INJURIES", "SUM_FATALITIES"))) %>% 
  group_by(EVTYPE) %>% 
  ggplot(aes(x = EVTYPE, y = Total_CASUALTIES, fill = TYPE))+
  geom_col() +
  labs(x = "Event type", y = "Total casulaties = fatalities + injuries",  
       title = "Weather event and population health") +
  coord_flip()



stormdata_evtype %>% 
  mutate(PROPDMG_final = PROPDMG * PROPDMGEXP_scale,
         CROPDMG_final = CROPDMG * CROPDMGEXP_scale) %>% 
  group_by(EVTYPE) %>% 
  summarise(SUM_PROPDMG = sum(PROPDMG_final, na.rm = TRUE),
            SUM_CROPDMG = sum(CROPDMG_final, na.rm = TRUE),
            Total_DMG   = SUM_PROPDMG + SUM_CROPDMG) %>% 
  arrange(desc(Total_DMG))


stormdata_evtype %>% 
  mutate(PROPDMG_final = PROPDMG * PROPDMGEXP_scale,
         CROPDMG_final = CROPDMG * CROPDMGEXP_scale) %>% 
  group_by(EVTYPE) %>% 
  summarise(SUM_PROPDMG = sum(PROPDMG_final, na.rm = TRUE),
            SUM_CROPDMG = sum(CROPDMG_final, na.rm = TRUE),
            Total_DMG   = SUM_PROPDMG + SUM_CROPDMG) %>% 
  arrange(desc(Total_DMG)) %>% 
  top_n(10) %>% 
  gather(TYPE, Total_DMG, -EVTYPE, -Total_DMG) %>% 
  mutate(EVTYPE = fct_reorder(as.factor(EVTYPE),Total_DMG)) %>% 
  group_by(EVTYPE) %>% 
  ggplot(aes(x = EVTYPE, y = Total_DMG, fill = TYPE))+
  geom_col() +
  labs(x = "Event type", y = "Total Damage = Property + Crop",  
       title = "Weather event and Economic consequences") +
  coord_flip()