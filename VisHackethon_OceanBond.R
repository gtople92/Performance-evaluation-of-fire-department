##ca_ems
library(gtools)
library(ggplot2)
library(dplyr)
rm(cafl_ems)

ca_ems <- read.csv("ca_ems.csv",stringsAsFactors = F,na.strings = '')

fl_ems <- read.csv("fl_ems.csv",stringsAsFactors = F,na.strings = '')

length(unique(fl_ems$FD_NAME)) #85
length(unique(ca_ems$FD_NAME)) #146

length(unique(fl_ems$FDID))

any(is.na(fl_ems$ALARM)) # No NA's in Alarm Date and Time
any(is.na(ca_ems$ALARM))# No NA's in Alarm Date and Time

unique(fl_ems$LOC_TYPE)
unique(ca_ems$LOC_TYPE)

any(is.na(fl_ems$ARRIVAL)) # No NA's in Arrival Date and Time
any(is.na(ca_ems$ARRIVAL))


any(is.na(fl_ems$LU_CLEAR)) # No NA's in Last Unit Cleared Date and Time
any(is.na(ca_ems$LU_CLEAR))

unique(fl_ems$EXP_NO)

any(is.na())

any(is.na(fl_ems$DEPT_STA))

fl_ems$ARRIVAL <- as.POSIXct(fl_ems$ARRIVAL,
                                      format='%m/%d/%Y %H:%M')

fl_ems$ALARM       <- as.POSIXct(fl_ems$ALARM,
                                      format='%m/%d/%Y %H:%M')
fl_ems$time_taken <- fl_ems$ARRIVAL - fl_ems$ALARM 

ca_ems$ARRIVAL <- as.POSIXct(ca_ems$ARRIVAL,
                             format='%m/%d/%Y %H:%M')

ca_ems$ALARM       <- as.POSIXct(ca_ems$ALARM,
                                 format='%m/%d/%Y %H:%M')

ca_ems$response_time <- ca_ems$ARRIVAL - ca_ems$ALARM 

fl_ems$LU_CLEAR <- as.POSIXct(fl_ems$LU_CLEAR,
                              format='%m/%d/%Y %H:%M')

ca_ems$LU_CLEAR <- as.POSIXct(ca_ems$LU_CLEAR,
                              format='%m/%d/%Y %H:%M')

ca_ems$INC_DATE <- as.POSIXct(ca_ems$INC_DATE,
                              format='%m/%d/%Y')
fl_ems$INC_DATE <- as.POSIXct(fl_ems$INC_DATE,
                              format='%m/%d/%Y')

merge_ems$site_time <- merge_ems$LU_CLEAR - merge_ems$ARRIVAL 

merge_ems$total_time <- merge_ems$LU_CLEAR - merge_ems$ALARM

merge_ems$site_per_eff <-  round(as.integer(merge_ems$site_time) / as.integer(merge_ems$total_time),4)*100


summary(fl_ems$INC_DATE)

simple.bind(ca_ems)
rm(merge_bind)

merge_ems <- rbind(ca_ems,fl_ems)

write.csv(merge_ems, file = "merge_ems.csv")

merge_ems$time_taken <- as.integer(merge_ems$time_taken)

summary(merge_ems)

nrow(merge_ems[,c(1,2,3,4,5,39)])

unique(merge_ems$TRANSPORT)
t1 <- merge_ems[which(merge_ems$INC_TYPE == "363"),]

summary(merge_ems$time_taken)

temp <- merge_ems[which(merge_ems$PROP_LOSS =="N"),]

summary(temp$OTH_PER)

unique(merge_ems$LOC_TYPE)
unique(merge_ems$MIXED_USE)

merge_ems[which(merge_ems$FD_NAME =="LITTLE LAKE FPD"),]

merge_ems[,c(1,2,3,4,5,"PATIENT_NO")]

temp <- merge_ems[which(merge_ems$CITY == merge_ems$FD_CITY),]

merge_ems[which(merge_ems$ALARM > "2016-12-01"),]


merge_ems$INC_NO <- as.character(merge_ems$INC_NO)  

merge_ems$INC_TYPE <- as.character(merge_ems$INC_TYPE) 

any(is.na(merge_ems$INC_NO))

length(unique(merge_ems$INC_NO))

#Heatmap
arrange(ems_heatmap,desc(inc_cnt))

ems_heatmap1 <- summarise(group_by(merge_ems,INC_TYPE,CITY),inc_cnt = n_distinct(INC_NO))

ems_heatmap <- summarise(group_by(merge_ems,INC_TYPE,CITY),inc_cnt = n_distinct(INC_NO))

ggplot(ems_heatmap,aes(x= INC_TYPE,y= CITY, fill = inc_cnt)) + geom_tile()
