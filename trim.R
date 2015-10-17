# Twitter project, trimming csv files
# Aug 29, 2015
# Julian Wills 


###Based on timestamp (3/9/2013  11:36:42 PM); right around 90k tweets

setwd("C:/Users/Julian/GDrive/Unprocessed/")
d1 <- read.csv("GunControl_moral_filter.csv", TRUE) 
write.csv(d1[1:150000,], file="GunControl_moral_filter_trim.csv", row.names = F)

d2 <- read.csv("GunControl.csv", TRUE) 
write.csv(d2[1:188805,], file="GunControl_trim.csv", row.names = F)


