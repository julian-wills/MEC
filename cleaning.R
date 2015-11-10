# Twitter project, data cleaning
# Oct 16, 2015
# Julian Wills 
# Purpose: preprocessing/cleaning

# Bugs: tweet ideology (tid) is incorrect
# update removeWord() to accomodate full file path
# print appended words in appendWord()


# Preprocessing -----------------------------------------------------------
if (grepl("^C:/",getwd())) {
  userDir <- "C:/Users/Julian/GDrive" #PC
} else {
  userDir <- "/Users/julian/GDrive" #Mac
}

setwd(paste0(userDir,"/1 Twitter Project/Julian/MEC"))
source("MC_funcs.R")

fsAll <- NULL
setwd(paste0(userDir,"/1 Twitter Project/pythonScripts/GunControl/Guns/split/"))
fs1 <- list.files(getwd(),".csv")
removeWord(fs1,"amendment")
setwd(paste0(userDir,"/1 Twitter Project/pythonScripts/GunControl/Guns/split/filt/"))
fs1 <- list.files(getwd(),".csv",full.names = T) 
convertRetweet(fs1,w=T,lastTime = T) #uncomment to write .csv
# dG <- convertRetweet(fs1) 

setwd(paste0(userDir,"/1 Twitter Project/pythonScripts/GayMarriage/marriage/split/"))
fs2 <- list.files(getwd(),".csv",full.names = T) 
convertRetweet(fs2,w=T,lastTime = T) #uncomment to write .csv
# dM <- convertRetweet(fs2) 

setwd(paste0(userDir,"/1 Twitter Project/pythonScripts/ClimateChange/Combined/split/"))
fs3 <- list.files(getwd(),".csv",full.names = T) 
removeWord(fs3,"storm") #remove storm
setwd(paste0(userDir,"/1 Twitter Project/pythonScripts/ClimateChange/Combined/split/filt/"))
fs3 <- list.files(getwd(),".csv",full.names = T) 
climateAppend = c("global warming","climate change", 
                  "climate science","climate policy") #words to add to moral climate filter
appendMoral(fs3,climateAppend) #swap tweets with these words from NM to M
setwd(paste0(userDir,"/1 Twitter Project/pythonScripts/ClimateChange/Combined/split/filt/append/"))
fs3 <- list.files(getwd(),".csv",full.names = T) 
convertRetweet(fs3,w=T,lastTime=T) #uncomment to write .csv
removeDupe(fs3)
# dC <- convertRetweet(fs3)

# examine top tweets
# dC %>% group_by(cond) %>% top_n(2,wt=count) 

# write megatweet 
fsAll <- c(fs1,fs2,fs3)
writeSummaryTweets(fsAll,twFile=T,sumFile=T) #takes about 1:15  to run

# moral (gay marriage)
setwd(paste0(userDir,"/1 Twitter Project/pythonScripts/GayMarriage/gayMarriageMoral/split/"))
fs2 <- list.files(getwd(),".csv",full.names = T) 
# convertRetweet(fs2,w=T,lastTime = T) #uncomment to write .csv
writeSummaryTweets(fs2,twFile=F,sumFile=F,dir=T) #uncomment to write .csv


# moral (gun control)
setwd(paste0(userDir,"/1 Twitter Project/pythonScripts/GunControl/GC2013/filt/"))
fs2 <- list.files(getwd(),".csv",full.names = T) 
# convertRetweet(fs2,w=T,lastTime = T) #uncomment to write .csv
writeSummaryTweets(fs2,twFile=F,sumFile=F,dir=T) #uncomment to write .csv


# tweets/day write csv ----------------------------------------------------

setwd(paste0(userDir,"/1 Twitter Project/pythonScripts/allTweets/"))
dAll <- tbl_df(read.csv("corpRTs.csv",header=T,sep=",")) %>% 
  mutate(day=yday(timestamp),dayDate = as.Date(day, origin = "2015-01-01"))
plotDf <- count(dAll  %>% filter(orig==1,topic=="C"), cond, topic, day=day) %>% 
  mutate(day=as.Date(day-1, origin = "2015-01-01"))
plotDf2 <- count(dAll  %>% filter(orig2==1,topic=="C"), cond, topic, day=day) %>% 
  mutate(day=as.Date(day-1, origin = "2015-01-01"))
write.csv(plotDf,file.path(paste0("climate/climateOrigTweets.csv")),row.names = F)
write.csv(plotDf2,file.path(paste0("climate/climateRetweets.csv")),row.names = F)

# count of retweets, grouped by retweet and day
setwd(paste0(userDir,"/1 Twitter Project/pythonScripts/allTweets/"))
dAll <- tbl_df(read.csv("corpRTs.csv",header=T,sep=",")) %>% 
  mutate(day=yday(timestamp),dayDate = as.Date(day, origin = "2015-01-01")) %>% 
  filter(!is.na(retweeted_status.id_str))
plotDf <- count(dAll  %>% filter(topic=="C"), cond, topic, rtid=retweeted_status.id_str,day=day) %>% 
  mutate(day=as.Date(day-1, origin = "2015-01-01"))
write.csv(plotDf,file.path(paste0("climate/climateRetweetsByID.csv")),row.names = F)


# combine old datasets ----------------------------------------------------

# combine climate and climate change tweets into one dataset
setwd(paste0(userDir,"/1 Twitter Project/pythonScripts/ClimateChange/Combined/cc_split/"))
dCC1 <- tbl_df(read.csv("CC_M_E.csv",header=T,sep=",")) 
dCC2 <- tbl_df(read.csv("CC_M_NE.csv",header=T,sep=",")) 
dCC3 <- tbl_df(read.csv("CC_NM_E.csv",header=T,sep=",")) 
dCC4 <- tbl_df(read.csv("CC_NM_NE.csv",header=T,sep=",")) 
setwd(paste0(userDir,"/1 Twitter Project/pythonScripts/ClimateChange/Climate/split/"))
dC1 <- tbl_df(read.csv("C_M_E.csv",header=T,sep=",")) 
dC2 <- tbl_df(read.csv("C_M_NE.csv",header=T,sep=",")) 
dC3 <- tbl_df(read.csv("C_NM_E.csv",header=T,sep=",")) 
dC4 <- tbl_df(read.csv("C_NM_NE.csv",header=T,sep=",")) 
dC.All <- rbind(dCC1,dCC2,dCC3,dCC4,dC1,dC2,dC3,dC4)
setwd(paste0(userDir,"/1 Twitter Project/pythonScripts/ClimateChange/Combined/"))
write.csv(dC.All,"C_combined.csv",row.names = F)


# Saving rdata ------------------------------------------------------------
setwd("C:/Users/Julian/GDrive/1 Twitter Project/pythonScripts/allTweets/climate/")
save(dCAll,file="climateAll.RData")
save(dCRT,file="climateRT.RData")
save(dCjoin,file="climate_join.RData")
save(dCRT.count,file="climateRT_cnt.RData")
save(dCRTsum,file="climateRT_dvs.RData")
save(dCjoin2,file="climate_joinCntIdeo.RData")
