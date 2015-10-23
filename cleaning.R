# Twitter project, data cleaning
# Oct 16, 2015
# Julian Wills 
# Purpose: includes functions to remove words (e.g. "storm") 
# and append words to moral dictionary (e.g. "gun control")


# Helper functions --------------------------------------------------------
require(magrittr) || {install.packages("magrittr"); require(magrittr)}
require(dplyr) || {install.packages("dplyr"); require(dplyr)}

# create function that moves tweets from dataset to another (e.g. nonmoral to moral)
# make sure your working directory contains 4 .csv files
removeWord <- function(fs,s) { #removes list of words from all files
  for (f in fs) {
    df <- tbl_df(read.csv(f,header=T,sep=",")) 
    df <- df %>% filter(!grepl(paste(s,collapse="|"),tolower(text)))
    dir.create(file.path(getwd(),"filt"),showWarnings = F)
    write.csv(df,file.path("filt",f),row.names = F)
    print("saved filtered .csv")
  }
  write(s, file.path("filt","removedWords.txt"), sep = "\n")
}

# f=fs3[4] fs=fs3 s=climateAppend
dAll %>% filter(M=="NM")  %>% select(M) 
dAll2 %>% filter(M=="NM")  %>% select(M) 

appendMoral<- function(fs,s) {
  dAll <- NULL
  for (f in fs) {
    df <- tbl_df(read.csv(f,header=T,sep=",")) 
    s2 = strsplit(f, ".csv") %$% strsplit(.[[1]],"/") %$% strsplit(.[[1]] %>% last(),"_") 
    topic = s2[[1]][1] #parse topic 
    cond = paste(s2[[1]][2],s2[[1]][3],sep="") #parse condition
    dAll <- rbind(dAll,df %>% mutate(M=ifelse(cond=="ME"|cond=="MNE","M","NM"),
                                     E=ifelse(cond=="ME"|cond=="NME","E","NE"))) #add subset and label
    print(paste("loaded",f))
  }
  dAll <- dAll %>% mutate(M=ifelse(grepl(paste(s,collapse="|"),tolower(text))|M=="M","M","NM"),
                          cond=paste0(M,E))
  print("swapped words")
  dir.create(file.path(getwd(), paste0("append")),showWarnings = F)
  for (c in   dAll %>%  distinct(cond) %>% select(cond) %>% unlist()) {
    M = dAll %>% filter(cond==c) %>% distinct(M) %>% select(M) %>% unlist()
    E = dAll %>% filter(cond==c) %>% distinct(E) %>% select(E) %>% unlist()
    fname=paste0(paste(c(topic,M,E),collapse="_"),".csv")
    dAll %>% filter(cond==c) %>% select(-M,-E,-cond) %>% 
      write.csv(file.path("append",paste0(paste(fname,collapse="_"))),row.names = F)
    print(paste("saved updated",fname))
  }
  write(s, file.path("append","appendedWords.txt"), sep = "\n")
}

# f = fs3[1]
convertRetweet <- function(fs,w=F,lastTime=F) { #convert df to retweet counts/summaries
  # optional: writes .csv when w==T. Otherwise returns dataframe
  dAll <- NULL
  for (f in fs) {
    df <- tbl_df(read.csv(f,header=T,sep=",")) 
    probTweets <- df %>% group_by(id_str) %>% summarise(count=n()) %>% filter(count>1) %>% select(1) %>% unlist
    if (length(probTweets)>0){
      # df <- df %>% filter(!id_str %in% probTweets)
      df <- df %>% distinct(id_str)
    }
    s = strsplit(f, ".csv") %$% strsplit(.[[1]],"/") %$% strsplit(.[[1]] %>% last(),"_") 
    topic = s[[1]][1] #parse topic 
    cond = paste(s[[1]][2],s[[1]][3],sep="") #parse condition
    tmpTxt <- filter(df,id_str %in% retweeted_status.id_str)$text #stores texts of original tweets
    tmpDate <- filter(df,id_str %in% retweeted_status.id_str) %>% arrange(id_str) %$% timestamp #stores timestamps 
    tmpDate2 <- rep(NA, length(tmpDate)) 
    if (lastTime==T){
      # if interested, filter to retweets based on original tweets,
      # remove duplicates, select last retweet and grab timestamp
      tmpDate2 <- filter(df,retweeted_status.id_str %in% id_str)  %>% 
        group_by(retweeted_status.id_str) %>% arrange(retweeted_status.id_str) %>% 
        do(tail(., n=1)) %$% timestamp
    }
    dfs <- df %>% mutate(origT = ifelse(id_str %in% df$retweeted_status.id_str, 1, 0)) %>% 
      mutate(origRT = ifelse(retweeted_status.id_str %in% df$id_str, 1, 0)) %>% 
      filter(origT==1|origRT==1)  %>% group_by(retweeted_status.id_str) %>% 
      summarise(count = n(),rtid = mean(ideology_estimate), targetIDsd=sd(ideology_estimate)) %>% 
      arrange(desc(count))  %>% filter(!is.na(retweeted_status.id_str)) %>% 
      mutate(tid = filter(df,id_str %in% retweeted_status.id_str)$ideology_estimate,
             text = tmpTxt, #returns text of original tweet
             origTime = tmpDate,
             lastTime = tmpDate2,
             persist=tmpDate2 %>% yday - tmpDate %>% yday,
             cond=cond,topic=topic,
             M=ifelse(cond=="ME"|cond=="MNE",1,-1),
             E=ifelse(cond=="ME"|cond=="NME",1,-1)) 
    dAll <- rbind(dAll,dfs)
    print(paste("finished processing file",f))
  }
  if (w==T) {
    dir.create(file.path(getwd(), "RT"),showWarnings = F)
    print(paste("writing .csv..."))
    write.csv(dAll,file.path(paste0("RT"),paste0(topic,"_RT.csv")),row.names = F)
  } else { 
    return(dAll)
  }
}

tweetsPerDay<- function(fs) {
  require(lubridate)
  require(scales)
  dAll <- NULL
  for (f in fs) {
    df <- tbl_df(read.csv(f,header=T,sep=",")) 
    s2 = strsplit(f, ".csv") %$% strsplit(.[[1]],"/") %$% strsplit(.[[1]] %>% last(),"_") 
    topic = s2[[1]][1] #parse topic 
    cond = paste(s2[[1]][2],s2[[1]][3],sep="") #parse condition
    dAll <- rbind(dAll,df %>% mutate(M=ifelse(cond=="ME"|cond=="MNE","M","NM"),
                                     E=ifelse(cond=="ME"|cond=="NME","E","NE"),
                                     cond=cond)) #add subset and label
  }
  dAll <- dAll %>% mutate(day=yday(timestamp),dayDate = as.Date(day, origin = "2015-01-01")) %>% 
    distinct(id_str)
  plotDf <- count(dAll, cond, day=day) %>% mutate(day=as.Date(day-1, origin = "2015-01-01"))
  p <-  ggplot(data=plotDf, aes(x=day, y=n, colour=cond)) + 
    geom_line(size=1) +
    scale_y_continuous(labels = comma) + 
    labs(y="Number of Tweets", x="Day",title=paste("Tweets/Day related to",topic)) + 
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14,face="bold"))
  print(p)
  return(plotDf)
}

# function for creating megafile
writeSummaryTweets<- function(fs,twFile=T,rtFile=T,sumFile=T) {
  d.allTweets <- NULL
  d.retweets <- NULL
  d.retweetSum <- NULL
  for (f in fs) {
    df <- tbl_df(read.csv(f,header=T,sep=",")) %>% distinct(id_str)
#     probTweets <- df %>% group_by(id_str) %>% summarise(count=n()) %>% filter(count>1) %>% select(1) %>% unlist
#     if (length(probTweets)>0){
#       df <- df %>% filter(!id_str %in% probTweets)
#       df <- df %>% distinct(id_str)
#     }
    s = strsplit(f, ".csv") %$% strsplit(.[[1]],"/") %$% strsplit(.[[1]] %>% last(),"_") 
    topic = s[[1]][1] #parse topic 
    cond = paste(s[[1]][2],s[[1]][3],sep="") #parse condition
    tmpTxt <- filter(df,id_str %in% retweeted_status.id_str)$text #stores texts of original tweets
    tmpDate <- filter(df,id_str %in% retweeted_status.id_str) %>% arrange(id_str) %$% timestamp #stores timestamps 
    tmpDate2 <- rep(NA, length(tmpDate))
    df <- df %>% mutate(topic=topic,cond=cond)
    d.allTweets <- rbind(d.allTweets,df)
    if (rtFile==T) {
      df <- df %>% mutate(orig = ifelse(id_str %in% df$retweeted_status.id_str, 1, 0)) %>% 
        mutate(orig2 = ifelse(retweeted_status.id_str %in% df$id_str, 1, 0)) %>% 
        filter(orig==1|orig2==1)
      d.retweets <- rbind(d.retweets,df)
      if (sumFile==T) {
        tmpDate2 <- filter(df,retweeted_status.id_str %in% id_str)  %>% 
          group_by(retweeted_status.id_str) %>% arrange(retweeted_status.id_str) %>% 
          do(tail(., n=1)) %$% timestamp
        df2 <- df %>% group_by(retweeted_status.id_str) %>% 
          summarise(count = n(),rtid = mean(ideology_estimate), targetIDsd=sd(ideology_estimate)) %>% 
          arrange(desc(count))  %>% filter(!is.na(retweeted_status.id_str)) %>% 
          mutate(tid = filter(df,id_str %in% retweeted_status.id_str)$ideology_estimate,
                 text = tmpTxt, #returns text of original tweet
                 origTime = tmpDate,
                 lastTime = tmpDate2,
                 cond=cond,topic=topic,
                 M=ifelse(cond=="ME"|cond=="MNE",1,-1),
                 E=ifelse(cond=="ME"|cond=="NME",1,-1)) 
        d.retweetSum <- rbind(d.retweetSum,df2)
        print(paste("finished processing file",f))
      }
    }
  }
  setwd("C:/Users/Julian/GDrive/1 Twitter Project/pythonScripts/allTweets/")
  if (twFile==T) {
    print(paste("writing allTweets.csv..."))
    write.csv(d.allTweets,file.path(paste0("allTweets.csv")),row.names = F)
  }
  if (rtFile==T) {
    print(paste("writing allRetweets.csv..."))
    write.csv(d.retweets,file.path(paste0("allRetweets.csv")),row.names = F)
  }
  if (sumFile==T) {
    print(paste("writing allRTSummary.csv..."))
    write.csv(d.retweetSum,file.path(paste0("allRTSummary.csv")),row.names = F)
  }
}

f=fs3[4]
# Preprocessing -----------------------------------------------------------
fsAll <- NULL
setwd("C:/Users/Julian/GDrive/1 Twitter Project/pythonScripts/GunControl/Guns/split/")
fs1 <- list.files(getwd(),".csv")
removeWord(fs1,"amendment")
setwd("C:/Users/Julian/GDrive/1 Twitter Project/pythonScripts/GunControl/Guns/split/filt/")
fs1 <- list.files(getwd(),".csv",full.names = T) 
convertRetweet(fs1,w=T,lastTime = T) #uncomment to write .csv
# fs3 <- list.files(getwd(),".csv")
fs1 <- list.files(getwd(),".csv",full.names = T) 
fsAll<-c(fsAll,normalizePath(fs1))
# dG <- convertRetweet(fs1) 

setwd("C:/Users/Julian/GDrive/1 Twitter Project/pythonScripts/GayMarriage/marriage/split/")
# fs2 <- list.files(getwd(),".csv")
fs2 <- list.files(getwd(),".csv",full.names = T) 
fsAll<-c(fsAll,normalizePath(fs2))
convertRetweet(fs2,w=T,lastTime = T) #uncomment to write .csv
# dM <- convertRetweet(fs2) 

setwd("C:/Users/Julian/GDrive/1 Twitter Project/pythonScripts/ClimateChange/Combined/split/")
fs3 <- list.files(getwd(),".csv",full.names = T) 
removeWord(fs3,"storm") #remove storm
setwd("C:/Users/Julian/GDrive/1 Twitter Project/pythonScripts/ClimateChange/Combined/split/filt/")
fs3 <- list.files(getwd(),".csv",full.names = T) 
climateAppend = c("fossil fuel","fossil-fuel") #words to add to moral climate filter
appendMoral(fs3,climateAppend) #swap tweets with these words from NM to M
setwd("C:/Users/Julian/GDrive/1 Twitter Project/pythonScripts/ClimateChange/Combined/split/filt/append/")
fs3 <- list.files(getwd(),".csv",full.names = T) 
fsAll<-c(fsAll,normalizePath(fs3))
convertRetweet(fs3,w=T,lastTime = T) #uncomment to write .csv
# dC <- convertRetweet(fs3)

# examine top tweets
# dC %>% group_by(cond) %>% top_n(2,wt=count) 

# write megatweet 
fsAll <- c(fs1,fs2,fs3)
writeSummaryTweets(fsAll,twFile=F) #takes about 45 seconds to run

# moral (gay marriage)
setwd("C:/Users/Julian/GDrive/1 Twitter Project/pythonScripts/GayMarriage/gayMarriageMoral/split/")
fs2 <- list.files(getwd(),".csv",full.names = T) 
convertRetweet(fs2,w=T,lastTime = T) #uncomment to write .csv

# moral (gun control)
setwd("C:/Users/Julian/GDrive/1 Twitter Project/pythonScripts/GunControl/GC2013/filt/")
fs2 <- list.files(getwd(),".csv",full.names = T) 
convertRetweet(fs2,w=T,lastTime = T) #uncomment to write .csv


# tweets/day write csv ----------------------------------------------------

setwd("C:/Users/Julian/GDrive/1 Twitter Project/pythonScripts/allTweets/")
dAll <- tbl_df(read.csv("allRetweets.csv",header=T,sep=",")) %>% 
  mutate(day=yday(timestamp),dayDate = as.Date(day, origin = "2015-01-01"))
plotDf <- count(dAll  %>% filter(orig==1,topic=="C"), cond, topic, day=day) %>% 
  mutate(day=as.Date(day-1, origin = "2015-01-01"))
plotDf2 <- count(dAll  %>% filter(orig2==1,topic=="C"), cond, topic, day=day) %>% 
  mutate(day=as.Date(day-1, origin = "2015-01-01"))
write.csv(plotDf,file.path(paste0("climate/climateOrigTweets.csv")),row.names = F)
write.csv(plotDf2,file.path(paste0("climate/climateRetweets.csv")),row.names = F)

# count of retweets, grouped by retweet and day
setwd("C:/Users/Julian/GDrive/1 Twitter Project/pythonScripts/allTweets/")
dAll <- tbl_df(read.csv("allRetweets.csv",header=T,sep=",")) %>% 
  mutate(day=yday(timestamp),dayDate = as.Date(day, origin = "2015-01-01")) %>% 
  filter(!is.na(retweeted_status.id_str))
plotDf <- count(dAll  %>% filter(topic=="C"), cond, topic, rtid=retweeted_status.id_str,day=day) %>% 
  mutate(day=as.Date(day-1, origin = "2015-01-01"))
write.csv(plotDf,file.path(paste0("climate/climateRetweetsByID.csv")),row.names = F)


# combine old datasets ----------------------------------------------------

# combine climate and climate change tweets into one dataset
setwd("C:/Users/Julian/GDrive/1 Twitter Project/pythonScripts/ClimateChange/Combined/cc_split/")
dCC1 <- tbl_df(read.csv("CC_M_E.csv",header=T,sep=",")) 
dCC2 <- tbl_df(read.csv("CC_M_NE.csv",header=T,sep=",")) 
dCC3 <- tbl_df(read.csv("CC_NM_E.csv",header=T,sep=",")) 
dCC4 <- tbl_df(read.csv("CC_NM_NE.csv",header=T,sep=",")) 
setwd("C:/Users/Julian/GDrive/1 Twitter Project/pythonScripts/ClimateChange/Climate/split/")
dC1 <- tbl_df(read.csv("C_M_E.csv",header=T,sep=",")) 
dC2 <- tbl_df(read.csv("C_M_NE.csv",header=T,sep=",")) 
dC3 <- tbl_df(read.csv("C_NM_E.csv",header=T,sep=",")) 
dC4 <- tbl_df(read.csv("C_NM_NE.csv",header=T,sep=",")) 
dC.All <- rbind(dCC1,dCC2,dCC3,dCC4,dC1,dC2,dC3,dC4)
setwd("C:/Users/Julian/GDrive/1 Twitter Project/pythonScripts/ClimateChange/Combined/")
write.csv(dC.All,"C_combined.csv",row.names = F)


