# Twitter project, data cleaning
# Oct 28, 2015
# Julian Wills 
# Purpose: includes functions to remove words (e.g. "storm") 
# and append words to moral dictionary (e.g. "gun control")


# Helper functions --------------------------------------------------------
require(magrittr) || {install.packages("magrittr"); require(magrittr)}
require(dplyr) || {install.packages("dplyr"); require(dplyr)}
require(readr) || {install.packages("readr"); require(readr)}
require(tictoc) || {install.packages("tictoc"); require(tictoc)}

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

# f=fs3[4] fs=fs3 s="fossil"
# dAll %>% filter(M=="NM")  %>% select(M) 
# dAll2 %>% filter(M=="NM")  %>% select(M) 

require(tm) || {install.packages("tm"); require(tm)}



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
  # s
  dAll %<>% mutate(M=ifelse(grepl(paste(s,collapse="|"),
                                  removePunctuation(tolower(text)))|M=="M","M","NM"),cond=paste0(M,E))
  print(paste("swapped words:",s),sep=" | ")
  dir.create(file.path(getwd(), paste0("append")),showWarnings = F)
  for (c in   dAll %>%  distinct(cond) %$% cond) {
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

# function for creating megafiles
writeSummaryTweets<- function(fs,twFile=T,rtFile=T,joinFile=T,sumFile=T) {
  tic()
  d.allTweets <- NULL; d.corpRT <- NULL; d.join <- NULL; d.retweetSum <- NULL
  for (f in fs) {
    df <- tbl_df(read_csv(f)) %>% distinct(id_str) #removes duplicate tweets
    s = strsplit(f, ".csv") %$% strsplit(.[[1]],"/") %$% strsplit(.[[1]] %>% last(),"_") 
    topic = s[[1]][1] #parse topic 
    cond = paste(s[[1]][2],s[[1]][3],sep="") #parse condition
    tmpTxt <- filter(df,id_str %in% retweeted_status.id_str)$text #stores texts of original tweets
    tmpDate <- filter(df,id_str %in% retweeted_status.id_str) %>% arrange(id_str) %$% timestamp #stores timestamps 
    tmpDate2 <- rep(NA, length(tmpDate))
    df %<>% mutate(topic=topic,cond=cond)
    d.allTweets <- rbind(d.allTweets,df)
    if (rtFile==T) {
      dfCorT <- df %>% mutate(orig = ifelse(id_str %in% df$retweeted_status.id_str, 1, 0)) %>% 
        mutate(orig2 = ifelse(retweeted_status.id_str %in% df$id_str, 1, 0)) %>% 
        filter(orig==1|orig2==1)
      d.corpRT <- rbind(d.corpRT,dfCorT) #should rename to corpus retweets
      print(paste0("finished filtering to corpus RTs on ",f))
      if (joinFile==T) { # create merged file with ideology/userID for both author and retweeter
        dJoin <- left_join(dfCorT %>% filter(orig==1), dfCorT %>% filter(orig2==1), 
                           by = c("id_str" = "retweeted_status.id_str")) %>% 
          select(contains("id_str"),contains("ideology"),text.x,-contains("ret")) %>% 
          transmute(tID = ideology_estimate.x, rtID = ideology_estimate.y,
                    author = user.id_str.x, retweeter = user.id_str.y, text = text.x) %>% 
          mutate(topic=topic,cond=cond,M=ifelse(cond=="ME"|cond=="MNE",1,-1),
                 E=ifelse(cond=="ME"|cond=="NME",1,-1)) 
        d.join <- rbind(d.join,dJoin) 
        print(paste0("finished joining ",f))
      }
      if (sumFile==T) {
        tmpDate2 <- filter(dfCorT,retweeted_status.id_str %in% id_str)  %>% 
          # mutate(authorID = tmpAuth) %>% 
          group_by(retweeted_status.id_str) %>% arrange(retweeted_status.id_str) %>% 
          do(tail(., n=1)) %$% timestamp
        dfRTSum <- dfCorT %>% group_by(retweeted_status.id_str) %>% 
          summarise(count = n(),rtid = mean(ideology_estimate), targetIDsd=sd(ideology_estimate)) %>% 
          arrange(desc(count))  %>% filter(!is.na(retweeted_status.id_str)) %>% 
          mutate(tid = filter(dfCorT,id_str %in% retweeted_status.id_str)$ideology_estimate,
                 text = tmpTxt, #returns text of original tweet
                 # authID = tmpAuth, #user ID of original tweeter
                 origTime = tmpDate,
                 lastTime = tmpDate2,
                 cond=cond,topic=topic,
                 M=ifelse(cond=="ME"|cond=="MNE",1,-1),
                 E=ifelse(cond=="ME"|cond=="NME",1,-1)) 
        d.retweetSum <- rbind(d.retweetSum,dfRTSum)
        print(paste("finished processing file",f))
      }
    }
  }
  if (grepl("^C:/",getwd())) {
    userDir <- "C:/Users/Julian/GDrive" #PC
  } else {
    userDir <- "/Users/julian/GDrive" #Mac
  }
  setwd(paste0(userDir,"/1 Twitter Project/pythonScripts/allTweets/"))
  if (twFile==T) {
    print(paste("writing allTweets.csv..."))
    write.csv(d.allTweets,"allTweets.csv")
  }
  if (rtFile==T) {
    print(paste("writing corpRTs.csv...")); 
    write.csv(d.corpRT,"corpRTs.csv",row.names=F); 
  }
  if (joinFile==T) {
    print(paste("writing joinedRTs.csv...")); 
    write.csv(d.join,"joinedRTs.csv",row.names=F); 
  }
  if (sumFile==T) {
    print(paste("writing allRTSummary.csv..."))
    write.csv(d.retweetSum,"allRTSummary2.csv",row.names=F); 
  }
  toc()
}