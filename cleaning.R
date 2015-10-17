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


appendMoral<- function(fs,s) {
  dAll <- NULL
  for (f in fs) {
    df <- tbl_df(read.csv(f,header=T,sep=",")) 
    s2 = strsplit(f, ".csv") %$% strsplit(.[[1]],"/") %$% strsplit(.[[1]] %>% last(),"_") 
    topic = s2[[1]][1] #parse topic 
    cond = paste(s2[[1]][2],s2[[1]][3],sep="") #parse condition
    dAll <- rbind(dAll,df %>% mutate(M=ifelse(cond=="ME"|cond=="MNE","M","NM"),
                                     E=ifelse(cond=="ME"|cond=="NME","E","NE"),
                                     cond=cond)) #add subset and label
    print(paste("loaded",f))
  }
  dAll <- dAll %>% mutate(M=ifelse(grepl(paste(s,collapse="|"),tolower(text))|M=="M","M","NM"))
  print("swapped words")
  dir.create(file.path(getwd(), paste0("append")),showWarnings = F)
  for (c in   dAll %>%  distinct(cond) %>% select(cond) %>% unlist()) {
    M = dAll %>% filter(cond==c) %>% distinct(M) %>% select(M) %>% unlist()
    E = dAll %>% filter(cond==c) %>% distinct(E) %>% select(E) %>% unlist()
    fname=paste0(paste(c(topic,M,E),collapse="_"),".csv")
    dAll %>% filter(cond==c) %>% select(-M,-E,-cond) %>% 
      write.csv(file.path("append",paste0(paste(fname,collapse="_"),".csv")),row.names = F)
    print(paste("saved updated",fname))
  }
  write(s, file.path("append","appendedWords.txt"), sep = "\n")
}

convertRetweet <- function(fs,w=F) { #convert df to retweet counts/summaries
  # optional: writes .csv when w==T. Otherwise returns dataframe
  dAll <- NULL
  for (f in fs) {
    df <- tbl_df(read.csv(f,header=T,sep=",")) 
    probTweets <- df %>% group_by(id_str) %>% summarise(count=n()) %>% filter(count>1) %>% select(1) %>% unlist
    if (length(probTweets)>0){
      df <- df %>% filter(!id_str %in% probTweets)
    }
    s = strsplit(f, ".csv") %$% strsplit(.[[1]],"/") %$% strsplit(.[[1]] %>% last(),"_") 
    topic = s[[1]][1] #parse topic 
    cond = paste(s[[1]][2],s[[1]][3],sep="") #parse condition
    dfs <- df %>% mutate(orig = ifelse(id_str %in% df$retweeted_status.id_str, 1, 0)) %>% 
      mutate(orig2 = ifelse(retweeted_status.id_str %in% df$id_str, 1, 0)) %>% 
      filter(orig==1|orig2==1)  %>% group_by(retweeted_status.id_str) %>% 
      summarise(count = n(),rtid = mean(ideology_estimate), targetIDsd=sd(ideology_estimate)) %>% 
      arrange(desc(count))  %>% filter(!is.na(retweeted_status.id_str)) %>% 
      mutate(tid = filter(df,id_str %in% retweeted_status.id_str)$ideology_estimate,
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

# Preprocessing -----------------------------------------------------------

setwd("C:/Users/Julian/GDrive/1 Twitter Project/pythonScripts/GunControl/Guns/split/")
fs1 <- list.files(getwd(),".csv")
removeWord(fs1,"amendment")
setwd("C:/Users/Julian/GDrive/1 Twitter Project/pythonScripts/GunControl/Guns/split/filt/")
# convertRetweet(fs1,w=T) #uncomment to write .csv
fs1 <- list.files(getwd(),".csv")
dG <- convertRetweet(fs1) 

setwd("C:/Users/Julian/GDrive/1 Twitter Project/pythonScripts/GayMarriage/marriage/split/")
fs2 <- list.files(getwd(),".csv")
# convertRetweet(fs2,w=T) #uncomment to write .csv
dM <- convertRetweet(fs2) 

setwd("C:/Users/Julian/GDrive/1 Twitter Project/pythonScripts/ClimateChange/Combined/split/")
fs3 <- list.files(getwd(),".csv") 
removeWord(fs3,"storm")
setwd("C:/Users/Julian/GDrive/1 Twitter Project/pythonScripts/ClimateChange/Combined/split/filt/")
fs3 <- list.files(getwd(),".csv") 
# convertRetweet(fs3,w=T) #uncomment to write .csv
dC <- convertRetweet(fs3)
