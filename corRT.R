# Twitter project, exploring data
# Oct 15, 2015
# Julian Wills 


# Helper Functions --------------------------------------------------------
require(magrittr) || {install.packages("magrittr"); require(magrittr)}
require(dplyr) || {install.packages("dplyr"); require(dplyr)}

writeRetweet <- function(fs) { #compute retweet counts/summaries and save to .csv
  dAll <- NULL
  for (f in fs) {
    df <- tbl_df(read.csv(f,header=T,sep=",")) 
    probTweets <- df %>% group_by(id_str) %>% summarise(count=n()) %>% filter(count>1) %>% select(1) %>% unlist
    if (length(probTweets)>0){
      df <- df %>% filter(!id_str %in% probTweets)
      print(length(probTweets))
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
  dir.create(file.path(getwd(), "RT"),showWarnings = F)
  write.csv(dAll,file.path(paste0("RT"),paste0(topic,"_RT.csv")),row.names = F)
}

removeWord <- function(fs,s) { #removes list of words from all files
  for (f in fs) {
    df <- tbl_df(read.csv(f,header=T,sep=",")) 
    df <- df %>% filter(!grepl(paste(s,collapse="|"),tolower(text)))
    dir.create(file.path(getwd(), paste0(s,"filt")),showWarnings = F)
    write.csv(df,file.path(paste0(s,"filt"),paste0(f)),row.names = F)
  }
  write(s, file.path("filt","removedWords.txt"), sep = "\n")
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

# create function that moves tweets from dataset to another (e.g. nonmoral to moral)
# make sure your working directory contains 4 .csv files
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


# Heat maps ---------------------------------------------------------------
setwd("C:/Users/Julian/GDrive/1 Twitter Project/pythonScripts/GunControl/Guns/split/")
fs1 <- list.files("C:/Users/Julian/GDrive/1 Twitter Project/pythonScripts/GunControl/Guns/split/",".csv")
dG <- convertRetweet(fs1)

setwd("C:/Users/Julian/GDrive/1 Twitter Project/pythonScripts/GayMarriage/marriage/split/")
fs2 <- list.files("C:/Users/Julian/GDrive/1 Twitter Project/pythonScripts/GayMarriage/marriage/split/",".csv")
dM <- convertRetweet(fs2)

dAll <- rbind(dG,dM)

## functions to construct heatmaps
min <- -3.5
max <- 3.5
breaks <- 0.25

expand_data <- function(df, breaks=0.10, min=-4, max=4){
  x <- df$rtid %>% as.numeric()
  y <- df$tid %>% as.numeric()
  x <- (round((x - min) / breaks, 0) * breaks) + min
  y <- (round((y - min) / breaks, 0) * breaks) + min
  tab <- table(x, y)
  tab <- melt(tab)
  tab$prop <- tab$value/sum(tab$value)
  return(tab)
}

require(reshape2)
# perform on each subset and then combine (ugly code... but in a hurry)
new.xy.me <- expand_data(dAll %>% filter(cond=="ME",topic=="G"),breaks=0.25) %>%  mutate(cond="ME")
new.xy.nme <- expand_data(dAll %>% filter(cond=="NME",topic=="G"),breaks=0.25) %>% mutate(cond="NME")
new.xy.mne <- expand_data(dAll %>% filter(cond=="MNE",topic=="G"),breaks=0.25) %>% mutate(cond="MNE")
new.xy.nmne <- expand_data(dAll %>% filter(cond=="NMNE",topic=="G"),breaks=0.25) %>%  mutate(cond="NMNE")
new.xy <- rbind(new.xy.me,new.xy.nme,new.xy.mne,new.xy.nmne)

ggplot(new.xy, aes(x=y, y=x)) +
  geom_tile(aes(fill=prop), colour="white") +
  scale_fill_gradient(name="% of\ntweets", 
                      low = "white", high = "black", 
                      breaks=c(0, .0050, 0.010, 0.015, 0.02), limits=c(0, .021),
                      labels=c("0.0%", "0.5%", "1.0%", "1.5%", ">2%")) +
  labs(y="Estimated Ideology of Retweeter", x="Estimated Ideology of Author",
       title="Ideological Correlations in Gun Tweets") + 
  scale_y_continuous(expand=c(0,0), breaks=(-2:2), limits=c(-3, 3)) +
  scale_x_continuous(expand=c(0,0), breaks=(-2:2), limits=c(-3, 3)) +
  theme(panel.border=element_rect(fill=NA), panel.background = element_blank()) +
  coord_equal()  +
  facet_wrap(~ cond)


# Overlay distributions ---------------------------------------------------

ggplot(dAll, aes(count, colour = cond)) + 
  stat_ecdf(lwd = .3) + xlab("Retweet Frequency") + ylab("Cumulative Density") + facet_grid(. ~ topic)
ggplot(dAll,aes(x=tid,y=..density..,fill=cond)) + geom_density(alpha=.3) + facet_grid(. ~ topic)
ggplot(dAll,aes(x=rtid,y=..density..,fill=cond)) + geom_density(alpha=.3) + facet_grid(. ~ topic)
ggplot(dAll,aes(x=targetSDID,y=..density..,fill=cond)) + geom_density(alpha=.3) + facet_grid(. ~ topic)
ggplot(dAll,aes(x=tid,y=targetSDID,color=cond)) + 
  geom_point(shape=1,aes(size=count,color=cond)) + 
  geom_smooth() +
  facet_grid(. ~ topic)


# Scratchpad --------------------------------------------------------------
options(dplyr.print_min = 15)
dAll %>% filter(topic=="G") %>% arrange(desc(count))
dAll %>% group_by(topic,cond) %>% summarise(meanRT=mean(count),
                                            totalRT=sum(count),
                                            totalUniqRT=n(),
                                            propUniqRT=totalUniqRT/totalRT*100,
                                            viral=100-propUniqRT,
                                            corRTID=cor(tid,rtid),
                                            vartID=var(tid),
                                            varrtID=var(rtid)) %>% 
  filter(cond=="C")

dC %>% group_by(topic,cond) %>% summarise(meanRT=mean(count),
                                    totalRT=sum(count),
                                    totalUniqRT=n(),
                                    propUniqRT=totalUniqRT/totalRT*100,
                                    viral=100-propUniqRT,
                                    corRTID=cor(tid,rtid),
                                    vartID=var(tid),
                                    varrtID=var(rtid))

dAll %>% group_by(cond) %>% summarise(meanRT=mean(count),
                                      totalRT=sum(count),
                                      totalUniqRT=n(),
                                      propUniqRT=totalUniqRT/totalRT*100,
                                      viral=100-propUniqRT,
                                      corRTID=cor(tid,rtid),
                                      vartID=var(tid),
                                      varrtID=var(rtid))
