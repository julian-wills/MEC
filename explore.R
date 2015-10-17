# Twitter project, exploring data
# Aug 29, 2015
# Julian Wills 
require(openxlsx) || {install.packages("openxlsx"); require(openxlsx)}
require(ggplot2) || {install.packages("ggplot2"); require(ggplot2)}
require(grid) || {install.packages("grid"); require(grid)}
require(reshape) || {install.packages("reshape"); require(reshape)}
require(igraph) || {install.packages("igraph"); require(igraph)}
require(smappR) || {install.packages("smappR"); require(smappR)}
require(readxl) || {install.packages("readxl"); require(readxl)}
require(tidyr) || {install.packages("tidyr"); require(tidyr)}
require(stringr) || {install.packages("stringr"); require(stringr)}
require(scales) || {install.packages("scales"); require(scales)}


# Ideology distributions for each topic [2x2x3] ---------------------------
setwd("C:/Users/Julian/GDrive/1 Twitter Project/")

# read in gun control data subsets and combine
dGC.NME <- tbl_df(read.xlsx("1 Gun Control Data Set/1 Data Files/GC_NM_E.xlsx",cols=c(1:2,4:5,11))) %>% 
  mutate(cond="NME", moral=0, emo=1)
dGC.NMNE <- tbl_df(read.xlsx("1 Gun Control Data Set/1 Data Files/GC_NM_NE.xlsx",cols=c(1:2,4:5,11))) %>% 
  mutate(cond="NMNE", moral=0, emo=0)
dGC.ME <- tbl_df(read.xlsx("1 Gun Control Data Set/1 Data Files/GC_M_E.xlsx",cols=c(1:2,4:5,11))) %>% 
  mutate(cond="ME", moral=1, emo=1) 
dGC.MNE <- tbl_df(read.xlsx("1 Gun Control Data Set/1 Data Files/GC_M_NE.xlsx",cols=c(1:2,4:5,11))) %>% 
  mutate(cond="MNE", moral=1, emo=0) 

dGC <- rbind(dGC.NME,dGC.NMNE,dGC.ME,dGC.MNE) %>% dplyr::rename(ideo=ideology_estimate) %>% 
  mutate(topic="Guns") %>%  left_join(
    rbind(dGC.NME,dGC.NMNE,dGC.ME,dGC.MNE) %>%  group_by(cond) %>% 
      summarise(meanIdeo=mean(ideology_estimate))) %>% 
  left_join(
    rbind(dGC.NME,dGC.NMNE,dGC.ME,dGC.MNE) %>%  group_by(cond) %>% na.omit() %>% 
      summarise(meanIdeoRT=mean(ideology_estimate)))

# read in climate data subsets and combine
dCC.NME <- tbl_df(read.xlsx("2 Environmental Data Set/1 Data Files/C_NM_E.xlsx",cols=c(1:2,4:5,11))) %>% 
  mutate(cond="NME", moral=0, emo=1)
dCC.NMNE <- tbl_df(read.xlsx("2 Environmental Data Set/1 Data Files/C_NM_NE.xlsx",cols=c(1:2,4:5,11))) %>% 
  mutate(cond="NMNE", moral=0, emo=0)
dCC.ME <- tbl_df(read.xlsx("2 Environmental Data Set/1 Data Files/C_M_E.xlsx",cols=c(1:2,4:5,11))) %>% 
  mutate(cond="ME", moral=1, emo=1) 
dCC.MNE <- tbl_df(read.xlsx("2 Environmental Data Set/1 Data Files/C_M_NE.xlsx",cols=c(1:2,4:5,11))) %>% 
  mutate(cond="MNE", moral=1, emo=0) 

dCC <- rbind(dCC.NME,dCC.NMNE,dCC.ME,dCC.MNE) %>% dplyr::rename(ideo=ideology_estimate) %>% 
  mutate(topic="Climate") %>%  left_join(
    rbind(dCC.NME,dCC.NMNE,dCC.ME,dCC.MNE) %>%  group_by(cond) %>% 
      summarise(meanIdeo=mean(ideology_estimate))) %>% 
  left_join(
    rbind(dCC.NME,dCC.NMNE,dCC.ME,dCC.MNE) %>%  group_by(cond) %>% na.omit() %>% 
      summarise(meanIdeoRT=mean(ideology_estimate)))

# read in marriage data subsets and combine
dGM.NME <- tbl_df(read.xlsx("3 Gay Marriage Data Set/M_NM_E.xlsx",cols=c(1:2,4:5,11))) %>% 
  mutate(cond="NME", moral=0, emo=1)
dGM.NMNE <- tbl_df(read.xlsx("3 Gay Marriage Data Set/M_NM_NE.xlsx",cols=c(1:2,4:5,11))) %>% 
  mutate(cond="NMNE", moral=0, emo=0)
dGM.ME <- tbl_df(read.xlsx("3 Gay Marriage Data Set/M_M_E.xlsx",cols=c(1:2,4:5,11))) %>% 
  mutate(cond="ME", moral=1, emo=1) 
dGM.MNE <- tbl_df(read.xlsx("3 Gay Marriage Data Set/M_M_NE.xlsx",cols=c(1:2,4:5,11))) %>% 
  mutate(cond="MNE", moral=1, emo=0) 

dGM <- rbind(dGM.NME,dGM.NMNE,dGM.ME,dGM.MNE) %>% dplyr::rename(ideo=ideology_estimate) %>% 
  mutate(topic="Marriage") %>%  left_join(
    rbind(dGM.NME,dGM.NMNE,dGM.ME,dGM.MNE) %>%  group_by(cond) %>% 
      summarise(meanIdeo=mean(ideology_estimate))) %>% 
  left_join(
    rbind(dGM.NME,dGM.NMNE,dGM.ME,dGM.MNE) %>%  group_by(cond) %>% na.omit() %>% 
      summarise(meanIdeoRT=mean(ideology_estimate)))

# combine all topics into one dataframe
dAll <- rbind(dCC,dGC,dGM) %>% mutate(cond=as.factor(cond))
levels(dAll$cond) <- c("Moral Emotional", "Moral Unemotional", "Nonmoral Emotional","Nonmoral Unemotional")

# stacked density plots for each condition, paneled by topic
ggplot(dAll,aes(x=ideo,y=..density..,fill=cond)) + 
  geom_density(alpha=.3) +
  facet_wrap( ~ topic,nrow=3) +
  geom_vline(aes(xintercept=meanIdeo,  colour=cond),linetype="dashed", size=1) +
  xlab("Ideology Estimate") + ylab("Density") +
  theme(legend.key.size = unit(2, "cm"),
        legend.text = element_text(size = 15))

# stacked density plots for each topic, paneled by condition
ggplot(dAll,aes(x=ideo,y=..density..,fill=topic)) + 
  geom_density(alpha=.3) +
  facet_wrap( ~ cond,ncol=2) +
  geom_vline(aes(xintercept=meanIdeo,  colour=topic),linetype="dashed", size=1) +
  xlab("Ideology Estimate") + ylab("Density")

# ------------ just looking @ retweets------------

# stacked density plots for each condition, paneled by topic [just retweets]
ggplot(dAll %>% na.omit(),aes(x=ideo,y=..density..,fill=cond)) + 
  geom_density(alpha=.3) +
  facet_wrap( ~ topic,nrow=3) +
  geom_vline(aes(xintercept=meanIdeoRT,  colour=cond),linetype="dashed", size=1) +
  xlab("Ideology Estimate") + ylab("Density") +
  theme(legend.key.size = unit(2, "cm"),
        legend.text = element_text(size = 15))

# stacked density plots for each topic, paneled by condition [just retweets]
ggplot(dAll %>% na.omit(),aes(x=ideo,y=..density..,fill=topic)) + 
  geom_density(alpha=.3) +
  facet_wrap( ~ cond,ncol=2) +
  geom_vline(aes(xintercept=meanIdeoRT,  colour=topic),linetype="dashed", size=1) +
  xlab("Ideology Estimate") + ylab("Density")

# Comparing ideologies between full dataset vs. retweets ------------------

dGC.NME2 <- tbl_df(read.xlsx("1 Gun Control Data Set/1 Data Files/GC_NM_E.xlsx")) %>% 
  select(ideo=ideology_estimate,tid=id_str,rtid=retweeted_status.id_str) %>% 
  mutate(cond="NME", moral=0, emo=1,topic="guns")
dGC.NMNE2 <- tbl_df(read.xlsx("1 Gun Control Data Set/1 Data Files/GC_NM_NE.xlsx")) %>% 
  select(ideo=ideology_estimate,tid=id_str,rtid=retweeted_status.id_str) %>% 
  mutate(cond="NMNE", moral=0, emo=0,topic="guns")

ggplot(dGC.NME2,aes(x=ideo,y=..density..)) + 
  # geom_density(alpha=.3) +
  geom_density(data=filter(dGC.NME2,!is.na(rtid)),alpha=.3,fill="black") +
  geom_density(data=filter(dGC.NME2,is.na(rtid)),alpha=.3,fill="blue") 

ggplot(dGC.NMNE2,aes(x=ideo,y=..density..)) + 
  # geom_density(alpha=.3) +
  geom_density(data=filter(dGC.NMNE2,!is.na(rtid)),alpha=.3,fill="black") +
  geom_density(data=filter(dGC.NMNE2,is.na(rtid)),alpha=.3,fill="blue") 

dGC.NME2 %>% filter(!is.na(rtid))




# Correlation grid of retweeted vs. retweeters ----------------------------
setwd("C:/Users/Julian/GDrive/1 Twitter Project/")
dCorGC.E <- tbl_df(read.xlsx("1 Gun Control Data Set/2 Analyses/Correlation Analysis Setup.xlsx",sheet=3)) %>% 
  select(tid=2,rtid=Average.of.Pid) %>% 
  mutate(cond="E", moral=0, emo=1,topic="guns")


dCor <- read_excel("Julian/Billy_summaryFiles/corSummary.xlsx") %>% 
  gather(topic,tid,seq(2,8,2),na.rm = TRUE) %>% gather(topic2,rtid,-tid,-topic,na.rm = TRUE) %>% 
  na.omit() %>% separate(topic,c("topic","moral","emo","who")) %>% 
  dplyr::select(-topic2,-who)
  
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

# perform on each subset and then combine (ugly code... but in a hurry)
new.xy.me <- expand_data(dCor %>% filter(moral=="m",emo=="e"),breaks=0.25) %>% mutate(cond="Moral Emotional")
new.xy.nme <- expand_data(dCor %>% filter(moral=="nm",emo=="e"),breaks=0.25) %>% mutate(cond="Nonmoral Emotional")
new.xy.mne <- expand_data(dCor %>% filter(moral=="m",emo=="ne"),breaks=0.25) %>% mutate(cond="Moral Unemotional")
new.xy.nmne <- expand_data(dCor %>% filter(moral=="nm",emo=="ne"),breaks=0.25) %>% mutate(cond="Nonmoral Unemotional")
new.xy <- rbind(new.xy.me,new.xy.nme,new.xy.mne,new.xy.nmne)

ggplot(new.xy, aes(x=y, y=x)) +
  geom_tile(aes(fill=prop), colour="white") +
  scale_fill_gradient(name="% of\ntweets", 
                      low = "white", high = "black", 
                      breaks=c(0, .0050, 0.010, 0.015, 0.02), limits=c(0, .021),
                      labels=c("0.0%", "0.5%", "1.0%", "1.5%", ">2%")) +
  labs(y="Estimated Ideology of Retweeter", x="Estimated Ideology of Author",
       title="Ideological Correlations in Gun Control Tweets") + 
  scale_y_continuous(expand=c(0,0), breaks=(-2:2), limits=c(-3, 3)) +
  scale_x_continuous(expand=c(0,0), breaks=(-2:2), limits=c(-3, 3)) +
  theme(panel.border=element_rect(fill=NA), panel.background = element_blank()) +
  coord_equal()  +
  facet_wrap(~ cond)


# Virality analysis -------------------------------------------------------

#read in data and merge into two columns
dT <- read_excel("Julian/Billy_summaryFiles/freqSummary.xlsx") %>% 
  gather(topic,freq,na.rm = TRUE) %>% separate(topic,c("topic","moral","emo")) %>% 
  unite(cond,moral,emo)  %>% group_by(topic,cond)

dTsum <- summarise(dT,
                   totalUniqueTweets = n(),
                   totalTweets = sum(freq)) %>% 
  mutate(RTlik=1-totalUniqueTweets/totalTweets)

#likelihood of retweet for each condition, paneled by topic
ggplot(dTsum, aes(x=factor(cond),y=RTlik,fill=factor(cond))) + 
  geom_bar(stat="identity") +
  scale_fill_manual(values = c("firebrick1","darkslateblue","lightcoral","lightslateblue")) +
  facet_wrap(~ topic) +
  labs(y="Likelihood of Retweet", x="Condition",
       title="Likelihood of Retweet for Each Condition, Paneled by Topic")

# stacked density plots for each topic, paneled by condition
ggplot(dT %>% filter(cume_dist(desc(freq)) < 0.001),aes(x=freq,y=..density..,fill=cond)) + 
  geom_density(alpha=.3) +
  facet_wrap( ~ topic,ncol=3)

# stacked density plots for gun control, split by condition
ggplot(dT %>% filter(cume_dist(desc(freq)) < 0.001,topic=="GC"),
       aes(x=freq,y=..density..,fill=cond)) + 
  geom_density(alpha=.3) +
  labs(y="Density", x="Number of times each tweet gets retweeted",
       title="Distribution of Retweet Frequency for Gun Control")

# stacked density plots for gay marriage, split by condition
ggplot(dT %>% filter(cume_dist(desc(freq)) < 0.01,topic=="GM"),
       aes(x=freq,y=..density..,fill=cond)) + 
  geom_density(alpha=.3) +
  labs(y="Density", x="Number of times each tweet gets retweeted",
       title="Distribution of Retweet Frequency for Gay Marriage")

# stacked density plots for climate, split by condition
ggplot(dT %>% filter(cume_dist(desc(freq)) < 0.001,topic=="C"),
       aes(x=freq,y=..density..,fill=cond)) + 
  geom_density(alpha=.3) +
  labs(y="Density", x="Number of times each tweet gets retweeted",
       title="Distribution of Retweet Frequency for Climate Change")

# # cumulative density plots for each topic, split by condition
# ggplot(dT %>% filter(cume_dist(desc(freq)) < 0.0007), aes(freq, colour = cond)) + 
#   stat_ecdf() +
#   facet_wrap( ~ topic,ncol=3) +
#   labs(y="Cumulative Density", x="Number of times each tweet gets retweeted",
#        title="Distribution of Retweet Frequency for Climate Change")

dT2 <- dT  %>% mutate(freqNormTopicCond = freq/sum(freq) )%>% group_by(topic) %>% 
  mutate(freqNormTopic = freq/sum(freq)) %>% ungroup() %>% mutate(freqNormTotal=freq/sum(freq)) %>% 
  group_by(topic,cond)

# # stacked density plots for each condition, paneled by topic
# ggplot(dT2 %>% filter(cume_dist(desc(freqNormTopic)) < 0.001),
#        aes(x=freqNormTopic,y=..density..,fill=cond)) + 
#   geom_density(alpha=.3) +
#   labs(y="Density", x="RT Frequency (Normed)",
#        title="Distribution of Retweet Frequency") +
#   facet_wrap( ~ topic,ncol=3)

# ggplot(dT2 %>% filter(cume_dist(desc(freqNormTopic)) < 0.01), aes(freqNormTopicCond, colour = cond)) + 
#   stat_ecdf() +
#   facet_wrap( ~ topic,ncol=3) +
#   xlab("Number of times each tweet gets retweeted") + ylab("Cumulative Density")

# sum((dT2)$freqNormTotal) #normalized wrt all retweets
# sum((dT2)$freqNormTopic) #normalized wrt retweets in that topic
# sum((dT2)$freqNormTopicCond) #normalized wrt retweets in that topic and condition

# ggplot(dT2 %>% filter(cume_dist(desc(freqNormTopic)) < 0.1), aes(freqNormTopicCond, colour = cond)) + 
#   stat_ecdf() +
#   facet_wrap( ~ topic,ncol=3) +
#   xlab("Number of times each tweet gets retweeted") + ylab("Cumulative Density")

ggplot(dT2 %>% filter(cume_dist(desc(freq)) < 1), aes(freq, colour = cond, linetype = cond)) + 
  stat_ecdf(lwd = .3) +
  facet_wrap( ~ topic,ncol=3) +
  scale_linetype_manual(values = c("solid","solid","dashed","dashed")) +
  scale_color_manual(values = c("red","blue","red","blue")) +
  ylim(.996,1) + 
  xlab("Retweet Frequency") + ylab("Cumulative Density")


# dT2 %>% filter(cume_dist(desc(freqNormTopic)) < 0.0007)

dT3 <- dT2 %>% summarise(meanFreq=mean(freqNormTotal),se=sd(freqNormTotal)/sqrt(n()))

# bar plots for retweet frequency between condition and topic
ggplot(dT2 %>% summarise(meanFreq=mean(freq),se=sd(freq)/sqrt(n())),
       aes(x=factor(cond),y=meanFreq,fill=factor(cond))) + 
  geom_bar(stat="identity") +
  scale_fill_manual(values = c("firebrick1","darkslateblue","lightcoral","lightslateblue")) +
  facet_wrap(~ topic) +
  geom_errorbar(aes(ymin=meanFreq-se, ymax=meanFreq+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))

# box and whisker for retweet 
ggplot(dT2, aes(x=cond, y = freq, color=cond))  + 
  scale_fill_manual(values = c("firebrick1","darkslateblue","lightcoral","lightslateblue")) +
  geom_boxplot() +
  facet_wrap(~ topic) +
  labs(title="Box Plots for each Condition, Paneled by Topic")


# Network -----------------------------------------------------------------
## not working yet -- need the right data
setwd("C:/Users/Julian/GDrive/1 Twitter Project/")
dNet<- tbl_df(read.csv("pythonScripts/GunControl/GC_M_M_E.csv",T,",")) %>% 
  select(Source=user.id_str,Target=retweeted_status.id_str) %>% na.omit()
  #mutate(cond="E", moral=0, emo=1,topic="guns")

library(igraph)
g <- graph.edgelist(as.matrix(dNet), directed=TRUE) ## network object
set.seed(12345)
l <-  layout.fruchterman.reingold(g, niter=2000, coolexp=20) ## layout
d <- data.frame(l); names(d) <- c("x", "y") ## data frame for plot
d$id <- V(g)$name ## adding names
d$degree <- degree(g)
d <- merge(d, data, all.x=TRUE) ## adding party

?lapply


edgelist <- get.edgelist(g, names=FALSE) ## edgelist
edges <- data.frame(d[edgelist[,1],c("x", "y")], d[edgelist[,2],c("x", "y")])
names(edges) <- c("x1", "y1", "x2", "y2") ## coordinates of each edge
edges$party <- NA

library(ggplot2)
p <- ggplot(d, aes(x=x, y=y, fill=party, size=degree))
pq <- p + geom_segment(
  aes(x=x1, y=y1, xend=x2, yend=y2), 
  data=edges, color="grey20", size=0.25, alpha=1/10) +
  geom_point(aes(size=degree), color="grey20", shape=21) +
  scale_size_continuous(range=c(2,7)) +
  scale_fill_manual(values=c("blue", "green", "red")) + 
  theme(
    panel.background = element_blank(),
    plot.background = element_blank(),
    axis.line = element_blank(), axis.text = element_blank(), 
    axis.ticks = element_blank(), 
    axis.title = element_blank(), panel.border = element_blank(), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  )
pq



# Streaming ----------------------------------------------------------------
library(streamR)
library(wordcloud)
library(smappR)


setwd("C:/Users/Julian/GDrive/1 Twitter Project/")
dGun<- tbl_df(read.csv("pythonScripts/GunControl/GC_M_M_E.csv",T,",")) %>% 
  select(1:2,4:5,text)

# dAll$topic
# wordFreq <- dAll %>% filter(topic=="Gun") %>% select(text) %>% word.frequencies()

wordFreq <- word.frequencies(dGun$text)

wordcloud(words=names(wordFreq), freq=wordFreq, max.words=50, 
          random.order=F, colors="black", scale=c(2.5,.5), rot.per=0)

wordcloud(words=names(wordFreq), freq=wordFreq, scale=c(5,.1), min.freq=5, max.words=50, 
          random.order=F, rot.per=0.5, use.r.layout=T, colors=brewer.pal(8, "Dark2"))

# Scraping ----------------------------------------------------------------

# load("~/../GDrive/1 Twitter Project/Julian/SMaPPtools/my_oauth")
# setwd("~/../GDrive/1 Twitter Project/Julian/SMaPPtools")
# 
# # How can I collect all tweets that mention certain keywords?
# filterStream(file.name="test_tweets.json", track=c("obama", "romney"), oauth=my_oauth,
#              tweets=500)
# 
# # How can I then see the tweets I collected?
# tweets <- parseTweets("obama_tweets.json") #names(tweets)
# 
# # a few quick analyses of the data
# table(tweets$lang) ## distribution by language
# sum(!is.na(tweets$lat)) ## how many are geolocated
# summary(tweets$retweet_count) ## how many RTs they have
# tweets$text[which.max(tweets$retweet_count)] ## most RTed tweet


# Retweet analysis --------------------------------------------------------
require(magrittr)



setwd("C:/Users/Julian/GDrive/1 Twitter Project/pythonScripts/GunControl/Guns/split/amendment_filt/")
fs1 <- list.files(getwd(),".csv")
dG <- convertRetweet(fs1) #writeRetweet(fs1)
# removeWord(fs1,"amendment")


setwd("C:/Users/Julian/GDrive/1 Twitter Project/pythonScripts/GayMarriage/marriage/split/")
fs2 <- list.files("C:/Users/Julian/GDrive/1 Twitter Project/pythonScripts/GayMarriage/marriage/split/",".csv")
dM <- convertRetweet(fs2) #writeRetweet(fs2)

setwd("C:/Users/Julian/GDrive/1 Twitter Project/pythonScripts/ClimateChange/Combined/split/storm_filt/")
fs3 <- list.files(,".csv") #writeRetweet(fs3)
# removeWord(fs3,"storm")
writeRetweet(fs3)
dC <- convertRetweet(fs3,w=T)

dAll <- rbind(dG,dM,dC)
fs=fs3 #f=fs1[1] #f=fs3[4] 
convertRetweet(fs)


removeWord <- function(fs,s) {
  for (f in fs) {
    df <- tbl_df(read.csv(f,header=T,sep=",")) 
    df <- df %>% filter(!grepl(s,tolower(text)))
    dir.create(file.path(getwd(), paste0(s,"_filt")),showWarnings = F)
    write.csv(df,file.path(paste0(s,"_filt"),paste0(f)),row.names = F)
  }
}

convertRetweet <- function(fs,w) {
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
  return(dAll)
}

# s=c("fossil fuel")
# appendMoral(fs3,c("fossil fuel","fossil-fuel")) #fs=fs3

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


# dAll <- rbind(dG.ME.sum,dG.NMNE.sum,dM.ME.sum,dM.NMNE.sum)

dAll %>% group_by(cond) %>% summarise(corID=cor(tid,rtid),
                                            tidM=mean(tid),
                                            rtidM=mean(rtid))
# average tweet and retweed ideology (in general)
dAll %>% summarise(corID=cor(tid,rtid),
                              tidM=mean(tid),
                              rtidM=mean(rtid))

ggplot(dAll, aes(count, colour = cond)) + 
  stat_ecdf(lwd = .3) + xlab("Retweet Frequency") + ylab("Cumulative Density") + facet_grid(. ~ topic)
ggplot(dAll,aes(x=tid,y=..density..,fill=cond)) + geom_density(alpha=.3) + facet_grid(. ~ topic)
ggplot(dAll,aes(x=rtid,y=..density..,fill=cond)) + geom_density(alpha=.3) + facet_grid(. ~ topic)
ggplot(dAll,aes(x=targetIDsd,y=..density..,fill=cond)) + geom_density(alpha=.3) + facet_grid(. ~ topic)
ggplot(dAll,aes(x=tid,y=targetIDsd,color=cond)) + 
  geom_point(shape=1,aes(size=count,color=cond)) + 
  geom_smooth() +
  facet_grid(. ~ topic)


dAll <- rbind(dGC.ME.sum,dGC.NMNE.sum)

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
new.xy.me <- expand_data(dAll %>% filter(cond=="ME",topic=="C"),breaks=0.25) %>%  mutate(cond="ME")
new.xy.nme <- expand_data(dAll %>% filter(cond=="NME",topic=="C"),breaks=0.25) %>% mutate(cond="NME")
new.xy.mne <- expand_data(dAll %>% filter(cond=="MNE",topic=="C"),breaks=0.25) %>% mutate(cond="MNE")
new.xy.nmne <- expand_data(dAll %>% filter(cond=="NMNE",topic=="C"),breaks=0.25) %>%  mutate(cond="NMNE")
new.xy <- rbind(new.xy.me,new.xy.nme,new.xy.mne,new.xy.nmne)

ggplot(new.xy, aes(x=y, y=x)) +
  geom_tile(aes(fill=prop), colour="white") +
  scale_fill_gradient(name="% of\ntweets", 
                      low = "white", high = "black", 
                      breaks=c(0, .0050, 0.010, 0.015, 0.02), limits=c(0, .021),
                      labels=c("0.0%", "0.5%", "1.0%", "1.5%", ">2%")) +
  labs(y="Estimated Ideology of Retweeter", x="Estimated Ideology of Author",
       title="Ideological Correlations in All Climate Tweets") + 
  scale_y_continuous(expand=c(0,0), breaks=(-2:2), limits=c(-3, 3)) +
  scale_x_continuous(expand=c(0,0), breaks=(-2:2), limits=c(-3, 3)) +
  theme(panel.border=element_rect(fill=NA), panel.background = element_blank()) +
  coord_equal()  +
  facet_wrap(~ cond)

# ggplot(dAll,aes(x=sourceID,y=targetID,color=cond)) + 
#   geom_point(aes(size=count,color=cond)) + 
#   geom_smooth() +
#   facet_grid(. ~ topic)

# dGC.NME %>% mutate(orig = ifelse(id_str %in% dGC.NME$retweeted_status.id_str, 1, 0)) %>% 
#   mutate(orig2 = ifelse(retweeted_status.id_str %in% dGC.NME$id_str, 1, 0)) %>% 
#   filter(orig==1|orig2==1) %>% arrange(retweeted_status.id_str) %>% 
#   mutate(timestamp=as.Date(strptime(timestamp,"%Y-%m-%d %H:%M:%S"))) %>%
#   group_by(retweeted_status.id_str) %>% summarise(count = n())

# dGC.NME$timestamp = as.Date(strptime(dGC.NME$timestamp, "%Y-%m-%d %H:%M:%S"))
# 
# dGC.NME$timestamp = as.Date(strptime(dGC.NME$timestamp, "%Y-%m-%d %H:%M:%S"))

?strptime
dGC <- rbind(dGC.NME,dGC.NMNE,dGC.ME,dGC.MNE) %>% dplyr::rename(ideo=ideology_estimate) %>% 
  mutate(topic="Guns") %>%  left_join(
    rbind(dGC.NME,dGC.NMNE,dGC.ME,dGC.MNE) %>%  group_by(cond) %>% 
      summarise(meanIdeo=mean(ideology_estimate))) %>% 
  left_join(
    rbind(dGC.NME,dGC.NMNE,dGC.ME,dGC.MNE) %>%  group_by(cond) %>% na.omit() %>% 
      summarise(meanIdeoRT=mean(ideology_estimate)))


# Combine old datasets ----------------------------------------------------

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
setwd("C:/Users/Julian/GDrive/1 Twitter Project/pythonScripts/ClimateChange/Combined/")
dC.All <- rbind(dCC1,dCC2,dCC3,dCC4,dC1,dC2,dC3,dC4)
write.csv(dC.All,"C_combined.csv",row.names = F)


####### Ideology distributions for each topic

# Old ideology histograms -------------------------------------------------

# setwd("C:/Users/Julian/GDrive/1 Twitter Project/")
# 
# d.gc <- read.csv("pythonScripts/GunControl/GC_tweets.csv", TRUE) 
# d.gc.nm.e <- read.csv("1 Gun Control Data Set/1 Data Files/GC_NM_E.csv", TRUE) 
# d.gc.nm.ne <- read.csv("1 Gun Control Data Set/1 Data Files/GC_NM_NE.csv", TRUE) 
# d.gc.m.e <- read.csv("1 Gun Control Data Set/1 Data Files/GC_M_E.csv", TRUE) 
# d.gc.m.ne <- read.csv("1 Gun Control Data Set/1 Data Files/GC_M_NE.csv", TRUE) 
# 
# d.gm <- read.csv("pythonScripts/GayMarriage/gaymar_tweets.csv", TRUE) 
# 
# d.cc <- read.csv("pythonScripts/Climate/climateFilt/climate_moral_emo.csv", TRUE) 
# 
# mean(d.gc[,1])
# mean(d.gc.nm.e[,1])
# mean(d.gc.nm.ne[,1])
# mean(d.gc.m.e[,1])
# mean(d.gc.mn.e[,1])
# 
# hist(d.gc[,1])
# hist(d.gc.m.e[,1]) #more liberals
# hist(d.gc.mn.e[,1]) #more conservatives
# hist(d.gc.nm.e[,1]) 
# hist(d.gc.nm.ne[,1]) #more conservative, more uniform
# 
# hist(d.gc[,1])
# hist(d.cc[,1])
# hist(d.gm[,1])


# Debugging ---------------------------------------------------------------


# df %>% mutate(orig = ifelse(id_str %in% df$retweeted_status.id_str, 1, 0)) %>% filter(orig==1)
# filter(df,id_str %in% retweeted_status.id_str)$ideology_estimate %>% na.omit()
# dfs %>% arrange(count)
# dfs %>% arrange(desc(count))
# dfs %>% filter(orig2==1)

# check1 <- df %>% mutate(orig = ifelse(id_str %in% df$retweeted_status.id_str, 1, 0)) %>% 
#          mutate(orig2 = ifelse(retweeted_status.id_str %in% df$id_str, 1, 0)) %>% 
#          filter(orig==1|orig2==1)  %>% group_by(retweeted_status.id_str) %>% 
#          summarise(count = n(),rtid = mean(ideology_estimate), targetIDvar=var(ideology_estimate)) %>% 
#          arrange(desc(count)) %>% filter(!is.na(retweeted_status.id_str)) %>%
#   arrange(retweeted_status.id_str) %>% mutate(id_str=retweeted_status.id_str)
# 
# check2 <- filter(df,id_str %in% retweeted_status.id_str) %>% arrange(id_str) %>% 
#   mutate(retweeted_status.id_str=id_str)
# 
# check1 %>% arrange(retweeted_status.id_str) %>% mutate(id_str=retweeted_status.id_str)
# check2 %>% arrange(id_str)
# 
# check2 %>% left_join(check1,by="id_str") %>% filter(retweeted_status.id_str.y!=id_str)
# 
# cbind(check2$id_str,check1$retweeted_status.id_str)
# 
# check2 %>% left_join(check1,by="id_str") %>% group_by(id_str) %>% summarise(count=n()) %>% 
#   arrange(desc(count))
# 
# ?rename
# filter(df,id_str %in% retweeted_status.id_str) %>% select(ideology_estimate) %>%
#   arrange(desc(ideology_estimate))

# Old code ----------------------------------------------------------------

# 
# writeRetweet <- function(fs) {
#   dAll <- NULL
#   for (f in fs) {
#     df <- tbl_df(read.csv(f,header=T,sep=",")) 
#     probTweets <- df %>% group_by(id_str) %>% summarise(count=n()) %>% filter(count>1) %>% select(1) %>% unlist
#     if (length(probTweets)>0){
#       df <- df %>% filter(!id_str %in% probTweets)
#       print(length(probTweets))
#     }
#     s = strsplit(f, ".csv") %$% strsplit(.[[1]],"/") %$% strsplit(.[[1]] %>% last(),"_") 
#     topic = s[[1]][1] #parse topic 
#     cond = paste(s[[1]][2],s[[1]][3],sep="") #parse condition
#     dfs <- df %>% mutate(orig = ifelse(id_str %in% df$retweeted_status.id_str, 1, 0)) %>% 
#       mutate(orig2 = ifelse(retweeted_status.id_str %in% df$id_str, 1, 0)) %>% 
#       filter(orig==1|orig2==1)  %>% group_by(retweeted_status.id_str) %>% 
#       summarise(count = n(),rtid = mean(ideology_estimate), targetIDsd=sd(ideology_estimate)) %>% 
#       arrange(desc(count))  %>% filter(!is.na(retweeted_status.id_str)) %>% 
#       mutate(tid = filter(df,id_str %in% retweeted_status.id_str)$ideology_estimate,
#              cond=cond,topic=topic,
#              M=ifelse(cond=="ME"|cond=="MNE",1,-1),
#              E=ifelse(cond=="ME"|cond=="NME",1,-1)) 
#     dAll <- rbind(dAll,dfs)
#     print(paste("finished processing file",f))
#   }
#   dir.create(file.path(getwd(), "RT"),showWarnings = F)
#   write.csv(dAll,file.path(paste0("RT"),paste0(topic,"_RT.csv")),row.names = F)
# }
