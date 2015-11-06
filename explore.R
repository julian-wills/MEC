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
dM <- convertRetweet(fs2) 

setwd("C:/Users/Julian/GDrive/1 Twitter Project/pythonScripts/ClimateChange/Combined/split/storm_filt/")
fs3 <- list.files(,".csv") 
# removeWord(fs3,"storm")
writeRetweet(fs3)
dC <- convertRetweet(fs3,w=T)

dAll <- rbind(dG,dM,dC)
fs=fs3 #f=fs1[1] #f=fs3[4] 
convertRetweet(fs)



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
dC.All <- rbind(dCC1,dCC2,dCC3,dCC4,dC1,dC2,dC3,dC4)
setwd("C:/Users/Julian/GDrive/1 Twitter Project/pythonScripts/ClimateChange/Combined/")
write.csv(dC.All,"C_combined.csv",row.names = F)

head(dCC1$text)


# Time --------------------------------------------------------------------
setwd("C:/Users/Julian/GDrive/1 Twitter Project/pythonScripts/GunControl/Guns/split/")
fs1 <- list.files(getwd(),".csv")
tweetsPerDay(fs1)
setwd("C:/Users/Julian/GDrive/1 Twitter Project/pythonScripts/GayMarriage/marriage/split/")
fs2 <- list.files(getwd(),".csv")
tweetsPerDay(fs2)
setwd("C:/Users/Julian/GDrive/1 Twitter Project/pythonScripts/ClimateChange/Combined/split/filt/")
fs3 <- list.files(getwd(),".csv")
tweetsPerDay(fs3)

require(lubridate)

setwd("C:/Users/Julian/GDrive/1 Twitter Project/pythonScripts/ClimateChange/Combined/")
setwd("C:/Users/Julian/GDrive/1 Twitter Project/pythonScripts/ClimateChange/Combined/split/filt/RT/")
dC.All <- read.csv('c_Tweets.csv') %>% tbl_df %>% mutate(day=as.Data(timsetamp %>% strsplit(" ")))

tmpN <- dC.All %>% group_by(user.id_str) %>% summarise(count=n()) %>% arrange(desc(count)) %>% dim()
tmpN / dC.All %>% dim() #~40% of tweeters are unique

# s2 = strsplit(f, ".csv") %$% strsplit(.[[1]],"/") %$% strsplit(.[[1]] %>% last(),"_") 
# tmp <- dC.All %>% select(timestamp) %>% unlist() %>% strsplit(" ") %>% strsplit([[.]],"-")

dC.All <- dC.All %>% mutate(day=yday(timestamp),dayDate = as.Date(day, origin = "2015-06-04"))

setwd("C:/Users/Julian/GDrive/1 Twitter Project/pythonScripts/ClimateChange/Climate/split/")
dC1 <- tbl_df(read.csv("C_M_E.csv",header=T,sep=",")) %>% mutate(cond="ME")
dC2 <- tbl_df(read.csv("C_M_NE.csv",header=T,sep=",")) %>% mutate(cond="MNE")
dC3 <- tbl_df(read.csv("C_NM_E.csv",header=T,sep=",")) %>% mutate(cond="NME")
dC4 <- tbl_df(read.csv("C_NM_NE.csv",header=T,sep=",")) %>% mutate(cond="NMNE")
dC.All <- rbind(dC1,dC2,dC3,dC4)

dC.All <- dC.All %>% mutate(day=yday(timestamp),dayDate = as.Date(day, origin = "2015-06-04"))
plotDf <- count(dC.All, cond, day=day) %>% mutate(day=as.Date(day-1, origin = "2015-01-01"))

require(scales)

ggplot(data=plotDf, aes(x=day, y=n, colour=cond)) + 
  geom_line(size=1) +
  scale_y_continuous(labels = comma) + 
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))


# Feature selection -------------------------------------------------------
require(quanteda)
dT2 <- textfile("C:/Users/Julian/GDrive/Misc/RTextAnalysis/ITAUR/3_file_import/GC_tweets.csv",textField = "text")

# popular hashtags
mycorpus <- corpus(dT2)
mydfm <- selectFeatures(dfm(mycorpus), "#*", "keep", valuetype = "glob") 
topfeatures(mydfm, nfeature(mydfm))

# mydfm <- dfm(mycorpus, verbose=FALSE, stem=TRUE,
#                 ignoredFeatures=c(stopwords('english')))


# plotting tweets ---------------------------------------------------------
require(lubridate)
require(scales)

setwd("C:/Users/Julian/GDrive/1 Twitter Project/pythonScripts/allTweets/")
dAll <- tbl_df(read.csv("allRetweets.csv",header=T,sep=",")) %>% 
  mutate(day=yday(timestamp),dayDate = as.Date(day, origin = "2015-01-01"))

# Original tweets/retweets over time for guns
plotDf <- count(dAll  %>% filter(orig==1), cond, topic, day=day) %>% 
  mutate(day=as.Date(day-1, origin = "2015-01-01"))
ggplot(data=plotDf %>% filter(topic=="G"), aes(x=day, y=n, colour=cond)) + 
  geom_line(size=1) +
  scale_y_continuous(labels = comma) + 
  labs(y="Number of Tweets", x="Day",title=paste("Original Tweets/Day related to Guns")) + 
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))
plotDf <- count(dAll  %>% filter(orig2==1), cond, topic, day=day) %>% 
  mutate(day=as.Date(day-1, origin = "2015-01-01"))
ggplot(data=plotDf %>% filter(topic=="G"), aes(x=day, y=n, colour=cond)) + 
  geom_line(size=1) +
  scale_y_continuous(labels = comma) + 
  labs(y="Number of Tweets", x="Day",title=paste("Retweets/Day related to Guns",topic)) + 
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

# Original tweets/retweets over time for marriage
plotDf <- count(dAll  %>% filter(orig==1), cond, topic, day=day) %>% 
  mutate(day=as.Date(day-1, origin = "2015-01-01"))
ggplot(data=plotDf %>% filter(topic=="M"), aes(x=day, y=n, colour=cond)) + 
  geom_line(size=1) +
  scale_y_continuous(labels = comma) + 
  labs(y="Number of Tweets", x="Day",title=paste("Original Tweets/Day related to Marriage")) + 
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))
plotDf <- count(dAll  %>% filter(orig2==1), cond, topic, day=day) %>% 
  mutate(day=as.Date(day-1, origin = "2015-01-01"))
ggplot(data=plotDf %>% filter(topic=="M"), aes(x=day, y=n, colour=cond)) + 
  geom_line(size=1) +
  scale_y_continuous(labels = comma) + 
  labs(y="Number of Tweets", x="Day",title=paste("Retweets/Day related to Marriage",topic)) + 
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

# Original tweets/retweets over time for climate
plotDf <- count(dAll  %>% filter(orig==1), cond, topic, day=day) %>% 
  mutate(day=as.Date(day-1, origin = "2015-01-01"))
ggplot(data=plotDf %>% filter(topic=="C"), aes(x=day, y=n, colour=cond)) + 
  geom_line(size=1) +
  scale_y_continuous(labels = comma) + 
  labs(y="Number of Tweets", x="Day",title=paste("Original Tweets/Day related to Climate")) + 
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))
plotDf <- count(dAll  %>% filter(orig2==1), cond, topic, day=day) %>% 
  mutate(day=as.Date(day-1, origin = "2015-01-01"))
ggplot(data=plotDf %>% filter(topic=="C"), aes(x=day, y=n, colour=cond)) + 
  geom_line(size=1) +
  scale_y_continuous(labels = comma) + 
  labs(y="Number of Tweets", x="Day",title=paste("Retweets/Day related to Climate",topic)) + 
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))


# Top tweets --------------------------------------------------------------

setwd("C:/Users/Julian/GDrive/1 Twitter Project/pythonScripts/GayMarriage/gayMarriageMoral/split/")
fs <- list.files(getwd(),".csv",full.names = T) 
dGM <- convertRetweet(fs,lastTime = T)
dGM %>% group_by(cond) %>% filter(M==1) %>% top_n(3,wt=count) %>% select(text) %>% unlist()


# Persistence of tweets ---------------------------------------------------

setwd("C:/Users/Julian/GDrive/1 Twitter Project/pythonScripts/allTweets/")

dAll <- tbl_df(read.csv("allRTSummary.csv",header=T,sep=",")) %>% 
  mutate(origDay=as.POSIXct(origTime),lastDay = as.POSIXct(lastTime),
         persist=as.numeric(lastDay-origDay,units="days"))
dAll %>% filter(topic=="C") %>% group_by(cond) %>% summarise(mPersist=mean(persist))
#! increases for each cell

# bar plots for retweet frequency between condition and topic
ggplot(dAll %>% filter(topic=="C")  %>% group_by(cond) %>%
         summarise(mPersist=mean(persist),se=sd(persist)/sqrt(n())),
       aes(x=factor(cond),y=mPersist,fill=factor(cond))) + 
  geom_bar(stat="identity") +
  scale_fill_manual(values = c("firebrick1","darkslateblue","lightcoral","lightslateblue")) +
  geom_errorbar(aes(ymin=mPersist-se, ymax=mPersist+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))

setwd("C:/Users/Julian/GDrive/1 Twitter Project/pythonScripts/GayMarriage/gayMarriageMoral/split/RT")
dGM <- tbl_df(read.csv("GM_RT.csv",header=T,sep=",")) %>% 
  mutate(origDay=yday(origTime),lastDay = yday(lastTime),persist=lastDay-origDay)
dGM %>% group_by(E) %>% summarise(mPersist=mean(persist)) %>% arrange(desc(E))
ggplot(dGM %>% group_by(E) %>%
         summarise(mPersist=mean(persist),se=sd(persist)/sqrt(n())),
       aes(x=factor(E),y=mPersist,fill=factor(E))) + 
  geom_bar(stat="identity") +
  scale_fill_manual(values = c("firebrick1","darkslateblue","lightcoral","lightslateblue")) +
  geom_errorbar(aes(ymin=mPersist-se, ymax=mPersist+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))

setwd("C:/Users/Julian/GDrive/1 Twitter Project/pythonScripts/GunControl/GC2013/filt/RT")
dGC <- tbl_df(read.csv("GC_RT.csv",header=T,sep=",")) %>% 
  mutate(origDay=yday(origTime),lastDay = yday(lastTime),persist=lastDay-origDay)
dGC %>% group_by(E) %>% summarise(mPersist=mean(persist)) %>% arrange(desc(E))
ggplot(dGC %>% group_by(E) %>%
         summarise(mPersist=mean(persist),se=sd(persist)/sqrt(n())),
       aes(x=factor(E),y=mPersist,fill=factor(E))) + 
  geom_bar(stat="identity") +
  scale_fill_manual(values = c("firebrick1","darkslateblue","lightcoral","lightslateblue")) +
  geom_errorbar(aes(ymin=mPersist-se, ymax=mPersist+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))

# dAll %>% filter(topic)

hist(dGM$persist)


# Contagion speed ---------------------------------------------------------

# load in tweets
setwd("C:/Users/Julian/GDrive/1 Twitter Project/pythonScripts/allTweets/")
dAll <- tbl_df(read.csv("corpRTs.csv",header=T,sep=",")) %>% filter(topic=="C")

# isolate tweets that get retweeted at least 10 times
dRT10  <- dAll %>% group_by(retweeted_status.id_str) %>% top_n(10,timestamp) %>% 
  summarise(count=n()) %>% filter(count==10) %>% left_join(dAll)

# create dataframe for leftovers 
dRT9  <- dAll %>% group_by(retweeted_status.id_str) %>% top_n(10,timestamp) %>% 
  summarise(count=n()) %>% filter(count<10)

# create dataframe of first 10 RTs for tweets that have been retweeted at least 10 times. 
# probably some redundancy here... but it's late
dRT10.1 <- dAll %>% group_by(retweeted_status.id_str) %>% do(head(., n=10)) %>% 
  filter(!retweeted_status.id_str %in% dRT9$retweeted_status.id_str)

# grab the timestamp of the 10th retweet
dRT10 <- dRT10.1 %>% do(tail(.,n=1)) %>% select(retweeted_status.id_str,time10=timestamp) 

# compute summary statistics for first 10 retweets
dRT10 <- dRT10 %>%  
  left_join(dRT10.1 %>% summarise(meanIdeo=mean(ideology_estimate),sdIdeo=sd(ideology_estimate),
                                  rangeIdeo=abs(max(ideology_estimate)-min(ideology_estimate)))) %>% 
  # join w/ original tweets, compute timespan (hours)
  left_join(dAll %>% filter(orig==1) %>% 
              select(origIdeo=ideology_estimate,id_str,time1=timestamp,topic,cond),
            by=c("retweeted_status.id_str"="id_str")) %>% 
  mutate(speedHrs=as.numeric(as.POSIXct(time10)-as.POSIXct(time1),units="hours")) %>% 
  select(RTid=retweeted_status.id_str,speedHrs,origIdeo,meanIdeo:rangeIdeo,topic,cond,time1,time10) 


# dRT10 %>% group_by(cond) %>% summarise_each(funs(mean),speedHrs:rangeIdeo) %>% 
#   mutate(diffIdeo=abs(origIdeo-meanIdeo))
# should maybe do this analysis on the top 10%
  
# load in persist tweets
setwd("C:/Users/Julian/GDrive/1 Twitter Project/pythonScripts/allTweets/")
dPersist <- tbl_df(read.csv("allRTSummary.csv",header=T,sep=",")) %>% 
  mutate(origDay=as.POSIXct(origTime),lastDay = as.POSIXct(lastTime),
         persist=as.numeric(lastDay-origDay,units="days")) %>% 
  select(RTid=retweeted_status.id_str,persist,count:text)

# merge w/ persist and rename variables
dRT10.2 <- dRT10 %>% left_join(dPersist,"RTid") %>% 
  select(RTid,count,speedHrs,persistDays=persist,ID.0=origIdeo,meanID.10=meanIdeo,sdID.10=sdIdeo,
         rangeID.10=rangeIdeo,meanID=rtid,sdID=targetIDsd,cond,topic,text)

# summary statistics for each condition
dRT10.2 %>% group_by(cond) %>% summarise_each(funs(mean),count:sdID) %>% 
  mutate(diffID.10=abs(ID.0-meanID.10),diffID=abs(ID.0-meanID)) %>% 
  mutate_each(funs(round(.,2)),count:diffID) %>% na.omit()

# constraining to top 10% in each condition
dRT10.2 %>% ungroup() %>% arrange(desc(count)) %>% group_by(cond) %>% 
  filter(cume_dist(desc(count)) < 0.1) %>% summarise_each(funs(mean),count:sdID) %>% 
  mutate(diffID.10=abs(ID.0-meanID.10),diffID=abs(ID.0-meanID)) %>% 
  mutate_each(funs(round(.,2)),count:diffID)

# load in retweets per day file
setwd("C:/Users/Julian/GDrive/1 Twitter Project/pythonScripts/allTweets/")
dAll <- tbl_df(read.csv("corpRTs.csv",header=T,sep=",")) %>% 
  mutate(day=yday(timestamp),dayDate = as.Date(day, origin = "2015-01-01")) %>% 
  filter(!is.na(retweeted_status.id_str))
dRT.Day <- count(dAll  %>% filter(topic=="C"), cond, topic, rtid=retweeted_status.id_str,day=day) %>% 
  mutate(day=as.Date(day-1, origin = "2015-01-01")) %>% summarise(RTperDay=mean(n)) %>% 
  ungroup() %>% select(RTid=rtid,RTperDay)

dRT10.3 <- dRT10.2 %>% left_join(dRT.Day) %>% select(RTid:count,RTperDay,speedHrs:text) %>% 
  ungroup() %>% arrange(desc(count))

setwd("C:/Users/Julian/GDrive/1 Twitter Project/pythonScripts/allTweets/")
write.csv(dRT10.3,"climate/climateRetweetSpeed.csv",row.names = F)


# Retweets over time (per day) repeated measured --------------------------

setwd(paste0(userDir,"/1 Twitter Project/pythonScripts/allTweets"))
dTime <- tbl_df(read.csv("corpRTs.csv",header=T,sep=",")) %>% filter(topic=="C")
dTime2 <- dTime %>% mutate(retweeted_status.id_str=ifelse(orig==1,id_str,retweeted_status.id_str)) %>% 
  group_by(retweeted_status.id_str) %>% mutate(timestamp=as.POSIXct(timestamp)) %>% 
  mutate(minsPassed=as.numeric(timestamp-min(timestamp))/60) %>% 
  mutate(cumsum1=row_number())
  
dTime3 <- dTime2 %>% filter(orig2==1) %>% mutate(cumsum1=row_number()) %>% 
  group_by(cumsum1,cond) %>% summarise(minsPassedM=mean(minsPassed))

dTime4 <- dTime2 %>% filter(orig2==1,minsPassed>0) %>% mutate(cumsum1=row_number()) %>% 
  group_by(cond)

ggplot(dTime3,aes(x=minsPassedM,y=cumsum1,color=factor(cond))) +
  geom_point(alpha=.3) +
  scale_x_log10(breaks=c(30,60,360,720,1440,2880,10080)) + xlab("Minuted Passed Since Initial Tweet (Log Scale)") + 
  ylab("Cumulative Count of Retweets") +
  stat_smooth(method="lm") +
  ggtitle("Retweet Frequency Over Time\n Each streak is One Tweet's Timeseries")

ggplot(dTime4,aes(x=minsPassed,y=cumsum1,color=factor(cond))) +
  geom_point(alpha=.1) +
  # scale_x_log10(breaks=c(30,60,360,720,1440,2880,10080)) +
  coord_trans(x="log10000") +
  scale_x_continuous(breaks=c(.5,2,5,30,60,720,1440,10080)) +
  scale_y_sqrt(breaks=c(1,5,10,50,100,250,500,750,1000)) +
  xlab("Minuted Passed Since Initial Tweet (Log Scale)") + 
  ylab("Cumulative Count of Retweets")

log10000_trans = function() trans_new("log10000", function(x) log(x,10000), function(x) log(x,10000))
exp10_trans = function() trans_new("exp10", function(x) exp(x,10), function(x) exp(x,10))

library(dplyr)
cum_var <- function(x){
  n <- 1:length(x)
  (cumsum(x^2)-n*cummean(x)^2)/(n-1)
}

dTime4 %<>% rename(ideo=ideology_estimate)
dTime4 <- dTime4 %>% group_by(retweeted_status.id_str) %>% mutate(diffIdeo=abs(ideo-lag(ideo)))
dTime4 <- dTime4 %>% group_by(retweeted_status.id_str) %>%
  mutate(avgIdeo=cummean(ideo),devIdeo=abs(ideo-avgIdeo),varIdeo=cum_var(ideo)) 

ggplot(dTime4,aes(x=minsPassed,y=cumsum1)) +
  geom_point(size=2,alpha=.05) +
  # scale_x_log10(breaks=c(30,60,360,720,1440,2880,10080)) +
  # scale_colour_gradient2(low="blue", high="red",limits=c(-2,2)) +
  coord_trans(x="log10000") +
  scale_x_continuous(breaks=c(.5,2,5,30,120,720,1440,10080)) +
  scale_y_sqrt(breaks=c(1,5,10,50,100,250,500,750,1000)) +
  xlab("Minuted Passed Since Initial Tweet (Log Scale)") + 
  ylab("Cumulative Count of Retweets") +
  facet_wrap( ~ cond,nrow=2)

ggplot(dTime4,aes(x=minsPassed,y=cumsum1,color=ideo)) +
  geom_point(size=2,alpha=.5) +
  # scale_x_log10(breaks=c(30,60,360,720,1440,2880,10080)) +
  scale_colour_gradient2(mid="grey", low="blue", high="red",limits=c(-2,2)) +
  coord_trans(x="log10000") +
  scale_x_continuous(breaks=c(.5,2,5,30,120,720,1440,10080)) +
  scale_y_sqrt(breaks=c(1,5,10,50,100,250,500,750,1000)) +
  xlab("Minuted Passed Since Initial Tweet (Log Scale)") + 
  ylab("Cumulative Count of Retweets") +
  facet_wrap( ~ cond,nrow=2)

ggplot(dTime4 %>% filter(ideo<0),aes(x=minsPassed,y=cumsum1,color=ideo)) +
  geom_point(size=2,alpha=1) +
  # scale_x_log10(breaks=c(30,60,360,720,1440,2880,10080)) +
  scale_colour_gradient2(low="blue", high="red",limits=c(-2,2)) +
  coord_trans(x="log10000") +
  scale_x_continuous(breaks=c(.5,2,5,30,120,720,1440,10080)) +
  scale_y_sqrt(breaks=c(1,5,10,50,100,250,500,750,1000)) +
  xlab("Minuted Passed Since Initial Tweet (Log Scale)") + 
  ylab("Cumulative Count of Retweets") +
  facet_wrap( ~ cond,nrow=2)

ggplot(dTime4 %>% filter(ideo>0),aes(x=minsPassed,y=cumsum1,color=ideo)) +
  geom_point(size=2,alpha=1) +
  # scale_x_log10(breaks=c(30,60,360,720,1440,2880,10080)) +
  scale_colour_gradient2(low="blue", high="red",limits=c(-2,2)) +
  coord_trans(x="log10000") +
  scale_x_continuous(breaks=c(.5,2,5,30,120,720,1440,10080)) +
  scale_y_sqrt(breaks=c(1,5,10,50,100,250,500,750,1000)) +
  xlab("Minuted Passed Since Initial Tweet (Log Scale)") + 
  ylab("Cumulative Count of Retweets") +
  facet_wrap( ~ cond,nrow=2)

# difference in ideology (ugly plot)
ggplot(dTime4,aes(x=minsPassed,y=cumsum1,color=diffIdeo)) +
  geom_point(size=2,alpha=.5) +
  # scale_x_log10(breaks=c(30,60,360,720,1440,2880,10080)) +
  # scale_colour_gradient2(low="blue", high="red",limits=c(-2,2)) +
  scale_color_continuous(low="white",high="red") +
  coord_trans(x="log10000") +
  scale_x_continuous(breaks=c(.5,2,5,30,120,720,1440,10080)) +
  scale_y_sqrt(breaks=c(1,5,10,50,100,250,500,750,1000)) +
  theme(panel.background = element_rect(fill = "black")) +
  xlab("Minuted Passed Since Initial Tweet (Log Scale)") + 
  ylab("Cumulative Count of Retweets") +
  facet_wrap( ~ cond,nrow=2)

# which tweets attract bipartisan support?
ggplot(dTime4 %>% filter(varIdeo<2),aes(x=minsPassed,y=cumsum1,color=varIdeo)) +
  geom_point(size=2,alpha=.8) +
  # scale_x_log10(breaks=c(30,60,360,720,1440,2880,10080)) +
  # scale_colour_gradient2(low="blue", high="red",limits=c(-2,2)) +
  scale_color_continuous(low="white",high="red") +
  coord_trans(x="log10000") +
  scale_x_continuous(breaks=c(.5,2,5,30,120,720,1440,10080)) +
  scale_y_sqrt(breaks=c(1,5,10,50,100,250,500,750,1000)) +
  theme(panel.background = element_rect(fill = "grey")) +
  xlab("Minuted Passed Since Initial Tweet (Log Scale)") + 
  ylab("Cumulative Count of Retweets") +
  facet_wrap( ~ cond,nrow=2)

# do bipartisan tweets get retweeted more or less?
ggplot(dTime4 %>% filter(varIdeo<2.5),aes(x=minsPassed,y=cumsum1,color=ideo)) +
  geom_point(aes(size=varIdeo),alpha=.9) +
  # scale_x_log10(breaks=c(30,60,360,720,1440,2880,10080)) +
  scale_colour_gradient2(low="blue", mid="grey",high="red",limits=c(-2,2)) +
  # scale_color_continuous(low="white",high="red") +
  coord_trans(x="log10000") +
  scale_x_continuous(breaks=c(.5,2,5,30,120,720,1440,10080)) +
  scale_y_sqrt(breaks=c(1,5,10,50,100,250,500,750,1000)) +
  theme(panel.background = element_rect(fill = "white")) +
  xlab("Minuted Passed Since Initial Tweet (Log Scale)") + 
  ylab("Cumulative Count of Retweets") +
  facet_wrap( ~ cond,nrow=2)


ggplot(dT2 %>% filter(cume_dist(desc(freq)) < 1), aes(freq, colour = cond, linetype = cond)) + 
  stat_ecdf(lwd = .3) +
hist(dTime2$minsPassed)

save(dTime4,file="MC_time.RData")

# Network of tweets ------------------------------------------------------
setwd("C:/Users/Julian/GDrive/1 Twitter Project/pythonScripts/allTweets/")
dNet <- tbl_df(read.csv("joinedRTs.csv",header=T,sep=",")) %>% 
  transmute(Source=author,Target=retweeter,topic=topic,cond=cond)
# df <- tweetsToDF(tweets) #SMaPP code
# names(df) <- c("Source", "Target")

# write.csv(dNet %>% filter(topic=="C"),"joinedRTs_Climate.csv",row.names = F) #climate only for Dominic


dNet <- tbl_df(read.csv("joinedRTs.csv",header=T,sep=",")) %>% 
  mutate(party=ifelse(tID+rtID<0,"L","R"),
         jointID=tID+rtID)
df <- dNet[1:2000,] 

library(igraph)
g <- graph.data.frame(df, directed=TRUE) ## network object

# bad.vs<-V(g)[degree(g)<3] #identify those vertices part of less than three edges
# g<-delete.vertices(g, bad.vs) #exclude them from the graph

# g <- graph.edgelist(as.matrix(df), directed=TRUE) ## network object
set.seed(12345)
l <- layout.fruchterman.reingold(g)
d <- data.frame(l); names(d) <- c("x", "y") ## data frame for plot
d$id <- V(g)$name ## adding names
d$degree <- degree(g)
d <- merge(d, df, all.x=TRUE) ## adding party

edgelist <- get.edgelist(g, names=FALSE) ## edgelist
edges <- data.frame(d[edgelist[,1],c("x", "y")], d[edgelist[,2],c("x", "y")])
names(edges) <- c("x1", "y1", "x2", "y2") ## coordinates of each edge
edges$party <- NA
edges$jointID <- NA

# coords <- layout_(g1, as_star())
# plot(g1, layout = coords)


# 4) creating plot with ggplot2

# library(ggplot2)
p <- ggplot(d, aes(x=x, y=y, fill=party, size=degree))
pq <- p + geom_segment(
  aes(x=x1, y=y1, xend=x2, yend=y2), 
  data=edges, color="grey20", size=0.25, alpha=1/10) +
  geom_point(aes(size=degree), color="grey20", shape=21) +
  scale_size_continuous(range=c(2,7)) +
  # scale_fill_manual(values=c("blue", "red")) + 
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


# wordclouds --------------------------------------------------------------
require(quanteda)

if (grepl("^C:/",getwd())) {
  userDir <- "C:/Users/Julian/GDrive" #PC
} else {
  userDir <- "/Users/julian/GDrive" #Mac
}

setwd(paste0(userDir,"/1 Twitter Project/pythonScripts/allTweets"))
dWord <- tbl_df(read.csv("allTweets.csv",header=T,sep=",")) %>% filter(topic=="C")

# most frequent MORAL EMOTIONAL tweets
dWordME <- dWord %>% filter(cond=="ME")
tweetCorpus <- corpus(dWordME$text, notes="Created as part of a demo.") #no news is good news
tweetDfm <- dfm(tweetCorpus, ignoredFeatures = c("t.co","http",stopwords("english")))
plot(tweetDfm, random.order=F,random.color = F, max.words=80, rot.per = .1, colors="black")

hist(sort(lexdiv(tweetDfm, "R")))
hist(readability(tweetCorpus, measure = "Flesch.Kincaid"))
hist(ntoken(tweetCorpus)) # how many tokens (total words)
hist(ntype(tweetCorpus)) # how many types (unique words)
topfeatures(tweetDfm)

# what are conservatives tweeting about?
dWordR <- dWordME %>% filter(meanID>0)
tweetCorpus <- corpus(dWordR$text, notes="Created as part of a demo.") #no news is good news
tweetDfm <- dfm(tweetCorpus, ignoredFeatures = c("t.co","http","climate","change",stopwords("english")))
plot(tweetDfm, random.order=F,random.color = F, max.words=80, rot.per = .1, colors="red")

hist(sort(lexdiv(tweetDfm, "R")))
hist(readability(tweetCorpus, measure = "Flesch.Kincaid"))
hist(ntoken(tweetCorpus)) # how many tokens (total words)
hist(ntype(tweetCorpus)) # how many types (unique words)
topfeatures(tweetDfm)

# what are liberals tweeting about?
dWordL <- dWordME %>% filter(meanID<0)
tweetCorpus <- corpus(dWordL$text, notes="Created as part of a demo.") #no news is good news
tweetDfm <- dfm(tweetCorpus, ignoredFeatures = c("t.co","http","climate","change",stopwords("english")))
plot(tweetDfm, random.order=F,random.color = F, max.words=80, rot.per = .1, colors="blue")

hist(sort(lexdiv(tweetDfm, "R")))
hist(readability(tweetCorpus, measure = "Flesch.Kincaid"))
hist(ntoken(tweetCorpus)) # how many tokens (total words)
hist(ntype(tweetCorpus)) # how many types (unique words)
topfeatures(tweetDfm)




# most frequent NONMORAL EMOTIONAL tweets
dWordNME <- dWord %>% filter(cond=="NME")
tweetCorpus <- corpus(dWordNME$text, notes="Created as part of a demo.") #no news is good news
tweetDfm <- dfm(tweetCorpus, ignoredFeatures = c("t.co","http",stopwords("english")))
plot(tweetDfm, random.order=F,random.color = F, max.words=80, rot.per = .1, colors="black")

hist(sort(lexdiv(tweetDfm, "R")))
hist(readability(tweetCorpus, measure = "Flesch.Kincaid"))
hist(ntoken(tweetCorpus)) # how many tokens (total words)
hist(ntype(tweetCorpus)) # how many types (unique words)
topfeatures(tweetDfm)

# what are conservatives tweeting about?
dWordR <- dWordNME %>% filter(meanID>0)
tweetCorpus <- corpus(dWordR$text, notes="Created as part of a demo.") #no news is good news
tweetDfm <- dfm(tweetCorpus, ignoredFeatures = c("t.co","http","climate","change",stopwords("english")))
plot(tweetDfm, random.order=F,random.color = F, max.words=80, rot.per = .1, colors="red")

hist(sort(lexdiv(tweetDfm, "R")))
hist(readability(tweetCorpus, measure = "Flesch.Kincaid"))
hist(ntoken(tweetCorpus)) # how many tokens (total words)
hist(ntype(tweetCorpus)) # how many types (unique words)
topfeatures(tweetDfm)

# what are liberals tweeting about?
dWordL <- dWordNME %>% filter(meanID<0)
tweetCorpus <- corpus(dWordL$text, notes="Created as part of a demo.") #no news is good news
tweetDfm <- dfm(tweetCorpus, ignoredFeatures = c("t.co","http","climate","change",stopwords("english")))
plot(tweetDfm, random.order=F,random.color = F, max.words=80, rot.per = .1, colors="blue")

hist(sort(lexdiv(tweetDfm, "R")))
hist(readability(tweetCorpus, measure = "Flesch.Kincaid"))
hist(ntoken(tweetCorpus)) # how many tokens (total words)
hist(ntype(tweetCorpus)) # how many types (unique words)
topfeatures(tweetDfm)






# most frequent MORAL UNEMOTIONAL tweets
dWordMNE <- dWord %>% filter(cond=="MNE")
tweetCorpus <- corpus(dWordMNE$text, notes="Created as part of a demo.") #no news is good news
tweetDfm <- dfm(tweetCorpus, ignoredFeatures = c("t.co","http",stopwords("english")))
plot(tweetDfm, random.order=F,random.color = F, max.words=80, rot.per = .1, colors="black")

hist(sort(lexdiv(tweetDfm, "R")))
hist(readability(tweetCorpus, measure = "Flesch.Kincaid"))
hist(ntoken(tweetCorpus)) # how many tokens (total words)
hist(ntype(tweetCorpus)) # how many types (unique words)
topfeatures(tweetDfm)


# what are conservatives tweeting about?
dWordR <- dWordMNE %>% filter(meanID>0)
tweetCorpus <- corpus(dWordR$text, notes="Created as part of a demo.") #no news is good news
tweetDfm <- dfm(tweetCorpus, ignoredFeatures = c("t.co","http","climate","change",stopwords("english")))
plot(tweetDfm, random.order=F,random.color = F, max.words=80, rot.per = .1, colors="red")

hist(sort(lexdiv(tweetDfm, "R")))
hist(readability(tweetCorpus, measure = "Flesch.Kincaid"))
hist(ntoken(tweetCorpus)) # how many tokens (total words)
hist(ntype(tweetCorpus)) # how many types (unique words)
topfeatures(tweetDfm)

# what are liberals tweeting about?
dWordL <- dWordMNE %>% filter(meanID<0)
tweetCorpus <- corpus(dWordL$text, notes="Created as part of a demo.") #no news is good news
tweetDfm <- dfm(tweetCorpus, ignoredFeatures = c("t.co","http","climate","change",stopwords("english")))
plot(tweetDfm, random.order=F,random.color = F, max.words=80, rot.per = .1, colors="blue")

hist(sort(lexdiv(tweetDfm, "R")))
hist(readability(tweetCorpus, measure = "Flesch.Kincaid"))
hist(ntoken(tweetCorpus)) # how many tokens (total words)
hist(ntype(tweetCorpus)) # how many types (unique words)
topfeatures(tweetDfm)




# most frequent NONMORAL UNEMOTIONAL tweets
dWordNMNE <- dWord %>% filter(cond=="NMNE")
tweetCorpus <- corpus(dWordNMNE$text, notes="Created as part of a demo.") #no news is good news
tweetDfm <- dfm(tweetCorpus, ignoredFeatures = c("t.co","http",stopwords("english")))
plot(tweetDfm, random.order=F,random.color = F, max.words=80, rot.per = .1, colors="black")

hist(sort(lexdiv(tweetDfm, "R")))
hist(readability(tweetCorpus, measure = "Flesch.Kincaid"))
hist(ntoken(tweetCorpus)) # how many tokens (total words)
hist(ntype(tweetCorpus)) # how many types (unique words)
topfeatures(tweetDfm)

# what are conservatives tweeting about?
dWordR <- dWordNMNE %>% filter(meanID>0)
tweetCorpus <- corpus(dWordR$text, notes="Created as part of a demo.") #no news is good news
tweetDfm <- dfm(tweetCorpus, ignoredFeatures = c("t.co","http","climate","change",stopwords("english")))
plot(tweetDfm, random.order=F,random.color = F, max.words=80, rot.per = .1, colors="red")

hist(sort(lexdiv(tweetDfm, "R")))
hist(readability(tweetCorpus, measure = "Flesch.Kincaid"))
hist(ntoken(tweetCorpus)) # how many tokens (total words)
hist(ntype(tweetCorpus)) # how many types (unique words)
topfeatures(tweetDfm)

# what are liberals tweeting about?
dWordL <- dWordNMNE %>% filter(meanID<0)
tweetCorpus <- corpus(dWordL$text, notes="Created as part of a demo.") #no news is good news
tweetDfm <- dfm(tweetCorpus, ignoredFeatures = c("t.co","http","climate","change",stopwords("english")))
plot(tweetDfm, random.order=F,random.color = F, max.words=80, rot.per = .1, colors="blue")

hist(sort(lexdiv(tweetDfm, "R")))
hist(readability(tweetCorpus, measure = "Flesch.Kincaid"))
hist(ntoken(tweetCorpus)) # how many tokens (total words)
hist(ntype(tweetCorpus)) # how many types (unique words)
topfeatures(tweetDfm)

summary(tweetCorpus)

lexdiv(tweetDfm, "R")
dotchart(sort(lexdiv(tweetDfm, "R")))

readab <- readability(tweetCorpus, measure = "Flesch.Kincaid")
dotchart(sort(readab))

# similarity(tweetDfm, method="cosine")
hist(sort(lexdiv(tweetDfm, "R")))
hist(readability(tweetCorpus, measure = "Flesch.Kincaid"))
hist(ntoken(tweetCorpus)) # how many tokens (total words)
hist(ntype(tweetCorpus)) # how many types (unique words)
topfeatures(tweetDfm)



# Descriptive/Distributions - SMaPP presentation --------------------------

setwd(paste0(userDir,"/1 Twitter Project/pythonScripts/allTweets"))
dCAll <- tbl_df(read.csv("allTweets.csv",header=T,sep=",")) %>% filter(topic=="C") 
dCAll %>% select(timestamp) %>% filter(row_number()==1) #when was beginning?
dCAll %>% select(timestamp) %>% filter(row_number()==n()) #when was end?
(nT.orig = dCAll %>% filter(is.na(retweeted_status.id_str)) %>% summarise(n()) )#how many of these are tweets?
(nRT.orig <- dCAll %>% filter(retweeted_status.id_str>0) %>% summarise(n()))  #how many RTs overall?
nRT.orig/(nT.orig+nRT.orig) #proportion of corpus that is RT

dCRT <- tbl_df(read.csv("corpRTs.csv",header=T,sep=",")) %>% filter(topic=="C") 
(nRT = dCRT %>% filter(retweeted_status.id_str>0) %>% summarise(n()))#how many RTs based on originals?
(nT = dCRT %>% filter(is.na(retweeted_status.id_str)) %>% summarise(n()))#how many tweets gets retweeted?
dCRT %>% summarise_each(funs(mean,sd,min,max),ideology_estimate) #ideology of final sample?

require(quanteda)
noisyWords = c("t.co","http","rt","u","009f")

# most frequent words overall
tweetCorpus <- corpus(dCAll$text, notes="Created as part of a demo.") 
tweetDfm <- dfm(tweetCorpus, ignoredFeatures = c(noisyWords,stopwords("english")))
plot(tweetDfm, random.order=F,random.color = F, max.words=80, rot.per = .1, colors="black")

                                
for (c in dCRT %>% distinct(cond) %$% cond) {
  print(paste("Descriptives for",c))
  (ncRT = dCRT %>% filter(cond==c,retweeted_status.id_str>0) %>% summarise(n())) #how many RTs based on originals?
  (ncT = dCRT %>% filter(cond==c,is.na(retweeted_status.id_str)) %>% summarise(n())) #how many tweets gets retweeted?
  ncRT/nRT #proportion of corpus orig RTs
  ncT/nT #proportion of corpus orig tweets

  print(ncRT)
  print(ncT)
  print(ncRT/nRT)
  print(ncT/nT)
  
  dCRT %>% filter(cond==c) %>% 
    summarise_each(funs(mean,sd,min,max),ideology_estimate) %>% print() #ideology of condition?
  
  corpus(dCRT %>% filter(cond==c) %$% text) %>% 
    dfm(ignoredFeatures = c(noisyWords,stopwords("english"))) %>% 
          plot(random.order=F,random.color = F, max.words=80, rot.per = .1, colors="black")
}



dCRT %>% group_by(cond) %>% 
  summarise(ncRT=)
  
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
