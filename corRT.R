# Twitter project, exploring data
# Oct 15, 2015
# Julian Wills 


# load packages  --------------------------------------------------------
require(magrittr) || {install.packages("magrittr"); require(magrittr)}
require(dplyr) || {install.packages("dplyr"); require(dplyr)}
require(reshape2) || {install.packages("reshape2"); require(reshape2)}


if (grepl("^C:/",getwd())) {
  userDir <- "C:/Users/Julian/GDrive" #PC
} else {
  userDir <- "/Users/julian/GDrive" #Mac
}

setwd(paste0(userDir,"/1 Twitter Project/pythonScripts/allTweets"))
dAll <- tbl_df(read.csv("joinedRTs.csv",header=T)) %>% rename(tid=tID,rtid=rtID)

# load in RT summary files
setwd("C:/Users/Julian/GDrive/1 Twitter Project/pythonScripts/")
dG <- tbl_df(read.csv(paste0(getwd(),"/","GunControl/Guns/split/filt/RT/","G_RT.csv"),header=T))
dM <- tbl_df(read.csv(paste0(getwd(),"/","GayMarriage/marriage/split/RT/","M_RT.csv"),header=T))
dC <- tbl_df(read.csv(paste0(getwd(),"/","ClimateChange/Climate/split/RT/","C_RT.csv"),header=T))
dAll <- rbind(dG,dM,dC)

# Heat maps ---------------------------------------------------------------

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

ideoHeatMap <- function(df) { 
  new.xy.me <- expand_data(df %>% filter(cond=="ME"),breaks=0.25) %>%  mutate(cond="ME")
  new.xy.nme <- expand_data(df %>% filter(cond=="NME"),breaks=0.25) %>% mutate(cond="NME")
  new.xy.mne <- expand_data(df %>% filter(cond=="MNE"),breaks=0.25) %>% mutate(cond="MNE")
  new.xy.nmne <- expand_data(df %>% filter(cond=="NMNE"),breaks=0.25) %>%  mutate(cond="NMNE")
  return (rbind(new.xy.me,new.xy.nme,new.xy.mne,new.xy.nmne))
}

# create heatmap
ggplot(ideoHeatMap(dAll %>% filter(topic=="C")), aes(x=y, y=x)) +
  geom_tile(aes(fill=prop), colour="white") +
  scale_fill_gradient(name="% of\ntweets", 
                      low = "white", high = "black", 
                      breaks=c(0, .0050, 0.010, 0.015, 0.02), limits=c(0, .021),
                      labels=c("0.0%", "0.5%", "1.0%", "1.5%", ">2%")) +
  labs(y="Estimated Ideology of Retweeter", x="Estimated Ideology of Author",
       title="Ideological Correlations in Gun Control 2013 Tweets") + 
  scale_y_continuous(expand=c(0,0), breaks=(-2:2), limits=c(-3, 3)) +
  scale_x_continuous(expand=c(0,0), breaks=(-2:2), limits=c(-3, 3)) +
  theme(panel.border=element_rect(fill=NA), panel.background = element_blank()) +
  coord_equal()  +
  facet_wrap(~ cond)


# Latest climate data -----------------------------------------------------

# create heatmap
ggplot(ideoHeatMap(dAll %>% filter(topic=="C")), aes(x=y, y=x)) +
  geom_tile(aes(fill=prop), colour="white") +
  scale_fill_gradient(name="% of\ntweets", 
                      low = "white", high = "black", 
                      breaks=c(0, .0050, 0.010, 0.015, 0.02), limits=c(0, .021),
                      labels=c("0.0%", "0.5%", "1.0%", "1.5%", ">2%")) +
  labs(y="Estimated Ideology of Retweeter", x="Estimated Ideology of Author",
       title="Ideological Correlations in Gun Control 2013 Tweets") + 
  scale_y_continuous(expand=c(0,0), breaks=(-2:2), limits=c(-3, 3)) +
  scale_x_continuous(expand=c(0,0), breaks=(-2:2), limits=c(-3, 3)) +
  theme(panel.border=element_rect(fill=NA), panel.background = element_blank()) +
  coord_equal()  +
  facet_wrap(~ cond)

# Incomplete data ---------------------------------------------------------
setwd("C:/Users/Julian/GDrive/1 Twitter Project/pythonScripts/")
dGM <- tbl_df(read.csv(paste0(getwd(),"/","GayMarriage/gayMarriageMoral/split/RT/","GM_RT.csv"),header=T))
dGC <- tbl_df(read.csv(paste0(getwd(),"/","GunControl/GC2013/filt/RT/","GC_RT.csv"),header=T))
dAll <- rbind(dGM,dGC)

# create heatmap
ggplot(ideoHeatMap(dAll %>% filter(M==1,topic=="GC")), aes(x=y, y=x)) +
  geom_tile(aes(fill=prop), colour="white") +
  scale_fill_gradient(name="% of\ntweets", 
                      low = "white", high = "black", 
                      breaks=c(0, .0050, 0.010, 0.015, 0.02), limits=c(0, .021),
                      labels=c("0.0%", "0.5%", "1.0%", "1.5%", ">2%")) +
  labs(y="Estimated Ideology of Retweeter", x="Estimated Ideology of Author",
       title="Ideological Correlations in Gun Control 2013 Tweets") + 
  scale_y_continuous(expand=c(0,0), breaks=(-2:2), limits=c(-3, 3)) +
  scale_x_continuous(expand=c(0,0), breaks=(-2:2), limits=c(-3, 3)) +
  theme(panel.border=element_rect(fill=NA), panel.background = element_blank()) +
  coord_equal()  +
  facet_grid(. ~ cond)

# create heatmap
ggplot(ideoHeatMap(dAll %>% filter(M==1,topic=="GM")), aes(x=y, y=x)) +
  geom_tile(aes(fill=prop), colour="white") +
  scale_fill_gradient(name="% of\ntweets", 
                      low = "white", high = "black", 
                      breaks=c(0, .0050, 0.010, 0.015, 0.02), limits=c(0, .021),
                      labels=c("0.0%", "0.5%", "1.0%", "1.5%", ">2%")) +
  labs(y="Estimated Ideology of Retweeter", x="Estimated Ideology of Author",
       title="Ideological Correlations in Gay Marriage Tweets") + 
  scale_y_continuous(expand=c(0,0), breaks=(-2:2), limits=c(-3, 3)) +
  scale_x_continuous(expand=c(0,0), breaks=(-2:2), limits=c(-3, 3)) +
  theme(panel.border=element_rect(fill=NA), panel.background = element_blank()) +
  coord_equal()  +
  facet_grid(. ~ cond)

dAll %>% distinct(topic) %>% select(topic)
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


# summary statistics ------------------------------------------------------
options(dplyr.print_min = 15)
dAll %>% group_by(topic,cond) %>% summarise(meanRT=mean(count),
                                            totalRT=sum(count),
                                            totalUniqRT=n(),
                                            propUniqRT=totalUniqRT/totalRT*100,
                                            viral=100-propUniqRT,
                                            corRTID=cor(tid,rtid),
                                            vartID=var(tid),
                                            varrtID=var(rtid))


# Scratchpad --------------------------------------------------------------

# to do: 
# examine simple effects of emotion for gun control 2013, gun control 2015, and gay marriage

# 2x2 trends for full climate set
dAll %>% group_by(topic,cond) %>% filter(topic=="C") %>% 
  summarise(meanRT=mean(count),
            totalRT=sum(count),
            totalUniqRT=n(),
            propUniqRT=totalUniqRT/totalRT*100,
            viral=100-propUniqRT,
            corRTID=cor(tid,rtid),
            vartID=var(tid),
            varrtID=var(rtid))

# simple effects of emotion for gun control 2013
setwd("C:/Users/Julian/GDrive/1 Twitter Project/pythonScripts/GunControl/GC2013/")
fs1 <- list.files(getwd(),".csv")
removeWord(fs1,"amendment") #uncomment to remove word
setwd("C:/Users/Julian/GDrive/1 Twitter Project/pythonScripts/GunControl/GC2013/filt/")
# convertRetweet(fs1,w=T) #uncomment to write .csv
dGC <- tbl_df(read.csv(paste0(getwd(),"/","RT/","GC_RT.csv"),header=T))
dGC %>% filter(M==1) %>% group_by(topic,E) %>% 
  summarise(meanRT=mean(count),
            totalRT=sum(count),
            totalUniqRT=n(),
            propUniqRT=totalUniqRT/totalRT*100,
            viral=100-propUniqRT,
            corRTID=cor(tid,rtid),
            vartID=var(tid),
            varrtID=var(rtid)) %>% arrange(desc(E))

# simple effects of emotion for gay marriage
setwd("C:/Users/Julian/GDrive/1 Twitter Project/pythonScripts/GayMarriage/split/")
fs1 <- list.files(getwd(),".csv")
# convertRetweet(fs1,w=T) #uncomment to write .csv
dGC <- tbl_df(read.csv(paste0(getwd(),"/","RT/","GC_RT.csv"),header=T))
dGC %>% filter(M==1) %>% group_by(topic,E) %>% 
  summarise(meanRT=mean(count),
            totalRT=sum(count),
            totalUniqRT=n(),
            propUniqRT=totalUniqRT/totalRT*100,
            viral=100-propUniqRT,
            corRTID=cor(tid,rtid),
            vartID=var(tid),
            varrtID=var(rtid)) %>% arrange(desc(E))






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
