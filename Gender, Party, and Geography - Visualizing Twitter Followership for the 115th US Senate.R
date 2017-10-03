#################
# CODE FOR INFORMATION VISUALIZATION - SUMMATIVE ASSIGNMENT
#################

## Set working directory:
getwd()
setwd('/Users/Xyz27/Dropbox/HILARY 2016-17/Information Visualization/InfoViz - Summative')
install.packages("ggplot2")
library(ggplot2)
library(xlsx)

##### IMPORT DATASETS:
## Senator Information ##
senators<-read.xlsx("twitter_foranalysis.xlsx", sheetIndex = 1)
# Remove "X" column:
names(senators)
senators$X<-NULL
senators$NA.<-NULL

## Followership Data ##
# I begin by loading each file into R, and then merging on the twitter handle variable present in each. 
followers<-read.xlsx("senator_tw_count.xlsx", sheetIndex=1)
senator_info<-read.xlsx("twitter_basicinfo.xlsx", sheetIndex=1)
df<-merge(senator_info, followers, by="twitter_handle")
#View(df)
names(df)

# Check out variables in the dataframe:
names(df)
df$X<-NULL
df$NA.<-NULL

## State Data ## 
state<-read.csv("state_pop.csv")
state_full<-read.csv("states.csv")
names(state_full)<-c("full_state", "abb")
names(state)<-c("full_state", "population")
state_df<-merge(state, state_full, by="full_state")
names(state_df)<-c("state_name", "population", "home_state")

# Merge population with df based on 'home_state' column
df$population.x<-NULL
df$population.y<-NULL
df$population<-NULL
df$state_name.x<-NULL
df$state_name.y<-NULL
df$state_name<-NULL
df2<-merge(df, state_df, by="home_state")
df2$population<-as.numeric(df2$population)
df<-df2
print(length(df2$home_state))

## New variable: Followership per state ('normalized followership')
df$nf<-df$follower_count/df$population

###### HORIZONTAL BARPLOT ######
partycolors <- c(republican="red", independent="grey", democrat="blue")
gendercolors<-c(female="#fa9fb5", male="#74a9cf")

length(df$party)
party_gender<-ggplot(df, aes(party, fill = gender, color=party)) +
  geom_histogram(stat="count", alpha=0.3, binwidth=10, size=1) + 
  coord_flip() + 
  scale_y_continuous(limits=c(0, 100)) + 
  #ggtitle("Getting to Know the US Senate: Party Breakdown by Gender") + 
  ylab("Total Senators") + 
  xlab("Party") +
  scale_color_manual(values=partycolors, guide=FALSE) + 
  scale_fill_manual(values=gendercolors) + 
  #stat_count(aes(y=..count.., label=..count..), size=6, geom="text", vjust=-.5, hjust=1.5)  + 
  guides(fill=guide_legend(title="Gender")) +
  theme(text=element_text(size=16, family="Verdana"))

# Plot:
plot_grid(party_gender)

##### HISTOGRAMS #####
## FULL FOLLOWERSHIP:
ggplot(df, aes(follower_count)) + 
  geom_histogram(data=df, fill="#433e90", bins=50) + guides(fill=FALSE) +
  #ggtitle("Senatorial Twitter Followership: ") + 
  xlab("Followership") + ylab("Count") + 
  theme(text=element_text(size=12, family="Verdana"))
## Test for normality:
shapiro.test(df$follower_count)
# Those with more than 2mn followers:
high<-df[df$follower_count>2000000,]
unique(high$twitter_handle)

### GENDER - NOT NORMALIZED
# Means for gender:
unique(df$gender)
# Women:
f<-df[df$gender=="female",]
range(f$follower_count)
mean(f$follower_count)
# Men:
m<-df[df$gender=="male",]
range(m$follower_count)
mean(m$follower_count)
# Mean lines:
vlines <- ddply(df, .(gender), summarize, meanc = mean(log(follower_count)), mean=round(exp(mean(log(follower_count))), 1))
# Histogram
gender<-ggplot(df, aes(log(follower_count))) + guides(fill=FALSE) + 
  geom_histogram(data=subset(df,gender=="female"), fill="pink", binwidth = 0.5) + 
  geom_histogram(data=subset(df,gender=="male"), fill="light blue", binwidth = 0.5) + 
  facet_grid(~gender,as.table=T) +
  geom_vline(data=vlines, aes(xintercept=meanc), linetype="dashed", colour="grey") + 
  geom_text(data=vlines, aes(x=meanc, label=mean), y=20, hjust=0, size=4) + 
  ggtitle("Senatorial Twitter Followership, by Party (Log)") + 
  xlab("Followership (Log)") + ylab("Count") +
  theme(text=element_text(size=12, family="Verdana"))

## PARTY - NOT NORMALIZED
# The graph below takes the mean of the log follower counts by gender, and adds it to a histogram for each gender:
## Means for party:
rep<-df[df$party=="republican",]
mean(rep$follower_count)
dem<-df[df$party=="democrat",]
mean(dem$follower_count)
ind<-df[df$party=="independent",]
View(ind)
mean(ind$follower_count)
# Mean lines:
vlines <- ddply(df, .(party), summarize, meanc = mean(log(follower_count)), mean=round(exp(mean(log(follower_count))), 0))
# Histogram:
party<-ggplot(df, aes(log(follower_count))) + guides(fill=FALSE) + 
  geom_histogram(data=subset(df,party=="democrat"), fill="blue", binwidth = 0.3) +
  geom_histogram(data=subset(df,party=="republican"), fill="red", binwidth=0.3) +
  geom_histogram(data=subset(df,party=="independent"), fill="grey", binwidth=0.3) + facet_grid(~ party,as.table=T) +
  geom_vline(data=vlines, aes(xintercept=meanc), linetype="dashed", colour="grey") +   
  geom_text(data=vlines, aes(x=meanc, label=mean), y=10, hjust=0, size=4) + 
  #geom_vline(data=vlines, aes(xintercept=meanc), linetype="dashed", colour="grey") +
  ggtitle("Senatorial Twitter Followership, by Party (Log)") + 
  xlab("Followership (Log)") + ylab("Count") +
  theme(text=element_text(size=12, family="Verdana"))
  #geom_text(aes(x=mean(df$follower_count), y=10),label="Median",hjust=1, size=9)
  
### GENDER - NORMALIZED
# Mean lines:
vlines <- ddply(df, .(gender), summarize, meanc = mean((log(nf))), mean=round(exp(mean(log(nf))), 6))
# Histogram:
gender_norm<-ggplot(df, aes(log(nf))) + guides(fill=FALSE) + 
    geom_histogram(data=subset(df,gender=="female"), fill="pink", binwidth = 0.5) + 
    geom_histogram(data=subset(df,gender=="male"), fill="light blue", binwidth = 0.5) + 
    facet_grid(~gender,as.table=T) +
    geom_vline(data=vlines, aes(xintercept=meanc), linetype="dashed", colour="grey") +   
   # geom_text(data=vlines, aes(x=meanc, label=mean), y=15, hjust=0, size=4) + 
    #ggtitle("Population-Normalized Senatorial Twitter Followership (Log): Gender View") + 
    xlab("Followership (Log, as of March 2017)") + ylab("Count") +
    theme(text=element_text(size=16, family="Verdana"), axis.text.x = element_text(angle=90, hjust=1))

## PARTY - NORMALIZED
# Mean lines:
vlines <- ddply(df, .(party), summarize, meanc = mean(log(nf)), mean=round(exp(mean(log(nf))), 4))
# Histogram:
party_norm<-ggplot(df, aes(log(nf))) + guides(fill=FALSE) + 
    geom_histogram(data=subset(df,party=="democrat"), fill="blue", binwidth = 0.3, size=3) +
    geom_histogram(data=subset(df,party=="republican"), fill="red", binwidth=0.3) +
    geom_histogram(data=subset(df,party=="independent"), fill="grey", binwidth=0.3) + facet_grid(~ party,as.table=T) +
    geom_vline(data=vlines, aes(xintercept=meanc), linetype="dashed", colour="grey") + 
    geom_vline(data=vlines, aes(xintercept=meanc), linetype="dashed", colour="grey") + 
    #geom_text(data=vlines, aes(x=meanc, label=mean), y=15, hjust=0, size=4) + 
    #ggtitle("Party View)") + 
    xlab("Followership (Log, as of March 2017)") + ylab("Count") + ylim(0,20) +
    theme(text=element_text(size=16, family="Verdana"), axis.text.x = element_text(angle=90, hjust=1))#+
    #geom_text(aes(x=mean(df$follower_count), y=10),label="Median",hjust=1, size=9)
  
## Plot together:
gender_norm
party_norm
plot_grid(gender_norm, party_norm)

### DEMOGRAPHIC INFO ABOUT STATE
# The relevant information regarding demographic and political variables for this analysis is what share of the state's senators
# are female versus male, and from each party. To include this information in the analysis, I calculate the following variables:
# share of senators from state that are female (either 0, 0.5, or 1)
# share of senators from state that are Republican
# share of senators from state that are Democrat
# share of senators from state that are Independent

## GENDER - share of senators from the state that are female
variable = "gender"
df_short<-df[,c(variable, "twitter_handle", "home_state")]
View(df_short)
# Counts for gender:
g<-reshape(df_short, direction="wide", idvar="home_state", timevar=c("gender"))
View(g)
g$male<-ifelse(is.na(g$twitter_handle.female), 2, ifelse(!is.na(g$twitter_handle.male), 1, 0)) 
g$female<-ifelse(g$male==0,2,ifelse(g$male==1, 1, 0))
g$male_share<-g$male/2
g$female_share<-g$female/2

## PARTY - share of senators from the state that are a) Rep, b) Dem, c) Ind
variable = "party"
df_short<-df[,c(variable, "twitter_handle", "home_state")]
p<-reshape(df_short, direction="wide", idvar="home_state", timevar=c("party"))
View(p)
# Totals for each party:
p$dem<-ifelse(!is.na(p$twitter_handle.democrat) & is.na(p$twitter_handle.republican) & is.na(p$twitter_handle.independent), 2,ifelse(!is.na(p$twitter_handle.democrat) & (!is.na(p$twitter_handle.republican) | !is.na(p$twitter_handle.independent)), 1, 0))
p$rep<-ifelse(!is.na(p$twitter_handle.republican) & is.na(p$twitter_handle.democrat) & is.na(p$twitter_handle.independent), 2,ifelse(!is.na(p$twitter_handle.republican) & (!is.na(p$twitter_handle.democrat) | !is.na(p$twitter_handle.independent)), 1, 0))
p$ind<-2-p$rep-p$dem
# Calculate share of total senators from each state:
p$dem_share<-p$dem/2
p$rep_share<-p$rep/2
p$ind_share<-p$ind/2

# Remove extra columns:
#p$twitter_handle.democrat<-NULL
#p$twitter_handle.republican<-NULL
#p$twitter_handle.independent<-NULL

#### MERGE THE SHARE INFORMATION DATASETs:
# Both datasets have the home_state column, so I merge on this value:
m<-merge(g, p, by="home_state")
m<-m[,c("home_state", "male_share", "female_share", "dem_share", "rep_share", "ind_share")]
names(m)<-c("region", "male_share", "female_share", "dem_share", "rep_share", "ind_share")
# Load states info:
states<-read.csv("states.csv")
levels(states$name) <- tolower(levels(states$name))
states<-rename(states, c("abb"="region"))
# Merge 'm' with state name info:
cat_data<-merge(m, states, by="region")

## Make full dataset, with a) nfm, b) party/gender share info, c) long/lat:
## Load state_coord data:
state_coord<-read.csv("state_coord.csv")
state_coord$name<-tolower(state_coord$name)
head(state_coord)
head(cat_data)
cd2<-merge(cat_data, state_coord, by="name")
View(cd2)

##### Add population information
## Create a data frame that has the total followers per state
# Reduce df to the necessary columsn:
cols<-df[,c("home_state", "follower_count")]
cols<-rename(cols, c("home_state"="region", "follower_count"="value"))
sum <- ddply(cols,~region,summarise,sum=sum(value))
sum<-rename(sum, c("sum"="value"))
# For renaming purposes:
states<-read.csv("states.csv")
levels(states$name) <- tolower(levels(states$name))
states<-rename(states, c("abb"="region"))
# File for analysis:
merge<-merge(sum, states, by="region")
merge$region<-NULL
merge<-rename(merge, c("name"="region"))
#View(merge)

# Add population information
# Merge w/ population info:
state_df<-state_df
state_df$region<- tolower(state_df$state_name)
#View(state_df)

# Merge with the followership data (merge) with the population data (state_df)
norm_df<-merge(merge, state_df, by="region")
#View(norm_df)
# Create the variable: TOTAL FOLLOWERSHIP BY STATE, NORMALIZED BY POPULATION (nfm)
norm_df$nfm<-norm_df$value/norm_df$population
range(norm_df$nfm)
mean(norm_df$nfm)
names(norm_df)<-c("name", "followership", "full_state_name", "population", "abb", "nfm")
#View(norm_df)
full<-merge(cd2, norm_df, by="name")
names(full)
full$full_state_name<-NULL
full$region<-NULL
View(full)

## COLORPLETH ##
# Load necessary packages:
install.packages("maptools", dependencies=TRUE)
library(maptools)
install.packages("GISTools")
install.packages("classInt")
library(GISTools)
library(classInt)
install.packages("devtools")
library(devtools)
install.packages("choroplethrMaps")
library(choroplethr)
library(choroplethrMaps)
install.packages("data.table")        # install it
install.packages("choroplethr", dependencies = TRUE)

## CHOLOROPLETH: NORMALIZED BY POPULATION:
## Now that we have state information, we can simply merge the population data with this dataset and replot the map.
# CHOLOROPLETH: with 8 bins
# Method 1:
#full_ch<-full[,c("name", "nfm")]
#names(full_ch)<-c("region", "value")
#state_choropleth(full_ch, 
#                 title="Senatorial Followership, Normalized by State Population (as of March, 2017)", 
#                 legend="Follower Counts (% State Population)", 
#                 num_colors = 8) #+ symbols(full$long, full$lat, circles=full$rep_share, add=TRUE, inches = 0.15, bg="#93ceef", fg="#ffffff")


# Method 2:
choro = StateChoropleth$new(full_ch)
choro
#choro$title = "Senatorial Followership, Normalized by State Population (as of March, 2017)"
choro$ggplot_scale = scale_fill_manual(name="Followership (% State Population)", values=c("#9efdff", "#50fcff", "#0ffbff", "#00e2e7", "#00ccd0",  "#00b2b6",  "#007c7f"), drop=FALSE)
choro$legend=FALSE
choro$render()

# Examine specific levels:
sub<-subset(full_ch, full_ch$value < 7)
print(length(unique(sub$region)))
print(unique(sub$region))
sub2<-subset(full_ch, full_ch$value >= 0.20)
mean(sub$value)
graphics.off()




