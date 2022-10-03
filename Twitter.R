# Packages
library(rtweet)
library(lubridate)
library(twitteR)
library(tidyverse)
library(httr)
library(igraph)


#--------------------------------------
# Setting up Twitter
appname <- "Mark S Tenenbaum"
key <- "ftGT9zwDzvHiyvHaUaGtEbhjp"
secret <- "vLiv0JVthqkNgj46orvBF2Ie03JfhgfWwBy4gVWm3IyU4SaRc6"
access_token <- "1093956998247473154-eexZ830W7WXSZHLElO6t5CTShpbw3p"
access_secret <- "AfAaeCiinDAkYEPY1zVg9x17Am0MXSyEOns649gOfod3G" 


twitter_token <- create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret,
  access_token = access_token,
  access_secret = access_secret)
#---------------------------------------
  
  
  
  
  
# Beginning here I was getting error message [403] I am copying the code from the assignment to review later and try to resolve this. 
rstatstweets <- search_tweets("#rstats", n = 50, include_rts = FALSE)
rstatstweets
View(rstatstweets)


users_data(rstatstweets)

ts_plot(rstatstweets)

datatweets <- search_tweets("data", n = 250, retryonratelimit = FALSE)


UStweets <- search_tweets("lang:en", geocode = lookup_coords("usa"), n = 500)
head(UStweets)
tail(UStweets)
View(UStweets)


#Create lat/lng variables using all available tweet and profile geo-location data
UStweetlatlong <- lat_lng(UStweets)


#Plot state boundaries
par(mar = c(0, 0, 0, 0))
maps::map("state", lwd = .25)
#Plot lat and lng points onto state map
with(UStweetlatlong, points(lng, lat, pch = 20, cex = .75, col = rgb(0, .3, .7, .75)))


#Random sample for 30 seconds (default)
randomtweets <- stream_tweets("")
View(randomtweets)


#Stream tweets from London for 60 seconds
londontweets <- stream_tweets(lookup_coords("london, uk"), timeout = 60)




#The timeout argument determines how long the stream_tweet function will run. The following
#line would stream tweets for a week (timeout = 60 * 60 * 24 * 7) meaning, 60 secs x 60 mins *
 # 24 hours * 7 days). In this example, let's reduce that to just 60 seconds). Will require you to
#authenticate with a Google Maps API key.
View(londontweets)


# Now let's capture some tweets about former president Trump
stream_tweets(
"realdonaldtrump,trump",
timeout = 60,
file_name = "tweetsabouttrump.json",
parse = FALSE
)

# Read in the data as a tidy tbl data frame
djt <- parse_stream("tweetsabouttrump.json")
View(djt)

# Now let's try to capture some tweets about President Biden
stream_tweets(
"JoeBiden,biden",
timeout = 60,
file_name = "tweetsaboutbiden.json",
parse = FALSE
)


#Read in the data as a tidy tbl data frame
jb <- parse_stream("tweetsaboutbiden.json")
View(jb)


# Get user IDs of accounts followed by CNN
cnn_fds <- get_friends("cnn")
View(cnn_fds)


#Lookup data on those accounts, and try this code first (which was the old code). You will get an
#error message. Can you interpret and correct the error?
  cnn_fds_data <- lookup_users(cnn_fds$user_id)
View(cnn_fds_data)


# Get user IDs of accounts following CNN
cnn_flw <- get_followers("cnn", n = 50)
View(cnn_flw)

# Lookup data on those accounts
cnn_flw_data <- lookup_users(cnn_flw$user_id)
View(cnn_fds_data)

# How many total follows does cnn have?
  cnn <- lookup_users("cnn")
View(cnn)


#Let’s now get 300 tweets that the author JK Rowling has Favorited recently?
jkr <- get_favorites("jk_rowling", n = 300)
View(jkr)



#Search for users with #rstats in their profiles
usrs <- search_users("#rstats", n = 1000)
?get_trends
sf <- get_trends("san francisco")
View(sf)


# Lookup users by screen_name or user_id
users <- c("KimKardashian", "justinbieber", "taylorswift13",
           "espn", "JoelEmbiid", "cstonehoops", "KUHoops",
           "upshotnyt", "fivethirtyeight", "hadleywickham",
           "cnn", "foxnews", "msnbc", "maddow", "seanhannity",
           "potus", "epa", "hillaryclinton", "realdonaldtrump",
           "natesilver538", "ezraklein", "annecoulter")
famous_tweeters <- lookup_users(users)
View(famous_tweeters)

# Extract most recent tweets data from the famous tweeters
tweets_data(famous_tweeters)








# EXAMPLE FROM DAKOTA
### 2021 - 2019 Tweets: 3/17/2021 ###

NI_tweets <- get_timeline(c("duponline", "sinnfeinireland", "SDLPlive", "allianceparty", 
                            "uuponline", "naomi_long", "columeastwood",
                            "mikenesbittni", "RobinSwannMoH", "SteveAikenUUP", "ArleneFosterUK", 
                            "MaryLouMcDonald", "GerryAdamsSF", "moneillsf", "M_McGuinness_SF"), n = 20000)
rtwt_name_sort <- rtwt_sort <- arrange(rtwt, desc(retweet_count))
rtwt_unique <- unique(rtwt_sort, by = "text")
rownames(rtwt_unique) <- NULL
top_tweets <- head(rtwt_unique, 1000)

sapply(rtwt, class)

NI_tweets$fight_words<-ifelse(grepl("Terrorist|
asshole|scum|rat|parasite|crazy|nazi|
jerk|thug|idiot|disgusting|traitor|sleazy|enemy|
a joke|treasonous|thief|liar|corrupt|beaten up|shit on|
rouhghed up|eliminated|harassed|mocked|crushed|heckeled|
made to pay|thrown in jail|murder|
flogging|flog|beating|burn down|burn them|
burn it|set fire to|bash|castrate|shoot in the head|
ball tap|scrag|execute|bomb|violence|attack|rebel|battle|
                                    gun|terror|reprisal|abuse|
                                    repression|oppression|intimidate",NI_tweets$text),1,0)

NI_tweets$remain<-ifelse(grepl("Remain|pro-Remain|stay in the EU|remain in the EU",
                               NI_tweets$text),1,0)
NI_tweets$leave<-ifelse(grepl("Leave|pro-Leave|leave the EU|leave the EU", 
  NI_tweets$text),1,0)
NI_tweets$proUK<-ifelse(grepl("British unity|pro-England|united England|British nation", 
  NI_tweets$text),1,0)
NI_tweets$proIE<-ifelse(grepl("Irish unification|pro-Irish|united Ireland|Irish nation", 
                              NI_tweets$text),1,0)
NI_tweets$Brexit<-ifelse(grepl("Brexit",NI_tweets$text),1,0)
NI_tweets$Party <- ifelse(NI_tweets$screen_name=="duponline", "DUP", 
                          ifelse(NI_tweets$screen_name=="sinnfeinireland", "SF",
                                 ifelse(NI_tweets$screen_name=="SDLPlive", "SDLP",
                                        ifelse(NI_tweets$screen_name=="allianceparty", "APNI", 
                                               ifelse(NI_tweets$screen_name=="uuponline","UUP", "politician")))))
NI_tweets$party_pol <- ifelse(NI_tweets$screen_name=="M_McGuinness_SF", "SF", 
                              ifelse(NI_tweets$screen_name=="moneillsf", "SF",
                                     ifelse(NI_tweets$screen_name=="GerryAdamsSF", "SF",
                                            ifelse(NI_tweets$screen_name=="MaryLouMcDonald", "SF", 
                                                   ifelse(NI_tweets$screen_name=="ArleneFosterUK","DUP", 
                                                          ifelse(NI_tweets$screen_name=="SteveAikenUUP","UUP",
                                                                 ifelse(NI_tweets$screen_name=="RobinSwannMoH","UUP",
                                                                        ifelse(NI_tweets$screen_name=="mikenesbittni","UUP",
                                                                               ifelse(NI_tweets$screen_name=="columeastwood","SDLP", "APNI")))))))))


rtwt <- NI_tweets[,c("text", "retweet_count", "screen_name", "created_at", 
                     "is_retweet", "favorite_count", "Brexit", "Party", 
                     "fight_words", "proIE", "proUK", "remain", "leave", 
                     "party_pol")]
                     
                     
# ANOTHER DAKOTA EXAMPLE
  library(rtweet)
library(httpuv)
library(dplyr)
library(readxl)
library(ggplot2)
library(stringr)

### 2021 - 2019 Tweets: 3/17/2021 ###

LGBT_tweets <- get_timeline(c("RepSeanMaloney", "RepCicilline", "sharicedavids", "SenatorBaldwin", "MondaireJones", "SenatorSinema", "RitchieTorres", "RepMarkTakano", "RepAngieCraig", "repmarkpocan", "RepChrisPappas"), n = 18000)
rtwt <- LGBT_tweets[,c("text", "retweet_count", "screen_name", "created_at", "is_retweet", "favorite_count")]
rtwt_name_sort <- rtwt_sort <- arrange(rtwt, desc(retweet_count))
rtwt_unique <- unique(rtwt_sort, by = "text")
rownames(rtwt_unique) <- NULL
top_tweets <- head(rtwt_unique, 1000)

sapply(rtwt, class)
write.csv(rtwt, "C:/Users/dstrode/Documents/LGBT_tweets_V2.csv", row.names = FALSE)
Cat_LGBT <- read.csv("LGBT_tweets_V2.csv")

Cat_LGBT$LGBT <- ifelse(grepl("gay|sexual orientation|transgender|LGBTQ|LGBT|bisexual|gender identity|Equality Act", Cat_LGBT$text), 1, 0)

ggplot(Cat_LGBT, aes(x = screen_name, fill = LGBT, fill = screen_name)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90), legend.position = "none")

### 2021 - 2019 Tweets: 3/17/2021 HETERO ###

H_tweets <- get_timeline(c("RepMMM", "RepKatiePorter", "RepVanTaylor", "RepSwalwell", "RepMoBrooks", "RepLarryBucshon", "RepMarciaFudge", "RepFrenchHill", "jacobsny27", "Jim_Jordan", "RepHankJohnson"), n = 18000)
rtwt2 <- H_tweets[,c("text", "retweet_count", "screen_name", "created_at", "is_retweet", "favorite_count")]
rtwt_name_sort2 <- rtwt_sort2 <- arrange(rtwt2, desc(retweet_count))
rtwt_unique2 <- unique(rtwt_sort2, by = "text")
rownames(rtwt_unique2) <- NULL
top_tweets2 <- head(rtwt_unique2, 100)

write.csv(rtwt2, "C:/Users/dstrode/Documents/H_tweets_V2.csv", row.names = FALSE)
Cat_H2 <- read.csv("H_tweets_V2.csv")

Cat_H2$LGBT <- ifelse(grepl("gay|sexual orientation|transgender|LGBTQ|LGBT|bisexual|gender identity|Equality Act", Cat_H2$text), 1, 0)

full_tweets <- rbind(Cat_LGBT, Cat_H2)

full_tweets$SO <- ifelse(grepl("RepSeanMaloney|RepCicilline|sharicedavids|SenatorBaldwin|MondaireJones|SenatorSinema|RitchieTorres|RepMarkTakano|RepAngieCraig|repmarkpocan|RepChrisPappas", full_tweets$screen_name), 1, 0)
write.csv(full_tweets, "C:/Users/dstrode/Documents/full_tweets20192021.csv", row.names = FALSE)

fig1 <- tapply(full_tweets$LGBT, full_tweets$SO, sum)
barplot(fig1, names = c("Heterosexual", "LGBTQ"), col = c("light blue", "light green"), xlab = "Sexual Orientation", ylab = "LGBTQ Tweet Count", main = "Congressional Tweets by Sexual Orientation")
mtext("LGBTQ Tweets", side = 3, line = .5)

ggplot(full_tweets, aes(y = LGBT, fill = SO)) +
  geom_bar() +
  labs(title = "Congressional Tweets by Sexual Orientation", subtitle = "LGBTQ Tweets", x = "Sexual Orientation", y = "LGBTQ Tweet Count") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) 


full_tweets$PID_strat <- ifelse(grepl("Democrat|Republican|GOP|Democrats|Repubclians", full_tweets$text), 1, 0)

fig2 <- tapply(full_tweets$PID_strat, full_tweets$LGBT, sum)
barplot(fig2, names = c("Non-LGBTQ", "LGBTQ"), col = c("light blue", "light green"), xlab = "LGBTQ Tweets", ylab = "Party Strategy Tweet Count", main = "LGBTQ Tweets in Congress")
mtext("Party Strategy Tweets", side = 3, line = .5)
  
