#Project stuff
setwd("d:/UTSA/Fall2020/DataMining/Project/stat-learning") # set working directory
library(sqldf) #needed to run sql queries on dataframes
install.packages("tidyverse") #needed to deal with times
library(lubridate) #^
library(data.table)
df = read.csv('rawraw.csv',na.strings = c("","NA")) # read in raw csv data
temps = read_excel("Leaderboard - Yards 75 updated.xlsx",sheet = "temps") # read in additional temp data

dim(df)

time_cols = c(5,6,7,8,12:86)

#function to convert time to seconds
time_to_Sec <- function(x){
  sec = period_to_seconds(seconds(hms(x)))
  return(sec)
}

#apply function to convert hours to seconds for easier math
df.seconds =data.frame(apply(df[time_cols],MARGIN = 2,time_to_Sec))

#add back original data
df.seconds$Runner = df$Runner
df.seconds$Team = df$Team
df.seconds$Yards = df$Yards
df.seconds$Nationality = df$Nationality

teamdf = split(df.seconds, df.seconds$Team) # team specific df

#avg analysis
# look into when 


#Rate analysis
# we want to see rate changes from lap to lap. 
# each runner has a unique number of total yards. 
# x - yard transition i.e.: 1 -> difference from yard 1 to yard 2
# y - the actual difference in seconds
teamdf$Australia$Yard.2[1]-teamdf$Australia$Yard.1[1]
teamdf$Australia[1,1:teamdf$Australia$Yards+4] #need to add 4 for all the extra columns in the beginning

rate_builder <- function(dFrame){
  #number of yards passed in with the dataframe
  rate <- dFrame
  for(i in 2:ncol(dFrame)){
    rate[,i] <- (dFrame[i] - dFrame[i-1])/3600
  }
  return(rate)
}

rate = rate_builder(teamdf$Australia[1,1:teamdf$Australia$Yards+4])
rate = rate[-1] # get rid of first lap since we don't want that one
rate=data.matrix(rate)
length(rate)
plot(1:length(rate),rate)

#looking at this graph we can start to see a lot of spread right before they end. How do we quantify this and look for that pattern. 

#we need to build this for all the runners and teams. 

#sqldf('select * from teamdf where  ')


