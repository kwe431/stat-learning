#Project stuff
setwd("d:/UTSA/Fall2020/DataMining/Project/stat-learning") # set working directory
library(sqldf) #needed to run sql queries on dataframes
install.packages("tidyverse") #needed to deal with times
library(lubridate) #^

df = read.csv('rawraw.csv',na.strings = c("","NA")) # read in raw csv data

time_cols = c(5,6,7,12:87)

df.seconds = df[time_cols]

time_to_Sec <- function(x){
  sec = period_to_seconds(seconds(hms(x)))
  return(sec)
}


apply(df.seconds,MARGIN = 2,time_to_Sec)

df.seconds$Runner = df$Runner
df.seconds$Team = df$Team
df.seconds$Yards = df$Yards
df.seconds$Nationality = df$Nationality

teamdf = split(df.seconds, df.seconds$Team) # team specific df


temps = read_excel("Leaderboard - Yards 75 updated.xlsx",sheet = "temps") # read in additional temp data
sqldf('select * from df ')

