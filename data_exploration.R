#Project stuff
setwd("d:/UTSA/Fall2020/DataMining/Project/stat-learning") # set working directory
library(sqldf) #needed to run sql queries on dataframes
install.packages("tidyverse") #needed to deal with times
library(lubridate) #^
df = read.csv('rawraw.csv',na.strings = c("","NA")) # read in raw csv data
#temps = read_excel("Leaderboard - Yards 75 updated.xlsx",sheet = "temps") # read in additional temp data


#function to convert time to seconds
time_to_Sec <- function(x){
  sec = period_to_seconds(seconds(hms(x)))
  return(sec)
}

#apply function to convert hours to seconds for easier math
time_cols = c(5,6,7,8,12:86)
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

rate_builder <- function(dFrame){
  #number of yards passed in with the dataframe
  rate <- dFrame
  for(i in 2:ncol(dFrame)){
    rate[,i] <- (dFrame[i] - dFrame[i-1])/3600 
  }
  return(rate)
}

#df[,grepl("Yard.",names(df))]  this will give you all the columns that are "Yard..." Warning it does include Total Yards

rate = rate_builder(teamdf$Australia[,1:max(teamdf$Australia$Yards)+4])
rate = rate[-1] # get rid of first lap since there is nothing to compare it too
rate=data.matrix(rate)
dim(rate)

rate_length = length(rate)
rate_length/4

q1 = round(rate_length/4,digits = 0)
q2 = q1*2+1
q3 = q1 *3 +2
q4 = rate_length

#q = quantile(rate) don't use quantile because it sorts the data BAD!

sd(rate[1,2:10])
sd(rate[q1:q2])
sd(rate[q2:q3])
sd(rate[q3:q4])


#looking at this graph we can start to see a lot of spread right before they end. How do we quantify this and look for that pattern. 

a =df[,grepl("Yard.",names(df))] #condense to just yard columns for time analysis
a = a[,colSums(is.na(a))<nrow(a)] #drop rows that are all na
a=subset(a,select = -c(Yards)) #drop count of total yards
a.sec =data.frame(apply(a,MARGIN = 2,time_to_Sec)) #convert time to seconds
#a.sec$Yards = df$Yards
b = rate_builder(a.sec) #build rate dataframe 
b= b[-1] #get rid of yard 1
b$Yards = df$Yards
b[1,]


plot.new()
plot(1:74,b[1,1:74], xlab = "lap #", ylab = "rate diff in minutes", type = "l", col=randomColor(10))
points(74,b[1,74])
for(i in 2:3){
  points(1:74,b[i,1:74],type = "l", col=randomColor(10, luminosity = "random"))
  points(b$Yards[i]-1,b[i,b$Yards[i]-1])
  }



#sqldf('select * from teamdf where  ')


