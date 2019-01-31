install.packages("dplyr")              # filtering out specific rows from data frame
install.packages("ggplot2")
install.packages("data.table")
install.packages("corrplot")
install.packages("arules")
install.packages("arulesViz")
install.packages("tidyverse")
install.packages("kernlab")

library(dplyr)
library(ggplot2)
library(data.table)
library(corrplot)
library(rpart)
library(randomForest)
library(arules)
library(arulesViz)
library(tidyverse)
library(kernlab)

# Start of Data cleaning ------------------------------------------------------------------------

filename <- read.csv("Satisfaction Survey.csv")
df.sample <- data.frame(filename, stringsAsFactors = FALSE)           #Do not need this line because 
row.names(df.sample) <- NULL
nrow(df.sample)

str(df.sample)                        # Looking at structure of dataset to find data type of columns
summary(df.sample)                    
# Looking at summary of dataset and observed below issues
# Column names to be changed
# last few columns have blank or NA values they need correction

# 1) Data cleaning
# changing column names of dataset to remove dots
colnames(df.sample) <- c("Satisfaction","AirlineStatus","Age","Gender","PriceSensitivity","YearofFirstFlight", "NoofFlightspa","percentofFlightwithotherAirlines","TypeofTravel","No.ofotherLoyaltyCards","ShoppingAmountatAirport","EatingandDrinkingatAirport", "Class", "DayofMonth","Flightdate","AirlineCode","AirlineName","OrginCity","OriginState","DestinationCity","DestinationState","ScheduledDepartureHour","DepartureDelayinMinutes","ArrivalDelayinMinutes","Flightcancelled","Flighttimeinminutes","FlightDistance","ArrivalDelaygreater5Mins")

# Column satisfaction is having 3 unnecessary values. Finding indexes and deleting these rows from dataset
unique(df.sample$Satisfaction)                # Finding these values (4.00.5 , 4.00.2.00,  4.00.2.00)
x <- grep("*.00.*", df.sample$Satisfaction)   # Finding their row number in dataset by grep (pattern matching)
df.sample <- df.sample[-x,]                       # deleting row numbers (38898 38899 38900)
row.names(df.sample) <- NULL                             # renumbering the rows
y <- grep("*.5", df.sample$Satisfaction)
df.sample <- df.sample[-y,]                       # deleting row numbers (1 3 7 52712 52714 52718)
nrow(df.sample)

# function to correct and convert columns to string and remove whitespaces
correcttostring <- function(col)
{
  col <- as.character(col)
  col <- trimws (col, which = c("both"))
  col <- as.factor(col)
  return (col)
}

# changing date format to a standard format
df.sample$Flightdate <- gsub("/","-",df.sample$Flightdate)
df.sample$Flightdate <- as.Date(df.sample$Flightdate,tryFormats = c("%m-%d-%y", "%m/%d/%y"))

#converting columns to character
df.sample$AirlineStatus <- correcttostring(df.sample$AirlineStatus)
df.sample$Gender <- correcttostring(df.sample$Gender)
df.sample$TypeofTravel <- correcttostring(df.sample$TypeofTravel)
df.sample$Class <- correcttostring(df.sample$Class)
df.sample$AirlineCode <- correcttostring(df.sample$AirlineCode)
df.sample$AirlineName <- correcttostring(df.sample$AirlineName)
df.sample$OrginCity <- correcttostring(df.sample$OrginCity)
df.sample$OriginState <- correcttostring(df.sample$OriginState)
df.sample$DestinationCity <- correcttostring(df.sample$DestinationCity)
df.sample$DestinationState <- correcttostring(df.sample$DestinationState)
df.sample$ArrivalDelaygreater5Mins <- correcttostring(df.sample$ArrivalDelaygreater5Mins)
df.sample$Flightcancelled <- correcttostring(df.sample$Flightcancelled)

str(df.sample)
#df$Genderlabel <- factor(df$Gender,labels = c(0,1))

# converting columns to numeric
df.sample$Satisfaction <- as.numeric(as.character(df.sample$Satisfaction))      # when you use as.numeric on a factor then you get the underlying integers
df.sample$Age <- as.numeric(df.sample$Age)
df.sample$PriceSensitivity <- as.numeric(df.sample$PriceSensitivity)
df.sample$YearofFirstFlight <- as.numeric(df.sample$YearofFirstFlight)
df.sample$NoofFlightspa<-as.numeric(df.sample$NoofFlightspa)
df.sample$percentofFlightwithotherAirlines <- as.numeric(df.sample$percentofFlightwithotherAirlines)
df.sample$No.ofotherLoyaltyCards <- as.numeric(df.sample$No.ofotherLoyaltyCards)
df.sample$ShoppingAmountatAirport <- as.numeric(df.sample$ShoppingAmountatAirport)
df.sample$EatingandDrinkingatAirport <- as.numeric(df.sample$EatingandDrinkingatAirport)
df.sample$ScheduledDepartureHour <- as.numeric(df.sample$ScheduledDepartureHour)
df.sample$FlightDistance <- as.numeric(df.sample$FlightDistance)
df.sample$DepartureDelayinMinutes <- as.numeric(df.sample$DepartureDelayinMinutes)
df.sample$ArrivalDelayinMinutes <- as.numeric(df.sample$ArrivalDelayinMinutes)
df.sample$Flighttimeinminutes <- as.numeric(df.sample$Flighttimeinminutes)
df.sample$DayofMonth <- as.numeric(df.sample$DayofMonth)

df.m <- df.sample[,c(-16,-18,-20,-27)]
# Deleted Day of Month,Airline Code,Orgin City,Destination City,Flight Distance

# replacing blank values in column (ArrivalDelayinMinutes,Flighttimeinminutes) when (flightcancelled=No) with mean of column 

df.fcyes <- filter(df.m,Flightcancelled=="Yes")
df.fcno <- filter(df.m,Flightcancelled=="No")
df.fcno$ArrivalDelayinMinutes[is.na(df.fcno$ArrivalDelayinMinutes)] <- floor(mean(df.fcno$ArrivalDelayinMinutes , na.rm = TRUE))
df.fcno$Flighttimeinminutes[is.na(df.fcno$Flighttimeinminutes)] <- floor(mean(df.fcno$Flighttimeinminutes , na.rm = TRUE))

df.cleaned <- rbind(df.fcno,df.fcyes)
df <- df.cleaned
df.model <- na.omit(df)
str(df.model)
summary(df.model)

# Summazing data airline wise with average mean and median satisfaction
df.airline <- group_by(df.model,AirlineName)
z <- summarize(df.airline, count = n(), meansatisfaction = mean(Satisfaction), mediansatisfaction = median(Satisfaction))

# Preparation of dataset of OnlyJets airline 
#unique(df.model$AirlineName)
df.onlyjet <- filter(df.model, AirlineName == "OnlyJets Airlines Inc.")

# Preparation of dataset of GoingNorth airline
df.GoingNorth <- filter(df.model, AirlineName == "GoingNorth Airlines Inc.")

#df.onlyjet.age <- group_by(df.onlyjet,Gender)
#z <- summarize(df.onlyjet.age, count = n(), meansat = mean(Satisfaction), median = median(Satisfaction))

# close of Data cleaning -----------------------------------------------------------------------------

# Start of circular boxplot Visualisation codes ------------------------------------------------------------------------

# Creating dataset for circular boxplot

df.Airline <- group_by(df.model,AirlineName)
z <- summarize(df.Airline, mediansatisfaction = median(Satisfaction), count = n())
#b <- z[order(-z$count),]
b <- z
b$AirlineName<- as.character(b$AirlineName)
b$AirlineName[b$AirlineName == "Cheapseats Airlines Inc."] <- c("Cheapseat")
b$AirlineName[b$AirlineName == "Northwest Business Airlines Inc."] <- c("Northwest Business")
b$AirlineName<- as.factor(b$AirlineName)

#unique(b$AirlineName)

data = data.frame(id=seq(1,14), individual = paste(b$AirlineName, sep=""), value= b$count)

# ----- This section prepare a dataframe for labels ---- #
# Get the name and the y position of each label
label_data=data

# calculate the ANGLE of the labels
number_of_bar=nrow(label_data)
angle= 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)

# calculate the alignment of labels: right or left
# If I am on the left part of the plot, my labels have currently an angle < -90
label_data$hjust<-ifelse( angle < -90, 1, 0)

# flip angle BY to make them readable
label_data$angle<-ifelse(angle < -90, angle+180, angle)
# ----- ------------------------------------------- ---- #

# Start the plot
p = ggplot(data, aes(x=as.factor(id), y=value)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  
  # This add the bars with a blue color
geom_bar(stat="identity", aes(fill= b$mediansatisfaction))+ guides(fill = guide_legend(title = "Satisfaction"))+    #alpha("skyblue", 0.7)) +
             
# Limits of the plot = very important. The negative value controls the size of the inner circle, the positive one is useful to add size over each bar
ylim(-10000,30000) +
             
# Custom the theme: no axis title and no cartesian grid
theme_minimal() +
theme(
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(rep(-1,4), "cm")      # Adjust the margin to make in sort labels are not truncated!
) +
             
# This makes the coordinate polar instead of cartesian.
coord_polar(start = 0) +
             
# Add the labels, using the label_data dataframe that we have created before
geom_text(data=label_data, aes(x=id, y=value+10, label=individual, hjust=hjust), color="black", fontface="bold",alpha=1, size=3.5, angle= label_data$angle, inherit.aes = FALSE )
p
           
# End of circular boxplot Visualisation codes ---------------------------------------------------------------------

# Start of Correlation Matrix ------------------------------------------------------------------

corr <- df.model %>% sapply(., as.numeric) %>% as.data.table()
corr <- cor(corr, use = 'pairwise.complete.obs')
corr[upper.tri(corr)] <- NA
corr <- melt(corr, na.rm = T) %>% as.data.table() %>% setorder(-value)
corr$text <- ifelse(abs(corr$value) >= .8 & corr$value != 1, round(corr$value, 2), '')

ggplot(data = corr, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = 'white') +
  geom_text(aes(label = text)) +
  scale_fill_gradient2(low = 'blue', high = 'red', mid = 'white',
                       midpoint = 0, limit = c(-1, 1),
                       name = 'Pearson Correlation') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = 'Correlation Matrix')


#End of Correlation Matrix ---------------------------------------------------------------------

# Start of Linear Modelling  -------------------------------------------------------------------

#Apply Linear model to entire dataset

lm.overalldata <- lm(formula=Satisfaction ~ AirlineStatus + Gender+ Age + PriceSensitivity + NoofFlightspa+ TypeofTravel+ ShoppingAmountatAirport + ArrivalDelaygreater5Mins,data = df.model)
summary(lm.overalldata)   # Multiple R-squared:  0.4456

plot(lm.overalldata)


ggplot(df.model, aes(x = AirlineStatus + Gender+ Age + PriceSensitivity + NoofFlightspa+ TypeofTravel+ ShoppingAmountatAirport + ArrivalDelaygreater5Mins, y = Satisfaction)) + geom_point() + stat_smooth(method = "lm", col = "red")

# Apply linear model to onlyjet
lm.onlyjet <- lm(formula=Satisfaction ~ AirlineStatus + Gender+ Age + PriceSensitivity + NoofFlightspa+ TypeofTravel+ ShoppingAmountatAirport
                 + ArrivalDelaygreater5Mins ,data = df.onlyjet)
summary(lm.onlyjet)                 #Multiple R-squared:  0.462


# Apply linear model to GoingNorth
lm.GoingNorth <- lm(formula=Satisfaction ~ AirlineStatus + Gender+ Age + PriceSensitivity + NoofFlightspa+ TypeofTravel+ ShoppingAmountatAirport
                 + ArrivalDelaygreater5Mins ,data = df.GoingNorth)
summary(lm.GoingNorth)                 #Multiple R-squared:  0.4586

# close of Linear Modelling --------------------------------------------------------------------

# start of Association Rule --------------------------------------------------------------------------

unique(df.model$AirlineStatus)
unique(df.model$Gender)
unique(df.model$TypeofTravel)
unique(df.model$Class)
unique(df.model$ArrivalDelaygreater5Mins)

createBuckets <-function(vec)
{
  q <- quantile(vec, c(0.4, 0.6))
  vBuckets <- replicate(length(vec), "Average")
  vBuckets[vec <= q[1]] <- "Low"
  vBuckets[vec > q[2]] <- "High"
  return (vBuckets)
}

summary(df.model$Satisfaction)
df.model$satis_ <- replicate(length(df.model$Satisfaction), "Average") 
df.model$satis_[df.model$Satisfaction >= 4] <- "High" #values higher than 7 are termed as high
df.model$satis_[df.model$Satisfaction <= 2] <- "Low"
unique(df.model$satis_)

hist(df.model$Age)
unique(df.model$Age)
median(df.model$Age)
df.model$age_ <- replicate(length(df.model$Age), "Medium")         # 30 to 60 as medium
df.model$age_[df.model$Age >= 15 & df.model$Age <= 30] <- "Young"  #15  to 30 as Young
df.model$age_[df.model$Age > 60 & df.model$Age <= 85] <- "Aged"   # 60 to 85 as Aged
#unique(df.model$Age[df.model$age_=="Aged"])

summary(df.model$PriceSensitivity)
hist(df.model$PriceSensitivity)
unique(df.model$PriceSensitivity) ## 0 to5
df.model$PriceSensitivity_<-createBuckets(df.model$PriceSensitivity)

hist(df.model$NoofFlightspa)
unique(df.model$NoofFlightspa)
median(df.model$NoofFlightspa)
df.model$NoofFlightspa_ <-createBuckets(df.model$NoofFlightspa)

unique(df.model$ShoppingAmountatAirport) ##
summary(df.model$ShoppingAmountatAirport)
hist(df.model$ShoppingAmountatAirport)
df.model$ShoppingAmountatAirport_<-createBuckets(df.model$ShoppingAmountatAirport)

ruleDF <- data.frame(df.model$satis_,df.model$AirlineStatus, df.model$Gender,df.model$age_, df.model$PriceSensitivity_, df.model$NoofFlightspa_,df.model$TypeofTravel,df.model$ArrivalDelaygreater5Mins, df.model$ShoppingAmountatAirport_)
colnames(ruleDF) <- c("Satisfaction","AirlineStatus","Gender","Age","PriceSensitivity","NoofFlightspa","TypeofTravel","ArrivalDelaygreater5Mins","ShoppingAmountatAirport")

summary(ruleDF)
View(ruleDF)
nrow(ruleDF)

aRuleDF <- as(ruleDF,"transactions")

ruleset.high <- apriori(aRuleDF, parameter = list(support=0.01, confidence = 0.5), appearance = list(default="lhs",rhs=("Satisfaction=High")))
summary(ruleset.high)
inspect(ruleset.high)

r1 <- sort(ruleset.high, decreasing =TRUE, by = "lift")
b <- head(a, n= 5)
inspect(head(b[1:2]))
#inspect(b)
plot(b[1:2], method="graph", control=list(type="items"))

plot(b[1:2], method="paracoord", control=list())

ruleset.low <- apriori(aRuleDF, parameter = list(support=0.01, confidence = 0.5), appearance = list(default="lhs",rhs=("Satisfaction=Low")))

r2 <- sort(ruleset.low, decreasing =TRUE, by = "lift")
b <- head(r2, n= 10)
inspect(b[c(1,4)])
#inspect(b)

plot(b[c(1,4)], method="graph", control=list(type="items"))

plot(b[c(1,4)], method="paracoord", control=list())

# Association rule ----------------------------------------------------------------------------

# Applying Association rules to onlyjet

createBuckets <-function(vec)
{
  q <- quantile(vec, c(0.4, 0.6))
  vBuckets <- replicate(length(vec), "Average")
  vBuckets[vec <= q[1]] <- "Low"
  vBuckets[vec > q[2]] <- "High"
  return (vBuckets)
}

summary(df.onlyjet$Satisfaction)
df.onlyjet$satis_ <- replicate(length(df.onlyjet$Satisfaction), "Average") 
df.onlyjet$satis_[df.onlyjet$Satisfaction >= 4] <- "High" #values higher than 7 are termed as high
df.onlyjet$satis_[df.onlyjet$Satisfaction <= 2] <- "Low"
unique(df.onlyjet$satis_)

hist(df.onlyjet$Age)
unique(df.onlyjet$Age)
median(df.onlyjet$Age)
df.onlyjet$age_ <- replicate(length(df.onlyjet$Age), "Medium")         # 30 to 60 as medium
df.onlyjet$age_[df.onlyjet$Age >= 15 & df.onlyjet$Age <= 30] <- "Young"  #15  to 30 as Young
df.onlyjet$age_[df.onlyjet$Age > 60 & df.onlyjet$Age <= 85] <- "Aged"   # 60 to 85 as Aged
#unique(df.onlyjet$Age[df.onlyjet$age_=="Aged"])

summary(df.onlyjet$PriceSensitivity)
hist(df.onlyjet$PriceSensitivity)
unique(df.onlyjet$PriceSensitivity) ## 0 to5
df.onlyjet$PriceSensitivity_<-createBuckets(df.onlyjet$PriceSensitivity)

hist(df.onlyjet$NoofFlightspa)
unique(df.onlyjet$NoofFlightspa) ##
median(df.onlyjet$NoofFlightspa)
df.onlyjet$NoofFlightspa_ <-createBuckets(df.onlyjet$NoofFlightspa)

unique(df.onlyjet$ShoppingAmountatAirport) ##
summary(df.onlyjet$ShoppingAmountatAirport)
hist(df.onlyjet$ShoppingAmountatAirport)
df.onlyjet$ShoppingAmountatAirport_<-createBuckets(df.onlyjet$ShoppingAmountatAirport)

ruleDF <- data.frame(df.onlyjet$satis_,df.onlyjet$AirlineStatus, df.onlyjet$Gender,df.onlyjet$age_, df.onlyjet$PriceSensitivity_, df.onlyjet$NoofFlightspa_,df.onlyjet$TypeofTravel,df.onlyjet$ArrivalDelaygreater5Mins, df.onlyjet$ShoppingAmountatAirport_)
colnames(ruleDF) <- c("Satisfaction","AirlineStatus","Gender","Age","PriceSensitivity","NoofFlightspa","TypeofTravel","ArrivalDelaygreater5Mins","ShoppingAmountatAirport")

aRuleDF <- as(ruleDF,"transactions")

ruleset.high <- apriori(aRuleDF, parameter = list(support=0.01, confidence = 0.5), appearance = list(default="lhs",rhs=("Satisfaction=High")))

r1.high <- sort(ruleset.high, decreasing =TRUE, by = "lift")
r1.high.head <- head(r1.high, n= 10)
inspect(head(r1.high.head[c(4,6)]))

inspect(r1.high.head[c(4,6)])

#plot(r1.high.head)
plot(r1.high.head[c(4,6)], method="graph", control=list(type="items"))
plot(r1.high.head[c(4,6)], method="paracoord", control=list(reorder = TRUE))


ruleset.low <- apriori(aRuleDF, parameter = list(support=0.01, confidence = 0.5), appearance = list(default="lhs",rhs=("Satisfaction=Low")))
r2.low <- sort(ruleset.low, decreasing =TRUE, by = "lift")
r2.low.head <- head(r2.low, n = 10)
inspect(r2.low.head[c(4,7)])

#plot(r1.high.head)
plot(r2.low.head[c(4,7)], method="graph", control=list(type="items"))
plot(r2.low.head[c(4,7)], method="paracoord", control=list(reorder = TRUE))


# Applying Association rules to GoingNorth

createBuckets <-function(vec)
{
  q <- quantile(vec, c(0.4, 0.6))
  vBuckets <- replicate(length(vec), "Average")
  vBuckets[vec <= q[1]] <- "Low"
  vBuckets[vec > q[2]] <- "High"
  return (vBuckets)
}

summary(df.GoingNorth$Satisfaction)
df.GoingNorth$satis_ <- replicate(length(df.GoingNorth$Satisfaction), "Average") 
df.GoingNorth$satis_[df.GoingNorth$Satisfaction >= 4] <- "High" #values higher than 7 are termed as high
df.GoingNorth$satis_[df.GoingNorth$Satisfaction <= 2] <- "Low"
unique(df.GoingNorth$satis_)

hist(df.GoingNorth$Age)
unique(df.GoingNorth$Age)
median(df.GoingNorth$Age)
df.GoingNorth$age_ <- replicate(length(df.GoingNorth$Age), "Medium")         # 30 to 60 as medium
df.GoingNorth$age_[df.GoingNorth$Age >= 15 & df.GoingNorth$Age <= 30] <- "Young"  #15  to 30 as Young
df.GoingNorth$age_[df.GoingNorth$Age > 60 & df.GoingNorth$Age <= 85] <- "Aged"   # 60 to 85 as Aged

summary(df.GoingNorth$PriceSensitivity)
hist(df.GoingNorth$PriceSensitivity)
unique(df.GoingNorth$PriceSensitivity) ## 0 to5
df.GoingNorth$PriceSensitivity_<-createBuckets(df.GoingNorth$PriceSensitivity)

hist(df.GoingNorth$NoofFlightspa)
unique(df.GoingNorth$NoofFlightspa) ##
median(df.GoingNorth$NoofFlightspa)
df.GoingNorth$NoofFlightspa_ <-createBuckets(df.GoingNorth$NoofFlightspa)

unique(df.GoingNorth$ShoppingAmountatAirport) ##
summary(df.GoingNorth$ShoppingAmountatAirport)
hist(df.GoingNorth$ShoppingAmountatAirport)
df.GoingNorth$ShoppingAmountatAirport_<-createBuckets(df.GoingNorth$ShoppingAmountatAirport)

ruleDF <- data.frame(df.GoingNorth$satis_,df.GoingNorth$AirlineStatus, df.GoingNorth$Gender,df.GoingNorth$age_, df.GoingNorth$PriceSensitivity_, df.GoingNorth$NoofFlightspa_,df.GoingNorth$TypeofTravel,df.GoingNorth$ArrivalDelaygreater5Mins, df.GoingNorth$ShoppingAmountatAirport_)
colnames(ruleDF) <- c("Satisfaction","AirlineStatus","Gender","Age","PriceSensitivity","NoofFlightspa","TypeofTravel","ArrivalDelaygreater5Mins","ShoppingAmountatAirport")

aRuleDF <- as(ruleDF,"transactions")

ruleset.high <- apriori(aRuleDF, parameter = list(support=0.01, confidence = 0.5), appearance = list(default="lhs",rhs=("Satisfaction=High")))


r1.high <- sort(ruleset.high, decreasing =TRUE, by = "lift")
r1.high.head <- head(r1.high, n = 10)
inspect(r1.high.head[2])

#plot(r1.high.head)
plot(r1.high.head[2], method="graph", control=list(type="items"))
plot(r1.high.head[2], method="paracoord", control=list(reorder = TRUE))

ruleset.low <- apriori(aRuleDF, parameter = list(support=0.01, confidence = 0.5), appearance = list(default="lhs",rhs=("Satisfaction=Low")))
r2.low <- sort(ruleset.low, decreasing =TRUE, by = "lift")
r2.low.head <- head(r2.low, n = 10)
inspect(r2.low.head[c(3,6)])

#plot(r1.high.head)
plot(r2.low.head[c(3,6)], method="graph", control=list(type="items"))
plot(r2.low.head[c(3,6)], method="paracoord", control=list(reorder = TRUE))

# Close of Association rule code ------------------------------------------------------------------------------------

# start of SVM -------------------------------------------------------------------------------------

rows.index <- row.names(df.model)
rows.index.sampling <- sample(rows.index, size = 50000)
df.sampled <- df.model[rows.index.sampling,]


summary(df.sampled$Satisfaction)
df.sampled$CustSat[df.sampled$Satisfaction <= 3] <- "Low"
df.sampled$CustSat[df.sampled$Satisfaction >= 4] <- "High" #values higher than 7 are termed as high
unique(df.sampled$CustSat)
df.sampled$CustSat <-  as.factor(df.sampled$CustSat)

dim(df.sampled)
randIndex <- sample(1:dim(df.sampled[1]))   # indexes of sample dataset
length(randIndex)                            # length of randIndex is same as total rows of hotel survey dataset
# 3. create a training data set and a test data set. 
cutPoint2_3 <- floor(2*dim(df.sampled[1])/3)
trainData <- df.sampled[randIndex[1:cutPoint2_3[1]],]   #Build our training set from the first 6666 rows of hotelSurvey Dataset
testData <- df.sampled[randIndex[(cutPoint2_3[1]+1):dim(df.sampled)[1]],]  

#Build a Model using ksvm( ) for sampled random dataset of 50,000 rows
Svmmodel1 <- ksvm(CustSat~AirlineStatus + Gender+ Age + PriceSensitivity + NoofFlightspa+ TypeofTravel+ ShoppingAmountatAirport
                  + ArrivalDelaygreater5Mins, data = trainData, kernel="rbfdot", kpar="automatic", C=5, cross=3, prob.model=TRUE)

Svmmodel1   #Training error :  0.199622 
#Cross validation error : 0.205142 
#predict the outcome for sampled DF
SvmPred <- predict(Svmmodel1, testData, type = "response")
pred <- data.frame(SvmPred)
table(testData$CustSat)
table(pred)
compTable <- data.frame(testData$CustSat,pred$SvmPred)
table(compTable)
#Calculate error rate for this sampled dataset
Error <- (table(compTable)[1,2]+table(compTable)[2,1])/nrow(testData)
Error  #0.2045084
Accuracy <- (table(compTable)[1,1]+table(compTable)[2,2])/nrow(testData)
Accuracy #0.7954916

# Applying SVM on dataset of onlyjets and goingnorth

# Preparation of dataset of onlyjets airline
df.onlyjet <- filter(df.model, AirlineName == "OnlyJets Airlines Inc.")
# Preparation of dataset of GoingNorth airline
df.GoingNorth <- filter(df.model, AirlineName == "GoingNorth Airlines Inc.")

summary(df.onlyjet$Satisfaction)
df.sampled$CustSat[df.onlyjet$Satisfaction <= 3] <- "Low"
df.sampled$CustSat[df.onlyjet$Satisfaction >= 4] <- "High" #values higher than 7 are termed as high
unique(df.onlyjet$CustSat)
df.sampled$CustSat <-  as.factor(df.onlyjet$CustSat)

dim(df.onlyjet)
randIndex <- sample(1:dim(df.onlyjet[1]))   # indexes of sample dataset
length(randIndex)                            # length of randIndex is same as total rows of hotel survey dataset
# 3. create a training data set and a test data set. 
cutPoint2_3 <- floor(2*dim(df.onlyjet[1])/3)
trainData <- df.sampled[randIndex[1:cutPoint2_3[1]],]   #Build our training set from the first 6666 rows of hotelSurvey Dataset
testData <- df.onlyjet[randIndex[(cutPoint2_3[1]+1):dim(df.onlyjet)[1]],]  

#Build a Model using ksvm( ) for onlyjets dataset
Svmmodel2 <- ksvm(CustSat~AirlineStatus + Gender+ Age + PriceSensitivity + NoofFlightspa+ TypeofTravel+ ShoppingAmountatAirport
                  + ArrivalDelaygreater5Mins, data = trainData, kernel="rbfdot", kpar="automatic", C=5, cross=3, prob.model=TRUE)

Svmmodel2   
SvmPred <- predict(Svmmodel2, testData, type = "response")
pred <- data.frame(SvmPred)
table(testData$CustSat)
table(pred)
compTable <- data.frame(testData$CustSat,pred$SvmPred)
table(compTable)
#Calculate error rate for onlyjet
Error <- (table(compTable)[1,2]+table(compTable)[2,1])/nrow(testData)
Error #0.1990899
Accuracy <- (table(compTable)[1,1]+table(compTable)[2,2])/nrow(testData)
Accuracy #0.8009101


summary(df.GoingNorth$Satisfaction)
df.GoingNorth$CustSat[df.GoingNorth$Satisfaction <= 3] <- "Low"
df.GoingNorth$CustSat[df.GoingNorth$Satisfaction >= 4] <- "High" #values higher than 7 are termed as high
unique(df.GoingNorth$CustSat)
df.GoingNorth$CustSat <-  as.factor(df.GoingNorth$CustSat)

dim(df.GoingNorth)
randIndex <- sample(1:dim(df.GoingNorth[1]))   # indexes of sample dataset
length(randIndex)                            # length of randIndex is same as total rows of hotel survey dataset
# 3. create a training data set and a test data set. 
cutPoint2_3 <- floor(2*dim(df.GoingNorth[1])/3)
trainData <- df.GoingNorth[randIndex[1:cutPoint2_3[1]],]   #Build our training set from the first 6666 rows of hotelSurvey Dataset
testData <- df.GoingNorth[randIndex[(cutPoint2_3[1]+1):dim(df.GoingNorth)[1]],]  

#Build a Model using ksvm( ) for GoingNorth dataset
Svmmodel3 <- ksvm(CustSat~AirlineStatus + Gender+ Age + PriceSensitivity + NoofFlightspa+ TypeofTravel+ ShoppingAmountatAirport
                  + ArrivalDelaygreater5Mins, data = trainData, kernel="rbfdot", kpar="automatic", C=5, cross=3, prob.model=TRUE)

Svmmodel3   
SvmPred <- predict(Svmmodel3, testData, type = "response")
pred <- data.frame(SvmPred)
table(testData$CustSat)
table(pred)
compTable <- data.frame(testData$CustSat,pred$SvmPred)
table(compTable)
#Calculate error rate for GoingNorth
Error <- (table(compTable)[1,2]+table(compTable)[2,1])/nrow(testData)
Error #0.2284069
Accuracy <- (table(compTable)[1,1]+table(compTable)[2,2])/nrow(testData)
Accuracy #0.7715931


# End of SVM -------------------------------------------------------------------------------

# Start of Decision Tree------------------------------------------------------------------------------

df.model$Target <- df.model$Satisfaction
df.model$Target <- ifelse(df.model$Satisfaction >3,"1","0")
View(df.model)
tree1<- rpart(Target~AirlineStatus + Gender+ Age + PriceSensitivity + NoofFlightspa+ TypeofTravel+ ShoppingAmountatAirport
              + ArrivalDelaygreater5Mins, data = trainData )
tpred <- predict(tree1,testData)
View(tpred)
# trainData and testData
randIndex <- sample(1:dim(df.model[1]))   # indexes of sample dataset
length(randIndex)                            # length of randIndex is same as total rows of hotel survey dataset
# 3. create a training data set and a test data set. 
cutPoint2_3 <- floor(2*dim(df.model[1])/3)
trainData <- df.model[randIndex[1:cutPoint2_3[1]],]   #Build our training set from the first 6666 rows of hotelSurvey Dataset
testData <- df.model[randIndex[(cutPoint2_3[1]+1):dim(df.model)[1]],]  

dim(trainData)
dim(testData)

tpred <-data.frame(tpred)
tpred <- round(tpred)
View(tpred)
compTable <- table(testData$Target,tpred$X1)
#Accura
Accuracy <- ((compTable)[1,1]+(compTable)[2,2])/nrow(testData)
Accuracy #0.7847696

#unique(df.model$AirlineName)
df.onlyjet <- filter(df.model, AirlineName == "OnlyJets Airlines Inc.")
# Preparation of dataset of GoingNorth airline
df.GoingNorth <- filter(df.model, AirlineName == "GoingNorth Airlines Inc.")
#========================
#####for OnlyJets
df.onlyjet$Target <- df.onlyjet$Satisfaction
df.onlyjet$Target <- ifelse(df.onlyjet$Satisfaction >3,"1","0")
View(df.onlyjet)
# trainData and testData
randIndex <- sample(1:dim(df.onlyjet[1]))   # indexes of sample dataset
length(randIndex)                            # length of randIndex is same as total rows of hotel survey dataset
# 3. create a training data set and a test data set. 
cutPoint2_3 <- floor(2*dim(df.onlyjet[1])/3)
trainData <- df.onlyjet[randIndex[1:cutPoint2_3[1]],]   #Build our training set from the first 6666 rows of hotelSurvey Dataset
testData <- df.onlyjet[randIndex[(cutPoint2_3[1]+1):dim(df.onlyjet)[1]],]  

dim(trainData)
dim(testData)
##
tree1<- rpart(Target~AirlineStatus + Gender+ Age + PriceSensitivity + NoofFlightspa+ TypeofTravel+ ShoppingAmountatAirport
              + ArrivalDelaygreater5Mins, data = trainData )
tpred <- predict(tree1,testData)
tpred <-data.frame(tpred)
tpred <- round(tpred)
View(tpred)
compTable <- table(testData$Target,tpred$X1)
compTable
#Accura
Accuracy <- ((compTable)[1,1]+(compTable)[2,2])/nrow(testData)
Accuracy # 0.7758817

#========================
#####for GoingNorth
df.GoingNorth$Target <- df.GoingNorth$Satisfaction
df.GoingNorth$Target <- ifelse(df.GoingNorth$Satisfaction >3,"1","0")
View(df.GoingNorth)
# trainData and testData
randIndex <- sample(1:dim(df.GoingNorth[1]))   # indexes of sample dataset
length(randIndex)                            # length of randIndex is same as total rows of hotel survey dataset
# 3. create a training data set and a test data set. 
cutPoint2_3 <- floor(2*dim(df.GoingNorth[1])/3)
trainData <- df.GoingNorth[randIndex[1:cutPoint2_3[1]],]   #Build our training set from the first 6666 rows of hotelSurvey Dataset
testData <- df.GoingNorth[randIndex[(cutPoint2_3[1]+1):dim(df.GoingNorth)[1]],]  

dim(trainData)#1041   25
dim(testData)#521  25

tree1<- rpart(Target~AirlineStatus + Gender+ Age + PriceSensitivity + NoofFlightspa+ TypeofTravel+ ShoppingAmountatAirport
              + ArrivalDelaygreater5Mins, data = trainData )
tpred <- predict(tree1,testData)
tpred <-data.frame(tpred)
tpred <- round(tpred)
View(tpred)
compTable <- table(testData$Target,tpred$X1)
compTable
#Accura
Accuracy <- ((compTable)[1,1]+(compTable)[2,2])/nrow(testData)
Accuracy # 0.7619962

# End of Decision Tree------------------------------------------------------------------------------

# Start of Visualisation code --------------------------------------------------------------------

#analysis the relationship between Satisfaction and 
df.AirlineStatus <- group_by(df.model,AirlineStatus)
z.AirlineStatus <- summarise(df.AirlineStatus, AveSat = mean(Satisfaction), MedianSat = median(Satisfaction),count=n())
View(z.AirlineStatus)
g <- ggplot(z.AirlineStatus,aes(x=AirlineStatus, y=AveSat))
g <- g + geom_col(aes(fill=count))
g
myboxPop <- ggplot(df.model,aes(x=AirlineStatus,y=Satisfaction,fill=AirlineStatus))+ geom_boxplot()
myboxPop
g <- ggplot(df.model,aes(x=AirlineStatus, y=nrow(df.model)))
g <- g + geom_col(aes(fill=reorder(Satisfaction,Satisfaction)))
g <- g +ylab("count")
g

#analysis the relationship between Satisfaction and Age
df.age <- group_by(df.model,Age)
z.Age <- summarise(df.age, AveSat = mean(Satisfaction), MedianSat = median(Satisfaction),count=n())
View(z.Age)
g1 <- ggplot(z.Age,aes(x=Age, y=AveSat))
g1 <- g1 + geom_line() +geom_point(aes(size=count))
g1

g <- ggplot(df.model,aes(x=Age, y=nrow(df.model)))
g <- g + geom_col(aes(fill=reorder(Satisfaction,Satisfaction)))
g <- g +ylab("count")
g


#analysis the relationship between Satisfaction and Gender
df.Gender <- group_by(df.model,Gender)
z.Gender <- summarise(df.Gender, AveSat = mean(Satisfaction), MedianSat = median(Satisfaction),count=n())
View(z.Gender)
g1 <- ggplot(z.Gender,aes(x=Gender, y=AveSat))
g1 <- g1 + geom_col(aes(fill=count))
g1

g2 <- ggplot(df.model,aes(x=Gender, y=nrow(df.model)))
g2 <- g2 + geom_col(aes(fill=reorder(Satisfaction,Satisfaction)))
g2 <- g2 +ylab("count")
g2

myboxPop <- ggplot(df.model,aes(x=Gender,y=Satisfaction,fill=Gender))+ geom_boxplot()
myboxPop
#pie chart
#make pie chart

AgedfPie <- data.frame(
  group = c("Male", "Female"),
  count = c(55614, 71866)
)
g3 <- ggplot(AgedfPie,aes(x="",y=count,fill=group))+geom_bar(width = 1, stat = "identity")
g3
Pie <- g3 + coord_polar("y", start=0)
Pie + scale_fill_brewer(palette="Blues")+
  theme_minimal()+geom_text(aes(y = count/2 + c(0, cumsum(count)[-length(count)]),label=percent(count/100)))



#analysis the relationship between Satisfaction and Price Sensitivity
df.PriceSensitivity <- group_by(df.model,PriceSensitivity)
z.PriceSensitivity <- summarise(df.PriceSensitivity, AveSat = mean(Satisfaction), MedianSat = median(Satisfaction), count = n())
View(z.PriceSensitivity)

# visualization
g <- ggplot(z.PriceSensitivity,aes(x=PriceSensitivity, y=AveSat))
g <- g + geom_line() +geom_point(aes(size=count))
g

g2 <- ggplot(df.model,aes(x=PriceSensitivity, y=nrow(df.model)))
g2 <- g2 + geom_col(aes(fill=reorder(Satisfaction,Satisfaction)))
g2 <- g2 +ylab("count")
g2

g2 <- ggplot(df.model,aes(x=PriceSensitivity, y=Satisfaction))
g2 <- g2 + geom_boxplot() 
g2
#analysis the relationship between Satisfaction and NoofFlightsp.a.
df.NoofFlightspa <- group_by(df.model,NoofFlightspa)
z.NoofFlightspa <- summarise(df.NoofFlightspa, AveSat = mean(Satisfaction), MedianSat = median(Satisfaction), count = n())
View(z.NoofFlightspa)

# visualization
g <- ggplot(z.NoofFlightspa,aes(x=NoofFlightspa, y=AveSat))
g <- g + geom_line()+geom_point(aes(size=count))
g
g2 <- ggplot(df.model,aes(x=NoofFlightspa, y=nrow(df.model)))
g2 <- g2 + geom_col(aes(fill=reorder(Satisfaction,Satisfaction)))
g2 <- g2 +ylab("count")
g2

### analysis the relationship between Satisfaction and TypeofTravel
df.TypeofTravel <- group_by(df.model,TypeofTravel)
z.TypeofTravel <- summarise(df.TypeofTravel, AveSat = mean(Satisfaction), MedianSat = median(Satisfaction),count=n())
View(z.TypeofTravel)

g <- ggplot(z.TypeofTravel, aes(x=TypeofTravel, y=AveSat,fill=count)) +geom_col()
g
g <- ggplot(df.model, aes(x=TypeofTravel, y=reorder(Satisfaction,Satisfaction) ,fill=Satisfaction)) +geom_col()
g
myboxPop <- ggplot(df.model,aes(x=TypeofTravel,y=Satisfaction,fill=TypeofTravel))+ geom_boxplot()
myboxPop
g2 <- ggplot(df.model,aes(x=TypeofTravel, y=nrow(df.model)))
g2 <- g2 + geom_col(aes(fill=reorder(Satisfaction,Satisfaction)))
g2 <- g2 +ylab("count")
g2

#analysis the relationship between Satisfaction and DepartureDelayinMinutes
df.DepartureDelayinMinutes <- group_by(df.model,DepartureDelayinMinutes)
z.DepartureDelayinMinutes <- summarise(df.DepartureDelayinMinutes, AveSat = mean(Satisfaction), 
                                       MedianSat = median(Satisfaction),count=n())
View(z.DepartureDelayinMinutes)

#use subset of the database to Avoid interference with outliers 
z.Sub.DepartureDelayinMinutesS <- z.DepartureDelayinMinutes[which(z.DepartureDelayinMinutes$count>100),]
View(z.Sub.DepartureDelayinMinutesS)

# visualization
g <- ggplot(z.Sub.DepartureDelayinMinutesS,aes(x=DepartureDelayinMinutes, y=AveSat))
g <- g + geom_line()+ geom_point(aes(size=count))
g
g2 <- ggplot(df.model,aes(x=DepartureDelayinMinutes, y=nrow(df.model)))
g2 <- g2 + geom_col(aes(fill=reorder(Satisfaction,Satisfaction)))
g2 <- g2 +ylab("count")
g2

#analysis the relationship between Satisfaction and ArrivalDelayinMinutes
df.ArrivalDelayinMinutes <- group_by(df.model,ArrivalDelayinMinutes)
z.ArrivalDelayinMinutes <- summarise(df.ArrivalDelayinMinutes, AveSat = mean(Satisfaction), 
                                     MedianSat = median(Satisfaction),count=n())
View(z.ArrivalDelayinMinutes)
z.Sub.ArrivalDelayinMinutes <- z.ArrivalDelayinMinutes[which(z.ArrivalDelayinMinutes$count>100),]
# visualization
g <- ggplot(z.Sub.ArrivalDelayinMinutes,aes(x=ArrivalDelayinMinutes, y=AveSat))
g <- g + geom_line()+ geom_point(aes(size=count))
g

#analysis the relationship between Satisfaction and ArrivalDelaygreater5Mins
df.ArrivalDelaygreater5Mins <- group_by(df.model,ArrivalDelaygreater5Mins)
z.ArrivalDelaygreater5Mins <- summarise(df.ArrivalDelaygreater5Mins, AveSat = mean(Satisfaction), 
                                        MedianSat = median(Satisfaction),count=n())
View(z.ArrivalDelaygreater5Mins)
# visualization
g <- ggplot(z.ArrivalDelaygreater5Mins,aes(x=ArrivalDelaygreater5Mins, y=AveSat, fill=count))
g <- g + geom_col()
g
g2 <- ggplot(df.model,aes(x=ArrivalDelaygreater5Mins, y=nrow(df.model)))
g2 <- g2 + geom_col(aes(fill=reorder(Satisfaction,Satisfaction)))
g2 <- g2 +ylab("count")
g2
g2 <- ggplot(df.model,aes(x=ArrivalDelaygreater5Mins, y=Satisfaction, fill = ArrivalDelaygreater5Mins))
g2 <- g2 + geom_boxplot() + theme_classic()
g2

### analysis the relationship between Satisfaction and percentofFlightwithotherAirlines
df.percentofFlightwithotherAirlines <- group_by(df.model,percentofFlightwithotherAirlines)
z.percentofFlightwithotherAirlines <- summarise(df.percentofFlightwithotherAirlines, AveSat = mean(Satisfaction), MedianSat = median(Satisfaction),count=n())
View(z.percentofFlightwithotherAirlines)

g <- ggplot(z.percentofFlightwithotherAirlines, aes(x=percentofFlightwithotherAirlines, y=AveSat)) +geom_line()+geom_point(aes(size=count))
g
g <- ggplot(df.model, aes(x=percentofFlightwithotherAirlines, y=reorder(Satisfaction,Satisfaction) ,fill=Satisfaction)) +geom_col()
g

g2 <- ggplot(df.model,aes(x=percentofFlightwithotherAirlines, y=nrow(df.model)))
g2 <- g2 + geom_col(aes(fill=reorder(Satisfaction,Satisfaction)))
g2 <- g2 +ylab("count")
g2
### analysis the relationship between Satisfaction and No.ofotherLoyaltyCards
df.No.ofotherLoyaltyCards <- group_by(df.model,No.ofotherLoyaltyCards)
z.No.ofotherLoyaltyCards <- summarise(df.No.ofotherLoyaltyCards, AveSat = mean(Satisfaction), MedianSat = median(Satisfaction),count=n())
View(z.No.ofotherLoyaltyCards)

g <- ggplot(z.No.ofotherLoyaltyCards, aes(x=No.ofotherLoyaltyCards, y=AveSat)) +geom_line()+geom_point(aes(size=count))
g
g <- ggplot(df.model, aes(x=percentofFlightwithotherAirlines, y=reorder(Satisfaction,Satisfaction) ,fill=Satisfaction)) +geom_line()+geom
g

g2 <- ggplot(df.model,aes(x=No.ofotherLoyaltyCards, y=nrow(df.model)))
g2 <- g2 + geom_col(aes(fill=reorder(Satisfaction,Satisfaction)))
g2 <- g2 +ylab("count")
g2

# End of Visualisation code ----------------------------------------------------------------------


