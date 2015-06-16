library(Metrics)
library(h2o)
h2oServer <- h2o.init()   ##we'd like to try this, but does not initialize
library(data.table)   ## load data in quickly with fread
library(Metrics)
x <- fread("train.csv")
test <- fread("test.csv")

## prep the species column by moving the test-only UNSPECIFIED CULEX to CULEX ERRATICUS, and re-doing the levels
## logistic regression will complain otherwise
vSpecies<-c(as.character(x$Species),as.character(test$Species))
vSpecies[vSpecies=="UNSPECIFIED CULEX"]<-"CULEX ERRATICUS"
vSpecies<-factor(vSpecies,levels=unique(vSpecies))

## data.table syntax for adding a column; could overwrite the existing column as well
x[,Species2:=factor(vSpecies[1:nrow(x)],levels=unique(vSpecies))]
test[,Species2:=factor(vSpecies[(nrow(x)+1):length(vSpecies)],levels=unique(vSpecies))]

## also add some fields for components of the date using simple substrings
x[,dMonth:=substr(x$Date,6,7)]
x[,dYear:=substr(x$Date,1,4)]
test[,dMonth:=substr(test$Date,6,7)]

## glance at some conditional probabilities
x[,mean(WnvPresent),by="Species"][order(V1)]  ##glance at average by species
x[,mean(WnvPresent),by="Block"][order(V1)]  ##glance at average by block
x[,mean(WnvPresent),by="dMonth"][order(V1)]  ##glance at average by month of year

### Start modeling
## use 2011 as a cross validation year; x1 will include the other three years; x2 will include 2011
x1<-x[dYear!=2011,]
x2<-x[dYear==2011,]

## check sizes of split frames
dim(x1)
dim(x2)

## fit a logistic regression model using just three key fields
fitCv<-glm(WnvPresent ~ dMonth + Species2 + Block, data = x1, family = "binomial")
p2<-predict(fitCv, newdata = x2, type = "response")
## check for a reasonable AUC of the model against unseen data (2011)
auc(x2$WnvPresent,p2)

## now fit a new model to all the data, so that our final submission includes information learned from 2011 as well
fitSubmit<-glm(WnvPresent ~ dMonth + Species2 + Block, data = x, family = "binomial")
levels(test$Species2)
pSubmit<-predict(fitSubmit, newdata = test, type = "response")

## look at the predicted distribution (AUC doesn't care about probabilities; just ordering. It's still a good diagnostic)
summary(pSubmit)

submissionFile<-cbind(test$Id,pSubmit)
colnames(submissionFile)<-c("Id","WnvPresent")
options("scipen"=100, "digits"=8)
write.csv(submissionFile,"logistic_regression_three_factors.csv",row.names=FALSE,quote=FALSE)

# dplyr provides a lot of great ways to handle your data.
# In this example, we will merge Train and Weather data by Date column.
# The main operator of this tutorial is %>%. It taxes argument from the left
# and send it functions on the righ. x %>% f(...) is equivivalent to f(x, ...).
# Looks useless for now, but believe me, you'll love it. 

library(dplyr)
library(data.table)

# Fast reading with data.table and converting to usual data.frame
train <- as.data.frame(fread("train.csv"))
weather <- as.data.frame(fread("weather.csv"))

# For example, I want to select few columns from weather (that are interesting for me),
# convert everything to numeric (by default everything is char),
# convert Date to date format, and take the mean temperatures between two stations for every date.
# With dplyr it's simple and beautiful!

weather <- weather %>%                              # Take weather data.frame and send it to select function.
  select( Date:Tmin, DewPoint ) %>%               # Selecting only interesting columns. Result will be sent further.
  mutate_each(funs(as.numeric), -Date) %>%    # Applying as.numeric to every (but not Date) columns.
  mutate(Date = as.Date(Date)) %>%        # applying as.Date to Date column
  group_by(Date) %>%                  #Grouping by date to summarise
  summarise_each(funs(mean))      # And take mean for every date for (average of two stations)

head(weather) # Take a look at our new data. Isn't it cool?

train <- train %>%
  mutate(Date = as.Date(Date)) %>%    # Transforming Date column to date format.
  left_join(weather, by="Date")   # Joining our data frames by Date column.

colnames(train) # Here you go. Now you can start your serious business. (Note last three columns)
