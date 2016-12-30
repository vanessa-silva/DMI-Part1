library(gdata)

perldir <- Sys.which("perl")

# one last try
if (perldir == "" & .Platform$OS.type == "windows")
  perldir <- "C:\\Perl64\\bin\\perl.exe"

fc <- "crime.xls" 
dat <- read.xls(fc, sheet = 1, header = TRUE, verbose=FALSE, perl=perldir)
#dat$Beat <- as.character(dat$Beat)

unique(dat$Offense.Type)
dat <- subset(dat, dat$Offense.Type != 1)
unique(dat$Beat)
#unique(dat$Premise)
unique(dat$Hour)

library(lubridate)
library(xts)
library(dplyr)
library(stringr)
library(DMwR)
library(class)
library(nnet)
library(e1071)
library(earth)
library(performanceEstimation)
library(DMwR) 
library(rpart.plot)

load("gps.RData")
incomplete <- is.na(dat$Beat)
tr <- gps[!incomplete,1:2]
ts <- gps[incomplete,1:2]
dat$Beat[incomplete] <- knn(tr, ts, dat$Beat[!incomplete], k = 3)

incomplete <- (dat$Suffix == "-")
tr <- gps[!incomplete,1:2]
ts <- gps[incomplete,1:2]
dat$Suffix[incomplete] <- is.character(knn(tr, ts, dat$Suffix[!incomplete], k = 3))

incomplete <- is.na(dat$BlockRange)
dat$BlockRange[incomplete] <- centralValue(dat$BlockRange)

incomplete <- (dat$Type == "-")
dat$Type[incomplete] <- centralValue(dat$Type[!incomplete])

dat <- tbl_df(dat)


datxts <- xts(dat, ymd_h(paste(dat$Date,' ',dat$Hour)))
i1 <- filter(dat, 8 <= dat$Hour, dat$Hour < 12)
i2 <- filter(dat, 12 <= dat$Hour, dat$Hour < 19)
i3 <- filter(dat, (19 <= dat$Hour & dat$Hour <= 23) | (0 <= dat$Hour & dat$Hour < 8))

sum1 <- group_by(i1, Beat) %>% summarise(num=sum(X..offenses))
sum2 <- group_by(i2, Beat) %>% summarise(num=sum(X..offenses))
sum3 <- group_by(i3, Beat) %>% summarise(num=sum(X..offenses))

sum4 <- group_by(dat, Offense.Type) %>% summarise(num=sum(X..offenses))

sum5 <- group_by(dat, Offense.Type, Beat) %>% summarise(num=sum(X..offenses))

sum1_ <- group_by(i1, Beat) %>% summarise(num=sum(X..offenses)/length(unique(i1$Date)))

fiscalDate <- function(arg) {
  date <- arg[2]
  hour <- arg[1]
  if (as.integer(hour) >= 0 && as.integer(hour) < 8)
    return(as.character(ymd(date)-1));
  return(as.character(ymd(date)));
}

#Identification of the time period
hourType <- function(hour) {
  hour <- as.integer(hour)
  if (8 <= hour && hour < 12)
    return(1)
  if (12 <= hour && hour < 19)
    return(2)
  return(3)
}

args <- mapply(c, dat$Hour, as.character(dat$Date), SIMPLIFY = FALSE)

dat <- mutate(dat, HourType=sapply(Hour, hourType))
dat <- mutate(dat, FiscalDate=sapply(args, fiscalDate))

modelDat <- group_by(dat, FiscalDate, Beat, HourType) %>% summarize(N=sum(X..offenses))


####Holdout Method

#Random data division
sp <- sample(1:nrow(modelDat), as.integer(nrow(modelDat)*0.7))
tr <- modelDat[sp,]
ts <- modelDat[-sp,]


#Neural Networks
(nn <- nnet(N ~ Beat + HourType, tr, size=8, decay=0.1, maxit=1000))
preds <- predict(nn,ts)
(mc <- table(preds,ts$N))

summary(nn)
head(predict(nn,ts))

(error <- 100*(1-sum(diag(mc))/sum(mc)))
regr.eval(ts$N, preds)


##SVMs
(s <- svm(N ~ Beat + HourType,tr))
preds <- predict(s,ts)

(mc <- table(preds,ts$N))
(error <- 100*(1-sum(diag(mc))/sum(mc)))

regr.eval(ts$N, preds)


##MARS
mars <- earth(N ~ Beat + HourType, tr)
preds <- predict(mars,ts) 

(mc <- table(preds,ts$N))
(error <- 100*(1-sum(diag(mc))/sum(mc)))

summary(mars)
(mae <- mean(abs(ts$N-preds)))

regr.eval(ts$N, preds)

#Tree-based 
m <- rpartXse(N ~ Beat + HourType,tr)
preds <- predict(m,ts) 

(mc <- table(preds, ts$N))
(error <- 100*(1-sum(diag(mc))/sum(mc)))
(mae <- mean(abs(preds-ts$N)))

regr.eval(ts$N, preds)

prp(m, type=4,extra=101)


#Mean Squared Error
#mse <- mean((trueVals-preds)^2)

#Root Mean Squared Error
#rmse <- sqrt(mse)

#Mean Absolute Error
#mae <- mean(abs(trueVals-preds))

#Normalized Mean Squared Error
#nmse <- sum((trueVals-preds)^2) / sum((trueVals-mean(trueVals))^2)

#Normalized Mean Absolute Error
#nmae <- sum(abs(trueVals-preds)) / sum(abs(trueVals-mean(trueVals)))

#Mean Average Percentage Error
#mape <- mean(abs(trueVals-preds)/trueVals)

#Correlation between the predictions and the true values
#corr <- cor(trueVals,preds)
