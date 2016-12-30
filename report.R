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
library(DMwR)
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


#Data Summarization

dat %>% summarise(avg.Off=mean(dat$X..offenses),
                  cen.OffTp=centralValue(dat$Offense.Type),
                  cen.StrNm=centralValue(dat$StreetName))


sum1_ <- group_by(i1, Beat) %>% summarise(num=sum(X..offenses)/length(unique(i1$Date)))



#Random data division
sp <- sample(1:nrow(dat), as.integer(nrow(dat)*0.7))
tr <- dat[sp,]
ts <- dat[-sp,]

#Obtain a multiple linear regression model using the largest set
library(DMwR) 
la <- lm(X..offenses ~ Beat + HourType, tr) 

#Check the diagnostic information provided for the model
summary(la) 

final_la <- step(la)
summary(final_la) 

#Obtain the predictions of the obtained model on the smaller set
preds <- predict(la,ts)


##Naive Bayes 

library(e1071) 

#Random data division
sp <- sample(1:nrow(modelDat), as.integer(nrow(modelDat)*0.7))
tr <- modelDat[sp,]
ts <- modelDat[-sp,]

nb <- naiveBayes(N ~ Beat + HourType, tr) 
nb
#pred <- predict(nb, ts)    ##Warning message: In data.matrix(newdata) : NAs introduced by coercion??
#(mtrx <- table(pred,ts$N))
#(err <- 1-sum(diag(mtrx))/sum(mtrx))
#head(predict(nb,ts,type='raw'))

##with Laplace correction
nb_ <- naiveBayes(N ~ Beat + HourType, tr, laplace=1)
#preds_ <- predict(nb_, ts)    ##Warning message: In data.matrix(newdata) : NAs introduced by coercion??
#(mtrx <- table(preds_,ts$N))
#(err <- 1-sum(diag(mtrx))/sum(mtrx))
#head(predict(nb_,ts,type='raw'))


##SVMs

s <- svm(N ~ Beat + HourType,tr) 
preds <- predict(s,ts)

ps <- predict(s,ts) 

mc <- table(ps,ts$N)
(error <- 100*(1-sum(diag(mc))/sum(mc)))


##??-SV Regression

s <- svm(N ~ Beat + HourType, tr, cost=10, epsilon=0.02) 
preds <- predict(s,ts) 
regr.eval(ts$N, preds)

plot(ts$N, preds, main='Errors Scaterplot', ylab='Predictions', xlab='True') 
abline(0,1,col='red',lty=2)


##Feed-forward Multilayer ANN

library(nnet)
nn <- nnet(N ~ Beat + HourType, tr, size=8, decay=0.1, maxit=1000)
(mtrx <- table(predict(nn,ts),ts$N))
nn

summary(nn)
head(predict(nn,ts))


##MARS 

library(earth) 

mars <- earth(N ~ Beat + HourType, tr)
preds <- predict(mars,ts) 

summary(mars)


##Random Forests

library(randomForest) 
#m <- randomForest(N ~ Beat + HourType, tr)
#preds <- predict(m,ts) 


##Holdout 

library(DMwR) 
m <- rpartXse(N ~ Beat + HourType,tr)
regr.eval(ts$N,p,train.y=tr$N)
p <- predict(m,ts) 

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
