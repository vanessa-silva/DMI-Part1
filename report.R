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
plot(sum4$num ~ sum4$Offense.Type, xlab = "Offense.Type", ylab = "Num", main = "Number of offenses (per offense)")


sum5 <- group_by(dat, Offense.Type, Beat) %>% summarise(num=sum(X..offenses))


#Data Summarization

dat %>% summarise(avg.Off=mean(dat$X..offenses),
                  cen.OffTp=centralValue(dat$Offense.Type),
                  cen.StrNm=centralValue(dat$StreetName))

