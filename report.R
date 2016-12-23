library(gdata)

perldir <- Sys.which("perl")

# one last try
if (perldir == "" & .Platform$OS.type == "windows")
  perldir <- "C:\\Perl64\\bin\\perl.exe"

fc <- "crime.xls" 
dat <- read.xls(fc, sheet = 1, header = TRUE, verbose=FALSE, perl=perldir)
#dat$Beat <- as.character(dat$Beat)

unique(dat$Offense.Type)
unique(dat$Beat)
#unique(dat$Premise)
unique(dat$Hour)

library(lubridate)
library(xts)
library(dplyr)
dat <- tbl_df(dat)


datxts <- xts(dat, ymd_h(paste(dat$Date,' ',dat$Hour)))
i1 <- filter(dat, 8 <= dat$Hour, dat$Hour < 12)
i2 <- filter(dat, 12 <= dat$Hour, dat$Hour < 19)
i3 <- filter(dat, (19 <= dat$Hour & dat$Hour <= 23) | (0 <= dat$Hour & dat$Hour < 8))

sum1 <- group_by(i1, Beat) %>% summarise(num=sum(X..offenses))
