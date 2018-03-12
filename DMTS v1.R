# TEMPLATE for starting new R script
# something new
#==============================================================================
#   1. Settings, packages, and options
#==============================================================================

# Clear the working space
salaries <- c(25000, 41000, 41000, 96000, 53000)
mean(salaries)

wants <- c("readxl","readr","dplyr","tidyr","ggplot2","magrittr")y
has   <- wants %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has])

# Remember only need to install once, after that hashtag out the install
#install.packages("PACKAGENAMEGOESHERE")
#library(LIBRARYNAME)
library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(magrittr)

# get_os from http://conjugateprior.org/2015/06/identifying-the-os-from-r/ 
# retrieved 16 Oct 2017

get_os <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}

# Set working directory (edit for YOUR folder, where you put the file "raw_anonymized_data.csv"

what_os <- get_os()

if (what_os == "osx") {
  setwd("/Users/MBell/Dropbox/R/DMTS") #Mac
} else {
  setwd("C:/Users/M Bell office/Dropbox/R/DMTS") #PC
}


#==============================================================================
#   2. Data section
#==============================================================================



#dmts <- read_csv("~/Dropbox/R/DMTS/30Jun17 DMTS Winter 2017 reversals v4 Rv1.csv")

dmts <- read_excel("~/Dropbox/R/DMTS/DMTS White & Brown 2011 rep R.xlsx",sheet = "delay sum R")
#View(dmts)
dl8 <- read_excel("~/Dropbox/R/DMTS/DMTS White & Brown 2011 rep R.xlsx",sheet = "last8")

dmts$age <- as.factor(dmts$age)
dmts$SID <- as.factor(dmts$SID)
dmts$condition <- as.factor(dmts$condition)


dl8$age <- as.factor(dl8$age)
dl8$bird <- as.factor(dl8$bird)
dl8$condition <- as.factor(dl8$condition)


#yy <- tapply(dmts$d02,dmts$SID,mean)
#yy <- tapply(dmts$d02, list(dmts$SID,dmts$condition),mean)
#yy <- tapply(dmts$d02,dmts$condition,mean)

#ggplot(yy, aes(x=dmts$SID, y=dmts$d01)) + geom_bar(stat="identity")

#nope
ggplot(dmts,aes(x=dmts$condition,y=d02, fill=age)) + geom_bar(stat="identity", position="dodge")

#individual bar graphs, frequncy by delay ???
qplot(d02, data=dmts, facets = ~ SID, geom="bar")

#trial-by-trial accuracy for birds
qplot(trial, d02, data=dmts, facets = ~ SID, geom="line")

#trial-by-trial accuracy for birds, all delays, all conditions
ggplot(dmts, aes(trial)) +
  geom_line(aes(y = d02, colour = "red")) +
  geom_line(aes(y = d3)) +
  geom_line(aes(y = d6)) +
  geom_line(aes(y = d12)) +
  geom_line(aes(y = d24)) +
  facet_wrap(~SID)

#same as above BUT each condition separate            
ggplot(dmts, aes(trial)) +
  geom_line(aes(y = d02, colour = "red")) +
  geom_line(aes(y = d3)) +
  geom_line(aes(y = d6)) +
  geom_line(aes(y = d12)) +
  geom_line(aes(y = d24)) +
  facet_grid(condition ~ SID)

#same as 
ggplot(dmts, aes(trial)) +
  geom_line(aes(y = d02, colour = "d .02")) +
  geom_line(aes(y = d3, colour = "d3")) +
  geom_line(aes(y = d6, colour = "d6")) +
  geom_line(aes(y = d12, colour = "d12")) +
  geom_line(aes(y = d24, colour = "d24")) +
  facet_grid(SID ~ condition)

ggplot(dmts, aes(trial)) +
  geom_line(aes(y = d02, colour="red") +
  geom_line(aes(y = d3) +
  geom_line(aes(y = d6) +
  geom_line(aes(y = d12) +
  geom_line(aes(y = d24) +
  facet_grid(SID ~ condition)


dmts <- dmts %>%
  filter(condition != "MTS1", condition != "MTS2")

dmts <- dmts[complete.cases(dmts), ]
  

qplot(condition, mean(d02), data=dl8)

jnk <- data.frame(dly = c("0.2 s", "3 s"," 6 s", "12 s", "24 s"),
                  dvs = c(mean(dl8$d02),mean(dl8$d3),mean(dl8$d6),mean(dl8$d12),mean(dl8$d24)))

# FROM HERE, CREATING WHITE & BROWN GRAPHS

dat <- with(dl8, tapply(d02, condition, mean))

jnk <- data.frame(dly = c(0.2, 3, 6, 12, 24),
                  dvs = c(mean(dl8$d02),mean(dl8$d3),mean(dl8$d6),mean(dl8$d12),mean(dl8$d24)))
install.packages("doBy")
library(doBy)
avgdly <- summaryBy(d02+d3 ~ age, data=dmts)

# TO HERE, CREATING W&B GRAPHS

ggplot(dl8, aes(x=condition, y=d02)) +
  geom_bar(stat="identity")

+
  facet_grid(SID ~ condition)
