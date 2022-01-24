rm(list=ls())
setwd("~/Academic Projects/Shellfish Mesocosms/1_viu.feeding.trials")

library(lubridate)
library(fishualize)
library(tidyverse)

# Load data 
datafile1 <- "~/Academic Projects/Acoustic Tagging Project/2021 Trials/mayne.tags/data/302202.csv"

rec.202 <- read.csv(datafile1, header=T, na.strings = c("", "NA", "na", "N/A"), strip.white = TRUE)
rec.204 <- read.csv(datafile2, header=T, na.strings = c("", "NA", "na", "N/A"), strip.white = TRUE)
rec.983 <- read.csv(datafile3, header=T, na.strings = c("", "NA", "na", "N/A"), strip.white = TRUE)
rec.989 <- read.csv(datafile4, header=T, na.strings = c("", "NA", "na", "N/A"), strip.white = TRUE)
rec.991 <- read.csv(datafile5, header=T, na.strings = c("", "NA", "na", "N/A"), strip.white = TRUE)
rec.992 <- read.csv(datafile6, header=T, na.strings = c("", "NA", "na", "N/A"), strip.white = TRUE)
rec.996 <- read.csv(datafile7, header=T, na.strings = c("", "NA", "na", "N/A"), strip.white = TRUE)
crab.data <- read.csv(datafile8, header=T, na.strings = c("", "NA", "na", "N/A"), strip.white = TRUE)
rec.locations <- read.csv(datafile9, header=T, na.strings = c("", "NA", "na", "N/A"), strip.white = TRUE)