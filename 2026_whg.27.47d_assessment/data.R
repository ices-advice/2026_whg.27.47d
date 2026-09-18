## Before: 
## After:  data.Rdata

rm(list=ls())
graphics.off()

library(icesTAF)
library(stockassessment)
library(FLCore)

year<-2026   # assessment, last survey year
datayear<-2025  # catch data


taf.boot(clean = FALSE, data = TRUE, software = FALSE) 

mkdir("data")
mkdir("model")
mkdir("output")
mkdir("report/ACOMplots")
mkdir("report/Mixfish")

source("utilities_data.R")

save.image( file="data/data.RData")
