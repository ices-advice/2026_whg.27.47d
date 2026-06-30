## Run analysis, write model results

## Before: www.stockassessment.org current assessment, SURBAR input data
## After: RData (model, residuals, model, leaveout, retros, forecast, forecast_options, s.results1 (all , north, south))

rm(list=ls())
graphics.off()

library(icesTAF)
library(stockassessment)
library(minpack.lm)
library(FLCore)
library(FLfse)

##  1 SURBAR

load("data/data.Rdata")
source("utilities_model.R")

#Define paths and load functions
data.path<-"bootstrap\\data\\"

set.seed(11111)


# SURBAR prep

startage<-1
  
  plus.gp <- 8
  mean.f.range <- c(2,4) # Survey data only used up to age 5 (coupled to age 4)
  mean.z.range <- c(2,4) 
  
  #Read in data for NS whiting
  
  now <- datayear  #last historic year data
  
  f.temp <- read.assessment.data(wk.data.path = data.path, wk.mean.f.range = mean.f.range, # add an extra line in bootstrap/data/wstock file
                                 wk.plus.gp = plus.gp, areas="all")
  
  s.stock <- f.temp$s.stock
  s.stock <-  trim(s.stock, age = 0:5, year = 1983:now)  # trim to age 5 not to deal with plusgroup
  s.index <- f.temp$s.index
  s.index[[1]] <-trim(s.index[[1]], age = 1:5)
  s.index[[2]] <-trim(s.index[[2]], age = 0:5)
  f.stock <- f.temp$f.stock
  f.stock <- trim(f.stock, age = 0:f.stock@range["max"], year = 1978:now)  #new
  f.stock <- setPlusGroup(f.stock, plus.gp)                 
  
 
  # Set up and produce SURBAR run
  set.seed(54321)
  s.results <- surbar.wrapper(wk.stock = s.stock, wk.index = s.index,
                              wk.lambda = 5, wk.refage = 3, wk.zrange = mean.z.range, startyear=1983, startage=startage)  
  
  save(s.results,f.stock, file=paste0("model\\surbar_results",startage,".Rdata"))
  save(s.index, file=paste0("model\\surbar_index",startage,".Rdata"))


# by area

cc<-c("north", "south")

for (ii in 1:2) {
  
  a<-cc[ii]
  plus.gp <- 8
  mean.f.range <- c(2,4)
  mean.z.range <- c(2,4) # Survey data only used up to age 5
  ref.points <- data.frame(Blim = NA, Bpa = NA, Flim = NA, Fpa = NA, Fmsy = NA)
  
  #Read in data for NS whiting
  
  now <- datayear  #last historic year data
  f.temp <- read.assessment.data(wk.data.path = data.path, wk.mean.f.range = mean.f.range,
                                 wk.plus.gp = plus.gp, areas=a)
  s.stock <- f.temp$s.stock
  s.stock <-  trim(s.stock, age = 0:5, year = 1983:now) 
  s.index <- f.temp$s.index
  s.index[[1]] <-trim(s.index[[1]], age = 1:5)
  s.index[[2]] <-trim(s.index[[2]], age = 0:5)
  x.index <- f.temp$f.index
  
  # Set up and produce SURBAR run
  set.seed(54321)
  s.results <- surbar.wrapper(wk.stock = s.stock, wk.index = s.index,
                              wk.lambda = 5.0, wk.refage = 3, wk.zrange = mean.z.range, startyear=1983,startage=1)
  
  save(s.results, file=paste("model\\surbar_results_",a,".Rdata", sep=""))
  
}


## 2.  download SAM results from stockassessment.org
###########################################################################################################

#current run
stockname<-as.character(substitute(NSwhiting_2026n))  # change to new current run!

options(download.file.method = "wininet")

download.file(url=sub("SN",stockname , "https://stockassessment.org/datadisk/stockassessment/userdirs/user3/SN/run/model.RData"),destfile="model/model.RData")
download.file(url=sub("SN",stockname , "https://stockassessment.org/datadisk/stockassessment/userdirs/user3/SN/run/data.RData"),destfile="model/data.RData")
download.file(url=sub("SN",stockname , "https://stockassessment.org/datadisk/stockassessment/userdirs/user3/SN/run/leaveout.RData"),destfile="model/leaveoneout.RData")
download.file(url=sub("SN",stockname , "https://stockassessment.org/datadisk/stockassessment/userdirs/user3/SN/run/residuals.RData"),destfile="model/residuals.RData")
download.file(url=sub("SN",stockname , "https://stockassessment.org/datadisk/stockassessment/userdirs/user3/SN/run/retro.RData"),destfile="model/retro.RData")

load("model/model.Rdata")
load("model/data.RData")
load("model/leaveoneout.RData")
load("model/residuals.RData")
load("model/retro.RData")
save.image( file="model/allmodel.RData")


# create output for MIXFISH with current run
stk_fit<-FLfse::SAM2FLStock(fit, catch_estimate = T, mat_est = F, stock.wt_est = F, catch.wt_est = F, m_est = T, spinoutyear=TRUE)
stk_fit@desc<-"FLStock created from SAM model fit, model estimated values, catches include IBC"
save(stk_fit, file="model\\whg.27.47d_FLStock_model_estimates.Rdata")


stk_dat<-FLfse::SAM2FLStock(fit)
stk_dat@desc<-"FLStock created from SAM model fit, input data, catches include IBC"
save(stk_dat, file="model\\whg.27.47d_FLStock_input_data.Rdata")


stockname<-as.character(substitute(NSwhiting_2025))  # change to old run for comparison!

options(download.file.method = "wininet")


download.file(url=sub("SN",stockname , "https://stockassessment.org/datadisk/stockassessment/userdirs/user3/SN/run/model.RData"),destfile="model/model_old.RData")
download.file(url=sub("SN",stockname , "https://stockassessment.org/datadisk/stockassessment/userdirs/user3/SN/run/data.RData"),destfile="model/data_old.RData")

stockname<-as.character(substitute(BM2026_12_newID_nomat_smooth))  # benchmark run for comparison

options(download.file.method = "wininet")

download.file(url=sub("SN",stockname , "https://stockassessment.org/datadisk/stockassessment/userdirs/user3/SN/run/model.RData"),destfile="model/model_BM.RData")
download.file(url=sub("SN",stockname , "https://stockassessment.org/datadisk/stockassessment/userdirs/user3/SN/run/data.RData"),destfile="model/data_BM.RData")

###########################################################################################################################
#SAM forecast
rm(list=ls())

library(TAF)
detach.packages()
library(stockassessment)
library(TMB)
library(Matrix)

source("model_forecast.R")
