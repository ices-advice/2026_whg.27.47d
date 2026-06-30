

load("model/model.RData")

base.y<-max(fit$data$years)-1  # in spring

#ref points
Fmsy     <-0.35  
Fmsylower<-0.29
Fpa      <-0.35

MSYBtrig <-201845
Bpa  <-201845
Blim <-144175

startR<-1983  # sampling from recruitment series, start year

# use recent 3 years of fishing selectivity
selYears<-max(fit$data$years) +(-3:-1)  
bioYears<-max(fit$data$years) +(-3:-1)
recYears<-startR:(base.y-1)

#estimated total F at age
summary_F<-summary(fit)
Fsq    <-tail(summary_F[,7],2)[[1]]



FC1<-list()

Fo<-0

# for range of Fs, catch option table
for(i in 1:51){
  Fo<-Fo+0.01
  set.seed(12345)
  FC1[[length(FC1)+1]] <- forecast(fit,year.base=base.y,ave.years=bioYears, rec.years=recYears,overwriteSelYears=selYears, addTSB=TRUE, deterministic=FALSE, splitLD =TRUE, fval=c(Fsq,Fsq,Fo, Fo),label=paste("option",round(Fo,3)))
  
}



# check SSB level

FC0<-list()

set.seed(12345)
FC0 <- forecast(fit,year.base=base.y,ave.years=bioYears, rec.years=recYears,overwriteSelYears=selYears, addTSB=TRUE, deterministic=FALSE, splitLD =TRUE, fval=c(Fsq,Fsq,Fsq,Fsq), label="Fsq")                                   

SSBnext<-attributes(FC0)$tab[,"ssb:median"][[3]] # check if above msy Btrigger



# advice table options

FC<-list()

set.seed(12345)
FC[["Fmsy"]] <- forecast(fit,year.base=base.y,ave.years=bioYears, rec.years=recYears,overwriteSelYears=selYears, addTSB=TRUE, deterministic=FALSE, splitLD =TRUE, fval=c(Fsq,Fsq,Fmsy,Fmsy),label="Fmsy", savesim=T)                                   

set.seed(12345)
FC[["Fpa"]] <- forecast(fit,year.base=base.y,ave.years=bioYears, rec.years=recYears,overwriteSelYears=selYears, addTSB=TRUE, deterministic=FALSE, splitLD =TRUE, fval=c(Fsq,Fsq,Fpa,Fpa), label="Fpa", savesim=T)

set.seed(12345)
FC[["Fmsyupper"]] <- forecast(fit,year.base=base.y,ave.years=bioYears, rec.years=recYears,overwriteSelYears=selYears, addTSB=TRUE, deterministic=FALSE, splitLD =TRUE, fval=c(Fsq,Fsq,Fpa,Fpa), label="Fmsyupper", savesim=T)

set.seed(12345)
FC[["Fmsylower"]] <- forecast(fit,year.base=base.y,ave.years=bioYears, rec.years=recYears,overwriteSelYears=selYears, addTSB=TRUE, deterministic=FALSE, splitLD =TRUE, fval=c(Fsq,Fsq,Fmsylower,Fmsylower), label="Fmsylower", savesim=T)

set.seed(12345)
FC[["No fishery"]] <- forecast(fit,year.base=base.y,ave.years=bioYears, rec.years=recYears,overwriteSelYears=selYears, addTSB=TRUE, deterministic=FALSE, splitLD =TRUE, fval=c(Fsq,Fsq,0,0),label="No fishery", savesim=T)                                   

set.seed(12345)
FC[["Fsq"]] <- forecast(fit,year.base=base.y,ave.years=bioYears, rec.years=recYears,overwriteSelYears=selYears, addTSB=TRUE, deterministic=FALSE, splitLD =TRUE, fval=c(Fsq,Fsq,Fsq,Fsq),label="Fsq", savesim=T)

set.seed(12345)
FC[["0.75*FSQ"]] <- forecast(fit,year.base=base.y,ave.years=bioYears, rec.years=recYears,overwriteSelYears=selYears, addTSB=TRUE, deterministic=FALSE,splitLD =TRUE, fval=c(Fsq,Fsq,0.75*Fsq, 0.75*Fsq), label="0.75*FSQ", savesim=T)

set.seed(12345)
FC[["1.25*FSQ"]] <- forecast(fit,year.base=base.y,ave.years=bioYears, rec.years=recYears,overwriteSelYears=selYears, addTSB=TRUE, deterministic=FALSE, splitLD =TRUE, fval=c(Fsq,Fsq,1.25*Fsq, 1.25*Fsq), label="1.25*FSQ", savesim=T)

set.seed(12345)
FC[["SSB=Bpa"]] <- forecast(fit,year.base=base.y,ave.years=bioYears, rec.years=recYears,overwriteSelYears=selYears, addTSB=TRUE, deterministic=FALSE, splitLD =TRUE,  fval = c(Fsq,Fsq, NA, NA), nextssb = c(NA, NA, Bpa,Bpa), label="SSB=Bpa")

set.seed(12345)
FC[["SSB=Blim"]] <- forecast(fit,year.base=base.y,ave.years=bioYears, rec.years=recYears,overwriteSelYears=selYears, addTSB=TRUE, deterministic=FALSE, splitLD =TRUE, fval = c(Fsq,Fsq, NA, NA), nextssb = c(NA, NA, Blim,Blim), label="SSB=Blim")


## Additional solver for more precise matching of median value for SSB targets
assess_year<-base.y+1

scen<-list()
scen[[("SSB=Bpa exact")]]  <-  list( fscale = c(NA, NA, NA, NA), catchval = c(NA, NA, NA, NA), fval = c(Fsq, Fsq, NA, NA), nextssb = c(NA, NA, Bpa, Bpa))
scen[[("SSB=Blim exact")]] <-  list( fscale = c(NA, NA, NA, NA), catchval = c(NA, NA, NA, NA), fval = c(Fsq, Fsq, NA, NA), nextssb = c(NA, NA, Blim, Blim))

scen_num <- which(grepl(paste0("SSB="),
                        names(scen), fixed = TRUE))

scen_num_FC<- c(length(FC)-1,length(FC))

FC2 <- vector("list", length(scen_num))
names(FC2) <- names(scen)[scen_num]

for(i in seq(FC2)){
  
  ARGS <- scen[[scen_num[i]]]
  ARGS <- c(ARGS,
            list(fit = fit, year.base=base.y, ave.years = bioYears, rec.years = recYears, label = names(scen)[scen_num[i]], overwriteSelYears = selYears,
                 splitLD = TRUE, savesim = TRUE,addTSB=TRUE))
  
  fun <- function(fval = 0.35, ARGS){
    set.seed(12345)
    ARGS2 <- ARGS
    ARGS2$nextssb <- c(NA, NA, NA, NA)
    ARGS2$fval[3:4]<- fval
    
    fc <- do.call(forecast, ARGS2)
    fc_tab <- attr(fc, "tab")
    ssbmed <- fc_tab[rownames(fc_tab) == as.character(assess_year + 2),
                     colnames(fc_tab) == "ssb:median"]
    fitness <- sqrt((ssbmed - ARGS$nextssb[4])^2)
    return(fitness)
  }
  
  message("\n## Optimization for scenario \"",
          names(scen)[scen_num][i], "\"...")
  ## Non optimized scenario should constitute a safe starting point:
  Fstart <- attr(FC[[scen_num_FC[i]]],
                 "tab")[as.character(assess_year + 1),
                        "fbar:median"]
  
  Frange <- Fstart * c(0.9, 1.1) # -/+ 10%
  
  system.time(
    res <- optim(par = Fstart, fn = fun, ARGS = ARGS,
                 lower = Frange[1], upper = Frange[2],
                 method = "Brent",
                 control = list(## trace = 4,
                   ## factr = 1e-3,
                   abstol = 0.49
                 )))
  
  set.seed(12345)
  ARGS2 <- ARGS
  ARGS2$nextssb <- c(NA, NA, NA, NA)
  ARGS2$fval[3:4] <- res$par
  
  FC2[[i]] <- do.call(forecast, ARGS2)
  attr(FC2[[i]], "tab")
  
}

# save complete data file for Fmsy run

set.seed(12345)
FC3<- forecast(fit, ave.years=bioYears, year.base = base.y, rec.years=recYears,overwriteSelYears=selYears, fval=c(Fsq,Fsq,Fmsy,Fmsy),splitLD =FALSE, label="Fmsy savesim",savesim=TRUE)



# save FC options, FC1 Franges, FC2 excat SSB, FC3 Fmsy complete simunlationd data
save(FC,FC1, FC2, FC3, file="model/forecast_new.RData")


