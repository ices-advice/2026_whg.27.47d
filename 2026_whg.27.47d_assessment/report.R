## Prepare plots and tables for report

## Before: SAM results (RES, RESP, RETRO,LO, CPUE), SURBAR results (surbar_results1), DATRAS CPUE
## After: Tables 17-22, Figures  CPUE maps (for all years in output, final Q1, Q3 in report), SAM, SAM-SURBAR comparison

rm(list=ls())
graphics.off()

library(icesTAF)
library(stockassessment)
library(grid)
library(FLCore)
#remotes::install_github("ices-tools-dev/mixfishtools")

library(ggplot2)
library(ggplotFL)
library(tidyverse)
library(lattice)
library(cowplot)
library(RColorBrewer)
library(EnvStats)
library(icesSAG)

source("utilities_output.R")
source("utilities_report.R")

load("data/data.Rdata")

datayear<-year-1
fyear<-year
cutage<-8


load("model/allmodel.Rdata")
source("report_SAMSURBAR_compare.R")

## 1  Report Plots ##

d<-read.taf("bootstrap/data/totalcatch.csv")

taf.png("Fig_4_Yield")  
par(mfrow = c(1,1),cex=1.3, mar = c(4,4,2,1) + 0.1)
plot(c(1978:datayear),d[,"landings"]/1000,lwd=3, lty="solid", col="black", type="l",bty="l", ylim=c(0,120),ylab="Yield (1000 tonnes)" ,xlab="Year")
lines(c(1978:datayear),d[,"discards"]/1000,lwd=3, lty="solid", col="purple", type="l")
lines(c(1978:datayear),d[,"ibc"]/1000,lwd=3, lty="solid", col="darkorange", type="l")
legend("topright", legend=c("Landings","Discards + BMS","IBC"),  col=c("black","purple","darkorange"),lwd=3, bty="n" )
dev.off()


# prop discarded of total catches, does not work with taf.png
df<-read.taf("bootstrap/data/df8.csv")

png('report/Fig_5_prop_discarded.png',  width=2800, height=2600, res=300)
par(mfrow = c(3,3),cex=1, mar = c(3,2,1,1))
for(i in 1:(cutage+1)){plot(c(1978:datayear),df[,i],lwd=1.8, lty="solid" ,col="black", type="l",ylab="", ylim=c(0,1.1),xlab="Year")
  legend("topleft",paste("Age",i-1,sep=" "), col="black", bty="n")
}
dev.off()

# plot mean weights at age


cweights<-read.csv("bootstrap/data/wcatch8.csv", header=TRUE) 
lweights<-read.csv("bootstrap/data/wlandings8.csv", header=TRUE) 
dweights<-read.csv("bootstrap/data/wdiscards8.csv", header=TRUE) 
iweights<-read.csv("bootstrap/data/wibc8.csv", header=TRUE) 

lweights[lweights==0]<-NA
dweights[dweights==0]<-NA
iweights[iweights==0]<-NA

colours<- palette.colors(palette="Okabe_Ito")

png('report/Fig_6_catchmeanweights.png',  width=3400, height=3000, res=400)

par(mfrow = c(2,2),cex=1, mar = c(4,4,2,1) + 0.1)
plot(cweights$Year, cweights$X0*1000, col="black", ylim=c(0,850), lwd=1.6, xlab="", ylab="Mean Weight (g)", bty="l",type="l",main="Total catch")
for(ll in 2:9)lines(cweights$Year, cweights[,ll+1]*1000, col=colours[ll+1], pch=20, lwd=1.6)
legend("topright", legend=c("0","1","2","3","4","5","6","7","8+"),  col=colours,lwd=1.6, bty="n",ncol=3 )

plot(lweights$Year, lweights$X0*1000, col="black", ylim=c(0,850), lwd=1.6, xlab="", ylab="Mean Weight (g)", bty="l",type="l",main="Landings")
for(ll in 2:9)lines(lweights$Year, lweights[,ll+1]*1000, col=colours[ll+1], pch=20, lwd=1.6)

plot(dweights$Year, dweights$X0*1000, col="black", ylim=c(0,850), lwd=1.6, xlab="", ylab="Mean Weight (g)", bty="l",type="l",main="Discards+BMS")
for(ll in 2:9)lines(dweights$Year, dweights[,ll+1]*1000, col=colours[ll+1], pch=20, lwd=1.6)

plot(dweights$Year, iweights$X0*1000, col="black", ylim=c(0,850), lwd=1.6, xlab="", ylab="Mean Weight (g)", bty="l",type="l",main="IBC")
for(ll in 2:9)lines(iweights$Year, iweights[,ll+1]*1000, col=colours[ll+1], pch=20, lwd=1.6)

dev.off()

# stock weights at age

sw <- (fit$data$stockMeanWeight)*1000

png(paste('report/Fig_7_stockweights_',stockname,'.png',sep=""),  width=2800, height=2300, res=400)
par(mfrow=c(1,1),mar = c(4,4,1,1))
matplot(1978:(1978+nrow(sw)-2), sw[1:(nrow(sw)-1),], type="l", lty="solid", lwd=2,col=colours[c(1:7,7,7)], xlab="Year", bty="l",ylab="Stock weights (g)",xlim=c(1978,(datayear+3)), ylim=c(0,800))
matplot((1978+nrow(sw)-1), tail(sw,1), pch=8, col=colours[c(1:7,7,7)], add=TRUE)
legend("topleft", legend=c("Age 0", "1","2","3","4","5","6+"), lwd=2, lty=1, bty="n",ncol=2, col=colours[1:7])
dev.off()

#maturity

mat <- (fit$data$propMat)

png(paste('report/Fig_8_maturity_',stockname,'.png',sep=""),  width=2800, height=2300, res=400)
par(mfrow=c(1,1),mar = c(4,4,1,1))
matplot(1978:(1978+nrow(mat)-1), mat, type="l", lty="solid", lwd=2,col=colours[c(1:7,7,7)], xlab="Year", bty="l",ylab="Proportion mature",xlim=c(1978,(datayear+3)), ylim=c(0,1.2))
legend("topleft", legend=c("0", "1","2","3","4","5","6+"), lwd=2, lty=1, bty="n",ncol=4, col=colours[c(1:7,7,7)])
abline(v=2021, lty="dashed")
abline(v=1991, lty="dashed")
dev.off()

# natural mortality

nm <- exp(fit$pl$logNM)
nm_lo <- exp(fit$pl$logNM-2*fit$plsd$logNM)
nm_up <- exp(fit$pl$logNM+2*fit$plsd$logNM)

le<-length(1978:datayear)

png(paste('report/Fig_9_natMor_GMRF_',stockname,'forecast values.png',sep=""),  width=3500, height=3000, res=400)
par(mfrow=c(1,1),mar = c(4,4,1,1))
matplot(1978:(1978+nrow(nm)-1), nm,bty="l", type="l", lty="solid", lwd=2,col=colours, xlab="Year", ylab="Natural mortality",xlim=c(1978,(datayear+3)), ylim=c(0,3))
matplot(1978:(1978+nrow(dat$natMor)-1), dat$natMor,pch=20, col=colours, add=TRUE)
matplot(1978:(1978+nrow(nm_lo)-1), nm_lo, type="l", lty="dotted", col=colours, add=TRUE)
matplot(1978:(1978+nrow(nm_up)-1), nm_up, type="l", lty="dotted", col=colours, add=TRUE)
matplot((1978+le+c(0:2)), nm[c((le+1):(le+3)),], pch=8, col=colours[c(1:7,7,7)], add=TRUE)
legend("topleft", legend=c("0", "1","2","3","4","5","6","7","8+"), lwd=2, lty=1, bty="n",ncol=2, col=colours)
abline(v=2022,lty="dashed")
dev.off()

nm <- exp(fit$pl$logNM)
nm<-cbind(1978:(1977+(dim(nm)[1])), nm)
colnames(nm)<-c("Year", 0:8)
write.taf(nm,"Table_M_gmrf.csv", dir="report")

# Biomass at age bar plot

catchobs<-read.csv("bootstrap/data/catage8.csv", header=TRUE) 
rownames(catchobs)<-catchobs[,1]
catchobs<-catchobs[,-1]
colnames(catchobs)<-c( "0", "1","2","3","4","5","6","7","8")

coul <- brewer.pal(9, "Paired")
png('report/Fig_Biomass at age prop.png',  width=3000, height=2500, res=400)
par(mfrow=c(1,1),mar = c(4,4,1,1))
barplot(prop.table(t(fit$data$stockMeanWeight*ntable(fit)),margin=2),xlab="Year",ylab="Proportion Stock biomass at age", space=0,legend=T,col=coul, args.legend = list(x = "topleft"))
dev.off()

png('report/Fig_Est Catch biomass at age prop.png',  width=3000, height=2500, res=400)
par(mfrow=c(1,1),mar = c(4,4,1,1))
barplot(prop.table(t(fit$data$catchMeanWeight[,,1]*(caytable_new(fit)[-nrow(caytable_new(fit)),])),margin=2),xlab="Year",ylab="Estimated proportion of catch biomass at age",space=0,legend=T,col=coul, args.legend = list(x = "topleft"))
dev.off()

png('report/Fig_Obs Catch biomass at age prop.png',  width=3000, height=2500, res=400)
par(mfrow=c(1,1),mar = c(4,4,1,1))
barplot(prop.table(t(fit$data$catchMeanWeight[,,1]*catchobs),margin=2),xlab="Year",ylab="Observed proportion of catch biomass at age",space=0,legend=T,col=coul, args.legend = list(x = "topleft"))
dev.off()

png('report/Fig_Obs Catch numbers at age prop.png',  width=3000, height=2500, res=400)
par(mfrow=c(1,1),mar = c(4,4,1,1))
barplot(prop.table(t(catchobs),margin=2),xlab="Year",ylab="Observed proportion of catch numbers at age",space=0,legend=T,col=coul, args.legend = list(x = "topleft"))
dev.off()


png('report/Fig_SSB at age prop.png',  width=3000, height=2500, res=400)
par(mfrow=c(1,1),mar = c(4,4,1,1))
barplot(prop.table(t(fit$data$stockMeanWeight*fit$data$propMat*ntable(fit)),margin=2),ylab="Proportion SSB at age", xlab="Year",space=0,legend=T,col=coul, args.legend = list(x = "topleft"))
dev.off()



# Survey/SURBAR plots
###########################################################################################################################
load("model/surbar_index1.Rdata")  # startage 1 SURBAR results
load("model/surbar_results1.Rdata")  # startage 1 SURBAR results


# Figure : Survey catch curves
png('report\\Fig_18_surveyCatchcurves.png', width=1700, height=1400,res=200)
plot.surbar(s.results, "catch.curve", nums=c(1,2))
dev.off()

png('report\\Fig_19_log_by_cohort.png', width=1700, height=1400,res=200)
plot.surbar(s.results, "log.by.cohort")
dev.off()

# SURBAR stock summary
png('report\\Fig_22_surbarSummary.png', width=1400, height=1100,res=200)
plot.surbar(s.results, "sum.line")
dev.off()

# SURBAR log survey residuals
png('report\\Fig_23_surbarresiduals.png', width=1400, height=1400,res=200)
plot.surbar(s.results, "res.smooth")
dev.off()

# SURBAR parameter estimates
png('report\\surbarparameterEstimates.png', width=1700, height=1000,res=200)
plot.surbar(s.results, "params")
dev.off()

#Figure : Commercial catch curves
png('report\\Fig_25_logcommercialcc.png', width=1400, height=1200,res=200)
plot.catch.curve.and.grads(f.stock, wk.ptype = "c", wk.ages = mean.f.range, 
                           wk.main = "Commercial Catch Data", wk.yrs = f.stock@range["minyear"]:f.stock@range["maxyear"])
dev.off()

#Figure : Commercial catch curve gradients
png('report\\Fig_26_commercialccgradients.png', width=1400, height=1200,res=200)
par(mfrow = c(1,1), mar = c(5,5,4,3))
plot.catch.curve.and.grads(f.stock, wk.ptype = "g", wk.ages = c(2,7), 
                           wk.main = "Commercial Catch Data", wk.yrs = f.stock@range["minyear"]:f.stock@range["maxyear"])
dev.off()

source("utilities_report.R")

#Figure : Commercial catch correlations
png('report\\Fig_27_commercialccorrelations.png', width=1200, height=1200,res=200)
plot.index.corr(wk.object=list(FLIndex(catch.n = f.stock@catch.n, name = "Catch numbers at age")),
                wk.type = "FLR")
dev.off()

#####################################################################################################
# Figures SAM

taf.png("Fig_28_Stocksummary", width=2200, height=1500)
par(mfrow=c(2,2),mar = c(4,4,1,1))
catchplot(fit,xlab="Year",las=0)
recplot(fit,xlab="Year",las=0,drop=1,addCI=TRUE)
fbarplot(fit,xlab="Year",partial=F,addCI=TRUE)
ssbplot(fit,addCI=T,xlab="Year", las=0)
dev.off()


attr(RESP, 'fleetNames')[[2]]<- c("Joint sample residuals log(F)") 
taf.png("Fig_29_Process Residuals",width=1600,height=1500)
plot(RESP)
dev.off()

taf.png("Fig_30_Obs Residuals",width=1300,height=1300)
plot(RES)
dev.off()

fig<-31:33

for(f in 1:fit$data$noFleets){
  taf.png(paste("Fig_",fig[f],"_observed_predicted_fleet_",f,sep=""))
  stockassessment::fitplot(fit, fleets=f)
  dev.off()
}


taf.png("Fig_34_Leaveoneout", width=2200, height=1500)
par(mfrow=c(2,2),mar = c(5,5,1,1))
catchplot(LO, xlab="Year")
recplot(LO, xlab="Year", drop=1)
fbarplot(LO, xlab="Year")
ssbplot(LO,xlab="Year")
dev.off()


mlag0<-stockassessment::mohn(RETRO, lag=0)
mlag1<-stockassessment::mohn(RETRO, lag=1)

taf.png("Fig_35_Retro")
par(mfrow=c(2,2),mar = c(4,4,1,1))
catchplot(RETRO,xlab="Year",las=0)
recplot(RETRO,xlab="Year", las=0, drop=1)
legend("topright", legend=round(mlag1[1],3), bty="n")
fbarplot(RETRO, las=0, drop=1, xlab="Year")
legend("topright", legend=round(mlag1[3],3), bty="n")
ssbplot(RETRO,xlab="Year", las=0, drop=0)
legend("topright", legend=round(mlag0[2],3), bty="n")
dev.off()

taf.png("Fig_36_SR")
srplot(fit)
dev.off()

taf.png("Fig_37_sdR")
idx <- names(fit$sdrep$value) == "logR"
sdLogR<-fit$sdrep$sd[idx]
plot(1978:datayear,sdLogR[1:length(1978:datayear)], ylim=c(0,0.5),xlab="Year",ylab="sdLogR", pch=16)
dev.off()

ssb<-summary(fit)[,"SSB"]
rec<-ntable(fit)[,"0"]
taf.png("Rec_per_SSB.png")
plot(1978:year,log(rec/ssb),type='b',xlab="Year",ylab="ln(Recruits/SSB) ",xlim=c(1978,datayear),cex.lab=1.5)
dev.off()


#Tables SAM
#
ftab<-read.taf("output/Fatage.csv")
ftab[,2:(cutage)]<-round(ftab[,2:(cutage)],3)
write.taf(ftab,"Table_20_F_SAM.csv", dir="report")

ntab<-read.taf("output/natage.csv")
ntab[,2:(cutage)]<-round(ntab[,2:(cutage)])
write.taf(ntab,"Table_21_N_SAM.csv", dir="report")

summary<-read.taf("output/summary.csv")
write.taf(summary,"Table_22_Summary_SAM_I.csv", dir="report")

ctab<-read.taf("output/catchtab.csv")
ctab[,2:4]<-round(ctab[,2:4])
write.taf(ctab,"Table_23_Summary_SAM_II.csv", dir="report")


ptab <- partable(fit)
ptab<-xtab2taf(ptab)
ptab[,-1]<-round(ptab[,-1],3)
colnames(ptab)[1]<-" "
write.taf(ptab,"Table_24_SAM_model_parameters.csv",dir="report")


mohn1<-as.data.frame(round(stockassessment::mohn(RETRO, lag=0),3))
mohn2<-as.data.frame(round(stockassessment::mohn(RETRO, lag=1),3))
mohn<-rbind(mohn2[1,],mohn1[2,], mohn2[3,])
rownames(mohn)<-rownames(mohn1)
write.taf(mohn,"Table_25_mohnsrho.csv", dir="report", row.names=T)



# Selectivity of the Fishery
taf.png("Fig_Fishselectivity")
sel <- t(faytable(fit)/fbartable(fit)[,1])
sel[is.na(sel)]<-0
op <- par(mfrow=c(3,3), mai=c(0.4,0.4,0.3,0.3), oma=c(3,3,0,0))
age.sel<-as.integer(rownames(sel))
for(i in round(seq(1,dim(sel)[2],length=9))){
  plot(age.sel, sel[,i], type="l", xlab="", ylab="", lwd=1.5, ylim=c(0,max(sel)))
  if (i+1<dim(sel)[2])try(lines(age.sel, sel[,i+1], col="red", lwd=1.5))
  if (i+2<dim(sel)[2]) try(lines(age.sel, sel[,i+2], col="blue", lwd=1.5))
  if (i+3<dim(sel)[2]) try(lines(age.sel, sel[,i+3], col="green", lwd=1.5))
  legend("topleft",paste(c(colnames(sel)[i],colnames(sel)[i+1],colnames(sel)[i+2],colnames(sel)[i+3])), lty=rep(1,4), col=c("black","red","blue","green"),bty="n")
}
mtext("Age", 1, outer=T, line=1)
mtext("F/Fbar", 2, outer=T, line=1)

dev.off()


taf.png("Fig_partial_F")
F<-faytable(fit)
matplot(rownames(F),F,lty=1:ncol(F),col=colours[1:ncol(F)], type='l', xlab='Year', lwd=3)
legend('topright', col=colours[1:ncol(F)], lty=1:ncol(F), legend=colnames(F), bty='n', lwd=3)

dev.off()



# create SAG xml file
stockinfo <-stockInfo(
  StockCode = "whg.27.47d",
  AssessmentYear = year, 
  StockCategory = "1",
  ModelType="A",
  ModelName ="SAM",
  ConfidenceIntervalDefinition="95%",
  ContactPerson = "tanja.miethe@gov.scot",
  MSYBtrigger=201845,
  Bmsy=240451,
  Bpa=201845,
  FMSY=0.35,
  Blim=144175,
  Fpa=0.35,
  RecruitmentAge=0)

#bms<-read.table("bootstrap/data/whg47d_bms.dat",skip=5)
t<-read.csv("bootstrap/data/totalcatch.csv")#-bms
d<-t[,"discards"]
l<-t[,"landings"]
i<-t[,"ibc"]
c<-t[,"catch"]

tsb    <- round(tsbtable(fit))
colnames(tsb) <- c("TSB","Low", "High")

tabsummary<- data.frame(summary(fit), tsb)
tsb<-data.frame(rownames(tsb), tsb)

tabsummary<-data.frame(rownames(tabsummary), tabsummary)
colnames(tabsummary)<-c("Year", "R.age.0", "R_Low","R_High","SSB","SSB_Low","SSB_High","Fbar.2.5","Fbar.2.5_Low","Fbar.2.5_High","TSB","TSB_Low","TSB_High")


fishdata <- stockFishdata(Year = tabsummary$Year)
# For the standard graphs, recruitment in intermediate year = geomatric mean(2003:data year), used in forecast
fishdata$Recruitment <- c(tabsummary$R.age.0[1:(length(tabsummary$R.age.0)-1)],geoMean(tabsummary$R.age.0[6:(length(tabsummary$R.age.0)-2)])) 
# Forecast is deterministic, so we don't show the CIs
fishdata$Low_Recruitment<-c(tabsummary$R_Low[1:(length(tabsummary$R_Low)-1)],NA)
fishdata$High_Recruitment<-c(tabsummary$R_High[1:(length(tabsummary$R_High)-1)],NA)
fishdata$Low_StockSize<-tabsummary$SSB_Low#/1000
fishdata$High_StockSize<-tabsummary$SSB_High#/1000
fishdata$StockSize<-tabsummary$SSB#/1000
fishdata$TBiomass<-tabsummary$TSB
fishdata$Low_TBiomass<-tabsummary$TSB_Low
fishdata$High_TBiomass<-tabsummary$TSB_High
fishdata$FishingPressure <-c(tabsummary$Fbar.2.5[1:(length(tabsummary$Fbar.2.5)-1)],NA)
fishdata$Low_FishingPressure<-c(tabsummary$Fbar.2.5_Low[1:(length(tabsummary$Fbar.2.5_Low)-1)],NA)
fishdata$High_FishingPressure<-c(tabsummary$Fbar.2.5_High[1:(length(tabsummary$Fbar.2.5_High)-1)],NA)
fishdata$Catches<-c(c,NA)
fishdata$Landings<-c(l,NA)
fishdata$Discards<-c(d,NA)
#fishdata$LandingsBMS<-rbind(bms,NA)$V1
fishdata$IBC<-c(i,NA)

# Add ref points
# Check IBC column name
# Recruitment for final year to add

xml <- createSAGxml(stockinfo, fishdata)


# ADD retro
# Note: ICES SAG expects commas as decimal separators in retro-bias values
fmt_comma <- function(x) gsub("\\.", ",", as.character(x))

retro_xml <- paste0(
  "\n<Assessment_retro-bias>\n",
  "<TerminalYear>", datayear, "</TerminalYear>\n",
  "<RetroAssessment>5</RetroAssessment>\n",
  "<Fbarrho>", fmt_comma(mohn[3,1]), "</Fbarrho>\n",
  "<SSBrhoYear>Y</SSBrhoYear>\n",
  "<SSBrho>", fmt_comma(mohn[2,1]), "</SSBrho>\n",
  "<RecruitmentrhoYear>Y</RecruitmentrhoYear>\n",
  "<Recruitmentrho>", fmt_comma(mohn[1,1]), "</Recruitmentrho>\n",
  "</Assessment_retro-bias>\n"
)

# Insert retro-bias block before the closing </Assessment> tag
xmlfile <- sub("</Assessment>", 
               paste0(retro_xml, 
                      "<Chart_Settings><GraphKey>0</GraphKey><SettingKey>0</SettingKey><SettingValue>yes</SettingValue></Chart_Settings>\n", "</Assessment>"), xml)

# SAVE to file
cat(xmlfile, file="report/whg_27_47d.xml")





#######################################################################################################################
#SURBAR comparisons

load("model\\surbar_results_north.Rdata")
s.north<-s.results
load("model\\surbar_results_south.Rdata")
s.south<-s.results
load("model\\surbar_results1.Rdata")
s.all<-s.results

for(i in 1:3){
  
  if(i==1) s.results<-s.north
  if(i==2) s.results<-s.south
  if(i==3) s.results<-s.all

  wk.stock<-s.results$s.stock
  wk.psim<-s.results$s.psim
  wk.y1 <- s.results$s.y1
  wk.y2 <- s.results$s.y2
  
  # meanZ
  wk.stock.meanz <- do.call(rbind, lapply(wk.psim, function(wk){wk$meanz}))
  wk.stock.meanz.quantile <- array(NA, dim = c(dim(wk.stock.meanz)[2],5))
  wk.stock.meanz.mean <- rep(NA, dim(wk.stock.meanz)[2])
  rownames(wk.stock.meanz.quantile) <- wk.y1:wk.y2
  colnames(wk.stock.meanz.quantile) <- c("5%","25%","50%","75%","95%")
  
  for (wk.i in 1:dim(wk.stock.meanz)[2])
  {
    wk.stock.meanz.quantile[wk.i,] <- quantile(wk.stock.meanz[,wk.i], c(0.05, 0.25, 0.5, 0.75, 0.95))
    wk.stock.meanz.mean[wk.i] <- mean(wk.stock.meanz[,wk.i])
  }
  
  # SSB
  wk.stock.ssb <- do.call(rbind, lapply(wk.psim, function(wk){wk$ssb}))
  wk.stock.ssb.quantile <- array(NA, dim = c(dim(wk.stock.ssb)[2],5))
  wk.stock.ssb.mean <- rep(NA, dim(wk.stock.ssb)[2])
  rownames(wk.stock.ssb.quantile) <- wk.y1:wk.y2
  colnames(wk.stock.ssb.quantile) <- c("5%","25%","50%","75%","95%")
  for (wk.i in 1:dim(wk.stock.ssb)[2])
  {
    wk.stock.ssb.quantile[wk.i,] <- quantile(wk.stock.ssb[,wk.i], c(0.05, 0.25, 0.5, 0.75, 0.95))
    wk.stock.ssb.mean[wk.i] <- mean(wk.stock.ssb[,wk.i])
  }
  
  # TSB
  wk.stock.tsb <- do.call(rbind, lapply(wk.psim, function(wk){wk$tsb}))
  wk.stock.tsb.quantile <- array(NA, dim = c(dim(wk.stock.tsb)[2],5))
  wk.stock.tsb.mean <- rep(NA, dim(wk.stock.tsb)[2])
  rownames(wk.stock.tsb.quantile) <- wk.y1:wk.y2
  colnames(wk.stock.tsb.quantile) <- c("5%","25%","50%","75%","95%")
  for (wk.i in 1:dim(wk.stock.tsb)[2])
  {
    wk.stock.tsb.quantile[wk.i,] <- quantile(wk.stock.tsb[,wk.i], c(0.05, 0.25, 0.5, 0.75, 0.95))
    wk.stock.tsb.mean[wk.i] <- mean(wk.stock.tsb[,wk.i])
  }
  
  # Recruitment
  
  wk.stock.rec <- do.call(rbind, lapply(wk.psim, function(wk){wk$rec}))
  wk.stock.rec.quantile <- array(NA, dim = c(dim(wk.stock.rec)[2],5))
  wk.stock.rec.mean <- rep(NA, dim(wk.stock.rec)[2])
  rownames(wk.stock.rec.quantile) <- wk.y1:wk.y2
  colnames(wk.stock.rec.quantile) <- c("5%","25%","50%","75%","95%")
  for (wk.i in 1:dim(wk.stock.rec)[2])
  {
    wk.stock.rec.quantile[wk.i,] <- quantile(wk.stock.rec[,wk.i], c(0.05, 0.25, 0.5, 0.75, 0.95))
    wk.stock.rec.mean[wk.i] <- mean(wk.stock.rec[,wk.i])
  }
  
  
  if(i==1){wk.stock.meanz.quantile1<-wk.stock.meanz.quantile
  wk.stock.ssb.quantile1<-wk.stock.ssb.quantile
  wk.stock.tsb.quantile1<-wk.stock.tsb.quantile
  wk.stock.rec.quantile1<-wk.stock.rec.quantile
  }
  
  if(i==2){wk.stock.meanz.quantile2<-wk.stock.meanz.quantile
  wk.stock.ssb.quantile2<-wk.stock.ssb.quantile
  wk.stock.tsb.quantile2<-wk.stock.tsb.quantile
  wk.stock.rec.quantile2<-wk.stock.rec.quantile
  }
  if(i==3){wk.stock.meanz.quantile3<-wk.stock.meanz.quantile
  wk.stock.ssb.quantile3<-wk.stock.ssb.quantile
  wk.stock.tsb.quantile3<-wk.stock.tsb.quantile
  wk.stock.rec.quantile3<-wk.stock.rec.quantile
  }

}


png('report\\Fig_56_plot_SSB_Rec_Z.png', width=2000, height=700,res=200)
par(mfrow=c(1,3))
plot(wk.y1:wk.y2, wk.stock.ssb.quantile2[,3], type = "n", lty = 1,bty="L",
     xlab = "Year", ylab = "SSB", 
     ylim = c(min(0, wk.stock.ssb.quantile2), max(wk.stock.ssb.quantile2)))

lines(wk.y1:wk.y2, wk.stock.ssb.quantile1[,3], lty = 1, col = "darkgrey", lwd = 2)
lines(wk.y1:wk.y2, wk.stock.ssb.quantile2[,3], lty = 1, col = "black", lwd = 2)
lines(wk.y1:wk.y2, wk.stock.ssb.quantile3[,3], lty = 1, col = "green", lwd = 2)

plot(wk.y1:wk.y2, wk.stock.rec.quantile2[,3], type = "n", lty = 1,bty="L",
     xlab = "Year", ylab = "Recruitment", 
     ylim = c(min(0, wk.stock.rec.quantile2), max(wk.stock.rec.quantile2[,4])))

lines(wk.y1:wk.y2, wk.stock.rec.quantile1[,3], lty = 1, col = "darkgrey", lwd = 2)
lines(wk.y1:wk.y2, wk.stock.rec.quantile2[,3], lty = 1, col = "black", lwd = 2)
lines(wk.y1:wk.y2, wk.stock.rec.quantile3[,3], lty = 1, col = "green", lwd = 2)


plot(wk.y1:wk.y2, wk.stock.meanz.quantile2[,3], type = "n", lty = 1,bty="L",
     xlab = "Year", ylab = "Mean Z", 
     ylim = c(min(0, wk.stock.meanz.quantile2), max(wk.stock.meanz.quantile2)))

lines(wk.y1:wk.y2, wk.stock.meanz.quantile1[,3], lty = 1, col = "darkgrey", lwd = 2)
lines(wk.y1:wk.y2, wk.stock.meanz.quantile2[,3], lty = 1, col = "black", lwd = 2)
lines(wk.y1:wk.y2, wk.stock.meanz.quantile3[,3], lty = 1, col = "green", lwd = 2)

legend(legend = c("North", "South","combined"), x = "topright",
       lty = c(1,1,1), lwd = c(2,2,2), bty = "n", col = c("darkgrey", "black", "green"))


dev.off()




png('report\\Fig_plot_TSB.png', width=1200, height=1000,res=200)
plot(wk.y1:wk.y2, wk.stock.tsb.quantile2[,3], type = "n", lty = 1,bty="L",
     xlab = "Year", ylab = "TSB", 
     ylim = c(min(0, wk.stock.tsb.quantile2), max(wk.stock.tsb.quantile2)))

lines(wk.y1:wk.y2, wk.stock.tsb.quantile1[,3], lty = 1, col = "darkgrey", lwd = 2)
lines(wk.y1:wk.y2, wk.stock.tsb.quantile2[,3], lty = 1, col = "black", lwd = 2)
lines(wk.y1:wk.y2, wk.stock.tsb.quantile3[,3], lty = 1, col = "green", lwd = 2)

legend(legend = c("North", "South","combined"), x = "topleft",
       lty = c(1,1,1), lwd = c(2,2,2), bty = "n", col = c("darkgrey", "black","green"))
dev.off()



#######################################################################################################


rm(list=ls())
graphics.off()

library(icesSAG)

#remotes::install_github("ices-tools-dev/mixfishtools")
library(FLCore)
library(FLasher)
library(mixfishtools)
library(tidyr)
library(kableExtra)

cutage<-8
load("data/data.Rdata")

source("report_mixfish.R")

graphics.off()
source("report_ACOMplots.R")







