
years <- 1978:year
meanFages <- c(2:5)
ages <- 0:cutage
savePlots<-TRUE
### Last year's model ----------------------------------------------------------

load("model/model_old.Rdata") 

## Create FLStock object

# Get stk using SAM2FLStock so that +group is computed correctly
stk <- FLfse::SAM2FLStock(fit)

# Set units
units(stk)[1:17]    <- as.list(c(rep(c("tonnes","thousands","kg"),4), rep("NA",2),"f",rep("NA",2)))

# Mean F range
range(stk)[c("minfbar","maxfbar")]    <- c(min(meanFages), max(meanFages))

# Last age a plusgroup (should be done already)
stk  <- setPlusGroup(stk,stk@range["max"])

# Update stock and fisheries from SAM fit
stock.n(stk)[] <- exp(fit$pl$logN)
# tmp <- t(read.ices("sw.dat")); dms <- list(intersect(ac(ages),dimnames(tmp)[[1]]),intersect(years,dimnames(tmp)[[2]]))
# stock.wt(stk)[] <- tmp[dms[[1]],dms[[2]]]; rm(tmp,dms)
stock.wt(stk)[is.na(stock.wt(stk))] <- 0.001  # Replace NA weights with something low
stock.wt(stk)[stock.wt(stk)==0] <- 0.001  # Replace 0 weights with something low
stock(stk)[] <- computeStock(stk)
# harvest is unique to the set code (i.e. depends on config)
# check conf. file for which F states are estimated
Fstates <- fit$conf$keyLogFsta[1,]
Fstates_start <- which(Fstates==0)
Fstates_end   <- which(Fstates==max(Fstates))
harvest(stk)[Fstates_start:min(Fstates_end),] <- exp(fit$pl$logF)
harvest(stk)[Fstates_end[-1],] <- harvest(stk)[Fstates_end[1],]
harvest(stk)[Fstates==-1,] <- 0

## Selectivity

if(min(ages)==0) {meanFages_ <- meanFages+1} else {meanFages_ <- meanFages}
meanF <- apply(harvest(stk)[meanFages_,],2, "mean")
meanF_old <- meanF[length(meanF)]
maxF <- apply(harvest(stk)[meanFages_,],2, "max")
maxF_old <- maxF[length(maxF)]
sel <- sweep(harvest(stk),2,meanF,"/")
sel1 <- sweep(harvest(stk),2,maxF,"/")

stk_old <- stk
sel_old <- sel
sel_old_max <- sel1

### This year's model ----------------------------------------------------------

load("model/whg.27.47d_FLStock_model_estimates.Rdata")
## Create FLStock object
stk<-stk_fit
load("model/model.Rdata")

# Set units
units(stk)[1:17]    <- as.list(c(rep(c("tonnes","thousands","kg"),4), rep("NA",2),"f",rep("NA",2)))

# Mean F range
range(stk)[c("minfbar","maxfbar")]    <- c(min(meanFages), max(meanFages))

# Last age a plusgroup (should be done already)
stk  <- setPlusGroup(stk,stk@range["max"])

# Update stock and fisheries from SAM fit
stock.n(stk)[] <- exp(fit$pl$logN)
# tmp <- t(read.ices("sw.dat")); dms <- list(intersect(ac(ages),dimnames(tmp)[[1]]),intersect(years,dimnames(tmp)[[2]]))
# stock.wt(stk)[] <- tmp[dms[[1]],dms[[2]]]; rm(tmp,dms)
stock.wt(stk)[is.na(stock.wt(stk))] <- 0.001  # Replace NA weights with something low
stock.wt(stk)[stock.wt(stk)==0] <- 0.001  # Replace 0 weights with something low
stock(stk)[] <- computeStock(stk)
# harvest is unique to the set code (i.e. depends on config)
# check conf. file for which F states are estimated
Fstates <- fit$conf$keyLogFsta[1,]
Fstates_start <- which(Fstates==0)
Fstates_end   <- which(Fstates==max(Fstates))
harvest(stk)[Fstates_start:min(Fstates_end),] <- exp(fit$pl$logF)
harvest(stk)[Fstates_end[-1],] <- harvest(stk)[Fstates_end[1],]
harvest(stk)[Fstates==-1,] <- 0

## Selectivity

if(min(ages)==0) {meanFages_ <- meanFages+1} else {meanFages_ <- meanFages}
meanF <- apply(harvest(stk)[meanFages_,],2, "mean")
meanF_new <- meanF[length(meanF)]
maxF <- apply(harvest(stk)[meanFages_,],2, "max")
maxF_new <- maxF[length(maxF)]
sel <- sweep(harvest(stk),2,meanF,"/")
sel1 <- sweep(harvest(stk),2,maxF,"/")

stk_new <- stk
sel_new <- sel
sel_new_max <- sel1

rm(stk,sel,meanF)

### Plot to compare forecast F

if (savePlots) x11() else windows()
# F in this year
plot(ages,harvest(stk_new)[,ac(max(years)-1)], type="l", ylim=c(0,0.25), xlab="Age", ylab="F", main="", lwd=2)
# F in last year
lines(0:6,harvest(stk_old)[,ac(max(years)-2)], col=2, lwd=2)
# Forecast F in this year
lines(ages,apply(sel_new[,ac((datayear-2):datayear)],1,mean)*meanF_new, col=1, lwd=5)
# Forecast F in last year
lines(0:6,apply(sel_old[,ac((datayear-2):datayear-1)],1,mean)*meanF_old, col=2, lwd=5)

legend("topleft", legend=c(paste0("F ",datayear),paste0("WGNSSK ", datayear+1," forecast F "),paste0("F ",datayear-1),paste0("WGNSSK ",datayear," forecast F")), lwd=c(2,5,2,5), col=c(1,1,2,2), bty="n")

if (savePlots) savePlot("report/ACOMplots/Fig_47_F comparison.png",type="png")
if (savePlots) dev.off()

### Plot to compare selectivities

if (savePlots) x11() else windows()
# Selectivity this year
plot(ages,apply(sel_new_max[,ac((max(years)-3):(max(years)-1))],1,mean), type="l", xlab="Age", ylab="Scaled F", main="", lwd=2)

# Selectivity last year
lines(0:6,apply(sel_old_max[,ac((max(years)-4):(max(years)-2))],1,mean), col=2, lwd=2)

legend("topleft", legend=c(paste0("Selectivity WGNSSK ",datayear+1," forecast"),paste0("Selectivity WGNSSK ",datayear," forecast")), lwd=c(2,2), col=c(1,2), bty="n")

if (savePlots) savePlot("report/ACOMplots/Fig_48_Selectivity comparison.png",type="png")
if (savePlots) dev.off()

### Plot to stock weights upto age 6+

if (savePlots) x11() else windows()
# sw in new
plot(0:6,stock.wt(stk_new)[1:7,ac(max(years)-1)], type="l", xlab="Age", ylab="Weight (kg)", main="Stock weight-at-age", lwd=2, ylim=c(0,0.6))
# sw in old
lines(0:6,stock.wt(stk_old)[,ac(max(years)-2)], col=2, lwd=2)
# Forecast F in new
lines(0:6,apply(stock.wt(stk_new)[1:7,ac((datayear-2):datayear)],1,mean), col=1, lwd=5)
# Forecast F in old
lines(0:6,apply(stock.wt(stk_old)[,ac((datayear-3):datayear-1)],1,mean), col=2, lwd=5)

legend("topleft", legend=c(paste0("Stock weights ",ac(max(years)-1)),paste0("WGNSSK ",datayear+1," forecast stock weights"),paste0("Stock weights ",ac(max(years)-2)),paste0("WGNSSK ",datayear," forecast stock weights")), lwd=c(2,5,2,5), col=c(1,1,2,2), bty="n")

if (savePlots) savePlot("report/ACOMplots/Fig_46_SW comparison.png",type="png")
if (savePlots) dev.off()

### Plot biomass accumulation

nn1 <- stock.n(stk_old) # Get numbers by age
nn1 <- flr2taf(nn1)
write.csv(nn1, paste0("report/ACOMplots/Natage_old.csv"))

nn <- stock.n(stk_new) # Get biomass by age
nn <- flr2taf(nn) # Convert FLQuant to dataframe
write.csv(nn, paste0("report/ACOMplots/Natage_new.csv"))

bb1 <- stock.n(stk_old)*stock.wt(stk_old) # Get biomass by age
bb1 <- flr2taf(bb1)
write.csv(bb1, paste0("report/ACOMplots/Batage_old.csv"))

bb <- stock.n(stk_new)*stock.wt(stk_new) # Get biomass by age
bb <- flr2taf(bb) # Convert FLQuant to dataframe
write.csv(bb, paste0("report/ACOMplots/Batage_new.csv"))



ssb1 <- stock.n(stk_old)*stock.wt(stk_old)*mat(stk_old) # Get biomass by age
ssb1 <- flr2taf(ssb1)
write.csv(ssb1, paste0("report/ACOMplots/SSBatage_old.csv"))

ssb <- stock.n(stk_new)*stock.wt(stk_new)*mat(stk_new) # Get biomass by age
ssb <- flr2taf(ssb) # Convert FLQuant to dataframe
write.csv(ssb, paste0("report/ACOMplots/SSBatage_new.csv"))


# plot new time series
rownames(bb) <- bb[,1]; bb <- bb[,-1] # Format
bb_percentage <- apply(bb, 1, function(x){x*100/sum(x,na.rm=T)}) # Get % biomass at age

rownames(ssb) <- ssb[,1]; ssb <- ssb[,-1] # Format
ssb_percentage <- apply(ssb, 1, function(x){x*100/sum(x,na.rm=T)}) # Get % biomass at age


coul <- brewer.pal(9, "Paired") # Choose colour palette

# Plot biomass at age
if (savePlots) x11() else windows()
par(mar=c(3, 3, 3, 5))
barplot(t(bb), col = coul, legend.text = TRUE, args.legend = list(x = "topright", bty = "n", inset = c(- 0.15, 0)), main = "Biomass at age")
if (savePlots) savePlot("report/ACOMplots/Biomass at age",type="png")
if (savePlots) dev.off()

# Plot % biomass at age
if (savePlots) x11() else windows()
par(mar=c(3, 3, 3, 5))
barplot(bb_percentage, col = coul, legend.text = TRUE, args.legend = list(x = "topright", bty = "n", inset = c(- 0.15, 0)), main = "Proportion biomass at age")
if (savePlots) savePlot("report/ACOMplots/Proportion biomass at age",type="png")
if (savePlots) dev.off()

# Plot biomass at age
if (savePlots) x11() else windows()
par(mar=c(3, 3, 3, 5))
barplot(t(ssb), col = coul, legend.text = TRUE, args.legend = list(x = "topright", bty = "n", inset = c(- 0.15, 0)), main = "Spawning Stock Biomass at age")
if (savePlots) savePlot("report/ACOMplots/Spawning Stock Biomass at age",type="png")
if (savePlots) dev.off()

# Plot % biomass at age
if (savePlots) x11() else windows()
par(mar=c(3, 3, 3, 5))
barplot(ssb_percentage, col = coul, legend.text = TRUE, args.legend = list(x = "topright", bty = "n", inset = c(- 0.15, 0)), main = "Proportion Spawning Stock Biomass at age")
if (savePlots) savePlot("report/ACOMplots/Proportion Spawning Stock Biomass at age",type="png")
if (savePlots) dev.off()


#Change in advice
####################################################################################

graphics.off()

load("data/data.Rdata")

load("model/forecast_new.Rdata")
load("model/model.Rdata")
new_assess<-fit
load("model/model_old.Rdata")
old_assess<-fit

ay <- year

# correct Rec from WGNSSK ay to account for geometric mean
Ry <- 1983:(ay-2)
R <- rectable(new_assess)[,1]
R <- R[as.character(1978:(ay-1))]
R_geoMean <- exp(mean(log(R[ac(Ry)]))) # better summary stat for tables and plots when length(Ry) is even


# this year
tab1 <- data.frame(WG =paste0("WGNSSK ",ay),Variable= sort(rep(c("SSB","Recruitment","Fbar","Total catch"),3)), 
                   Year = rep((ay-1):(ay+1),4), Type=NA, Value= NA, Source = "Forecast")

tab1$Type[tab1$Year %in% (ay-1)] <- "Data year"
tab1$Type[tab1$Year %in% (ay)] <- "Intermediate year"
tab1$Type[tab1$Year %in% (ay+1)] <- "Advice year"

tab1$Source[tab1$Year %in% (ay-1)] <- "Assessment"

astab <- as.data.frame(summary(new_assess))
astab$Year <- rownames(astab)
tmp_tab <- catchtable(new_assess)
fctab <- attr(FC3,"tab")

# data year values
idx <- which(astab$Year %in% c(ay-1)) # data yr
tab1$Value[tab1$Year %in% (ay-1) & tab1$Variable %in% "Recruitment"] <- fctab[as.character(ay-1),"rec:median"] # data year
tab1$Value[tab1$Year %in% (ay-1) & tab1$Variable %in% "SSB"] <- fctab[as.character(ay-1),"ssb:median"] # data year
tab1$Value[tab1$Year %in% (ay-1) & tab1$Variable %in% "Fbar"] <- fctab[as.character(ay-1),"fbar:median"] # data year
tab1$Value[tab1$Year %in% (ay-1) & tab1$Variable %in% "Total catch"] <- fctab[as.character(ay-1),"catch:median"] # data year

# int year values
tab1$Value[tab1$Year %in% (ay) & tab1$Variable %in% "SSB"] <- fctab[as.character(ay),"ssb:median"]
tab1$Value[tab1$Year %in% (ay) & tab1$Variable %in% "Fbar"] <- fctab[as.character(ay),"fbar:median"]
tab1$Value[tab1$Year %in% (ay) & tab1$Variable %in% "Total catch"] <- fctab[as.character(ay),"catch:median"]
tab1$Value[tab1$Year %in% (ay) & tab1$Variable %in% "Recruitment"] <- fctab[as.character(ay),"rec:median"]

# advice year values
tab1$Value[tab1$Year %in% (ay+1) & tab1$Variable %in% "SSB"] <- fctab[as.character(ay+1),"ssb:median"]
tab1$Value[tab1$Year %in% (ay+1) & tab1$Variable %in% "Fbar"] <- fctab[as.character(ay+1),"fbar:median"]
tab1$Value[tab1$Year %in% (ay+1) & tab1$Variable %in% "Total catch"] <- fctab[as.character(ay+1),"catch:median"]
tab1$Value[tab1$Year %in% (ay+1) & tab1$Variable %in% "Recruitment"] <- fctab[as.character(ay+1),"rec:median"]


tab1$Value[tab1$WG %in% paste0("WGNSSK ",ay) & tab1$Variable %in% "Recruitment" & !tab1$Source %in% "Assessment"] <- R_geoMean

# last years values from MFDP

dat <- read.csv("bootstrap/data/Compare_forecast_assumptions.csv")

astab <- as.data.frame(summary(old_assess))
astab$Year <- rownames(astab)
tmp_tab <- catchtable(old_assess)
tab2 <- data.frame(WG =paste0("WGNSSK ",ay-1),Variable= sort(rep(c("SSB","Recruitment","Fbar","Total catch"),3)), 
                   Year = rep((ay-2):(ay),4), Type=NA, Value= NA, Source = "Forecast")

tab2$Type[tab2$Year %in% (ay-2)] <- "Data year"
tab2$Type[tab2$Year %in% (ay-1)] <- "Intermediate year"
tab2$Type[tab2$Year %in% (ay)] <- "Advice year"

tab2$Source[tab2$Year %in% (ay-2)] <- "Assessment"

astab <- getSAG(stock = "whg.27.47d",year=(ay-1),purpose="Advice")


# data year values
idx <- which(astab$Year %in% c(ay-2)) # data yr
tab2$Value[tab2$Year %in% (ay-2) & tab2$Variable %in% "Recruitment"] <- astab$recruitment[idx] # data year
tab2$Value[tab2$Year %in% (ay-2) & tab2$Variable %in% "SSB"] <- astab$SSB[idx] # data year
tab2$Value[tab2$Year %in% (ay-2) & tab2$Variable %in% "Fbar"] <- astab$"F"[idx] # data year
tab2$Value[tab2$Year %in% (ay-2) & tab2$Variable %in% "Total catch"] <- astab$catches[idx] # data year

# int year values
tab2$Value[tab2$Year %in% (ay-1) & tab2$Variable %in% "SSB"] <-  dat[dat$WG%in% paste("WGNSSK", datayear) & dat$Year %in% (ay-1) & dat$Variable %in% "SSB" , "Value"]
tab2$Value[tab2$Year %in% (ay-1) & tab2$Variable %in% "Fbar"] <- dat[dat$WG%in% paste("WGNSSK", datayear) & dat$Year %in% (ay-1) & dat$Variable %in% "Fbar" , "Value"]
tab2$Value[tab2$Year %in% (ay-1) & tab2$Variable %in% "Total catch"] <- dat[dat$WG%in% paste("WGNSSK", datayear) & dat$Year %in% (ay-1) & dat$Variable %in% "Total catch" , "Value"]
tab2$Value[tab2$Year %in% (ay-1) & tab2$Variable %in% "Recruitment"] <- dat[dat$WG%in% paste("WGNSSK", datayear) & dat$Year %in% (ay-1) & dat$Variable %in% "Recruitment" , "Value"]

# advice year values
tab2$Value[tab2$Year %in% (ay) & tab2$Variable %in% "SSB"] <-  dat[dat$WG%in% paste("WGNSSK", datayear)& dat$Year %in% (ay) & dat$Variable %in% "SSB" , "Value"]
tab2$Value[tab2$Year %in% (ay) & tab2$Variable %in% "Fbar"] <- dat[dat$WG%in% paste("WGNSSK", datayear)& dat$Year %in% (ay) & dat$Variable %in% "Fbar" , "Value"]
tab2$Value[tab2$Year %in% (ay) & tab2$Variable %in% "Total catch"] <-  dat[dat$WG%in% paste("WGNSSK", datayear) & dat$Year %in% (ay) & dat$Variable %in% "Total catch" , "Value"]
tab2$Value[tab2$Year %in% (ay) & tab2$Variable %in% "Recruitment"] <- dat[dat$WG%in% paste("WGNSSK", datayear)& dat$Year %in% (ay) & dat$Variable %in% "Recruitment" , "Value"]


tab <- rbind(tab1,tab2)

write.csv(tab, "report/ACOMplots/Compare_forecast_assumptions.csv",row.names=F)

# compare forecast assumptions----------------------------------------------####

# read in data
dat <-tab
dat <- dat[dat$Year >(ay-2),]
dat$Type <- factor(dat$Type,levels=c("Data year","Intermediate year","Advice year"))
dat$Variable <- factor(dat$Variable,levels=c("SSB","Fbar","Total catch","Recruitment"))

png("report/ACOMplots/Fig_49_Compare_forecast_assumptions.png",width = 11, height = 7, units = "in", res = 600)

ggplot(dat,aes(x=Year,y=Value,colour=WG,shape=Type))+geom_point(size=3)+facet_wrap(~Variable,scales="free_y")+
  theme_bw()+scale_shape_manual(values=c(16, 2, 0))+ labs(x="Year",y="",colour="",shape="")

dev.off()



# N at age compared to old forecast ------------------------------------------
dat <- read.csv("bootstrap/data/N_at_age_dat.csv")


# get forecast n at age tables

frcst.Fmsy <- FC3 
label <-attr(frcst.Fmsy,"label")

ft <- attr(FC3, "fit")
N <- ntable(ft)
Nfc <-as.data.frame(do.call(rbind, lapply(frcst.Fmsy , function(x)exp(colMeans(x$sim[,1:ncol(N)])))))
rownames(Nfc) <- lapply(frcst.Fmsy, function(x)x$year)
colnames(Nfc) <- colnames(N)
Nfc$Year<-rownames(Nfc)
Nfc[rownames(Nfc)%in%c(ay:(ay+2)),1]<-R_geoMean

natage_fc <- attr(Nfc,"naytable")
n_new <- as.data.frame(nn)
n_new <- n_new[n_new$Year< (ay-1),]

nums <- rbind(n_new,Nfc)
nums_new<-nums
write.csv(nums_new,file=paste0("report/ACOMplots/","Combined_N_at_age_new.csv"),row.names=F)

nums <- reshape2::melt(nums,id.vars="Year")
colnames(nums) <- c("Year","Age","N")

dat0 <- nums[nums$Year == (ay-2),]
dat0$WG <- paste0("WGNSSK ",ay)
dat0$Type="Data"
dat1 <- nums[nums$Year == (ay-1),]
dat1$WG <- paste0("WGNSSK ",ay)
dat1$Type="Data"
dat2 <- dat[dat$Year == (ay-2) & (dat$WG==paste0("WGNSSK ",ay-1)) & dat$Type=="Data",]

dat3 <- nums[nums$Year == (ay),]
dat3$WG <- paste0("WGNSSK ",ay)
dat3$Type="Intermediate year"
dat4 <- dat[dat$Year == (ay-1) & (dat$WG==paste0("WGNSSK ",ay-1)) & dat$Type=="Intermediate year",]

dat5 <- nums[nums$Year == (ay+1),]
dat5$WG <- paste0("WGNSSK ",ay)
dat5$Type="Advice year"
dat6 <- dat[dat$Year == (ay) & (dat$WG==paste0("WGNSSK ",ay-1)) & dat$Type=="Advice year",]


datall <- rbind(dat0,dat1,dat2,dat3,dat4,dat5,dat6)
colnames(datall) <- c("Year","Age","N","WG","Type")
datall$Type <- factor(datall$Type,levels=c("Data","Intermediate year","Advice year"))


write.csv(datall,file=paste0("report/ACOMplots/","Forecast_N_at_age.csv"),row.names=F)

png(paste0("report/ACOMplots/Fig_44_Compare inputs N_at_age.png"),width = 11, height = 7, units = "in", res = 600)

ggplot(datall,aes(x=Age,y=N,group=interaction(Type,WG),colour=WG,shape=Type))+ geom_line()+geom_point(size=3)+labs(colour="",y="Numbers (thousands)",shape="")+
  facet_wrap(~Year,nrow=2)+theme_bw()+scale_shape_manual(values=c(16, 2, 0))

dev.off()


png(paste0("report/ACOMplots/Compare inputs N_at_age v2.png"),width = 11, height = 7, units = "in", res = 600)

p1 <- ggplot(datall,aes(x=Age,y=N,group=interaction(Year,WG),colour=WG,shape=as.factor(Year)))+ geom_line()+geom_point(size=3)+labs(colour="",y="Numbers (thousands)",shape="")+
  facet_wrap(~Type)+theme_bw()+scale_shape_manual(values=c(16, 2, 0,3))

p2 <- ggplot(datall,aes(x=Age,y=N,group=interaction(Type,WG),colour=WG,shape=Type))+ geom_line()+geom_point(size=3)+labs(colour="",y="Numbers",shape="")+
  facet_wrap(~Year,nrow=1)+theme_bw()+scale_shape_manual(values=c( 16, 2, 0))

plot_grid(p1,p2,nrow=2)

dev.off()

# compare N at age table ----------------------------------
#last years
dat<-read.csv("bootstrap/data/N_at_age_dat.csv")

# last year's assessment and forecast

dat_old<-dat[dat$WG==paste("WGNSSK",ay-1) & dat$Year%in%c((ay-2):(ay)),]
natage_old <- as.data.frame(cbind((ay-1):(ay),rbind(dat_old[dat_old$Year==ay-1,"N"],dat_old[dat_old$Year==ay,"N"])))
colnames(natage_old)<-c("Year",0:6)

n_old <- rbind(nn1[nn1$Year<(ay-1),],natage_old)
row.names(n_old) <- n_old$Year

# save results
write.csv(n_old,file=paste0("report/ACOMplots/","Combined_Natage_old.csv"),row.names=F)


# biomass at age compared to previous forecast -----------------------------------------

dat <- read.csv("bootstrap/data/B_at_age_dat.csv")
wtatage_new <- new_assess$data$stockMeanWeight[as.character(ay),]
wt_new <- new_assess$data$stockMeanWeight[as.character(1978:ay),]

b_new <- nn[,-1]*wt_new
b_new$Year<-c(1978:ay)
Bfc<-Nfc[,1:9]*rbind(wt_new[as.character(year-1),],wtatage_new,wtatage_new,wtatage_new)
Bfc$Year<-c((ay-1):(ay+2))
b_nums <- rbind(b_new[b_new$Year<(ay-1),],Bfc)

b_new<-b_nums
write.csv(b_new,file=paste0("report/ACOMplots/","Combined_B_at_age_new.csv"),row.names=F)

b_nums <- reshape2::melt(b_nums,id.vars="Year")
colnames(b_nums) <- c("Year","Age","B")

dat0 <- b_nums[b_nums$Year == (ay-2),]
dat0$WG <- paste0("WGNSSK ",ay)
dat0$Type="Data"
dat1 <- b_nums[b_nums$Year == (ay-1),]
dat1$WG <- paste0("WGNSSK ",ay)
dat1$Type="Data"
dat2 <- dat[dat$Year == (ay-2) & (dat$WG==paste0("WGNSSK ",ay-1)) & dat$Type=="Data",]

dat3 <- b_nums[b_nums$Year == (ay),]
dat3$WG <- paste0("WGNSSK ",ay)
dat3$Type="Intermediate year"
dat4 <- dat[dat$Year == (ay-1) & (dat$WG==paste0("WGNSSK ",ay-1)) & dat$Type=="Intermediate year",]

dat5 <- b_nums[b_nums$Year == (ay+1),] 
dat5$WG <- paste0("WGNSSK ",ay)
dat5$Type="Advice year"
dat6 <- dat[dat$Year == (ay) & (dat$WG==paste0("WGNSSK ",ay-1)) & dat$Type=="Advice year",]

datallB <- rbind(dat0,dat1,dat2,dat3,dat4,dat5,dat6)
colnames(datallB) <- c("Year","Age","B","WG","Type")
datallB$Type <- factor(datallB$Type,levels=c("Data","Intermediate year","Advice year"))

write.csv(datallB,file=paste0("report/ACOMplots/","Forecast_B_at_age.csv"),row.names=F)


png(paste0("report/ACOMplots/Fig_45_Compare inputs B_at_age.png"),width = 11, height = 7, units = "in", res = 600)

 ggplot(datallB,aes(x=Age,y=B,group=interaction(Type,WG),colour=WG,shape=Type))+ geom_line()+geom_point(size=3)+labs(colour="",y="Biomass (t)",shape="")+
  facet_wrap(~Year,nrow=2)+theme_bw()+scale_shape_manual(values=c(16, 2, 0))


dev.off()


png(paste0("report/ACOMplots/Compare inputs B_at_age_v2.png"),width = 11, height = 7, units = "in", res = 600)

p1 <- ggplot(datallB,aes(x=Age,y=B,group=interaction(Year,WG),colour=WG,shape=as.factor(Year)))+ geom_line()+geom_point(size=3)+labs(colour="",y="Biomass (t)",shape="")+
  facet_wrap(~Type)+theme_bw()+scale_shape_manual(values=c(16, 2, 0,3))

p2 <- ggplot(datallB,aes(x=Age,y=B,group=interaction(Type,WG),colour=WG,shape=Type))+ geom_line()+geom_point(size=3)+labs(colour="",y="Biomass (t)",shape="")+
  facet_wrap(~Year,nrow=1)+theme_bw()+scale_shape_manual(values=c( 16, 2, 0))

plot_grid(p1,p2,nrow=2)

dev.off()

# compare B at age table ----------------------------------

dat<-read.csv("bootstrap/data/B_at_age_dat.csv")

# last year's assessment and forecast

dat_old<-dat[dat$WG==paste("WGNSSK",ay-1) & dat$Year%in%c((ay-2):(ay)),]
batage_old <- as.data.frame(cbind((ay-1):(ay),rbind(dat_old[dat_old$Year==ay-1,"B"],dat_old[dat_old$Year==ay,"B"])))
colnames(batage_old)<-c("Year",0:6)


b_old <- rbind(bb1[bb1$Year<(ay-1),],batage_old)
row.names(b_old) <- b_old$Year

# save results
write.csv(b_old,file=paste0("report/ACOMplots/","Combined_Batage_old.csv"),row.names=F)


####################################################

# n at age change table
dat.new <- nums_new
dat.old <- n_old
#colnames(dat.old) <- colnames(dat.new) <- c("Year",ages[1:7])


# Find ratio
comp.yrs <- intersect(dat.new$Year,dat.old$Year)
rat.n <- dat.new[dat.new$Year %in% comp.yrs,2:7] / dat.old[dat.old$Year %in% comp.yrs,2:7]
rat.n$Year <- comp.yrs

rat.n<-rat.n[rat.n$Year>2015,]

dat2tab <- round(rat.n[,as.character(0:5)],3) # to be adapted if 8 age groups
row.names(dat2tab) <- rat.n$Year

dat2tab<-t(dat2tab)

write.csv(dat2tab,file="report/ACOMplots/Table_33_Numbers_changetable.csv")



# B at age change table
dat.new <- b_new
dat.old <- b_old


# Find ratio
comp.yrs <- intersect(dat.new$Year,dat.old$Year)
rat.b <- dat.new[dat.new$Year %in% comp.yrs,as.character(0:5)] / dat.old[dat.old$Year %in% comp.yrs,as.character(0:5)]
rat.b$Year <- comp.yrs
rat.b<-rat.b[rat.b$Year>2015,]

dat2tab <- round(rat.b[,as.character(0:5)],3) # to be adapted if 8 age groups
row.names(dat2tab) <- rat.b$Year

dat2tab<-t(dat2tab)

write.csv(dat2tab,file="report/ACOMplots/Table_34_B_changetable.csv")

