
############################### compare SAM and SURBAR ###############################
#old run
load("bootstrap/data/surbar_results1_BM.Rdata")  # startage 1 SURBAR results

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

wk.stock.meanz.old<-wk.stock.meanz 
wk.stock.meanz.quantile.old<-wk.stock.meanz.quantile
wk.stock.meanz.mean.old<- wk.stock.meanz.mean
wk.stock.ssb.quantile.old<-wk.stock.ssb.quantile
wk.stock.ssb.mean.old<-wk.stock.ssb.mean
wk.stock.ssb.old<-wk.stock.ssb
wk.stock.rec.old<-wk.stock.rec
wk.stock.rec.quantile.old<-wk.stock.rec.quantile
wk.stock.rec.mean_old<-wk.stock.rec.mean
s.results.old<-s.results

#new run
load("model/surbar_results1.Rdata")  # run from new WGNSSK 

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


tab12<-summary(fit)
tabN<-ntable(fit)

natmor<- rowMeans(exp(fit$pl$logNM)[c(1:(nrow(fit$pl$logNM)-10)),3:6])
fmor<- rowMeans(faytable(fit)[,3:6])


png('report\\Fig_38_plot_SAM_SURBAR.png', width=2000, height=700,res=200)
par(mfrow=c(1,3))
plot(wk.y1:wk.y2, wk.stock.ssb.quantile[,3], type = "n", lty = 1,bty="L",
     xlab = "Year", ylab = "SSB", xlim=c(1978,year),
     ylim = c(min(0, wk.stock.ssb.quantile), max(wk.stock.ssb.quantile[,3],tab12[,4]/mean(tab12[,4])))
)
lines(wk.y1:wk.y2, wk.stock.ssb.quantile[,3], lty = 1, col = "darkorange", lwd = 2)
lines(rownames(tab12),tab12[,4]/mean(tab12[,4]), col="black", lwd=2)


plot(wk.y1:wk.y2, wk.stock.rec.quantile[,3], type = "n", lty = 1,bty="L",
     xlab = "Year", ylab = "Recruitment (age 1)", xlim=c(1978,year),
     ylim = c(min(0, wk.stock.rec.quantile), max(wk.stock.rec.quantile[,4])))

lines(wk.y1:wk.y2, wk.stock.rec.quantile[,3], lty = 1, col = "darkorange", lwd = 2)
lines(rownames(tabN),tabN[,2]/mean(tabN[,2]), col="black", lwd=2)


plot(wk.y1:wk.y2, wk.stock.meanz.quantile[,3], type = "n", lty = 1,bty="L",
     xlab = "Year", ylab = "Mortality Z", xlim=c(1978,year),
     ylim = c(min(0, wk.stock.meanz.quantile), max(wk.stock.meanz.quantile[,4])))

lines(wk.y1:wk.y2, wk.stock.meanz.quantile[,3], lty = 1, col = "darkorange", lwd = 2)
lines(rownames(tab12),fmor+natmor, col="black", lwd=2)

legend(legend = c("SAM Z(2-5)","SURBAR Z(2-4)"), x = "topright",
       lty = c(1,1), lwd = c(2,2), bty = "n", col = c("black","darkorange"))

dev.off()

#compare SURBAR SSB
png('report\\Fig_24_plot_SURBAR_BM_new.png', width=2000, height=700,res=200)
par(mfrow=c(1,3))
plot(wk.y1:wk.y2, wk.stock.ssb.quantile[,3], type = "n", lty = 1,bty="L",
     xlab = "Year", ylab = "SSB", xlim=c(1978,year),
     ylim = c(min(0, wk.stock.ssb.quantile), max(wk.stock.ssb.quantile[,3],tab12[,4]/mean(tab12[,4])))
)
lines(wk.y1:(wk.y2), wk.stock.ssb.quantile[,3], lty = 1, col = "darkorange", lwd = 2)
lines(wk.y1:(wk.y2-1), wk.stock.ssb.quantile.old[,3], lty = 1, col = "royalblue", lwd = 2)

plot(wk.y1:wk.y2, wk.stock.rec.quantile[,3], type = "n", lty = 1,bty="L",
     xlab = "Year", ylab = "Recruitment", xlim=c(1978,year),
     ylim = c(min(0, wk.stock.rec.quantile), max(wk.stock.rec.quantile[,4])))

lines(wk.y1:(wk.y2), wk.stock.rec.quantile[,3], lty = 1, col = "darkorange", lwd = 2)
lines(wk.y1:(wk.y2-1), wk.stock.rec.quantile.old[,3], lty = 1, col = "royalblue", lwd = 2)


plot(wk.y1:wk.y2, wk.stock.meanz.quantile[,3], type = "n", lty = 1,bty="L",
     xlab = "Year", ylab = "Mortality Z(age 2-4)", xlim=c(1978,year),
     ylim = c(min(0, wk.stock.meanz.quantile), max(wk.stock.meanz.quantile[,4])))

lines(wk.y1:wk.y2, wk.stock.meanz.quantile[,3], lty = 1, col = "darkorange", lwd = 2)
lines(wk.y1:(wk.y2-1), wk.stock.meanz.quantile.old[,3], lty = 1, col = "royalblue", lwd = 2)


legend(legend = c("SURBAR","SURBAR BM"), x = "topright",
       lty = c(1,1), lwd = c(2,2), bty = "n", col = c("darkorange","royalblue"))

dev.off()

##########################################################################################
#compare assessments


load("model/model_old.Rdata")
fit_before<-fit
load("model/model_BM.Rdata")
fit_BM<-fit
load("model/model.Rdata")


tab<-summary(fit)
tab_before<-summary(fit_before)
tab_BM<-summary(fit_BM)


ctab<-catchtable(fit)
ctab_before<-catchtable(fit_before)
ctab_BM<-catchtable(fit_BM)


png('report\\Fig_39_plot_SAM compare.png', width=1500, height=1500,res=200)

par(mfrow=c(2,2))
plot(rownames(ctab_before),ctab_before[,1],  col="black",type = "l", lwd=1,lty = "dotted",bty="L",
     xlab = "Year", ylab = "Modelled catch (t)", xlim=c(1978,year), ylim=c(0,1.1*max(ctab[,1])))
lines(rownames(ctab),ctab[,1], col="orange", lwd=3, lty=1)
lines(rownames(ctab_BM),ctab_BM[,1], col="royalblue", lwd=3, lty="dotted")

legend(legend = c("WGNSSK old","Benchmark","WGNSSK new"),x="topright",
       lty = c(3,3,1), lwd = c(1,3,3), cex=0.9,bty = "n", col = c("black","royalblue","orange"))

plot(rownames(tab_before)[1:nrow(tab_before)-1],tab_before[1:nrow(tab_before)-1,1],  col="black",type = "l", lwd=1,lty = "dotted",bty="L",
     xlab = "Year", ylab = "Rec (age 0)", xlim=c(1978,year),ylim=c(0,1.1*max(tab[,1])))
lines(rownames(tab)[1:nrow(tab)-1],tab[1:nrow(tab)-1,1], col="orange", lwd=3, lty=1)
lines(rownames(tab_BM)[1:nrow(tab_BM)-1],tab_BM[1:nrow(tab_BM)-1,1], col="royalblue", lwd=3, lty="dotted")

plot(rownames(tab_before)[1:nrow(tab_before)-1],tab_before[1:nrow(tab_before)-1,7],  col="black",type = "l", lwd=1,lty = "dotted",bty="L",
     xlab = "Year", ylab = "F (2-5)", xlim=c(1978,year),ylim=c(0,1.1*max(tab[,7])))
lines(rownames(tab)[1:nrow(tab)-1],tab[1:nrow(tab)-1,7], col="orange", lwd=3, lty=1)
lines(rownames(tab_BM)[1:nrow(tab_BM)-1],tab_BM[1:nrow(tab_BM)-1,7], col="royalblue", lwd=3, lty="dotted")

plot(rownames(tab_before),tab_before[,4],  col="black",type = "l", lwd=1,lty = "dotted",bty="L",
     xlab = "Year", ylab = "SSB", xlim=c(1978,year), ylim=c(0,1.1*max(tab[,4])))
lines(rownames(tab),tab[,4], col="orange", lwd=3, lty=1)
lines(rownames(tab_BM),tab_BM[,4], col="royalblue", lwd=3, lty="dotted")

dev.off()


geom<-exp(mean(log(tab[26:(nrow(tab)-2),1])))/1000000
geom_all<-exp(mean(log(tab[6:(nrow(tab)-2),1])))/1000000

png('report\\Fig_plot_Rec_SAM geom mean.png', width=1200, height=1000,res=200)
plot(rownames(tab)[1:nrow(tab)-1],tab[1:nrow(tab)-1,1]/1000000,  col="black",type = "l", lwd=3,bty="L",
     xlab = "Year", ylab = "Rec (age 0, billions)", xlim=c(1978,year))
lines(c(1983:(year-2)),rep(geom_all,length(1983:(year-2))), col="royalblue", lwd=2, type="l",lty="dotted")

legend("top", legend=c(paste0("geom. mean 1983-", (year-2), ": ", round(geom_all*1000)," millions")),col=c("royalblue"), lwd=2,lty="dotted", bty="n")

dev.off()

