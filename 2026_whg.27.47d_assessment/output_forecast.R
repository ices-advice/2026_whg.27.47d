
# SAM forecast 
##############################################################################################


if(season=="spring") xy<-3
if(season=="autumn") xy<-2

load("model\\forecast_new.RData")

Blim <-144175

#status quo F at age
summary_F <-summary(fit)
f_est     <-summary_F[,7]
Fsq       <- f_est[length(f_est)-1]

# mean recent 3 years of F, scaled to final catch data year
fsel <- faytable(fit)/f_est
catch_mean<-apply(fsel[length(fsel[,1])+(-3:-1),],2,mean)*Fsq

F_past<-faytable(fit)[(length(summary(fit)[,1])-3):(length(summary(fit)[,1])-1),]
ForcF<-rbind(F_past, catch_mean)
write.taf(ForcF, file="output/recentF_forecastF.csv", row.names=T)

colours<- palette.colors(palette="Okabe_Ito")

taf.png("output/forecastF.png")
plot(0:8,ForcF[4,], type="l", ylim=c(0,max(ForcF)*1.2), col="transparent", xlab="Age", ylab="F", lwd=4)
for (i in 1:3) lines(0:8,ForcF[i,], col=colours[i+1], lwd=4)
lines(0:8,ForcF[4,], col=colours[1], lwd=4)
legend("topleft", legend=c((datayear-2):datayear,"Forecast F"), lwd=4, col=colours[c(2:4,1)], bty="n")
dev.off()

# Advice table
scen_num <- which(grepl("SSB=",
                        names(FC), fixed = TRUE))
FC[scen_num] <- FC2
res<-NULL
  
  for(i in 1:length(FC)){
    
    foc<-FC[[i]]

    tc <-(attributes(foc)$tab[,"catch:median"][[xy]])   # catch median 
    wc <-(attributes(foc)$tab[,"Land:median"][[xy]])
    uc <-(attributes(foc)$tab[,"Discard:median"][[xy]])
  
    F_all<-icesRound(attributes(foc)$tab[,"fbar:median"][[xy]])
    F_wc <-icesRound(attributes(foc)$tab[,"fbarL:median"][[xy]])
    F_uwc<-icesRound(attributes(foc)$tab[,"fbarD:median"][[xy]])
 
    SSB  <-(attributes(foc)$tab[,"ssb:median"][[xy]])
    rec  <-(attributes(foc)$tab[,"rec:median"][[xy]])

    SSB_all  <-attributes(foc)$tab[,"ssb:median"]
    SSBchange<-icesRound((SSB_all[xy+1]-SSB_all[xy])/SSB_all[xy]*100)
    
    Advice_change<-icesRound(((tc-advice)/advice)*100)
    TAC_change<-icesRound(((tc-tac)/tac)*100)
    
    Pblim<-sapply(foc, function(x) mean(x$ssb < Blim))[[xy+1]]
    
    res_table<-cbind(attributes(foc)$label,  round(tc), round(wc), round(uc), F_all, F_wc, F_uwc, round(SSB_all[xy+1]), SSBchange, TAC_change, Advice_change, Pblim )
    res_table<-as.data.frame(res_table)
    colnames(res_table)<-as.character(c("Basis",paste0("Total catch ", names(SSB_all[xy])),"Landings","Discards","Total F", "F Landings","F Discards",paste0("SSB ", names(SSB_all[xy+1])),"% SSB change","% TAC change","% Advice change","P(SSB < Blim)"))
    
    res<-rbind(res,res_table)
    
  }

  write.taf(res, file=paste0("output/Advice_Table_",season,".csv"), row.names=F)
  
# intermediate year  
  foc<-FC[[1]]
  
  tc <-(attributes(foc)$tab[,"catch:median"][[xy-1]])   
  wc <-(attributes(foc)$tab[,"Land:median"][[xy-1]])
  uc <-(attributes(foc)$tab[,"Discard:median"][[xy-1]])
 
  
  F_all<-icesRound(attributes(foc)$tab[,"fbar:median"][[xy-1]])
  F_wc<-icesRound(attributes(foc)$tab[,"fbarL:median"][[xy-1]])
  F_uwc<-icesRound(attributes(foc)$tab[,"fbarD:median"][[xy-1]])
 
  SSB_all  <-attributes(foc)$tab[,"ssb:median"]
  TSB_all  <-attributes(foc)$tab[,"tsb:median"]
  Rec_all  <-attributes(foc)$tab[,"rec:median"]
 
  res_table<-cbind(F_all, round(rbind(SSB_all[1:3])), round(rbind(Rec_all[1:4]/1000)),  round(tc), round(wc), round(uc))
  res_table<-as.data.frame(res_table)
  
  colnames(res_table)<-c("Total F", paste0("SSB ", names(SSB_all[1:3])),paste0("Rec",names(Rec_all[1:4])), paste0("Total catch ", names(SSB_all[xy-1])),"Landings","Discards")
  
  write.taf(res_table, file=paste0("output/Advice_Table_intermediate_year_",season,".csv"), row.names=F)
  
  
###############################################################################################################################
# F ranges catch option table

  res<-NULL
  
  for(i in 1:length(FC1)){
    
    foc<-FC1[[i]]
    
    tc <-(attributes(foc)$tab[,"catch:median"][[xy]])   # catch median 
    wc <-(attributes(foc)$tab[,"Land:median"][[xy]])
    uc <-(attributes(foc)$tab[,"Discard:median"][[xy]])
  
    
    F_all<-icesRound(attributes(foc)$tab[,"fbar:median"][[xy]])
    F_wc <-icesRound(attributes(foc)$tab[,"fbarL:median"][[xy]])
    F_uwc<-icesRound(attributes(foc)$tab[,"fbarD:median"][[xy]])

    SSB  <-(attributes(foc)$tab[,"ssb:median"][[xy]])
    rec  <-(attributes(foc)$tab[,"rec:median"][[xy]])
    TSB  <-(attributes(foc)$tab[,"tsb:median"][[xy]])
    
    TSB_all  <-attributes(foc)$tab[,"tsb:median"]
    SSB_all  <-attributes(foc)$tab[,"ssb:median"]
    SSBchange<-icesRound((SSB_all[xy+1]-SSB_all[xy])/SSB_all[xy]*100)
    
    Advice_change<-icesRound(((tc-advice)/advice)*100)
    TAC_change<-icesRound(((tc-tac)/tac)*100)
    
    res_table<-cbind(round(rec), round(TSB), round(SSB), round(tc), round(wc), round(uc), F_all, F_wc, F_uwc, round(SSB_all[xy+1]), SSBchange, TAC_change, Advice_change, attributes(foc)$label)
    res_table<-as.data.frame(res_table)
    
    colnames(res_table)<-c("Rec", paste0("TSB ", names(TSB_all[xy])), paste0("SSB ", names(SSB_all[xy])), paste0("Total catch", names(SSB_all[xy])),"Landings", "Discards", "Total F", "F Landings", "F Discards", paste0("SSB ", names(SSB_all[xy+1])),"%SSB change", "% TAC change","%Advice change","Basis")
    
    res<-rbind(res,res_table)
    
  }
  
  colnames(res)<-c("Rec",paste0("TSB ", names(TSB_all[xy])), paste0("SSB ", names(SSB_all[xy])),paste0("Total catch", names(SSB_all[xy])),"Landings","Discards","Total F", "F Landings","F Discards", paste0("SSB ", names(SSB_all[xy+1])),"%SSB change", "%TAC change","%Advice change","Basis")
  
  write.taf(res, file=paste0("output/Advice_Table_Franges_forecastoptions.csv"), row.names=F)
  

  ###################################################################################

  #plot FMSY option
  
  frcst.Fmsy <- FC[["Fmsy"]] 
  label <-attr(frcst.Fmsy,"label")
  attr(frcst.Fmsy,"estimateLabel") <- "median"
  png("output//Fig_43_Forecast_Fmsy.png",width = 7, height = 8, units = "in", res = 400)
  par(mar=c(4,5,2,2), cex.lab=1.6)
  plot(frcst.Fmsy,xlab="Year")
  mtext("Fmsy", side=3,line=-1, outer=TRUE)
  dev.off()
 
#####################################################################################
  
  ## Extract results of interest, write TAF output tables
 
  
  #col.pal9 <- c(brewer.pal(n = 8, name = "Dark2"),brewer.pal(n=6,name="Set2")[4])
  cols<-rainbow(9)
 
  # Settings
  ay <- year
  data_yrs <- 1978:(ay-1)
  
  ## Forecast parameters:
  Ay <- (ay-3):(ay-1) # for biols
  Sy <- (ay-3):(ay-1)  # for sel
  Ry <- 1983:(ay-2) # for rec
  
  
  # Forecast results tables and plots ----------------------------------------------------------------------
  
  #plot FMSY option
  
  frcst.Fmsy <- FC3 
  label <-attr(frcst.Fmsy,"label")
  
  frcst.fit <- attr(frcst.Fmsy, "fit")
  
 
  # get data from forecast fit
  cwts<-as.data.frame(frcst.fit$data$catchMeanWeight)
  cmWt <- apply(cwts[as.character(Ay),],2,mean)
  cwts <-rbind(cwts,t(cbind(cmWt,cmWt,cmWt)))
  cwts$Year <- 1978:(ay+2)
  colnames(cwts) <- c(0:8,"Year")
  
  cwts <- pivot_longer(cwts,cols=c(as.character(0:8)),names_to="age",values_to="wt")
  
  # weights for stock 
  wts <- as.data.frame(frcst.fit$data$stockMeanWeight)
  # add mean weights
  mWt <- apply(wts[as.character(Ay),],2,mean)
  wts <-rbind(wts,t(cbind(mWt,mWt)))
  wts$Year <- 1978:(ay+2)
  
  wts <- pivot_longer(wts,cols=c(as.character(0:8)),names_to="age",values_to="wt")
 
  # mat
  mat <- as.data.frame(frcst.fit$data$propMat)
  mato <- apply(mat[as.character(Ay),],2,mean)
  mat <-rbind(mat,t(cbind(mato,mato)))
  mat$Year <- 1978:(ay+2)
   
  mat <- pivot_longer(mat,cols=c(as.character(0:8)),names_to="age",values_to="mat")
  
  # numbers - catch
  catage_fc <- attr(frcst.Fmsy ,"caytable")
  idx <- which(row.names(catage_fc) %in% "Estimate")
  catage_fc <- as.data.frame(catage_fc[idx,])
  catage_fc$Year <- (ay-1):(ay+2)
  colnames(catage_fc) <- c(0:8,"Year")
  catage_fc <- catage_fc[catage_fc$Year%in%c(ay:(ay+1)),]
  
  c_now <- as.data.frame(caytable(frcst.fit, fleet=1))
  c_now <- c_now[as.numeric(rownames(c_now))< (ay),]
  c_now$Year <- rownames(c_now)
  cnums <- rbind(c_now,catage_fc)
  
  # reshape
  cnums <- pivot_longer(cnums,cols=c(as.character(0:8)),names_to="age",values_to="num")
  
  
  N <- ntable(frcst.fit)
  Nfc <- do.call(rbind, lapply(frcst.Fmsy , function(x)exp(colMedians(x$sim[,1:ncol(N)]))))
  rownames(Nfc) <- lapply(frcst.Fmsy, function(x)x$year)
  colnames(Nfc) <- colnames(N)

  natage_fc <- attr(Nfc,"naytable")
  n_now <- as.data.frame(ntable(frcst.fit))
  n_now <- n_now[as.numeric(rownames(n_now))< (ay-1),]
 
  nums <- rbind(n_now,Nfc)
  nums$Year<-as.numeric(rownames(nums))
  # reshape
  nums <- pivot_longer(nums,cols=c(as.character(0:8)),names_to="age",values_to="num")

  cnums$Year <- as.numeric(cnums$Year)
  cnums$age <- as.numeric(cnums$age)
  nums$age <- as.numeric(nums$age)
  cwts$Year <- as.numeric(cwts$Year)
  cwts$age <- as.numeric(cwts$age)
  wts$Year <- as.numeric(wts$Year)
  wts$age <- as.numeric(wts$age)
  mat$Year <- as.numeric(mat$Year)
  mat$age <- as.numeric(mat$age)
  
 
  ct <- left_join(cnums,cwts,by=c("Year","age"))
  ct <- ct %>% group_by(Year,age) %>% mutate(catch=num*wt)
  ct$age <- factor(ct$age,levels=rev(0:8))
  
  # Proportion of each age in SSB
  st <- left_join(left_join(nums,wts,by=c("Year","age")),mat,by=c("Year","age"))
    st <- st %>% group_by(Year,age) %>% mutate(ssb=num*wt*mat)
  st$age <- factor(st$age,levels=rev(0:8))
  
  coul <- brewer.pal(9, "Paired")
  
  png(filename="output/Fig_50_Age distribution in SSB.png", height=10, width=8, units = "in", res = 300)

  g1<-ggplot(st,aes(x=Year,y=ssb/1000,fill=age))+geom_col(color="black", linewidth=0.4)+theme_bw()+
    scale_fill_manual(values=rev(coul))+labs(x="Year",y="SSB (1000 t)",fill="")

  g2<-ggplot(st,aes(x=Year,y=ssb,fill=age))+geom_col(position="fill", color="black", linewidth=0.4)+theme_bw()+
    scale_fill_manual(values=rev(coul))+labs(x="Year",y="SSB (proportion)",fill="")+ scale_y_continuous(breaks=seq(0,1,0.2))
  
  plot_grid(g1, g2, ncol = 1, align="vh")
   dev.off()
  
  png(filename="output/Fig_51_Age distribution in catch.png", height=10, width=8, units = "in", res = 300)
  
  gg1<-ggplot(ct,aes(x=Year,y=catch/1000,fill=age))+geom_col(color="black", linewidth=0.4)+theme_bw()+
    scale_fill_manual(values=rev(coul))+labs(x="Year",y="Estimated Catch (1000 t)",fill="")

  gg2<-ggplot(ct,aes(x=Year,y=catch,fill=age))+geom_col(position="fill", color="black", linewidth=0.4)+theme_bw()+
    scale_fill_manual(values=rev(coul))+labs(x="Year",y="Estimated Catch (proportion)",fill="")+ scale_y_continuous(breaks=seq(0,1,0.2))
  
  plot_grid(gg1, gg2, ncol = 1,align="vh")
  dev.off()
  