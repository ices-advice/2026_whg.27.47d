################################## readStockOverview ###########################

readStockOverview <- function(StockOverviewFile,NumbersAtAgeLengthFile){
  
  Wdata <- read.table(StockOverviewFile,header=TRUE,sep="\t")
  names(Wdata)[7] <- "Fleet"
  names(Wdata)[10] <- "CatchWt"
  names(Wdata)[11] <- "CatchCat"
  names(Wdata)[12] <- "ReportCat"
  names(Wdata)[13]<-"D_Imported_Raised"
  Wdata$CatchCat <- substr(Wdata$CatchCat,1,1)
  #Wdata <- Wdata[,-ncol(Wdata)]
  
  Ndata <- read.table(NumbersAtAgeLengthFile,header=TRUE,sep="\t",skip=1)
  names(Ndata)[7] <- "CatchCat"
  names(Ndata)[9] <- "Fleet"
  
  Wdata <- merge(Wdata,Ndata[,c(3,4,5,7,9,10,11)],by=c("Area","Season","Fleet","Country","CatchCat"),all.x=TRUE)
  Wdata$Sampled <- ifelse(is.na(Wdata$SampledCatch),FALSE,TRUE)
  
  return(Wdata)
}
################################## readNumbersAtAgeLength ######################

readNumbersAtAgeLength <- function(NumbersAtAgeLengthFile){
  
  Ndata <- read.table(NumbersAtAgeLengthFile,header=TRUE,sep="\t",skip=1)
  names(Ndata)[7] <- "CatchCat"
  names(Ndata)[8] <- "ReportCat"
  names(Ndata)[9] <- "Fleet"
  Ndata <- Ndata[,-ncol(Ndata)]
  ageNames <- names(Ndata)[16:ncol(Ndata)]
  ages <- as.numeric(substr(ageNames,16,nchar(ageNames)))
  allAges <- min(ages,na.rm=T):max(ages,na.rm=T)
  missingAges <- allAges[allAges %in% ages]
  
  return(Ndata)
}


############################ plotAgeDistribution ###############################

plotAgeDistribution1 <- function(dat,plotType="perCent") {
  
  plotTypes <- c("perCent","frequency")
  
  if (!(plotType %in% plotTypes)) 
    stop(paste("plotType needs to be one of the following:", paste(plotTypes,collapse=", ")))
  
  sampAge <- dat
  ageCols <- 16:ncol(dat)
  colnames(sampAge)[ageCols]<- substr(colnames(sampAge)[ageCols], 13,17)
  
  
  if (TRUE) {
    par(mar=c(4,2,1,1)+0.1)
    par(mfcol=c(4,2))
    
    year <- sampAge$Year[1]
    
    sampAge$AFC <- paste(sampAge$Area,sampAge$Fleet,sampAge$Country)
    AFClist <- sort(unique(sampAge$AFC))
    nAFC <- length(AFClist)
    
    overallAgeDist <- list()
    overallAgeDist[["L"]] <- colSums(sampAge[sampAge$CatchCat=="L" ,ageCols])/sum(sampAge[sampAge$CatchCat=="L" ,ageCols])
    overallAgeDist[["D"]] <- colSums(sampAge[sampAge$CatchCat=="D" ,ageCols])/sum(sampAge[sampAge$CatchCat=="D" ,ageCols])
    
    ymax <- max(sampAge[sampAge$Country!="UK(Scotland)",ageCols])
    for (i  in 1:nAFC) {
      for (catch in c("L","D")) {
        for (season in 1:4) {
          if (sampAge[sampAge$AFC==AFClist[i],"Country"][1]=="UK(Scotland)" & catch=="D" & season==1) {
            season <- year
          }
          indx <- sampAge$AFC==AFClist[i] & sampAge$CatchCat==catch & sampAge$Season==season
          dat <- sampAge[indx,]
          if (nrow(dat)==0) {
            plot(0,0,type="n",xlab="",ylab="")
          } else {
            ageDist <- as.matrix(dat[,ageCols],nrow=nrow(dat),ncol=length(ageCols))
            if (plotType=="perCent") ageDist <- ageDist/rowSums(ageDist)
            newYmax <- max(sampAge[sampAge$AFC==AFClist[i],ageCols],na.rm=TRUE)
            if (plotType=="perCent") newYmax <- max(ageDist)
            newYmax <- 1.05*newYmax
            #newYmax <- max(ageDist,na.rm=TRUE)
            barplot(ageDist,ylim=c(0,newYmax),las=2)
            box()
          }
          title(paste(AFClist[i],catch,season),cex.main=0.7,line=0.5)
        }
      }
      
    }
    
  }
  
}


################################## plotStockOverview ###############################

plotStockOverview <- function(dat,plotType="LandPercent",byFleet=TRUE,byCountry=TRUE,bySampled=TRUE,bySeason=FALSE,byArea=FALSE,countryColours=NULL,set.mar=TRUE,markSampled=TRUE,individualTotals=TRUE,ymax=NULL){
  
  plotTypes <- c("LandWt","LandPercent","CatchWt","DisWt","DisRatio","DiscProvided")
  if (!(plotType %in% plotTypes)) stop(paste("PlotType needs to be one of the following:",paste(plotTypes)))
  
  stock <- dat$Stock[1]
  
  impLand <- dat[dat$CatchCat=="L" ,]
  impDis <- dat[dat$CatchCat%in%c("D") ,]
  
  nArea <- nSeason <- nCountry <- nFleet <- 1
  
  SeasonNames <- sort(unique(impLand$Season))
  AreaNames <- sort(unique(impLand$Area))
  
  countryLegend <- FALSE
  
  if (byFleet) nFleet <- length(unique(c(impLand$Fleet, impDis$Fleet))) ## YV
  if (byCountry) { 
    nCountry <- length(unique(c(impLand$Country, impDis$Country))) # YV
    countryLegend <- TRUE
  }    
  if (byArea) nArea <- length(AreaNames)
  if (bySeason) nSeason <- length(SeasonNames)
  if (!bySampled) markSampled <- FALSE
  
  
  if (length(countryColours)==1 &&countryColours){
    #countryColours <- data.frame(
    #"Country"=c("Belgium","Denmark","France","Germany","Netherlands","Norway","Poland","Sweden","UK (England)","UK(Scotland)"),
    #"Colour"=c("green", "red", "darkblue", "black", "orange","turquoise" ,"purple","yellow","magenta","blue")
    #, stringsAsFactors=FALSE)
    countryColours <- data.frame("Country"=unique(dat$Country)[order(unique(dat$Country))],
                                 "Colour"=rainbow(length(unique(dat$Country)))
                                 , stringsAsFactors=FALSE)
  }
  if (length(countryColours)==1 && countryColours==FALSE){
    countryLegend <- FALSE
    #countryColours <- data.frame(
    #"Country"=c("Belgium","Denmark","France","Germany","Norway","Netherlands","Poland","Sweden","UK (England)","UK(Scotland)")
    # , stringsAsFactors=FALSE)
    countryColours <- data.frame("Country"=unique(dat$Country)[order(unique(dat$Country))],
                                 stringsAsFactors=FALSE)
    countryColours$Colour <- rep("grey",length(countryColours$Country))
  }
  
  
  LsummaryList <- list()
  DsummaryList <- list()
  summaryNames <- NULL
  i <- 1
  if (byFleet) {
    LsummaryList[[i]] <- impLand$Fleet
    DsummaryList[[i]] <- impDis$Fleet
    summaryNames <- c(summaryNames,"Fleet")
    i <- i+1
  }
  if(byCountry) {
    LsummaryList[[i]] <- impLand$Country
    DsummaryList[[i]] <- impDis$Country
    summaryNames <- c(summaryNames,"Country")
    i <- i+1
  }
  if(bySeason) {
    LsummaryList[[i]] <- impLand$Season
    DsummaryList[[i]] <- impDis$Season
    summaryNames <- c(summaryNames,"Season")
    i <- i+1
  }
  if (byArea) {
    LsummaryList[[i]] <- impLand$Area
    DsummaryList[[i]] <- impDis$Area
    summaryNames <- c(summaryNames,"Area")
    i <- i+1
  }
  if (bySampled) {
    LsummaryList[[i]] <- impLand$Sampled
    DsummaryList[[i]] <- impDis$Sampled
    summaryNames <- c(summaryNames,"Sampled")
    i <- i+1
  }
  byNames <- summaryNames
  summaryNames <- c(summaryNames,"CatchWt")
  
  ### YV changed
  if(plotType%in%c("LandWt","LandPercent")){
    Summary <- aggregate(impLand$CatchWt,LsummaryList,sum)
  } else if (plotType=="DisWt"){
    Summary <- aggregate(impDis$CatchWt,DsummaryList,sum)
  } else if (plotType=="DisRatio"){
    SummaryD <- aggregate(impDis$CatchWt,DsummaryList,sum)
    SummaryL <- aggregate(impLand$CatchWt,LsummaryList,sum)
    if(bySampled){
      testN <- colnames(SummaryD)[grep('Group', colnames(SummaryD)) - 1]
    } else {
      testN <- colnames(SummaryD)[grep('Group', colnames(SummaryD))]	
    }
    Summary <- merge(SummaryD, SummaryL, all=T, by=testN)
    Summary$DRatio <- Summary$x.x / (Summary$x.x+Summary$x.y)
    Summary <- Summary[!is.na(Summary$DRatio),c(testN,paste('Group.',length(testN)+1,'.x', sep=''),paste('Group.',length(testN)+1,'.y', sep=''),'DRatio')]
  } else if (plotType=="DiscProvided"){
    if(bySampled){
      DsummaryList <- DsummaryList[1: (length(DsummaryList)- 1)]
      LsummaryList <- LsummaryList[1: (length(LsummaryList)- 1)]
    } else {
      DsummaryList <- DsummaryList[ length(DsummaryList)]
      LsummaryList <- LsummaryList[ length(LsummaryList)]
    }
    SummaryD <- aggregate(impDis$CatchWt,DsummaryList,sum)
    SummaryL <- aggregate(impLand$CatchWt,LsummaryList,sum)
    if(bySampled){
      testN <- colnames(SummaryD)[grep('Group', colnames(SummaryD))]
    } else {
      testN <- colnames(SummaryD)[grep('Group', colnames(SummaryD))]	
    }
    Summary <- merge(SummaryD, SummaryL, all=T, by=testN)
    Summary$DRatio <- Summary$x.x / (Summary$x.x+Summary$x.y)
    Summary <- Summary[,c(testN,'DRatio','x.y')]
    Summary$DRatio[!is.na(Summary$DRatio)] <- TRUE  
    Summary$DRatio[is.na(Summary$DRatio)] <- FALSE
    
    Summary <- unique(Summary) 
    ProvidedDiscards <<- Summary
    
  }
  ### end YV changed
  
  
  #disSummary <- aggregate(impDis$CatchWt,DsummaryList,sum) YV
  if (plotType!="DisRatio") {
    names(Summary) <- summaryNames #YV
  } else {
    names(Summary) <- c(summaryNames[-c(grep(c('Sampled'),summaryNames), grep(c('CatchWt'),summaryNames))],
                        "SampledD", "SampledL", "DRatio")
  }
  #names(disSummary) <- summaryNames # yv
  #names(disSummary) <- c("Fleet" ,  "Country", "Area" ,   "SampledD" ,"DisWt") # yv
  #names(landSummary) <- c("Fleet" ,  "Country", "Area" ,   "SampledL" ,"LandWt") # yv
  
  
  #stratumSummary <- merge(landSummary,disSummary,by=byNames,all=TRUE) #YV
  stratumSummary <- Summary #YV
  if(plotType%in%c("LandWt","LandPercent","DiscProvided")){
    names(stratumSummary)[names(stratumSummary)=="CatchWt"] <- "LandWt" # YV
  } else if (plotType=="DisWt"){
    names(stratumSummary)[names(stratumSummary)=="CatchWt"] <- "DisWt" # YV
  } 
  
  #names(stratumSummary)[names(stratumSummary)=="CatchWt.y"] <- "DisWt"  # YV
  
  #if (bySampled ) {
  ##  stratumSummary <- stratumSummary[rev(order(stratumSummary$Sampled,stratumSummary$LandWt)),
  #	if(plotType%in%c("LandWt","LandPercent")){
  #	  stratumSummary <- stratumSummary[rev(order(stratumSummary$Sampled,stratumSummary$LandWt)),]
  #	} else if (plotType=="DisWt"){
  #	  stratumSummary <- stratumSummary[rev(order(stratumSummary$Sampled,stratumSummary$DisWt)),]
  #	}
  #} else {
  #	if(plotType%in%c("LandWt","LandPercent")){
  #	  stratumSummary <- stratumSummary[rev(order(stratumSummary$LandWt)),]
  #	} else if (plotType=="DisWt"){
  #	  stratumSummary <- stratumSummary[rev(order(stratumSummary$DisWt)),]
  #	}
  #}
  if (bySampled ) {
    if(plotType!="DisRatio"){
      stratumSummary <- stratumSummary[rev(order(stratumSummary$Sampled,stratumSummary[,dim(stratumSummary)[2]])),]
    } else {
      stratumSummary <- stratumSummary[rev(order(stratumSummary$SampledL,stratumSummary$SampledD,stratumSummary[,dim(stratumSummary)[2]])),]	
    }
  } else {
    stratumSummary <- stratumSummary[rev(order(stratumSummary[,dim(stratumSummary)[2]])),]
  }
  
  #catchData <- matrix(c(stratumSummary$LandWt,stratumSummary$DisWt),byrow=TRUE,nrow=2) YV
  catchData <- stratumSummary[,dim(stratumSummary)[2]] #YV
  
  
  if (set.mar) par(mar=c(10,4,1,1)+0.2) 
  
  for (a in 1:nArea) {
    #windows()
    if (bySeason & !(byCountry | byFleet)) nSeason <- 1
    for (s in 1:nSeason) {
      area <- AreaNames[a]
      season <- SeasonNames[s]
      
      indx <- 1:nrow(stratumSummary)
      if (bySeason & !byArea & (byCountry | byFleet)) indx <- stratumSummary$Season==season 
      if (!bySeason & byArea) indx <- stratumSummary$Area==area 
      if (bySeason & byArea & (byCountry | byFleet | bySampled)) indx <- stratumSummary$Area==area & stratumSummary$Season==season
      
      if (individualTotals) {
        sumLandWt <- sum(stratumSummary$LandWt[indx],na.rm=TRUE)
      } else {
        sumLandWt <- sum(stratumSummary$LandWt,na.rm=TRUE)
      }  
      if(byCountry) {
        colVec <- countryColours$Colour[match(stratumSummary$Country[indx],countryColours$Country)]
      } else {
        colVec <- "grey"
      }  
      
      if (plotType%in%c("LandWt","DiscProvided")) yvals <- stratumSummary$LandWt[indx]
      if (plotType=="LandPercent") yvals <- 100*stratumSummary$LandWt[indx]/sumLandWt    
      if (plotType=="DisWt") yvals <- stratumSummary$DisWt[indx] 
      if (plotType=="CatchWt") yvals <- catchData[,indx] 
      if (plotType=="DisRatio") yvals <- stratumSummary$DRatio
      
      if(plotType!="DisRatio"){
        if (!is.null(ymax)) newYmax <- ymax
        if (is.null(ymax)) newYmax <- max(yvals,na.rm=TRUE)
        if (is.null(ymax) & plotType=="LandPercent") newYmax <- max(cumsum(yvals),na.rm=TRUE)
        #if (is.null(ymax) & plotType=="CatchWt") newYmax <- max(colSums(yvals),na.rm=TRUE) #YV
        #if (plotType=="CatchWt") colVec <- c("grey","black") #YV
        #if (plotType=="CatchWt") countryLegend <- FALSE #YV
        if (markSampled) newYmax <- 1.06*newYmax
        if (byFleet) namesVec=stratumSummary$Fleet[indx]
        if (!byFleet) {
          if (byArea & bySeason) namesVec=paste(stratumSummary$Area[indx],stratumSummary$Season[indx]) 
          if (!byCountry & !byArea & bySeason) namesVec=paste(stratumSummary$Season[indx]) 
          if (byCountry) namesVec=paste(stratumSummary$Country[indx]) 
          if (bySampled & !byCountry & nArea==1 & nSeason==1) namesVec=paste(stratumSummary$Season[indx]) 
        }
        
        
        cumulativeY <- cumsum(yvals)
        yvals[yvals>newYmax] <- newYmax
        
        if ((newYmax==-Inf)) {
          plot(0,0,type="n",axes=FALSE,xlab="",ylab="")
          box()
        } else {
          b <- barplot(yvals,names=namesVec,las=2,cex.names=0.7,col=colVec,ylim=c(0,newYmax),yaxs="i")   
          
          if (bySampled & markSampled) {
            nSampled <- sum(stratumSummary$Sampled[indx])
            if(nSampled>0){
              arrows(b[1]-(b[2]-b[1])/2,newYmax*102/106,b[nSampled]+(b[2]-b[1])/2,newYmax*102/106,code=3,length=0.1) 
              arrows(b[nSampled]+(b[2]-b[1])/2,newYmax*102/106,b[length(b)]+(b[2]-b[1])/2,newYmax*102/106,code=3,length=0.1) 
              if(plotType!="DiscProvided"){
                text((b[nSampled]+b[1])/2,newYmax*104/106,"sampled",cex=0.8)
                text((b[length(b)]+b[nSampled])/2+(b[2]-b[1])/2,newYmax*104/106,"unsampled",cex=0.8)
              }else{
                text((b[nSampled]+b[1])/2,newYmax*104/106,"Landings with Discards",cex=0.8)
                text((b[length(b)]+b[nSampled])/2+(b[2]-b[1])/2,newYmax*104/106,"no Discards",cex=0.8)	
              }
            } else {
              arrows(b[1]-(b[2]-b[1])/2,newYmax*102/106,b[length(b)]+(b[2]-b[1])/2,newYmax*102/106,code=3,length=0.1) 
              if(plotType!="DiscProvided"){
                text(b[length(b)]+(b[2]-b[1])/4,newYmax*104/106,"unsampled",cex=0.8)
              }else{
                text(b[length(b)]+(b[2]-b[1])/4,newYmax*104/106,"no Discards",cex=0.8)
              }
            }
          }
          if (countryLegend) legend("topright",inset=0.05,legend=countryColours$Country,col=countryColours$Colour,pch=15)
          box()
          if (plotType=="LandPercent") lines(b-(b[2]-b[1])/2,cumulativeY,type="s")
          if (plotType=="LandPercent") abline(h=c(5,1),col="grey",lty=1)
          if (plotType=="LandPercent") abline(h=c(90,95,99),col="grey",lty=1)
          if (plotType=="LandPercent") abline(h=100)
        }
        if (!bySeason & !byArea) title.txt <- paste(stock)
        if (!bySeason & byArea) title.txt <- paste(stock,area)
        if (bySeason & !byArea & !(byCountry | byFleet | bySampled)) title.txt <- paste(stock)
        if (bySeason & !byArea & (byCountry | byFleet | bySampled)) title.txt <- paste(stock,season)
        if (bySeason & byArea) title.txt <- paste(stock,area,season)
        title.txt <- paste(title.txt,plotType)
        title(title.txt)
      } else {
        par(mfrow=c(2,2))
        listSample <- unique(paste(stratumSummary$SampledD,stratumSummary$SampledL))
        for(i in 1:length(listSample)){
          idx <- which(stratumSummary$SampledD[indx]==strsplit(listSample[i],' ')[[1]][1] & stratumSummary$SampledL[indx]==strsplit(listSample[i],' ')[[1]][2])
          if(length(idx)>0){
            if(byCountry) {
              colVec <- countryColours$Colour[match(stratumSummary$Country[indx][idx],countryColours$Country)]
            } else {
              colVec <- "grey"
            }  
            
            if (!is.null(ymax)) newYmax <- ymax
            if (is.null(ymax)) newYmax <- max(yvals,na.rm=TRUE)
            if (is.null(ymax) & plotType=="LandPercent") newYmax <- max(cumsum(yvals),na.rm=TRUE)
            #if (is.null(ymax) & plotType=="CatchWt") newYmax <- max(colSums(yvals),na.rm=TRUE) #YV
            #if (plotType=="CatchWt") colVec <- c("grey","black") #YV
            #if (plotType=="CatchWt") countryLegend <- FALSE #YV
            if (markSampled) newYmax <- 1.06*newYmax
            if (byFleet) namesVec=stratumSummary$Fleet[indx][idx]
            if (!byFleet) {
              if (byArea & bySeason) namesVec=paste(stratumSummary$Area[idx],stratumSummary$Season[indx][idx]) 
              if (!byCountry & !byArea & bySeason) namesVec=paste(stratumSummary$Season[indx][idx]) 
              if (byCountry) namesVec=paste(stratumSummary$Country[indx][idx]) 
              if (bySampled & !byCountry & nArea==1 & nSeason==1) namesVec=paste(stratumSummary$Season[indx][idx]) 
            }
            
            
            cumulativeY <- cumsum(yvals[indx][idx])
            yvals[yvals>newYmax] <- newYmax
            
            if ((newYmax==-Inf)) {
              plot(0,0,type="n",axes=FALSE,xlab="",ylab="")
              box()
            } else {
              b <- barplot(yvals[indx][idx],names=namesVec,las=2,cex.names=0.7,col=colVec,ylim=c(0,newYmax),yaxs="i")   
              
              #if (bySampled & markSampled) {
              #  nSampledD <- sum(stratumSummary$SampledD[idx]==T)
              #  nSampledL <- sum(stratumSummary$SampledL[idx]==T)
              #  if (nSampledD>0) arrows(b[1]-(b[2]-b[1])/2,newYmax*102/106,b[nSampledD]+(b[2]-b[1])/2,newYmax*102/106,code=3,length=0.1) 
              #  if (nSampledD>0) arrows(b[nSampledD]+(b[2]-b[1])/2,newYmax*102/106,b[length(b)]+(b[2]-b[1])/2,newYmax*102/106,code=3,length=0.1) 
              
              #  if (nSampledL>0) arrows(b[1]-(b[2]-b[1])/2,newYmax*92/106,b[nSampledL]+(b[2]-b[1])/2,newYmax*92/106,code=3,length=0.1) 
              #  if (nSampledL>0) arrows(b[nSampledL]+(b[2]-b[1])/2,newYmax*92/106,b[length(b)]+(b[2]-b[1])/2,newYmax*92/106,code=3,length=0.1) 
              
              #  } 
              if (countryLegend) legend("topright",inset=0.05,legend=countryColours$Country,col=countryColours$Colour,pch=15)
              box()
              if (plotType=="LandPercent") lines(b-(b[2]-b[1])/2,cumulativeY,type="s")
              if (plotType=="LandPercent") abline(h=c(5,1),col="grey",lty=1)
              if (plotType=="LandPercent") abline(h=c(90,95,99),col="grey",lty=1)
              if (plotType=="LandPercent") abline(h=100)
            }
            if (!bySeason & !byArea) title.txt <- paste(stock)
            if (!bySeason & byArea) title.txt <- paste(stock,area)
            if (bySeason & !byArea & !(byCountry | byFleet | bySampled)) title.txt <- paste(stock)
            if (bySeason & !byArea & (byCountry | byFleet | bySampled)) title.txt <- paste(stock,season)
            if (bySeason & byArea) title.txt <- paste(stock,area,season)
            title.txt <- paste(title.txt,plotType, "D/L", listSample[i])
            title(title.txt)
          }
        }
      }
    }
  }                                                                                                                
}

################################## plotsurvey CPUE maps ###############################

basemap<-function (lon1, lon2, lat1, lat2, grid = FALSE, zoom = FALSE, 
                   landcolor = "springgreen3", seacolor = "white", data = gmt3) 
{
  xrange <- c(lon1, lon2)
  yrange <- c(lat1, lat2)
  aspect <- c(cos((mean(yrange) * pi)/180), 1)
  d <- c(diff(xrange), diff(yrange)) * (1 + 2 * 0) * aspect
  if (!par("new")) 
    plot.new()
  p <- par("fin") - as.vector(matrix(c(0, 1, 1, 0, 0, 1, 1, 
                                       0), nrow = 2) %*% par("mai"))
  par(pin = p)
  p <- par("pin")
  p <- d * min(p/d)
  par(pin = p)
  d <- d * 0 + ((p/min(p/d) - d)/2)/aspect
  realusr <- c(xrange, yrange) + rep(c(-1, 1), 2) * rep(d, 
                                                        c(2, 2))
  par(usr = realusr)
  rect(lon1, lat1, lon2, lat2, col = seacolor)
  if (grid) {
    axis(1, tck = 1)
    axis(2, tck = 1)
  }
  if (xrange[1] < 0) {
    par(usr = realusr + c(360, 360, 0, 0))
    polygon(data, border = landcolor, col = landcolor)
  }
  if (xrange[2] > 360) {
    par(usr = realusr - c(360, 360, 0, 0))
    polygon(data, border = landcolor, col = landcolor)
  }
  par(usr = realusr)
  polygon(data, border = landcolor, col = landcolor)
  rect(lon1, lat1, lon2, lat2, lwd = 1)
  #axis(1)
  #mtext("Longitude", side = 1, line = 3)
  #par(las = 1)
  #axis(2)
  #mtext("Latitude", side = 2, line = 3, las = 0)
  if (zoom) {
    ret <- locator(2)
    if (length(ret$x) < 2) {
      zoom <- FALSE
    }
    else {
      lon1 <- min(ret$x)
      lon2 <- max(ret$x)
      lat1 <- min(ret$y)
      lat2 <- max(ret$y)
    }
    basemap(lon1, lon2, lat1, lat2, grid, zoom, landcolor, seacolor, data)
  }
}

xy<-function(subArea){
  key<-c(E=-10,F=0,G=10,H=20)
  txt<-as.character(cpue$SubArea)
  x<-key[substr(txt,3,3)]+as.numeric(substr(txt,4,4))+.5
  y<-(as.numeric(substr(txt,1,2))+71)/2+.25
  cbind(x,y)
}

plotone<-function(age, year, quarter, lon1=-5, lon2=12, lat1=50, lat2=62, scale=gscale){
  basemap(lon1,lon2,lat1,lat2)
  idx<-which((cpue$Year==year)&(cpue$Quarter==quarter))
  if(age=='3+'){
    vec<-rowSums(cpue[idx,11:14])
  }else{
    vec<-cpue[idx,8+as.numeric(age)]
  }
  ll<-lonlat[idx,]
  points(ll,cex=sqrt(vec)*scale)
}


############################ aggregateStockOverview ############################

aggregateCatchWt <- function(dat,byFleet=TRUE,byCountry=TRUE,byArea=FALSE,bySeason=FALSE,byLandBioImp=TRUE,byDisBioImp=TRUE,byDisWtImp=TRUE) {
  
  
  if (is.null(dat$D_Imported_Raised)) dat$RsdOrImp <- "Imp" else{dat$RsdOrImp<-dat$D_Imported_Raised}
  
  summaryNames <- NULL
  if (byFleet) 
    summaryNames <- c(summaryNames,"Fleet")
  if(byCountry) 
    summaryNames <- c(summaryNames,"Country")
  if(bySeason)
    summaryNames <- c(summaryNames,"Season")
  if (byArea) 
    summaryNames <- c(summaryNames,"Area")
  if (byLandBioImp) 
    summaryNames <- c(summaryNames,"LandBioImp")
  if (byDisBioImp) 
    summaryNames <- c(summaryNames,"DisBioImp")
  if (byDisWtImp) 
    summaryNames <- c(summaryNames,"DisWtImp")
  
  if (any(dat$Season.type=="Year") & any(dat$Season.type=="Quarter")) {
    id <- paste(dat$Fleet,dat$Country,dat$Area)    
    Yid <- id[dat$Season.type=="Year"]
    year <- dat[dat$Season.yType=="Year",][1,"Season"]
    Ydat <- dat[(id %in% Yid),]
    aggYdat <- aggregate(CatchWt~Fleet+Country+Area+Sampled+RsdOrImp+CatchCat,data=Ydat,sum)
    Yland <- aggYdat[aggYdat$CatchCat=="L",]
    Ydis <- aggYdat[aggYdat$CatchCat=="D",]
    catch <- merge(Yland,Ydis,by=c("Fleet","Country","Area"),all=TRUE)
    catch$Season <- year
    
    catch1 <- catch
    
    id <- paste(dat$Fleet,dat$Country,dat$Area)
    Yid <- paste(Ydat$Fleet,Ydat$Country,Ydat$Area)
    Qdat <- dat[!(id %in% Yid),]
    Qland <- Qdat[Qdat$CatchCat=="L",]
    Qdis <- Qdat[Qdat$CatchCat=="D",]
    catch <- merge(Qland,Qdis,by=c("Fleet","Country","Area","Season"),all=TRUE)
    catch2 <- catch
    
    catch <- merge(catch1,catch2,all=TRUE)
  } else {
    land <- dat[dat$CatchCat=="L",-which(names(dat)%in% "CatchCat")]
    dis <- dat[dat$CatchCat=="D",-which(names(dat)%in% "CatchCat")]
    catch <- merge(land,dis,by=c("Year","Stock","Area","Season","Fleet","Country","Effort","Season.type"),all=TRUE)
  } 
  
  names(catch)[names(catch)=="Sampled.x"] <- "LandBioImp"
  names(catch)[names(catch)=="Sampled.y"] <- "DisBioImp"
  names(catch)[names(catch)=="CatchWt.x"] <- "LandWt"
  names(catch)[names(catch)=="CatchWt.y"] <- "DisWt"
  names(catch)[names(catch)=="RsdOrImp.y"] <- "DisWtImp"
  
  catch$LandBioImp[is.na(catch$LandBioImp)] <- FALSE
  catch$DisBioImp[is.na(catch$DisBioImp)] <- FALSE
  catch$DisWtImp<-as.character(catch$DisWtImp)
  catch$DisWtImp[catch$DisWtImp=="Imported"] <- TRUE
  catch$DisWtImp[catch$DisWtImp=="Raised Discard"] <- FALSE
  catch$DisWtImp[is.na(catch$DisWtImp)] <- FALSE
  catch$DisWtImp <- as.logical(catch$DisWtImp)
  
  sum.na <- function(x){sum(x,na.rm=TRUE)}
  stratumSummary <- eval(parse(text=paste("aggregate(cbind(LandWt,DisWt)~",paste(summaryNames,collapse="+"),",data=catch,sum.na,na.action=na.pass)",sep="")))
  
  if (byLandBioImp) {
    stratumSummary <- stratumSummary[rev(order(stratumSummary$LandBioImp,stratumSummary$LandWt,na.last=FALSE)),]
    if (byDisBioImp) {
      stratumSummary <- stratumSummary[rev(order(stratumSummary$LandBioImp,stratumSummary$LandBioImp,stratumSummary$LandWt,na.last=FALSE)),]
    }
  } else {
    stratumSummary <- stratumSummary[rev(order(stratumSummary$LandWt,na.last=FALSE)),]
  }
  return(stratumSummary)                                                        
}

############################## readOutputCatchWt ###############################

readOutputCatchWt <- function(CatchAndSampleDataTablesFile){
  
  dataFile <- CatchAndSampleDataTablesFile
  datText <- readLines(dataFile)
  table1Start <- which(datText=="TABLE 1.")
  table2Start <- which(datText=="TABLE 2.")
  nRows <- table2Start - 3 - table1Start - 6
  
  datNames <- scan(dataFile,sep="\t",skip=table1Start+3,nlines=1,what=character(),quiet=TRUE)
  tab1 <- read.table(dataFile,sep="\t",skip=table1Start+5,nrows=nRows,header=FALSE,as.is=TRUE)
  names(tab1) <- datNames
  names(tab1)[4] <- "CatchCat"
  names(tab1)[6] <- "RsdOrImp"
  names(tab1)[12] <- "CatchWt"
  names(tab1)[15] <- "OffLand"
  names(tab1)[16] <- "Sampled"
  names(tab1)[18] <- "NumLenSamp"
  names(tab1)[19] <- "NumLenMeas"
  names(tab1)[20] <- "NumAgeSamp"
  names(tab1)[21] <- "NumAgeMeas"
  tab1$Area <- gsub(" ","",tab1$Area)
  
  Wdata <- tab1
  Wdata$CatchCat <- substr(Wdata$CatchCat,1,1)
  Wdata$RsdOrImp <- ifelse(substr(Wdata$RsdOrImp,1,1)=="R","Rsd","Imp")
  Wdata$Sampled <- ifelse(substr(Wdata$Sampled,1,1)=="S",TRUE,FALSE)
  
  return(Wdata)
}

########################## readOutputNumbersAtAgeLength ########################

readOutputNumbersAtAgeLength <- function(CatchAndSampleDataTablesFile){
  
  dataFile <- CatchAndSampleDataTablesFile
  datText <- readLines(dataFile)
  table2Start <- which(datText=="TABLE 2.")
  
  datNames <- scan(dataFile,sep="\t",skip=table2Start+3,nlines=1,what=character(),quiet=TRUE)
  tab2 <- read.table(dataFile,sep="\t",skip=table2Start+6,header=FALSE,as.is=TRUE)
  names(tab2) <- datNames
  tab2$Area <- gsub(" ","",tab2$Area)
  #tab2$AgeOrLength <- factor(tab2$AgeOrLength)
  #tab2$ID <- paste(tab2$Area,tab2$Fleet,tab2$CatchCategory,tab2$Country,tab2$Season)
  
  names(tab2)[4] <- "CatchCat"
  names(tab2)[6] <- "RsdOrImp"
  names(tab2)[12] <- "CatchWt"
  names(tab2)[15] <- "OffLand"
  names(tab2)[16] <- "Sampled"
  names(tab2)[24] <- "NumLenSamp"
  names(tab2)[25] <- "NumLenMeas"
  names(tab2)[26] <- "NumAgeSamp"
  names(tab2)[27] <- "NumAgeMeas"
  tab2$Area <- gsub(" ","",tab2$Area)
  
  Ndata <- tab2
  Ndata$CatchCat <- substr(Ndata$CatchCat,1,1)
  Ndata$RsdOrImp <- ifelse(substr(Ndata$RsdOrImp,1,1)=="R","Rsd","Imp")
  Ndata$Sampled <- ifelse(substr(Ndata$Sampled,1,1)=="S",TRUE,FALSE)
  
  return(Ndata)
}

######################## aggTotalStockCatchWtByCountry #########################

aggTotalCatchWtByCountry <- function(OutWtData){
  
  OWF <- OutWtData
  WtInfo <- tapply(OWF$CatchWt,list(OWF$Country,OWF$RsdOrImp,OWF$CatchCat),sum,na.rm=T)/1000                        
  WtInfo[is.na(WtInfo)] <- 0   
  WtSumm <- as.data.frame(matrix(WtInfo,nrow=nrow(WtInfo),ncol=4,byrow=F,dimnames=list(rownames(WtInfo),c("DisImp","DisRsd","LanImp","LanRsd"))))
  WtSumm <- WtSumm[rev(order(WtSumm[,"LanImp"])),c("LanImp","DisImp","DisRsd")]
  WtPC <- 100*WtSumm/sum(WtSumm)
  
  WtBoth <- cbind(WtSumm,WtPC)
  names(WtBoth) <- c("LanTon","DisTonImp","DisTonRsd","LanPC","DisImpPC","DidRsdPC")
  WtBoth$DisRate <- 100*rowSums(WtBoth[,2:3])/rowSums(WtBoth[,1:3])
  
  return(WtBoth)
}