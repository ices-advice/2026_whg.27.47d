#######################################################################################
# SAM function needed updating

caytable_new<-function (fit, fleet = which(fit$data$fleetTypes == 0),useNMmodel = (fit$conf$mortalityModel >= 1)) 
{
  getfleet <- function(f) {
    idx <- fit$conf$keyLogFsta[f, ] + 2
    F <- cbind(NA, exp(t(fit$pl$logF)))[, idx]
    F[is.na(F)] <- 0
    if (useNMmodel) M <- exp(fit$pl$logNM[1:dim(fit$pl$logN)[2],])
    else M <- fit$data$natMor
    N <- exp(t(fit$pl$logN))
    F/(F + M) * N * (1 - exp(-F - M))
  }
  ret <- Reduce("+", lapply(fleet, getfleet))
  colnames(ret) <- fit$conf$minAge:fit$conf$maxAge
  rownames(ret) <- fit$data$years
  return(ret)
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

# xy<-function(subArea){
#   key<-c(E=-10,F=0,G=10,H=20)
#   txt<-as.character(cpue$SubArea)
#   x<-key[substr(txt,3,3)]+as.numeric(substr(txt,4,4))+.5
#   y<-(as.numeric(substr(txt,1,2))+71)/2+.25
#   cbind(x,y)
# }

xy<-function(subArea){ 
  key<-c(E=-10,F=0,G=10,H=20)
  txt<-as.character(cpue$SubArea)
  zz<-data.frame(array(NA,dim=c(length(txt),2)))
  colnames(zz) <- c("x","y")
  for(i in 1:length(txt)) { #i<-2
    if(nchar(txt[i])==4) {
      zz[i,"x"]<-key[substr(txt[i],3,3)]+as.numeric(substr(txt[i],4,4))+.5
      zz[i,"y"]<-(as.numeric(substr(txt[i],1,2))+71)/2+.25    
    }
    if(nchar(txt[i])==8) {
      zz[i,"x"]<-key[substr(txt[i],5,5)]+as.numeric(substr(txt[i],7,8))-.5
      zz[i,"y"]<-((as.numeric(substr(txt[i],1,3))*10)+71)/2+.25      
    }
  }
  zz
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

#######################################  plot.log.cpue  #############################

# Plot survey log catch per unit effort by age

plot.log.cpue <- function(wk.x, wk.do.legend = F, wk.ages = NULL, wk.lty = NULL, wk.col = NULL, wk.lwd = NULL) 
{
  
  wk.dim.x <- lapply(wk.x, dims)
  wk.y1 <- min(unlist(lapply(wk.dim.x, function(wk) wk$minyear)))
  wk.y2 <- max(unlist(lapply(wk.dim.x, function(wk) wk$maxyear)))
  wk.a1 <- min(unlist(lapply(wk.dim.x, function(wk) wk$min)))
  wk.a2 <- max(unlist(lapply(wk.dim.x, function(wk) wk$max)))
  
  if (is.null(wk.ages)) wk.ages <- wk.a1:wk.a2
  
  par(mfrow = c(3,2), bty = "l", mar = c(4,3,3,1), xpd = NA)
  
  for (wk.i in wk.ages)
  {
    wk.survs <- unlist(lapply(wk.x, function(wk, wk.i) wk.i %in% dims(wk)$min:dims(wk)$max, wk.i))
    wk.x.set <- wk.x[wk.survs]
    
    min.wk.x <- function(wk,wk.i) 
    {
      wk.y <- wk@index@.Data[paste(wk.i),,1,1,1,1]
      wk.y[wk.y == 0] <- NA
      min(wk.y, na.rm = T)
    }
    wk.min <- min(unlist(lapply(wk.x.set, min.wk.x ,wk.i)))
    wk.max <- max(unlist(lapply(wk.x.set, function(wk,wk.i) max(wk@index@.Data[paste(wk.i),,1,1,1,1], na.rm=T) ,wk.i)))
    wk.ylim <- log(c(wk.min, wk.max))
    
    if(is.null(wk.lty)) wk.lty <- c(1:6)
    if(is.null(wk.col)) wk.col <- c(2:7)
    if(is.null(wk.lwd)) wk.lwd <- 1
    
    plot(0, 0, type = "n", ylab = "", xlab = "", xlim = c(wk.y1,wk.y2),
         ylim = wk.ylim, main = paste("age",wk.i,sep=" "))
    if(wk.i>=4) title(xlab="Year")
    for (wk.j in 1:length(wk.x.set)) 
    {
      wk.y <- log(wk.x.set[[wk.j]]@index@.Data[paste(wk.i),,1,1,1,1])
      wk.xx <- as.numeric(names(wk.y))
      lines(wk.xx, wk.y, lwd = wk.lwd[wk.survs][wk.j], col = wk.col[wk.survs][wk.j], 
            lty = wk.lty[wk.survs][wk.j], type = "l", cex = 0.8)
    }
  }	
  
  if (wk.do.legend) 
  {
    plot(0, 0, xlab = "", ylab = "", xlim = c(0,1), ylim = c(0,1), axes = F, type = "n")
    legend(0, 1,
           legend = unlist(lapply(wk.x, function(wk) wk@name)),
           col = c(wk.col)[1:length(wk.x)], lwd = wk.lwd, 
           lty = c(wk.lty)[1:length(wk.x)], bty="n")
  }
}

####################################################################

# plots.R - DESC

# Copyright (c) WUR, 2023.
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

library(ggplotFL)

# plotAtAge {{{

#' @examples
#' data(ple4)
#' library(patchwork)
#' # Uses patchwork to paste both plots
#' (plotAtAge(stock.wt(ple4)) + ylab("Weight (kg)")) +
#' (plotAtAge(catch.sel(ple4)) + ylab("Selectivity"))

plotAtAge <- function(x, years=20, means=c(3, 5, 10, 20),
                      legend.position=c(0.8, 0.15)) {
  
  inp <- window(x, start=-years)
  
  dat <- FLQuants(c(divide(inp, dim=2), lapply(means, function(y)
    yearMeans(window(x, start=-y)))))
  
  names(dat) <- c(dimnames(inp)$year, paste("Mean", means, "years"))
  
  ggplot(dat, aes(x=factor(age), y=data, colour=qname, group=qname)) +
    geom_line(linewidth=rep(c(rep(0.5, years), rep(2, length(means))),
                            each=dim(x)[1]),
              alpha=rep(c(rep(0.5, years), rep(1, length(means))), each=dim(x)[1])) +
    xlab("Age") + ylab("") +
    scale_color_manual(
      name="",
      breaks=names(dat)[seq(years + 1, years + length(means))],
      values=unlist(setNames(c("blue", "red", "green", "cyan")[seq(length(means))],
                             nm=names(dat)[seq(years + 1, years + length(means))]))) +
    theme(legend.position=legend.position)
}
# }}}

############################### plot.surbar  #####################################

# General function to plot SURBAR outputs

plot.surbar <- function(wk.list, wk.type, nums=NULL)
{
  # Transfer data from input list to local variables
  wk.stock <- wk.list$s.stock
  wk.idx <- wk.list$s.idx
  wk.sumtab <- wk.list$s.sumtab
  wk.s <- wk.list$s.s
  wk.f <- wk.list$s.f
  wk.r <- wk.list$s.r
  wk.psim <- wk.list$s.psim
  wk.psim.s <- wk.list$s.psim.s
  wk.psim.f <- wk.list$s.psim.f
  wk.psim.r <- wk.list$s.psim.r
  wk.res <- wk.list$s.res
  wk.y1 <- wk.list$s.y1
  wk.y2 <- wk.list$s.y2
  wk.a1 <- wk.list$s.a1
  wk.a2 <- wk.list$s.a2
  
  # Determine number of indices
  wk.numk <- length(wk.res)
  if (is.null(nums)) nums <-(1:wk.numk)
  wk.ny <- wk.y2 - wk.y1 + 1
  wk.na <- wk.a2 - wk.a1 + 1
  
  # Define window
  if (wk.type == "sum.line" | wk.type == "sum.boxplot")
  {
    par(mfrow = c(2,2), mar = c(5,4,1,1)+0.1)
  } else if (wk.type == "res.line" | wk.type == "res.smooth")
  {
    config <- switch(wk.numk,
                     c(1,1), c(2,1), c(2,2), c(2,2), c(2,3), c(2,3), c(3,3), c(3,3), c(3,3))
    par(mfrow = config, mar = c(5,4,1,1)+0.1)
  } else if (wk.type == "catch.curve" | wk.type == "log.by.cohort")
  {
    config <- switch(wk.numk,
                     c(1,1), c(2,1), c(2,2), c(2,2), c(2,3), c(2,3), c(3,3), c(3,3), c(3,3))
    
    
    par(mfrow = config, mar = c(4,4,2,1)+0.1)
  } else if (wk.type == "params")
  {
    par(mfrow = c(2,3), mar = c(5,4,1,1)+0.1)
  } else if (wk.type == "age.scatterplot")
  {
    par(mfrow = c(2,1), mar = c(5,4,1,1)+0.1)
  }
  
  # Generate plots
  if (wk.type == "sum.line")
  {
    # Mean Z
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
    plot(wk.y1:wk.y2, wk.stock.meanz.quantile[,3], type = "n", lty = 1,
         xlab = "Year", ylab = "Mean Z", 
         ylim = c(min(0, wk.stock.meanz.quantile), max(wk.stock.meanz.quantile)))
    polygon(x = c(wk.y1:wk.y2, rev(wk.y1:wk.y2)), 
            y = c(wk.stock.meanz.quantile[,1], rev(wk.stock.meanz.quantile[,5])), density = -1, col = "grey",
            lty = 0)
    lines(wk.y1:wk.y2, wk.stock.meanz.quantile[,3], lty = 1, col = "black", lwd = 2)
    points(wk.sumtab$year, wk.stock.meanz.mean, pch = 3, col = "red")
    points(wk.sumtab$year, wk.sumtab$meanz, pch = 16, col = "green")
    legend(legend = c("NLS estimate", "Bootstrap mean", "Bootstrap median", "90% CI"), x = "bottomleft",
           pch = c(16,3,-1,-1), lty = c(-1,-1,1,1), lwd = c(-1,-1,2,5), bty = "n", col = c("green","red","black", "grey"))
    
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
    plot(wk.y1:wk.y2, wk.stock.ssb.quantile[,3], type = "n", lty = 1,
         xlab = "Year", ylab = "SSB", ylim = c(min(0, wk.stock.ssb.quantile), max(wk.stock.ssb.quantile)))
    polygon(x = c(wk.y1:wk.y2, rev(wk.y1:wk.y2)), 
            y = c(wk.stock.ssb.quantile[,1], rev(wk.stock.ssb.quantile[,5])), density = -1, col = "grey",
            lty = 0)
    lines(wk.y1:wk.y2, wk.stock.ssb.quantile[,3], lty = 1, col = "black", lwd = 2)
    points(wk.sumtab$year, wk.stock.ssb.mean, pch = 3, col = "red")
    points(wk.sumtab$year, wk.sumtab$ssb, pch = 16, col = "green")
    
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
    plot(wk.y1:wk.y2, wk.stock.tsb.quantile[,3], type = "n", lty = 1,
         xlab = "Year", ylab = "Total biomass", ylim = c(min(0, wk.stock.tsb.quantile), max(wk.stock.tsb.quantile)))
    polygon(x = c(wk.y1:wk.y2, rev(wk.y1:wk.y2)), 
            y = c(wk.stock.tsb.quantile[,1], rev(wk.stock.tsb.quantile[,5])), density = -1, col = "grey",
            lty = 0)
    lines(wk.y1:wk.y2, wk.stock.tsb.quantile[,3], lty = 1, col = "black", lwd = 2)
    points(wk.sumtab$year, wk.stock.tsb.mean, pch = 3, col = "red")
    points(wk.sumtab$year, wk.sumtab$tsb, pch = 16, col = "green")
    
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
    plot(wk.y1:wk.y2, wk.stock.rec.quantile[,3], type = "n", lty = 1,
         xlab = "Year", ylab = "Recruitment", ylim = c(min(0, wk.stock.rec.quantile), max(wk.stock.rec.quantile)))
    polygon(x = c(wk.y1:wk.y2, rev(wk.y1:wk.y2)), 
            y = c(wk.stock.rec.quantile[,1], rev(wk.stock.rec.quantile[,5])), density = -1, col = "grey",
            lty = 0)
    lines(wk.y1:wk.y2, wk.stock.rec.quantile[,3], lty = 1, col = "black", lwd = 2)
    points(wk.sumtab$year, wk.stock.rec.mean, pch = 3, col = "red")
    points(wk.sumtab$year, wk.sumtab$rec, pch = 16, col = "green")
  } else if (wk.type == "sum.boxplot")
  {
    # Mean Z
    wk.stock.meanz <- do.call(rbind, lapply(wk.psim, function(wk){wk$meanz}))
    wk.bp.meanz <- data.frame(wk.stock.meanz)
    names(wk.bp.meanz) <- wk.y1:wk.y2
    wk.bp.meanz <- stack(wk.bp.meanz)
    boxplot(values ~ ind, data = wk.bp.meanz, xlab = "Year", ylab = "Mean Z")
    
    # SSB
    wk.stock.ssb <- do.call(rbind, lapply(wk.psim, function(wk){wk$ssb}))
    wk.bp.ssb <- data.frame(wk.stock.ssb)
    names(wk.bp.ssb) <- wk.y1:wk.y2
    wk.bp.ssb <- stack(wk.bp.ssb)
    boxplot(values ~ ind, data = wk.bp.ssb, xlab = "Year", ylab = "SSB")
    
    # TSB
    wk.stock.tsb <- do.call(rbind, lapply(wk.psim, function(wk){wk$tsb}))
    wk.bp.tsb <- data.frame(wk.stock.tsb)
    names(wk.bp.tsb) <- wk.y1:wk.y2
    wk.bp.tsb <- stack(wk.bp.tsb)
    boxplot(values ~ ind, data = wk.bp.tsb, xlab = "Year", ylab = "Total biomass")
    
    # Recruitment
    wk.stock.rec <- do.call(rbind, lapply(wk.psim, function(wk){wk$rec}))
    wk.bp.rec <- data.frame(wk.stock.rec)
    names(wk.bp.rec) <- wk.y1:wk.y2
    wk.bp.rec <- stack(wk.bp.rec)
    boxplot(values ~ ind, data = wk.bp.rec, xlab = "Year", ylab = "Recruitment")
  } else if (wk.type == "res.line")
  {
    wk.ymin <- min(as.numeric(unlist(wk.res)), na.rm = TRUE)
    wk.ymax <- max(as.numeric(unlist(wk.res)), na.rm = TRUE)
    
    for (wk.k in 1:wk.numk)
    {
      wk.y1.a <- as.numeric(rownames(wk.res[[wk.k]])[1])
      wk.y2.a <- as.numeric(rev(rownames(wk.res[[wk.k]]))[1])
      
      plot (wk.y1:wk.y2, rep(0, wk.ny), xlab = "Year", ylab = "Log residual",
            ylim = c(wk.ymin, wk.ymax), type = "n")
      for (wk.i in 1:dim(wk.res[[wk.k]])[2])
      {
        lines(wk.y1.a:wk.y2.a, wk.res[[wk.k]][,wk.i], col = wk.i)
      }
      abline(h = 0, col = 1)
      legend(x = "topleft", legend = names(wk.res)[wk.k], bty = "n", cex = 0.75)	
      legend(x = "bottomleft", legend = paste("Age", names(wk.res[[wk.k]])), lty = 1, col = 1:dim(wk.res[[wk.k]])[2],
             bty = "n", cex = 0.75, ncol = 2)
    }
  } else if (wk.type == "res.smooth")
  {
    wk.ymin <- min(as.numeric(unlist(wk.res)), na.rm = TRUE)
    wk.ymax <- max(as.numeric(unlist(wk.res)), na.rm = TRUE)
    
    for (wk.k in 1:wk.numk)
    {
      wk.y1.a <- as.numeric(rownames(wk.res[[wk.k]])[1])
      wk.y2.a <- as.numeric(rev(rownames(wk.res[[wk.k]]))[1])
      
      plot (wk.y1:wk.y2, rep(0, wk.ny), xlab = "Year", ylab = "Log residual",
            ylim = c(wk.ymin, wk.ymax), type = "n")
      for (wk.i in 1:dim(wk.res[[wk.k]])[2])
      {
        if (length(wk.res[[wk.k]][!is.na(wk.res[[wk.k]][,wk.i]),wk.i]) > 0)
        {
          points(wk.y1.a:wk.y2.a, wk.res[[wk.k]][,wk.i], pch = 3, col = wk.i)
          wk.data.loess <- data.frame(x = wk.y1.a:wk.y2.a, y = wk.res[[wk.k]][,wk.i])
          wk.res.loess <- loess(y ~ x, data = wk.data.loess, span = 1.0)
          wk.res.loess.pred.x <- seq(wk.y1.a, wk.y2.a, length = 100)
          wk.res.loess.pred <- predict(wk.res.loess, newdata = wk.res.loess.pred.x, se = TRUE)
          lines(wk.res.loess.pred.x, wk.res.loess.pred$fit, col = wk.i, lty = 1)
        }
      }
      abline(h = 0, col = 1)
      legend(x = "topleft", legend = names(wk.res)[wk.k], bty = "n", cex = 0.75)	
      legend(x = "bottomleft", legend = paste("Age", names(wk.res[[wk.k]])), lty = 1, col = 1:dim(wk.res[[wk.k]])[2],
             bty = "n", cex = 0.75, ncol = 2)
    }
  } else if (wk.type == "params")
  {
    # Boxplots: s
    wk.bp.s <- data.frame(wk.psim.s)
    names(wk.bp.s) <- wk.a1:wk.a2
    wk.bp.s <- stack(wk.bp.s)
    boxplot(values ~ ind, data = wk.bp.s, xlab = "Age", ylab = "s")
    
    # Boxplots: f
    wk.bp.f <- data.frame(wk.psim.f)
    names(wk.bp.f) <- wk.y1:wk.y2
    wk.bp.f <- stack(wk.bp.f)
    boxplot(values ~ ind, data = wk.bp.f, xlab = "Year", ylab = "f")
    
    # Boxplots: r
    wk.bp.r <- data.frame(wk.psim.r)
    names(wk.bp.r) <- (wk.y1 - wk.na + 1 - wk.a1):(wk.y2 - wk.a1)
    wk.bp.r <- stack(wk.bp.r)
    boxplot(values ~ ind, data = wk.bp.r, xlab = "Cohort", ylab = "r")
    
    # Lineplots: s
    wk.stock.s.quantile <- array(NA, dim = c(dim(wk.psim.s)[2],5))
    wk.stock.s.mean <- rep(NA, dim(wk.psim.s)[2])
    rownames(wk.stock.s.quantile) <- wk.a1:wk.a2
    colnames(wk.stock.s.quantile) <- c("5%","25%","50%","75%","95%")
    for (wk.i in 1:dim(wk.psim.s)[2])
    {
      wk.stock.s.quantile[wk.i,] <- quantile(wk.psim.s[,wk.i], c(0.05, 0.25, 0.5, 0.75, 0.95))
      wk.stock.s.mean[wk.i] <- mean(wk.psim.s[,wk.i])
    }
    plot(wk.a1:wk.a2, wk.stock.s.quantile[,3], type = "n", lty = 1,
         xlab = "Age", ylab = "s", ylim = c(min(0, wk.stock.s.quantile), max(wk.stock.s.quantile)))
    polygon(x = c(wk.a1:wk.a2, rev(wk.a1:wk.a2)), 
            y = c(wk.stock.s.quantile[,1], rev(wk.stock.s.quantile[,5])), density = -1, col = "grey",
            lty = 0)
    lines(wk.a1:wk.a2, wk.stock.s.quantile[,3], lty = 1, col = "black", lwd = 2)
    points(wk.a1:wk.a2, wk.stock.s.mean, pch = 3, col = "red")
    points(wk.a1:wk.a2, wk.s, pch = 16, col = "green")
    legend(legend = c("NLS estimate", "Bootstrap mean", "Bootstrap median", "90% CI"), x = "bottomright",
           pch = c(16,3,-1,-1), lty = c(-1,-1,1,1), lwd = c(-1,-1,2,5), bty = "n", col = c("green","red","black", "grey"))
    
    # Lineplots: f
    wk.stock.f.quantile <- array(NA, dim = c(dim(wk.psim.f)[2],5))
    wk.stock.f.mean <- rep(NA, dim(wk.psim.f)[2])
    rownames(wk.stock.f.quantile) <- wk.y1:wk.y2
    colnames(wk.stock.f.quantile) <- c("5%","25%","50%","75%","95%")
    for (wk.i in 1:dim(wk.psim.f)[2])
    {
      wk.stock.f.quantile[wk.i,] <- quantile(wk.psim.f[,wk.i], c(0.05, 0.25, 0.5, 0.75, 0.95))
      wk.stock.f.mean[wk.i] <- mean(wk.psim.f[,wk.i])
    }
    plot(wk.y1:wk.y2, wk.stock.f.quantile[,3], type = "n", lty = 1,
         xlab = "Year", ylab = "f", ylim = c(min(0, wk.stock.f.quantile), max(wk.stock.f.quantile)))
    polygon(x = c(wk.y1:wk.y2, rev(wk.y1:wk.y2)), 
            y = c(wk.stock.f.quantile[,1], rev(wk.stock.f.quantile[,5])), density = -1, col = "grey",
            lty = 0)
    lines(wk.y1:wk.y2, wk.stock.f.quantile[,3], lty = 1, col = "black", lwd = 2)
    points(wk.y1:wk.y2, wk.stock.f.mean, pch = 3, col = "red")
    points(wk.y1:wk.y2, wk.f, pch = 16, col = "green")
    
    # Lineplots: r
    wk.r1 <- wk.y1 - wk.na + 1 - wk.a1
    wk.r2 <- wk.y2 - wk.a1
    wk.stock.r.quantile <- array(NA, dim = c(dim(wk.psim.r)[2],5))
    wk.stock.r.mean <- rep(NA, dim(wk.psim.r)[2])
    rownames(wk.stock.r.quantile) <- wk.r1:wk.r2
    colnames(wk.stock.r.quantile) <- c("5%","25%","50%","75%","95%")
    for (wk.i in 1:dim(wk.psim.r)[2])
    {
      wk.stock.r.quantile[wk.i,] <- quantile(wk.psim.r[,wk.i], c(0.05, 0.25, 0.5, 0.75, 0.95))
      wk.stock.r.mean[wk.i] <- mean(wk.psim.r[,wk.i])
    }
    plot(wk.r1:wk.r2, wk.stock.r.quantile[,3], type = "n", lty = 1,
         xlab = "Cohort", ylab = "r", ylim = c(min(0, wk.stock.r.quantile), max(wk.stock.r.quantile)))
    polygon(x = c(wk.r1:wk.r2, rev(wk.r1:wk.r2)), 
            y = c(wk.stock.r.quantile[,1], rev(wk.stock.r.quantile[,5])), density = -1, col = "grey",
            lty = 0)
    lines(wk.r1:wk.r2, wk.stock.r.quantile[,3], lty = 1, col = "black", lwd = 2)
    points(wk.r1:wk.r2, wk.stock.r.mean, pch = 3, col = "red")
    points(wk.r1:wk.r2, wk.r, pch = 16, col = "green")
    abline(v = wk.y1 - wk.a1 - 0.5, lty = 8, col = "blue")
    
  } else if (wk.type == "catch.curve")
  {
    # Set plot limits to cover all cohorts in all series
    
    wk.lbc.xmin <-  min(as.numeric(row.names(wk.idx[[1]])), na.rm=T)
    wk.lbc.xmax <- max(as.numeric(row.names(wk.idx[[1]])), na.rm=T)
    
    for (wk.k in nums)
    {
      
      wk.cc.idx <- na.omit(wk.idx[[wk.k]])
      wk.cc.years <- as.numeric(row.names(wk.cc.idx))
      wk.cc.ages <- as.numeric(names(wk.cc.idx))
      
      wk.cc <- stack(wk.cc.idx)
      wk.cc$ind <- as.numeric(as.character(wk.cc$ind))
      wk.cc <- data.frame(cbind(wk.cc, wk.cc.years))
      names(wk.cc) <- c("index","age","year")
      wk.cc <- data.frame(cbind(wk.cc, wk.cc$year - wk.cc$age))
      names(wk.cc) <- c("index","age","year","cohort")
      
      wk.cc.cohorts <- unique(wk.cc$cohort)[order(unique(wk.cc$cohort))]
      plot(x = 0, y = 0, 
           xlim = c(wk.lbc.xmin, wk.lbc.xmax),
           ylim = c(min(log(wk.cc.idx), na.rm = TRUE), max(log(wk.cc.idx), na.rm = TRUE) + 0.25),
           type = "n", xlab = "Year", ylab = "Log survey index",
           main = names(wk.idx)[wk.k])
      
      wk.jj <- 0
      for (wk.j in wk.cc.cohorts)
      {
        wk.jj <- wk.jj + 1
        wk.cc0 <- wk.cc[wk.cc$cohort == wk.j,]
        lines(wk.cc0$year, log(wk.cc0$index), type = "l", lty = 1, pch = -1,
              col = rainbow(n = length(wk.cc.cohorts))[wk.jj])
        text(x = wk.cc0$year[1], y = log(wk.cc0$index)[1] + 0.25, wk.cc0$cohort[1], cex = 0.75)
      }
    }
  } else if (wk.type == "log.by.cohort")
  {
    
    # Set plot limits to cover all cohorts in all series
    
    wk.lbc.idx <- wk.idx
    wk.lbc.xmin <- 9999
    wk.lbc.xmax <- -9999
    for (wk.k in nums)
    {
      wk.lbc.idx[[wk.k]] <- na.omit(wk.idx[[wk.k]])
      wk.xmin <- min(as.numeric(rownames(wk.lbc.idx[[wk.k]]))) - max(as.numeric(names(wk.lbc.idx[[wk.k]])))
      wk.xmax <- max(as.numeric(rownames(wk.lbc.idx[[wk.k]])))
      if (wk.xmin < wk.lbc.xmin)
      {
        wk.lbc.xmin <- wk.xmin
      }
      if (wk.xmax > wk.lbc.xmax)
      {
        wk.lbc.xmax <- wk.xmax
      }
    }
    
    
    
    for (wk.k in nums)
    {
      
      wk.lbc <- wk.lbc.idx[[wk.k]]
      
      # Mean standardise by age
      for (wk.kk in 1:dim(wk.lbc)[2])
      {
        wk.lbc[,wk.kk] <- wk.lbc[,wk.kk] / mean(wk.lbc[,wk.kk])
      }
      
      # Take logs
      wk.lbc <- log(wk.lbc)
      
      wk.lbc.ages <- as.numeric(names(wk.lbc))
      wk.lbc.years <- as.numeric(rownames(wk.lbc))
      
      # Generate stacked dataframe
      wk.lbc <- stack(wk.lbc)
      wk.lbc$ind <- as.numeric(as.character(wk.lbc$ind))
      wk.lbc <- data.frame(cbind(wk.lbc, wk.lbc.years))
      names(wk.lbc) <- c("index","age","year")
      wk.lbc <- data.frame(cbind(wk.lbc, wk.lbc$year - wk.lbc$age))
      names(wk.lbc) <- c("index","age","year","cohort")
      
      wk.lbc.cohorts <- unique(wk.lbc$cohort)[order(unique(wk.lbc$cohort))]
      plot(x = 0, y = 0, 
           xlim = c(wk.lbc.xmin, wk.lbc.xmax),
           ylim = c(min(wk.lbc$index, na.rm = TRUE), max(wk.lbc$index, na.rm = TRUE)),
           type = "n", xlab = "Cohort", ylab = "Mean-std log survey index",
           main = names(wk.idx)[wk.k])
      
      wk.jj <- 0
      for (wk.j in wk.lbc.ages)
      {
        wk.jj <- wk.jj + 1
        wk.lbc0 <- wk.lbc[wk.lbc$age == wk.j,]
        lines(wk.lbc0$cohort, wk.lbc0$index, type = "l", lty = 1, 
              col = rainbow(n = length(wk.lbc.ages))[wk.jj])
      }
      wk.jj <- 0
      for (wk.j in wk.lbc.ages)
      {
        wk.jj <- wk.jj + 1
        wk.lbc0 <- wk.lbc[wk.lbc$age == wk.j,]
        points(x = wk.lbc0$cohort[1], y = wk.lbc0$index[1], pch = 16, cex = 2.5, col = "white")
        points(x = wk.lbc0$cohort[1], y = wk.lbc0$index[1], pch = 1, cex = 2.5, col = rainbow(n = length(wk.lbc.ages))[wk.jj])
        text(x = wk.lbc0$cohort[1], y = wk.lbc0$index[1], wk.lbc0$age[1], cex = 0.75)
      }
    }
  } else if (wk.type == "age.scatterplot")
  {
    for (wk.k in nums)
    {
      # windows()
      plot.index.corr(wk.idx[wk.k], wk.type = "SURBAR")
    }
  }
}

####################################################################
####################################################################


################################## plot.index.corr ##########################################

plot.index.corr <- function(wk.object, wk.type) 
{
  # par(bty="n")
  # trellis.par.set(box.rectangle = list(col = "white"))
  for (wk.i in seq(length(wk.object))) 
  {
    # Select one tuning fleet
    if (wk.type == "FLR")
    {
      wk.tune.mat <- t(wk.object[[wk.i]]@catch.n@.Data[,,1,1,1,1])
      wk.main <- wk.object[[wk.i]]@name
    } else
    {
      wk.tune.mat <- wk.object[[wk.i]]
      wk.main <- names(wk.object)
    }
    # Make cohort matrix
    if(is.na(wk.tune.mat[1,1])) wk.tune.mat<-wk.tune.mat[,]
    wk.n <- dim(wk.tune.mat)[2]
    wk.cohort.mat <- matrix(NA, ncol = wk.n, nrow = dim(wk.tune.mat)[1] + wk.n - 1)
    colnames(wk.cohort.mat) <- colnames(wk.tune.mat)
    for (wk.j in 1:wk.n) 
    {
      wk.cohort.mat[,wk.j] <- c(rep(NA,wk.n-wk.j), 
                                wk.tune.mat[,wk.j], rep(NA,wk.j-1))
      
    }
    
   return(splom(~centre.log(wk.cohort.mat), superpanel = panel.pairs.cm, xlab = wk.main, col = "white"))
  }
}


