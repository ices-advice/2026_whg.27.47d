
load("model/model.Rdata")

load("model/whg.27.47d_FLStock_input_data.Rdata")
stkInp <- trim(stk_dat, year=1978:datayear)
load("model/whg.27.47d_FLStock_model_estimates.Rdata")
stkEst <- trim(stk_fit, year=1978:datayear)


L <- FLStocks(list(input = stkInp, estimated = stkEst))

taf.png("report/Mixfish/Mixfish_1_stock.png") 
plot(L) + 
  aes(linetype = stock) +
  scale_color_manual(values = c(8,1)) + 
  scale_linetype_manual(values = c(1,2)) +
  theme_bw()
dev.off()

# forecast year definitions
yrAssess <- datayear # final year of assessment data
yrNow <- yrAssess+1 # intermediate year
yrTAC <- yrAssess+2 # advice year
yrTACp1 <- yrAssess+3 # advice year +1 (needed to get SSB at end of yrTAC)

# extend FLStock object
stkProj <- stf(object = stkEst, nyears = 3, wts.nyears = 3, 
               fbar.nyears = 3, f.rescale = TRUE, disc.nyears = 3)

# replace nm

ay<-year
nm <- exp(fit$pl$logNM)
nm<-cbind(nm,1978:(1977+(dim(nm)[1])))
stkProj@m[,as.character(ay)]<-nm[which(nm[,10]==ay),1:9]
stkProj@m[,as.character(ay+1)]<-nm[which(nm[,10]==(ay+1)),1:9]
stkProj@m[,as.character(ay+2)]<-nm[which(nm[,10]==(ay+2)),1:9]

#geometric mean since 1983, ignore intermediate year
rec<-as.data.frame(rec(stkEst))
geom<-exp(mean(log(rec$data[6:(length(rec$data)-1)])))

# stock-recruitment model (manual input within a geometric mean model)
srPar <- FLPar(c(geom, geom, geom), 
               dimnames = list(params="a", year = c(yrNow, yrTAC, yrTACp1), iter = 1))
srMod <- FLSR(model = "geomean", params = srPar)

# View the extended FLStock
df <- as.data.frame(stkProj)
df <- subset(df, slot %in% c("landings.wt", "discards.wt", "catch.wt", "m", "mat", "harvest") & year > (yrAssess-20))
df$forecast <- df$year %in% c(yrNow, yrTAC, yrTACp1)



taf.png("report/Mixfish/Mixfish_2_inputs") 
ggplot(df) + aes(x = year, y = data, group = age, color = forecast) +
  facet_wrap(~slot, scales = "free_y") +
  geom_line(show.legend = F) + 
  scale_color_manual(values = c(8,1)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + 
  theme_bw()
dev.off()

# stf control (Fsq, followed by 2 years at Fmsy)
Fmsy <- 0.35
ctrl <- fwdControl( 
  data.frame(
    year = c(yrNow, yrTAC, yrTACp1),
    value = c(1, Fmsy, Fmsy),
    quant = c("f"),
    relYear = c(yrAssess, NA, NA)                               
  )
)

# projection
stkProj <- fwd(object = stkProj, control = ctrl, sr = srMod)

# plot
L <- FLStocks(list(assessment = stkEst, forecast = stkProj[,ac(yrAssess:yrTACp1)]))

taf.png("report/Mixfish/Mixfish_3_proj") 
plot(L) + 
  # aes(linetype = stock) +
  scale_color_manual(values = c(8,1)) + 
  # scale_linetype_manual(values = c(2,1)) +
  theme_bw()
dev.off()

df <- data.frame(year = yrAssess:yrTACp1, 
                 catch = c(catch(stkProj)[, ac(yrAssess:yrTACp1)]),
                 fbar = c(fbar(stkProj)[, ac(yrAssess:yrTACp1)])
)

kable(df, digits = 3)


load("model/forecast_new.RData")
fc<-FC[[1]]
# Reported output from single stock headline advice
stfRef <- data.frame(
  model = "NSSK",
  year =c(yrAssess:yrTACp1),
  catch = c(attributes(fc)$tab[,"catch:median"][[1]], attributes(fc)$tab[,"catch:median"][[2]], attributes(fc)$tab[,"catch:median"][[3]], NA),  # estimated catches
  landings = c(attributes(fc)$tab[,"Land:median"][[1]], attributes(fc)$tab[,"Land:median"][[2]], attributes(fc)$tab[,"Land:median"][[3]], NA),  # here wrong estimated landings in object, but correct landings for follwoing years
  fbar = c(attributes(fc)$tab[,"fbar:median"][[1]], attributes(fc)$tab[,"fbar:median"][[2]], attributes(fc)$tab[,"fbar:median"][[3]], NA),
  ssb = c(attributes(fc)$tab[,"ssb:median"][[1]], attributes(fc)$tab[,"ssb:median"][[2]], attributes(fc)$tab[,"ssb:median"][[3]], attributes(fc)$tab[,"ssb:median"][[4]])
)

stfDet <- data.frame(
  model = "FLR",
  year = ac(yrAssess:yrTACp1),
  catch = c(catch(stkProj[,ac(yrAssess:yrTACp1)])),
  landings = c(landings(stkProj[,ac(yrAssess:yrTACp1)])),
  fbar = c(fbar(stkProj[,ac(yrAssess:yrTACp1)])),
  ssb = c(ssb(stkProj[,ac(yrAssess:yrTACp1)]))
)  

df <- merge(stfRef, stfDet, all = T)
df <- pivot_longer(df, cols = c(catch, landings, fbar, ssb), 
                   names_to = "variable", values_to = "value")
df <- df |>
  filter(
    (variable %in% c("catch", "landings", "fbar") & year <= yrTAC) |
      (variable %in% c("ssb") & year <= yrTACp1))

df2 <- pivot_wider(df, names_from = model, values_from = value)
df2$percErr <- round((df2$FLR - df2$NSSK)/df2$NSSK * 100, 1)

taf.png("report/Mixfish/Mixfish_4_compare") 
ggplot(df) + aes(x = year, y = value, group = model, color = model, shape = model) +
  facet_wrap(~variable, scales = "free_y") +
  geom_line() +
  geom_point(size = 3, stroke = 1) +
  scale_shape_discrete(solid = F) +
  coord_cartesian(ylim = c(0, NA)) +
  theme_bw()
dev.off()

kable(df2, digits = 3)
kable(df2, digits = 3)%>%save_kable("report/Mixfish/table_output.txt")


taf.png("report/Mixfish/Mixfish_5_compare diff") 
ggplot(df2) + aes(x = year, y = percErr) + 
  facet_wrap(~variable) +
  geom_col(fill = 4, color = 4) +
  geom_hline(yintercept = 0, linetype = 1) +
  geom_hline(yintercept = c(-10,10), linetype = 3) + 
  theme_bw()

dev.off()


