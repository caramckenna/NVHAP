require(ggplot2)
require(data.table)
require(scales)

setwd("/Users/jessicayoung/Documents/Documents/CDC_NVHAP")

#hazrisks<-readRDS("haz_cums_bysite_50_CDCinterim_0421old.rds")
#hazrisks<-readRDS("haz_cums_bysite_COMBINED.rds")
hazrisks<-readRDS("haz_cums_bysite_COMBINED_Jan.rds")
cis<-readRDS("CIs_forCumIncs_Feb.rds")

#statistics that can be reported in tables with 95% cis (will create separate program for bootstrap): abs risk by end of follow-up under intervention, relative risks, risk differences
#First element is intervention [[1]] or natural course [[2]]
#Second element is stratum with [[1]] overall
#Third element is death ci [[3]] or discharge ci [[4]]

risksdeathint<-hazrisks[[1]][[1]][[3]]
#endriskdeathint<-risksdeathint[length(risksdeathint)]

risksdeathnc<-hazrisks[[2]][[1]][[3]]
#endriskdeathnc<-risksdeathnc[length(risksdeathnc)]

#rrdeath<-endriskdeathint/endriskdeathnc
#rddeath<-endriskdeathint-endriskdeathnc
#endriskdeathint
#endriskdeathnc
#rrdeath
#rddeath

risksdischargeint<-hazrisks[[1]][[1]][[4]]
#endriskdischargeint<-risksdischargeint[length(risksdischargeint)]

risksdischargenc<-hazrisks[[2]][[1]][[4]]
#endriskdischargenc<-risksdischargenc[length(risksdischargenc)]

#rrdischarge<-endriskdischargeint/endriskdischargenc
#rddischarge<-endriskdischargeint-endriskdischargenc
#endriskdischargeint
#endriskdischargenc
#rrdischarge
#rddischarge




cutTimes<-seq(3:60)


cildeathint<-cis[[1]][[1]][[1]][1,]
ciudeathint<-cis[[1]][[1]][[1]][2,]
  
cildeathnc<-cis[[2]][[1]][[1]][1,]
ciudeathnc<-cis[[2]][[1]][[1]][2,]


cildischargeint<-cis[[1]][[2]][[1]][1,]
ciudischargeint<-cis[[1]][[2]][[1]][2,]


cildischargenc<-cis[[2]][[2]][[1]][1,]
ciudischargenc<-cis[[2]][[2]][[1]][2,]

#cbind(cildeathint,risksdeathint,ciudeathint)
#cbind(cildischargeint,risksdischargeint,ciudischargeint)


#plot of mortality risk curves over time under intervention versus natural course
#pdf("Deathrisks_revise.pdf")
setEPS()
postscript("Deathrisks_revise.eps")
plot(cutTimes,risksdeathint, type="l",ylim=c(0,.02), ylab="Estimated Cumulative Incidence (Risk)", 
     xlab="Days from admission",lty=1,lwd=1, xaxt="n", cex.lab=1.2,cex.main=1.2)
axis(1, at=c(3,15,30,45,60))
lines(cutTimes, cildeathint, type="l",col=1,ylim=c(0,.02),lty="dashed",lwd=1)
lines(cutTimes, ciudeathint, type="l",col=1,ylim=c(0,.02),lty="dashed", lwd=1)
lines(cutTimes, risksdeathnc, type="l",col=2,ylim=c(0,.023),lwd=1)
lines(cutTimes, cildeathnc, type="l",col=2,ylim=c(0,.02),lty="dashed",lwd=1)
lines(cutTimes, ciudeathnc, type="l",col=2,ylim=c(0,.02),lty="dashed", lwd=1)
legend(x=23, y= .012, c("Eliminate NV-HAP","Current Care"),
       col=c(1,2), lty=c(1,1),cex=1.2,pt.cex=1.2,lwd=2)
dev.off()



#pdf("Dischargerisks_revise.pdf")
setEPS()
postscript("Dischargerisks_revise.eps")
plot(cutTimes,risksdischargeint, type="l",ylim=c(0.2,1), ylab="Estimated Cumulative Incidence (Risk)", 
     xlab="Days from admission",lty=1,lwd=1,xaxt="n", cex.lab=1.2,cex.main=1.2) #main =c(paste("IPW cumulative discharge risk estimates and 95% CIs (dashed lines)"),paste("Eliminate NV-HAP versus Existing Care"))) 
axis(1, at=c(3,15,30,45,60))
lines(cutTimes, cildischargeint, type="l",col=1,ylim=c(0,.02),lty="dashed",lwd=1)
lines(cutTimes, ciudischargeint, type="l",col=1,ylim=c(0,.02),lty="dashed", lwd=1)
lines(cutTimes, risksdischargenc, type="l",col=2,ylim=c(0.2,1),lwd=1)
lines(cutTimes, cildischargenc, type="l",col=2,ylim=c(0,.02),lty="dashed",lwd=1)
lines(cutTimes, ciudischargenc, type="l",col=2,ylim=c(0,.02),lty="dashed", lwd=1)
legend(x=23, y= .6, c("Eliminate NV-HAP","Current Care"),
       col=c(1,2), lty=c(1,1),cex=1.2,pt.cex=1.2,lwd=2)
dev.off()
