#normalising climate indicators using 7 year window centered on observation
#Y = y-mean(y(t+3:t-3))/std(y(t+3:t-3))

library(plyr); library(zoo); library(reshape)
load("/Users/chinpihoikipgen/Desktop/climate_clean/data/raw/climate/climate_raw2.Rda")
#data = climate_raw2[order(climate_raw2$year),]
#climate_split = split(data, f = list(data$fips,data$climate))
#data_norm_all =ddply(data, .(fips,climate),transform,m= rollapply(dat[,c(1:12)],7, mean, fill=NA),s=rollapply(dat[,c(1:12)],7, sd, fill=NA)) takes too long
#save(data_norm_all, file = "data_norm_all.Rda")

#method2
load("/Users/chinpihoikipgen/Desktop/climate_clean/data/raw/climate/climate_split.Rda")

data_norm_complete=list()
for(i in 1:length(climate_split)){
    dat = climate_split[[i]]
    datm =rollapply(dat[,c(1:12)],7, mean, na.rm = TRUE, fill=NA)
    dats =rollapply(dat[,c(1:12)],7, sd, na.rm  =TRUE,fill=NA)
    dat[,c(1:12)] = (dat[,c(1:12)] - datm)/(dats)
    data_norm_complete[[i]] = dat
}

climate_norm_complete = data_norm_complete

setwd("/Users/chinpihoikipgen/Desktop/climate_clean/data/normalised")
save(climate_norm_complete, file = "climate_norm.Rda")



#faster method

#normalising climate data
library(caTools); library(data.table)
new=list()
for(i in 1:length(split_copy)){
    dat = split_copy[[i]]
    dat = dat[order(dat$year),]
    x=as.matrix(dat[,c(1:12)])
    xx = runmean(x,k, endrule="mean", alg = "C")
    xx2 = runsd(x,k, endrule="NA")
    xx_cal = (x-xx)/(xx2)
    xx_cal = data.frame(xx_cal)
    xx = cbind(xx_cal, dat[,c(13:15)])
    #xx[c(1:3), c(1:12)] = NA
    #xx[c((NROW(xx)-2):NROW(xx)), c(1:12)] =NA
    new[[i]]=xx
    
}

climate_norm = as.data.frame(data.table::rbindlist(new))
states= read.csv("County_Master.csv")
states = states[,c(3,6)]; colnames(states)=c("state", "fips")
clim_norm = merge(clim_norm, states, by = "fips")
save(clim_norm, file = "clim_norm.Rda")






