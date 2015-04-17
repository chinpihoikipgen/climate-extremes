#read in climate_raw
load("/Users/chinpihoikipgen/Desktop/climate_clean/data/raw/climate/climate_raw.Rda")
#master =read.csv(file.choose())

library(data.table); library(plyr)
months = c("Jan","Feb", "Mar", "Apr", "May", 
"June", "July", "Aug", "Sep", "Oct", "Nov", "Dec")
fips = master$FIPS; fips=fips[which(!is.na(fips))] 
years = seq(1949, 2010);sep =c( seq(1, 744, by = 12))


climate_raw2 = vector("list", length = length(climate_raw))
for(s in 1:length(climate_raw)){
        ds = climate_raw[[s]]
        ds2 = split(ds, cumsum(1:nrow(ds)%in%sep))
        ds3= lapply(ds2, function(x)t(x))
        ds4 = lapply(seq(length(ds3)), function(i){data.frame(cbind(ds3[[i]], fips=fips, year = years[i]))})
        ds5 = as.data.frame(data.table::rbindlist(ds4))
        colnames(ds5)[1:12]= months
        ds5$climate = names(climate_raw)[s]
        climate_raw2[[s]] = ds5
}

climate_raw2 = as.data.frame(data.table::rbindlist(climate_raw2))
setwd("/Users/chinpihoikipgen/Desktop/climate_clean/data/raw/climate")
save(climate_raw2, file = "climate_raw2.Rda")






			
			