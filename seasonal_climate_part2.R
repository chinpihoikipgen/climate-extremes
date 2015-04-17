#seasonal means for each crop
library(data.table); library(plyr); library(reshape)

load("/Users/chinpihoikipgen/Desktop/climate_clean/data/normalised/clim_norm.Rda")
clim = split(clim_norm, f =clim_norm$state)
clim =Filter(function(x) NROW(x)>0, clim)
setwd('/Users/chinpihoikipgen/Desktop/climate_clean/data/raw/master list')
season = read.csv("PlantingHarvesting.csv")
counties = read.csv("County_Master.csv")
crops = unique(season$crop)
set2 = list()
#corn
for(i in 1:length(clim)){
	data = clim[[i]]
	set1 = list()
		for(s in 1:length(crops)){
			if(!crops[s]=="ww"){
				data2= seasonal_means_nw(x = data, y=unique(crops)[s])
			} else{	
				data2= seasonal_means_ww(x = data, y=unique(crops)[s])
			}
			nn1 = c("climate","state","fips","growing","planting","year")
			data3 = data2[,colnames(data2)%in%nn1]
			colnames(data3)[which(colnames(data3)=="growing")] = paste(crops[s],"growing",sep="_")
			colnames(data3)[which(colnames(data3)=="planting")] =paste(crops[s],"planting",sep="_")
			
			set1[[s]] = data3		
		}
		#merge all growing
		data4 = Reduce(function(x,y) merge(x,y, all = TRUE, 
		by = c("fips","state","year","climate")), set1, accumulate=FALSE)
		set2[[i]] = data4		
}


setwd("/Users/chinpihoikipgen/Desktop/climate_clean/data/normalised")

seasonal_means = as.data.frame(data.table::rbindlist(set2))
save(seasonal_means, file = "seasonal_means.Rda")