#---------------------------- hd plots ----------------------

#load("/Users/chinpihoikipgen/Desktop/climate_clean/data/final_merged_sets/data_final.Rda")
library(hdrcde)
data = data_final
hist(data$yield)
#remove outlier - fips = 35023
data = data[data$yield<300,]
data = data[!data$climate =="nfrost",] # we didnt end up looking at look at nfrost for the paper 
data =split(data,list(data$climate, data$method, data$crop))
data =data[sapply(data, function(x) dim(x)[1]) > 0]

data_planting = list()
data_growing = list()

for(i in 1:length(data)){
	dat1 = data[[i]]
	plant = cde(dat1$planting, dat1$norm_yield,x.name = "", y.name = "")
	grow = cde(dat1$growing, dat1$norm_yield,x.name = "", y.name = "")
	data_planting[[i]] = plant
	data_growing[[i]] = grow

}
names(data_planting) = names(data)
names(data_growing) = names(data)

data_growingHDRCDE = data_growing
data_plantingHDRCDE = data_planting

setwd('/Users/chinpihoikipgen/Desktop/climate_clean/data/final_merged_sets')
save(data_plantingHDRCDE, file = "data_plantingHDRCDE.Rda")
save(data_growingHDRCDE, file = "data_growingHDRCDE.Rda")

setwd("/Users/chinpihoikipgen/Desktop/climate_clean/plots/planting")

for (i in 1:length(data_plantingHDRCDE)){  
   png(paste("plot_",names(data_plantingHDRCDE)[[i]],".png",sep = ""),width = 300, height = 300) 
   par(cex.axis=1.65)

   plot(data_plantingHDRCDE[[i]] ,plot.fn="hdr", xlim = c(-2.5,2.5))
   dev.off()
}


setwd("/Users/chinpihoikipgen/Desktop/climate_clean/plots/growing")

for (i in 1:length(data_growingHDRCDE)){  
   png(paste("plot_",names(data_growingHDRCDE)[[i]],".png",sep = ""),width = 300, height = 300) 
   par(cex.axis=1.65)

   plot(data_growingHDRCDE[[i]] ,plot.fn="hdr", xlim = c(-2.5,2.5))
   dev.off()
}
