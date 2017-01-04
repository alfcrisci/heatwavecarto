
# install.packages(c("XLConnect","lubridate","leaflet","mapview","cartography","RColorBrewer","rworldmap","maptools"),dependencies=T) 

options(java.parameters = "-Xmx4g" )


library(XLConnect)
library(lubridate)
library(cartography)
library(RColorBrewer)
library(rworldmap)
library(maptools)



#setwd("C:/ALFIO/HEAT_WAVE/new")

setwd("/home/alf/Documenti/lav_morabito_maps")


#source("custom_layoutLayer.r")

capitals=readRDS("capitals.rds")

summer=loadWorkbook("DATI_HWHI_CHANGE.xlsx")
sheets=getSheets(summer)


data_summer_DATI_1980_1997=readWorksheet(summer, sheet = "DATI_1980-1997_NEW",startRow = 1,endRow = 29,startCol = 0)
data_summer_DATI_1998_2015=readWorksheet(summer, sheet = "DATI_1998-2015_NEW",startRow = 1,endRow = 29,startCol = 0)
  
#########################################################################################################################
res=list()

j=1

for ( i in data_summer_DATI_1980_1997$PAESE) {
      res[j]=which(capitals@data$CNTR_ID == i)
      j=j+1
} 

id_paese=unlist(res)
capitals_eu=capitals[id_paese,]
capitals_eu_nuts=spTransform(capitals_eu, CRS(proj4string(cartography::nuts0.spdf)))

capitals_DATI_1980_1997=capitals_eu_nuts
capitals_DATI_1998_2015=capitals_eu_nuts

#########################################################################################################################

data_summer_DATI_1980_1997$HWHI_C=cut(data_summer_DATI_1980_1997$HWHI, breaks=c(0.00,0.20,0.40,0.60,0.80,1),labels = 1:5)
data_summer_DATI_1998_2015$HWHI_C=cut(data_summer_DATI_1998_2015$HWHI, breaks=c(0.00,0.20,0.40,0.60,0.80,1),labels = 1:5)
data_summer_DATI_1980_1997$HWHI_N=as.numeric(cut(data_summer_DATI_1980_1997$HWHI, breaks=c(0.00,0.20,0.40,0.60,0.80,1),labels = 1:5))
data_summer_DATI_1998_2015$HWHI_N=as.numeric(cut(data_summer_DATI_1998_2015$HWHI, breaks=c(0.00,0.20,0.40,0.60,0.80,1),labels = 1:5))
data_summer_DATI_1980_1997$HWHI_S=1
data_summer_DATI_1998_2015$HWHI_S=1


capitals_DATI_1980_1997@data=cbind(capitals_DATI_1980_1997@data,data_summer_DATI_1980_1997)
capitals_DATI_1998_2015@data=cbind(capitals_DATI_1998_2015@data,data_summer_DATI_1980_1997)
capitals_DATI_1980_1997@data$HWHI_S=0.5
capitals_DATI_1998_2015@data$HWHI_S=0.5



######################################################################################################################################à
# Plot a layer with the extent of the EU28 countries with only a background color

newmap <- rworldmap::getMap(resolution = "low")  # different resolutions available "coarse","low","less islands","li","high".
orig.world.map_nuts=spTransform(newmap, CRS(proj4string(nuts0.spdf)))
carto.pal = colorRampPalette(c("green","yellow","orange","red","purple"))(5)


############################################################################################################################
# propSymbolsChoroLayer
# Plot a proportional symbols layer + choro

png(filename = "HWHI_1980_1997.png",width=800,height=800)

opar <- par(mar = c(0.1,0.1,1,0.1),cex.main=1)
plot(nuts0.spdf, border=NA, col  = "#E3DEBF",xlim=c(2666496, 6435690), ylim=c(1439611, 5009635),bg = "#A6CAE0")
layoutLayer(title="HWHI index 1980-1997",
            scale = NULL,col = NA, coltitle = "black",
            sources = "", author = "",
            frame = FALSE, bg = "#A6CAE0",
            south = TRUE)

plot(orig.world.map_nuts, border="grey10", col  = "#E3DEBF",  lwd=0.15, add=TRUE)

suppressWarnings(propSymbolsChoroLayer(spdf = capitals_DATI_1980_1997,
                      df = capitals_DATI_1980_1997@data, 
                      var2 = "HWHI_N", 
                      var = "HWHI_S",
                      add=T, 
                      inches = 0.05,
                      col = carto.pal, 
                      legend.var.pos = "n", 
                      legend.var2.pos = "n"))


legendTypo(pos = "topleft", title.txt = "Heat-Wave Hazard Index (1980-1997)",
           title.cex = 1, 
           cex = 1,
           values.cex = 1, 
           col = carto.pal, 
           categ = c("Very Low","Low","Medium","High","Very High"),
           nodata = FALSE, 
           frame = FALSE,
           symbol="box")


dev.off()




png(filename = "HWHI_1998_2015.png",width=800,height=800)

opar <- par(mar = c(0.1,0.1,1,0.1),cex.main=1)
plot(nuts0.spdf, border=NA, col  = "#E3DEBF",xlim=c(2666496, 6435690), ylim=c(1439611, 5009635),bg = "#A6CAE0")
layoutLayer(title="HWHI index 1980-1997",
            scale = NULL,col = NA, coltitle = "black",
            sources = "", author = "",
            frame = FALSE, bg = "#A6CAE0",
            south = TRUE)

plot(orig.world.map_nuts, border="grey10", col  = "#E3DEBF",  lwd=0.15, add=TRUE)

suppressWarnings(propSymbolsChoroLayer(spdf = capitals_DATI_1998_2015,
                                       df = capitals_DATI_1998_2015@data, 
                                       var2 = "HWHI_N", 
                                       var = "HWHI_S",
                                       add=T, 
                                       inches = 0.05,
                                       col = carto.pal, 
                                       legend.var.pos = "n", 
                                       legend.var2.pos = "n"))


legendTypo(pos = "topleft", title.txt = "Heat-Wave Hazard Index (1998-2015)",
           title.cex = 1, 
           cex = 1,
           values.cex = 1, 
           col = carto.pal, 
           categ = c("Very Low","Low","Medium","High","Very High"),
           nodata = FALSE, 
           frame = FALSE,
           symbol="box")


dev.off()


################################################################################################################
# Reference for R package cartography

# https://cran.r-project.org/web/packages/cartography/vignettes/cartography.html
# http://www.endmemo.com/program/R/pchsymbols.php
# https://elementr.hypotheses.org/284
# http://www.r-graph-gallery.com/184-cartography-library-demo/




