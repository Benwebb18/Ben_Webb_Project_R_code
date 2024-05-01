#****************************************************
#*****LONGHURST PROVINCE MAP FOR ARCTIC**************
#****************************************************


library(RColorBrewer)
library(classInt)
library(gpclib)

PE<-c(Combo_data$LAT, Combo_data$LONG)
Longhurst_1x1<-read.table(file='C:\\Users\\Lenovo\\OneDrive\\Documents\\Uni work\\4th year project\\R stuff\\regions_alan.dat',sep='',header=F)

# Coastal Color in Hex = #99d8c9  #31a354
# Polar Color in Hex = #9ecae1 OR #deebf7
# Trades Color in Hex = #fdbb84 OR #fee8c8
# Westerlies in Hex = #ffeda0 OR #fff7bc

subregion=subset(Longhurst_1x1,V2 >= -90 & V2 <= 90 & V3 >= -180 & V3 <=180)

provcols=c("#000000","#9ecae1","#9ecae1","#9ecae1","#fff7bc","#fff7bc","#fff7bc","#fdbb84","#fdbb84","#fdbb84","#fdbb84","#31a354","#31a354","#31a354","#31a354","#31a354","#fff7bc","#fdbb84",
           "#fff7bc","#31a354","#31a354","#31a354","#31a354","#31a354","#31a354","#31a354","#31a354","#31a354","#31a354","#31a354","#fdbb84","#fdbb84","#31a354","#31a354","#31a354","#31a354",
           "#31a354","#31a354","#00FF88FF","#00FF99FF","#00FFAAFF","#00FFBBFF","#00FFCCFF","#00FFDDFF","#00FFEEFF","#00FFFFFF","#00EEFFFF","#00DDFFFF","#00CCFFFF","#00CCFFFF","#9ecae1","#fff7bc","#fff7bc","#fff7bc",
           "#fff7bc","#fff7bc","#fff7bc","#fff7bc","#fff7bc","#fdbb84","#fdbb84","#fdbb84","#fdbb84","#fdbb84","#fdbb84","#31a354","#31a354","#31a354","#31a354","#31a354","#31a354","#31a354",
           "#31a354","#31a354","#31a354","#31a354","#31a354","#31a354","#77FF00FF","#0011FFFF","#fff7bc","#fff7bc","#9ecae1","#9ecae1","#9ecae1","#9ecae1","#9ecae1","#9ecae1","#9ecae1","#9ecae1","#9ecae1")

for(l in (1:nrow(subregion))){
  y1=subregion[l,2]+0.5
  x1=subregion[l,3]-0.5
  prov=subregion[l,4]
  polygon(x=c(x1,x1,x1+1,x1+1),y=c(y1,y1-1,y1-1,y1),col=provcols[prov+1],border=provcols[prov+1])
}

points(Combo_data$LONG, Combo_data$LAT, cex=0.2, col="#dd1c77", pch=19)
