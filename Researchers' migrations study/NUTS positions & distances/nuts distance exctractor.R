# https://github.com/SoftwareImpacts/SIMPAC-2022-61/blob/master/R/eudistance.R

library("eurostat")
library("SimDesign")
library("sf")

level=2
nuts <- SimDesign::quiet(eurostat::get_eurostat_geospatial(nuts_level = 2,year="2021"))
nuts_centroid = SimDesign::quiet(sf::st_centroid(nuts$geometry,of_largest_polygon = TRUE))
nuts_distance <- as.data.frame(sf::st_distance(nuts_centroid, nuts_centroid))
output <- data.frame(nuts$id,nuts$NUTS_NAME,nuts$LEVL_CODE,nuts$CNTR_CODE,nuts$geometry,nuts_centroid)
output <- cbind(output,nuts_distance)
colnames(output)  <- c(paste("NUTS_",level,"_code",sep=""),paste("NUTS_",level,"_name",sep=""),"Level_code","Country_code","Geometry","Centroid",as.character(nuts$id))

map_centroids =TRUE
if (!is.null("map_centroids") & map_centroids == TRUE){
  
  pkgs <- c("rworldmap")
  if(length(pkgs <- setdiff(pkgs, rownames(installed.packages())))) install.packages(pkgs)
  
  library("rworldmap")
  
  coord <- as.data.frame(sf::st_coordinates(output$Centroid))
  newmap <- rworldmap::getMap(resolution = "low")
  
  rworldmap::mapCountryData(newmap,colourPalette = c("white","white"),mapRegion="Europe",borderCol = "grey",oceanCol = "lightblue",mapTitle = paste("Centroids of NUTS-",level," regions (n = ",nrow(output),")",sep=""),addLegend=FALSE)
  points(coord$X,coord$Y,cex=1,col="black",bg="red",pch=21)
  
}

data<-output[,-c(2,3,4,5,6)]

nuts<-unique(nuts_years$nuts2)
idx<-setdiff(data$NUTS_2_code,nuts)
setdiff(nuts,data$NUTS_2_code)

#keep only london centre 
data$NUTS_2_code[which(data$NUTS_2_code=="UKI3")]<-"UKI0"
colnames(data)[which(colnames(data)=="UKI3")]<-"UKI0"

data<-data[-which(data$NUTS_2_code %in% idx),]
idx2<-setdiff(colnames(data),nuts)
idx2<-idx2[-1]

data<-data[,-which(colnames(data) %in% idx2)]

#srotolare
n<-dim(data)[1]
sender<-rep(data$NUTS_2_code,n)
data_col<-data.frame(sender)
data_col$receiver<-rep("NONE",length(data_col$sender))

for (i in 1:n){
  data_col$receiver[(1+n*(i-1)):(n+n*(i-1))]<-colnames(data)[i+1]
}

data_col$dist<-rep(-1,length(data_col$senders))
for (i in 1:n){
  data_col$dist[(1+n*(i-1)):(n+n*(i-1))]<-data[,i+1]
}
safers<-data_col
data_col<-data_col[-which(data_col$senders==data_col$receiver),]
dim(safers)[1]-dim(data_col)[1]

write.csv(data_col,"nuts2_distances.csv",row.names = F)
