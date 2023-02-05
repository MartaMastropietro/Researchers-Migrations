#USA GEONAMES PER IRLANDA

library(progress)
country <- "IE"
colnames(pc2020_IE_NUTS.2021_v1.0)<-c("NUTS3", "PCode")
pc2020_IE_NUTS.2021_v1.0 <- pc2020_IE_NUTS.2021_v1.0[-1,]

data_or <- data
data_pc <- geonames_IE
data_nuts <- pc2020_IE_NUTS.2021_v1.0

levels(as.factor(data_pc$admin.name1[which(data_pc$country.code == country)]))
levels(as.factor(data_pc$admin.name2[which(data_pc$country.code == country)]))
levels(as.factor(data_pc$admin.name3[which(data_pc$country.code == country)]))

indices <- which(data_or$country == country)

data_or$region <- tolower(data_or$region)
data_or$city <- tolower(data_or$city)
data_or$city <- gsub(" ", "", data_or$city)
data_or$city <- gsub("-", "", data_or$city)
data_or$region <- gsub(" ", "", data_or$region)
data_or$region <- gsub("-", "", data_or$region)
data_pc$admin.name1 <- gsub(" ", "", data_pc$admin.name1)
data_pc$postal.code <- gsub(" ", "", data_pc$postal.code)


data_or$region[which(data_or$city=="galway")]<-"northernandwestern"
data_or$region[which(data_or$city=="galwaycity")]<-"northernandwestern"
data_or$region[which(data_or$city=="athenry")]<-"northernandwestern"
data_or$region[which(data_or$city=="sligo")]<-"northernandwestern"
data_or$region[which(data_or$city=="derry")]<-"northernandwestern"
data_or$region[which(data_or$city=="cavan")]<-"northernandwestern"
data_or$region[which(data_or$city=="monaghan")]<-"northernandwestern"
data_or$region[which(data_or$city=="letterkenny")]<-"northernandwestern"
data_or$region[which(data_or$city=="mayo")]<-"northernandwestern"
data_or$region[which(data_or$city=="cork")]<-"southern"
data_or$region[which(data_or$city=="co.cork")]<-"southern"
data_or$region[which(data_or$city=="corkcity")]<-"southern"
data_or$region[which(data_or$city=="fermoy")]<-"southern"
data_or$region[which(data_or$city=="limerick")]<-"southern"
data_or$region[which(data_or$city=="tipperary")]<-"southern"
data_or$region[which(data_or$city=="wexford")]<-"southern"
data_or$region[which(data_or$city=="tralee")]<-"southern"
data_or$region[which(data_or$city=="thurles")]<-"southern"
data_or$region[which(data_or$city=="ballydine")]<-"southern"
data_or$region[which(data_or$city=="shannon")]<-"southern"
data_or$region[which(data_or$city=="waterford")]<-"southern"
data_or$region[which(data_or$city=="clonmel")]<-"southern"
data_or$region[which(data_or$city=="kilkenny")]<-"southern"
data_or$region[which(data_or$city=="clare")]<-"southern"
data_or$region[which(data_or$city=="dublin")]<-"easternandmidland"
data_or$region[which(data_or$city=="dubli")]<-"easternandmidland"
data_or$region[which(data_or$city=="dubin")]<-"easternandmidland"
data_or$region[which(data_or$city=="75merrionsquaredublin")]<-"easternandmidland"
data_or$region[which(data_or$city=="75merrionsquaredublinireland")]<-"easternandmidland"
data_or$region[which(data_or$city=="dublincity")]<-"easternandmidland"
data_or$region[which(data_or$city=="dublic")]<-"easternandmidland"
data_or$region[which(data_or$city=="dublin(andvicinity)")]<-"easternandmidland"
data_or$region[which(data_or$city=="dublin,ireland")]<-"easternandmidland"
data_or$region[which(data_or$city=="tullamore")]<-"easternandmidland"
data_or$region[which(data_or$city=="tallaght")]<-"easternandmidland"
data_or$region[which(data_or$city=="blackrock")]<-"easternandmidland"
data_or$region[which(data_or$city=="portlaoise")]<-"easternandmidland"
data_or$region[which(data_or$city=="longford")]<-"easternandmidland"
data_or$region[which(data_or$city=="coleraine")]<-"easternandmidland"
data_or$region[which(data_or$city=="navan")]<-"easternandmidland"
data_or$region[which(data_or$city=="dunlaoghaire")]<-"easternandmidland"
data_or$region[which(data_or$city=="wicklow")]<-"easternandmidland"
data_or$region[which(data_or$city=="leixlip")]<-"easternandmidland"
data_or$region[which(data_or$city=="kildare")]<-"easternandmidland"
data_or$region[which(data_or$city=="naas")]<-"easternandmidland"
data_or$region[which(data_or$city=="athlone")]<-"easternandmidland"
data_or$region[which(data_or$city=="drogheda")]<-"easternandmidland"
data_or$region[which(data_or$city=="dundalk")]<-"easternandmidland"
data_or$region[which(data_or$city=="carlow")]<-"easternandmidland"
data_or$region[which(data_or$city=="maynooth")]<-"easternandmidland"


data_or$region[which(data_or$region=="dublin")]<-"easternandmidland"
data_or$region[which(data_or$region=="co.dublin")]<-"easternandmidland"
data_or$region[which(data_or$region=="countydublin")]<-"easternandmidland"
data_or$region[which(data_or$region=="co.kildare")]<-"easternandmidland"
data_or$region[which(data_or$region=="wicklow")]<-"easternandmidland"
data_or$region[which(data_or$region=="kildare")]<-"easternandmidland"
data_or$region[which(data_or$region=="coleraine")]<-"easternandmidland"
data_or$region[which(data_or$region=="cork")]<-"southern"
data_or$region[which(data_or$region=="co.cork")]<-"southern"
data_or$region[which(data_or$region=="countycork")]<-"southern"
data_or$region[which(data_or$region=="limerick")]<-"southern"
data_or$region[which(data_or$region=="wexford")]<-"southern"
data_or$region[which(data_or$region=="clare")]<-"southern"
data_or$region[which(data_or$region=="co.clare")]<-"southern"
data_or$region[which(data_or$region=="galway")]<-"northernandwestern"
data_or$region[which(data_or$region=="co.galway")]<-"northernandwestern"
data_or$region[which(data_or$region=="galwayco.")]<-"northernandwestern"
data_or$region[which(data_or$region=="mayo")]<-"northernandwestern"
data_or$region[which(data_or$region=="co.mayo")]<-"northernandwestern"



regions <- levels(as.factor(data_pc$admin.name1))

manuals <- data.frame(cbind(Region = c("Region"), City = c("City"), ID = c("ID")))

pb <- progress_bar$new(total=length(indices))
pb$tick(0)

for(i in indices){
  ind2 <- intersect(data_or[i,5], regions)
  if(length(ind2) != 0 && ind2 != ""){
    code <- data_pc$postal.code[which(data_pc$admin.name1==data_or[i,5])]
    data_or$NUTS3[i] <- data_nuts$NUTS3[which(data_nuts$PCode==code)]
  }
  else{
    ind3 <- intersect(data_or[i,6], regions)
    if(length(ind3) != 0 && ind3 != ""){
      code <- data_pc$postal.code[which(data_pc$admin.name1==data_or[i,6])]
      data_or$NUTS3[i] <- data_nuts$NUTS3[which(data_nuts$PCode==code)]
    }
  }
  
  if(data_or$NUTS3[i]=="None"){
    manuals <- rbind(manuals, c(data_or$region[i], data_or$city[i], i))
  }
  pb$tick()
}

#levels(as.factor(data$NUTS3))
#manuals

#corrections
data_or$NUTS3[as.numeric(manuals$ID[which(manuals$City=="belfast")])] <- "UKN06"
data_or$NUTS3[as.numeric(manuals$ID[which(manuals$City=="dublin4")])] <- "IE061"
data_or$NUTS3[as.numeric(manuals$ID[which(manuals$City=="dublin2")])] <- "IE061"


data$NUTS3[indices] <- data_or$NUTS3[indices]
save(data, file="C:/Users/user/Desktop/forORCID/ORCID_2021_activities_extract_uptoIE.RData")
