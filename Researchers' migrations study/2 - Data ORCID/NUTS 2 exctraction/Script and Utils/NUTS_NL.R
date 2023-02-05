# netherlands 

library(progress)
country <- "NL"

#pc<-read.table("D:/Marta/Politecnico/Tesi/NUTS adders/pc2020_NL_NUTS-2021_v2.0.csv", sep=";", quote="'", comment.char="")
pc <- pc2020_NL_NUTS.2021_v2.0
colnames(pc)<-c("NUTS3", "PCode")
pc <- pc[-1,]

#ORCID_2021_activities_extract_full <- read.csv("D:/Marta/Politecnico/Tesi/NUTS adders/ORCID_2021_activities_extract_full.csv")
#geonames.postal.codes <- read.csv("D:/Marta/Politecnico/Tesi/NUTS adders/geonames.postal.codes_filtered.csv")

data_or <- data
data_pc <- geonames.postal.codes_filtered
data_nuts <- pc

data_nuts$PCode<-gsub(" .*","",data_nuts$PCode)
indices <- which(data_or$country == country)

levels(as.factor(data_pc$admin.name1[which(data_pc$country.code == country)]))
levels(as.factor(data_pc$admin.name2[which(data_pc$country.code == country)]))
levels(as.factor(data_pc$admin.name3[which(data_pc$country.code == country)]))

##################################################

data_or$region <- tolower(data_or$region)
data_or$city <- tolower(data_or$city)
data_pc$admin.name1 <- tolower(data_pc$admin.name1)
data_pc$admin.name2 <- tolower(data_pc$admin.name2)
data_pc$admin.name3 <- tolower(data_pc$admin.name3)
data_or$city <- gsub(" ", "", data_or$city)
data_or$city <- gsub("-", "", data_or$city)
data_or$region <- gsub(" ", "", data_or$region)
data_or$region <- gsub("-", "", data_or$region)
data_pc$admin.name1 <- gsub(" ", "", data_pc$admin.name1)
data_pc$admin.name1 <- gsub("-", "", data_pc$admin.name1)
data_pc$admin.name2 <- gsub(" ", "", data_pc$admin.name2)
data_pc$admin.name2 <- gsub("-", "", data_pc$admin.name2)
data_pc$admin.name3 <- gsub(" ", "", data_pc$admin.name3)
data_pc$admin.name3 <- gsub("-", "", data_pc$admin.name3)

cities1 <- levels(as.factor(data_pc$admin.name2[which(data_pc$country.code==country)]))
cities2 <- levels(as.factor(data_pc$admin.name3[which(data_pc$country.code==country)]))
regions <- levels(as.factor(data_pc$admin.name1[which(data_pc$country.code==country)]))

### da modificare Paese dipendente 

data_or$city[which(data_or$city=="bilthoven")]<-"utrecht"
data_or$city[which(data_or$city=="breukelen")]<-"utrecht"
data_or$city[which(data_or$city=="denbosch")]<-"noordbrabant"
data_or$city[which(data_or$city=="denburg")]<-"noordholland"
data_or$city[which(data_or$city=="denhaag")]<-"zuidholland"
data_or$city[which(data_or$city=="dwingeloo")]<-"drenthe"
data_or$city[which(data_or$city=="elsevier")]<-"noordholland"
data_or$city[which(data_or$city=="geleen")]<-"limburg"
data_or$city[which(data_or$city=="hague")]<-"zuidholland"
data_or$city[which(data_or$city=="hoofddorp")]<-"noordholland"
data_or$city[which(data_or$city=="ijmuiden")]<-"noordholland"
data_or$city[which(data_or$city=="petten")]<-"noordholland"
data_or$city[which(data_or$city=="soesterberg")]<-"utrecht"
data_or$city[which(data_or$city=="thehague")]<-"zuidholland"
data_or$city[which(data_or$city=="twente")]<-"overijssel"
data_or$city[which(data_or$city=="yerseke")]<-"zuidholland"
data_or$city[which(data_or$city=="zaandam")]<-"noordholland"
data_or$city[which(data_or$city=="")]<-""
data_or$city[which(data_or$city=="")]<-""

###

manuals <- data.frame(cbind(Region = c("Region"), City = c("City"), ID = c("ID")))

pb <- progress_bar$new(total=length(indices))
pb$tick(0)

for(i in indices){
  ind1 <- intersect(data_or[i,5], regions)
  if(length(ind1) != 0 && ind1 != ""){
    poss_code <- data_pc$postal.code[which(data_pc$admin.name1==data_or[i,5])]
    found <- FALSE
    for(c in poss_code){
      if(length(which(data_nuts$PCode==c))!=0){
        code <- c
        found <- TRUE
        break
      }
    }
    if(found){
      data_or$NUTS3[i] <- data_nuts$NUTS3[which(data_nuts$PCode==code)]
    }
  }
  else{
    ind2 <- intersect(data_or[i,6], cities1)
    if(length(ind2) != 0 && ind2 != ""){
      poss_code <- data_pc$postal.code[which(data_pc$admin.name2==data_or[i,6])]
      found <- FALSE
      for(c in poss_code){
        if(length(which(data_nuts$PCode==c))!=0){
          code <- c
          found <- TRUE
          break
        }
      }
      if(found){
        data_or$NUTS3[i] <- data_nuts$NUTS3[which(data_nuts$PCode==code)]
      }
    }
    else{
      ind3 <- intersect(data_or[i,6], cities2)
      if(length(ind3) != 0 && ind3 != ""){
        poss_code <- data_pc$postal.code[which(data_pc$admin.name3==data_or[i,6])]
        found <- FALSE
        for(c in poss_code){
          if(length(which(data_nuts$PCode==c))!=0){
            code <- c
            found <- TRUE
            break
          }
        }
        if(found){
          data_or$NUTS3[i] <- data_nuts$NUTS3[which(data_nuts$PCode==code)]
        }
      }
      else{
        ind4 <- intersect(data_or[i,5], cities1)
        if(length(ind4) != 0 && ind4 != ""){
          poss_code <- data_pc$postal.code[which(data_pc$admin.name2==data_or[i,5])]
          found <- FALSE
          for(c in poss_code){
            if(length(which(data_nuts$PCode==c))!=0){
              code <- c
              found <- TRUE
              break
            }
          }
          if(found){
            data_or$NUTS3[i] <- data_nuts$NUTS3[which(data_nuts$PCode==code)]
          }
        }
        else{
          ind5 <- intersect(data_or[i,5], cities2)
          if(length(ind5) != 0 && ind5 != ""){
            poss_code <- data_pc$postal.code[which(data_pc$admin.name3==data_or[i,5])]
            found <- FALSE
            for(c in poss_code){
              if(length(which(data_nuts$PCode==c))!=0){
                code <- c
                found <- TRUE
                break
              }
            }
            if(found){
              data_or$NUTS3[i] <- data_nuts$NUTS3[which(data_nuts$PCode==code)]
            }
          }
          else{
            ind6 <- intersect(data_or[i,6], regions)
            if(length(ind6) != 0 && ind6 != ""){
              poss_code <- data_pc$postal.code[which(data_pc$admin.name1==data_or[i,6])]
              found <- FALSE
              for(c in poss_code){
                if(length(which(data_nuts$PCode==c))!=0){
                  code <- c
                  found <- TRUE
                  break
                }
              }
              if(found){
                data_or$NUTS3[i] <- data_nuts$NUTS3[which(data_nuts$PCode==code)]
              }
            }
          }
        }
        
      }
    }
  }
  if(data_or$NUTS3[i]=="None"){
    manuals <- rbind(manuals, c(data_or$region[i], data_or$city[i], i))
  }
  pb$tick()
}


#levels(as.factor(data_or$NUTS3[which(data_or$country==country)]))
#manuals


#correzioni
data_or$NUTS3[as.numeric(manuals$ID[which(manuals$City=="leidschendam")])] <- "NL333"
data_or$NUTS3[as.numeric(manuals$ID[which(manuals$City=="voorburg")])] <- "NL333"
data_or$NUTS3[as.numeric(manuals$ID[which(manuals$City=="heeze")])] <- "NL411"
data_or$NUTS3[as.numeric(manuals$ID[which(manuals$City=="haren")])] <- "NL111"
data_or$NUTS3[as.numeric(manuals$ID[which(manuals$City=="sittard")])] <- "NL421"
data_or$NUTS3[as.numeric(manuals$ID[which(manuals$City=="naarden")])] <- "NL321"
data_or$NUTS3[as.numeric(manuals$ID[which(manuals$City=="bussum")])] <- "NL321"


data$NUTS3[indices] <- data_or$NUTS3[indices]
save(data, file="C:/Users/user/Desktop/forORCID/ORCID_2021_activities_extract_uptoNL.RData")
