#slovakia 
library(progress)
country <- "SK"

#pc<-read.table("D:/Marta/Politecnico/Tesi/NUTS adders/pc2020_SK_NUTS-2021_v2.0.csv", sep=";", quote="'", comment.char="")
pc <- pc2020_SK_NUTS.2021_v2.0
colnames(pc)<-c("NUTS3", "PCode")
pc <- pc[-1,]

#ORCID_2021_activities_extract_full <- read.csv("D:/Marta/Politecnico/Tesi/NUTS adders/ORCID_2021_activities_extract_full.csv")
#geonames.postal.codes <- read.csv("D:/Marta/Politecnico/Tesi/NUTS adders/geonames.postal.codes_filtered.csv")

data_or <- data
data_pc <- geonames.postal.codes_filtered
data_nuts <- pc

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


levels(as.factor(data_pc$admin.name1[which(data_pc$country.code == country)]))
levels(as.factor(data_pc$admin.name2[which(data_pc$country.code == country)]))
levels(as.factor(data_pc$admin.name3[which(data_pc$country.code == country)]))

### correzioni nomi pc

regions <- levels(as.factor(data_pc$admin.name1[which(data_pc$country.code==country)]))

data_pc$admin.name1[which(data_pc$admin.name1==regions[1])]<-"zilina" 
data_pc$admin.name1[which(data_pc$admin.name1==regions[2])]<-"banskabystrica" 
data_pc$admin.name1[which(data_pc$admin.name1==regions[3])]<-"bratislav" 
data_pc$admin.name1[which(data_pc$admin.name1==regions[4])]<-"kosice" 
data_pc$admin.name1[which(data_pc$admin.name1==regions[5])]<-"nitrian" 
data_pc$admin.name1[which(data_pc$admin.name1==regions[6])]<-"presov" 
data_pc$admin.name1[which(data_pc$admin.name1==regions[7])]<-"trencin" 
data_pc$admin.name1[which(data_pc$admin.name1==regions[8])]<-"trnava" 

cities1 <- levels(as.factor(data_pc$admin.name2[which(data_pc$country.code==country)]))
cities2 <- levels(as.factor(data_pc$admin.name3[which(data_pc$country.code==country)]))
regions <- levels(as.factor(data_pc$admin.name1[which(data_pc$country.code==country)]))

### da modificare Paaese dipendente 

data_or$city[which(data_or$city=="bratislava")]<-"bratislav"
data_or$city[which(data_or$city=="koå¡ice")]<-"kosice"
data_or$city[which(data_or$city=="komarno")]<-"nitrian"
data_or$city[which(data_or$city=="tatranskã¡javorina")]<-"presov"
data_or$city[which(data_or$city=="luå¾ianky")]<-"nitrian"
data_or$city[which(data_or$city=="bratislava4")]<-"bratislav"
data_or$city[which(data_or$city=="bratislava1")]<-"bratislav"
data_or$city[which(data_or$city=="slovenskalupca")]<-"banskabystrica"
data_or$city[which(data_or$city=="novemestonadvahom")]<-"trencin"
data_or$city[which(data_or$city=="tatranskalomnica")]<-"presov"


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
data_or$NUTS3[as.numeric(manuals$ID[which(manuals$Region=="zilina")])] <- "SK031"


data$NUTS3[indices] <- data_or$NUTS3[indices]
save(data, file="C:/Users/user/Desktop/forORCID/ORCID_2021_activities_extract_uptoSK.RData")
