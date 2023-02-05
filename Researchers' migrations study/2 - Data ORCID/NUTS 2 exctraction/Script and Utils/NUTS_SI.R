#USARE GEONAMES SI

library(progress)
country <- "SI"
colnames(pc2020_SI_NUTS.2021_v1.0)<-c("NUTS3", "PCode")
pc2020_SI_NUTS.2021_v1.0 <- pc2020_SI_NUTS.2021_v1.0[-1,]

data_or <- data
data_pc <- geonames_SI
data_nuts <- pc2020_SI_NUTS.2021_v1.0

levels(as.factor(data_pc$admin.name1[which(data_pc$country.code == country)]))
levels(as.factor(data_pc$admin.name2[which(data_pc$country.code == country)]))
levels(as.factor(data_pc$admin.name3[which(data_pc$country.code == country)]))


indices <- which(data_or$country == country)

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

data_or$region[which(data_or$region=="ljubljana")]<-"zahodnaslovenija"
data_or$region[which(data_or$city=="ljubljana")]<-"zahodnaslovenija"
data_or$region[which(data_or$region=="1000ljubljana")]<-"zahodnaslovenija"
data_or$region[which(data_or$city=="1000ljubljana")]<-"zahodnaslovenija"
data_or$region[which(data_or$region=="ajdovscina")]<-"zahodnaslovenija"
data_or$region[which(data_or$city=="ajdovscina")]<-"zahodnaslovenija"
data_or$region[which(data_or$region=="ajdovå¡ä\u008dina")]<-"zahodnaslovenija"
data_or$region[which(data_or$city=="ajdovå¡ä\u008dina")]<-"zahodnaslovenija"
data_or$region[which(data_or$region=="kranj")]<-"zahodnaslovenija"
data_or$region[which(data_or$city=="kranj")]<-"zahodnaslovenija"
data_or$region[which(data_or$region=="kamnik")]<-"zahodnaslovenija"
data_or$region[which(data_or$city=="kamnik")]<-"zahodnaslovenija"
data_or$region[which(data_or$region=="bled")]<-"zahodnaslovenija"
data_or$region[which(data_or$city=="bled")]<-"zahodnaslovenija"
data_or$region[which(data_or$region=="ankaran")]<-"zahodnaslovenija"
data_or$region[which(data_or$city=="ankaran")]<-"zahodnaslovenija"
data_or$region[which(data_or$region=="portoroz")]<-"zahodnaslovenija"
data_or$region[which(data_or$city=="portoroz")]<-"zahodnaslovenija"
data_or$region[which(data_or$region=="portoroå¾")]<-"zahodnaslovenija"
data_or$region[which(data_or$city=="portoroå¾")]<-"zahodnaslovenija"
data_or$region[which(data_or$region=="novagorica")]<-"zahodnaslovenija"
data_or$region[which(data_or$city=="novagorica")]<-"zahodnaslovenija"
data_or$region[which(data_or$region=="izola")]<-"zahodnaslovenija"
data_or$region[which(data_or$city=="izola")]<-"zahodnaslovenija"
data_or$region[which(data_or$region=="golnik")]<-"zahodnaslovenija"
data_or$region[which(data_or$city=="golnik")]<-"zahodnaslovenija"
data_or$region[which(data_or$region=="koper")]<-"zahodnaslovenija"
data_or$region[which(data_or$city=="koper")]<-"zahodnaslovenija"
data_or$region[which(data_or$region=="maribor")]<-"vzhodnaslovenija"
data_or$region[which(data_or$city=="maribor")]<-"vzhodnaslovenija"
data_or$region[which(data_or$region=="celje")]<-"vzhodnaslovenija"
data_or$region[which(data_or$city=="celje")]<-"vzhodnaslovenija"
data_or$region[which(data_or$region=="novomesto")]<-"vzhodnaslovenija"
data_or$region[which(data_or$city=="novomesto")]<-"vzhodnaslovenija"
data_or$region[which(data_or$region=="slovenjgradec")]<-"vzhodnaslovenija"
data_or$region[which(data_or$city=="slovenjgradec")]<-"vzhodnaslovenija"
data_or$region[which(data_or$region=="velenje")]<-"vzhodnaslovenija"
data_or$region[which(data_or$city=="velenje")]<-"vzhodnaslovenija"
data_or$region[which(data_or$region=="murskasobota")]<-"vzhodnaslovenija"
data_or$region[which(data_or$city=="murskasobota")]<-"vzhodnaslovenija"
data_or$region[which(data_or$region=="ptuj")]<-"vzhodnaslovenija"
data_or$region[which(data_or$city=="ptuj")]<-"vzhodnaslovenija"


manuals <- data.frame(cbind(Region = c("Region"), City = c("City"), ID = c("ID")))

pb <- progress_bar$new(total = length(indices))
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

#levels(as.factor(data_or$NUTS3))
#manuals

data$NUTS3[indices] <- data_or$NUTS3[indices]
save(data, file="C:/Users/user/Desktop/forORCID/ORCID_2021_activities_extract_uptoSI.RData")
