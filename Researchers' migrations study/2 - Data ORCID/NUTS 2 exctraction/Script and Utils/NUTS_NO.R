library(progress)
country <- "NO"
colnames(pc2020_NO_NUTS.2021_v2.0)<-c("NUTS3", "PCode")
pc2020_NO_NUTS.2021_v2.0 <- pc2020_NO_NUTS.2021_v2.0[-1,]

data_or <- data
data_pc <- geonames.postal.codes_filtered
data_nuts <- pc2020_NO_NUTS.2021_v2.0

#manual correction
data_pc <- data.frame(rbind(data_pc,c("NO",9170,"svalbard","","")))


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

regions <- levels(as.factor(data_pc$admin.name1[which(data_pc$country.code==country)]))

data_pc$admin.name1[which(data_pc$admin.name1=="oslocounty")]<-"oslo"
data_or$region[which(data_or$region=="oslocounty")]<-"oslo"
data_or$city[which(data_or$city=="oslocounty")]<-"oslo"
data_pc$admin.name1[which(data_pc$admin.name1==regions[3])]<-"moreogromsdal"
data_or$region[which(data_or$region==regions[3])]<-"moreogromsdal"
data_or$city[which(data_or$city==regions[3])]<-"moreogromsdal"
data_pc$admin.name1[which(data_pc$admin.name1==regions[8])]<-"trondelag"
data_or$region[which(data_or$region==regions[8])]<-"trondelag"
data_or$city[which(data_or$city==regions[8])]<-"trondelag"


cities1 <- levels(as.factor(data_pc$admin.name2[which(data_pc$country.code==country)]))
cities2 <- levels(as.factor(data_pc$admin.name3[which(data_pc$country.code==country)]))
regions <- levels(as.factor(data_pc$admin.name1[which(data_pc$country.code==country)]))

data_or$region[which(data_or$city=="oslo")]<-"oslo"
data_or$region[which(data_or$city=="molde")]<-"moreogromsdal"
data_or$region[which(data_or$city=="bodo")]<-"nordland"
data_or$region[which(data_or$city=="stavanger")]<-"rogaland"
data_or$region[which(data_or$city=="bergen")]<-"vestland"
data_or$region[which(data_or$city=="hamar")]<-"innlandet"


data_or$region[which(data_or$city=="tromso")]<-"nordland"
data_or$region[which(data_or$city=="tromsoe")]<-"nordland"
data_or$region[which(data_or$city=="moirana")]<-"nordland"
data_or$region[which(data_or$city=="bodoe")]<-"nordland"
data_or$region[which(data_or$city=="bodã¸,")]<-"nordland"
data_or$region[which(data_or$city=="bodã~")]<-"nordland"
data_or$region[which(data_or$city=="aas")]<-"viken"
data_or$region[which(data_or$city=="as")]<-"viken"
data_or$region[which(data_or$city=="kjeller")]<-"viken"
data_or$region[which(data_or$city=="lysaker")]<-"viken"
data_or$region[which(data_or$city=="fornebu")]<-"viken"
data_or$region[which(data_or$city=="nesoddtangen")]<-"viken"
data_or$region[which(data_or$city=="lillestrom")]<-"viken"
data_or$region[which(data_or$city=="lorenskog")]<-"viken"
data_or$region[which(data_or$city=="sandvika")]<-"viken"
data_or$region[which(data_or$city=="hã¸nefoss")]<-"viken"
data_or$region[which(data_or$city=="vikersund")]<-"viken"
data_or$region[which(data_or$city=="baerum")]<-"viken"
data_or$region[which(data_or$city=="honefoss")]<-"viken"
data_or$region[which(data_or$city=="sessvollmoen")]<-"viken"
data_or$region[which(data_or$city=="arnes")]<-"viken"
data_or$region[which(data_or$city=="ã.rnes")]<-"viken"
data_or$region[which(data_or$city=="hokksund")]<-"viken"
data_or$region[which(data_or$city=="drã¸bak")]<-"viken"
data_or$region[which(data_or$city=="longyearbyen")]<-"svalbard"
data_or$region[which(data_or$city=="tonsberg")]<-"vestfoldogtelemark"
data_or$region[which(data_or$city=="toensberg")]<-"vestfoldogtelemark"
data_or$region[which(data_or$city=="gjovik")]<-"innlandet"
data_or$region[which(data_or$city=="rena")]<-"innlandet"
data_or$region[which(data_or$city=="bodo")]<-"nordland"
data_or$region[which(data_or$city=="andenes")]<-"nordland"
data_or$region[which(data_or$city=="stokmarknes")]<-"nordland"
data_or$region[which(data_or$city=="aalesund")]<-"moreogromsdal"
data_or$region[which(data_or$city=="alesund")]<-"moreogromsdal"
data_or$region[which(data_or$city=="borre")]<-"vestfoldogtelemark"
data_or$region[which(data_or$city=="bã¸itelemark")]<-"vestfoldogtelemark"
data_or$region[which(data_or$city=="bã¸intelemark")]<-"vestfoldogtelemark"
data_or$region[which(data_or$city=="boitelemark")]<-"vestfoldogtelemark"
data_or$region[which(data_or$city=="vestfold")]<-"vestfoldogtelemark"
data_or$region[which(data_or$city=="stathelle")]<-"vestfoldogtelemark"
data_or$region[which(data_or$city=="telemark")]<-"vestfoldogtelemark"
data_or$region[which(data_or$city=="bã¸intelemark")]<-"vestfoldogtelemark"
data_or$region[which(data_or$city=="bakkenteigen")]<-"vestfoldogtelemark"
data_or$region[which(data_or$city=="evenstad")]<-"innlandet"
data_or$region[which(data_or$city=="brumunddal")]<-"innlandet"
data_or$region[which(data_or$city=="otta")]<-"innlandet"
data_or$region[which(data_or$city=="ottestad")]<-"innlandet"
data_or$region[which(data_or$city=="2312ottestad")]<-"innlandet"
data_or$region[which(data_or$city=="gjoevik")]<-"innlandet"
data_or$region[which(data_or$city=="raufoss")]<-"innlandet"
data_or$region[which(data_or$city=="koppang")]<-"innlandet"
data_or$region[which(data_or$city=="gjã~vik")]<-"innlandet"
data_or$region[which(data_or$city=="kristiansands")]<-"agder"
data_or$region[which(data_or$city=="kristansand")]<-"agder"
data_or$region[which(data_or$city=="adger")]<-"agder"
data_or$region[which(data_or$city=="mandal")]<-"agder"
data_or$region[which(data_or$city=="hã¸vik")]<-"vestland"
data_or$region[which(data_or$city=="hovik")]<-"vestland"
data_or$region[which(data_or$city=="fã¸rde")]<-"vestland"
data_or$region[which(data_or$city=="forde")]<-"vestland"
data_or$region[which(data_or$city=="bergen,")]<-"vestland"
data_or$region[which(data_or$city=="bergen:norway")]<-"vestland"
data_or$region[which(data_or$city=="bergenandglobal")]<-"vestland"
data_or$region[which(data_or$city=="fyllingsdalen")]<-"vestland"
data_or$region[which(data_or$city=="olso")]<-"oslo"
data_or$region[which(data_or$city=="kautokeino")]<-"tromsogfinnmark"
data_or$region[which(data_or$city=="honningsvã¥g")]<-"tromsogfinnmark"
data_or$region[which(data_or$city=="kirkenes")]<-"tromsogfinnmark"
data_or$region[which(data_or$city=="tromsã~")]<-"tromsogfinnmark"
data_or$region[which(data_or$city=="karasjok")]<-"tromsogfinnmark"
data_or$region[which(data_or$city=="bardufoss")]<-"tromsogfinnmark"
data_or$region[which(data_or$city=="ekne")]<-"trondelag"
data_or$region[which(data_or$city=="trondehim")]<-"trondelag"
data_or$region[which(data_or$city=="trondiem")]<-"trondelag"
data_or$region[which(data_or$city=="trondhem")]<-"trondelag"
data_or$region[which(data_or$city=="snillfjord")]<-"trondelag"
data_or$region[which(data_or$city=="sistranda")]<-"trondelag"


data_or$region[which(data_or$region=="troms")]<-"nordland"
data_or$region[which(data_or$region=="tromso")]<-"nordland"
data_or$region[which(data_or$region=="akershus")]<-"viken"
data_or$region[which(data_or$region=="arnes")]<-"viken"
data_or$region[which(data_or$region=="ã.rnes")]<-"viken"
data_or$region[which(data_or$region=="ã~stfold")]<-"viken"
data_or$region[which(data_or$region=="honefoss")]<-"viken"
data_or$region[which(data_or$region=="slemmestad")]<-"viken"
data_or$region[which(data_or$region=="ostfold")]<-"viken"
data_or$region[which(data_or$region=="vestfold")]<-"vestfoldogtelemark"
data_or$region[which(data_or$region=="telemark")]<-"vestfoldogtelemark"
data_or$region[which(data_or$region=="hordaland")]<-"vestland"

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

#correzioni
data_or$NUTS3[as.numeric(manuals$ID[which(manuals$Region=="ã~stfold")])] <- "NO082"
data_or$NUTS3[as.numeric(manuals$ID[which(manuals$City=="gralum")])] <- "NO082"

data$NUTS3[indices] <- data_or$NUTS3[indices]
save(data, file="C:/Users/user/Desktop/forORCID/ORCID_2021_activities_extract_uptoNO.RData")
