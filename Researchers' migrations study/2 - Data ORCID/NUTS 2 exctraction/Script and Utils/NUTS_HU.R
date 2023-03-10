library(progress)
country <- "HU"
colnames(pc2020_HU_NUTS.2021_v1.0)<-c("NUTS3", "PCode")
pc2020_HU_NUTS.2021_v1.0 <- pc2020_HU_NUTS.2021_v1.0[-1,]

data_or <- data
data_pc <- geonames.postal.codes_filtered
data_nuts <- pc2020_HU_NUTS.2021_v1.0

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

data_or$region[which(data_or$region=="b??k??s")] <- "bekes"
data_or$region[which(data_or$region=="j??sznagykunszolnok")]<-"jasznagykunszolnok"
data_or$region[which(data_or$region=="b??cskiskun")]<-"bacskiskun"
data_or$region[which(data_or$region=="csongr??d")]<-"csongrad"
data_or$region[which(data_or$region=="hajd??bihar")]<-"hajdubihar"
data_or$region[which(data_or$region=="gy?'rmosonsopron")]<-"gyarmosonsopron"
data_or$region[which(data_or$region=="borsodaba??jzempl??n")]<-"borsodabaajzemplan"
data_or$region[which(data_or$region=="csongr??dcsan??d")]<-"csongrad"
data_or$region[which(data_or$region=="csongr??dmegye")]<-"csongrad"


data_or$region[which(data_or$city=="veszpr??m")]<-"veszprem"
data_or$region[which(data_or$city=="szeged")]<-"csongrad"
data_or$region[which(data_or$city=="debrecen")]<-"hajdubihar"
data_or$region[which(data_or$city=="p??cs")]<-"baranya"
data_or$region[which(data_or$city=="pecs")]<-"baranya"
data_or$region[which(data_or$city=="miskolc")]<-"borsodabaajzemplan"
data_or$region[which(data_or$city=="piliscsaba")]<-"pest"
data_or$region[which(data_or$city=="godollo")]<-"pest"
data_or$region[which(data_or$city=="g??d??ll?'")]<-"pest"
data_or$region[which(data_or$city=="gy?'r")]<-"gyarmosonsopron"
data_or$region[which(data_or$city=="gy?'r")]<-"gyarmosonsopron"
data_or$region[which(data_or$city=="gyor")]<-"gyarmosonsopron"
data_or$region[which(data_or$city=="sopron")]<-"gyarmosonsopron"
data_or$region[which(data_or$city=="kaposv??r")]<-"somogy"
data_or$region[which(data_or$city=="kaposvar")]<-"somogy"
data_or$region[which(data_or$city=="szombathely")]<-"vas"
data_or$region[which(data_or$city=="kecskem??t")]<-"bacskiskun"
data_or$region[which(data_or$city=="keszthely")]<-"zala"
data_or$region[which(data_or$city=="eger")]<-"heves"
data_or$region[which(data_or$city=="szolnok")]<-"jasznagykunszolnok"
data_or$region[which(data_or$city=="sz??kesfeh??rv??r")]<-"fej??r"
data_or$region[which(data_or$city=="ny??regyh??za")]<-"szabolcsszatm??rbereg"
data_or$region[which(data_or$city=="tihany")]<-"veszprem"
data_or$region[which(data_or$city=="baja")]<-"bacskiskun"
data_or$region[which(data_or$city=="kecskemet")]<-"bacskiskun"
data_or$region[which(data_or$city=="szentendre")]<-"pest"
data_or$region[which(data_or$city=="budapeste")]<-"budapest"
data_or$region[which(data_or$city=="szarvas")]<-"bekes"
data_or$region[which(data_or$city=="zalaegerszeg")]<-"zala"


data_pc$admin.name1[which(data_pc$admin.name1=="b??k??s")] <- "bekes"
data_pc$admin.name1[which(data_pc$admin.name1=="csongr??d")] <- "csongrad"
data_pc$admin.name1[which(data_pc$admin.name1=="hajd??bihar")] <- "hajdubihar"
data_pc$admin.name1[which(data_pc$admin.name1=="gy?'rmosonsopron")]<-"gyarmosonsopron"
data_pc$admin.name1[which(data_pc$admin.name1=="borsodaba??jzempl??n")]<-"borsodabaajzemplan"
data_pc$admin.name1[which(data_pc$admin.name1=="veszpr??m")]<-"veszprem"
data_pc$admin.name1[which(data_pc$admin.name1=="b??cskiskun")]<-"bacskiskun"
data_pc$admin.name1[which(data_pc$admin.name1=="j??sznagykunszolnok")]<-"jasznagykunszolnok"

cities1 <- levels(as.factor(data_pc$admin.name2[which(data_pc$country.code==country)]))
cities2 <- levels(as.factor(data_pc$admin.name3[which(data_pc$country.code==country)]))
regions <- levels(as.factor(data_pc$admin.name1[which(data_pc$country.code==country)]))

manuals <- data.frame(cbind(Region = c("Region"), City = c("City"), ID = "ID"))

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

#corrections
data_or$NUTS3[as.numeric(manuals$ID[which(manuals$City=="g??d??ll?'")])] <- "HU120"

data$NUTS3[indices] <- data_or$NUTS3[indices]
save(data, file="C:/Users/user/Desktop/forORCID/ORCID_2021_activities_extract_uptoHU.RData")
