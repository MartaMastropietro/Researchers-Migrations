#romania 
library(progress)
country <- "RO"

#pc<-read.table("D:/Marta/Politecnico/Tesi/NUTS adders/pc2020_RO_NUTS-2021_v2.0.csv", sep=";", quote="'", comment.char="")
pc <- pc2020_RO_NUTS.2021_v2.0
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

### correzione nomi pc

regions <- levels(as.factor(data_pc$admin.name1[which(data_pc$country.code==country)]))

data_pc$admin.name2[which(data_pc$admin.name1==regions[10])]<-"bucuresti"
data_pc$admin.name1[which(data_pc$admin.name1==regions[9])]<-"braila"
data_pc$admin.name1[which(data_pc$admin.name1==regions[3])]<-"arges"
data_pc$admin.name1[which(data_pc$admin.name1==regions[4])]<-"bacau"
data_pc$admin.name1[which(data_pc$admin.name1==regions[10])]<-"bucuresti"
data_pc$admin.name1[which(data_pc$admin.name1==regions[6])]<-"bistritanasaud"
data_pc$admin.name1[which(data_pc$admin.name1==regions[7])]<-"botosani"
data_pc$admin.name1[which(data_pc$admin.name1==regions[8])]<-"brasov"
data_pc$admin.name1[which(data_pc$admin.name1==regions[11])]<-"buzau"
data_pc$admin.name1[which(data_pc$admin.name1==regions[12])]<-"calarasi"
data_pc$admin.name1[which(data_pc$admin.name1==regions[13])]<-"carasseverin"
data_pc$admin.name1[which(data_pc$admin.name1==regions[15])]<-"costanta"
data_pc$admin.name1[which(data_pc$admin.name1==regions[17])]<-"dambovita"
data_pc$admin.name1[which(data_pc$admin.name1==regions[19])]<-"galati"
data_pc$admin.name1[which(data_pc$admin.name1==regions[24])]<-"iasi"
data_pc$admin.name1[which(data_pc$admin.name1==regions[25])]<-"ialomita"
data_pc$admin.name1[which(data_pc$admin.name1==regions[27])]<-"maramures"
data_pc$admin.name1[which(data_pc$admin.name1==regions[28])]<-"mehedinti"
data_pc$admin.name1[which(data_pc$admin.name1==regions[29])]<-"mures"
data_pc$admin.name1[which(data_pc$admin.name1==regions[30])]<-"neamt"
data_pc$admin.name1[which(data_pc$admin.name1==regions[33])]<-"salaj"
data_pc$admin.name1[which(data_pc$admin.name1==regions[38])]<-"timis"
data_pc$admin.name1[which(data_pc$admin.name1==regions[40])]<-"valcea"

###


cities1 <- levels(as.factor(data_pc$admin.name2[which(data_pc$country.code==country)]))
cities2 <- levels(as.factor(data_pc$admin.name3[which(data_pc$country.code==country)]))
regions <- levels(as.factor(data_pc$admin.name1[which(data_pc$country.code==country)]))

### da modificare Paaese dipendente 

data_or$city[which(data_or$city=="albaiulia")]<-"alba"
data_or$city[which(data_or$city=="bucahrest")]<-"bucuresti"
data_or$city[which(data_or$city=="bucarest")]<-"bucuresti"
data_or$city[which(data_or$city=="bucharest")]<-"bucuresti"
data_or$city[which(data_or$city=="bucharest,romania")]<-"bucuresti"
data_or$city[which(data_or$city=="bucharesti")]<-"bucuresti"
data_or$city[which(data_or$city=="bucure?Tti")]<-"bucuresti"
data_or$city[which(data_or$city=="bucahrest")]<-"bucuresti"
data_or$city[which(data_or$city=="bucarestmagurele")]<-"bucuresti"
data_or$city[which(data_or$city=="bucahrest")]<-"bucuresti"
data_or$city[which(data_or$city=="bucharest,")]<-"bucuresti"
data_or$city[which(data_or$city=="bucharest/magurele")]<-"bucuresti"
data_or$city[which(data_or$city=="bucharestmagurele")]<-"bucuresti"
data_or$city[which(data_or$city=="buchuarest")]<-"bucuresti"
data_or$city[which(data_or$city=="bucure?Tti/bucharest")]<-"bucuresti"
data_or$city[which(data_or$city=="bucuresti,galati")]<-"bucuresti"
data_or$city[which(data_or$city=="bucuresti/bucharest")]<-"bucuresti"
data_or$city[which(data_or$city=="blaj,albacounty")]<-"alba"
data_or$city[which(data_or$city=="bra?Tov")]<-"brasov"
data_or$city[which(data_or$city=="bucuresti,sector1")]<-"bucuresti"
data_or$city[which(data_or$city=="bucurestisector5")]<-"bucuresti"
data_or$city[which(data_or$city=="buharest")]<-"bucuresti"

data_or$city[which(data_or$city=="clujnapoca")]<-"cluj"
data_or$city[which(data_or$city=="craiova")]<-"dolj"
data_or$city[which(data_or$city=="gala?>i")]<-"galati"
data_or$city[which(data_or$city=="ia?Ti")]<-"iasi"
data_or$city[which(data_or$city=="m?fgurele")]<-"ilfov"
data_or$city[which(data_or$city=="magurele")]<-"ilfov"
data_or$city[which(data_or$city=="magurelebucharest")]<-"ilfov"
data_or$city[which(data_or$city=="mioveni")]<-"arges"
data_or$city[which(data_or$city=="oradea")]<-"bihor"
data_or$city[which(data_or$city=="petrosani")]<-"hunedoara"
data_or$city[which(data_or$city=="pite??ti")]<-"arges"
data_or$city[which(data_or$city=="pitesti")]<-"arges"
data_or$city[which(data_or$city=="ploie??ti")]<-"prahova"
data_or$city[which(data_or$city=="ploiesti")]<-"prahova"
data_or$city[which(data_or$city=="t??rgumure??")]<-"mures"
data_or$city[which(data_or$city=="t??rgumure?T")]<-"mures"
data_or$city[which(data_or$city=="targoviste")]<-"dambovita"
data_or$city[which(data_or$city=="targumures")]<-"mures"
data_or$city[which(data_or$city=="timi??oara")]<-"timis"
data_or$city[which(data_or$city=="timi?Toara")]<-"timis"
data_or$city[which(data_or$city=="timisoara")]<-"timis"
data_or$city[which(data_or$city=="tirgumures")]<-"mures"

data_or$city[which(data_or$city=="baiamare")]<-"mures"
data_or$city[which(data_or$city=="bucure??ti")]<-"bucuresti"
data_or$city[which(data_or$city=="constan?>a")]<-"costanta"
data_or$city[which(data_or$city=="constanta")]<-"costanta"
data_or$city[which(data_or$city=="t??rgovi??te")]<-"dambovita"
data_or$city[which(data_or$city=="t??rgumure??")]<-"mures"

data_or$city[which(data_or$city=="resita")]<-"carasseverin"
data_or$city[which(data_or$city=="ramnicuvalcea")]<-"valcea"
data_or$city[which(data_or$city=="miercureaciuc")]<-"harghita"
data_or$city[which(data_or$city=="ia??i")]<-"iasi"


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
#levels(as.factor(manuals$City))

#correzioni
data_or$NUTS3[as.numeric(manuals$ID[which(manuals$Region=="timi?T")])] <- "RO424"
data_or$NUTS3[as.numeric(manuals$ID[which(manuals$City=="timi?Toara")])] <- "RO424"
data_or$NUTS3[as.numeric(manuals$ID[which(manuals$Region=="constanta")])] <- "RO223"
data_or$NUTS3[as.numeric(manuals$ID[which(manuals$Region=="constan?>a")])] <- "RO223"
data_or$NUTS3[as.numeric(manuals$ID[which(manuals$City=="constan?>a")])] <- "RO223"
data_or$NUTS3[as.numeric(manuals$ID[which(manuals$City=="ia?Ti")])] <- "RO213"
data_or$NUTS3[as.numeric(manuals$ID[which(manuals$City=="m?fgurele")])] <- "RO322"
data_or$NUTS3[as.numeric(manuals$ID[which(manuals$City=="bucure?Tti")])] <- "RO321"
data_or$NUTS3[as.numeric(manuals$ID[which(manuals$City=="gala?>i")])] <- "RO224"


data$NUTS3[indices] <- data_or$NUTS3[indices]
save(data, file="C:/Users/user/Desktop/forORCID/ORCID_2021_activities_extract_uptoRO.RData")
