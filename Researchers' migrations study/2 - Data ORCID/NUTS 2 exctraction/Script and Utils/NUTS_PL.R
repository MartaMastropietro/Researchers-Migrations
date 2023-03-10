library(progress)
country <- "PL"
colnames(pc2020_PL_NUTS.2021_v1.0)<-c("NUTS3", "PCode")
pc2020_PL_NUTS.2021_v1.0 <- pc2020_PL_NUTS.2021_v1.0[-1,]

data_or <- data
data_pc <- geonames.postal.codes_filtered
data_nuts <- pc2020_PL_NUTS.2021_v1.0

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

data_or$region[which(data_or$region=="?swi?Ttokrzyskie")]<-"swietokrzyskie"
data_pc$admin.name1[which(data_pc$admin.name1=="?swi?Ttokrzyskie")]<-"swietokrzyskie"
data_or$region[which(data_or$region=="?\u0081??d??voivodeship")]<-"lodz"
data_pc$admin.name1[which(data_pc$admin.name1=="?\u0081??d??voivodeship")]<-"lodz"



data_or$region[which(data_or$city=="krakow")]<-"lesserpoland"
data_or$region[which(data_or$city=="cracow")]<-"lesserpoland"
data_or$region[which(data_or$city=="cracov")]<-"lesserpoland"
data_or$region[which(data_or$city=="tarnow")]<-"lesserpoland"
data_or$region[which(data_or$city=="wroclaw")]<-"lowersilesia"
data_or$region[which(data_or$city=="poznan")]<-"greaterpoland"
data_or$region[which(data_or$city=="kalisz")]<-"greaterpoland"
data_or$region[which(data_or$city=="gdansk")]<-"pomerania"
data_or$region[which(data_or$city=="slupsk")]<-"pomerania"
data_or$region[which(data_or$city=="bialystok")]<-"podlasie"
data_or$region[which(data_or$city=="rzeszow")]<-"subcarpathia"
data_or$region[which(data_or$city=="torun")]<-"kujawskopomorskie"
data_or$region[which(data_or$city=="czestochowa")]<-"silesia"
data_or$region[which(data_or$city=="bielskobiala")]<-"silesia"
data_or$region[which(data_or$city=="dabrowagornicza")]<-"silesia"
data_or$region[which(data_or$city=="zielonagora")]<-"lubusz"
data_or$region[which(data_or$city=="pulawy")]<-"lublin"

data_or$city[which(data_or$city=="zielonka")]<-"warszawa"
data_or$city[which(data_or$city=="otwock")]<-"warszawa"
data_or$city[which(data_or$city=="raszyn")]<-"warszawa"
data_or$city[which(data_or$city=="warsawa")]<-"warszawa"
data_or$city[which(data_or$city=="warszawa/warsaw")]<-"warszawa"
data_or$city[which(data_or$city=="warszawa(warsaw)")]<-"warszawa"
data_or$city[which(data_or$city=="plock")]<-"radom"
data_or$city[which(data_or$city=="pultusk")]<-"radom"



data_or$region[which(data_or$region=="ma?,opolska")]<-"lesserpoland"
data_or$region[which(data_or$region=="ma?,opolskie")]<-"lesserpoland"
data_or$region[which(data_or$region=="malopolska")]<-"lesserpoland"
data_or$region[which(data_or$region=="wielkopolska")]<-"greaterpoland"
data_or$region[which(data_or$region=="wojew??dztwopodkarpackie")]<-"subcarpathia"
data_or$region[which(data_or$region=="podkarpackie")]<-"subcarpathia"
data_or$region[which(data_or$region=="lubuskie")]<-"lubusz"
data_or$region[which(data_or$region=="lubelskie")]<-"lublin"


cities1 <- levels(as.factor(data_pc$admin.name2[which(data_pc$country.code==country)]))
cities2 <- levels(as.factor(data_pc$admin.name3[which(data_pc$country.code==country)]))
regions <- levels(as.factor(data_pc$admin.name1[which(data_pc$country.code==country)]))
regions <- regions[-which(regions=="mazovia")]

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
data_or$NUTS3[as.numeric(manuals$ID[which(manuals$City=="nowys?.cz")])] <- "PL214"
data_or$NUTS3[as.numeric(manuals$ID[which(manuals$City=="k??rnik")])] <- "PL411"


data$NUTS3[indices] <- data_or$NUTS3[indices]
save(data, file="C:/Users/user/Desktop/forORCID/ORCID_2021_activities_extract_uptoPL.RData")
