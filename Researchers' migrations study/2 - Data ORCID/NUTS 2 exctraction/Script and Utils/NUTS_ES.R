library(progress)
country <- "ES"
colnames(pc2020_ES_NUTS.2021_v1.0)<-c("NUTS3", "PCode")
pc2020_ES_NUTS.2021_v1.0 <- pc2020_ES_NUTS.2021_v1.0[-1,]

data_or <- data
data_pc <- geonames.postal.codes_filtered
data_nuts <- pc2020_ES_NUTS.2021_v1.0

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


data_pc$admin.name2[which(data_pc$admin.name2=="mã¡laga")]<-"malaga"
data_pc$admin.name2[which(data_pc$admin.name2=="cã¡diz")]<-"cadiz"
data_pc$admin.name2[which(data_pc$admin.name2=="leã³n")]<-"leon"
data_pc$admin.name2[which(data_pc$admin.name2=="jaã©n")]<-"jaen"
data_pc$admin.name2[which(data_pc$admin.name2=="castellã³n")]<-"castellodelaplana"
data_pc$admin.name2[which(data_pc$admin.name2=="guipãºzcoa")]<-"gipuzcoa"

data_or$city[which(data_or$city=="mã¡laga")]<-"malaga"
data_or$city[which(data_or$city=="cã¡diz")]<-"cadiz"
data_or$city[which(data_or$city=="leã³n")]<-"leon"
data_or$city[which(data_or$city=="jaã©n")]<-"jaen"
data_or$city[which(data_or$city=="guipãºzcoa")]<-"gipuzcoa"
data_or$city[which(data_or$city=="vitoriagasteiz")]<-"gipuzcoa"
data_or$city[which(data_or$city=="vitoria")]<-"gipuzcoa"
data_or$city[which(data_or$city=="sansebastian")]<-"gipuzcoa"
data_or$city[which(data_or$city=="palma")]<-"baleares"
data_or$city[which(data_or$city=="castellã³n")]<-"castellodelaplana"
data_or$city[which(data_or$city=="castellondelaplana")]<-"castellodelaplana"
data_or$city[which(data_or$city=="castellã³delaplana")]<-"castellodelaplana"
data_or$city[which(data_or$city=="castellã³ndelaplana")]<-"castellodelaplana"
data_or$city[which(data_or$city=="bellaterra")]<-"barcelona"
data_or$city[which(data_or$city=="cerdanyoladelvallã¨s")]<-"barcelona"
data_or$city[which(data_or$city=="cerdanyoladelvalles")]<-"barcelona"
data_or$city[which(data_or$city=="bellaterra(cerdanyoladelvallã¨s)")]<-"barcelona"
data_or$city[which(data_or$city=="lalaguna")]<-"santacruzdetenerife"
data_or$city[which(data_or$city=="derio")]<-"paisvasco"
data_or$city[which(data_or$city=="donostiasansebastian")]<-"paisvasco"
data_or$city[which(data_or$city=="donostiasansebastiã¡n")]<-"paisvasco"
data_or$city[which(data_or$city=="donostia")]<-"paisvasco"
data_or$city[which(data_or$city=="sansebastiã¡n")]<-"paisvasco"
data_or$city[which(data_or$city=="avila")]<-"castillaleon"
data_or$city[which(data_or$city=="valã¨ncia")]<-"comunidadvalenciana"
data_or$city[which(data_or$city=="sanvicentedelraspeig")]<-"comunidadvalenciana"
data_or$city[which(data_or$city=="almeria")]<-"andalucia"
data_or$city[which(data_or$city=="cordova")]<-"andalucia"

data_or$region[which(data_or$region=="paã­svasco")]<-"paisvasco"
data_or$region[which(data_or$region=="basquecountry")]<-"paisvasco"
data_or$region[which(data_or$region=="bizkaia")]<-"paisvasco"
data_or$region[which(data_or$region=="comunidaddemadrid")]<-"madrid"
data_or$region[which(data_or$region=="gipuzkoa")]<-"gipuzcoa"
data_or$region[which(data_or$region=="guipuzcoa")]<-"gipuzcoa"
data_or$region[which(data_or$region=="andalucã­a")]<-"andalucia"
data_or$region[which(data_or$region=="andalucã\u008da")]<-"andalucia"
data_or$region[which(data_or$region=="catalonia")]<-"cataluna"
data_or$region[which(data_or$region=="catalunya")]<-"cataluna"
data_or$region[which(data_or$region=="aragã³n")]<-"aragon"
data_or$region[which(data_or$region=="comunitatvalenciana")]<-"comunidadvalenciana"
data_or$region[which(data_or$region=="valenciana")]<-"comunidadvalenciana"
data_or$region[which(data_or$region=="illesbalears")]<-"baleares"
data_or$region[which(data_or$region=="islasbaleares")]<-"baleares"
data_or$region[which(data_or$region=="balearicislands")]<-"baleares"
data_or$region[which(data_or$region=="castillayleã³n")]<-"castillaleon"
data_or$region[which(data_or$region=="catalonia³n")]<-"cataluna"
data_or$region[which(data_or$region=="cataluã±a")]<-"cataluna"
data_or$region[which(data_or$region=="islascanarias")]<-"canarias"
data_or$region[which(data_or$region=="canaryislands")]<-"canarias"


cities1 <- levels(as.factor(data_pc$admin.name2[which(data_pc$country.code==country)]))
cities2 <- levels(as.factor(data_pc$admin.name3[which(data_pc$country.code==country)]))
regions <- levels(as.factor(data_pc$admin.name1[which(data_pc$country.code==country)]))


manuals <- data.frame(cbind(Region = c("Region"), City = c("City")))

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
    manuals <- rbind(manuals, c(data_or$region[i], data_or$city[i]))
  }
  pb$tick()
}

#levels(as.factor(data_or$NUTS3))
#manuals

data$NUTS3[indices] <- data_or$NUTS3[indices]
save(data, file="C:/Users/user/Desktop/forORCID/ORCID_2021_activities_extract_uptoES.RData")
