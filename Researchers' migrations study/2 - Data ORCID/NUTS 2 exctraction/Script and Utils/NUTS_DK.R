library(progress)
country <- "DK"
colnames(pc2020_DK_NUTS.2021_v1.0)<-c("NUTS3", "PCode")
pc2020_DK_NUTS.2021_v1.0 <- pc2020_DK_NUTS.2021_v1.0[-1,]

data_or <- data
data_pc <- geonames.postal.codes_filtered
data_nuts <- pc2020_DK_NUTS.2021_v1.0

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


data_pc$admin.name1[which(data_pc$admin.name1=="regionhovedstaden")]<-"hovedstaden"
data_pc$admin.name1[which(data_pc$admin.name1=="regionmidtjylland")]<-"midtjylland"
data_pc$admin.name1[which(data_pc$admin.name1=="regionsyddanmark")]<-"syddanmark"

data_pc$admin.name2[which(data_pc$admin.name2=="hillerã¸d")]<-"hillerod"
data_pc$admin.name2[which(data_pc$admin.name2=="hjã¸rring")]<-"hjorring"
data_pc$admin.name2[which(data_pc$admin.name2=="hã¸rsholm")]<-"horsholm"
data_pc$admin.name2[which(data_pc$admin.name2=="ã.lborgkommune")]<-"aalborg"
data_pc$admin.name2 <- gsub("kommune", "", data_pc$admin.name2)


data_or$region[which(data_or$region=="sjaelland")]<-"zealand"
data_or$region[which(data_or$region=="sjã¦lland")]<-"zealand"
data_or$region[which(data_or$region=="regionzealand")]<-"zealand"


data_or$city[which(data_or$city=="hã¸rsholm")]<-"horsholm"
data_or$city[which(data_or$city=="hoersholm")]<-"horsholm"
data_or$city[which(data_or$city=="hillerã¸d")]<-"hillerod"
data_or$city[which(data_or$city=="hjã¸rring")]<-"hjorring"
data_or$city[which(data_or$city=="ã.lborgkommune")]<-"aalborg"
data_or$city[which(data_or$city=="ã.rhus")]<-"aarhus"
data_or$city[which(data_or$city=="aarhusc")]<-"aarhus"
data_or$city[which(data_or$city=="arhus")]<-"aarhus"
data_or$city[which(data_or$city=="aarhusn")]<-"aarhus"
data_or$city[which(data_or$city=="frederiksbergc")]<-"frederiksberg"
data_or$city[which(data_or$city=="hellerup")]<-"gentofte"
data_or$city[which(data_or$city=="kã¸benhavn")]<-"copenhagen"
data_or$city[which(data_or$city=="kã¸benhavnk")]<-"copenhagen"
data_or$city[which(data_or$city=="copenhagenn")]<-"copenhagen"
data_or$city[which(data_or$city=="copenhagennv")]<-"copenhagen"
data_or$city[which(data_or$city=="copenhagenk")]<-"copenhagen"
data_or$city[which(data_or$city=="copenhagenarea")]<-"copenhagen"
data_or$city[which(data_or$city=="copenhagenã~")]<-"copenhagen"
data_or$city[which(data_or$city=="copenahgen")]<-"copenhagen"
data_or$city[which(data_or$city=="copanhagen")]<-"copenhagen"
data_or$city[which(data_or$city=="copenhage")]<-"copenhagen"
data_or$city[which(data_or$city=="kobenhavn")]<-"copenhagen"
data_or$city[which(data_or$city=="kobenhavns")]<-"copenhagen"
data_or$city[which(data_or$city=="copehangen")]<-"copenhagen"
data_or$city[which(data_or$city=="copenghagen")]<-"copenhagen"
data_or$city[which(data_or$city=="copenhaga")]<-"copenhagen"
data_or$city[which(data_or$city=="copenhagensv")]<-"copenhagen"
data_or$city[which(data_or$city=="copenhague")]<-"copenhagen"
data_or$city[which(data_or$city=="copenhaguen")]<-"copenhagen"
data_or$city[which(data_or$city=="kgs.lyngby")]<-"copenhagen"
data_or$city[which(data_or$city=="lyngby")]<-"copenhagen"
data_or$city[which(data_or$city=="kongenslyngby")]<-"copenhagen"
data_or$city[which(data_or$city=="kgslyngby")]<-"copenhagen"
data_or$city[which(data_or$city=="hvidovre")]<-"copenhagen"
data_or$city[which(data_or$city=="aalborgã~st")]<-"aalborg"
data_or$city[which(data_or$city=="aalborgost")]<-"aalborg"
data_or$city[which(data_or$city=="ballerupmunicipality")]<-"ballerup"
data_or$city[which(data_or$city=="roskilde")]<-"zealand"

data_or$region[which(data_or$region=="regionnordjylland")] <- "northdenmark"
data_or$region[which(data_or$region=="nordjylland")] <- "northdenmark"
data_or$region[which(data_or$region=="northdenmarkregion")] <- "northdenmark"
data_or$region[which(data_or$city=="odensem")] <- "syddanmark"
data_or$region[which(data_or$city=="taastrup")] <- "hovedstaden"
data_or$region[which(data_or$city=="charlottenlund")] <- "hovedstaden"
data_or$region[which(data_or$city=="bagsvaerd")] <- "hovedstaden"
data_or$region[which(data_or$city=="bagsvã¦rd")] <- "hovedstaden"
data_or$region[which(data_or$city=="hjorring")] <- "northdenmark"
data_or$region[which(data_or$city=="hirtshals")] <- "northdenmark"
data_or$region[which(data_or$city=="sonderborg")] <- "southdenmark"
data_or$region[which(data_or$city=="koege")] <- "zealand"
data_or$city[which(data_or$city=="tjele")] <- "viborg"
data_or$city[which(data_or$city=="risskov")] <- "aarhus"
data_or$city[which(data_or$city=="odensec")] <- "odense"
data_or$city[which(data_or$city=="valby")] <- "copenhagen"
data_or$city[which(data_or$city=="copenhagens")] <- "copenhagen"
data_or$city[which(data_or$city=="copehagen")] <- "copenhagen"
data_or$city[which(data_or$city=="kobenhavno")] <- "copenhagen"
data_or$city[which(data_or$city=="kã¸benhavnv")] <- "copenhagen"
data_or$city[which(data_or$city=="maaloev")] <- "ballerup"

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

levels(as.factor(data_or$NUTS3))
#manuals

data$NUTS3[indices] <- data_or$NUTS3[indices]
save(data, file="C:/Users/user/Desktop/forORCID/ORCID_2021_activities_extract_uptoDK.RData")
