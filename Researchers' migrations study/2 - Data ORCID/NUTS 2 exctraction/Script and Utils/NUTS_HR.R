library(progress)
country <- "HR"
colnames(pc2020_hr_NUTS.2021_v2.0)<-c("NUTS3", "PCode")
pc2020_hr_NUTS.2021_v2.0 <- pc2020_hr_NUTS.2021_v2.0[-1,]

data_or <- data
data_pc <- geonames.postal.codes_filtered
data_nuts <- pc2020_hr_NUTS.2021_v2.0

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

data_or$city[which(data_or$city=="split")]<-"zadar"
data_or$city[which(data_or$city=="spit")]<-"zadar"
data_or$city[which(data_or$city=="split,21000")]<-"zadar"
data_or$city[which(data_or$city=="split,croatia")]<-"zadar"
data_or$city[which(data_or$city=="split21000")]<-"zadar"
data_or$city[which(data_or$city=="cvitefiskoviä???a5,21000split")]<-"zadar"
data_or$city[which(data_or$city=="plomin")]<-"zadar"
data_or$city[which(data_or$city=="pula")]<-"zadar"
data_or$city[which(data_or$city=="betina")]<-"zadar"
data_or$city[which(data_or$city=="gospic")]<-"zadar"
data_or$city[which(data_or$city=="rovinj")]<-"zadar"
data_or$city[which(data_or$city=="umag")]<-"zadar"
data_or$city[which(data_or$city=="poreä\u008d")]<-"zadar"
data_or$city[which(data_or$city=="sibenik")]<-"zadar"
data_or$city[which(data_or$city=="porec")]<-"zadar"
data_or$city[which(data_or$city=="makarska")]<-"zadar"
data_or$city[which(data_or$city=="osijek")]<-"bjelovar"
data_or$city[which(data_or$city=="pozega")]<-"bjelovar"
data_or$city[which(data_or$city=="varazdin")]<-"krapinsketoplice"
data_or$city[which(data_or$city=="djakovo")]<-"krapinsketoplice"
data_or$city[which(data_or$city=="marusevec")]<-"krapinsketoplice"
data_or$city[which(data_or$city=="novagradska")]<-"krapinsketoplice"
data_or$city[which(data_or$city=="10430samobor")]<-"krapinsketoplice"
data_or$city[which(data_or$city=="cakovec")]<-"krapinsketoplice"
data_or$city[which(data_or$city=="zagrebcentar")]<-"zagreb"
data_or$city[which(data_or$city=="zapresic")]<-"zagreb"
data_or$city[which(data_or$city=="donjigrad")]<-"zagreb"
data_or$city[which(data_or$city=="10000zagreb")]<-"zagreb"
data_or$city[which(data_or$city=="zgreb")]<-"zagreb"
data_or$city[which(data_or$city=="zagabria")]<-"zagreb"
data_or$city[which(data_or$city=="universityofzagreb")]<-"zagreb"
data_or$city[which(data_or$city=="zagreb,hrvatska")]<-"zagreb"

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

#levels(as.factor(data$NUTS3))
#manuals

data$NUTS3[indices] <- data_or$NUTS3[indices]
save(data, file="C:/Users/user/Desktop/forORCID/ORCID_2021_activities_extract_uptoHR.RData")
