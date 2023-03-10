library(progress)
country <- "BG"
colnames(pc2020_BG_NUTS.2021_v1.0)<-c("NUTS3", "PCode")
pc2020_BG_NUTS.2021_v1.0 <- pc2020_BG_NUTS.2021_v1.0[-1,]

data_or <- data
data_pc <- geonames.postal.codes_filtered
data_nuts <- pc2020_BG_NUTS.2021_v1.0

levels(as.factor(data_pc$admin.name1[which(data_pc$country.code == country)]))
levels(as.factor(data_pc$admin.name2[which(data_pc$country.code == country)]))
levels(as.factor(data_pc$admin.name3[which(data_pc$country.code == country)]))


#leave only english name
data_pc$admin.name1 <- gsub("^.*?/","",data_pc$admin.name1)
data_pc$admin.name2 <- gsub("^.*?/","",data_pc$admin.name2)
data_pc$admin.name3 <- gsub("^.*?/","",data_pc$admin.name3)



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

data_or$city[which(data_or$city=="sofia")]<-"sofija"
data_or$city[which(data_or$city=="sofia,")]<-"sofija"
data_or$city[which(data_or$city=="sophia")]<-"sofija"
data_or$city[which(data_or$city=="sofya")]<-"sofija"
data_or$city[which(data_or$city=="sofiq")]<-"sofija"
data_or$city[which(data_or$city=="?????????")]<-"sofija"
data_or$city[which(data_or$city=="1113,sofia")]<-"sofija"
data_or$city[which(data_or$city=="sofiacity")]<-"sofija"
data_or$city[which(data_or$city=="sofiastateuni.")]<-"sofija"
data_or$city[which(data_or$city=="sofiag")]<-"sofija"
data_or$city[which(data_or$city=="sofia,bulgaria")]<-"sofija"
data_or$city[which(data_or$city=="velikotarnovo")]<-"velikoturnovo"
data_or$city[which(data_or$city=="svishtov")]<-"velikoturnovo"
data_or$city[which(data_or$city=="vratza")]<-"vraca"
data_or$city[which(data_or$city=="kozloduy")]<-"vraca"
data_or$city[which(data_or$city=="?'??????????")]<-"varna"
data_or$city[which(data_or$city=="vratsa")]<-"vraca"
data_or$city[which(data_or$city=="sumen")]<-"shumen"
data_or$city[which(data_or$city=="shoumen")]<-"shumen"
data_or$city[which(data_or$city=="russe")]<-"ruse"
data_or$city[which(data_or$city=="rousse")]<-"ruse"
data_or$city[which(data_or$city=="bourgas")]<-"burgas"
data_or$city[which(data_or$city=="yambol")]<-"jambol"
data_or$city[which(data_or$city=="kyustendil")]<-"kjustendil"
data_or$city[which(data_or$city=="kazanlak")]<-"starazagora"
data_or$city[which(data_or$city=="pazardjik")]<-"pazardzhik"
data_or$city[which(data_or$city=="pazard??ik")]<-"pazardzhik"
data_or$city[which(data_or$city=="plovduv")]<-"plovdiv"
data_or$city[which(data_or$city=="plovdivtown")]<-"plovdiv"
data_or$city[which(data_or$city=="??????????????")]<-"plovdiv"
data_or$city[which(data_or$city=="??????.??????????????")]<-"plovdiv"
data_or$city[which(data_or$city=="saintvlas")]<-"svetivlas"
data_or$city[which(data_or$city=="????????????")]<-"pleven"
data_or$city[which(data_or$city=="haskovo")]<-"khaskovo"
data_or$city[which(data_or$city=="?'??????????????????????")]<-"blagoevgrad"
data_or$region[which(data_or$region=="sofia")]<-"sofija"
data_or$region[which(data_or$region=="sofiacity")]<-"sofija"
data_or$region[which(data_or$region=="?????????")]<-"sofija"
data_or$region[which(data_or$region=="velikotarnovo")]<-"velikoturnovo"
data_or$region[which(data_or$region=="?'?????????????s????????????")]<-"velikoturnovo"
data_or$region[which(data_or$region=="??????????????")]<-"velikoturnovo"
data_or$region[which(data_or$region=="vratsa")]<-"vraca"
data_or$region[which(data_or$region=="vratza")]<-"vraca"
data_or$region[which(data_or$region=="sboryanovoarchaeologicalreserve")]<-"isperikh"

data_or$city[which(data_or$city=="?????"???\u008f")] <- "sofija"
data_or$city[which(data_or$city=="1606sofia")] <- "sofija"
data_or$city[which(data_or$city=="\tsofia")] <- "sofija"
data_or$city[which(data_or$city=="sofia1113")] <- "sofija"
data_or$city[which(data_or$city=="6000starazagora")] <- "starazagora"
data_or$city[which(data_or$city=="?'??????????")] <- "varna"
data_or$city[which(data_or$city=="?'?f?????????\u0081")] <- "burgas"
data_or$city[which(data_or$city=="?'?????????????s????????????")] <- "velikoturnovo"
data_or$city[which(data_or$city=="velikotarnovo,bulgaria")] <- "velikoturnovo"
data_or$city[which(data_or$city=="???f??????")] <- "shumen"
data_or$city[which(data_or$city=="?'??????????????????????")] <- "blagoevgrad"


manuals <- data.frame(cbind(Region = c("Region"), City = c("City")))

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
    manuals <- rbind(manuals, c(data_or$region[i], data_or$city[i]))
  }
  pb$tick()
}

#levels(as.factor(data_or$NUTS3[which(data_or$country==country)]))
#manuals

data$NUTS3[indices] <- data_or$NUTS3[indices]
save(data, file="C:/Users/user/Desktop/forORCID/ORCID_2021_activities_extract_uptoBG.RData")
