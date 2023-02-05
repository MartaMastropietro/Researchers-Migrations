#switzerland 
library(progress)
country <- "CH"

#pc<-read.table("D:/Marta/Politecnico/Tesi/NUTS adders/pc2020_CH_NUTS-2021_v1.0.csv", sep=";", quote="'", comment.char="")
pc <- pc2020_CH_NUTS.2021_v1.0
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

### correzioni nomi pc

regions <- levels(as.factor(data_pc$admin.name1[which(data_pc$country.code==country)]))

data_pc$admin.name1[which(data_pc$admin.name1==regions[5])]<-"geneve"
data_pc$admin.name1[which(data_pc$admin.name1==regions[13])]<-"graubanden"
data_pc$admin.name1[which(data_pc$admin.name1=="kantonst.gallen")]<-"stgallen"
data_pc$admin.name1[which(data_pc$admin.name1==regions[23])]<-"zurich"
data_pc$admin.name1[which(data_pc$admin.name1=="cantondeberne")]<-"berne"
data_pc$admin.name1[which(data_pc$admin.name1=="cantondefribourg")]<-"fribourg"
data_pc$admin.name1[which(data_pc$admin.name1=="cantondevaud")]<-"vaud"
data_pc$admin.name1[which(data_pc$admin.name1=="kantonappenzellausserrhoden")]<-"appenzellausserrhoden"
data_pc$admin.name1[which(data_pc$admin.name1=="kantonappenzellinnerrhoden")]<-"appenzellinnerrhoden"
data_pc$admin.name1[which(data_pc$admin.name1=="kantonbasellandschaft")]<-"basellandschaft"
data_pc$admin.name1[which(data_pc$admin.name1=="kantonbaselstadt")]<-"baselstadt"
data_pc$admin.name1[which(data_pc$admin.name1=="kantonglarus")]<-"glarus"
data_pc$admin.name1[which(data_pc$admin.name1=="kantonluzern")]<-"luzern"
data_pc$admin.name1[which(data_pc$admin.name1=="kantonnidwalden")]<-"nidwalden"
data_pc$admin.name1[which(data_pc$admin.name1=="kantonobwalden")]<-"obwalden"
data_pc$admin.name1[which(data_pc$admin.name1=="kantonschaffhausen")]<-"schaffhausen"
data_pc$admin.name1[which(data_pc$admin.name1=="kantonschwyz")]<-"schwyz"
data_pc$admin.name1[which(data_pc$admin.name1=="kantonsolothurn")]<-"olothurn"
data_pc$admin.name1[which(data_pc$admin.name1=="kantonthurgau")]<-"thurgau"
data_pc$admin.name1[which(data_pc$admin.name1=="kantonzug")]<-"zug"
data_pc$admin.name1[which(data_pc$admin.name1=="kantonaargau")]<-"aargau"
data_pc$admin.name1[which(data_pc$admin.name1=="kantonstgallen")]<-"stgallen"
data_pc$admin.name1[which(data_pc$admin.name1=="kantonuri")]<-"uri"
data_pc$admin.name1[which(data_pc$admin.name1=="kantonzurich")]<-"zurich"
data_pc$admin.name1[which(data_pc$admin.name1=="cantonduvalais")]<-"valais"
data_pc$admin.name1[which(data_pc$admin.name1=="kantongraubanden")]<-"graubanden"
data_pc$admin.name1[which(data_pc$admin.name1=="ginevra")]<-"geneve"


###


cities1 <- levels(as.factor(data_pc$admin.name2[which(data_pc$country.code==country)]))
cities2 <- levels(as.factor(data_pc$admin.name3[which(data_pc$country.code==country)]))
regions <- levels(as.factor(data_pc$admin.name1[which(data_pc$country.code==country)]))

### da modificare Paaese dipendente 

data_or$city[which(data_or$city=="dubendorf")]<-"zurich"
data_or$city[which(data_or$city=="sanktgallen")]<-"stgallen"
data_or$city[which(data_or$city=="zuerich")]<-"zurich"

data_or$city[which(data_or$city=="badendã¤ttwil")]<-"aargau"
data_or$city[which(data_or$city=="badendattwil")]<-"aargau"
data_or$city[which(data_or$city=="basilea")]<-"baselstadt"
data_or$city[which(data_or$city=="berninselspital")]<-"berne"
data_or$city[which(data_or$city=="biel")]<-"berne"
data_or$city[which(data_or$city=="bienne")]<-"berne"
data_or$city[which(data_or$city=="birmensdorf")]<-"zurich"
data_or$city[which(data_or$city=="brig")]<-"valais"
data_or$city[which(data_or$city=="bruggwindisch")]<-"aargau"
data_or$city[which(data_or$city=="buchs")]<-"stgallen"
data_or$city[which(data_or$city=="carouge")]<-"geneve"
data_or$city[which(data_or$city=="chambesy")]<-"geneve"
data_or$city[which(data_or$city=="davosdorf")]<-"graubanden"

data_or$city[which(data_or$city=="delemont")]<-"jura"
data_or$city[which(data_or$city=="duebendorf")]<-"zurich"
data_or$city[which(data_or$city=="freiburg")]<-"fribourg"
data_or$city[which(data_or$city=="fribourg/freiburg")]<-"fribourg"
data_or$city[which(data_or$city=="ginebra")]<-"geneve"
data_or$city[which(data_or$city=="kastanienbaum")]<-"luzern"
data_or$city[which(data_or$city=="laussane")]<-"vaud"
data_or$city[which(data_or$city=="lelignon")]<-"geneve"
data_or$city[which(data_or$city=="lucerne")]<-"luzern"
data_or$city[which(data_or$city=="mannolugano")]<-"ticino"
data_or$city[which(data_or$city=="rapperswil")]<-"stgallen"
data_or$city[which(data_or$city=="rotkreuz")]<-"zug"
data_or$city[which(data_or$city=="sauverny")]<-"geneve"
data_or$city[which(data_or$city=="stafa")]<-"nidwalen"
data_or$city[which(data_or$city=="viganello")]<-"ticino"
data_or$city[which(data_or$city=="villigenpsi")]<-"aargau"
data_or$city[which(data_or$city=="wadenswil")]<-"zurich"
data_or$city[which(data_or$city=="yverdon")]<-"vaud"
data_or$city[which(data_or$city=="zurichhonggerberg")]<-"zurich"
data_or$city[which(data_or$city=="zurichschlieren")]<-"zurich"
data_or$city[which(data_or$city=="zurique")]<-"zurich"

data_or$city[which(data_or$city=="waedenswil")]<-"zurich"
data_or$city[which(data_or$city=="wetzikon")]<-"zurich"
data_or$city[which(data_or$city=="renens")]<-"vaud"
data_or$city[which(data_or$city=="genebra")]<-"geneve"
data_or$city[which(data_or$city=="ettenhausen")]<-"thurgau"
data_or$city[which(data_or$city=="chavannes")]<-"vaud"



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
data_or$NUTS3[as.numeric(manuals$ID[which(manuals$Region=="ne")])] <- "CH024"
data_or$NUTS3[as.numeric(manuals$ID[which(manuals$Region=="vd")])] <- "CH011"
data_or$NUTS3[as.numeric(manuals$ID[which(manuals$Region=="ge")])] <- "CH013"
data_or$NUTS3[as.numeric(manuals$ID[which(manuals$City=="neuchatel")])] <- "CH024"
data_or$NUTS3[as.numeric(manuals$ID[which(manuals$City=="windisch")])] <- "CH033"
data_or$NUTS3[as.numeric(manuals$ID[which(manuals$City=="rã¼schlikon")])] <- "CH040"
data_or$NUTS3[as.numeric(manuals$ID[which(manuals$City=="zollikofen")])] <- "CH021"

data$NUTS3[indices] <- data_or$NUTS3[indices]
save(data, file="C:/Users/user/Desktop/forORCID/ORCID_2021_activities_extract_uptoCH.RData")