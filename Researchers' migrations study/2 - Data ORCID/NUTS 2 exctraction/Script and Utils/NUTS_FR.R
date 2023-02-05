library(progress)
country <- "FR"
colnames(pc2020_FR_NUTS.2021_v2.0)<-c("NUTS3", "PCode")
pc2020_FR_NUTS.2021_v2.0 <- pc2020_FR_NUTS.2021_v2.0[-1,]

data_or <- data
data_pc <- geonames.postal.codes_filtered
data_nuts <- pc2020_FR_NUTS.2021_v2.0

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

data_or$city[which(data_or$city=="villeurbanne")]<- "lyon"
data_or$city[which(data_or$city=="bron")]<- "lyon"
data_or$city[which(data_or$city=="saintmartind'heres")]<- "grenoble"
data_or$city[which(data_or$city=="saintmartind'hã¨res")]<- "grenoble"
data_or$city[which(data_or$city=="villeneuved'ascq")]<- "nord"


data_or$city[which(data_or$city=="talence")]<- "bordeaux"
data_or$region[which(data_or$city=="lemans")]<- "paysdelaloire"
data_or$city[which(data_or$city=="lemans")]<- "paysdelaloire"
data_or$region[which(data_or$city=="jouyenjosas")]<- "iledefrance"
data_or$city[which(data_or$city=="jouyenjosas")]<- "iledefrance"
data_or$region[which(data_or$city=="villetaneuse")]<- "iledefrance"
data_or$city[which(data_or$city=="villetaneuse")]<- "iledefrance"
data_or$region[which(data_or$city=="rueilmalmaison")]<- "iledefrance"
data_or$city[which(data_or$city=="rueilmalmaison")]<- "iledefrance"
data_or$region[which(data_or$city=="chatenaymalabry")]<- "iledefrance"
data_or$city[which(data_or$city=="chatenaymalabry")]<- "iledefrance"
data_or$region[which(data_or$city=="chã¢tenaymalabry")]<- "iledefrance"
data_or$city[which(data_or$city=="chã¢tenaymalabry")]<- "iledefrance"
data_or$region[which(data_or$city=="saclay")]<- "iledefrance"
data_or$city[which(data_or$city=="saclay")]<- "iledefrance"
data_or$region[which(data_or$city=="cachan")]<- "iledefrance"
data_or$city[which(data_or$city=="cachan")]<- "iledefrance"
data_or$region[which(data_or$city=="champssurmarne")]<- "iledefrance"
data_or$city[which(data_or$city=="champssurmarne")]<- "iledefrance"
data_or$region[which(data_or$city=="orsay")]<- "iledefrance"
data_or$city[which(data_or$city=="orsay")]<- "iledefrance"
data_or$region[which(data_or$city=="guyancourt")]<- "iledefrance"
data_or$city[which(data_or$city=="guyancourt")]<- "iledefrance"
data_or$region[which(data_or$city=="gifsuryvette")]<- "iledefrance"
data_or$city[which(data_or$city=="gifsuryvette")]<- "iledefrance"
data_or$region[which(data_or$city=="orleans")]<- "centrevaldeloire"
data_or$city[which(data_or$city=="orleans")]<- "centrevaldeloire"
data_or$city[which(data_or$city=="besanã§on")]<-"besancon"
data_or$city[which(data_or$city=="montsaintaignan")]<- "rouen"
data_or$city[which(data_or$city=="larochelle")]<- "rochefort"
data_or$region[which(data_or$city=="sophiaantipolis")]<- "provencealpescatedazur"
data_or$city[which(data_or$city=="sophiaantipolis")]<- "provencealpescatedazur"
data_or$region[which(data_or$city=="valbonne")]<- "provencealpescatedazur"
data_or$city[which(data_or$city=="valbonne")]<- "provencealpescatedazur"
data_or$region[which(data_or$city=="cergypontoise")]<- "iledefrance"
data_or$city[which(data_or$city=="cergypontoise")]<- "iledefrance"
data_or$city[which(data_or$city=="pessac")]<- "bordeaux"
data_or$city[which(data_or$city=="gradignan")]<- "bordeaux"
data_or$region[which(data_or$city=="villejuif")]<- "iledefrance"
data_or$city[which(data_or$city=="villejuif")]<- "iledefrance"
data_or$region[which(data_or$city=="evry")]<- "iledefrance"
data_or$city[which(data_or$city=="evry")]<- "iledefrance"
data_or$region[which(data_or$city=="paris")]<- "iledefrance"
data_or$region[which(data_or$city=="marnelavallee")]<- "iledefrance"
data_or$city[which(data_or$city=="marnelavallee")]<- "iledefrance"
data_or$region[which(data_or$city=="buressuryvette")]<- "iledefrance"
data_or$city[which(data_or$city=="buressuryvette")]<- "iledefrance"
data_or$region[which(data_or$city=="fontenayauxroses")]<- "iledefrance"
data_or$city[which(data_or$city=="fontenayauxroses")]<- "iledefrance"
data_or$region[which(data_or$city=="malakoff")]<- "iledefrance"
data_or$city[which(data_or$city=="malakoff")]<- "iledefrance"
data_or$region[which(data_or$city=="parã­s")]<- "iledefrance"
data_or$city[which(data_or$city=="parã­s")]<- "iledefrance"
data_or$city[which(data_or$city=="saintetienne")]<- "loire"
data_or$city[which(data_or$city=="saintetiennedurouvray")]<- "loire"
data_or$city[which(data_or$city=="lehavre")]<- "seinemaritime"
data_or$city[which(data_or$city=="compiegne")]<- "oise"
data_or$city[which(data_or$city=="castanettolosan")]<- "toulouse"
data_or$city[which(data_or$city=="illkirch")]<- "strasbourg"
data_or$city[which(data_or$city=="illkirchgraffenstaden")]<- "strasbourg"
data_or$city[which(data_or$city=="vandoeuvrelesnancy")]<- "nancy"
data_or$city[which(data_or$city=="aubiere")]<- "clermontferrand"
data_or$city[which(data_or$city=="roubaix")]<- "nord"
data_or$city[which(data_or$city=="solaize")]<- "lyon"
data_or$city[which(data_or$city=="nimes")]<- "gard"
data_or$city[which(data_or$city=="ales")]<- "gard"
data_or$city[which(data_or$city=="bagnolssurceze")]<- "gard"
data_or$city[which(data_or$city=="verneuilenhalatte")]<- "oise"

data_or$region[which(data_or$region=="rhã´nealpes")]<-"lyon"
data_or$region[which(data_or$region=="rhonealpes")]<-"lyon"
data_or$region[which(data_or$region=="ãzledefrance")]<-"iledefrance"
data_or$region[which(data_or$region=="aquitaine")]<-"bordeaux"
data_or$region[which(data_or$region=="provencealpescã´ted'azu")]<-"provencealpescatedazur"
data_or$region[which(data_or$region=="provencealpescã´ted'azur")]<-"provencealpescatedazur"
data_or$region[which(data_or$region=="lorraine")]<-"metz"
data_or$region[which(data_or$region=="bourgognefranchecomtã©")]<-"bourgognefranchecomte"
data_or$region[which(data_or$region=="auvergnerhã´nealpes")]<-"auvergnerhanealpes"
data_or$region[which(data_or$region=="auvergne")]<-"clermontferrand"
data_or$region[which(data_or$region=="centre")]<-"centrevaldeloire"
data_or$region[which(data_or$region=="poitoucharentes")]<-"poitiers"
data_or$region[which(data_or$region=="france/rã©union")]<-"saintdenis"
data_or$region[which(data_or$region=="rã©union")]<-"saintdenis"
data_or$region[which(data_or$region=="larã©union")]<-"saintdenis"
data_or$region[which(data_or$region=="reunionisland")]<-"saintdenis"


data_or$city[which(data_or$region=="alsace")]<-"strasbourg"

data_or$region[which(data_or$city=="cergy")]<-"iledefrance"
data_or$region[which(data_or$region=="brittany")]<-"bretagne"
data_or$city[which(data_or$region=="languedocroussillon")]<-"gard"
data_or$region[which(data_or$region=="ilesdefrance")]<-"iledefrance"
data_or$region[which(data_or$region=="idf")]<-"iledefrance"
data_or$region[which(data_or$region=="ã®ledefrance")]<-"iledefrance"
data_or$region[which(data_or$region=="ildefrance")]<-"iledefrance"
data_or$region[which(data_or$city=="maisonsalfort")]<-"iledefrance"
data_or$city[which(data_or$city=="banyulssurmer")]<-"cã©ret"
data_or$city[which(data_or$city=="aubiã¨re")]<-"clermont"
data_or$city[which(data_or$city=="latronche")]<-"grenoble"

data_pc$admin.name1[which(data_pc$admin.name1=="ãzledefrance")]<-"iledefrance"
data_pc$admin.name1[which(data_pc$admin.name1=="bourgognefranchecomtã©")]<-"bourgognefranchecomte"
data_pc$admin.name1[which(data_pc$admin.name1=="auvergnerhã´nealpes")]<-"auvergnerhanealpes"
data_pc$admin.name1[which(data_pc$admin.name1=="provencealpescã´ted'azur")]<-"provencealpescatedazur"


data_pc$admin.name3[which(data_pc$admin.name3=="besanã§on")]<-"besancon"



cities1 <- levels(as.factor(data_pc$admin.name2[which(data_pc$country.code==country)]))
cities2 <- levels(as.factor(data_pc$admin.name3[which(data_pc$country.code==country)]))
regions <- c("provencealpescatedazur","centrevaldeloire","paysdelaloire","bretagne","iledefrance","corse")

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

#levels(as.factor(data$NUTS3))
#manuals

data$NUTS3[indices] <- data_or$NUTS3[indices]
save(data, file="C:/Users/user/Desktop/forORCID/ORCID_2021_activities_extract_uptoFR.RData")
