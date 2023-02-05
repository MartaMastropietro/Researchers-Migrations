library(progress)
country <- "BE"
colnames(pc2020_BE_NUTS.2021_v1.0)<-c("NUTS3", "PCode")
pc2020_BE_NUTS.2021_v1.0 <- pc2020_BE_NUTS.2021_v1.0[-1,]

data_or <- data
data_pc <- geonames.postal.codes_filtered
data_nuts <- pc2020_BE_NUTS.2021_v1.0

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

cities1[which(cities1=="bruxelles(19communes)")]<-"bruxelles"
data_pc$admin.name2[which(data_pc$admin.name2=="bruxelles(19communes)")]<-"bruxelles"

data_or$city[which(data_or$city=="brussels")]<-"bruxelles"
data_or$city[which(data_or$city=="bruselles")]<-"bruxelles"
data_or$city[which(data_or$city=="brussles")]<-"bruxelles"
data_or$city[which(data_or$city=="brussels,belgium")]<-"bruxelles"
data_or$city[which(data_or$city=="brusselsbelgium")]<-"bruxelles"
data_or$city[which(data_or$city=="bruxel")]<-"bruxelles"
data_or$city[which(data_or$city=="1050brussel")]<-"bruxelles"
data_or$city[which(data_or$city=="bruxelle")]<-"bruxelles"
data_or$city[which(data_or$city=="bruxells")]<-"bruxelles"
data_or$city[which(data_or$city=="bruessels")]<-"bruxelles"
data_or$city[which(data_or$city=="bruxels")]<-"bruxelles"
data_or$city[which(data_or$city=="bruseels")]<-"bruxelles"
data_or$city[which(data_or$city=="brusel")]<-"bruxelles"
data_or$city[which(data_or$city=="brusells")]<-"bruxelles"
data_or$city[which(data_or$city=="brussel/brussels")]<-"bruxelles"
data_or$city[which(data_or$city=="brussel(jette)")]<-"bruxelles"
data_or$city[which(data_or$city=="bruxelles[1050]")]<-"bruxelles"
data_or$city[which(data_or$city=="brussel")]<-"bruxelles"
data_or$city[which(data_or$city=="bruselas")]<-"bruxelles"
data_or$city[which(data_or$city=="bruxelas")]<-"bruxelles"
data_or$city[which(data_or$city=="bruxcel")]<-"bruxelles"
data_or$city[which(data_or$city=="elsene")]<-"bruxelles"
data_or$city[which(data_or$city=="jette")]<-"bruxelles"
data_or$city[which(data_or$city=="woluwesaintlambert")]<-"bruxelles"
data_or$city[which(data_or$city=="woluwã©saintlambert")]<-"bruxelles"
data_or$city[which(data_or$city=="woluwã©stlambert")]<-"bruxelles"
data_or$city[which(data_or$city=="woluwe")]<-"bruxelles"
data_or$city[which(data_or$city=="uccle")]<-"bruxelles"
data_or$city[which(data_or$city=="ukkel")]<-"bruxelles"
data_or$city[which(data_or$city=="brã¼ssel")]<-"bruxelles"
data_or$city[which(data_or$city=="ghent")]<-"gent"
data_or$city[which(data_or$city=="melle")]<-"gent"
data_or$city[which(data_or$city=="sintmartenslatem")]<-"gent"
data_or$city[which(data_or$city=="zwijnaarde")]<-"gent"
data_or$city[which(data_or$city=="b9000ghent")]<-"gent"
data_or$city[which(data_or$city=="ghen")]<-"gent"
data_or$city[which(data_or$city=="ghent(9000)")]<-"gent"
data_or$city[which(data_or$city=="ghent(university)")]<-"gent"
data_or$city[which(data_or$city=="deinze")]<-"gent"
data_or$city[which(data_or$city=="b9000gent,belgium")]<-"gent"
data_or$city[which(data_or$city=="merelbeke")]<-"gent"
data_or$city[which(data_or$city=="gand")]<-"gent"
data_or$city[which(data_or$city=="gante")]<-"gent"
data_or$city[which(data_or$city=="zelzate")]<-"eeklo"
data_or$city[which(data_or$city=="zottegem")]<-"aalst"
data_or$city[which(data_or$city=="lommel")]<-"maaseik"
data_or$city[which(data_or$city=="louvainlaneuve")]<-"nivelles"
data_or$city[which(data_or$city=="louvianlaneuve")]<-"nivelles"
data_or$city[which(data_or$city=="louvainlanauve")]<-"nivelles"
data_or$city[which(data_or$city=="louvainlaneue")]<-"nivelles"
data_or$city[which(data_or$city=="1.2348louvainlaneuve")]<-"nivelles"
data_or$city[which(data_or$city=="louvainlaneuvelln")]<-"nivelles"
data_or$city[which(data_or$city=="louvainlaneuve(ucl)")]<-"nivelles"
data_or$city[which(data_or$city=="brainel'alleud")]<-"nivelles"
data_or$city[which(data_or$city=="ottignies")]<-"nivelles"
data_or$city[which(data_or$city=="ottignieslouvainlaneuve")]<-"nivelles"
data_or$city[which(data_or$city=="ottignieslln")]<-"nivelles"
data_or$city[which(data_or$city=="waterloo")]<-"nivelles"
data_or$city[which(data_or$city=="rixensart")]<-"nivelles"
data_or$city[which(data_or$city=="montsaintguibert")]<-"nivelles"
data_or$city[which(data_or$city=="antwerp")]<-"anvers"
data_or$city[which(data_or$city=="mol")]<-"anvers"
data_or$city[which(data_or$city=="wilrijk")]<-"anvers"
data_or$city[which(data_or$city=="niel")]<-"anvers"
data_or$city[which(data_or$city=="sintkatelijnewaver")]<-"anvers"
data_or$city[which(data_or$city=="geel")]<-"turnhout"
data_or$city[which(data_or$city=="wavre")]<-"nivelles"
data_or$city[which(data_or$city=="yvoir")]<-"dinant"
data_or$city[which(data_or$city=="diepenbeek")]<-"limbourg"
data_or$city[which(data_or$city=="bruges")]<-"brugge"
data_or$city[which(data_or$city=="brugges")]<-"brugge"
data_or$city[which(data_or$city=="bouge")]<-"namur"
data_or$city[which(data_or$city=="gembloux")]<-"namur"
data_or$city[which(data_or$city=="diegem")]<-"mechelen"
data_or$city[which(data_or$city=="ostend")]<-"oostende"
data_or$city[which(data_or$city=="heverlee")]<-"leuven"
data_or$city[which(data_or$city=="leuven(heverlee)")]<-"leuven"
data_or$city[which(data_or$city=="leuven,flanders")]<-"leuven"
data_or$city[which(data_or$city=="leuvenheverlee")]<-"leuven"
data_or$city[which(data_or$city=="louvain")]<-"leuven"
data_or$city[which(data_or$city=="kortenberg")]<-"leuven"
data_or$city[which(data_or$city=="tienen")]<-"leuven"
data_or$city[which(data_or$city=="universitã©catholiquedelouvain")]<-"leuven"
data_or$city[which(data_or$city=="everberg")]<-"leuven"
data_or$city[which(data_or$city=="tervuren")]<-"leuven"
data_or$city[which(data_or$city=="machelen")]<-"hallevilvoorde"
data_or$city[which(data_or$city=="grimbergen")]<-"hallevilvoorde"
data_or$city[which(data_or$city=="asse")]<-"hallevilvoorde"
data_or$city[which(data_or$city=="sintgenesiusrode")]<-"hallevilvoorde"
data_or$city[which(data_or$city=="vilvoorde")]<-"hallevilvoorde"
data_or$city[which(data_or$city=="meise")]<-"hallevilvoorde"
data_or$city[which(data_or$city=="zaventem")]<-"hallevilvoorde"
data_or$city[which(data_or$city=="dilbeek")]<-"hallevilvoorde"
data_or$city[which(data_or$city=="kraainem")]<-"hallevilvoorde"
data_or$city[which(data_or$city=="genk")]<-"hasselt"
data_or$city[which(data_or$city=="sinttruiden")]<-"hasselt"
data_or$city[which(data_or$city=="gosselies")]<-"charleroi"
data_or$city[which(data_or$city=="fleurus")]<-"charleroi"
data_or$city[which(data_or$city=="seneffe")]<-"charleroi"
data_or$city[which(data_or$city=="zwevegem")]<-"kortrijk"
data_or$city[which(data_or$city=="waregem")]<-"kortrijk"
data_or$city[which(data_or$city=="wetteren")]<-"dendermonde"
data_or$city[which(data_or$city=="lalouviere")]<-"soignies"
data_or$city[which(data_or$city=="lalouviã¨re")]<-"soignies"
data_or$city[which(data_or$city=="bonheiden")]<-"anvers"
data_or$city[which(data_or$city=="anwerp")]<-"anvers"
data_or$city[which(data_or$city=="antwertp")]<-"anvers"
data_or$city[which(data_or$city=="antwerp,belgium")]<-"anvers"
data_or$city[which(data_or$city=="antwerpen/antwerp")]<-"anvers"
data_or$city[which(data_or$city=="mortsel")]<-"anvers"
data_or$city[which(data_or$city=="lier")]<-"anvers"
data_or$city[which(data_or$city=="edegem")]<-"anvers"
data_or$city[which(data_or$city=="amberes")]<-"anvers"
data_or$city[which(data_or$city=="wommelgem")]<-"anvers"
data_or$city[which(data_or$city=="aartselaar")]<-"anvers"
data_or$city[which(data_or$city=="puurs")]<-"anvers"
data_or$city[which(data_or$city=="beerse")]<-"turnhout"
data_or$city[which(data_or$city=="olen")]<-"turnhout"
data_or$city[which(data_or$city=="liege")]<-"liã¨ge"
data_or$city[which(data_or$city=="liã©ge")]<-"liã¨ge"
data_or$city[which(data_or$city=="liege,belgium")]<-"liã¨ge"
data_or$city[which(data_or$city=="b9100sintniklaas")]<-"sintniklaas"
data_or$city[which(data_or$city=="libramont")]<-"neufchã¢teau"
data_or$city[which(data_or$city=="lokeren")]<-"sintniklaas"
data_or$region[which(data_or$region=="brussel")]<-"bruxelles"
data_or$region[which(data_or$region=="brussels")]<-"bruxelles"
data_or$region[which(data_or$region=="bruselas")]<-"bruxelles"
data_or$region[which(data_or$region=="antwerp")]<-"anvers"
data_or$region[which(data_or$region=="liege")]<-"liã¨ge"


manuals <- data.frame(cbind(Region = c("Region"), City = c("City")))

pb <- progress_bar$new(total=length(indices))
pb$tick(0)

for(i in indices){
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
save(data, file="C:/Users/user/Desktop/forORCID/ORCID_2021_activities_extract_uptoBE.RData")
