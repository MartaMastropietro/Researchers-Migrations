library(progress)
country <- "IT"

#pc<-read.table("D:/Marta/Politecnico/Tesi/NUTS adders/pc2020_IT_NUTS-2021_v1.0.csv", sep=";", quote="'", comment.char="")
pc <- pc2020_IT_NUTS.2021_v1.0
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


###
data_pc$admin.name1[which(data_pc$admin.name1=="abruzzi")] <- "abruzzo"
data_or$city[which(data_or$city=="rome")]<-"roma"
data_or$city[which(data_or$region=="rome")]<-"roma"
data_or$city[which(data_or$city=="forlã¬")]<-"forli"
data_or$city[which(data_or$city=="milan")]<-"milano"
data_or$city[which(data_or$city=="florence")]<-"firenze"
data_or$city[which(data_or$city=="turin")]<-"torino"
data_or$region[which(data_or$region=="turin")]<-"torino"
data_or$region[which(data_or$region=="piedmont")]<-"piemonte"
data_or$region[which(data_or$region=="tuscany")]<-"toscana"
data_or$region[which(data_or$region=="lombardy")]<-"lombardia"
data_or$region[which(data_or$region=="lumbardy")]<-"lombardia"
data_or$region[which(data_or$region=="apulia")]<-"puglia"
data_or$region[which(data_or$region=="italy/abruzzo")]<-"abruzzo"
data_or$region[which(data_or$region=="sicily")]<-"sicilia"
data_or$region[which(data_or$region=="naples")]<-"napoli"
data_or$region[which(data_or$region=="massa/toscana")]<-"toscana"
data_or$region[which(data_or$region=="trento")]<-"trentino-alto adige"
data_or$region[which(data_or$region=="sardinia")]<-"sardegna"
data_or$region[which(data_or$city=="valled'aosta")]<-"valled'aosta"
data_or$region[which(data_or$city=="aosta")]<-"valled'aosta"
data_or$city[which(data_or$region=="milan")]<-"milano"
data_or$city[which(data_or$region=="torino")]<-"torino"
data_or$city[which(data_or$region=="rimini")]<-"rimini"
data_or$city[which(data_or$region=="pisa")]<-"pisa"
data_or$city[which(data_or$region=="firenze")]<-"firenze"
data_or$city[which(data_or$region=="padova/padua")]<-"padova"
data_or$city[which(data_or$city=="naples")]<-"napoli"
data_or$city[which(data_or$city=="bustoarsizio(varese)")]<-"varese"
data_or$city[which(data_or$city=="padua")]<-"padova"
data_or$city[which(data_or$city=="padiva")]<-"padova"
data_or$region[which(data_or$region=="padua")]<-"padova"
data_or$city[which(data_or$city=="fisciano(sa)")]<-"fisciano"
data_or$city[which(data_or$city=="genoa")]<-"genova"
data_or$city[which(data_or$city=="venice")]<-"venezia"
data_or$city[which(data_or$city=="fienze")]<-"firenze"
data_or$city[which(data_or$city=="florence")]<-"firenze"
data_or$city[which(data_or$city=="arcofelice(naples)")]<-"napoli"
data_or$city[which(data_or$city=="pozzuoli(naples)")]<-"napoli"
data_or$city[which(data_or$city=="universityofferrara")]<-"ferrara"
data_or$city[which(data_or$city=="monterotondoscalo(rome)")]<-"roma"
data_or$city[which(data_or$city=="eboli,salerno")]<-"salerno"
data_or$city[which(data_or$city=="baronissi,salerno")]<-"salerno"
data_or$city[which(data_or$city=="lainate")]<-"linate"
data_or$city[which(data_or$city=="reggiocalabria")]<-"reggiodicalabria"
data_or$city[which(data_or$city=="reggioemilia")]<-"reggionell'emilia"
data_or$city[which(data_or$city=="trento(tn)")]<-"trento"
data_or$city[which(data_or$city=="bolzano")]<-"bolzanobozen"
data_or$city[which(data_or$city=="chietipescara")]<-"pescara"
data_or$city[which(data_or$city=="civitavecchia")]<-"roma"
data_or$city[which(data_or$city=="gemonadelfriuli,udine")]<-"udine"
data_or$city[which(data_or$city=="urbino")]<-"pesaroeurbino"
data_or$region[which(data_or$region=="pesarourbino")]<-"pesaroeurbino"
data_or$city[which(data_or$city=="lâ???Taquila")]<-"l'aquila"
data_or$city[which(data_or$region=="bologna")]<-"bologna"
data_or$city[which(data_or$city=="bologne")]<-"bologna"
data_or$region[which(data_or$region=="valled'aostapiemonte")]<-"valled'aosta"
data_or$region[which(data_or$region=="ve")]<-"venezia"
data_or$region[which(data_or$region=="tn")]<-"trento"
data_or$region[which(data_or$region=="bg")]<-"bergamo"
data_or$region[which(data_or$region=="bo")]<-"bologna"
data_or$region[which(data_or$region=="to")]<-"torino"
data_or$city[which(data_or$city=="rozzano(mi)")]<-"milano"
data_or$city[which(data_or$city=="pievedicadore")]<-"belluno"
data_or$city[which(data_or$city=="sestofiorentino")]<-"firenze"
data_or$region[which(data_or$region=="frfrosinone")]<-"frosinone"
data_or$city[which(data_or$city=="ispra")]<-"varese"
data_or$city[which(data_or$city=="monza")]<-"milano"
data_or$city[which(data_or$city=="rozzano")]<-"milano"
data_or$city[which(data_or$city=="sandonatomilanese")]<-"milano"
data_or$city[which(data_or$city=="grugliasco")]<-"torino"
data_or$city[which(data_or$city=="fisciano")]<-"salerno"
data_or$city[which(data_or$city=="fisciano(salerno)")]<-"salerno"
data_or$city[which(data_or$city=="capua")]<-"caserta"
data_or$city[which(data_or$city=="esine")]<-"brescia"
data_or$city[which(data_or$city=="vasto")]<-"chieti"
data_or$city[which(data_or$city=="sandonatomil.se")]<-"milano"
data_or$city[which(data_or$city=="sandonatomilanese(mi)")]<-"milano"
data_or$city[which(data_or$city=="sestofiorentino(fi)")]<-"firenze"
data_or$city[which(data_or$city=="nerviano")]<-"milano"
data_or$city[which(data_or$city=="linate")]<-"milano"
data_or$city[which(data_or$city=="agratebrianza")]<-"milano"
data_or$city[which(data_or$city=="arcavacatadirende(cs)")]<-"cosenza"
data_or$city[which(data_or$city=="arcavacatadirende")]<-"cosenza"
data_or$city[which(data_or$city=="arcavacatadirende(cosenza)")]<-"cosenza"
data_or$city[which(data_or$city=="arcavacatadirende,cs")]<-"cosenza"
data_or$city[which(data_or$city=="assergi")]<-"l'aquila"
data_or$city[which(data_or$city=="aversa")]<-"caserta"
data_or$city[which(data_or$city=="aversa(ce)")]<-"caserta"
data_or$city[which(data_or$city=="aviano")]<-"pordenone"
data_or$city[which(data_or$city=="bari(ba)")]<-"bari"
data_or$city[which(data_or$city=="barletta")]<-"barlettaandriatrani"
data_or$city[which(data_or$city=="baronissi")]<-"salerno"
data_or$city[which(data_or$city=="baronissi(sa)")]<-"salerno"
data_or$city[which(data_or$city=="bolonia")]<-"bologna"
data_or$city[which(data_or$city=="bolzano/bozen")]<-"bolzanobozen"
data_or$city[which(data_or$city=="bozen")]<-"bolzanobozen"
data_or$city[which(data_or$city=="bozen/bolzano")]<-"bolzanobozen"
data_or$city[which(data_or$city=="bozenbolzano")]<-"bolzanobozen"
data_or$city[which(data_or$city=="bustoarsizio")]<-"varese"
data_or$city[which(data_or$city=="bustoarsizio(va)")]<-"varese"
data_or$city[which(data_or$city=="camerino")]<-"macerata"
data_or$city[which(data_or$city=="candiolo")]<-"torino"
data_or$city[which(data_or$city=="cassino")]<-"frosinone"
data_or$city[which(data_or$city=="cesena")]<-"forli"
data_or$city[which(data_or$city=="faenza")]<-"ravenna"
data_or$city[which(data_or$city=="frascati")]<-"roma"
data_or$city[which(data_or$city=="lâ???Taquila")]<-"l'aquila"
data_or$city[which(data_or$city=="lagnaro")]<-"padova"
data_or$city[which(data_or$city=="monserrato")]<-"cagliari"
data_or$city[which(data_or$city=="monterotondo")]<-"roma"
data_or$city[which(data_or$city=="monterotondo(rm)")]<-"roma"
data_or$city[which(data_or$city=="monterotondo(roma)")]<-"roma"
data_or$city[which(data_or$city=="novedrate")]<-"como"
data_or$city[which(data_or$city=="orbassano")]<-"torino"
data_or$city[which(data_or$city=="rende(cs)")]<-"cosenza"
data_or$city[which(data_or$city=="rendecosenza")]<-"cosenza"
data_or$city[which(data_or$city=="rendecs")]<-"cosenza"
data_or$city[which(data_or$city=="rovereto")]<-"trento"
data_or$city[which(data_or$city=="sanmicheleall'adige")]<-"trento"


cities1 <- levels(as.factor(data_pc$admin.name2[which(data_pc$country.code=="IT")]))
cities2 <- levels(as.factor(data_pc$admin.name3[which(data_pc$country.code=="IT")]))
regions <- levels(as.factor(data_pc$admin.name1[which(data_pc$country.code=="IT")]))
regions[which(regions=="abruzzi")] <- "abruzzo"


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
#levels(as.factor(manuals[,1]))
#levels(as.factor(manuals[,2]))


#correzioni
data_or$NUTS3[which(data_or$city=="lâ???Taquila")] <- "ITF11"
data_or$NUTS3[which(data_or$city=="rende")] <- "ITF61"
data_or$NUTS3[which(data_or$city=="santamariadigaleria")] <- "ITI43"
data_or$NUTS3[which(data_or$city=="legnaro")] <- "ITH36"
data_or$NUTS3[which(data_or$city=="portici")] <- "ITF33"

data$NUTS3[indices] <- data_or$NUTS3[indices]
save(data, file="C:/Users/user/Desktop/forORCID/ORCID_2021_activities_extract_uptoIT.RData")
