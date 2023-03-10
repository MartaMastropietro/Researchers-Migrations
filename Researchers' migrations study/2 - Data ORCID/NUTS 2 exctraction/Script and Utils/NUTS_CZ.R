library(progress)
country <- "CZ"
colnames(pc2020_CZ_NUTS.2021_v2.0)<-c("NUTS3", "PCode")
pc2020_CZ_NUTS.2021_v2.0 <- pc2020_CZ_NUTS.2021_v2.0[-1,]

data_or <- data
data_pc <- geonames.postal.codes_filtered
data_nuts <- pc2020_CZ_NUTS.2021_v2.0

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


data_or$city[which(data_or$city=="praha")]<-"prague"
data_or$city[which(data_or$city=="prag")]<-"prague"
data_or$city[which(data_or$city=="prace")]<-"prague"
data_or$city[which(data_or$city=="pragha")]<-"prague"
data_or$city[which(data_or$city=="prague,")]<-"prague"
data_or$city[which(data_or$city=="prauge")]<-"prague"
data_or$city[which(data_or$city=="prgaue")]<-"prague"
data_or$city[which(data_or$city=="praha/prag")]<-"prague"
data_or$city[which(data_or$city=="prague,ceskebudejovice")]<-"prague"
data_or$city[which(data_or$city=="prague,czechrepublic")]<-"prague"
data_or$city[which(data_or$city=="prague,praha")]<-"prague"
data_or$city[which(data_or$city=="prague,prague4")]<-"prague"
data_or$city[which(data_or$city=="praga")]<-"prague"
data_or$city[which(data_or$city=="praque")]<-"prague"
data_or$city[which(data_or$city=="prague1")]<-"prague"
data_or$city[which(data_or$city=="prague2")]<-"prague"
data_or$city[which(data_or$city=="prague3")]<-"prague"
data_or$city[which(data_or$city=="prague4")]<-"prague"
data_or$city[which(data_or$city=="prague4krc")]<-"prague"
data_or$city[which(data_or$city=="praha4kr??")]<-"prague"
data_or$city[which(data_or$city=="prague5")]<-"prague"
data_or$city[which(data_or$city=="prague6")]<-"prague"
data_or$city[which(data_or$city=="prague10")]<-"prague"
data_or$city[which(data_or$city=="prague8")]<-"prague"
data_or$city[which(data_or$city=="prague9")]<-"prague"
data_or$city[which(data_or$city=="praha1")]<-"prague"
data_or$city[which(data_or$city=="praha2")]<-"prague"
data_or$city[which(data_or$city=="praha3")]<-"prague"
data_or$city[which(data_or$city=="praha4")]<-"prague"
data_or$city[which(data_or$city=="praha5")]<-"prague"
data_or$city[which(data_or$city=="praha6")]<-"prague"
data_or$city[which(data_or$city=="praha8")]<-"prague"
data_or$city[which(data_or$city=="praha9")]<-"prague"
data_or$city[which(data_or$city=="praha10")]<-"prague"
data_or$city[which(data_or$city=="pargue")]<-"prague"
data_or$city[which(data_or$city=="praque")]<-"prague"
data_or$city[which(data_or$city=="pragues")]<-"prague"
data_or$city[which(data_or$city=="pprague")]<-"prague"
data_or$city[which(data_or$city=="praguesuchdol")]<-"prague"
data_or$city[which(data_or$city=="prahasuchdol")]<-"prague"
data_or$city[which(data_or$city=="praha6suchdol")]<-"prague"
data_or$city[which(data_or$city=="praha(prague)")]<-"prague"
data_or$city[which(data_or$city=="prague(praha)")]<-"prague"
data_or$city[which(data_or$city=="praha/prague")]<-"prague"
data_or$city[which(data_or$city=="prahastod??lky")]<-"prague"
data_or$city[which(data_or$city=="plzen")]<-"pilsen"
data_or$city[which(data_or$city=="plzen3")]<-"pilsen"
data_or$city[which(data_or$city=="plze?^")]<-"pilsen"
data_or$city[which(data_or$city=="plze?^")]<-"pilsen"
data_or$city[which(data_or$city=="pislen")]<-"pilsen"
data_or$city[which(data_or$city=="holovousy")]<-"pilsen"
data_or$city[which(data_or$city=="facultyodmedicineinpilsen")]<-"pilsen"
data_or$city[which(data_or$city=="kolin")]<-"kol??n"
data_or$city[which(data_or$city=="ostravaporuba")]<-"ostrava"
data_or$city[which(data_or$city=="?oesk??bud?>jovice")]<-"ceskebudejovice"
data_or$city[which(data_or$city=="?oesk??bud?>jovice")]<-"ceskebudejovice"
data_or$city[which(data_or$city=="ceskebudjevoice")]<-"ceskebudejovice"
data_or$city[which(data_or$city=="ceskebudjovice")]<-"ceskebudejovice"
data_or$city[which(data_or$city=="budweis")]<-"ceskebudejovice"
data_or$city[which(data_or$city=="cesk??budejovice")]<-"ceskebudejovice"
data_or$city[which(data_or$city=="ceskebud?>jovice")]<-"ceskebudejovice"
data_or$city[which(data_or$city=="novehrady")]<-"ceskebudejovice"
data_or$city[which(data_or$city=="nov??hrady")]<-"ceskebudejovice"
data_or$city[which(data_or$city=="vod?^any")]<-"ceskebudejovice"
data_or$city[which(data_or$city=="hradechralove")]<-"hradeckr??lov??"
data_or$city[which(data_or$city=="hradeckr??love")]<-"hradeckr??lov??"
data_or$city[which(data_or$city=="hradeckralove")]<-"hradeckr??lov??"
data_or$city[which(data_or$city=="hradeckr??lov????")]<-"hradeckr??lov??"
data_or$city[which(data_or$city=="hradeckr??lov??,czechrepublic")]<-"hradeckr??lov??"
data_or$city[which(data_or$city=="headeckralove")]<-"hradeckr??lov??"
data_or$city[which(data_or$city=="vestec")]<-"prahaz??pad"
data_or$city[which(data_or$city=="vestec(prague)")]<-"prahaz??pad"
data_or$city[which(data_or$city=="vestec,pragueregion")]<-"prahaz??pad"
data_or$city[which(data_or$city=="vestec,praguewest")]<-"prahaz??pad"
data_or$city[which(data_or$city=="vestecatprague,prumyslova595,25250")]<-"prahaz??pad"
data_or$city[which(data_or$city=="vestecbyprague")]<-"prahaz??pad"
data_or$city[which(data_or$city=="vestecnearprague")]<-"prahaz??pad"
data_or$city[which(data_or$city=="ustinadlabem")]<-"karlovarsk??kraj"
data_or$city[which(data_or$city=="litv??nov")]<-"karlovarsk??kraj"
data_or$city[which(data_or$city=="trebon")]<-"jind?Tich??vhradec"
data_or$city[which(data_or$city=="t?Tebo?^")]<-"jind?Tich??vhradec"
data_or$city[which(data_or$city=="ondrejov")]<-"prahaz??pad"
data_or$city[which(data_or$city=="ond?Tejov")]<-"prahaz??pad"
data_or$city[which(data_or$city=="ond?Tejov")]<-"prahaz??pad"
data_or$city[which(data_or$city=="pr??honice")]<-"prahaz??pad"
data_or$city[which(data_or$city=="dolnibrezany")]<-"prahaz??pad"
data_or$city[which(data_or$city=="doln??b?Te??any")]<-"prahaz??pad"
data_or$city[which(data_or$city=="doln??b?Te??any")]<-"prahaz??pad"
data_or$city[which(data_or$city=="pruhonice")]<-"prahaz??pad"
data_or$city[which(data_or$city=="husinec")]<-"prachatice"
data_or$city[which(data_or$city=="husinec?~e??")]<-"prachatice"
data_or$city[which(data_or$city=="novyjicin")]<-"moravskoslezsk??kraj"
data_or$city[which(data_or$city=="nov??m?>stonadmetuj??")]<-"n??chod"
data_or$city[which(data_or$city=="rez")]<-"klecany"
data_or$city[which(data_or$city=="reznearprague")]<-"klecany"
data_or$city[which(data_or$city=="husinecrez")]<-"klecany"
data_or$city[which(data_or$city=="husinec?~ez")]<-"klecany"
data_or$city[which(data_or$city=="husinec?????~e??")]<-"klecany"
data_or$city[which(data_or$city=="husinecre??")]<-"klecany"
data_or$city[which(data_or$city=="husinecrezbyprague")]<-"klecany"
data_or$city[which(data_or$city=="celakovice")]<-"klecany"
data_or$city[which(data_or$city=="zdiby")]<-"klecany"
data_or$city[which(data_or$city=="?~e??")]<-"klecany"
data_or$city[which(data_or$city=="?~e??")]<-"klecany"
data_or$city[which(data_or$city=="vodnany")]<-"strakonice"
data_or$city[which(data_or$city=="brno,czechrepublic")]<-"brno"
data_or$city[which(data_or$city=="brno,masarykuniversity")]<-"brno"
data_or$city[which(data_or$city=="troubsko")]<-"brno"
data_or$city[which(data_or$city=="62500brno")]<-"brno"
data_or$city[which(data_or$city=="rosiceonlabe")]<-"brno"
data_or$city[which(data_or$city=="libechov")]<-"m?>ln??k"
data_or$city[which(data_or$city=="lib?>chov")]<-"m?>ln??k"
data_or$city[which(data_or$city=="t?Tinec")]<-"fr??dekm??stek"
data_or$city[which(data_or$city=="lednice")]<-"b?Teclav"
data_or$city[which(data_or$city=="b?Teclav")]<-"b?Teclav"
data_or$city[which(data_or$city=="zl??n")]<-"zlin"	
data_or$city[which(data_or$city=="ostravav??tkovice")]<-"ostrava"
data_or$city[which(data_or$city=="mladaboleslav")]<-"mlad??boleslav"
data_or$city[which(data_or$city=="mlad??bolelsav")]<-"mlad??boleslav"
data_or$city[which(data_or$city=="karvin??")]<-"karvina"
data_or$city[which(data_or$city=="havirov")]<-"karvina"
data_or$city[which(data_or$city=="hav???Tov")]<-"karvina"
data_or$city[which(data_or$city=="schoolofbusinessadministrationinkarvina")]<-"karvina"
data_or$city[which(data_or$city=="hav???Tov")]<-"karvina"
data_or$city[which(data_or$city=="dobrany")]<-"plze?^jih"
data_or$city[which(data_or$city=="dobra")]<-"plze?^jih"
data_or$city[which(data_or$city=="dob?Tany")]<-"plze?^jih"
data_or$city[which(data_or$city=="uherskehradiste")]<-"uhersk??hradi??t?"
data_or$city[which(data_or$city=="kunovice")]<-"uhersk??hradi??t?"
data_or$city[which(data_or$city=="doln??dobrou??")]<-"pardubice"
data_or$city[which(data_or$city=="dolnidobrouc")]<-"pardubice"
data_or$city[which(data_or$city=="rybitv??")]<-"pardubice"
data_or$city[which(data_or$city=="str????podralskem")]<-"liberec"
data_or$city[which(data_or$city=="turnov")]<-"liberec"
data_or$city[which(data_or$city=="liberc")]<-"liberec"
data_or$city[which(data_or$city=="liberec2")]<-"liberec"
data_or$city[which(data_or$city=="liberec10")]<-"liberec"
data_or$city[which(data_or$city=="mohelnice")]<-"olomouc"
data_or$city[which(data_or$city=="vyskov")]<-"vy??kov"
data_or$city[which(data_or$city=="trebic")]<-"jihlava"
data_or$city[which(data_or$city=="dukovany")]<-"jihlava"
data_or$city[which(data_or$city=="pod?>brady")]<-"nymburk"
data_or$city[which(data_or$city=="kraluvdvur")]<-"beroun"
data_or$city[which(data_or$city=="bustehrad")]<-"kladno"
data_or$city[which(data_or$city=="uhersk??hradi??t?")]<-"zlin"

data_or$region[which(data_or$region=="praha")]<-"prague"

data_pc$admin.name1[which(data_pc$admin.name1 == "hlavn??m?>stopraha")] <- "prague"

data_pc$admin.name2[which(data_pc$admin.name2=="karvin??")]<-"karvina"
data_pc$admin.name2[which(data_pc$admin.name2=="prahav??chod")]<-"klecany"
data_pc$admin.name2[which(data_pc$admin.name2=="brnovenkov")]<-"brno"
data_pc$admin.name2[which(data_pc$admin.name2=="?oesk??bud?>jovice")]<-"ceskebudejovice"
data_pc$admin.name2[which(data_pc$admin.name2=="plze?^m?>sto")]<-"pilsen"
data_pc$admin.name2[which(data_pc$admin.name2=="ostravam?>sto")]<-"ostrava"
data_pc$admin.name2[which(data_pc$admin.name2=="zl??n")]<-"zlin"


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
save(data, file="C:/Users/user/Desktop/forORCID/ORCID_2021_activities_extract_uptoCZ.RData")
