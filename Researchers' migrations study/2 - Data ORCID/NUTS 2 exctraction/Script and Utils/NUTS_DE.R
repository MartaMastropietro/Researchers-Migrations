library(progress)
country <- "DE"
colnames(pc2020_DE_NUTS.2021_v4.0)<-c("NUTS3", "PCode")
pc2020_DE_NUTS.2021_v4.0 <- pc2020_DE_NUTS.2021_v4.0[-1,]

data_or <- data
data_pc <- geonames.postal.codes_filtered
data_nuts <- pc2020_DE_NUTS.2021_v4.0

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


data_pc$admin.name3[which(data_pc$admin.name3=="berlin,stadt")]<-"berlin"
data_pc$admin.name3[which(data_pc$admin.name3=="bonn,stadt")]<-"bonn"
data_pc$admin.name3[which(data_pc$admin.name3=="hamburg,freieundhansestadt")]<-"hamburg"
data_pc$admin.name3[which(data_pc$admin.name3=="landkreisbarnim")]<-"brandenburg"
data_pc$admin.name3[which(data_pc$admin.name3=="zeuthen")]<-"brandenburg"
data_pc$admin.name3[which(data_pc$admin.name3=="m??ncheberg")]<-"brandenburg"
data_pc$admin.name3[which(data_pc$admin.name3=="jena,stadt")]<-"jena"
data_pc$admin.name3[which(data_pc$admin.name3=="chemnitz,stadt")]<-"chemnitz"
data_pc$admin.name3[which(data_pc$admin.name3=="solingen,stadt")]<-"solingen"
data_pc$admin.name3[which(data_pc$admin.name3=="chemnitz,stadt")]<-"chemnitz"
data_pc$admin.name3[which(data_pc$admin.name3=="dortmund,stadt")]<-"dortmund"
data_pc$admin.name3[which(data_pc$admin.name3=="darmstadt,wissenschaftsstadt")]<-"darmstadt"
data_pc$admin.name3[which(data_pc$admin.name3=="bremerhaven,stadt")]<-"bremerhaven"
data_pc$admin.name3[which(data_pc$admin.name3=="weimar,stadt")]<-"weimar"
data_pc$admin.name3[which(data_pc$admin.name3=="wuppertal,stadt")]<-"wuppertal"
data_pc$admin.name3[which(data_pc$admin.name3=="m??nster,stadt")]<-"m??nster"
data_pc$admin.name3[which(data_pc$admin.name3=="w??rzburg")]<-"wurzburg"
data_pc$admin.name3[which(data_pc$admin.name3=="trier,kreisfreiestadt")]<-"trier"
data_pc$admin.name3[which(data_pc$admin.name3=="kreisfreiestadtpotsdam")]<-"potsdam"
data_pc$admin.name3[which(data_pc$admin.name3=="d??sseldorf,stadt")]<-"dusseldorf"
data_pc$admin.name3[which(data_pc$admin.name3=="landkreism??nchen")]<-"munich"
data_pc$admin.name3[which(data_pc$admin.name3=="hamm,stadt")]<-"hamm"
data_pc$admin.name3[which(data_pc$admin.name3=="oldenburg(oldenburg),stadt")]<-"oldenburg"
data_pc$admin.name3[which(data_pc$admin.name3=="wiesbaden,landeshauptstadt")]<-"wiesbaden"
data_pc$admin.name3[which(data_pc$admin.name3=="duisburg,stadt")]<-"duisburg"
data_pc$admin.name3[which(data_pc$admin.name3=="wolfsburg,stadt")]<-"wolfsburg"
data_pc$admin.name3[which(data_pc$admin.name3=="emden,stadt")]<-"emden"
data_pc$admin.name3[which(data_pc$admin.name3=="essen,stadt")]<-"essen"
data_pc$admin.name3[which(data_pc$admin.name3=="erfurt,stadt")]<-"erfurt"
data_pc$admin.name3[which(data_pc$admin.name3=="osnabr??ck,stadt")]<-"osnabr??ck"
data_pc$admin.name3[which(data_pc$admin.name3=="worms,kreisfreiestadt")]<-"worms"
data_pc$admin.name3[which(data_pc$admin.name3=="k??ln,stadt")]<-"cologne"
data_pc$admin.name3[which(data_pc$admin.name3=="kreisfreiestadtbremen")]<-"bremen"
data_pc$admin.name3[which(data_pc$admin.name3=="landkreisg??ttingen")]<-"gottingen"
data_pc$admin.name3[which(data_pc$admin.name3=="d??ren")]<-"duren"
data_pc$admin.name3[which(data_pc$admin.name3=="kreisfreiestadtdresden")]<-"dresden"
data_pc$admin.name3[which(data_pc$admin.name3=="passau")]<-"landkreispassau"
data_pc$admin.name3[which(data_pc$admin.name3=="rostock")]<-"landkreisrostock"
data_pc$admin.name3[which(data_pc$admin.name3=="n??rnberg")]<-"nurnberg"
data_pc$admin.name3[which(data_pc$admin.name3=="kaiserslautern,kreisfreiestadt")]<-"kaiserslautern"


data_or$city[which(data_or$city=="berlin,stadt")]<-"berlin"
data_or$city[which(data_or$city=="bonn,stadt")]<-"bonn"
data_or$city[which(data_or$city=="aachen")]<-"bonn"
data_or$city[which(data_or$city=="w??rzburg")]<-"wurzburg"
data_or$city[which(data_or$city=="wuerzburg")]<-"wurzburg"
data_or$city[which(data_or$city=="m??nchen")]<-"munich"
data_or$city[which(data_or$city=="m??nchen,bayern")]<-"munich"
data_or$city[which(data_or$city=="munchen")]<-"munich"
data_or$city[which(data_or$city=="muenchen")]<-"munich"
data_or$city[which(data_or$city=="garching")]<-"munich"
data_or$city[which(data_or$city=="martinsried")]<-"munich"
data_or$city[which(data_or$city=="neubiberg")]<-"munich"
data_or$city[which(data_or$city=="garchingnearmunich")]<-"munich"
data_or$city[which(data_or$city=="garchingbeim??nchen")]<-"munich"
data_or$city[which(data_or$city=="garchingb.m??nchen")]<-"munich"
data_or$city[which(data_or$city=="garching,munich")]<-"munich"
data_or$city[which(data_or$city=="frankfurt")]<-"frankfurtammain,stadt"
data_or$city[which(data_or$city=="frankfurtammain")]<-"frankfurtammain,stadt"
data_or$city[which(data_or$city=="brandenburg")]<-"kreisfreiestadtbrandenburganderhavel"
data_or$city[which(data_or$city=="badnauheim")]<-"wetteraukreis"
data_or$city[which(data_or$city=="bielefeld")]<-"herford"
data_or$city[which(data_or$city=="kiel")]<-"kreisfreiestadtflensburg"
data_or$city[which(data_or$city=="badnauheim")]<-"darmstadt,wissenschaftsstadt"
data_or$city[which(data_or$city=="krefeld,stadt")]<-"krefeld"
data_or$city[which(data_or$city=="bochum")]<-"hamm"
data_or$city[which(data_or$city=="siegen")]<-"hamm"
data_or$city[which(data_or$city=="arnsberg")]<-"hamm"
data_or$city[which(data_or$city=="hagen")]<-"hamm"
data_or$city[which(data_or$city=="bremerhaven")]<-"bremer"
data_or$city[which(data_or$city=="kreisfreiestadtbremen")]<-"bremen"
data_or$city[which(data_or$city=="d??sseldorf")]<-"dusseldorf"
data_or$city[which(data_or$city=="duesseldorf")]<-"dusseldorf"
data_or$city[which(data_or$city=="mulheimanderruhr")]<-"dusseldorf"
data_or$city[which(data_or$city=="m??lheimanderruhr")]<-"dusseldorf"
data_or$city[which(data_or$city=="k??ln")]<-"cologne"
data_or$city[which(data_or$city=="koln")]<-"cologne"
data_or$city[which(data_or$city=="sanktaugustin")]<-"cologne"
data_or$city[which(data_or$city=="braunschweig")]<-"kreisfreiestadtbraunschweig"
data_or$city[which(data_or$city=="erlangen")]<-"kreisfreiestadterlangen"
data_or$city[which(data_or$city=="freiberg")]<-"landkreismittelsachsen"
data_or$city[which(data_or$city=="d??ren")]<-"duren"
data_or$city[which(data_or$city=="julich")]<-"duren"
data_or$city[which(data_or$city=="j??lich")]<-"duren"
data_or$city[which(data_or$city=="osnabruck")]<-"osnabr??ck"
data_or$city[which(data_or$city=="magdeburg")]<-"kreisfreiestadthalle"
data_or$city[which(data_or$city=="halle")]<-"kreisfreiestadthalle"
data_or$city[which(data_or$city=="halle(salle)")]<-"kreisfreiestadthalle"
data_or$city[which(data_or$city=="halle(saale)")]<-"kreisfreiestadthalle"
data_or$city[which(data_or$city=="hanover")]<-"landkreisschaumburg"
data_or$city[which(data_or$city=="homburg")]<-"neunkirchen"
data_or$city[which(data_or$city=="saarbr??cken")]<-"neunkirchen"
data_or$city[which(data_or$city=="ilmenau")]<-"ilmkreis"
data_or$city[which(data_or$city=="freiburg")]<-"freiburgimbreisgau"
data_or$city[which(data_or$city=="leipzig")]<-"kreisfreiestadtleipzig"
data_or$city[which(data_or$city=="stadtseeland")]<-"salzlandkreis"
data_or$city[which(data_or$city=="gatersleben")]<-"salzlandkreis"
data_or$city[which(data_or$city=="mainz")]<-"mainz,kreisfreiestadt"
data_or$city[which(data_or$city=="landau")]<-"mainz,kreisfreiestadt"
data_or$city[which(data_or$city=="marburg")]<-"marburgbiedenkopf"
data_or$city[which(data_or$city=="g??ttingen")]<-"gottingen"
data_or$city[which(data_or$city=="goettingen")]<-"gottingen"
data_or$city[which(data_or$city=="tubingen")]<-"t??bingen"
data_or$city[which(data_or$city=="tuebingen")]<-"t??bingen"
data_or$city[which(data_or$city=="giessen")]<-"gie??en"
data_or$city[which(data_or$city=="freising")]<-"landkreisfreising"
data_or$city[which(data_or$city=="hildesheim")]<-"landkreishildesheim"
data_or$city[which(data_or$city=="n??rnberg")]<-"nurnberg"
data_or$city[which(data_or$city=="nuremberg")]<-"nurnberg"
data_or$city[which(data_or$city=="neuherberg")]<-"nurnberg"
data_or$city[which(data_or$city=="munster")]<-"kreisfreiestadtneum??nster"
data_or$city[which(data_or$city=="muenster")]<-"kreisfreiestadtneum??nster"
data_or$city[which(data_or$city=="hannover")]<-"landkreisdiepholz"
data_or$city[which(data_or$city=="geesthacht")]<-"kreisherzogtumlauenburg"
data_or$city[which(data_or$city=="greifswald")]<-"vorpommerngreifswald"
data_or$city[which(data_or$city=="rostock")]<-"landkreisrostock"
data_or$city[which(data_or$city=="kreisfreiestadtbremen")]<-"bremen"
data_or$city[which(data_or$city=="l??beck")]<-"kreisfreiestadtflensburg"
data_or$city[which(data_or$city=="lubeck")]<-"kreisfreiestadtflensburg"
data_or$city[which(data_or$city=="luebeck")]<-"kreisfreiestadtflensburg"
data_or$city[which(data_or$city=="flensburg")]<-"kreisfreiestadtflensburg"
data_or$city[which(data_or$city=="passau")]<-"landkreispassau"
data_or$city[which(data_or$city=="eggensteinleopoldshafen")]<-"karlsruhe"
data_or$city[which(data_or$city=="eschborn")]<-"maintaunuskreis"
data_or$city[which(data_or$city=="koblenz")]<-"koblenz,kreisfreiestadt"
data_or$city[which(data_or$city=="ludwigshafen")]<-"ludwigshafenamrhein,kreisfreiestadt"
data_or$city[which(data_or$city=="ludwigshafenamrhein")]<-"ludwigshafenamrhein,kreisfreiestadt"
data_or$city[which(data_or$city=="zittau")]<-"dresden"
data_or$city[which(data_or$city=="witten")]<-"enneperuhrkreis"
data_or$city[which(data_or$city=="wessling")]<-"starnberg"
data_or$city[which(data_or$city=="oberpfaffenhofenwessling")]<-"starnberg"
data_or$city[which(data_or$city=="oberpfaffenhofen")]<-"starnberg"
data_or$city[which(data_or$city=="weingarten")]<-"landkreisravensburg"
data_or$city[which(data_or$city=="clausthalzellerfeld")]<-"landkreisgoslar"
data_or$city[which(data_or$city=="luneburg")]<-"kreisherzogtumlauenburg"
data_or$city[which(data_or$city=="l??neburg")]<-"kreisherzogtumlauenburg"
data_or$city[which(data_or$city=="hildesheim")]<-"landkreishildesheim"
data_or$city[which(data_or$city=="bremer")]<-"bremerhaven"
data_or$city[which(data_or$city=="weinheim")]<-"rheinneckarkreis"
data_or$city[which(data_or$city=="rheinbach")]<-"rheinneckarkreis"
data_or$city[which(data_or$city=="friedrichshafen")]<-"t??bingen"
data_or$city[which(data_or$city=="biberach")]<-"t??bingen"
data_or$city[which(data_or$city=="wilhelmshaven")]<-"wilhelmshaven,stadt"
data_or$city[which(data_or$city=="leverkusen")]<-"cologne"
data_or$city[which(data_or$city=="krefeld")]<-"dusseldorf"
data_or$city[which(data_or$city=="offenburg")]<-"ortenaukreis"
data_or$city[which(data_or$city=="eichstatt")]<-"eichst??tt"
data_or$city[which(data_or$city=="langen")]<-"offenbach"
data_or$city[which(data_or$city=="zeuthen")]<-"landkreisdahmespreewald"
data_or$city[which(data_or$city=="garmischpartenkirchen")]<-"landkreisgarmischpartenkirchen"
data_or$city[which(data_or$city=="witzenhausen")]<-"werramei??nerkreis"
data_or$city[which(data_or$city=="osnabrueck")]<-"osnabr??ck"
data_or$city[which(data_or$city=="m??lheim")]<-"dusseldorf"
data_or$city[which(data_or$city=="frankfurt/main")]<-"frankfurtammain,stadt"
data_or$city[which(data_or$city=="pl??n")]<-"kreispl??n"
data_or$city[which(data_or$city=="plon")]<-"kreispl??n"



data_or$region[which(data_or$region=="saarland")]<-"neunkirchen"

cities1 <- levels(as.factor(data_pc$admin.name2[which(data_pc$country.code==country)]))
cities2 <- levels(as.factor(data_pc$admin.name3[which(data_pc$country.code==country)]))

manuals <- data.frame(cbind(Region = c("Region"), City = c("City")))

pb <- progress_bar$new(total = length(indices))
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

#correct error in postal code conversion
data_or$NUTS3[which(data_or$NUTS3=="37956")] <- "DEC01"
data_or$NUTS3[which(data_or$NUTS3=="38322")] <- "DEC01"
data_or$NUTS3[which(data_or$NUTS3=="39052")] <- "DEC01"

data$NUTS3[indices] <- data_or$NUTS3[indices]
save(data, file="C:/Users/user/Desktop/forORCID/ORCID_2021_activities_extract_uptoDE.RData")
