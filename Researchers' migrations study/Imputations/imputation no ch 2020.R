library(Amelia)
library(dplyr)
library(readxl)
library(readr)

### IMPORT DATA ###
#GDP_PC_NO_CH_to_impute <- ...

data_gdp<-data.frame(GDP_PC_NO_CH_to_impute)
set.seed(666)
data_im <- amelia(data_gdp, m=5,ts = "year", cs="geo",polytime=1, intercs = TRUE)

new<-(data_im$imputations[[1]]$gdp+data_im$imputations[[2]]$gdp+data_im$imputations[[3]]$gdp+data_im$imputations[[4]]$gdp+data_im$imputations[[5]]$gdp)/5
data_gdp$gdp_im<-new

write.csv(data_gdp,"GDP_per_capita_NO_CH_imputed.csv",row.names = F)
