library(Amelia)
library(readr)
library(readxl)

### IMPORT DATA ###
#TED_2009_2020 <- ...

data<-TED_2009_2020
data$TED<-log(data$TED) #imputation on log to reduce variance

set.seed(666)
data_im <- amelia(data,ts="year", cs="nuts2",polytime=1,intercs=T, bound=rbind(c(3,0,Inf)))

new<-(data_im$imputations[[1]]$TED+data_im$imputations[[2]]$TED+data_im$imputations[[3]]$TED+data_im$imputations[[4]]$TED+data_im$imputations[[5]]$TED)/5
data$ted_im<-new
write.csv(data,"ted_imputed_2009-2020.csv",row.names = F)

