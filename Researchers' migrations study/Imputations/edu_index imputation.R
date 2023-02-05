library(readxl)
library(Amelia)
library(readr)

### IMPORT DATA ###
#edu_index <- 

data<-edu_index
set.seed(666)
data_im <- amelia(data,ts="year", cs="nuts2",polytime=1,intercs=T, bound=rbind(c(3,0,1)))

new<-(data_im$imputations[[1]]$edu_index+data_im$imputations[[2]]$edu_index+data_im$imputations[[3]]$edu_index+data_im$imputations[[4]]$edu_index+data_im$imputations[[5]]$edu_index)/5
data$edu_index_im<-new
write.csv(data,"edu_index 2009-2020 imputed.csv",row.names = F)

