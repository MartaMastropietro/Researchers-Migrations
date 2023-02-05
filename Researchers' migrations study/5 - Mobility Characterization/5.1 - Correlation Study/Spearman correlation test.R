
library(readr)
library(readxl)

library(fda)
library(roahd)


### DATA IMPORT ###
# data_trips_gdp_score_ted_edu <- ...
data2<-data_trips_gdp_score_ted_edu


data_in<-data.frame()
for (c in unique(data2$nuts2)){
  flow_vec<-data2$flow_in[which(data2$nuts2==c)]
  year_vec<-data2$year[which(data2$nuts2==c)]
  flow_vec<-flow_vec[order(year_vec,decreasing = F)]
  newrow<-c(c)
  newrow<-append(newrow,flow_vec)
  data_in=rbind(data_in,newrow)
}
colnames(data_in)<-c("nuts2","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020")
data_in$`2009`<-as.numeric(data_in$`2009`)
data_in$`2010`<-as.numeric(data_in$`2010`)
data_in$`2011`<-as.numeric(data_in$`2011`)
data_in$`2012`<-as.numeric(data_in$`2012`)
data_in$`2013`<-as.numeric(data_in$`2013`)
data_in$`2014`<-as.numeric(data_in$`2014`)
data_in$`2015`<-as.numeric(data_in$`2015`)
data_in$`2016`<-as.numeric(data_in$`2016`)
data_in$`2017`<-as.numeric(data_in$`2017`)
data_in$`2018`<-as.numeric(data_in$`2018`)
data_in$`2019`<-as.numeric(data_in$`2019`)
data_in$`2020`<-as.numeric(data_in$`2020`)

data_out<-data.frame()
for (c in unique(data2$nuts2)){
  flow_vec<-data2$flow_out[which(data2$nuts2==c)]
  year_vec<-data2$year[which(data2$nuts2==c)]
  flow_vec<-flow_vec[order(year_vec,decreasing = F)]
  newrow<-c(c)
  newrow<-append(newrow,flow_vec)
  data_out=rbind(data_out,newrow)
}
colnames(data_out)<-c("nuts2","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020")
data_out$`2009`<-as.numeric(data_out$`2009`)
data_out$`2010`<-as.numeric(data_out$`2010`)
data_out$`2011`<-as.numeric(data_out$`2011`)
data_out$`2012`<-as.numeric(data_out$`2012`)
data_out$`2013`<-as.numeric(data_out$`2013`)
data_out$`2014`<-as.numeric(data_out$`2014`)
data_out$`2015`<-as.numeric(data_out$`2015`)
data_out$`2016`<-as.numeric(data_out$`2016`)
data_out$`2017`<-as.numeric(data_out$`2017`)
data_out$`2018`<-as.numeric(data_out$`2018`)
data_out$`2019`<-as.numeric(data_out$`2019`)
data_out$`2020`<-as.numeric(data_out$`2020`)


set.seed(666)

inflow <- data_in
outflow <- data_out

full_tab <- data2

n <- dim(inflow)[1]



# time of analysis
tt <- 2009:2020


# plot data by nuts2
x11()
par(mfrow=c(1,2))
matplot(tt, t(inflow[,-1]), type = 'l', main='Inflow', xlab='Year', ylab='Number of people')
matplot(tt, t(outflow[,-1]), type = 'l', main='Outflow', xlab='Year', ylab='Number of people')

x11()
par(mfrow=c(1,2))
matplot(tt, t(inflow[,-1]), type = 'l', xlab='year', ylab='in_flows')
matplot(tt, t(outflow[,-1]), type = 'l', xlab='year', ylab='out_flows')



# smooth the curves
basis <- create.bspline.basis(rangeval = range(tt), nbasis = 20)     #default order is 4 (degree = 3), cubic spline
inflow.fd <- Data2fd(y=as.matrix(t(inflow[,-1])), argvals = tt, basisobj = basis)
outflow.fd <- Data2fd(y=as.matrix(t(outflow[,-1])), argvals = tt, basisobj = basis)

x11()
par(mfrow=c(1,2))
plot(inflow.fd,xlab='year', ylab='in_flows')
plot.fd(outflow.fd, xlab='year', ylab='out_flows')


# create functional data objects
tt.grid <- seq(2009, 2020, length.out = 1000)
inflow.eval <- t(eval.fd(tt.grid, inflow.fd))     # I transpose because in fData I need data by row
outflow.eval <- t(eval.fd(tt.grid, outflow.fd))   # I transpose because in fData I need data by row


fData.in <- fData(tt.grid, inflow.eval)
fData.out <- fData(tt.grid, outflow.eval)


# build bivariate inflow-outflow dataset
bivariate_data <- as.mfData(list(fData.in, fData.out))

x11()
plot(bivariate_data)


# compute Spearman Correlation index (absolute value)
SPC0 <- abs(cor_spearman(bivariate_data, ordering='MEI'))
SPC0


# perform the permutational test
B <- 1000
SPC <- numeric(B)
for(i in 1:B){
  outflow.eval.shuffled <- outflow.eval[sample(n),]                   # shuffle outflow functions
  fData.in.curr <- fData(tt.grid, inflow.eval)
  fData.out.curr <- fData(tt.grid, outflow.eval.shuffled)
  bivariate_data <- as.mfData(list(fData.in.curr, fData.out.curr))
  SPC[i] <- abs(cor_spearman(bivariate_data, ordering='MEI'))
}


x11()
hist(SPC, breaks = 50, xlim = c(0,1), main='Spearman Correlation Index')
abline(v=SPC0, col='red', lwd=3)

pvalue <- sum(SPC>=SPC0)/B
pvalue #0, we reject hp that spearman index are the same 

