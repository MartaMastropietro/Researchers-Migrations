
library(mgcv)
library(mgcViz)
library(readr)

my_summary<-function(mod,data){
  rsq<-as.numeric(cor(mod$fitted.values,data[as.character(mod$formula)[2]]))
  adj_rsq<-summary(mod)$r.sq
  aic<-AIC(mod)

  return(c(rsq=rsq,adj_rsq=adj_rsq,aic=aic))
}

options(scipen=999)


data_trips_gdp_score_ted_edu <- read_delim("D:/Marta/Politecnico/Tesi/data_trips_gdp_score_ted_edu.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
data <- data_trips_gdp_score_ted_edu
cor(data$flow_in,data$flow_out)#0.988

# log flow out ~ log gdp, uni_score, ted, edu index, re country, re year
data$year <- as.factor(data$year)
data$country <- gsub(".{2}$","", data$nuts2)
data$country <- as.factor(data$country)
data$ted <- as.numeric(data$ted)

data$uni_score <- (data$score)^(1/3)
data$log_flow <- log(data$flow_out +1)

#complete model
mod<-gam(log_flow~ s(log(gdp),bs='cr')+s(uni_score,bs='cr',k=6)+s(log(ted),bs='cr',k=6)+s(edu_index, bs='cr')+s(country,bs='re')+s(year,bs='re'),data=data)
my_summary(mod,data) # 0.6858946     0.4627296 12098.3246432 
print(plot(getViz(mod), allTerms = T), pages = 1)

#single
mod_ted<-gam(log_flow~ s(log(ted),bs='cr',k=6)+s(country,bs='re')+s(year,bs='re'),data=data)
my_summary(mod_ted,data) #0.5784072     0.3263270 12889.1577576 
mod_score<-gam(log_flow~ s(uni_score,bs='cr',k=6)+s(country,bs='re')+s(year,bs='re'),data=data)
my_summary(mod_score,data)# 0.6312915     0.3919310 12523.0407757
mod_gdp<-gam(log_flow~ s(log(gdp),bs='cr')+s(country,bs='re')+s(year,bs='re'),data=data)
my_summary(mod_gdp,data) # 0.5303086     0.2720129 13164.0857620
mod_edu<-gam(log_flow~ s(edu_index, bs='cr')+s(country,bs='re')+s(year,bs='re'),data=data)
my_summary(mod_edu,data)# 0.5785497     0.3263457 12889.3181786 

a_t<-anova_np(mod,mod_ted,data) #0
a_s<-anova_np(mod,mod_score,data) #0.049-0.059 limit, potentially score holds itself 
a_e<-anova_np(mod,mod_edu,data) #0
a_g<-anova_np(mod,mod_gdp,data) #0

#linear gdp, no edu
mod0<-gam(log_flow~ log(gdp)+s(uni_score,bs='cr',k=6)+s(ted,bs='cr', k=6)+s(country,bs='re')+s(year,bs='re'),data=data)
my_summary(mod0,data) # 0.6648828     0.4347296 12273.0806247 
print(plot(getViz(mod0), allTerms = T), pages = 1)

a0<-anova_np(mod,mod0,data) #no reject , fine to reduce 

#only ted score
mod1<-gam(log_flow~ s(uni_score,bs='cr',k=6)+s(ted,bs='cr', k=6)+s(country,bs='re')+s(year,bs='re'),data=data)
my_summary(mod1,data) # 0.6593925     0.4274895 12317.2489817 
print(plot(getViz(mod1), allTerms = T), pages = 1)

a1<-anova_np(mod,mod1,data) #no reject, ted score hold themselves 

#only score edu 
mod2<-gam(log_flow~ s(uni_score,bs='cr',k=6)+s(edu_index,bs='cr')+s(country,bs='re')+s(year,bs='re'),data=data)
my_summary(mod2,data) # 0.6707526     0.4425729 12223.9085078 
print(plot(getViz(mod2), allTerms = T), pages = 1)

a2<-anova_np(mod,mod2,data)  #no reject, edu score hold themselves 

#only score gdp
mod3<-gam(log_flow~ s(uni_score,bs='cr',k=6)+log(gdp)+s(country,bs='re')+s(year,bs='re'),data=data)
my_summary(mod3,data) # 0.6438512     0.4076602 12433.1744488 
print(plot(getViz(mod3), allTerms = T), pages = 1)

a3<-anova_np(mod,mod3,data) #0.04 limit 

#ted score edu
mod4<-gam(log_flow~ s(uni_score,bs='cr',k=6)+s(ted,bs='cr', k=6)+s(edu_index,bs='cr')+s(country,bs='re')+s(year,bs='re'),data=data)
my_summary(mod4,data) # 0.6853393     0.4621395 12101.0312712 
print(plot(getViz(mod4), allTerms = T), pages = 1)

a4<-anova_np(mod,mod4,data) # no reject 

