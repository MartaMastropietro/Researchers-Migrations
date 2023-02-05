
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

### DATA IMPORT ###
#data_trips_gdp_score_ted_edu <- ...


data <- data_trips_gdp_score_ted_edu


# 
data$year <- as.factor(data$year)
data$country <- gsub(".{2}$","", data$nuts2)
data$country <- as.factor(data$country)
data$ted <- as.numeric(data$ted)
data$flow_tot<-data$flow_in+data$flow_out

data$uni_score <- (data$score)^(1/3)
data$log_flow <- log(data$flow_tot +1)

#complete model log flow out ~ log gdp, r3score, ted, edu index, re country, re year
mod_no_smooth<-gam(log_flow~ s(log(gdp),bs='cr')+s(uni_score,bs='cr')+s(log(ted),bs='cr')+s(edu_index, bs='cr')+s(country,bs='re')+s(year,bs='re'),data=data)
my_summary(mod_no_smooth,data) # 0.6882028     0.4654455 12237.9477005 
print(plot(getViz(mod_no_smooth), allTerms = T), pages = 1)

#complete smooth
mod<-gam(log_flow~ s(log(gdp),bs='cr')+s(uni_score,bs='cr')+s(log(ted),bs='cr',k=6)+s(edu_index, bs='cr',k=5)+s(country,bs='re')+s(year,bs='re'),data=data)
my_summary(mod,data) # 0.6866968     0.4639284 12244.1799982
print(plot(getViz(mod), allTerms = T), pages = 1)

#single
mod_ted<-gam(log_flow~ s(log(ted),bs='cr',k=6)+s(country,bs='re')+s(year,bs='re'),data=data)
my_summary(mod_ted,data) # 0.5824601     0.3309874 13019.4668975 

mod_score<-gam(log_flow~ s(uni_score,bs='cr')+s(country,bs='re')+s(year,bs='re'),data=data)
my_summary(mod_score,data)# 0.6251557     0.3838031 12725.9722979

mod_gdp<-gam(log_flow~ s(log(gdp),bs='cr')+s(country,bs='re')+s(year,bs='re'),data=data)
my_summary(mod_gdp,data) # 0.5311595     0.2731234 13311.6639108 

mod_edu<-gam(log_flow~ s(edu_index, bs='cr')+s(country,bs='re')+s(year,bs='re'),data=data)
my_summary(mod_edu,data)#  0.5811487     0.3294140 13027.3197557

a_t<-anova_np(mod,mod_ted,data) #0
a_s<-anova_np(mod,mod_score,data) #0.02
a_e<-anova_np(mod,mod_edu,data) #0
a_g<-anova_np(mod,mod_gdp,data) #0.006-0.016

#all reject , score is higer 

#linear gdp, no edu
mod0<-gam(log_flow~ log(gdp)+s(uni_score,bs='cr')+s(ted,bs='cr', k=6)+s(country,bs='re')+s(year,bs='re'),data=data)
my_summary(mod0,data) #0.6641424     0.4335989 12435.1297389 
print(plot(getViz(mod0), allTerms = T), pages = 1)

a0<-anova_np(mod,mod0,data) #no reject 

#score ted
mod1<-gam(log_flow~ s(uni_score,bs='cr')+s(ted,bs='cr', k=6)+s(country,bs='re')+s(year,bs='re'),data=data)
my_summary(mod1,data) # 0.6579194     0.4253294 12485.8934330
print(plot(getViz(mod1), allTerms = T), pages = 1)

a1<-anova_np(mod,mod1,data) # 0.023 not enough

#score edu 
mod2<-gam(log_flow~ s(uni_score,bs='cr')+s(edu_index,bs='cr',k=5)+s(country,bs='re')+s(year,bs='re'),data=data)
my_summary(mod2,data) # 0.6675732     0.4384838 12402.5820711 
print(plot(getViz(mod2), allTerms = T), pages = 1)

a2<-anova_np(mod,mod2,data) # 0.10 we may keep this as reduced 

#score gdp
mod3<-gam(log_flow~ s(uni_score,bs='cr')+log(gdp)+s(country,bs='re')+s(year,bs='re'),data=data)
my_summary(mod3,data) #0.6390716     0.4014083 12624.6385545 
print(plot(getViz(mod3), allTerms = T), pages = 1)

a3<-anova_np(mod,mod3,data) # 0.03 not enough 

#score ted edu
mod4<-gam(log_flow~ s(uni_score,bs='cr')+s(ted,bs='cr', k=6)+s(edu_index,bs='cr',k=5)+s(country,bs='re')+s(year,bs='re'),data=data)
my_summary(mod4,data) # 0.6857453     0.4627848 12250.5888981 
print(plot(getViz(mod4), allTerms = T), pages = 1)

a4<-anova_np(mod,mod4,data) # no reject, enough

#final model score+edu or score+edu+ted