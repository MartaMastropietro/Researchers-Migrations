library(mgcv)
library(readr)
library(mgcViz)
seed = 27011999
set.seed(seed)

### IMPORT DATA ###
#network_data <- ...
#language_mapper <- ...


data <- network_data

data$same_country<-as.factor(data$same_country)
data$year<-as.factor(data$year)

data$uniscore_sender <- (data$score_sender)^(1/3)
data$uniscore_receiver <- (data$score_receiver)^(1/3)

data$uniscore_sum <- data$uniscore_sender + data$uniscore_receiver
data$ted_sum <- data$ted_receiver + data$ted_sender


data$lcount <- log(data$count + 1)



anova_np_nuts2<-function(mod_big,mod_small,data,seed=999,B=1000){
  
  
  rsq_big<-summary(mod_big)$r.sq
  aic_big<-mod_big$aic
  
  rsq_small<-summary(mod_small)$r.sq
  aic_small<-mod_small$aic
  
  fit <- mod_small$fitted.values
  res <- mod_small$residuals
  
  s <- summary(mod_big)
  T_0 <- max(s$s.table[1,3], s$s.table[2,3], s$s.table[3,3], s$s.table[4,3])
  if(is.na(T_0)){
    warning("T_0 is NaN, test is stopped")
    return(list(p.val=NULL,T0=NULL,T.stat=NULL,rsq_small=rsq_small,rsq_big=rsq_big,aic_small=aic_small,aic_big=aic_big))
  }
  
  library(progress)
  pb <- progress_bar$new(total=B)
  pb$tick(0)
  
  T_stat <- c()
  data_perm<-data
  
  set.seed(seed)
  for(b in 1:B){
    res_perm <- sample(res)
    y_perm <- fit + res_perm
    
    data_perm[as.character(mod_small$formula)[2]] <- y_perm
    mod_perm <- gam( mod_big$formula, data=data_perm)
    s_p <- summary(mod_perm)
    
    T_stat <- append(T_stat, max(s_p$s.table[1,3], s_p$s.table[2,3], s_p$s.table[3,3], s_p$s.table[4,3]))
    pb$tick()
  }
  
  p_val <- sum(T_stat >= T_0)/B
  
  if(is.na(p_val)){
    T_stat_upper<-T_stat
    T_stat_upper[which(is.na(T_stat_upper))] <- T_0+1
    T_stat_lower<-T_stat
    T_stat_lower[which(is.na(T_stat_lower))] <- T_0-1
    p_val <- c(lower = sum(T_stat_lower >= T_0)/B, upper = sum(T_stat_upper >= T_0)/B)
    warning("NaN are produced, p-value bounds are provided instead")
  }
  
  ret<-list(p.val=p_val,T0=T_0,T.stat=T_stat,rsq_small=rsq_small,rsq_big=rsq_big,aic_small=aic_small,aic_big=aic_big)
  return(ret)
  
}

mod_b <- gam(lcount ~ s(uniscore_sender,bs="cr",k = 5) + s(ted_sender,bs="cr",k = 5) + s(uniscore_receiver,bs="cr",k = 5) + s(ted_receiver,bs="cr",k = 5) + s(uniscore_sum,bs="cr",k = 5) + s(ted_sum,bs="cr",k = 5) + s(log(dist),bs="cr") + same_country + s(year,bs="re"), data=data)
mod_s <- gam(lcount ~ s(uniscore_sum,bs="cr",k = 5) + s(ted_sum,bs="cr",k = 5) + s(log(dist),bs="cr") + same_country + s(year,bs="re"), data=data)

a<-anova_np_nuts2(mod_b,mod_s,data)
a$p.val#0.974