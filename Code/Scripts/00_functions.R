source("Scripts/00_functions_viz.R")


lu = function(x){
  length(unique(x))
}


sigmoid = function(x, ix, s){ 1/(1+exp(-(x-ix)*s))}



replace_NAs_with_latest_value = function(x) {   # repeats the last non NA value. Keeps leading NA
  ind = which(!is.na(x))      # get positions of nonmissing values
  if(is.na(x[1]))             # if it begins with a missing, add the 
    ind = c(1,ind)            # first position to the indices
  rep(x[ind], times = diff(   # repeat the values at these indices
    c(ind, length(x) + 1) ))  # diffing the indices + length yields how often 
}                               # they need to be repeated
# copied from: https://stackoverflow.com/questions/7735647/replacing-nas-with-latest-non-na-value




extract_obs = function(d, obs_names, finish_before_new_cycle = FALSE){
  
  obs = data.frame(date = seq(min(d$date), max(d$date), by = 1))
  
  for(obs_name in obs_names){
    sub_d = d[which(d$variable == obs_name),]
    k = match(obs$date, sub_d$date)
    eval(parse(text = paste0("obs$",obs_name," = sub_d$value[k]")))
  }
  
  j = min(which(obs$first_day == 1))
  if(j %in% 1:nrow(obs)){
    obs = obs[j:nrow(obs),]
  }else{
    obs$first_day[1] = 1
  }
  
  
  obs$bleeding[is.na(obs$bleeding)] = 0
  #obs$temp[is.na(obs$temp)] = 0
  #obs$mucus[is.na(obs$mucus)] = 0
  obs$LH[is.na(obs$LH)] = 0 
  obs$preg_test[is.na(obs$preg_test)] = 0 
  obs$anylog[is.na(obs$anylog)] = 0
  obs$gap = replace_NAs_with_latest_value(obs$gap)
  obs$first_day[is.na(obs$first_day)] = 0

  if(finish_before_new_cycle){
    j = max(which(obs$first_day == 1))-1
    if(j %in% 1:nrow(obs)){obs = obs[1:j,]}else{obs = data.frame()}
  }
  
  return(obs)
}
