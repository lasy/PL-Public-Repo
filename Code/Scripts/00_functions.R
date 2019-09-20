source("Scripts/00_functions_viz.R")


lu = function(x){
  length(unique(x))
}


sigmoid = function(x, ix, s){ 1/(1+exp(-(x-ix)*s))}

my_cumsum = function(t = d$days, x = d$menstruation, lags = -7:-1){
  if(any(is.na(t))){stop("NAs in t are not allowed\n")}
  tt = seq(min(min(t),min(t)+min(lags)), max(max(t)+ max(lags), max(t)), 1)
  x[is.na(x)] = 0
  xx = 0*tt
  m = match(t, tt)
  xx[m] = x
  yy = rollapply(xx, length(lags), FUN = sum)
  y = yy[m+min(lags)]
  return(y)
}



replace_NAs_with_latest_value = function(x) {   # repeats the last non NA value. Keeps leading NA
  ind = which(!is.na(x))      # get positions of nonmissing values
  if(is.na(x[1]))             # if it begins with a missing, add the 
    ind = c(1,ind)            # first position to the indices
  rep(x[ind], times = diff(   # repeat the values at these indices
    c(ind, length(x) + 1) ))  # diffing the indices + length yields how often 
}                               # they need to be repeated
# copied from: https://stackoverflow.com/questions/7735647/replacing-nas-with-latest-non-na-value



interpolate = function(x, v_before, v_after){
  y = na_interpolation(c(v_before,v_before,as.numeric(x),v_after,v_after), option = "stine")
  y = y[3:(length(y)-2)]
  return(y)
}



find_cycle_starts = function(this_user_day = this_user_day, debug = FALSE){
  
  this_user_day$day = as.numeric(this_user_day$date - min(this_user_day$date))
  this_user_day$preg_test[is.na(this_user_day$preg_test)] = 0
  this_user_day$menstruation[is.na(this_user_day$menstruation)] = 0
  sum_bleeding_last_7_days = my_cumsum(t = this_user_day$day, x = this_user_day$menstruation, lags = -7:-1)
  sum_bleeding_2_days = my_cumsum(t = this_user_day$day, x = this_user_day$menstruation, lags = 0:1)
  
  
  day_last_bleeding =  -10
  day_last_cycle_start = -29
  median_cycle_length = 29
  is_pregnant = FALSE
  potential_pregnancy_end_type = "none"
  cycle_starts = c()
  pregnancies = c()
  for(i in 1:nrow(this_user_day)){ # nrow(this_user_day)
    today = this_user_day$day[i]
    if(debug){cat(today,"\n")}
    current_cycle_length = today  - day_last_cycle_start
    
    period_start = (this_user_day$menstruation[i] > 0) & 
      (sum_bleeding_2_days[i] > 1) & 
      (sum_bleeding_last_7_days[i] <= 1) &
      (current_cycle_length >= min(0.6*median_cycle_length,12))
    
    user_defined_cycle_start = this_user_day$first_day[i] & (current_cycle_length >= 3*7) & (sum_bleeding_last_7_days[i] <= 2)
    
    # users get pregnant
    if(!is_pregnant & (this_user_day$preg_test[i] == 1)){
      if(debug){cat("\t got pregnant\n")}
      is_pregnant = TRUE
      day_first_pos_preg_test = today
      if(current_cycle_length < min(23,(median_cycle_length-6))){ 
        # positive pregnancy test early in the cycle > probably got pregnant at previous cycle
        # we cancel the last cycle start
        if(length(cycle_starts)>0){
          cycle_starts = cycle_starts[-length(cycle_starts)]
          day_last_cycle_start = max(-29,cycle_starts[length(cycle_starts)])
        }
      }
    }
    
    # users not pregnant & period starts or user-defined new cycle
    if(!is_pregnant & (period_start | user_defined_cycle_start)){
      if(debug){cat("\t new cycle\n")}
      day_last_cycle_start = today
      cycle_starts = c(cycle_starts,day_last_cycle_start)
      pregnancies = c(pregnancies, 0)
    }
    
    # user is pregnant and all went well
    if(is_pregnant & (potential_pregnancy_end_type == "none")){
      
      time_since_first_positive_preg_test = today - day_first_pos_preg_test
      # pregnancy may end
      # either with a negative pregnancy test
      # or with a period
      # or with a user-defined cycle start
      potential_pregnancy_end_type = case_when(
        (this_user_day$first_day[i]) ~ "first day",
        (this_user_day$preg_test[i] == -1) ~ "neg preg test",
        period_start ~ "period",
        TRUE ~ potential_pregnancy_end_type
      ) 
      
      if(potential_pregnancy_end_type != "none"){
        day_at_potential_pregnancy_end = today
        if(debug){cat("\t potential pregnancy end: ",potential_pregnancy_end_type,"\n")}
      }
      
      # if current pregnancy has been going on for more than 2 cycle; pregnancy ends
      if((potential_pregnancy_end_type != "none")& 
         ((time_since_first_positive_preg_test >= (2*median_cycle_length)) |
          (current_cycle_length >= (3*median_cycle_length))
          )){
        day_last_cycle_start = today
        cycle_starts = c(cycle_starts,day_last_cycle_start)
        pregnancies = c(pregnancies, 1)
        is_pregnant = FALSE
        potential_pregnancy_end_type = "none"
        if(debug){cat("\t pregnancy end\n")}
      }
    }
    
    # user is pregnant and something happened
    if(is_pregnant && (potential_pregnancy_end_type != "none") && (day_at_potential_pregnancy_end != today)){
      # something happens again! => need to decide if to end pregnancy now or at previous event
      
      # new event is a period start and previous event happened within the last 3 weeks > end now
      # new event is a period start and previous event happened more than 3 weeks ago but less than 1.75*median cycle length ago > end then + end new cycle now
      if(period_start){
        if((today - day_at_potential_pregnancy_end ) %in% (3*7):round(1.75*median_cycle_length)){
          cycle_starts = c(cycle_starts,day_at_potential_pregnancy_end)
          pregnancies = c(pregnancies, 1,0)
        }else{pregnancies = c(pregnancies, 1)}
        day_last_cycle_start = today
        cycle_starts = c(cycle_starts,day_last_cycle_start)
        is_pregnant = FALSE
        if(debug){cat("\t period started; pregnancy end\n")}
        
      }
      
      # new event is not a period start 
      #   - but previous event was a period start > end then + continue new cycle
      #   - previous event was not a period start > continue "pregnancy"
      if((potential_pregnancy_end_type == "period")&
         ( (this_user_day$first_day[i]) | (this_user_day$preg_test[i] == -1) )){
        cycle_starts = c(cycle_starts,day_at_potential_pregnancy_end)
        pregnancies = c(pregnancies, 1)
        day_last_cycle_start = day_at_potential_pregnancy_end
        is_pregnant = FALSE
        potential_pregnancy_end_type = "none"
        if(debug){cat("\t previous period was actually a pregnancy end\n")}
      }
      
      # if a positive pregnancy test is logged, previous event is cancelled
      if(this_user_day$preg_test[i] == 1){ 
        potential_pregnancy_end_type = "none"
        if(debug){cat("\t pregnancy continues\n")}
      }
      
    }
    
    if(this_user_day$menstruation[i] > 0){
      day_last_bleeding =  today
      if(debug){cat("\t\t bleeding\n")}
    }
    
    if(sum(pregnancies == 0)>4){
      prev_median_cycle_length = median_cycle_length
      cycle_starts_no_preg = cycle_starts[pregnancies == 0]
      median_cycle_length = median(diff(cycle_starts_no_preg[(length(cycle_starts_no_preg)-3):length(cycle_starts_no_preg)]), na.rm = TRUE)
      if(is.na(median_cycle_length)){median_cycle_length = prev_median_cycle_length}
      if(debug & (prev_median_cycle_length != median_cycle_length)){cat("\t median cycle length : ",median_cycle_length,"\n")}
    }
    
  
    
  }
  #if(length(cycle_starts) == 0){ cycle_starts = 0}
  return(cycle_starts)
}

impute_temp_and_mucus = function(obs, d){
  
  d$cycle_id = as.character(d$cycle_id)
  if(any(is.na(d$cycle_id))) d$cycle_id[is.na(d$cycle_id)] = "extra cycle"
  cycle_ids  = unique(d$cycle_id)
  cycles_agg = unique(select(d, user_id, cycle_id, cycle_nb, cycle_length))
  median_cycle_length = median(cycles_agg$cycle_length, na.rm = TRUE)
  df = foreach(cycle_id = cycle_ids, .combine = rbind) %do% {
    #cat(cycle_id)
    this_cycle = cycles_agg[which(cycles_agg$cycle_id == cycle_id),]
    d_this_cycle = d[which(d$cycle_id == cycle_id),]
    this_cycle_length = this_cycle$cycle_length
    if(is.na(this_cycle_length)) this_cycle_length = as.numeric(max(d_this_cycle$date) - min(d_this_cycle$date))+1
    this_cycle_dates = seq(min(d_this_cycle$date), min(d_this_cycle$date)+this_cycle_length-1, by = 1)
    m = match(this_cycle_dates, obs$date)
    temp = obs$temp[m]
    temp_logged = 1 * !is.na(temp) 
    n_temp = sum(temp_logged)
    mucus = obs$mucus[m]
    mucus_logged = 1 * !is.na(mucus) 
    n_mucus = sum(mucus_logged)
    
    temp = interpolate(temp, v_before = 0.1, v_after = 0.5)
    mucus = interpolate(mucus, v_before = 0, v_after = 0)
    
    df = data.frame(date = this_cycle_dates, 
                    temp = temp, temp_logged = temp_logged, 
                    mucus = mucus, mucus_logged = mucus_logged)
    return(df)  
  }
  m = match(obs$date, df$date)
  df = df[m,]
  return(df)
}





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
  # temp and mucus imputed below
  obs$LH[is.na(obs$LH)] = 0 
  obs$preg_test[is.na(obs$preg_test)] = 0 
  obs$anylog[is.na(obs$anylog)] = 0
  obs$gap = replace_NAs_with_latest_value(obs$gap)
  obs$first_day[is.na(obs$first_day)] = 0
  
  imputed_temp_and_mucus = impute_temp_and_mucus(obs, d)
  obs$temp = imputed_temp_and_mucus$temp
  obs$temp_logged = imputed_temp_and_mucus$temp_logged
  obs$mucus = imputed_temp_and_mucus$mucus
  obs$mucus_logged = imputed_temp_and_mucus$mucus_logged
  
  if(finish_before_new_cycle){
    j = max(which(obs$first_day == 1))-1
    if(j %in% 1:nrow(obs)){obs = obs[1:j,]}else{obs = data.frame()}
  }
  
  return(obs)
}
