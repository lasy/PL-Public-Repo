

################
# PLOT tracking history
################


plot.tracking.history = function(d = days, 
                                 show_tests = FALSE, 
                                 show_consec_cycles = FALSE,
                                 show_fertile_window = FALSE, 
                                 relative_date = TRUE,
                                 average_temp = TRUE,
                                 show_app_defined_cycle_starts = FALSE){
  
  n.lines = 5 # number of tracking lines (besides temperature)
  if(show_tests){n.lines = 7}
  n.lines.temp = 3
  
  par(las = 1, mar = c(2, 4, 2, 0.5) + 0.1)
  
  d$bleeding = d$menstruation
  d$bleeding[which((d$spotting == "True") & (d$bleeding == 0))] = 0.5
  
  mucus = rep(0, nrow(d)) * NA
  mucus[d$no_fluid == "True"] = 0
  mucus[which(d$fluid_sticky>0)] = d$fluid_sticky[which(d$fluid_sticky>0)]
  mucus[which(d$fluid_creamy>0)] = d$fluid_creamy[which(d$fluid_creamy>0)] + 3
  mucus[which(d$fluid_eggwhite>0)] = d$fluid_eggwhite[which(d$fluid_eggwhite>0)] + 6
  mucus[which(d$fluid_watery>0)] = d$fluid_watery[which(d$fluid_watery>0)] + 9
  
  d$mucus = mucus
  rm(mucus)
  
  
  k = (d$bleeding > 0) |
    (!is.na(d$mucus)) |
    (!is.na(d$temperature)) |
    (d$opk > 0) |
    (d$sex > 0) |
    (d$preg_test > 0)  |
    (d$cervix_firmness > 0) |
    (d$cervix_height > 0) |
    (d$cervix_openness > 0)
  if("vaginal_sensation" %in% colnames(d)){ k = k | (d$vaginal_sensation > 0)}
  d$user_input = k
  
  
  d = d[order(d$user_id, d$date),]
  
  main = paste('user ID: ',unique(d$user_id))
  
  ylim = c(0, n.lines + n.lines.temp)
  
  plot(range(d$date), ylim, type = 'n' , axes = FALSE, 
       xlab = '', ylab = '', main = main, adj = 0, yaxs = 'i', ylim = ylim)
  if(show_app_defined_cycle_starts){
    abline(v = d$date[which(d$first_day)], col = 'gray90')
  }else{
    abline(v = d$date[which(d$is_first_day)], col = 'gray90')
  }
  abline(h = c(0:n.lines,ylim[2]), col = 'gray90')
  abline(h = seq(n.lines+0.5, ylim[2]-0.5, by = 0.5), col = 'gray95', lty = 2)
  if(show_fertile_window){
    xleft = aggregate(date ~ cycle_id, d[d$fertile_window,], min); xleft = xleft$date
    xright = aggregate(date ~ cycle_id, d[d$fertile_window,], max); xright = xright$date
    
    rect(xleft = xleft, xright = xright,
         ybottom = 0, ytop = ylim[2],
         col = rgb(0,0,1,0.1), border = NA)
  }
  
  # any tracking
  line.tracking = 0
  col = 'black'
  if(show_consec_cycles){col = as.numeric(as.factor(d$consec_cycle_id))}
  points(d$date, rep(line.tracking+0.5, nrow(d)), pch = 16, cex =  d$user_input/3, col = col)
  
  # temperature
  min.temp = min(d$temperature, na.rm = TRUE) - 0.5
  max.temp = max(d$temperature, na.rm = TRUE) + 0.5
  d$temp_norm = (d$temperature - min.temp)/(max.temp - min.temp) * n.lines.temp
  points(d$date, rep(n.lines, nrow(d))+(d$temp_norm),
         type = 'l', col = 'gray90')
  temp.col = dict$temp$colors[as.numeric(cut(d$temperature, dict$temp$values))]
  points(d$date, rep(n.lines, nrow(d)) + d$temp_norm,
         cex = 0.5, pch = 16, col = temp.col)
  
  
  #lo = loess(d$temperature ~ as.numeric(d$date), degree = 2)
  
  #points(d$date[!is.na(d$temperature)], 5+lo$fitted-min(d$temperature, na.rm = TRUE), type = 'l', col = 'gray40')
  
  if(average_temp){
    for(c in unique(d$cycle_nb)){
      j = which(d$cycle_nb == c)
      segments(x0 = min(d$date[j], na.rm = TRUE), 
               x1= max(d$date[j], na.rm = TRUE), 
               y0 = n.lines+median(d$temperature[j], na.rm = TRUE)-min.temp, 
               y1 = n.lines+median(d$temperature[j], na.rm = TRUE)-min.temp, col= 'gray40')
    }
  }
  
  # bleeding
  line.bleeding = show_tests*2 + 4
  #  points(d$date, rep(line.bleeding, nrow(d))+d$bleeding/4, pch = 16, 
  #         cex =  d$bleeding/2, 
  #         col = dict$bleeding$colors[match(d$bleeding, dict$bleeding$index)] )
  
  segments(x0 = d$date, x1 = d$date, 
           y0 = line.bleeding, y1 = line.bleeding + d$bleeding/4,
           col= dict$bleeding$colors[match(d$bleeding, dict$bleeding$index)] )  
  
  # mucus
  line.mucus = show_tests*2 + 3
  points(d$date, rep(line.mucus, nrow(d))+d$mucus/20, pch = 16, 
         cex =  dict$mucus$cex[match(d$mucus, dict$mucus$index)]/3,
         col = dict$mucus$colors[match(d$mucus, dict$mucus$index)]
  )
  # cervix
  line.cervix = show_tests*2 + 2
  points(d$date, rep(line.cervix, nrow(d))+d$cervix_height/5, pch = 21, 
         cex =  d$cervix_openness/3,
         col = dict$cervix$colors[match(d$cervix_firmness, dict$cervix$index)]
  )
  
  # sex
  line.sex = show_tests*2 + 1
  tot.sex = 3+show_tests*2
  points(d$date, rep(line.sex, nrow(d))+d$sex/tot.sex,
         cex =  0.5,
         pch = dict$sex$symbols[match(d$sex, dict$sex$index)],
         col = dict$sex$colors[match(d$sex, dict$sex$index)],
         bg = dict$sex$bg[match(d$sex, dict$sex$index)]
  )
  
  # Tests
  if(show_tests){
    line.OPK = 2
    points(d$date, rep(line.OPK, nrow(d))+d$opk/3,
           cex = 0.5, pch = dict$opk$pch[match(d$opk, dict$opk$index)], 
           col = dict$opk$colors[match(d$opk, dict$opk$index)])
    line.preg = 1
    if(any(d$preg_test == -1)){d$preg_test[d$preg_test == -1] = 2}
    points(d$date, rep(line.preg, nrow(d))+d$preg_test/3,
           cex = 1, pch = dict$preg.test.viz$pch[match(d$preg_test, dict$preg.test.viz$index)], 
           col = dict$preg.test.viz$colors[match(d$preg_test, dict$preg.test.viz$index)])
  }
  
  xaxis = seq(min(d$date),max(d$date),by = 'quarter')
  i.l = seq(1,length(xaxis),by = 4)
  xaxis_label = xaxis[i.l];
  
  if(relative_date){xaxis_label = paste0('Y',0:(length(i.l)-1)) }
  
  axis(1, at =xaxis, labels = NA )
  axis(1, at =xaxis[i.l], labels = xaxis_label )
  
  axis.at = c(c(1:n.lines)-0.5, n.lines + 1.5)
  axis.labels = c('any tracking','sex','cervix','mucus','bleeding','temp')
  if(show_tests){axis.labels = c(axis.labels[1],'Preg. test','OPK', axis.labels[2:6])}
  axis(2, at = axis.at, 
       labels = axis.labels,
       tick = FALSE, pos = xaxis[1])
}


