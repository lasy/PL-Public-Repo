# code parameters
par = list()
par$IO_user = Sys.getenv("LOGNAME")
par$dataset =  "Kindara_full" # "Kindara_full" "Kindara_subset"
par$n_cores = detectCores() - 1
par$max_batch_size = 5000
par$min_n_batches = 10


# Input and output variables
source("Scripts/00_variables_IO.R")


# variables

#var = list()




# VIZ variables

# axes
viz = list()
viz$xaxis_j = c(seq(0,91,by = 7), 120, 180, 210, 240, 270, 300 )
viz$xaxis_s = 0:100
viz$xaxis_m = 0:30


# colors
viz$cols = list()

viz$cols$transparent = hsv(h = 0, s = 1, v = 0, alpha = 0)
viz$cols$alpha = 0.4



# temperature
viz$cols$cold = 'steelblue'
viz$cols$neutral = 'gray80'
viz$cols$warm = 'tomato'


# bleeding
viz$cols$red.hue = 0.05 
viz$cols$dark.red = hsv(viz$cols$red.hue, s = 1, v = 0.8)
viz$cols$medium.red = hsv(viz$cols$red.hue, s = 1, v = 1)
viz$cols$light.red = hsv(viz$cols$red.hue, s = 0.8, v = 1)
viz$cols$very.light.red = hsv(viz$cols$red.hue, s = 0.5, v = 1)


# mucus
viz$cols$light.creamy = hsv(0.2,s = 0.3,v = 0.9)
viz$cols$light.creamy.transp = hsv(0.2,s = 0.3,v = 0.9, alpha = viz$cols$alpha)
viz$cols$light.blue = hsv(0.61,s = 0.3,v = 1)
viz$cols$light.blue.transp = hsv(0.61,s = 0.3,v = 1, alpha = viz$cols$alpha)
viz$cols$blue = hsv(0.61,s = 0.5,v = 1)
viz$cols$blue.transp = hsv(0.61,s = 0.5,v = 1, alpha = viz$cols$alpha)
viz$cols$yellow = hsv(0.12,s = 0.5,v = 1)
viz$cols$yellow.transp = hsv(0.12,s = 0.5,v = 1, alpha = viz$cols$alpha)
viz$cols$light.yellow = hsv(0.12, s = 0.3, v = 1)
viz$cols$dark.yellow = hsv(0.12,s = 0.8,v = 0.85)
viz$cols$gray = hsv(0,s=0,v = 0.5)
viz$cols$gray.transp = hsv(0,s=0,v = 0.5, alpha = viz$cols$alpha)



# sex
viz$cols$sex.red = hsv(0.97, s = 1, v = 1)


# dictionaries

dict = list()

dict$preg_test = data.frame(names = c("no test","pos","neg"),
                              values = c(0,1,-1))

dict$preg_test_o = data.frame(names = c("no test","pos","neg"),
                            values = c(0,1,2))

dict$sex = data.frame(name = c("no sex","protected","unprotected","withdrawal","insemination"),
                      values = 0:4)

dict$pregnancy_timeline = data.frame(
  abbreviation = c("FP-VEPL","EPL","LPL","ExPTB","PTB","TB","BF","unclear"),
  name = c("False Positive or Very Early Pregnancy Loss","Early Pregnancy Loss","Late Pregnancy Loss","Extreme Pre-Term Birth","Pre-Term Birth","Term Birth","Breast Feeding", "Unclear"),
  duration_in_weeks = c(6,13, 22 ,28, 37,43,80,Inf), 
  colors = c("gray40","orange","tomato","red","plum","seagreen3","cornflowerblue","gray80"),
  stringsAsFactors = FALSE)

dict$pregnancy_timeline$duration_in_days = dict$pregnancy_timeline$duration_in_weeks*7
dict$pregnancy_timeline$duration_in_4weeks = dict$pregnancy_timeline$duration_in_weeks/4


write.table(dict$pregnancy_timeline[,1:3], 
            file = paste0(IO$tables,"Pregnancy_timeline.csv"),
            sep = ",",quote = FALSE, row.names = FALSE)


dict$fertility_counting = data.frame(cycleday_from_end = -20:-8)
dict$fertility_counting$fertility = 
  sigmoid(dict$fertility_counting$cycleday_from_end, ix = -15, s = 0.75) - 
  sigmoid(dict$fertility_counting$cycleday_from_end, ix = -11, s = 1)
dict$fertility_counting$fertility = dict$fertility_counting$fertility/max(dict$fertility_counting$fertility)


# temperature

temp.col.function <- colorRampPalette(c(viz$cols$cold, viz$cols$neutral, viz$cols$warm))

temp.values = seq(96,99, by = 0.5)
temp.colors = temp.col.function(length(temp.values))
dict$temp =  data.frame(values = temp.values, 
                        colors = temp.colors, 
                        stringsAsFactors = FALSE)

rm(temp.values, temp.colors)


# bleeding
bleeding.index = sort(c(0.5,0:3))
bleeding.names = c('No bleeding','Spotting','Light bleeding','Medium bleeding','Heavy bleeding')
bleeding.colors = c(viz$cols$transparent,viz$cols$very.light.red, viz$cols$light.red,viz$cols$medium.red,viz$cols$dark.red)
bleeding.pch = c(1,1,16,16,16)

dict$bleeding = data.frame(index = bleeding.index, 
                           names = bleeding.names, 
                           colors = bleeding.colors, 
                           pch = bleeding.pch,
                           stringsAsFactors = FALSE)

rm(bleeding.pch, bleeding.colors, bleeding.names, bleeding.index )



# mucus
quantities = c('little','medium','lots')
mucus.numbers = c(NA,0:12)
mucus.names = c('Undefined','Nothing', 
                paste('Sticky -',quantities), 
                paste('Creamy -',quantities), 
                paste('Eggwhite -',quantities), 
                paste('Watery -',quantities))
mucus.colors = c(viz$cols$transparent, viz$cols$gray, 
                 rep(viz$cols$yellow,3), 
                 rep(viz$cols$light.creamy,3), 
                 rep(viz$cols$light.blue,2), viz$cols$blue , 
                 viz$cols$light.blue, rep(viz$cols$blue,2))
mucus.symbols = c(16,4,rep(16,9),rep(15,3))
mucus.cex = c(0,1,1:3,1:3,1:3,1:3)
mucus.lty = c(rep(1,11),rep(2,3))
dict$mucus = data.frame(index = mucus.numbers, 
                        names = mucus.names, 
                        colors = mucus.colors, 
                        symbols = mucus.symbols,
                        cex = mucus.cex,
                        lty = mucus.lty,
                        stringsAsFactors = FALSE)

rm(mucus.numbers, mucus.names, mucus.colors, mucus.symbols, mucus.cex,mucus.lty, quantities)



# CERVIX

#firmness
firmness.numbers = c(0:3)
firmness.names = c('Undefined','Firm','Medium','Soft')
firmness.colors = c('gray80', viz$cols$yellow, viz$cols$light.blue, viz$cols$blue)
dict$firmness = data.frame(index = firmness.numbers, 
                           names = firmness.names, 
                           colors = firmness.colors, 
                           stringsAsFactors = FALSE)

rm(firmness.numbers, firmness.names, firmness.colors)



#openness
openness.numbers = c(0:3)
openness.names = c('Undefined','Closed','Medium','Open')
openness.colors = c('gray80', viz$cols$yellow, viz$cols$light.blue, viz$cols$blue)
dict$openness = data.frame(index = openness.numbers, 
                           names = openness.names, 
                           colors = openness.colors, 
                           stringsAsFactors = FALSE)

rm(openness.numbers, openness.names, openness.colors)



#height
height.numbers = c(0:3)
height.names = c('Undefined','Low','Medium','High')
height.colors = c('gray80', viz$cols$yellow, viz$cols$light.blue, viz$cols$blue)
dict$height = data.frame(index = height.numbers, 
                         names = height.names, 
                         colors = height.colors, 
                         stringsAsFactors = FALSE)

rm(height.numbers, height.names, height.colors)


# CERVIX (max of any of the 3)

cervix.numbers = c(0:3)
cervix.names = c('Undefined','Low fertility','Medium','High fertility')
cervix.colors = c(viz$cols$transparent, viz$cols$yellow, viz$cols$light.blue, viz$cols$blue )
dict$cervix =  data.frame(index = cervix.numbers, 
                          names = cervix.names, 
                          colors = cervix.colors, 
                          stringsAsFactors = FALSE)

rm(cervix.numbers, cervix.names, cervix.colors)



# SEX

sex.index = 0:4
sex.names = c('No sex','Protected sex','Unprotected sex','Withdrawal','Insemination')
sex.short.names = c('no_sex','prot_sex','unprot_sex','withdrawal','insemination')
sex.colors = c(viz$cols$transparent, viz$cols$sex.red, viz$cols$sex.red, viz$cols$sex.red,'black')
sex.bg = c(viz$cols$transparent, viz$cols$transparent, viz$cols$sex.red, NA, NA)
sex.symbols = c(1,23,23,9,8)
dict$sex = data.frame(index = sex.index, 
                      names = sex.names, 
                      short.names =sex.short.names, 
                      symbols = sex.symbols,
                      colors  = sex.colors,
                      bg = sex.bg,
                      stringsAsFactors = FALSE)

rm(sex.index, sex.names,sex.short.names,sex.colors,sex.bg,sex.symbols)


# TESTS

preg.test.index = 0:2
preg.test.names = c('Undefined','Positive','Negative')
preg.test.colors = c(viz$cols$transparent, 'steelblue', 'red4')
preg.test.pch = c(NA, 16, 4)
dict$preg.test.viz = data.frame(index = preg.test.index, 
                            names = preg.test.names, 
                            colors = preg.test.colors,
                            pch = preg.test.pch, 
                            stringsAsFactors = FALSE)
rm(preg.test.index, preg.test.names,preg.test.colors,preg.test.pch)


opk.index = 0:2
opk.names = c('Undefined','Positive','Negative')
opk.colors = c(viz$cols$transparent, 'steelblue', 'red4')
opk.pch = c(NA, 16, 4)
dict$opk = data.frame(index = opk.index, names = opk.names, 
                      colors = opk.colors,
                      pch = opk.pch, 
                      stringsAsFactors = FALSE)
rm(opk.index, opk.names,opk.colors,opk.pch)



