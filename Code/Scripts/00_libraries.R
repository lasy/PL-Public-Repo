
package_list = c('knitr','bookdown','styler',
                 'tidyverse','readr','magrittr',
                 #'MASS',
                 'ggplot2','ggthemes','gridExtra','quantreg','hexbin',
                 # 'plotly',
                 'scales','reshape',
                 #'HMM',
                 'chron','lubridate',
                 'plyr','dplyr',
                 'parallel','doParallel',
                 'tictoc',
                 'feather',
                 'igraph',
                 'mhsmm'
                 
                 #'biwavelet'
                 #'klaR', 'cluster',
                 #'Rcpp',
                 #'e1071', 'parallelSVM', 'randomForest',
)

pckgs = installed.packages()

for(package in package_list){
  cat(package,"\n")
  pckgs = installed.packages()
  need.to.install = (!(package %in% pckgs[,1]))
  if(need.to.install){cat('installing package ',package,'\n');install.packages(package,dependencies = TRUE)}
  library(package = package, character.only = TRUE)
}

rm(package_list, pckgs,need.to.install,package)


