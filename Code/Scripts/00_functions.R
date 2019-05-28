source("Scripts/00_functions_viz.R")


lu = function(x){
  length(unique(x))
}


sigmoid = function(x, ix, s){ 1/(1+exp(-(x-ix)*s))}



