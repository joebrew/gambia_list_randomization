library(gtools)
library(dplyr)
library(tidyr)
library(readr)

set.seed(1)

combined <- data_frame(iteration = as.numeric(c()),
                       question = as.character(c()))
generate_list <- function(use_case = FALSE){
  controls <- c('I used a telephone yesterday.',
                'I ate benachin yesterday.',
                'I worked yesterday.',
                'I used transportation other than walking yesterday.')
  case <- 'I slept under a mosquito net last night.'
  if(use_case){
    use <- c(controls, case)
    file_name <- 'intervention.csv'
  } else {
    use <- controls
    file_name <- 'non_intervention.csv'
  }
  out <- permutations(n = length(use), r = length(use), v = use)
  out <- t(out)
  out <- data.frame(out)
  if(use_case){
    out <- gather(out, key, value, X1:X120)
  } else {
    out <- gather(out, key, value, X1:X24)
  }
  out <- out %>%
    mutate(key = as.numeric(gsub('X', '', key))) %>%
    rename(iteration = key,
           question = value)
  
  write_csv(out, file_name)
  return(out)
}
for (i in c(TRUE, FALSE)){
  generate_list(use_case = i)
}
write_csv(combined, 'combined.csv')




