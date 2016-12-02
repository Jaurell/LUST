reprime <- function(prefix){
rm(list = ls())
source('functions.R')
library(haven)
library(mice)
  wrkspc <- sprintf('H:/LUST/new_data/%simputed.Rdata',prefix)
  load(wrkspc)
## loads workspace from imputation
#load('H:/LUST/new_data/x4_imputed.Rdata')
#load('H:/LUST/new_data/x6_imputed.Rdata')

fulldata <- complete(midata, include = T, action = 'long') # creates a imputed dataset including the original data as 0
input_exp <- read.csv('input_exposure_vars.csv') # dataframe of chosen cutoffs to create longitudinal exposure variables
exp_cut <- sprintf('%scutoff',prefix) # which column to use

indeps <- c(indep_scales,indep_items)
input[cut_mod][is.na(input[cut_mod])]<-0

## the following creates a list of primes to be used in the recoding. It also creates a value label dictionary

library(numbers)
ps <- Primes(n1 = 1,n2 = 1000)
ps <- ps[1:(length(times)*2)] 
names(ps) <- rep(c(0,1),length(ps)/2)
prime_list <- NULL
##The number of loops is eaqual to how many occations you wish to use for the creation of the exposure variables.
for (x1 in 1:2){
  for (x2 in 3:4){
    prime_list[sprintf('%s_%s',names(ps[x1]),names(ps[x2]))] <- prod(c(ps[x1],ps[x2]))
    #for (x3 in 5:6){
     # for (x4 in 7:8){
      #  prime_list[sprintf('%s_%s_%s_%s',names(ps[x1]),names(ps[x2]),names(ps[x3]),names(ps[x4]))] <- prod(c(ps[x1],ps[x2],ps[x3],ps[x4]))
        #for (x5 in 9:10){
          #prime_list[sprintf('%s_%s_%s_%s_%s',names(ps[x1]),names(ps[x2]),names(ps[x3]),names(ps[x4]),names(ps[x5]))] <- prod(c(ps[x1],ps[x2],ps[x3],ps[x4],ps[x5]))
        #}
      #}
    #}
  }
}

exp_n <- 1:(length(times)-1) # how many exposures for first and last
times2 <- times[exp_n]
tempdf <- fulldata

for  (indep in indeps){ 
  items <- input['items'][input[version] ==  indep | input[single_item] == indep]
  items_check <- sapply(items[!is.na(items)], function(x) sprintf('%s%s%s',prefix,times2,x))
  if (length(items_check[!items_check %in% colnames(tempdf)]) > 0) {
    print(sprintf('%s does not have items for all occations',indep))
    }else{
  n <- 1
  for (t in times2){
    unexp_prime <- ps[n]
    exp_prime <- ps[n+1]
    n <- n+2
    items <- input['items'][input[version] ==  indep | input[single_item] == indep]
    items <- sapply(items[!is.na(items)], function(x) sprintf('%s%s%s',prefix,t,x))
    items[which(!items %in% colnames(fulldata))] <- NA
    items <- items[!is.na(items)]
    for (i in items){
      if ("factor" %in% attributes(tempdf[[i]])$class){
        tempdf[sprintf('%s_exp',i)] <- as.numeric(tempdf[[i]]) > 1
      } else {
    mid <- attributes(fulldata[[i]])$labels
    mid <- ceiling(max(mid[!mid %in% missing_vals])/2)
    mod <- input[cut_mod][input['items'] == substring(i,5)]
    dicot <- mid+mod
    tempdf[sprintf('%s_exp',i)] <- as.numeric(tempdf[i] > dicot)
      }
    }
    cut <- as.numeric(input_exp[exp_cut][input_exp['indep'] == indep])
    
    temp <- rowSums(tempdf[sapply(items, function(x) sprintf('%s_exp',x))])
    temp2 <- temp
    temp[which(temp >= (cut))] <- exp_prime
    temp[which(temp < (cut))] <- unexp_prime
    temp2[which(temp2 < (cut))] <- 0
    temp2[which(temp2 >= (cut))] <- 1
    tempdf[sprintf('%s%s%s_prime',prefix,t,indep)] <- temp
    tempdf[sprintf('%s%s%s',prefix,t,indep)] <- temp2
    }
  prime <- as.vector(sapply(indep, function(x) sprintf('%s%s%s_prime',prefix,times2,x)))
  sums <- as.vector(sapply(indep, function(x) sprintf('%s%s%s',prefix,times2,x)))
  tempdf[sprintf('%s_prime',indep)] <- apply(tempdf[prime], 1, prod)
  tempdf[sprintf('%s_sum',indep)]<- rowSums(tempdf[sums])
  attributes(tempdf[[sprintf('%s_prime',indep)]])$labels <- prime_list
  attributes(tempdf[[sprintf('%s_prime',indep)]])$class <- 'labelled'
  for (n in exp_n){
  first <- as.vector(sapply(indep, function(x) sprintf('%s%s%s',prefix,times2[1:n],x)))
  last <- as.vector(sapply(indep, function(x) sprintf('%s%s%s',prefix,times2[(length(times2) - n +1):length(times2)],x)))
  tempdf[sprintf('%s_first_%s',indep,n)] <- as.numeric(rowSums(tempdf[first]) == n)
  tempdf[sprintf('%s_last_%s',indep,n)] <- as.numeric(rowSums(tempdf[last]) == n)
  }
    }
}

## formats the dataset so that spss can interpret it as a multiple imputaion dataset
tempdf <- format_spss_impute(tempdf)

## reorders the columns in the dataset so that exposure variables are last
all_vars <- colnames(tempdf)
ut <- as.vector(sapply(indeps, function(x) sprintf('%s_%s',x,c('prime','sum',sprintf('first_%s',exp_n),sprintf('last_%s',exp_n)))))
ut <- ut[ut %in% all_vars]
all_ut <- subset(all_vars, !(all_vars %in% ut))
tempdf <- tempdf[c(all_ut, ut)]

## saves an image of the workspace for later steps
save.image(file = sprintf('H:/LUST/new_data/%sprime.Rdata',prefix))
}