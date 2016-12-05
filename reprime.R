reprime <- function(prefix){
  
  source('functions.R') # load functions
  library(haven)
  library(mice)
  wrkspc <- sprintf('H:/LUST/new_data/%simputed.Rdata',prefix) # location of saved workspace form the imputation step
  load(wrkspc) # load the workspace
  
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
  times2 <- times[exp_n] # only use the first X items
  tempdf <- fulldata 
  
  for  (indep in indeps){ 
    items <- input['items'][input[version] ==  indep | input[single_item] == indep] # list of independent variables
    items_check <- sapply(items[!is.na(items)], function(x) sprintf('%s%s%s',prefix,times2,x)) # create a list of prefixed items for the occations set in times2
    if (length(items_check[!items_check %in% colnames(tempdf)]) > 0) { 
      print(sprintf('%s does not have items for all occations',indep))
    }else{
      n <- 1
      for (t in times2){
        unexp_prime <- ps[n] # unexposed prime number
        exp_prime <- ps[n+1] # exposed prime number
        n <- n+2 # increse value of n
        items <- input['items'][input[version] ==  indep | input[single_item] == indep] 
        items <- sapply(items[!is.na(items)], function(x) sprintf('%s%s%s',prefix,t,x)) # list of items for this occation
        items[which(!items %in% colnames(fulldata))] <- NA
        items <- items[!is.na(items)]
        for (i in items){ # for every item set cutoffs from input instructions
          if ("factor" %in% attributes(tempdf[[i]])$class){ # if item is factor
            tempdf[sprintf('%s_exp',i)] <- as.numeric(tempdf[[i]]) > 1
          } else {
            mid <- attributes(fulldata[[i]])$labels 
            mid <- ceiling(max(mid[!mid %in% missing_vals])/2)
            mod <- input[cut_mod][input['items'] == substring(i,5)]
            dicot <- mid+mod
            tempdf[sprintf('%s_exp',i)] <- as.numeric(tempdf[i] > dicot)
          }
        }
        cut <- as.numeric(input_exp[exp_cut][input_exp['indep'] == indep]) # cutoff for scales
        
        temp <- rowSums(tempdf[sapply(items, function(x) sprintf('%s_exp',x))]) # new column variable name+_exp set to the sum of items
        temp2 <- temp # this is used to create the variable that sum the number of exposures
        temp[which(temp >= (cut))] <- exp_prime # set all values equal or over cutoff to exposed prime
        temp[which(temp < (cut))] <- unexp_prime # set all values below cutoff to unexposed prime
        temp2[which(temp2 < (cut))] <- 0 
        temp2[which(temp2 >= (cut))] <- 1
        tempdf[sprintf('%s%s%s_prime',prefix,t,indep)] <- temp #  creates the prime variable
        tempdf[sprintf('%s%s%s',prefix,t,indep)] <- temp2 # creates the a exposed // unexposed variable
      }
      prime <- as.vector(sapply(indep, function(x) sprintf('%s%s%s_prime',prefix,times2,x))) # list of prime variables
      sums <- as.vector(sapply(indep, function(x) sprintf('%s%s%s',prefix,times2,x))) # list of exposed // unexposed variables
      tempdf[sprintf('%s_prime',indep)] <- apply(tempdf[prime], 1, prod) # product of primes to new variabel
      tempdf[sprintf('%s_sum',indep)]<- rowSums(tempdf[sums]) # sum of exposures to new variabel
      attributes(tempdf[[sprintf('%s_prime',indep)]])$labels <- prime_list # sets the value labels  for the prime variabel
      attributes(tempdf[[sprintf('%s_prime',indep)]])$class <- 'labelled' # set class to labelled
      for (n in exp_n){
        first <- as.vector(sapply(indep, function(x) sprintf('%s%s%s',prefix,times2[1:n],x))) # exposed the first x times?
        last <- as.vector(sapply(indep, function(x) sprintf('%s%s%s',prefix,times2[(length(times2) - n +1):length(times2)],x))) #exposed the last x times?
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
  
  return(tempdf)
}