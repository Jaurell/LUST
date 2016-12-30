reprime <- function(prefix,last = 0){
  
  #source('functions.R') # load functions
  library(haven)
  library(mice)
  #wrkspc <- sprintf('H:/LUST/new_data/%simputed.Rdata',prefix) # location of saved workspace form the imputation step
  #load(wrkspc) # load the workspace
  
  fulldata <- complete(midata, include = T, action = 'long') # creates a imputed dataset including the original data as 0
  input_exp <- read.csv('input_exposure_vars.csv') # dataframe of chosen cutoffs to create longitudinal exposure variables
  exp_cut <- sprintf('%scutoff',prefix) # which column to use
  
  indeps <- indep_scales
  input[cut_mod][is.na(input[cut_mod])]<-0
  
  ## the following creates a list of primes to be used in the recoding. It also creates a value label dictionary
  
  library(numbers)
  ps <- Primes(n1 = 1,n2 = 1000)
  ps <- ps[1:((length(times)-last)*2)] 
  names(ps) <- rep(c(0,1),length(ps)/2)
  l <- split(ps, ceiling(seq_along(ps)/2))
  f <- do.call(expand.grid, l)
  n <- sapply(f,names)
  plist <- list()
  for (h in 1:nrow(f)){
    val <- prod(f[h,])
    nam <- paste(n[h,],collapse = '_')
    plist[nam] <- val
  }
  prime_list <- unlist(plist)
  
  exp_n <- 1:(length(times)-last) # how many exposures for first and last
  times2 <- times[exp_n] # only use the first X items
  #fulldata <- fulldata 
  
  for  (indep in indeps){ 
    items <- input['items'][input[version] ==  indep | input[version2] == indep] # list of independent variables
    items_check <- sapply(items[!is.na(items)], function(x) sprintf('%s%s%s',prefix,times2,x)) # create a list of prefixed items for the occations set in times2
    if (length(items_check[!items_check %in% colnames(fulldata)]) > 0) { 
      print(sprintf('%s does not have items for all occations',indep))
    }else{
      n <- 1
      for (t in times2){
        unexp_prime <- ps[n] # unexposed prime number
        exp_prime <- ps[n+1] # exposed prime number
        n <- n+2 # increse value of n
        items <- input['items'][input[version] ==  indep | input[version2] == indep] 
        items <- sapply(items[!is.na(items)], function(x) sprintf('%s%s%s',prefix,t,x)) # list of items for this occation
        items[which(!items %in% colnames(fulldata))] <- NA
        items <- items[!is.na(items)]
        for (i in items){ # for every item set cutoffs from input instructions
          if ("factor" %in% attributes(fulldata[[i]])$class){ # if item is factor
            fulldata[sprintf('%s_exp',i)] <- as.numeric(fulldata[[i]]) > 1
          } else {
            mid <- attributes(fulldata[[i]])$labels 
            mid <- ceiling(max(mid[!mid %in% missing_vals])/2)
            mod <- input[cut_mod][which(input['items'] == substring(i,5)),]
            dicot <- mid+mod
            fulldata[sprintf('%s_exp',i)] <- as.numeric(fulldata[i] > dicot)
          }
        }
        cut <- as.numeric(input_exp[exp_cut][input_exp['indep'] == indep]) # cutoff for scales
        
        first_last <- rowSums(fulldata[sapply(items, function(x) sprintf('%s_exp',x))]) # new column variable name+_exp set to the sum of items
        sum_variable <- first_last # this is used to create the variable that sum the number of exposures
        first_last[which(first_last >= (cut))] <- exp_prime # set all values equal or over cutoff to exposed prime
        first_last[which(first_last < (cut))] <- unexp_prime # set all values below cutoff to unexposed prime
        sum_variable[which(sum_variable < (cut))] <- 0 
        sum_variable[which(sum_variable >= (cut))] <- 1
        fulldata[sprintf('%s%s%s_prime',prefix,t,indep)] <- first_last #  creates the prime variable
        fulldata[sprintf('%s%s%s',prefix,t,indep)] <- sum_variable # creates the a exposed // unexposed variable
      }
      prime <- as.vector(sapply(indep, function(x) sprintf('%s%s%s_prime',prefix,times2,x))) # list of prime variables
      sums <- as.vector(sapply(indep, function(x) sprintf('%s%s%s',prefix,times2,x))) # list of exposed // unexposed variables
      fulldata[sprintf('%s_prime',indep)] <- apply(fulldata[prime], 1, prod) # product of primes to new variabel
      fulldata[sprintf('%s_sum',indep)]<- rowSums(fulldata[sums]) # sum of exposures to new variabel
      attributes(fulldata[[sprintf('%s_prime',indep)]])$labels <- prime_list # sets the value labels  for the prime variabel
      attributes(fulldata[[sprintf('%s_prime',indep)]])$class <- 'labelled' # set class to labelled
      for (n in exp_n){
        first <- as.vector(sapply(indep, function(x) sprintf('%s%s%s',prefix,times2[1:n],x))) # exposed the first x times?
        last <- as.vector(sapply(indep, function(x) sprintf('%s%s%s',prefix,times2[(length(times2) - n +1):length(times2)],x))) #exposed the last x times?
        fulldata[sprintf('%s_first_%s',indep,n)] <- as.numeric(rowSums(fulldata[first]) == n)
        fulldata[sprintf('%s_last_%s',indep,n)] <- as.numeric(rowSums(fulldata[last]) == n)
      }
    }
  }
  
  ## formats the dataset so that spss can interpret it as a multiple imputaion dataset
  fulldata <- format_spss_impute(fulldata)
  
  ## restores the labels if they have gotten lost during the process
  for (v in colnames(fulldata)){
    atr <- attributes(fulldata[[v]])
    if (is.null(atr[['label']])){
      attributes(fulldata[[v]])['label'] <- attributes(org_data[[v]])['label']
    }
  }
  
  ## reorders the columns in the dataset so that exposure variables are last
  all_vars <- colnames(fulldata)
  ut <- as.vector(sapply(indeps, function(x) sprintf('%s_%s',x,c('prime','sum',sprintf('first_%s',exp_n),sprintf('last_%s',exp_n))))) 
  ut <- ut[ut %in% all_vars]
  all_ut <- subset(all_vars, !(all_vars %in% ut))
  fulldata <<- fulldata[c(all_ut, ut)]
  

  #return(tempdf)
}