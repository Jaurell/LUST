rm(list = ls())
##uncomment to choose cohort
load('H:/LUST/new_data/x4_pre_impute.Rdata') ## EX2004 
#load('H:/LUST/new_data/x6_pre_impute.Rdata') ## EX2006
##

library(mice)

## creates a dataframe where each column contains the variables to be imputed together, excludes dependent variables
deps <- as.vector(input['items'][which(input['dependent'] == 1),])
deps <- sapply(deps,function(x) sprintf('%s%s%s',prefix,times,x))
vars <- as.data.frame(select_vars)
vars <- vars[,-which(colnames(vars) %in% colnames(deps))]
deps <- as.vector(unlist(deps))
deps <- deps[which(deps %in% colnames(rdata_rev))]
deps <- deps[!is.na(deps)]


iter <- 5 #number of iterations while imputing
imputs <- 50 # number of imputations
miss_if_less <- 1 ## how many non-missing occations are needed for an item to be imputed


nonimp <- c(id,gender,deps) #variables to include in full dataset but not to impute
df <- list() 
first_imp <- 0

for (var in colnames(vars)){
  meth <- NULL
  mi <- vars[var]
  mi <- mi[!is.na(mi)]
  for (v in mi){
    l <- unique(rdata_rev[[v]])
    if (length(l[!is.na(l)]) == 2){
      meth[v] <- 'logreg'
    } else {
      meth[v] <- 'pmm'
    }
  }
  if (length(mi) > 1){## skips imputation if an item is measured less than twice
    if (first_imp == 0){ ## first imputation cycle includes all variables that are not to be imputed and removes them from the imputation and prediction matrix
      init = mice(rdata_rev[c(nonimp,mi)], maxit=0) 
      methni = init$method
      predM = init$predictorMatrix
      predM[, nonimp] <- 0
      methni[nonimp] <- ""
      methni[mi] <- unique(meth)
      mi <- vars[var]
      mi <- mi[!is.na(mi)]
      m <- mice(rdata_rev[c(nonimp,mi)], m = imputs, maxit = iter,method = methni, predictorMatrix = predM)
      first_imp <- 1
    }else{ # second and following imputation cycle follows
      m <- mice(rdata_rev[mi],m = imputs, maxit = iter,method = meth)
    }
    for (impvar in mi){ ## recodes item to missing if less than 'miss_if_less' occations
      df_na <- cbind(rownames(rdata_rev[mi]),apply(rdata_rev[mi], 1, function(x) sum(!is.na(x))))
      rm_row <- df_na[which(df_na[,2] < miss_if_less),][,1]
      m$imp[[impvar]][which(rownames(m$imp[[impvar]]) %in% rm_row),] <- NA
    }
    df[[var]] <- m #appends imputed variables to a list
  }
}

## bind all imputed datasets to one object
for (x in 1:(length(df)-1)){
  if (x == 1){
    midata <- cbind.mids(df[[x]],df[[x+1]])
  }else{
    midata <- cbind.mids(midata,df[[x+1]])
  }
}


## saves workspace for later steps
save.image(file = sprintf('H:/LUST/new_data/%simputed.Rdata',prefix))

## uncomment if you want to do the analysis right after the imputation
source('analyse.R')
