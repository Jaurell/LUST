##Checks that max and min responses are equal to max and min value labels (excluding missing value labels)
## if variable is in rev_vars it reverses the item and the value label
check_reverse <- function(df,vars,rev_vars,missing_vals = NULL){
  file.create('check_log.txt')
  for (var in vars){
    var_max <- max(df[var], na.rm = T) # max response
    var_min <- min(df[var], na.rm = T) # min response
    atr <- attributes(df[[var]])$labels # value lables
    atr <- atr[which(!atr %in% missing_vals)] # removing value labels for missing
    if (is.null(atr)){ # if there are no value labels for the variable
      warning('No labels for this variable\nGetting max value from input')
      temp_var <- substring(var,5) # removing prefix and time in order to identify the item in the input file
      max_val <- input['max_val'][which(input['items'] == temp_var),] # getting max value from input file in the column max_val
      min_val <- 1 # setting min val to one
    } else {
      max_val <- max(atr) # setting max val to max value label
      min_val <- min(atr) # setting min val to min value label
    }
    if (!is.na(max_val)){ # if statements for the warnings
      if (var_max != max_val){
        warning(sprintf('%s maximum response not equal to maximum value in label!\nMax repsonse = %s\nMax value    = %s\n',var,var_max,max(atr)))
      }
    } else {
      mes <- sprintf('No maximum value in set for:\n%s\n',var)
      warning(mes)
      write(mes, file = 'check_log.txt', append = T)
    }
    if (var_min != min_val){ # if statements for the warnings
      warning(sprintf('%s minimum response not equal to maximum value in label!\\nMin repsonse = %s\nMin value    = %s\n',var,var_min,min(atr)))
    }
    if (var %in% rev_vars){ # if variable also is in the list for reversed variables 
      df[var] <- (max_val+1) - df[var] # computes new value, (max val + 1) - observed value
      rev_values <- rev(as.numeric(atr)) # reversing the value lables
      names(rev_values) <- names(atr) # give names to the reversed values
      attributes(df[[var]])$labels <- rev_values # assigning the new value labels to the dataset
    }
    
  }
  return(df)
}

## checks if scales from items have sufficent chronbacs alpha, mean inter item correlation and item total correlation. 
## if only two items it checks the correlation instead. Results are printet to a logfile (check_out)
## it also checks that all the correlations over time are positive for each item.
check_variables <- function(df,scales,input, itc_low = .3,miic_low = .3, check_out){
  write.table(t(c('scale','comment','alpha','miic','min_itc','max_itc','warning','items')),file=check_out,row.names = F, sep = ',', col.names = F) # writes first row
  for (scale in scales){ 
    items <- input['items'][input[version] == scale] # list of items in the scale
    items <- items[!is.na(items)] # removing NA
    items <- sapply(items, function(x) sprintf('%s%s%s',prefix,times,x)) # adding prefixes to all items in the list
    items[which(!items %in% colnames(org_data))] <- NA 
    for (x in 1:nrow(items)){
      row_check <- items[x,][!is.na(items[x,])]
      if (length(row_check) > 1){ # if scale is more than one item
        if (length(row_check) > 2){ # if scale is more than two items
          a <- psych::alpha(as.data.frame(df[row_check]),na.rm = T) # 
          raw_alpha <- a$total$raw_alpha # chronbachs alpha
          miic <- a$total$average_r # mean inter item correlation
          max_itc <- max(a$item.stats$r.drop) # max inter item correlation
          min_itc <- min(a$item.stats$r.drop) # min inter item correlation
          message <- ''
        }else{
          c <- round(cor(df[row_check], use = 'complete.obs')[1,2],3) # correlation between the two items
          # setting the variables below so that it can print the new line in the output
          raw_alpha <- c
          miic <- c
          max_itc <- c
          min_itc <- c
          message <- 'only correlations' # flag telling you it is only a bivariate correlation
        }
        warn <- 0
        neg <- 0
        
        if (min_itc < itc_low){ # setting warning value, if anything looks problematic
          #warning(sprintf('%s: ITC is less than the cutoff',scale))
          warn <- warn + 1
        } 
        if (miic  < miic_low){
          #warning(sprintf('%s: MIIC is less than the cutoff',scale))
          warn <- warn + 1
        }
        out <- c(scale,message,round(c(raw_alpha,miic,min_itc,max_itc),3),warn,row_check) # create the object containg all output information
        write.table(t(out),file = check_out, append = T, row.names = F, col.names = F,sep = ',') # writing it to file
      }
    }
    for (y in 1:ncol(items)){ # checking if a scale has a negative correlation between any of the occations.
      col_check <- items[,y][!is.na(items[,y])]
      if (length(col_check > 1)){
        cormat <- cor(df[col_check], use = 'complete.obs')
        if (any(cormat < 0)){
          warning(sprintf('Correlation between occations is negative',min_corr))
          neg <- 1
          write.table(cormat, file = sprintf('out/%s_%s_corrlog.csv',items[y],y),sep = ',')
        }
      }
    }
  }
}

## recodes items and changes value labels according to the input file.
recode_input <- function(df,input,missing_vals,prefix,times){
  instr <- input[c('items','recode_from','recode_to','val_lab')][which(rowSums(is.na(input[c('recode_from','recode_to')])) == 0),] # getting columns of interest
  for (row in instr[['items']]){ # for every item
    vars <- sapply(times, function(x) sprintf('%s%s%s',prefix,x,row)) # adds prefix to items
    for (v in vars){ # for every seperate item
      if (v %in% colnames(df)){ # if it exists in dataset
        ins <- instr[which(instr['items'] == row),] 
        from <- ins[['recode_from']]
        from <-  unlist(strsplit(as.character(from),',')) #splits up the string based on ','
        to <- ins[['recode_to']]
        to <- unlist(strsplit(as.character(to),','))
        value_lab <- ins[['val_lab']] 
        value_lab <- unlist(strsplit(as.character(value_lab), ','))
        for (x in 1:length(from)){ 
          f <- from[x]
          t <- to[x]
          df[v][which(df[v] == as.integer(f)),] <- as.integer(t) # recodes value f to value t
          if (t == '-99'){ # if any value is set to -99 recodes it to NA
            df[v][which(df[v] == as.integer(t)),] <- NA
          }
        }
        if (!is.null(value_lab)){ 
          check <- unique(df[[v]])
          check <- check[!is.na(check)]
          vals <- seq_along(value_lab)
          names(vals) <- value_lab
          if (length(check) != length(vals)){ # checks that the value label lenght is the same as responses in the data
            warning('value lables not same lenght as unique values in data')
            print(vals)
            print(check)
          }
          attributes(df[[v]])$labels <- vals # sets new value labels
        }
      }
    }
  }
  return(df)
}

## renames items, recodes and changes value labels according to the input file. This function needs to be called first for the original dataset
##  this function is very similar to the 'recode_input' function.
## first it renames variables if specified in the inputfile. then recodes it so that it has the right form in the data.
rename <- function(df,input,times,prefix){
  recode <- input[c('items','rename','recode_from','recode_to','val_lab')][which(!is.na(input['rename'])),] 

  for (r in nrow(recode)){
    re <- recode[r,]
    for (t in times){
      old_name <- sprintf('%s%s%s',prefix,t,re[[1]])
      if (old_name %in% colnames(df)){
        warning(old_name)
        new_name <- sprintf('%s%s%s',prefix,t,re[[2]])
        names(df)[names(df) == old_name] <- new_name
        from <- recode[['recode_from']]
        from <-  unlist(strsplit(as.character(from),','))
        to <- recode[['recode_to']]
        to <- unlist(strsplit(as.character(to),','))
        value_lab <- recode[['val_lab']]
        value_lab <- unlist(strsplit(as.character(value_lab), ','))
        for (x in 1:length(from)){
          f <- from[x]
          t <- to[x]
          df[new_name][which(df[new_name] == as.integer(f)),] <- as.integer(t)
          if (t == '-99'){
            df[new_name][which(df[new_name] == as.integer(t)),] <- NA
          }
        }
        if (!is.null(value_lab)){
          check <- unique(df[[new_name]])
          check <- check[!is.na(check)]
          vals <- seq_along(value_lab)
          names(vals) <- value_lab
          if (length(check) != length(vals)){
            warning('value lables not same lenght as unique values in data')
            warning(c(vals,check))
          }
          attributes(df[[new_name]])$labels <- vals
        }
      }
    }
  }
  return(df)
}


## this function prepares the data according to the input instructions. It utilises the different functions in this file.
## it selects the variables defined in the scale and singel item column with the prefix given to the function.
## it checks if the items are coded correctly, recodes based on instructions fron the input file and reverses items.
## The fucntion returns a dataframe
prepare_data <- function(org_data,input,prefix,times,missing_vals,recode_9_to_NA = NULL){
  
  #selection of items
  indep_items <- unique(input[single_item][!is.na(input[single_item]) & is.na(input['dependent'])]) # independent variables 
  indep_scales <- unique(input[version][!is.na(input[version]) & is.na(input['dependent'])]) # independet scales 
  dep_scales <- unique(input[version][!is.na(input[version]) & !is.na(input['dependent'])]) # dependent scales
  select_vars <- as.vector(input[['items']][which(!is.na(input[version]) | !is.na(input[single_item]))])
  rev_vars <- select_vars[select_vars %in% as.vector(input[['items']][which(!is.na(input['reverse']))])] # items to reverse
  #append prefix and time
  rev_vars <- sapply(rev_vars, function(x) sprintf('%s%s%s',prefix,times,x))
  select_vars <- sapply(select_vars, function(x) sprintf('%s%s%s',prefix,times,x))
  #removes variables not in dataframe
  select_vars[which(!select_vars %in% colnames(org_data))] <- NA
  rev_vars[which(!rev_vars %in% colnames(org_data))] <- NA
  vars <- as.vector(select_vars);vars <- vars[!is.na(vars)]
  #defines which items to reverse
  rev_list <- as.vector(rev_vars)
  rev_list <- rev_vars[which(!is.na(rev_vars))]
  select_list <- c(id,gender,as.vector(select_vars))
  select_list <- select_list[which(!is.na(select_list))]
  #creates dataframe with only the selected variables
  rdata <- org_data[select_list]
  
  #recodes all values that signifies missing into NA
  for (miss in missing_vals){
    rdata[rdata == miss]<- NA
  }
  rdata[is.na(rdata)]<- NA
  #removes missing value labels
  for (var in colnames(rdata)){
    lab <- attributes(rdata[[var]])$labels
    attributes(rdata[[var]])$labels <- lab[!lab %in% missing_vals]
  }
  
  #if recode9_to_NA is defined this recodes 9 into missing values
  if (!is.null(recode_9_to_NA)){
    for (var in recode_9_to_NA){
      rdata[var][which(rdata[var] == 9),] <- NA
    }  
  }
  
  #reverses items and performes checks, prints warnings if anything dosent pass
  rdata_rev <- check_reverse(rdata,vars,rev_list,missing_vals = missing_vals)
  #performes other checks on items and prints warnings if anything dosent pass
  check_variables(rdata_rev,c(indep_scales,dep_scales),input = input, check_out = sprintf('out/%sscales_log.csv',prefix))
  #recodes values accordning to the input file
  rdata_rev <- recode_input(df = rdata_rev,input = input,missing_vals = missing_vals,prefix = prefix,times = times)
  #turns items with only two repsons categories in to factors
  for (v in colnames(rdata_rev)){
    atr <- attributes(rdata_rev[[v]])
    l <- unique(rdata_rev[[v]])
    if (length(atr$labels) == 2 & length(l[!is.na(l)]) == 2){
      rdata_rev[[v]] <- as_factor(rdata_rev[[v]])
    }
  }
  returnobj <- list(rdata_rev,select_vars,indep_scales,dep_scales,indep_items)
  return(returnobj)
}

## this function renames and recodes the imputation variables so that spss understands how to interpret the dataset.
format_spss_impute <- function(df){
  names(df)[which(names(df) == '.imp')] <- 'Imputation_' # rename .imp to Imputation_
  names(df)[which(names(df) == '.id')] <-  'ID'
  df <- transform(df, Imputation_ = as.numeric(Imputation_), # transforms the two variables to numeric variables
                         ID = as.numeric(ID))
  df$Imputation_<-df$Imputation_-1 # centers the Imputation_ varible to 0
  coln <- colnames(df) # column names
  last <- coln[!coln %in% c('Imputation_','ID')]  # all columnnames except 'Imputation_' and 'ID'
  return(df[c('Imputation_','ID',last)]) #returns dataframe with 'Imputation_' and 'ID' as the first two variables
}
