##Checks that max and min responses are equal to max and min value labels (excluding missing value labels)
## if variable is in rev_vars it reverses the item and the value label
check_reverse <- function(df,vars,rev_vars,missing_vals = NULL){
  file.create('check_log.txt')
  for (var in vars){
    var_max <- max(df[var], na.rm = T)
    var_min <- min(df[var], na.rm = T)
    atr <- attributes(df[[var]])$labels
    atr <- atr[which(!atr %in% missing_vals)]
    if (is.null(atr)){
      warning('No labels for this variable\nGetting max value from input')
      temp_var <- substring(var,5)
      max_val <- input['max_val'][which(input['items'] == temp_var),]
      min_val <- 1
    } else {
      max_val <- max(atr)
      min_val <- min(atr)
    }
    if (!is.na(max_val)){
      if (var_max != max_val){
        warning(sprintf('%s maximum response not equal to maximum value in label!\nMax repsonse = %s\nMax value    = %s\n',var,var_max,max(atr)))
      }
    } else {
      mes <- sprintf('No maximum value in set for:\n%s\n',var)
      warning(mes)
      write(mes, file = 'check_log.txt', append = T)
    }
    if (var_min != min_val){
      warning(sprintf('%s minimum response not equal to maximum value in label!\\nMin repsonse = %s\nMin value    = %s\n',var,var_min,min(atr)))
    }
    if (var %in% rev_vars){
      df[var] <- (max_val+1) - df[var]
      rev_values <- rev(as.numeric(atr))
      names(rev_values) <- names(atr)
      attributes(df[[var]])$labels <- rev_values
    }
    
  }
  return(df)
}

## checks if scales from items have sufficent chronbacs alpha, mean inter item correlation and item total correlation. 
## if only two items it checks the correlation instead. Results are printet to a logfile (check_out)
## it also checks that all the correlations over time are positive for each item.
check_variables <- function(df,scales,input, itc_low = .3,miic_low = .3, check_out){
  write.table(t(c('scale','comment','alpha','miic','min_itc','max_itc','warning','items')),file=check_out,row.names = F, sep = ',', col.names = F)
  for (scale in scales){
    items <- input['items'][input[version] == scale]
    items <- items[!is.na(items)]
    items <- sapply(items, function(x) sprintf('%s%s%s',prefix,times,x))
    items[which(!items %in% colnames(org_data))] <- NA
    for (x in 1:nrow(items)){
      row_check <- items[x,][!is.na(items[x,])]
      if (length(row_check) > 1){
        if (length(row_check) > 2){
          a <- psych::alpha(as.data.frame(df[row_check]),na.rm = T)
          raw_alpha <- a$total$raw_alpha
          miic <- a$total$average_r
          max_itc <- max(a$item.stats$r.drop)
          min_itc <- min(a$item.stats$r.drop)
          message <- ''
        }else{
          c <- round(cor(df[row_check], use = 'complete.obs')[1,2],3)
          raw_alpha <- c
          miic <- c
          max_itc <- c
          min_itc <- c
          message <- 'only correlations'
        }
        warn <- 0
        neg <- 0
        
        if (min_itc < itc_low){
          #warning(sprintf('%s: ITC is less than the cutoff',scale))
          warn <- warn + 1
        } 
        if (miic  < miic_low){
          #warning(sprintf('%s: MIIC is less than the cutoff',scale))
          warn <- warn + 1
        }
        out <- c(scale,message,round(c(raw_alpha,miic,min_itc,max_itc),3),warn,row_check)
        write.table(t(out),file = check_out, append = T, row.names = F, col.names = F,sep = ',')
      }
    }
    for (y in 1:ncol(items)){
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
  instr <- input[c('items','recode_from','recode_to','val_lab')][which(rowSums(is.na(input[c('recode_from','recode_to')])) == 0),]
  for (row in instr[['items']]){
    vars <- sapply(times, function(x) sprintf('%s%s%s',prefix,x,row))
    for (v in vars){
      if (v %in% colnames(df)){
        ins <- instr[which(instr['items'] == row),]
        from <- ins[['recode_from']]
        from <-  unlist(strsplit(as.character(from),','))
        to <- ins[['recode_to']]
        to <- unlist(strsplit(as.character(to),','))
        value_lab <- ins[['val_lab']]
        value_lab <- unlist(strsplit(as.character(value_lab), ','))
        for (x in 1:length(from)){
          f <- from[x]
          t <- to[x]
          df[v][which(df[v] == as.integer(f)),] <- as.integer(t)
          if (t == '-99'){
            df[v][which(df[v] == as.integer(t)),] <- NA
          }
        }
        if (!is.null(value_lab)){
          check <- unique(df[[v]])
          check <- check[!is.na(check)]
          vals <- seq_along(value_lab)
          names(vals) <- value_lab
          if (length(check) != length(vals)){
            warning('value lables not same lenght as unique values in data')
            print(vals)
            print(check)
          }
          attributes(df[[v]])$labels <- vals
        }
      }
    }
  }
  return(df)
}

## renames items, recodes and changes value labels according to the input file. This function needs to be called first for the original dataset
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
  indep_items <- unique(input[single_item][!is.na(input[single_item]) & is.na(input['dependent'])])
  indep_scales <- unique(input[version][!is.na(input[version]) & is.na(input['dependent'])])
  dep_scales <- unique(input[version][!is.na(input[version]) & !is.na(input['dependent'])])
  select_vars <- as.vector(input[['items']][which(!is.na(input[version]) | !is.na(input[single_item]))])
  rev_vars <- select_vars[select_vars %in% as.vector(input[['items']][which(!is.na(input['reverse']))])]
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

format_spss_impute <- function(df){
  names(df)[which(names(df) == '.imp')] <- 'Imputation_'
  names(df)[which(names(df) == '.id')] <-  'ID'
  df <- transform(df, Imputation_ = as.numeric(Imputation_),
                         ID = as.numeric(ID))
  df$Imputation_<-df$Imputation_-1
  coln <- colnames(df)
  last <- coln[!coln %in% c('Imputation_','ID')]
  return(df[c('Imputation_','ID',last)])
}
