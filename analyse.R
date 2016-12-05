analyse <- function(prefix){
  wrkspc <- sprintf('H:/LUST/new_data/%simputed.Rdata',prefix) # location of saved workspace form the imputation step
  load(wrkspc) # load the workspace


library(mice)
## creates a full dataset minus the original to enable calculation of OR via crosstabs
fulldata <- complete(midata, include = F, action = 'long')
outfile <- sprintf('out/%sodds.csv',prefix) 

indeps <- c(indep_scales,indep_items) # use both scales and single items

headers <- 0 # flag for later use inside the loop
for (t in times){ # for each occation
  print(t)
  for (indep in indeps){ # for each independent variable (scale)
    items <- input['items'][input[version] ==  indep | input[single_item] == indep] # items 
    items <- sapply(items[!is.na(items)], function(x) sprintf('%s%s%s',prefix,t,x)) # prefixed items
    items <- items[items %in% colnames(fulldata)] # prefixed items that exist in the data
    if (length(items) > 0){  # if more than 0 items
      preds <- list() # creates a empty list to contain predictors
      pred_df <- NULL  # dataframe of predictors
      pred_df_no_mi <- NULL # dataframe of predictors in the non-imputed dataset
      for (var in items){ 
        if ("factor" %in% attributes(rdata_rev[[var]])$class){ # what to do if the item is a factor
          pred_df_no_mi <- cbind(pred_df_no_mi,rdata_rev[[var]] == attributes(rdata_rev[[var]])$levels[2])
          pred_df <- cbind(pred_df,fulldata[[var]] == attributes(fulldata[[var]])$levels[2])
          preds[var] <- sprintf('(as.numeric(%s)-1)',var) # append predictor list with the item and how to interpret the item
        } else {
          in_var <- substring(var,5) # removes prefix to find input instruction for the item
          atr <- attributes(rdata_rev[[var]])$labels 
          atr <- atr[!atr %in% missing_vals]
          cut_val <- ceiling(max(atr/2)) # defualt cutoff value, the cealing value of the the max value divided by two
          mod <- input[cut_mod][input['items'] == in_var] # modyfier to the cutofvalue from the input instruction
          mod[is.na(mod)] <- 0 
          cut_val <- cut_val + mod 
          preds[var] <- sprintf('(%s > %s)',var,cut_val) # append predictor list with how to interpret the item
          pred_df <- cbind(pred_df,fulldata[var] > cut_val) 
          pred_df_no_mi <- cbind(pred_df_no_mi,rdata_rev[var] > cut_val)
        }
      }
      fx1 <- sprintf('(%s)',paste(preds,collapse = ' + ')) # precursor of the X part in logistic function, it combines all the input from the pred list
      for (dep in dep_scales){ # this function does the same for dependent variables
        items <- input['items'][input[version] ==  dep]
        items <- sapply(items[!is.na(items)], function(x) sprintf('%s%s%s',prefix,t,x))
        items <- items[items %in% colnames(fulldata)]
        if (length(items) > 0){
          last_occ <- sprintf('%s%s%s',prefix,times[length(times)],substring(items,5)) 
          median <- median(rowSums(fulldata[last_occ]),na.rm = T) # sets median, median is the median from the last occation
          median_no_mi <- median(rowSums(rdata_rev[last_occ]),na.rm = T)
          fy_no_mi <- sprintf('as.numeric((%s) > %s)',paste(items,collapse = ' + '),median_no_mi) # Y part of the logistic regression
          fy <- sprintf('as.numeric((%s) > %s)',paste(items,collapse = ' + '),median)
          deps_01_no_mi <- rowSums(rdata_rev[items],na.rm = T)>median_no_mi 
          deps_01 <- rowSums(fulldata[items])>median
          cuts <- length(preds) # cuts is defined as number of independent variables in the scale
          for (cut in 0:cuts){ 
            indep_01 <- rowSums(pred_df) > cut
            indep_01_no_mi <- rowSums(pred_df_no_mi) > cut
            if (length(table(indep_01)) == 2){
              if (length(preds) > 1){ # creates the final X part of the logistic regression
                fx <- sprintf('as.numeric(%s > %s)',fx1,cut)  # 
              }else{
                fx <- sprintf('as.numeric%s',fx1)
              }
              f <- formula(paste(fy,fx,sep = ' ~ ')) # combines the Y and X part into a formula to be used inside the glm function
              f_no_mi <- formula(paste(fy_no_mi,fx,sep = ' ~ ')) # 
              log_reg <- glm.mids(f,data = midata, family = 'binomial') # logistic regression with the f formula for imputed dataset
              log_reg_no_mi <- glm(f_no_mi,data = rdata_rev, family = 'binomial') # logistic regression with the f formula for UNimputed dataset
              s_reg <- summary(pool(log_reg)) # summary of the pooled results of the logistic regression
              OR <- round(exp(s_reg[2,][c(1,6,7)]),3) # calculates odds ratio 
              OR_est <- OR[1]; OR_95_lo <- OR[2]; OR_95_hi <- OR[3] 
              xtab <- table(indep_01,deps_01)/midata$m # crosstabs for imputed data
              n00 <- xtab[1,1];n01 <- xtab[1,2];n10 <- xtab[2,1];n11 <- xtab[2,2]
              crossOR <- round((n00*n11)/(n01*n10),3) # odds ratio based on the crosstabs for the imputed data
              n_proc_dep <- xtab[1,1]/(xtab[1,1]+xtab[2,1])
              n_proc_indep <- round(sum(xtab[2,])/sum(xtab),4)*100
              n_proc_dep <- round(sum(xtab[,2])/sum(xtab),4)*100
              imputed <- 1
              OR_no_mi <- round(exp(cbind(OR = coef(log_reg_no_mi), confint(log_reg_no_mi))),2)[2,] # odds ratio for UNimputed data
              out <- cbind(imputed,prefix,t,dep,indep,cut,OR_est,OR_95_lo,OR_95_hi,crossOR,n00,n01,n10,n11,n_proc_dep,n_proc_indep,substring(fy,11),substring(fx,11)) # object to print to outfile
              out_no_mi <- cbind(0,prefix,t,dep,indep,cut,OR_no_mi[1],OR_no_mi[2],OR_no_mi[3]) # second object to print to outfile, contains results for UNimputed data
              if (headers == 0){ # write file with headers if this is the first row
                write.table(out,file = outfile,col.names = T, row.names = F, sep = ',')
                write.table(out_no_mi, file = outfile,col.names = F,row.names = F,sep = ',',append = T)
                headers <- 1
              }else{
                write.table(out,file=outfile,col.names = F,row.names = F,append = T, sep = ',') # writes out to file
                write.table(out_no_mi, file = outfile,col.names = F,row.names = F,sep = ',',append = T) # writes out_no_mi to file
              }
            }
          }   
        }
      }
    }
  }
}

## makes a density-plot that describes the discrepancy of oddsratio between original data and the imputed dataset
library(ggplot2)
odds <- read.csv(outfile) # reads the oddsfile
odds_diff <- odds['OR_est'][odds['imputed'] == 1]-odds['OR_est'][odds['imputed'] == 0] # calculates difference between imputed and unimputed results for all analysies
odds_diff <- odds_diff[odds_diff > -100 & odds_diff < 100] # excludes rows with extreme values 
odds_m <- round(mean(odds_diff),3) 
odds_sd <- round(sd(odds_diff),3)
outplot <- qplot(odds_diff,geom = 'density') + theme_bw() +  # creates plot
  annotate("text", label = sprintf("m= %s\nsd= %s",odds_m,odds_sd), colour = 'Dark blue', x=-Inf,y=Inf, vjust=2, hjust=-.1)
ggsave(outplot,filename = sprintf('out/%sodds_diff.png',prefix)) # saves plot
}