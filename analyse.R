analyse <- function(prefix){
rm(list = ls())
  wrkspc <- sprintf('H:/LUST/new_data/%simputed.Rdata',prefix)
  load(wrkspc)
## loads workspace saved from the imputation
#load('H:/LUST/new_data/x4_imputed.Rdata')
#load('H:/LUST/new_data/x6_imputed.Rdata')

library(mice)
## creates a full dataset minus the original to enable calculation of OR via crosstabs
fulldata <- complete(midata, include = F, action = 'long')
outfile <- sprintf('out/%sodds.csv',prefix)

indeps <- c(indep_scales,indep_items)

headers <- 0
for (t in times){ # for each occation
  print(t)
  for (indep in indeps){ # for each independent variable (scale)
    items <- input['items'][input[version] ==  indep | input[single_item] == indep]
    items <- sapply(items[!is.na(items)], function(x) sprintf('%s%s%s',prefix,t,x))
    items <- items[items %in% colnames(fulldata)]
    if (length(items) > 0){ 
      preds <- list()
      pred_df <- NULL 
      pred_df_no_mi <- NULL
      for (var in items){
        if ("factor" %in% attributes(rdata_rev[[var]])$class){
          pred_df_no_mi <- cbind(pred_df_no_mi,rdata_rev[[var]] == attributes(rdata_rev[[var]])$levels[2])
          pred_df <- cbind(pred_df,fulldata[[var]] == attributes(fulldata[[var]])$levels[2])
          preds[var] <- sprintf('(as.numeric(%s)-1)',var)
        } else {
          in_var <- substring(var,5)
          atr <- attributes(rdata_rev[[var]])$labels
          atr <- atr[!atr %in% missing_vals]
          cut_val <- ceiling(max(atr/2))
          mod <- input[cut_mod][input['items'] == in_var]
          mod[is.na(mod)] <- 0
          cut_val <- cut_val + mod
          preds[var] <- sprintf('(%s > %s)',var,cut_val)
          pred_df <- cbind(pred_df,fulldata[var] > cut_val)
          pred_df_no_mi <- cbind(pred_df_no_mi,rdata_rev[var] > cut_val)
        }
      }
      fx1 <- sprintf('(%s)',paste(preds,collapse = ' + '))
      for (dep in dep_scales){
        items <- input['items'][input[version] ==  dep]
        items <- sapply(items[!is.na(items)], function(x) sprintf('%s%s%s',prefix,t,x))
        items <- items[items %in% colnames(fulldata)]
        if (length(items) > 0){
          last_occ <- sprintf('%s%s%s',prefix,times[length(times)],substring(items,5))
          median <- median(rowSums(fulldata[last_occ]),na.rm = T)
          median_no_mi <- median(rowSums(rdata_rev[last_occ]),na.rm = T)
          fy_no_mi <- sprintf('as.numeric((%s) > %s)',paste(items,collapse = ' + '),median_no_mi)
          fy <- sprintf('as.numeric((%s) > %s)',paste(items,collapse = ' + '),median)
          deps_01_no_mi <- rowSums(rdata_rev[items],na.rm = T)>median_no_mi
          deps_01 <- rowSums(fulldata[items])>median
          cuts <- length(preds)
          for (cut in 0:cuts){
            indep_01 <- rowSums(pred_df) > cut
            indep_01_no_mi <- rowSums(pred_df_no_mi) > cut
            if (length(table(indep_01)) == 2){
              if (length(preds) > 1){
                fx <- sprintf('as.numeric(%s > %s)',fx1,cut)
              }else{
                fx <- sprintf('as.numeric%s',fx1)
              }
              f <- formula(paste(fy,fx,sep = ' ~ '))
              f_no_mi <- formula(paste(fy_no_mi,fx,sep = ' ~ '))
              log_reg <- glm.mids(f,data = midata, family = 'binomial')
              log_reg_no_mi <- glm(f_no_mi,data = rdata_rev, family = 'binomial')
              s_reg <- summary(pool(log_reg))
              OR <- round(exp(s_reg[2,][c(1,6,7)]),3)
              OR_est <- OR[1]; OR_95_lo <- OR[2]; OR_95_hi <- OR[3]
              xtab <- table(indep_01,deps_01)/midata$m
              n00 <- xtab[1,1];n01 <- xtab[1,2];n10 <- xtab[2,1];n11 <- xtab[2,2]
              crossOR <- round((n00*n11)/(n01*n10),3)
              n_proc_dep <- xtab[1,1]/(xtab[1,1]+xtab[2,1])
              n_proc_indep <- round(sum(xtab[2,])/sum(xtab),4)*100
              n_proc_dep <- round(sum(xtab[,2])/sum(xtab),4)*100
              imputed <- 1
              OR_no_mi <- round(exp(cbind(OR = coef(log_reg_no_mi), confint(log_reg_no_mi))),2)[2,]
              out <- cbind(imputed,prefix,t,dep,indep,cut,OR_est,OR_95_lo,OR_95_hi,crossOR,n00,n01,n10,n11,n_proc_dep,n_proc_indep,substring(fy,11),substring(fx,11))
              out_no_mi <- cbind(0,prefix,t,dep,indep,cut,OR_no_mi[1],OR_no_mi[2],OR_no_mi[3])
              if (headers == 0){
                write.table(out,file = outfile,col.names = T, row.names = F, sep = ',')
                write.table(out_no_mi, file = outfile,col.names = F,row.names = F,sep = ',',append = T)
                headers <- 1
              }else{
                write.table(out,file=outfile,col.names = F,row.names = F,append = T, sep = ',')
                write.table(out_no_mi, file = outfile,col.names = F,row.names = F,sep = ',',append = T)
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
odds <- read.csv(outfile)
odds_diff <- odds['OR_est'][odds['imputed'] == 1]-odds['OR_est'][odds['imputed'] == 0]
odds_diff <- odds_diff[odds_diff > -100 & odds_diff < 100]
odds_m <- round(mean(odds_diff),3)
odds_sd <- round(sd(odds_diff),3)
outplot <- qplot(odds_diff,geom = 'density') + theme_bw() + 
  annotate("text", label = sprintf("m= %s\nsd= %s",odds_m,odds_sd), colour = 'Dark blue', x=-Inf,y=Inf, vjust=2, hjust=-.1)
ggsave(outplot,filename = sprintf('out/%sodds_diff.png',prefix))
outplot



######################################
}