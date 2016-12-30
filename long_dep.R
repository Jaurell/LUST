prefix <- 'x4_'
reprime_image <- sprintf('H:/LUST/new_data/%sprime.Rdata',prefix)
load(reprime_image)
longexp <- read.csv('longexp.csv')

#sss8 <- c('x4_8belly1b','x4_8backac1b','x4_8headac1b','x4_8heartbeat1b','x4_8dizzy1b','x4_8tired1b','x4_8armhandac1b','x4_8hipac1b','x4_8legac1b')
#mdi <- c('x4_8moodno1','x4_8moodno2','x4_8moodno3','x4_8moodno6','x4_8moodno7','x4_8moodno12','x4_8moodno4','x4_8moodno5','x4_8moodno9','x4_8moodno11','x4_8moodno14','x4_8moodno15')
x4_8insomnia <- c('x4_8jasleep4','x4_8jasleep5','x4_8jasleep6','x4_8jasleep7')
x4_8memory <- c('x4_8conc1','x4_8conc2','x4_8conc3')
x4_8attention <- c('x4_8conc5','x4_8conc6','x4_8conc7')
x4_8mood_slow <- c('x4_8moodno3','x4_8moodno11')
x4_8olbi <- c('x4_8olbi2','x4_8olbi3','x4_8olbi4','x4_8olbi6','x4_8olbi18','x4_8olbi9','x4_8olbi12')

fulldata$x4_8sss8_index <- rowSums(fulldata[c('x4_8belly1b','x4_8backac1b','x4_8headac1b','x4_8heartbeat1b','x4_8dizzy1b','x4_8tired1b')]) +
  apply(fulldata[, c('x4_8armhandac1b','x4_8hipac1b','x4_8legac1b')], 1, mean)

fulldata$x4_8mdi_index <- rowSums(fulldata[c('x4_8moodno1','x4_8moodno2','x4_8moodno3','x4_8moodno6','x4_8moodno7','x4_8moodno12')]) +
  apply(fulldata[, c('x4_8moodno4','x4_8moodno5')], 1, max) + apply(fulldata[, c('x4_8moodno9','x4_8moodno11')], 1, max) +
  apply(fulldata[, c('x4_8moodno14','x4_8moodno15')], 1, max)

fulldata$x4_8_mdi_dsm <- as.numeric((rowSums(fulldata[c('x4_8moodno1','x4_8moodno2','x4_8moodno3','x4_8moodno6','x4_8moodno7','x4_8moodno12')] > 2) + 
  (apply(fulldata[, c('x4_8moodno4','x4_8moodno5')], 1, max) > 2) + (apply(fulldata[, c('x4_8moodno9','x4_8moodno11')], 1, max) > 2) +
  (apply(fulldata[, c('x4_8moodno14','x4_8moodno15')], 1, max) > 2) > 5) & (rowSums(fulldata[c('x4_8moodno1','x4_8moodno2')] > 2) > 0))



#utils::View(df[c('x8_mdi_dsm',mdi)])


expose_variables <- list('x4_8sss8_index',x4_8insomnia,x4_8memory,x4_8attention,x4_8mood_slow,x4_8olbi,'x4_8mdi_index','x4_8moodev4','x4_8moodev6',
             'x4_8moodev7','x4_8absworkinpne','x4_8moodno7','x4_8moodno8','x4_8abswork1nres','x4_8absworkinall',
             'x4_8genhealt','x4_8sleepq1','x4_8_mdi_dsm',x4_8olbi)
names(expose_variables) <- c('x4_8sss8_index','x4_8insomnia','x4_8memory','x4_8attention','x4_8mood_slow','x4_8olbi','x4_8mdi_index','x4_8moodev4','x4_8moodev6',
                             'x4_8moodev7','x4_8absworkinpne','x4_8moodno7','x4_8moodno8','x4_8abswork1nres','x4_8absworkinall',
                             'x4_8genhealt','x4_8sleepq1','x4_8_mdi_dsm','x4_8olbimax')


proc_exposed <- function(dataframe,expose_variables){
  df0 <- fulldata[which(fulldata$Imputation_ == 0),]
  write.table(file = 'longdep_proc.csv', x= t(c('scale','cut','proc','factor')), sep = ',', row.names = F, col.names = F)
  factor_flag <- 0
  for (t in names(expose_variables)){
    items <- expose_variables[[t]]
    if (length(items) < 2){
      atr <- attributes(fulldata[[items]])
      if (!is.null(atr)){
        if ('factor' %in% atr$class){
          tab <- table(fulldata[items])
          range <- 0
          factor_flag <- 1
        }
      }
    }
    if (factor_flag == 0){
      
      ss <- summary(rowSums(df0[items]))
      range <- ss[1]:ss[6]
      for (cut in range){
        tab <- table(rowSums(df0[items]) > cut)
        proc <- 1-tab[1]/sum(tab)
        if (proc < 1 & proc > 0){
          write.table(file = 'longdep_proc.csv',x = t(c(t,cut,round(proc,2),factor_flag)), col.names = F, row.names = F, append = T, sep = ',')
          #print(c(t,cut,round(proc,2)))  
        }
      }
    } else {
      proc <- 1-tab[1]/sum(tab)
      write.table(file = 'longdep_proc.csv',x = t(c(t,cut,round(proc,2),factor_flag)), col.names = F, row.names = F, append = T, sep = ',')
      factor_flag <- 0
    }
  }
}

log_reg <- function(dataframe,expose_variables,longexp,indep,outfile){
  library(mice)
  for (index in names(expose_variables)){
    factor_flag <- 0
    rdata <- as.mids(dataframe[c('Imputation_','ID',expose_variables[[index]],indep)])
    cut <- longexp['cutoff'][longexp['item'] == index]
    deps <- paste(expose_variables[[index]],collapse = ' + ')
    if (length(deps) < 2){
      if ('factor' %in% attributes(dataframe[[index]])){
        factor_flag <- 1
      }
    }
    if (factor_flag == 1){
      y <- deps
    }else{
    y <- sprintf('((%s)>%s)',deps,cut)
    }
    #for (x in 0:3){
      #indep0 <- sprintf('(%s > %s)',indep,x)
      f.txt <- paste(c(y,indep), collapse = ' ~ ')
      f <- formula(f.txt)
      ss <- summary(pool(glm.mids(as.formula(f), data = rdata, family = 'binomial')))
      OR <- exp(ss[2,c(1,6,7)])
      obj <- c(indep,index,OR[1],OR[2],OR[3],f.txt)
      write.table(t(obj), outfile, col.names = F, row.names = F, sep = ',', append = T)
    #}
  }
}


first_last <- sapply(indep_scales, function(x) sprintf('%s_%s_%s',x,c('first','last'),1:4))

#write.table(t(c('indep','dep','OR','OR_lo95','OR_hi95','formula')),outfile, col.names = F, row.names = F, sep = ',')
for (indeps in indep_scales){
  outfile <- sprintf('out/%s.csv',indeps)
  write.table(t(c('indep','dep','OR','OR_lo95','OR_hi95','formula')),outfile, col.names = F, row.names = F, sep = ',')
  for (fix in c('first','last')){
    for (n in 1:4){
      indep <- sprintf('%s_%s_%s',indeps,fix,n)
      print(indep)
      if (indep %in% colnames(fulldata)){
        log_reg(fulldata,expose_variables,longexp,indep,outfile)
        #print(sum(table(fulldata[indep][which(fulldata$Imputation_ == 0),])))
        #print(sum(table(fulldata[indep][which(fulldata$Imputation_ == 1),])))
      }
    }
  }
}

