one_items <- c("(x4_8genhealt > 2)","(x4_8sleepq1 > 2)",'(x4_8moodev4 < )','(x4_8moodev6 < )',
               '(x4_8moodev7 < )','(x4_8absworkinpne > )','(x4_8abswork3nres > )','(mdi < )')

insomnia <- '((x4_8jasleep4 + x4_8jasleep5 + x4_8jasleep6 + x4_8jasleep7) < )'

sss8 <- '((x4_8belly1b + x4_8backac1b + (x4_8armhandac1b + x4_8hipac1b + x4_8legac1b)/3 + x4_8headac1b + x4_8heartbeat1b + x4_8dizzy1b + x4_8tired1b) > )'

#mdi <- c('x4_8moodno1','x4_8moodno2','x4_8moodno3','x4_8moodno4','x4_8moodno5','x4_8moodno6','x4_8moodno7', 
#'x4_8moodno9','x4_8moodno10','x4_8moodno11','x4_8moodno12','x4_8moodno13','x4_8moodno14','x4_8moodno15')

df$mdi <- rowSums(df[c('x4_8moodno1','x4_8moodno2','x4_8moodno3','x4_8moodno6','x4_8moodno7','x4_8moodno12')]) +
  apply(df[, c('x4_8moodno4','x4_8moodno5')], 1, min) + apply(df[, c('x4_8moodno9','x4_8moodno11')], 1, min) +
  apply(df[, c('x4_8moodno14','x4_8moodno15')], 1, min)
utils::View(df[c(mdi,'mdi')])



tempdat <- as.mids(df[c('Imputation_','ID','x4_8genhealt','x4_4copsoq33','x4_5copsoq33','x4_6copsoq33','x4_7copsoq33')])

s <- glm.mids((x4_8genhealt > 2) ~ ( (x4_4copsoq33 > 4) + (x4_5copsoq33 > 4) + (x4_6copsoq33 > 4) + (x4_7copsoq33 > 4)) > 0, data = tempdat, family = 'binomial')
ss <- summary(pool(s))
s <- glm.mids((x4_8genhealt > 2) ~ apply(c(x4_4copsoq33,x4_7copsoq33),1,max), data = tempdat, family = 'binomial')

exp(ss[2,c(1,6,7)])
unique(df$x4_8genhealt)
t <- fulldata[which(fulldata$Imputation_ == 1),]
cor(t[c('x4_4copsoq33','x4_5copsoq33','x4_6copsoq33','x4_7copsoq33')], use = 'complete.obs')
?cor

max(c(5,3))
max(1,2)

tes <- c('roleamb1','roleamb2','roleamb4')
f <- sapply(tes, function(x) sprintf('%s%s%s',prefix,4:8,x))
f[f %in% colnames(df)]
