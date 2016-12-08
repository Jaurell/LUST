prefix <- 'x6_'
source(sprintf('prepare_data_x.R',prefix))
source('impute.R')
source('analyse.R')
source('reprime.R')
source('append variables.')


for (x1 in 1:2){
  for (x2 in 3:4){
    prime_list[sprintf('%s_%s',names(ps[x1]),names(ps[x2]))] <- prod(c(ps[x1],ps[x2]))
    
    #for (x3 in 5:6){
     # for (x4 in 7:8){
        #prime_list[sprintf('%s_%s_%s_%s',names(ps[x1]),names(ps[x2]),names(ps[x3]),names(ps[x4]))] <- prod(c(ps[x1],ps[x2],ps[x3],ps[x4]))
        #for (x5 in 9:10){
        #prime_list[sprintf('%s_%s_%s_%s_%s',names(ps[x1]),names(ps[x2]),names(ps[x3]),names(ps[x4]),names(ps[x5]))] <- prod(c(ps[x1],ps[x2],ps[x3],ps[x4],ps[x5]))
        #}
      #}
    #}
  }
}

prime_list
}

t(matrix(t(ps),2))

seq(1:10,2)
split(ps, seq(1,10,2))

l <- list(ps[1:2],ps[3:4],ps[5:6])


f <- do.call(expand.grid, l)
n <- sapply(f,names)
plist <- list()
for (h in 1:nrow(f)){
  val <- print(prod(f[h,]))
  nam <- paste(n[h,],collapse = '_')
  plist[nam] <- val
}
unlist(plist)







l <- list(ps[1:2],ps[3:4],ps[5:6])



f <- do.call(expand.grid, l)
n <- sapply(f,names)
plist <- list()
for (h in 1:nrow(f)){
  val <- print(prod(f[h,]))
  nam <- paste(n[h,],collapse = '_')
  plist[nam] <- val
}
unlist(plist)

