item_rev <- function(df,rev_vars,missing_vals = NULL){
  for (var in rev_vars){
    var_max <- max(df[var], na.rm = T)
    var_min <- min(df[var], na.rm = T)
    atr <- attributes(df[[var]])$labels
    atr <- atr[which(!atr %in% missing_vals)]
    if (var_max != max(atr)){
      cat(sprintf('%s maximum response not equal to maximum value in label!\nMax repsonse = %s\nMax value    = %s\n',var,var_max,max(atr)))
      }
    if (var_min != min(atr)){
      cat(sprintf('%s minimum response not equal to maximum value in label!\\nMin repsonse = %s\nMin value    = %s\n',var,var_min,min(atr)))
      }
    df[var] <- (var_max+1) - df[var]
    #atr <- attributes(df[[var]])$labels
    rev_values <- rev(as.numeric(atr))
    names(rev_values) <- names(atr)
    attributes(df[[var]])$labels <- rev_values
  }
  return(df)
}

