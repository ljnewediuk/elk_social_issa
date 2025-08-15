

moving_window <- function(df, m, n, by){   
  
  for(k in m:n){
    
    df2 <- df[JDate >= k & JDate <= (k + 7)] 
    
    DI = df2[, get_sri(.SD, 'IDYr', 
                 by = by)]
    DI$JDate = k
    out[[k]] = DI
  }
  return(rbindlist(out))
}