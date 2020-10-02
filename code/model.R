your_function <- function(y_t,t,maxcap) {
  
  #restrict data size if input data is too large
  L = length(t)
  if(L > 2000){
    y_t = y_t[(L-1999):L]
    t = t[(L-1999):L]
  }
  
  #convert letters to NA
  checkup = toupper(y_t)
  checklow = tolower(y_t)
  clean_index = which(checkup == checklow)
  y_t[!clean_index] = NA
  y_t = as.numeric(y_t)
  
  #last NAs
  if(is.na(y_t[length(y_t)])){
    numberoflastNAs = min(which(is.na(rev(y_t)) == F)) - 1
  } else{numberoflastNAs = 0}
  
  #prepare data
  y_t2 = as.numeric(y_t[!is.na(y_t)])
  t2 = 1:sum(!is.na(y_t))
  t_lostforletterNA = length(t) - length(clean_index) 
  
  #test linearity
  mylm = function(y_t2 = y_t2, t2 = t2, L = L, maxcap = maxcap){
    #check whether there is a sudden drop
    cut_index = 1
    if (sum(diff(y_t2) < -0.4 * maxcap) > 0) {
      cut_index = max(which(diff(y_t2) < -0.4 * maxcap)) + 1
      len = length(y_t2)
      y_t3 = y_t2[cut_index : len]; t3 = t2[cut_index : len]
    } else{
      y_t3 = y_t2;  t3 = t2
    }
    #fit linear model and make predictions
    model = lm(y_t3 ~ t3)
    intercept = as.numeric(coef(model)[1])
    slop = as.numeric(coef(model)[2])
    n.ahead = floor((maxcap - intercept)/slop - L)
    return(c(n.ahead - 1 + L, summary(model)$r.squared))
  }
  
  Result.lr = tryCatch(mylm(y_t2 = y_t2, t2 = t2, L = L,
                            maxcap = maxcap), error=function(e) "error")
  #use linear model if data has significant linearity
  if(Result.lr[1] != "error" && Result.lr[2] > 0.999){
    return(max(Result.lr[1], L))
  }
  #if not then build ARIMA model
  else{
    myarima = function(y_t2 = y_t2, t2 = t2, L = L, maxcap = maxcap){
      y_t.ts = ts(y_t2, start = 1, frequency = 1)
      #split train and test set
      test_length = ceiling(0.33*length(y_t.ts))
      train = y_t.ts[1:(length(y_t.ts)-test_length)]
      test = y_t.ts[(length(y_t.ts)-(test_length-1)):length(y_t.ts)]
      ferror = c()
      ar = c()
      ma = c()
      diff = c()
      #fit several models and use MSE to determine orders of ARIMA model
      for (i in 0:3) {
        for (j in 0:3) {
          for (k in 1:2) {
            model = arima(train, order = c(i,k,j), method = "ML")
            prediction = predict(model, n.ahead = test_length)$pred
            err = sum((test-prediction)^2)/test_length
            ferror = c(ferror, err)
            ar = c(ar, i)
            ma = c(ma, j)
            diff = c(diff, k)
          }
        }
      }
      result = data.frame(Forecast_Error = ferror, AR = ar, MA = ma, Diff = diff)
      result = head(result[order(result[,1],decreasing=FALSE),])
      i = result$AR[1]; j =result$MA[1]; k = result$Diff[1]
      bestfit = arima(as.ts(y_t2), order = c(i, k, j))
      
      #make model predictions
      #fix a problem that predictions might be similar to white noise
      if (sum(diff(predict(bestfit, n.ahead = 10)$pred[5:10])>maxcap/L^2)==0 && 
          max(predict(bestfit, n.ahead = 10)$pred) > 0.8*maxcap) {
        return(L)
      }
      
      if (sum(diff(predict(bestfit, n.ahead = 10)$pred[5:10])>maxcap/L^2)==0) {
        return("error")
      }
      
      #make predictions through ARIMA model
      n = 10
      for (q in 1:100) {
        pv = predict(bestfit, n.ahead = n*q)$pred
        pvindex = pv > 0.95 * maxcap
        if (sum(pvindex) > 0){
          n = which(pvindex == TRUE)[1]
          break
        }
      }
      return(n + L - numberoflastNAs) #return t in advance 5
    }
    
    #check suitability of ARIMA model
    Result = tryCatch(myarima(y_t2 = y_t2, t2 = t2, L = L, 
                              maxcap = maxcap), error=function(e) "error")
    if(Result != "error"){
      return(max(Result, L))
    }
    #when ARIMA model is not working
    else{
      #use linear model if ARIMA fails
      if(Result.lr != "error"){
        Result.lr = tryCatch(mylm(y_t2 = y_t2, t2 = t2, L = L,
                                maxcap = 0.9*maxcap),error=function(e) "error")
        return(max(Result.lr[1], L))
      }
      #return max(t) if both methods fail
      else{
        return(L)
      }
    }
  }
}
