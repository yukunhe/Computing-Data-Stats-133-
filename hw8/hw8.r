xUnique = 1:5
trueCoeff = c(0, 1, 1)

getData = function(coefs = c(0, 1, 1), xs = 1:5, dupl = 10,
                   sd = 5, seed=2222){
  ### This function creates the artificial data
  set.seed(seed)
  x = rep(xs, each = dupl)
  y = coefs[1] + coefs[2]*x + coefs[3] * x^2 + 
      rnorm(length(x), 0, sd)
  return(data.frame(x, y))
}

### 
genBootY = function(x, y, rep = TRUE){
  ### For each unique x value, take a sample of the
  ### corresponding y values, with replacement.
  ### Return a vector of random y values the same length as y
  ### You can assume that the xs are sorted
  ### Hint use tapply here!
  #new.vector = rep(0,50)
  #for (i in 1:10) {
    #new.vector[i] = sample(y[1:10], 1 , replace = rep)
  #}
  #for (i in 11:20) {
    #new.vector[i] = sample(y[11:20], 1 ,replace = rep)
  #}
  #for (i in 21:30) {
    #new.vector[i] = sample(y[21:30],1,replace = rep)
  #}
  #for (i in 31:40) {
    #new.vector[i] = sample(y[31:40],1, replace = rep)
  #}
  #for (i in 41:50) {
    #new.vector[i] = sample(y[41:50],1, replace = rep)
  #}
  #return (new.vector)
  new.vector = unlist(tapply(y,x,sample,simplify=F,replace=rep))
  names(new.vector) = c()
  return(new.vector)
  }

genBootR = function(fit, err, rep = TRUE){
  ### Sample the errors 
  ### Add the errors to the fit to create a y vector
  ### Return a vector of y values the same length as fit
  ### HINT: It can be easier to sample the indices than the values
  #yvector = rep(0, length(fit))
  #for (i in 1:length(fit)) {
    #yvector[i] = fit[i] + sample(err, 1, replacement = rep)
  #}
  #return (yvector) 
  new_err = sample(err, replace=F)
  yvector = fit + new_err
  return(yvector)
}

fitModel = function(x, y, degree = 1){
  ### use the lm function to fit a line of a quadratic 
  ### e.g. y ~ x or y ~ x + I(x^2)
  ### y and x are numeric vectors of the same length
  ### Return the coefficients as a vector 
  ### HINT: Take a look at the repBoot function to see how to use lm()
  if (degree ==1) {
    coeff = lm(y~x)[1][[1]]
  }
  if (degree ==2) {
    coeff = lm(y~x + I(x^2))[1][[1]]
  }
  return(coeff)
}

oneBoot = function(data, fit = NULL, degree = 1){
  ###  data are either your data (from call to getData)
  ###  OR fit and errors from fit of line to data
  ###  OR fit and errors from fit of quadratic to data
  if (is.null(fit)) {
    new_y=genBootY(data$x, data$y, rep = T)
    coeff=fitModel(data$x, new_y, degree=degree)
  }
  else {
    new_y = genBootR(fit[,1], fit[,2], rep = T)
    coeff = fitModel(data$x, new_y, degree=degree)
  } 
  ### Use fitModel to fit a model to this bootstrap Y 
  return(coeff)
}

repBoot = function(data, B = 1000){
  
  ### Set up the inputs you need for oneBoot, i.e.,
  ### create errors and fits for line and quadratic
  lmfit = lm(data$y~data$x)
  fit = cbind(lmfit$fitted.values, lmfit$residuals)

  ### replicate a call to oneBoot B times
  ### format the return value so that you have a list of
  ### length 4, one for each set of coefficients
  ### each element will contain a data frame with B rows
  ### and one or two columns, depending on whether the 
  ### fit is for a line or a quadratic
  ### Return this list
  coeff = list()
  coeff[[1]] = replicate(B, oneBoot(data, fit = NULL, degree = 1))
  coeff[[2]] = replicate(B, oneBoot(data, fit = NULL, degree = 2))
  coeff[[3]] = replicate(B, oneBoot(data, fit = fit, degree = 1))
  coeff[[4]] = replicate(B, oneBoot(data, fit = fit, degree = 2))
  
  ### Replicate a call to oneBoot B times for 
  ### each of the four conditions
  
  
  ### Format the return value so that you have a list of
  ### length 4, one for each set of coefficients
  ### each element will contain a matrix with B columns
  ### and two or three rows, depending on whether the 
  ### fit is for a line or a quadratic
  ### Return this list
  
  return(coeff)
} 

bootPlot = function(x, y, coeff, trueCoeff){
  ### x and y are the original data
  ### coeff is a matrix from repBoot
  ### trueCoeff contains the true coefficients 
  ### that generated the data
  
  ### Make a scatter plot of data

  ### Add lines or curves for each row in coeff
  ### Use transparency
  ### You should use mapply to construct all 
  ### 1000 of the bootstrapped lines of best fit 
  ### Have a look at ?mapply for details.
  ### This can be done in ggplot2 or base graphics.
  
  ### Use trueCoeff to add true line/curve - 
  ###  Make the true line/curve stand out
  if (nrow(coeff)==3){
    plot(x,y, col="red")
    mapply(function(a,b,c){curve(a*(x^2)+b*x+c, add=T, col="blue")},coeff[3,],coeff[2,],coeff[1,])
    curve(trueCoeff[3]*(x^2)+trueCoeff[2]*x+trueCoeff[1],add=T,col="red",lwd=3)
  }else {
    plot(x,y,col="red")
    mapply(function(intercept,slope){abline(a=intercept,b=slope,col="blue")},coeff[1,],coeff[2,])
    curve(trueCoeff[3]*(x^2)+trueCoeff[2]*x+trueCoeff[1],add=T,col="red",lwd=3)
  }
}

### Run your simulation by calling this function
### This function doesn't need any changing
runSim = function() {
  xUnique = 1:5
  trueCoeff = c(0, 1, 1)
  myData = getData(coefs = trueCoeff, xs = xUnique)
  expt = repBoot(data = myData)
  par(mfrow = c(2, 2))
  for (i in 1:4){
   bootPlot(myData$x, myData$y, 
            coeff = expt[[i]], trueCoeff) 
  }
  return(expt)
}
