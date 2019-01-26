###################################################### HW2 SHUO JIN ########################################################

library(XML)   # package ‘XML’ was built under R version 3.4.4 
ubase = "http://www.cherryblossom.org/"

femaleURLs = 
  c("results/2009/09cucb-F.htm","results/2010/2010cucb10m-f.htm", 
    "results/2011/2011cucb10m-f.htm","results/2012/2012cucb10m-f.htm")

urls = paste(ubase, femaleURLs, sep = "")

extractResTable =
  #
  # Retrieve data from web site, 
  # find the preformatted text,
  # and write lines or return as a character vector.
  #
  function(url = "http://www.cherryblossom.org/results/2009/09cucb-F.htm",
           year = 2009, sex = "female", file = NULL)
  {
    doc = htmlParse(url)
    # Get preformatted text from <pre> elefemalets
    pres = getNodeSet(doc, "//pre")
    txt = xmlValue(pres[[1]])
    els = strsplit(txt, "\r\n")[[1]]   
    
    if (is.null(file)) return(els)
    # Write the lines as a text file.
    writeLines(els, con = file)
  }

years = 2009:2012

femaleTables = mapply(extractResTable, url = urls, year = years)
names(femaleTables) = years
sapply(femaleTables, length)

save(femaleTables, file = "CBFemaleTextTables_0912.rda")

femaleTables[[1]]
femaleTables[[1]][[4]]
femaleTables[[1]][1:4]

#################### Age Box Plot #####################################

extractVariables = 
  function(file, varNames =c("name", "home", "ag", "gun",
                             "net", "time"))
  {
    
    # Find the index of the row with =s
    eqIndex = grep("^===", file)
    # Extract the two key rows and the data 
    spacerRow = file[eqIndex] 
    headerRow = tolower(file[ eqIndex - 1 ])
    body = file[ -(1 : eqIndex) ]
    # Remove footnotes and blank rows
    footnotes = grep("^[[:blank:]]*(\\*|\\#)", body)
    if ( length(footnotes) > 0 ) body = body[ -footnotes ]
    blanks = grep("^[[:blank:]]*$", body)
    if (length(blanks) > 0 ) body = body[ -blanks ]
    
    
    # Obtain the starting and ending positions of variables   
    searchLocs = findColLocs(spacerRow)
    locCols = selectCols(varNames, headerRow, searchLocs)
    
    Values = mapply(substr, list(body), start = locCols[1, ], 
                    stop = locCols[2, ])
    colnames(Values) = varNames
    
    return(Values)
  }

femaleResMat = lapply(femaleTables, extractVariables)
length(femaleResMat)
sapply(femaleResMat, nrow)

age = sapply(femaleResMat,
             function(x) as.numeric(x[ , 'ag']))
boxplot(age, ylab = "Age", xlab = "Year")

########################### Scatter plot for Run Time vs Age ##############################

createDF = function(Res, year, sex) 
{
  # Determine which time to use
  if ( !is.na(Res[1, 'net']) ) useTime = Res[ , 'net']
  else if ( !is.na(Res[1, 'gun']) ) useTime = Res[ , 'gun']
  else useTime = Res[ , 'time']
  
  # Remove # and * and blanks from time
  useTime = gsub("[#\\*[:blank:]]", "", useTime)
  runTime = convertTime(useTime[ useTime != "" ])
  
  # Drop rows with no time
  Res = Res[ useTime != "", ]
  
  Results = data.frame(year = rep(year, nrow(Res)),
                       sex = rep(sex, nrow(Res)),
                       name = Res[ , 'name'], home = Res[ , 'home'],
                       age = as.numeric(Res[, 'ag']), 
                       runTime = runTime,
                       stringsAsFactors = FALSE)
  invisible(Results)
}

femaleDF = mapply(createDF, femaleResMat, year = 2009:2012,
                  sex = rep("F", 4), SIMPLIFY = FALSE)

sapply(femaleDF, function(x) sum(is.na(x$runTime)))

separatorIdx = grep("^===", femaleTables[["2009"]])
separatorRow = femaleTables[['2009']][separatorIdx]
separatorRowX = paste(substring(separatorRow, 1, 63), " ", 
                      substring(separatorRow, 65, nchar(separatorRow)), 
                      sep = "")
femaleTables[['2009']][separatorIdx] = separatorRowX

femaleResMat = sapply(femaleTables, extractVariables)
femaleDF = mapply(createDF, femaleResMat, year = 2009:2012,
                  sex = rep("F", 4), SIMPLIFY = FALSE)

sapply(femaleDF, function(x) sum(is.na(x$runTime)))

boxplot(sapply(femaleDF, function(x) x$runTime), 
        xlab = "Year", ylab = "Run Time (min)")


cbFemale = do.call(rbind, femaleDF)
save(cbFemale, file = "cbFemale.rda")

dim(cbFemale)

#load("cbFemale.rda")


plot(runTime ~ age, data = cbFemale, ylim = c(40, 180),
     xlab = "Age (years)", ylab = "Run Time (minutes)")


library(RColorBrewer)
ls("package:RColorBrewer")

display.brewer.all()

Purples8 = brewer.pal(9, "Purples")[8]
Purples8

Purples8A = paste(Purples8, "14", sep = "")


plot(runTime ~ jitter(age, amount = 0.5), 
     data = cbFemale, 
     pch = 19,cex = 0.2, col = Purples8A,
     ylim = c(45, 165), xlim = c(15, 85),
     xlab = "Age (years)", ylab = "Run Time (minutes)")

smoothScatter(y = cbFemale$runTime, x = cbFemale$age,
              ylim = c(40, 165), xlim = c(15, 85),
              xlab = "Age (years)", ylab = "Run Time (minutes)")



cbFemaleSub = cbFemale[cbFemale$runTime > 30 &
                         !is.na(cbFemale$age) & cbFemale$age > 15, ]

############### Side-by-Side Boxplots of Female Runners’ Run Time vs. Age ####################

ageCat = cut(cbFemaleSub$age, breaks = c(seq(15, 75, 10), 80))
table(ageCat)

plot(cbFemaleSub$runTime ~ ageCat, 
     xlab = "Age (years)", ylab = "Run Time (minutes)")

############### Residual Plot from Fitting a Simple Linear Model of Performance to Age  ####################

lmAge = lm(runTime ~ age, data = cbFemaleSub)

lmAge$coefficients

summary(lmAge)

class(lmAge)

smoothScatter(x = cbFemaleSub$age, y = lmAge$residuals,
              xlab = "Age (years)", ylab = "Residuals")
abline(h = 0, col = "purple", lwd = 3)

resid.lo = loess(resids ~ age, 
                 data = data.frame(resids = residuals(lmAge),
                                   age = cbFemaleSub$age))

age20to80 = 20:80
age20to80

resid.lo.pr = 
  predict(resid.lo, newdata = data.frame(age = age20to80))

lines(x = age20to80, y = resid.lo.pr, col = "green", lwd = 2)

femaleRes.lo = loess(runTime ~ age, cbFemaleSub)

femaleRes.lo.pr = predict(femaleRes.lo, data.frame(age = age20to80))

over50 = pmax(0, cbFemaleSub$age - 50)

lmOver50 = lm(runTime ~ age + over50, data = cbFemaleSub)

summary(lmOver50)

decades = seq(30, 60, by = 10)
overAge = lapply(decades, 
                 function(x) pmax(0, (cbFemaleSub$age - x)))
names(overAge) = paste("over", decades, sep = "")
overAge = as.data.frame(overAge)
tail(overAge)

############## Piecewise Linear and Loess Curves Fitted to Run Time vs. Age ##########


lmPiecewise = lm(runTime ~ . , 
                 data = cbind(cbFemaleSub[, c("runTime", "age")], 
                              overAge))

summary(lmPiecewise)

overAge20 = lapply(decades, function(x) pmax(0, (age20to80 - x)))
names(overAge20) = paste("over", decades, sep = "")
overAgeDF = cbind(age = data.frame(age = age20to80), overAge20)

tail(overAgeDF)

predPiecewise = predict(lmPiecewise, overAgeDF)

plot(predPiecewise ~ age20to80,
     type = "l", col = "purple", lwd = 3,
     xlab = "Age (years)", ylab = "Run Time Prediction")

lines(x = age20to80, y = femaleRes.lo.pr, 
      col = "green", lty = 2, lwd = 3)
legend("topleft", col = c("purple", "green"),
       lty = c(1, 2), lwd= 3,
       legend = c("Piecewise Linear", "Loess Curve"), bty = "n")


plot(predPiecewise ~ age20to80,
     type = "l", col = "#984ea3", lwd = 3,
     #   type = "l", col = "purple", lwd = 2,
     xlab = "Age (years)", ylab = "Run Time Prediction")

lines(x = age20to80, y = femaleRes.lo.pr, col = "#4daf4a", lwd = 3, lty = 2)
legend("topleft", col = c("#984ea3", "#4daf4a"), lty = c(1, 2), lwd = 3,
       legend = c("Piecewise Linear", "Loess Curve"), bty = "n")

######### Line Plot of the Number of Female Runners by Year ####################

numRunners = with(cbFemale, tapply(runTime, year, length))
plot(numRunners ~ names(numRunners), type="l", lwd = 2,
     xlab = "Years", ylab = "Number of Runners")

######### Density Curves for the Age of Female Runners for 2 years# ############
summary(cbFemaleSub$runTime[cbFemaleSub$year == 2009])

summary(cbFemaleSub$runTime[cbFemaleSub$year == 2012])

age2009 = cbFemaleSub[ cbFemaleSub$year == 2009, "age" ]
age2012 = cbFemaleSub[ cbFemaleSub$year == 2012, "age" ]

plot(density(age2009, na.rm = TRUE), 
     ylim = c(0, 0.07), col = "purple",
     lwd = 3,  xlab = "Age (years)",  main = "")
lines(density(age2012, na.rm = TRUE), 
      lwd = 3, lty = 2, col="green")
legend("topleft", col = c("purple", "green"), lty= 1:2, lwd = 3,
       legend = c("2009", "2012"), bty = "n")

############## Loess Curves Fit to Performance for 2 years ####################
qqplot(age2009, age2012, pch = 19, cex = 0.5, 
       ylim = c(10,90), xlim = c(10,90), 
       xlab = "Age in 2009 Race",
       ylab = "Age in 2012 Race", 
       main = "Quantile-quantile plot of female runner's age")
abline(a =0, b = 1, col="red", lwd = 2)

mR.lo09 = loess(runTime ~ age, cbFemaleSub[ cbFemaleSub$year == 2009,])
mR.lo.pr09 = predict(mR.lo09, data.frame(age = age20to80))

mR.lo12 = loess(runTime ~ age, cbFemaleSub[ cbFemaleSub$year == 2012,])
mR.lo.pr12 = predict(mR.lo12, data.frame(age = age20to80))

plot(mR.lo.pr09 ~ age20to80,
     type = "l", col = "purple", lwd = 3,
     xlab = "Age (years)", ylab = "Fitted Run Time (minutes)")


#################### Difference between Loess Curves of the predicted run time for 2 years ############
lines(x = age20to80, y = mR.lo.pr12,
      col = "green", lty = 2, lwd = 3)

legend("topleft", col = c("purple", "green"), lty = 1:2, lwd = 3,
       legend = c("2009", "2012"), bty = "n")


plot(mR.lo.pr09 ~ age20to80,
     type = "l", col = "#984ea3", lwd = 3,
     xlab = "Age (years)", ylab = "Prediction (minutes)")  
lines(x = age20to80, y = mR.lo.pr12, col="#4daf4a", lty = 2, lwd = 3) 
legend("topleft", col = c("#984ea3", "#4daf4a"), lty = 1:2, lwd = 3,
       legend = c("2009", "2012"), bty = "n")
gap12 = mR.lo.pr12 - mR.lo.pr09

plot(gap12 ~ age20to80, type = "l" , xlab = "Age (years)", 
     ylab = "Difference in Fitted Curves (minutes)", lwd = 2)

######################### Male ###################################################################
library(XML)   # package ‘XML’ was built under R version 3.4.4 
ubase = "http://www.cherryblossom.org/"

maleURLs = 
  c("results/2009/09cucb-M.htm",
    "results/2010/2010cucb10m-m.htm", 
    "results/2011/2011cucb10m-m.htm",
    "results/2012/2012cucb10m-m.htm")

urls = paste(ubase, maleURLs, sep = "")

extractResTable =
  #
  # Retrieve data from web site, 
  # find the preformatted text,
  # and write lines or return as a character vector.
  #
  function(url = "http://www.cherryblossom.org/results/2009/09cucb-M.htm",
           year = 2009, sex = "male", file = NULL)
  {
    doc = htmlParse(url)
    # Get preformatted text from <pre> elefemalets
    pres = getNodeSet(doc, "//pre")
    txt = xmlValue(pres[[1]])
    els = strsplit(txt, "\r\n")[[1]]
    
    if (year == 2009 & sex == "male") {
      # Get preformatted text from <div class="Section1"> element
      # Each line of results is in a <pre> element
      div1 = getNodeSet(doc, "//div[@class='Section1']")
      pres = getNodeSet(div1[[1]], "//pre")
      els = sapply(pres, xmlValue)
    }
    
    if (is.null(file)) return(els)
    # Write the lines as a text file.
    writeLines(els, con = file)
  }

years = 2009:2012

maleTables = mapply(extractResTable, url = urls, year = years)
names(maleTables) = years
sapply(maleTables, length)

save(maleTables, file = "CBMaleTextTables_0912.rda")

maleTables[[1]]
maleTables[[1]][[4]]
maleTables[[1]][1:4]

#################### Age Box Plot #####################################

extractVariables = 
  function(file, varNames =c("name", "home", "ag", "gun",
                             "net", "time"))
  {
    
    # Find the index of the row with =s
    eqIndex = grep("^===", file)
    # Extract the two key rows and the data 
    spacerRow = file[eqIndex] 
    headerRow = tolower(file[ eqIndex - 1 ])
    body = file[ -(1 : eqIndex) ]
    # Remove footnotes and blank rows
    footnotes = grep("^[[:blank:]]*(\\*|\\#)", body)
    if ( length(footnotes) > 0 ) body = body[ -footnotes ]
    blanks = grep("^[[:blank:]]*$", body)
    if (length(blanks) > 0 ) body = body[ -blanks ]
    
    
    # Obtain the starting and ending positions of variables   
    searchLocs = findColLocs(spacerRow)
    locCols = selectCols(varNames, headerRow, searchLocs)
    
    Values = mapply(substr, list(body), start = locCols[1, ], 
                    stop = locCols[2, ])
    colnames(Values) = varNames
    
    return(Values)
  }

maleResMat = lapply(maleTables, extractVariables)
length(maleResMat)
sapply(maleResMat, nrow)

age = sapply(maleResMat,
             function(x) as.numeric(x[ , 'ag']))
boxplot(age, ylab = "Age", xlab = "Year")

########################### Scatter plot for Run Time vs Age ##############################

createDF = function(Res, year, sex) 
{
  # Determine which time to use
  if ( !is.na(Res[1, 'net']) ) useTime = Res[ , 'net']
  else if ( !is.na(Res[1, 'gun']) ) useTime = Res[ , 'gun']
  else useTime = Res[ , 'time']
  
  # Remove # and * and blanks from time
  useTime = gsub("[#\\*[:blank:]]", "", useTime)
  runTime = convertTime(useTime[ useTime != "" ])
  
  # Drop rows with no time
  Res = Res[ useTime != "", ]
  
  Results = data.frame(year = rep(year, nrow(Res)),
                       sex = rep(sex, nrow(Res)),
                       name = Res[ , 'name'], home = Res[ , 'home'],
                       age = as.numeric(Res[, 'ag']), 
                       runTime = runTime,
                       stringsAsFactors = FALSE)
  invisible(Results)
}

maleDF = mapply(createDF, maleResMat, year = 2009:2012,
                  sex = rep("F", 4), SIMPLIFY = FALSE)

sapply(maleDF, function(x) sum(is.na(x$runTime)))

separatorIdx = grep("^===", maleTables[["2009"]])
separatorRow = maleTables[['2009']][separatorIdx]
separatorRowX = paste(substring(separatorRow, 1, 63), " ", 
                      substring(separatorRow, 65, nchar(separatorRow)), 
                      sep = "")
maleTables[['2009']][separatorIdx] = separatorRowX

maleResMat = sapply(maleTables, extractVariables)
maleDF = mapply(createDF, maleResMat, year = 2009:2012,
                  sex = rep("F", 4), SIMPLIFY = FALSE)

sapply(maleDF, function(x) sum(is.na(x$runTime)))

boxplot(sapply(maleDF, function(x) x$runTime), 
        xlab = "Year", ylab = "Run Time (min)")


cbMale = do.call(rbind, maleDF)
save(cbMale, file = "cbMmale.rda")

dim(cbMale)

#load("cbFemale.rda")


plot(runTime ~ age, data = cbMale, ylim = c(40, 180),
     xlab = "Age (years)", ylab = "Run Time (minutes)")


library(RColorBrewer)
ls("package:RColorBrewer")

display.brewer.all()

Purples8 = brewer.pal(9, "Purples")[8]
Purples8

Purples8A = paste(Purples8, "14", sep = "")


plot(runTime ~ jitter(age, amount = 0.5), 
     data = cbMale, 
     pch = 19,cex = 0.2, col = Purples8A,
     ylim = c(45, 165), xlim = c(15, 85),
     xlab = "Age (years)", ylab = "Run Time (minutes)")

smoothScatter(y = cbMale$runTime, x = cbMale$age,
              ylim = c(40, 165), xlim = c(15, 85),
              xlab = "Age (years)", ylab = "Run Time (minutes)")



cbMaleSub = cbMale[cbMale$runTime > 30 &
                         !is.na(cbMale$age) & cbMale$age > 15, ]

############### Side-by-Side Boxplots of Female Runners’ Run Time vs. Age ####################

ageCat = cut(cbMaleSub$age, breaks = c(seq(15, 75, 10), 80))
table(ageCat)

plot(cbMaleSub$runTime ~ ageCat, 
     xlab = "Age (years)", ylab = "Run Time (minutes)")

############### Residual Plot from Fitting a Simple Linear Model of Performance to Age  ####################

lmAge = lm(runTime ~ age, data = cbMaleSub)

lmAge$coefficients

summary(lmAge)

class(lmAge)

smoothScatter(x = cbMaleSub$age, y = lmAge$residuals,
              xlab = "Age (years)", ylab = "Residuals")
abline(h = 0, col = "purple", lwd = 3)

resid.lo = loess(resids ~ age, 
                 data = data.frame(resids = residuals(lmAge),
                                   age = cbMaleSub$age))

age20to80 = 20:80
age20to80

resid.lo.pr = 
  predict(resid.lo, newdata = data.frame(age = age20to80))

lines(x = age20to80, y = resid.lo.pr, col = "green", lwd = 2)

MaleRes.lo = loess(runTime ~ age, cbMaleSub)

MaleRes.lo.pr = predict(MaleRes.lo, data.frame(age = age20to80))

over50 = pmax(0, cbMaleSub$age - 50)

lmOver50 = lm(runTime ~ age + over50, data = cbMaleSub)

summary(lmOver50)

decades = seq(30, 60, by = 10)
overAge = lapply(decades, 
                 function(x) pmax(0, (cbMaleSub$age - x)))
names(overAge) = paste("over", decades, sep = "")
overAge = as.data.frame(overAge)
tail(overAge)

############## Piecewise Linear and Loess Curves Fitted to Run Time vs. Age ##########


lmPiecewise = lm(runTime ~ . , 
                 data = cbind(cbMaleSub[, c("runTime", "age")], 
                              overAge))

summary(lmPiecewise)

overAge20 = lapply(decades, function(x) pmax(0, (age20to80 - x)))
names(overAge20) = paste("over", decades, sep = "")
overAgeDF = cbind(age = data.frame(age = age20to80), overAge20)

tail(overAgeDF)

predPiecewise = predict(lmPiecewise, overAgeDF)

plot(predPiecewise ~ age20to80,
     type = "l", col = "purple", lwd = 3,
     xlab = "Age (years)", ylab = "Run Time Prediction")

lines(x = age20to80, y = MaleRes.lo.pr, 
      col = "green", lty = 2, lwd = 3)
legend("topleft", col = c("purple", "green"),
       lty = c(1, 2), lwd= 3,
       legend = c("Piecewise Linear", "Loess Curve"), bty = "n")


plot(predPiecewise ~ age20to80,
     type = "l", col = "#984ea3", lwd = 3,
     #   type = "l", col = "purple", lwd = 2,
     xlab = "Age (years)", ylab = "Run Time Prediction")

lines(x = age20to80, y = MaleRes.lo.pr, col = "#4daf4a", lwd = 3, lty = 2)
legend("topleft", col = c("#984ea3", "#4daf4a"), lty = c(1, 2), lwd = 3,
       legend = c("Piecewise Linear", "Loess Curve"), bty = "n")

######### Line Plot of the Number of Female Runners by Year ####################

numRunners = with(cbMale, tapply(runTime, year, length))
plot(numRunners ~ names(numRunners), type="l", lwd = 2,
     xlab = "Years", ylab = "Number of Runners")

######### Density Curves for the Age of Female Runners for 2 years# ############
summary(cbMaleSub$runTime[cbMaleSub$year == 2009])

summary(cbMaleSub$runTime[cbMaleSub$year == 2012])

age2009 = cbMaleSub[ cbMaleSub$year == 2009, "age" ]
age2012 = cbMaleSub[ cbMaleSub$year == 2012, "age" ]

plot(density(age2009, na.rm = TRUE), 
     ylim = c(0, 0.05), col = "purple",
     lwd = 3,  xlab = "Age (years)",  main = "")
lines(density(age2012, na.rm = TRUE), 
      lwd = 3, lty = 2, col="green")
legend("topleft", col = c("purple", "green"), lty= 1:2, lwd = 3,
       legend = c("2009", "2012"), bty = "n")

############## Loess Curves Fit to Performance for 2 years ####################
qqplot(age2009, age2012, pch = 19, cex = 0.5, 
       ylim = c(10,90), xlim = c(10,90), 
       xlab = "Age in 2009 Race",
       ylab = "Age in 2012 Race", 
       main = "Quantile-quantile plot of male runner's age")
abline(a =0, b = 1, col="red", lwd = 2)

mR.lo09 = loess(runTime ~ age, cbMaleSub[ cbMaleSub$year == 2009,])
mR.lo.pr09 = predict(mR.lo09, data.frame(age = age20to80))

mR.lo12 = loess(runTime ~ age, cbMaleSub[ cbMaleSub$year == 2012,])
mR.lo.pr12 = predict(mR.lo12, data.frame(age = age20to80))

plot(mR.lo.pr09 ~ age20to80,
     type = "l", col = "purple", lwd = 3,
     xlab = "Age (years)", ylab = "Fitted Run Time (minutes)")


#################### Difference between Loess Curves of the predicted run time for 2 years ############
lines(x = age20to80, y = mR.lo.pr12,
      col = "green", lty = 2, lwd = 3)

legend("topleft", col = c("purple", "green"), lty = 1:2, lwd = 3,
       legend = c("2009", "2012"), bty = "n")


plot(mR.lo.pr09 ~ age20to80,
     type = "l", col = "#984ea3", lwd = 3,
     xlab = "Age (years)", ylab = "Prediction (minutes)")  
lines(x = age20to80, y = mR.lo.pr12, col="#4daf4a", lty = 2, lwd = 3) 
legend("topleft", col = c("#984ea3", "#4daf4a"), lty = 1:2, lwd = 3,
       legend = c("2009", "2012"), bty = "n")
gap12 = mR.lo.pr12 - mR.lo.pr09

plot(gap12 ~ age20to80, type = "l" , xlab = "Age (years)", 
     ylab = "Difference in Fitted Curves (minutes)", lwd = 2)

