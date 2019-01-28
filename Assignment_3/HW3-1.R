
############################## 1. Reading financial data ############################
library("quantmod")
sp500  <-  new.env()
getSymbols("^GSPC",  env  =  sp500,  src  =  "yahoo", from  =  as.Date("2008-01-01"),  to  =  as.Date("2018-01-01"))

# PepsiCo
getSymbols("PEP",src="yahoo")
# Coca-Cola
getSymbols("KO",src="yahoo")

#  Error: DPS download failed after two attempts. Error message:  HTTP error 404.#
# Keurig Dr Pepper##
# getSymbols("DPS",src="yahoo")

pepsi = as.data.frame(PEP)
pepsi=cbind(datestamp=rownames(pepsi),pepsi)
rownames(pepsi)=c()
colnames(pepsi) <- c("Date","Open","High","Low","Close","Volume","Adj.Close")
summary(pepsi)

coca = as.data.frame(KO)
coca =cbind(datestamp=rownames(coca),coca)
rownames(coca)=c()
colnames(coca) <- c("Date","Open","High","Low","Close","Volume","Adj.Close")
summary(coca)

# convert Data into Date data type.
pepsi$Date = as.Date(pepsi$Date)
coca$Date = as.Date(coca$Date)

rr=intersect(pepsi$Date, coca$Date)
pepsi[which(pepsi$Date %in% rr),]

############################### 2. Combine DataSets ########################################

combine2Stocks = 
  
  function(a, b, stockNames = c(deparse(substitute(a)), 
                                deparse(substitute(b)))) 
  { 
    rr = intersect(a$Date, b$Date)
    a.sub=a[which(a$Date %in% rr),]
    b.sub=b[which(b$Date %in% rr),]
    structure(data.frame(as.Date(a.sub$Date), 
                         a.sub$Adj.Close, 
                         b.sub$Adj.Close),
              names = c("Date", stockNames)) 
  }

overlap = combine2Stocks(pepsi, coca)

head(overlap)
names(overlap)

overlap=overlap[order(overlap$Date),]
head(overlap)

range(overlap$Date)

r = overlap$pepsi/overlap$coca
cor(overlap$pepsi,overlap$coca)

  ########################### 3. Visualizing Time Series #####################################
plotRatio =
  function(r, k = 1, date = seq(along = r), ...)
  {
    plot(date, r, type = "l", ...)
    abline(h = c(mean(r), 
                 mean(r) + k * sd(r), 
                 mean(r) - k * sd(r)), 
           col = c("darkgreen", rep("red", 2*length(k))), 
           lty = "dashed")
  }

plotRatio(r, k=0.85, overlap$Date, col = "lightgray", xlab = "Date", ylab = "Ratio")
# ,ylim=c(0.5,1.5)


######################### 4. Finding Opening and Closing Positions #########################

findNextPosition =
  # e.g.,  findNextPosition(r)
  #        findNextPosition(r, 1174)
  # Check they are increasing and correctly offset
  function(ratio, startDay = 1, k = 1, 
           m = mean(ratio), s = sd(ratio))
  {
    up = m + k *s
    down = m - k *s
    
    if(startDay > 1)
      ratio = ratio[ - (1:(startDay-1)) ]
    
    isExtreme = ratio >= up | ratio <= down
    
    if(!any(isExtreme))
      return(integer())
    
    start = which(isExtreme)[1]
    backToNormal = if(ratio[start] > up)
      ratio[ - (1:start) ] <= m
    else
      ratio[ - (1:start) ] >= m
    
    # return either the end of the position or the index 
    # of the end of the vector.
    # Could return NA for not ended, i.e. which(backToNormal)[1]
    # for both cases. But then the caller has to interpret that.
    
    end = if(any(backToNormal))
      which(backToNormal)[1] + start
    else
      length(ratio)
    
    c(start, end) + startDay - 1 
  }

#### Finding all positions ### 

getPositions =
  function(ratio, k = 1, m = mean(ratio), s = sd(ratio))
  {
    when = list()
    cur = 1
    
    while(cur < length(ratio)) {
      tmp = findNextPosition(ratio, cur, k, m, s)
      if(length(tmp) == 0)  # done
        break
      when[[length(when) + 1]] = tmp
      if(is.na(tmp[2]) || tmp[2] == length(ratio))
        break
      cur = tmp[2]
    }
    
    when
  }

#### visualizing all positions ###

showPosition = 
  function(days, ratio, radius = 50)
  {
    if(is.list(days))
      days = unlist(days)
    
    symbols(days, ratio[days], 
            circles = rep(radius, length(days)), 
            fg = c("darkgreen", "red"),
            add = TRUE, inches = FALSE)
  }

k = 0.85
pos=getPositions(r,k)
plotRatio(r, k, col = "lightgray", xlab = "Date", ylab = "Ratio")
showPosition(pos,r)

######################## 5. Compute Profit for a Positions #################################
positionProfit =
  #  r = overlap$pepsi/overlap$coca
  #  k = 1.7
  #  pos = getPositions(r, k)
  #  positionProfit(pos[[1]], overlap$pepsi, overlap$coca)
  function(pos, stockPriceA, stockPriceB, 
           ratioMean = mean(stockPriceA/stockPriceB),  ### ratio range
           p = .001, byStock = FALSE)
  {
    if(is.list(pos)) {
      ans = sapply(pos, positionProfit, 
                   stockPriceA, stockPriceB, ratioMean, p, byStock)
      if(byStock)
        rownames(ans) = c("A", "B", "commission")
      return(ans)
    }
    # prices at the start and end of the positions
    priceA = stockPriceA[pos]
    priceB = stockPriceB[pos]
    
    # how many units can we by of A and B with $1
    unitsOfA = 1/priceA[1]
    unitsOfB = 1/priceB[1]
    
    # The dollar amount of how many units we would buy of A and B
    # at the cost at the end of the position of each.
    amt = c(unitsOfA * priceA[2], unitsOfB * priceB[2])
    
    # Which stock are we selling
    sellWhat = if(priceA[1]/priceB[1] > ratioMean) "A" else "B"
    
    profit = if(sellWhat == "A") 
      c((1 - amt[1]),  (amt[2] - 1), - p * sum(amt))
    else 
      c( (1 - amt[2]),  (amt[1] - 1),  - p * sum(amt))
    
    if(byStock)
      profit
    else
      sum(profit)
  }

#### split data into train and test dataset ####

i = 1:floor(nrow(overlap)/2)
train = overlap[i, ]
test = overlap[ - i, ]

r.train = train$pepsi/train$coca
r.test = test$pepsi/test$coca

k.max = max((r.train - mean(r.train))/sd(r.train)) #+5

k.min = min((abs(r.train - mean(r.train))/sd(r.train)))

ks = seq(k.min, k.max, length = 100)
ks
m  = mean(r.train)

profits =
  sapply(ks,
         function(k) {
           pos = getPositions(r.train, k)
           if(length(pos)>0){
             sum(positionProfit(pos, train$pepsi, train$coca, 
                                mean(r.train)))
           }
           else{
             return(0)
             
           }
         })

plot(ks, profits, type = "l", xlab = "k", ylab = "Profit")

ks[  profits == max(profits) ]  

tmp.k = ks[  profits == max(profits) ]  
pos = getPositions(r.train, tmp.k[1])
all(sapply(tmp.k[-1],
           function(k) 
             identical(pos, getPositions(r.train, k))))

k.star = mean(ks[  profits == max(profits) ]  )

pos = getPositions(r.test, k.star, mean(r.test), sd(r.test))
testProfit = sum(positionProfit(pos, test$pepsi, test$coca)) 
testProfit

