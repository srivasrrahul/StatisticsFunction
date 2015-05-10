#Statistics basic tools
tsquare <- function(x,y) {
    m1 <- mean(x)
    m2 <- mean(y)
    s1 <- sd(x)
    s2 <- sd(y)
    
    numerator <- (m1-m2)
    d1 <- sqrt((length(x)*s1*s1 + length(y)*s2*s2)/(length(x)+ length(y)-2))
    
    d2 <- sqrt((length(x) + length(y))/(length(x)*length(y)))
    #print(d1*d2)
    return (numerator/(d1*d2))
    
}
findDf <- function(x,y) {
    return (length(x)-1 + length(y)-1)
}
sumOfSquareDeviation <- function(x) {
    m1 <- mean(x)
    s1 <- sd(x)
    data <- 0
    for (item in x) {
        diff <- item - m1
        data <- data + (diff*diff)
    }
    
    return (data)
}

sumOfSuare <- function(x) {
    sum <- 0
    for (item in x) {
        sum <- sum + (item *item)
    }
    
    return (sum)
}

overallMean <- function(lst) {
    len <- length(lst)
    sum <- 0
    count <- 0
    for (i in 1:len) {
        for (item in lst[[i]]) {
            sum <- sum + item
            count <- count + 1
        }    
    }
    
    return (sum / count)
}

sumOfSquaresBetween <- function(lst) {
    len <- length(lst)
    om <- overallMean(lst)
    ssb <- 0
    for (i in 1:len) {
        l <- lst[[i]]
        m <- mean(l)
        diff <- m - om
        ssb <- ssb + length(l)*(diff * diff)
    }
    
    return (ssb)
}

sumLst <- function(lst) {
    sumLsts <- c()
    for (arr in lst) {
        sumLst <- c(sumLsts,sum(arr))
    }
    
    return (sumLsts)
}

meanLst <- function(lst) {
    meanVec <- c()
    for (arr in lst) {
        meanVec <- c(meanVec,mean(arr))
    }
    
    return (meanVec)
}

squareLst <- function(lst) {
    squareVec <- c()
    for (arr in lst) {
        s <- 0
        for (item in arr) {
            s <- s + (item*item)
        }
        
        squareVec <- c(squareVec,s)
    }
    
    return (squareVec)
}

overallCount <- function(lst) {
    items <- 0
    for (arr in lst) {
        for (item in arr) {
            items <- items + 1
        }
    }
    
    #print(items)
    return (items)
}

sstInternal <- function(overallMean,meanArr,meanSquareArr,itemsPerGroup) {
    s <- 0
    l <- length(meanSquareArr)
    sq <- sum(meanSquareArr)
    
    return (sq - itemsPerGroup*(length(meanArr)*(overallMean*overallMean)))
}
sst <- function(lst) {
    #sum(squares) - N*(overallMean*overallMean)
    squares <- squareLst(lst)
    N <- overallCount(lst)
    om <- overallMean(lst)
    s <- 0
    for (sq in squares) {
        s <- s + sq
    }
    #print(s)
    return (s - N*(om*om))
}

ssbInternal <- function(overallMean,meanArr,meanSquareArr,itemsPerGroup) {
    s <- 0
    l <- length(meanArr)
    for (i in 1:l) {
        diff <- overallMean - meanArr[i]
        s <- s + itemsPerGroup * (diff*diff)
    }
    
    return (s)
}


ssb <- function(lst) {
    om <- overallMean(lst)
    
    s <- 0
    for (arr in lst) {
        diff <- om - mean(arr)
        s <- s + length(arr) * (diff*diff)
    }
    
    return (s)
}

ssw <- function(lst) {
    return (sst(lst) - ssb(lst))
}

dfw <- function(lst) {
    return (overallCount(lst) - length(lst))
}

dfb <- function(lst) {
    return (length(lst)-1)
}

msw <- function(lst) {
    return (ssw(lst)/dfw(lst))
}

msb <- function(lst) {
    return (ssb(lst)/dfb(lst))
}

fstats <- function(lst) {
    return (msb(lst)/msw(lst))
}

scalarProduct <- function(x,y) {
    
}
basicRegressionModel <- function(xAxis,yAxis) {
    n <- length(xAxis)
    scalarProduct <- xAxis * yAxis
    xSquare <- xAxis * xAxis
    m <- mean(xAxis)
    
    numerator <- (n*sum(scalarProduct)) - (sum(xAxis)*sum(yAxis))
    denominator <- (n*sum(xSquare)) - (sum(xAxis) * sum(xAxis))
    
    return (numerator/denominator)
}

alphaStat <- function(xAxis,yAxis) {
    regression <- basicRegressionModel(xAxis,yAxis)
    alpha <- mean(y) - regression*mean(x)
    return (alpha)
}

smallSigmaForRegression <- function(xAxis,yAxis) {
    meanY <- mean(yAxis)
    predictValues <- c()
    alphaS <- alphaStat(xAxis,yAxis)
    regressionCoeff <- basicRegressionModel(xAxis,yAxis)
    for (x in xAxis) {
        predictValue <- alphaS + regressionCoeff * x
        predictValues <- c(predictValues,predictValue)
    }
    
    s <- 0
    diffValues <- predictValues - yAxis
    s <- sum(diffValues * diffValues)
    print("gello")
    print(s)
    numerator <- sqrt(s / (length(yAxis)-2))
    
    meanX <- mean(xAxis)
    s <- 0
    for (x in xAxis) {
        s <- s + (meanX-x)*(meanX-x)
    } 
    
    print(s)
    denominator <- sqrt(s)
    return (numerator/denominator)
}

significanceValueForRegression <- function(xAxis,yAxis) {
    return (basicRegressionModel(xAxis,yAxis)/(smallSigmaForRegression(xAxis,yAxis)))
}
