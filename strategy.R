getOrders <- function(store, newRowList, currentPos, info, params) {
    #print(c("store: ", store))
    #print(c("newRowList: ", newRowList))
    #print(c("params: ", params))
    ###########################################################################
    # You do not need to edit this next part of the code
    ###########################################################################
    allzero  <- rep(0,length(newRowList)) # used for initializing vectors
    pos <- allzero

    if (is.null(store)) 
        store <- initStore(newRowList)
    else
        store <- updateStore(store, newRowList)
    ###########################################################################

    ###########################################################################
    # This next code section is the only one you
    # need to edit for getOrders
    #
    # The if condition is already correct:
    # you should only start computing the moving 
    # averages when you have enough (close) prices 
    # for the long moving average 
    ###########################################################################
    if (store$iter > params$lookbacks$long) {
      #print("in")
        # ENTER STRATEGY LOGIC HERE
      for(i in params$series){
        #print(c("i", i))
        cl <- store$cl[[i]]
        #cl <- store$cl[i][[1]]
        tma_list = getTMA(cl, params$lookbacks)
        #pos = pos+getPosSignFromTMA(tma_list)*getPosSize(cl)
        #print(c("tma", tma_list))
        #print(c("cl", cl))
        sign = getPosSignFromTMA(tma_list)
        size = getPosSize(cl)
        #print(c("sign", sign))
        #print(c("size", size))
        abs = coredata(getPosSignFromTMA(tma_list)*getPosSize(cl))
        #print(c("abs", abs))
        #pos[i] = abs[1]
        pos[i] = tail(abs,n=1)
        #pos = pos+getPosSignFromTMA(tma_list)*getPosSize(cl)
      }

        # remember to only consider the series in params$series

        # You will need to get the current_close
        # either from newRowList or from store$cl

        # You will also need to get prices 
        # from store$cl

        # With these you can use getTMA, getPosSignFromTMA
        # and getPosSize to assign positions to the vector pos
    }
    ###########################################################################

    ###########################################################################
    # You do not need to edit the rest of this function
    ###########################################################################
    marketOrders <- -currentPos + pos
    #print(c("pos: ", pos))
    #print(c("market orders: ", marketOrders))
    return(list(store=store,marketOrders=marketOrders,
	                    limitOrders1=allzero,limitPrices1=allzero,
	                    limitOrders2=allzero,limitPrices2=allzero))
}

###############################################################################
checkE01 <- function(prices, lookbacks) {
    # Return FALSE if lookbacks contains named elements short, medium, and long
    # otherwise return TRUE to indicate an error
    if(length(lookbacks$short)==0 | length(lookbacks$medium)==0 | length(lookbacks$long)==0) TRUE
    else FALSE
}
checkE02 <- function(prices, lookbacks) {
    # Return FALSE if all the elements of lookbacks are integers (as in the R
    # data type) otherwise return TRUE to indicate an error
    cnt = 0
    for (i in lookbacks){
      if (is.integer(i)) cnt = cnt + 1
    }
    if(cnt==3) FALSE
    else TRUE
}
checkE03 <- function(prices, lookbacks) {
    # Return FALSE if lookbacks$short < lookbacks$medium < lookbacks$long 
    # otherwise return TRUE to indicate an error
    if((lookbacks$short)<(lookbacks$medium)&&(lookbacks$medium)<(lookbacks$long)) FALSE
    else TRUE
}
checkE04 <- function(prices, lookbacks) {
    # Return FALSE if prices is an xts object, otherwise return TRUE to
    # indicate an error
    if(is.xts(prices)) FALSE
    else TRUE
}
checkE05 <- function(prices, lookbacks) {
    # Return FALSE if prices has enough rows to getTMA otherwise return TRUE
    # to indicate an error
    if(nrow(prices)>=lookbacks$long) FALSE
    else TRUE
}
checkE06 <- function(prices, lookbacks) {
    # Return FALSE if prices contains a column called "Close" otherwise return 
    # TRUE to indicate an error
    if("Close" %in% colnames(prices)) FALSE
    else TRUE
}
###############################################################################
# You should not edit allChecks

atLeastOneError <- function(prices, lookbacks) {
    # return TRUE if any of the error checks return TRUE
    ret <- FALSE
    ret <- ret | checkE01(prices,lookbacks)
    ret <- ret | checkE02(prices,lookbacks)
    ret <- ret | checkE03(prices,lookbacks)
    ret <- ret | checkE04(prices,lookbacks)
    ret <- ret | checkE05(prices,lookbacks)
    ret <- ret | checkE06(prices,lookbacks)
    return(ret)
}

###############################################################################

getTMA <- function(prices, lookbacks, with_checks=FALSE) {
    library(TTR)
    # prices and lookbacks should pass (return FALSE) when used with
    # the 6 checks, as tested in the following call to allChecks that 
    # you should not edit
    if (with_checks)
        if (atLeastOneError(close_prices, lookbacks))
            stop('At least one of the errors E01...E06 occured')
  
    #price <- coredata(prices)
    #price = price[,1]
    #print(price)
    
    n=lookbacks$short
    short = SMA(prices$Close, n=n)
    short = as.numeric(short)
    
    n=lookbacks$medium
    medium = SMA(prices$Close, n=n)
    medium = as.numeric(medium)
    
    n=lookbacks$long
    long = SMA(prices$Close, n=n)
    long = as.numeric(long)
    
    # short <- vector(mode='numeric', length(price)) 
    # n=lookbacks$short 
    # short[1:n-1] <- NA
    # short[n] <- sum(price[1:n])/n
    # for (i in (n+1):length(price)) {
    #   short[i] <- short[i-1] + (price[i]-price[i-n])/n
    # } 
    # 
    # medium <- vector(mode='numeric', length(price)) 
    # n=lookbacks$medium
    # medium[1:n-1] <- NA
    # medium[n] <- sum(price[1:n])/n
    # for (i in (n+1):length(price)) {
    #   medium[i] <- medium[i-1] + (price[i]-price[i-n])/n
    # } 
    # 
    # long <- vector(mode='numeric', length(price)) 
    # n=lookbacks$long
    # long[1:n-1] <- NA
    # long[n] <- sum(price[1:n])/n
    # for (i in (n+1):length(price)) {
    #   if(i>length(price)) break
    #   long[i] <- long[i-1] + (price[i]-price[i-n])/n
    # } 
    
    #print(short)
    #print(medium)
    #print(long)
    
    ret = list('short'=tail(short,n=1), 'medium'=tail(medium,n=1), 'long'=tail(long,n=1))
    # You need to replace the assignment to ret so that the returned object:
    #    - is a list 
    #    - has the right names (short, medium, long), and
    #    - contains numeric and not xts objects
    #    - and contains the correct moving average values, which should 
    #      have windows of the correct sizes that all end in the 
    #      same period, be the last row of prices

    return(ret)
}

getPosSignFromTMA <- function(tma_list) {
    # This function takes a list of numbers tma_list with three elements 
    # called short, medium, and long, which correspond to the SMA values for 
    # a short, medium and long lookback, respectively.

    # Note that if both this function and getTMA are correctly implemented 
    # then the following should work with correct input arguments:
    # getPositionFromTMA(getTMA(prices,lookbacks))

    # This function should return a single number that is:
    #       -1 if the short SMA < medium SMA < long SMA
    #        1 if the short SMA > medium SMA > long SMA
    #        0 otherwise
    pos=0
  
    short=tma_list$short
    medium=tma_list$medium
    long=tma_list$long
    
    if (short<medium&&medium<long) pos=-1
    else if(short>medium&&medium>long) pos=1
    
    return(pos)
}

getPosSize <- function(current_close,constant=5000) {
    # This function should return (constant divided by current_close) 
    # rounded down to the nearest integer
    
    ret = floor(constant/current_close)
    return(ret)
}

###############################################################################
# The functions below do NOT need to be edited
###############################################################################
initClStore  <- function(newRowList) {
  clStore <- lapply(newRowList, function(x) x$Close)
  return(clStore)
}
updateClStore <- function(clStore, newRowList) {
  clStore <- mapply(function(x,y) rbind(x,y$Close),clStore,newRowList,SIMPLIFY=FALSE)
  return(clStore)
}
initStore <- function(newRowList,series) {
  return(list(iter=1,cl=initClStore(newRowList)))
}
updateStore <- function(store, newRowList) {
  store$iter <- store$iter + 1
  store$cl <- updateClStore(store$cl,newRowList) 
  return(store)
}
