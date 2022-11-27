book.total_volumes <- function(book) {
    # Arguments:
    #   book - A list containing "ask" and "bid", each of which are dataframes
    #       containing the collection of limit orders.
    #
    # Returns:
    #   The total volume in the book.
  return (list('ask'=sum(book[[1]]$size), 'bid'=sum(book[[2]]$size)))
}

book.best_prices <- function(book) {
    # Arguments:
    #   book - A list containing "ask" and "bid", each of which are dataframes
    #       containing the collection of limit orders.
    #
    # Returns:
    #   A list with "ask" and "bid", the values of which are the best prices in
    #       the book.
  return (list('ask'=min(book[[1]]$price), 'bid'=max(book[[2]]$price)))
}

book.midprice <- function(book) {
    # Arguments:
    #   book - A list containing "ask" and "bid", each of which are dataframes
    #       containing the collection of limit orders.
    #
    # Returns:
    #   The midprice of the book.
  best_price = book.best_prices(book)
  midprice = (((best_price[[1]])+(best_price[[2]]))/2)
  return (midprice)
}

book.spread <- function(book) {
    # Arguments:
    #   book - A list containing "ask" and "bid", each of which are dataframes
    #       containing the collection of limit orders.
    #
    # Returns:
    #   The spread of the book.
  best_price = book.best_prices(book)
  spread = ((best_price[[1]])-(best_price[[2]]))
  return (spread)
}

###############################################################################

book.add <- function(book, message) {
    # Arguments:
    #   book - A list containing "ask" and "bid", each of which are dataframes
    #       containing the collection of limit orders.
    #   message - A list containing "oid", "side", "price" and "size" entries.
    #
    # Returns:
    #   The updated book.
    
    if(message$side == 'B'){
      buf = data.frame(oid=message$oid, price=message$price, size=message$size, stringsAsFactors=FALSE)
      book$bid = rbind(book$bid, buf)
      
    }else{
      buf = data.frame(oid=message$oid, price=message$price, size=message$size, stringsAsFactors=FALSE)
      book$ask = rbind(book$ask, buf)
    }
    
    book = book.sort(book, sort_bid=TRUE, sort_ask=TRUE)
    
    #print(book)
    
    #handle crossing
    while(is.na(book.spread(book))==F && book.spread(book) <= 0){
      
      if(is.na(book.spread(book))) break
      
      # print(c("spread: ", book.spread(book)))
      # print(c('nrow: ', nrow(book$ask)))
      # print(c('best bid: ', book$bid[1, 'price']))
      # print(c('best ask: ', book$ask[1, 'price']))
      
      # after sorting best prices always at first line
      if(book$ask[1, 'price'] <= book$bid[1, 'price']){
        
        delta = book$ask[1, 'size'] - book$bid[1, 'size']

        # print(c("delta: ",delta))

        if(delta == 0){
          book$ask = book$ask[-1,]
          book$bid = book$bid[-1,]

        }else if(delta > 0){
          #print(c('size:', book$bid[1, 'size']))
          book$ask[1, 'size'] = book$ask[1, 'size'] - book$bid[1, 'size']
          book$bid = book$bid[-1,]

        }else{# size of best bid > size of best offer
          # multilevel trade
          book$bid[1, 'size'] = book$bid[1, 'size'] - book$ask[1, 'size']
          book$ask = book$ask[-1,]

        }

      }

    }
    
  #print(book)
  
  return(book)
}

book.reduce <- function(book, message) {
    # Arguments:
    #   book - A list containing "ask" and "bid", each of which are dataframes
    #       containing the collection of limit orders.
    #   message - A list containing "oid" and "amount".
    #
    # Returns:
    #   The updated book.
  
  
    #print(book$ask)
    
    for (j in 1:nrow(book$ask)){
      
      #print(book$ask[j, 'price'])
      if(nrow(book$ask)==0) break
      #print(j)
      
      if(book$ask[j, 'oid'] == message$oid){
        #print(j)
         
         if(message$amount >= book$ask[j, 'size']){
           
           book$ask = book$ask[-j,]
           break
           
         }else{
           
           book$ask[j, 'size'] = book$ask[j, 'size'] - message$amount
           break
           
         }
        
      }
      
    }
  
  
    for (j in 1:nrow(book$bid)){
      
      if(nrow(book$bid)==0) break
      #if(j<1) break
      
      #print(j)
      
      if(book$bid[j, 'oid'] == message$oid){
        
         if(message$amount >= book$bid[j, 'size']){
           
           book$bid = book$bid[-j,]
           break
           
         }else{
           
           book$bid[j, 'size'] = book$bid[j, 'size'] - message$amount
           break
         }
        
      }
      
    }
  
    return(book)
}

###############################################################################
###############################################################################

# The following functions are the "extra" functions; marks for these functions
# are only available if you have fully correct implementations for the 6
# functions above

book.extra1 <- function(book, size) {
    
    M = book.total_volumes(book)
    M = M$ask
    if(size == M) 
      return (NA)
    
    ret = 0
    set = unique(book$ask[, 'price'])
    range = length(set)
    #price = sample(book$ask[, 'price'], size=1, replace=T)
    #print(c('sampled price: ', price))
    #print(c(is.numeric(price), is.numeric(96)))
    #print(c('size: ', size))
    for(price in set){
      book_b = book
      message = list('oid'='test', 'side'='B', 'price'=price, 'size'=size)
      #print('book before:')
      #print(book)
      book_b = book.add(book_b, message)
      #print('book after:')
      #print(book)
      ret = ret + book.midprice(book_b)
      #print(c('mean:', ret))
    }
    
    ret = ret / range
    
    #print(c('mean:', ret))
    
    return (ret)
}

book.extra2 <- function(book, size) {
    
    M = book.total_volumes(book)
    M = M$ask
    if(size == M) 
      return (NA)
    
    ret = 0
    best = book.best_prices(book)
    best_ask = best$ask
    for(price in seq(best_ask, max(book$ask[,'price']), by=1)){
    #print(c('sampled price: ', price))
      book_b = book
      message = list('oid'='test', 'side'='B', 'price'=price, 'size'=size)
      book_b = book.add(book_b, message)
      ret = ret + book.midprice(book_b)
      #print(c('mean:', ret))
    }
    
    range = length(seq(best_ask, max(book$ask[,'price']), by=1))
    #print(c('range:', range))
    #print(c('mean:', ret))
    ret = ret / range
    
    return (ret)
}

book.extra3 <- function(book) {
    
    M = book.total_volumes(book)
    M = M$ask
    ret = 0
    
    for(size in 1:(M-1)){
      #size = sample(seq(1, M-1, by=1), size=1, replace=T)
      #print(c('sampled size: ', size))
      
      #print('book before:')
      #print(book)
      book_b = book
      while(size > 0){
        
        best = book.best_prices(book_b)
        best_ask = best$ask
        portion = book_b$ask[1, 'size']
        if(size - portion >= 0){
          message = list('oid'='test', 'side'='B', 'price'=best_ask, 'size'=portion)
          book_b = book.add(book_b, message)
        }else{
          message = list('oid'='test', 'side'='B', 'price'=best_ask, 'size'=size)
          book_b = book.add(book_b, message)
          break
        }
        size = size - portion
      }
      #print('book after:')
      #print(book)
      ret = ret + book.midprice(book_b)
    }
    #print(c('mean:', ret))
    
    return (ret/(M-1))
}

book.extra4 <- function(book, k) {
    old_midprice = book.midprice(book)
    M = book.total_volumes(book)
    M = M$ask
    #print(c('k: ', k))
    v = 0
    k_prime = 0
    
    if (nrow(book$ask)==0){
      return (0)
    }else{
      while(k_prime <= k && v<=M){
        
        best = book.best_prices(book)
        best_ask = best$ask
        message = list('oid'='test', 'side'='B', 'price'=best_ask, 'size'=1)
        book = book.add(book, message)
        #print(c('v: ', v))
        v = v+1
        k_prime = ((book.midprice(book) - old_midprice)/old_midprice)*100
        #print(c('k_prime: ', k_prime))
        if(is.na(k_prime)) break
      }
    }
    
    return (v-1)
}
