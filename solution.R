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
  #return ((min(book[[1]]$price)+max(book[[2]]$price))/2)
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
  #return (min(book[[1]]$price)-max(book[[2]]$price))
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
    while(book.spread(book) <= 0){

      #print(c("spread: ", book.spread(book)))

      if(is.na(book.spread(book))) break
      
      #print(c('nrow: ', nrow(book$ask)))
      
      #for (j in 1:nrow(book$ask)){
        #print(c("j: ",j))
        #print(c('ask j: ',book$ask[j, 'price']))
        #print(c('highest bid: ', book$bid[1, 'price']))
        
        # after sorting best prices always at first line
        if(book$ask[1, 'price'] <= book$bid[1, 'price']){

          #print(c("match j: ",j))

          delta = book$ask[1, 'size'] - book$bid[1, 'size']

          #print(c("delta: ",delta))

          if(delta == 0){

            book$ask = book$ask[-1,]
            book$bid = book$bid[-1,]
            #break

          }else if(delta > 0){

            book$ask[1, 'size'] = book$ask[1, 'size'] - book$bid[1, 'size']
            book$bid = book$bid[-1,]
            #break

          }else{# delta < 0

            book$bid[1, 'size'] = book$bid[1, 'size'] - book$ask[1, 'size']
            book$ask = book$ask[-1,]
            #break

          }
          #print(book)
        }
        
      #}

    }
    
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
    # See handout for instructions
    M = book.total_volumes(book[1])
    if(size == M) 
      return (NA)
    
    price = sample(book[1][3],size=1,replace=T)
    order = list('price'=price, 'size'=size)
    book.add(book, order)
    return (mean(book.midprice(book)))
}

book.extra2 <- function(book, size) {
    # See handout for instructions
    M = book.total_volumes(book[1])
    if(size == M) 
      return (NA)
    
    price = sample(seq(book.best_prices[1], max(book[1][3]), by=1),size=1,replace=T)
    order = list('price'=price, 'size'=size)
    book.add(book, order)
    return (mean(book.midprice(book)))
}

book.extra3 <- function(book) {
    # See handout for instructions
    M = book.total_volumes(book[1])
    book.sort(book)
    size = sample(seq(1, M-1, by=1),size=1,replace=T)
    idx = 1
    while(size > 0){
      if(book[1][idx][4]>= size){
        book[1][idx][4] = book[1][idx][4] - size
        size = 0
      }else{
        size = size - book[1][idx][4]
        book[1][idx][4] = book[1][idx][4] - size
        idx = idx + 1
      }
    }
    return (mean(book.midprice(book)))
}

book.extra4 <- function(book, k) {
    # See handout for instructions
    old_midprice = book.midprice(book)
    book.sort(book)
    M = book.total_volumes(book[1])
    v = 0
    k_prime = 0
    if (length(book[1]) == 0){
      return (0)
    }else{
      while(k_prime <= k || v<=M){
        
        idx = 1
        
        if(book[1][idx][4]>= v){
          book[1][idx][4] = book[1][idx][4] - v
          
        }else{
          v = v - book[1][idx][4]
          book[1][idx][4] = book[1][idx][4] - v
          idx = idx + 1
        }
        
        
        v = v+1
        k_prime = (book.midprice(book) - old_midprice)/old_midprice
      }
    }
    return (v)
}
