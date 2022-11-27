reduce = function(book, message) {
  
  for (i in message){
    #print(i)
    for (j in 1:nrow(book[[1]])){
      print(j)
      #if(book[j, 'oid'] == message$oid[i]){
        #within(book, rm(book[[1]]$oid==message$oid[i]))
      #}
      
    }
    
  }
  return(book)
}

oid = c('a', 'b')
side = c('S', 'B')
price = c(105, 95)
size = c(100, 100)

core = data.frame(oid, side, price, size)
book = list('ask'=core, 'bid'=core)

message = list('oid'='a', 'amount'=50)

reduce(book, message)
