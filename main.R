source('framework/data.R')
source('framework/backtester.R')
source('framework/processResults.R')
source('example_strategies.R')

# load data
## in AS2 use MWS subset data
#dataList <- getData(directory="EXAMPLE")
dataList <- getData(directory="A2")

# subset data: used to choose in/out sample period
# in
#dataListIn <- lapply(dataList, function(x) x[1:847])
# debug period
dataListIn <- lapply(dataList, function(x) x[1:902])
# out
#dataListOut <- lapply(dataList, function(x) x[848:2000])
# debug period
dataListOut <- lapply(dataList, function(x) x[903:2000])

#dataList <- lapply(dataList, function(x) x[1:250])
#dataList <- lapply(dataList, function(x) x[500:1500])

strategy <- "strategy"

# check that the choice is valid
stopifnot(is_valid_example_strategy(strategy))

# load in strategy and params
load_strategy(strategy) # function from example_strategies.R

a = asplit(combn(1:4, 2), 2)
b = asplit(combn(1:4, 3), 2)
c = asplit(combn(1:4, 4), 2)
#print(expand.grid(c(5,10),c(50,100),c(200,300), c(a,b,c)))
grid = expand.grid(c(5,10),c(50,100),c(200,300), c(a,b,c))

in_all = c(0)
out_all = c(0)
in_idx = 1
out_idx = 1
best_in = 0
best_out = 0

best_in_params = list("lookbacks"=list(short=as.integer(5), medium=as.integer(50), long=as.integer(200)), "series"=c(1,2))
best_out_params = list("lookbacks"=list(short=as.integer(5), medium=as.integer(50), long=as.integer(200)), "series"=c(1,2))

for(i in 1:nrow(grid)){
  # override params
  params$lookbacks = list(short=as.integer(grid[i,1]), medium=as.integer(grid[i,2]), long=as.integer(grid[i,3]))
  params$series=as.vector(grid[i,4][[1]])
  print(params)
  sMult <- 0.20 # slippage multiplier
  
  # Do backtest
  # in
  results <- backtest(dataListIn,getOrders,params,sMult)
  #plotResults(dataListIn,results)
  cat("In Profit:", results$aggProfit, '\n')
  in_all = append(in_all, results$aggProfit[[1]])
  if(results$aggProfit>best_in) {
    best_in = results$aggProfit
    best_in_params = params
    in_idx = i
  }
  # out
  results <- backtest(dataListOut,getOrders,params,sMult)
  #plotResults(dataListOut,results)
  cat("Out Profit:", results$aggProfit, '\n')
  out_all = append(out_all, results$aggProfit[[1]])
  if(results$aggProfit>best_out) {
    best_out = results$aggProfit
    best_out_params = params
    out_idx = i
  }
}
print(in_idx)
print(out_idx)
best_in = best_in[[1]]
best_out = best_out[[1]]

## rank process
#out_all = append(out_all, best_in)
ranking = rank(out_all[2:89], ties.method='average')
print(ranking)
rank_on_out = ranking[in_idx]
rank_on_out = 89-rank_on_out
#print(rank_on_out)

#in_all = append(in_all, best_out)
ranking = rank(in_all[2:89], ties.method='average')
print(ranking)
rank_on_in = ranking[out_idx]
rank_on_in = 89-rank_on_in
#print(rank_on_in)

print("ins:")
print(best_in_params)
print(best_in)
print(rank_on_out)

print("out:")
print(best_out_params)
print(best_out)
print(rank_on_in)
