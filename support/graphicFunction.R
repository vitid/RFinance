# generating graph, etc...
# 
# Author: vitid
###############################################################################

GF.generateBenchmarkGraph <- function(asset,benchmark){
	asset = asset/asset[1]
	benchmark = benchmark/benchmark[1]
	
	min_value = min(min(asset),min(benchmark))
	max_value = max(max(asset),max(benchmark))
	
	plot(asset,type="l",col="blue",ylim=c(min_value,max_value))
	lines(benchmark,type="l",col="blue",lty=6)
}

#data - data.fram as returned from test.fixedWidthAnticor(...)
#       with column "total_asset" removed
GF.generatePortfolioCompositionPlot <- function(data){
	data = as.matrix(t(data));
	#color index start from 2 because 1 is color black and it looks ugly...
	barplot(data,col=2:(nrow(data)+1));
}
