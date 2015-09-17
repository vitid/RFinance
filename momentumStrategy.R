# TODO: Add comment
# 
# Author: vitid
###############################################################################

source("custom_lib.R");
source("anticor.test.R");
source("anticor.R");

testMomentumStrategy<-function(stock_names,from="2014-01-01",to="2016-01-01",capital=100,window_size=22,lookback_window=1,select_proportion=0.2){
	r = test.getRelativeDataMatrixFromDB(stock_names,from,to,column="CLOSE");
	
	if(!is.matrix(r)){
		print("can't generate relative-return matrix");
		return();
	}
	
	asset = c();
	uniform_b = rep(1/nrow(r),times=nrow(r));
	b = uniform_b;
	for(index in seq(from=(window_size*lookback_window),to=ncol(r),by=window_size)){
		
		index_lookback = index - (window_size*lookback_window) + 1;
		if(length(asset) == 0){
			index_adopt_b = index_lookback;
		}else{
			index_adopt_b = index - window_size + 1;
		}
		
		#update assets during the last period
		last_asset = ifelse(length(asset) > 0, asset[length(asset)] , capital);
		asset = c(asset,getCapitalValues(last_asset,r[,index_adopt_b:index],b));
		
		#a vector, each data a(i) represent a return of stock(i) during (window_size*lookback_window) period
		r_period = apply(r[,index_lookback:index],1,prod);
		#the number of selected winner is rounded down
		selected_winner = (order(r_period,decreasing = TRUE))[1:(length(r_period)*select_proportion)];
		b = rep(0,times=nrow(r));
		b[selected_winner] = 1/length(selected_winner);
	}
	
	#update assets for the remaining window left
	index_adopt_b = index + 1;
	index = ncol(r);
	
	last_asset = ifelse(length(asset) > 0, asset[length(asset)] , capital);
	asset = c(asset,getCapitalValues(last_asset,r[,index_adopt_b:index],b));
	
	benchmark = getCapitalValues(capital,r,uniform_b);
	
	test.generateBenchmarkGraph(asset,benchmark);
}
