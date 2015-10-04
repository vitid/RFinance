source("support\\instrumentData.R");
source("support\\portfolioData.R");
source("support\\tsFunction.R");
source("support\\graphicFunction.R");

#suppress all warning messages...
options(warn=-1);

test.movingAvg<-function(stock_name,from="2014-01-01",to="2016-01-01",capital=100,...){
	close = getDataMatrixFromDB(c(stock_name),from,to,"CLOSE")[1,];
	upper = EMA(close,22);
	lower = EMA(close,11);
	
	signal = upper - lower;
	signal = sign(signal);
	signal = signal + lag(signal,1);
	
	cross_index = which(signal==0);
	
	if(signal[cross_index[1] - 1] < 0){
		#if the first cross-over is cross-down,skip it...
		cross_index = cross_index[2:length(cross_index)];
	}
	
	entry_index = cross_index[seq(from=1,to=length(cross_index),by=2)];
	exit_index = cross_index[seq(from=2,to=length(cross_index),by=2)];
	if(length(exit_index) < length(entry_index)){
		#if exit for the last entry is not occur, we use the last trading date to close the position
		#the last exit index is set as length(close)-1 because the position will be closed at the last trading date
		exit_index = c(exit_index,length(close)-1);
	}
	
	if(length(entry_index) <= 1){
		print("insufficient data, program exits");
		return();
	}
	
	#prepare necessary feature vectors
	rsi = RSI(close,14);
	#==============================
	
	data = data.frame(is_profit=integer(0),day_from_last_exit=integer(0),rsi=numeric(0));
	
	#index started at 2 because we need day_from_last_exit info.
	for(index in 2:length(entry_index)){
		#buy & sell at 1 day after the signals occur
		entry_price = close[entry_index[index]+1];
		exit_price = close[exit_index[index]+1];
		
		is_profit = ifelse(exit_price - entry_price > 0,1,0);
		day_from_last_exit = entry_index[index] - exit_index[index-1];
		
		data = rbind(data,data.frame(is_profit,day_from_last_exit,rsi[entry_index[index]]));
	}
	
	print(data);
	
	plot(close,type="l");
	lines(upper,col="green");
	lines(lower,col="red");
	
}
