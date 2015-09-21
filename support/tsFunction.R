# Misc time series functions
# 
# Author: vitid
###############################################################################
library(quantmod);
library(tseries);

#get integrated time series(arima(0,lag,0))
getDiff<-function(data,lag,na.rm=T){
	data = data - Lag(data,lag);
	if(na.rm){
		data = na.omit(data);
	}
	return(data);
}

#get relative time series data
#value of x(1) is set to 1
getRelative<-function(x){
	lag_x = as.vector(Lag(x,1))
	relative = x / lag_x
	#no definition for the first sequence
	relative[1] = 1
	return(relative);
}

#if data(i) < 0, label "down", else label "up"
getUpDown<-function(data){
	r = ifelse(data < 0,"down","up");
	return(r);
}

#randomly sampling a block of data from time series and append that data
#to the end of time series
blockBootstrap <- function(data,n=10){
	b_data = c();
	for(i in 1:n){
		indexs = sample(1:length(data),size=2,replace=F);
		index0 = min(indexs);
		index1 = max(indexs);
		new_data = data[index0:index1];
		if(length(b_data) > 0){
			#smooth the end of time series with the begining of sampling data
			new_data = new_data + b_data[length(b_data)] - new_data[1];
			new_data = new_data[2:length(new_data)];
		}
		b_data = c(b_data,new_data);
	}
	return (b_data);
}
