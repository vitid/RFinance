# TODO: Add comment
# 
# Author: vitid
###############################################################################
library(RMySQL);
library(dplyr);
library("database.R");

getAvailableSymbols<-function(from,to,dbConnection){
	data = dbSendQuery(dbConnection,paste("SELECT MIN(DATE) as min_date,MAX(DATE) as max_date FROM stock_daily_info WHERE DATE BETWEEN '",from,"' AND '",to,"' ",sep=""));
	data = fetch(data,n=-1);
	min_date = data$min_date;
	max_date = data$max_date;
	
	sql = paste("SELECT s1.SYMBOL as symbol FROM stock_daily_info AS s1 INNER JOIN stock_daily_info AS s2 WHERE s1.DATE = '",min_date,"' AND s2.DATE = '",max_date,"' AND s1.SYMBOL = s2.SYMBOL" , sep="");
	
	data = dbSendQuery(dbConnection,sql);
	data = fetch(data,n=-1);
	return(data$symbol);
}

#stock_data - data.frame contains columns c("DATE","SYMBOL","OPEN","HIGH","LOW","CLOSE","VOLUME)
#			  as extracted from DB
fillMissingOHLCV<-function(all_data){
	
	all_date = distinct(all_data,DATE)$DATE;
	symbols = distinct(all_data,SYMBOL)$SYMBOL;
	all_date_count = length(all_date);
	
	if(nrow(all_data) == all_date_count*length(symbols)){
		return(all_data);
	}
	
	all_date_frame = data.frame(DATE=all_date,stringsAsFactors = FALSE);
	
	for(symbol in symbols){
		symbol_data = filter(all_data,SYMBOL==symbol);
		if(nrow(symbol_data) == all_date_count){
			next;
		}
		
		symbol_data = merge(symbol_data,all_date_frame,all=TRUE);
		symbol_data[,"SYMBOL"] = symbol;
		symbol_data[,"OPEN"] = na.locf(symbol_data[,"OPEN"]);
		symbol_data[,"HIGH"] = na.locf(symbol_data[,"HIGH"]);
		symbol_data[,"LOW"] = na.locf(symbol_data[,"LOW"]);
		symbol_data[,"CLOSE"] = na.locf(symbol_data[,"CLOSE"]);
		symbol_data[,"VOLUME"] = na.locf(symbol_data[,"VOLUME"]);
		
		all_data = filter(all_data,SYMBOL!=symbol);
		all_data = bind_rows(all_data,symbol_data);
	}
	
	return(all_data);
}

adf.pairTestFromDB<-function(symbol1,symbol2,...){
	data = loadData(c(symbol1,symbol2),...);
	data = fillMissingOHLCV(data);
	
	close1 = filter(data,SYMBOL==symbol1)$CLOSE;
	close2 = filter(data,SYMBOL==symbol2)$CLOSE;
	
	return(adf.pairTest(close1,close2));
}

adf.pairTest<-function(p1,p2){
	lm.model = lm(p1 ~ p2 + 0);
	beta = coef(lm.model)[1];
	spread = p1 - (p2 * beta);
	return(adf.test(spread,alternative="stationary", k=0));
}

#price_matrix - data.frame which have the following columns:
#				c(price_0,price_1,price_2,...,price_n)
#				and each row is data of each instrument
getClusterCoIntegratedData<-function(price_matrix){
	num = nrow(price_matrix);
	dst_matrix = matrix(0,nrow=num,ncol=num);
	
	for(i in 1:(nrow(dst_matrix)-1)){
		for(j in (i+1):ncol(dst_matrix)){
			test_data = adf.pairTest(price_matrix[i,],price_matrix[j,]);
			dst_matrix[i,j] = test_data[["p.value"]];
			dst_matrix[j,i] = dst_matrix[i,j];
		}
	}
	return(dst_matrix);
}

#noted that from my observation, cluster of correlated
#data is less reliable than cluster of cointegrated data...
getClusterCorrelatedData<-function(r_matrix){
	return(cor(t(r_matrix)));
}
