library(RMySQL)
library(dplyr)
library(quantmod)
library(tseries)
library(forecast)

#suppress all warning messages...
options(warn=-1);

createDbConnection<-function(dbUsername="root",dbPassword="settrade",dbname="stock_simulator",host="localhost"){
	dbConnection = dbConnect(MySQL(),user=dbUsername,password=dbPassword,dbname=dbname,host=host)
	return(dbConnection);
}

closeDbConnection<-function(dbConnection){
	dbDisconnect(dbConnection)
}

loadData<-function(symbols,from,to,dbConnection=createDbConnection()){
	symbols = sapply(symbols,function(name){return(paste("'",name,"'",sep=""));});
	symbols_txt = paste(symbols,collapse = ",",sep="")
	data = dbSendQuery(dbConnection,paste("select * from stock_daily_info where symbol in (",symbols_txt,") and date between '",from,"' and '",to,"' order by date ",sep=""))
	data = fetch(data,n=-1)
	return(data);
}

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

#price_matrix - data.frame which have following columns:
#				c(price_0,price_1,price_2,...,price_n)
#				and each row is data of each instrument
#symbols		- vector consists of symbol names(lenght is thus 
#				  equal to price_matrix's number of row)
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

forwardAddition<-function(diff,numFw){
        r = diff
        for(i in 1:numFw){
                r = r + Lag(diff,i);
        }
        return(r);
}

getDiff<-function(data,lag,na.rm=T){
        data = data - Lag(data,lag);
        if(na.rm){
                data = na.omit(data);
        }
        return(data);
}

getUpDown<-function(data){
        r = ifelse(data < 0,"down","up");
        return(r);
}

blockBootstrap <- function(data,n=10){
        b_data = c();
        for(i in 1:n){
                indexs = sample(1:length(data),size=2,replace=F);
                index0 = min(indexs);
                index1 = max(indexs);
                new_data = data[index0:index1];
                if(length(b_data) > 0){
                        new_data = new_data + b_data[length(b_data)] - new_data[1];
                        new_data = new_data[2:length(new_data)];
                }
                b_data = c(b_data,new_data);
        }
        return (b_data);
}
