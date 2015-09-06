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
