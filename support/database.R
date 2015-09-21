# local db connection
# 
# Author: vitid
###############################################################################
library(RMySQL);

createDbConnection<-function(dbUsername="root",dbPassword="settrade",dbname="stock_simulator",host="localhost"){
	dbConnection = dbConnect(MySQL(),user=dbUsername,password=dbPassword,dbname=dbname,host=host);
	return(dbConnection);
}

closeDbConnection<-function(dbConnection){
	dbDisconnect(dbConnection);
}

loadData<-function(symbols,from,to,dbConnection=createDbConnection()){
	symbols = sapply(symbols,function(name){return(paste("'",name,"'",sep=""));});
	symbols_txt = paste(symbols,collapse = ",",sep="")
	data = dbSendQuery(dbConnection,paste("select * from stock_daily_info where symbol in (",symbols_txt,") and date between '",from,"' and '",to,"' order by date ",sep=""))
	data = fetch(data,n=-1)
	return(data);
}
