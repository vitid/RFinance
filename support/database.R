# TODO: Add comment
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
