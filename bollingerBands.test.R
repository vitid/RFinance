source("support\\instrumentData.R");
source("support\\portfolioData.R");
source("support\\tsFunction.R");
source("support\\graphicFunction.R");

#suppress all warning messages...
options(warn=-1);

test.bollingerBands<-function(stock_name,from="2014-01-01",to="2016-01-01",capital=100,...){
	close = getDataMatrixFromDB(c(stock_name),from,to,"CLOSE")[1,];
	bb = BBands(close,...);
	down = bb[,"dn"];
	up = bb[,"up"];
	par(mfrow=c(2,1));
	plot(close,type="l");
	lines(down,col="red");
	lines(up,col="green");
	plot((up/down),type="l");
	abline(h=1.10,col="red");
}
