source("anticor.R")
source("custom_lib.R")

test.anticor.getNextPortfolio<-function(){
    m_r0=matrix(data=rnorm(24,mean=100,sd=10),nrow=4,ncol=6)
    m_r0 = m_r0 / 100
    m_r1=matrix(data=rnorm(24,mean=100,sd=10),nrow=4,ncol=6)
    m_r1 = m_r1 / 100
    next_b = anticor.getNextPortfolio(w=ncol(m_r0),m_r0,m_r1,b=rep(0.25,times=4),index=100)
    print(next_b)
}

test.getCapitalValues<-function(){
    current_value = 100
    r1 = c(0.9,1.1,1.2)
    r2 = c(1.0,0.9,0.8)
    r3 = c(0.8,1.1,0.7)
    r = rbind(r1,r2,r3)
    b = c(0.5,0.3,0.2)
    values = getCapitalValues(current_value,r,b)
    print(values)
	
}

test.generatePrice<-function(){
	p0 = generatePrice(n=100,mean=1,sd=5,p0=100)
	p1 = generatePrice(n=100,mean=1,sd=5,p0=100)
	plot(p0,type="l",col="green")
	lines(p1,type="l",col="red")
}

test.getRelativeReturn<-function(){
	price = c(1,2,4,8,16,64)
	getRelativeReturn(price)
}

test.simulation<-function(stocks,w=5,from="2014-01-01",to="2015-12-31",capital=100){
	dbConnection = createDbConnection();
	
	r = sapply(stocks,function(stock){
				close = loadData(stock,from=from,to=to,dbConnection)$CLOSE;
				return(close);
			});
	
	#if we get data with unequal length, r will be converted to list(instead of matrix)
	if(class(r) == "list"){
		print("some of the stocks have unequal data length");
		return();
	}
	
	for(col_index in 1:ncol(r)){
		r[,col_index] = getRelativeReturn(r[,col_index]);
	}
	r = t(r)
	
	uniform_b = rep(1/length(stocks),times=length(stocks))
	asset = c()
	
	#get total asset value when anti-cor is not ready
	b = uniform_b
	asset = getCapitalValues(capital,r[,1:((2*w) - 1)],b)
	
	for(index in (2*w):(ncol(r)-1)){
		m_r0 = r[,((index - w + 1) - w):(index - w)]
		m_r1 = r[,(index - w + 1):index]
		
		b = anticor.getNextPortfolio(w,m_r0,m_r1,b,index)
		b[which(b<0)] = 0
		
		last_asset_value = asset[length(asset)]
		new_asset_value = getCapitalValues(last_asset_value,as.matrix(r[,index+1]),b)
		asset = c(asset,new_asset_value)
	}
	
	benchmark = getCapitalValues(capital,r,uniform_b)
	
	test.generateResultGraph(asset,benchmark)
	
	closeDbConnection(dbConnection)
}

test.generateResultGraph <- function(asset,benchmark){
	asset = asset/asset[1]
	benchmark = benchmark/benchmark[1]
	
	min_value = min(min(asset),min(benchmark))
	max_value = max(max(asset),max(benchmark))
	
	plot(asset,type="l",col="blue",ylim=c(min_value,max_value))
	lines(benchmark,type="l",col="blue",lty=6)
}