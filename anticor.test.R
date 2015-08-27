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

test.simulation<-function(){
	dbConnection = createDbConnection();
	
	#p0 = generatePrice(n=100,mean=0,sd=5,p0=1000)
	p0 = loadData("ADVANC",from="2014-01-01",to="2015-12-31",dbConnection)$CLOSE
	relative_p0 = getRelativeReturn(p0)
	
	#p1 = generatePrice(n=100,mean=0,sd=10,p0=1000)
	p1 = loadData("BANPU",from="2014-01-01",to="2015-12-31",dbConnection)$CLOSE
	relative_p1 = getRelativeReturn(p1)
	
	#p2 = generatePrice(n=100,mean=0,sd=15,p0=1000)
	p2 = loadData("BCP",from="2014-01-01",to="2015-12-31",dbConnection)$CLOSE
	relative_p2 = getRelativeReturn(p2)
	
	#p3 = generatePrice(n=100,mean=0,sd=20,p0=1000)
	p3 = loadData("BH",from="2014-01-01",to="2015-12-31",dbConnection)$CLOSE
	relative_p3 = getRelativeReturn(p3)
	
	r = rbind(relative_p0,relative_p1,relative_p2,relative_p3)
	b = c(0.25,0.25,0.25,0.25)
	w = 3
	capital = 1000
	asset = c()
	
	#get total asset value when anti-cor is not ready
	asset = getCapitalValues(capital,r[,1:((2*w) - 1)],b)
	
	for(index in (2*w):(length(p0)-1)){
		m_r0 = r[,((index - w + 1) - w):(index - w)]
		m_r1 = r[,(index - w + 1):index]
		
		b = anticor.getNextPortfolio(w,m_r0,m_r1,b,index)
		b[which(b<0)] = 0
		
		last_asset_value = asset[length(asset)]
		new_asset_value = getCapitalValues(last_asset_value,as.matrix(r[,index+1]),b)
		asset = c(asset,new_asset_value)
	}
	
	benchmark = getCapitalValues(capital,r,c(0.25,0.25,0.25,0.25))
	
	p0 = p0/p0[1];
	p1 = p1/p1[1];
	p2 = p2/p2[1];
	p3 = p3/p3[1];
	asset = asset/asset[1];
	benchmark = benchmark/benchmark[1];
	
	all = c(p0,p1,p2,p3,asset,benchmark)
	plot(p0,type="l",col="green",ylim=c(min(all),max(all)))
	lines(p1,type="l",col="red")
	lines(p2,type="l",col="orange")
	lines(p3,type="l",col="black")
	lines(asset,type="l",col="blue")
	lines(benchmark,type="l",col="blue",lty=6)
}
