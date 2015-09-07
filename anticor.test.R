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

test.getRelativeMatrixFromDB <- function(stock_names,from="2014-01-01",to="2015-12-31"){
	dbConnection = createDbConnection();
	
	r = tryCatch({
		all_data = loadData(stock_names,from=from,to=to,dbConnection);
		all_data = fillMissingOHLCV(all_data);
		
		all_date = distinct(all_data,DATE)$DATE;
		all_date_count = length(all_date);
		
		price_matrix = matrix(0,nrow=length(stock_names),ncol=all_date_count);
		for(i in 1:(length(stock_names))){
			name = stock_names[i];
			price_matrix[i,] = filter(all_data,SYMBOL==name)$CLOSE;
		}
		
		price_matrix;
	}, error = function(e) {
			print("can't get relative matrix from DB");
			return(FALSE);
	}, finally = {
		closeDbConnection(dbConnection);
	});
		
	#if we get data with unequal length, r will be converted to list(instead of matrix)
	if(class(r) == "list"){
		print("some of the instruments have unequal data length");
		return(FALSE);
	}
	
	for(i in 1:nrow(r)){
		r[i,] = getRelativeReturn(r[i,]);
	}
	
	return(r);
}

test.buyAndHoldAnticor<-function(r,max_w=5,is_fixed_width=FALSE,capital=100){
	
	num_instrument = nrow(r);
	uniform_b = rep(1/num_instrument,times=num_instrument)
	
	if(is_fixed_width){
		result = test.fixedWidthAnticor(r,max_w,capital);
		asset = result[,"total_asset"]
	}else{
		l = list();
		asset_matrix_r = matrix(rep(NA,times=(max_w-1)*(ncol(r))),nrow=(max_w-1),ncol=ncol(r))
		for(i in 2:max_w){
			result = test.fixedWidthAnticor(r,i,capital);
			l[[i-1]] = result;
		}
		
		for(i in 1:length(l)){
			total_asset = l[[i]][,"total_asset"];
			asset_matrix_r[i,] = getRelativeReturn(total_asset);
		}
		uniform_w_b = rep(1/(max_w-1),times=(max_w-1))
		asset = getCapitalValues(capital,asset_matrix_r,uniform_w_b)
	}
	
	benchmark = getCapitalValues(capital,r,uniform_b)
	
	test.generateResultGraph(asset,benchmark);
}

test.testWithDB<-function(stock_names,from="2014-01-01",to="2015-12-31",max_w=5,is_fixed_width=FALSE){
	r = test.getRelativeMatrixFromDB(stock_names,from,to);
	
	if(!is.matrix(r)){
		print("can't generate relative-return matrix");
		return();
	}
	
	test.buyAndHoldAnticor(r,max_w=max_w,is_fixed_width = is_fixed_width,capital = 100);
}

#verify the correctness of the implemented algorithm
#tested data is from this site: https://www.t6labs.com/article/a_winning_portfolio_selection_algorithm/
test.testWithFile<-function(max_w=6,is_fixed_width=TRUE,capital=100,file_path="SPDR.csv"){
	r = read.csv(file_path,header=TRUE,colClasses=c(c("NULL"),rep("numeric",times=9)));
	r = t(r);
		
	for(row_index in 1:nrow(r)){
		r[row_index,] = getRelativeReturn(r[row_index,]);
	}
		
	test.buyAndHoldAnticor(r,max_w,is_fixed_width,capital)
}

#@return	data.frame, which has following column:
#			(if is_return_b == TRUE): c(total_asset,b_of_instrument_1,b_of_instrument_2,...,b_of_instrument_i)
#			or
#			(if is_return_b == FALSE): c(total_asset)
#			each row account for data in that period
test.fixedWidthAnticor<-function(r,w,capital,is_return_b=FALSE){
	
	num_instrument = nrow(r);
	num_period = ncol(r);
	uniform_b = rep(1/num_instrument,times=num_instrument)
	asset = c()
	
	return.data = data.frame("total_asset"=numeric(num_period));
	if(is_return_b){
		for(i in 1:num_instrument){
			return.data[,as.character(i)] = numeric(num_period);
		}
	}
	
	#get total asset value when anti-cor is not ready
	b = uniform_b
	asset = getCapitalValues(capital,r[,1:((2*w))],b)
	
	if(is_return_b){
		#set uniform portfolio value to data from the begining period to a period before using anti-cor
		#skip the first column because that is total_asset column
		return.data[1:(2*w),2:(num_instrument+1)] = b[1];
	}
	
	for(index in (2*w):(ncol(r)-1)){
		m_r0 = r[,((index - w + 1) - w):(index - w)]
		m_r1 = r[,(index - w + 1):index]
		
		b = anticor.getNextPortfolio(w,m_r0,m_r1,b,index)
		b[which(b<0)] = 0
		
		if(is_return_b){
			return.data[index,2:(num_instrument+1)] = b;
		}
		
		last_asset_value = asset[length(asset)]
		new_asset_value = getCapitalValues(last_asset_value,as.matrix(r[,index+1]),b)
		asset = c(asset,new_asset_value)
		b = getAdjustedPortfolio(b,r[,index+1])
	}
		
	return.data[,"total_asset"] = asset;
	return(return.data);
}

test.generateResultGraph <- function(asset,benchmark){
	asset = asset/asset[1]
	benchmark = benchmark/benchmark[1]
	
	min_value = min(min(asset),min(benchmark))
	max_value = max(max(asset),max(benchmark))
	
	plot(asset,type="l",col="blue",ylim=c(min_value,max_value))
	lines(benchmark,type="l",col="blue",lty=6)
}