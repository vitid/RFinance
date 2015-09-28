source("anticor.R");
source("support\\instrumentData.R");
source("support\\portfolioData.R");
source("support\\tsFunction.R");
source("support\\graphicFunction.R");

#suppress all warning messages...
options(warn=-1);

#Interface to test Anti-Cor strategy
test.basketAnticor<-function(r,max_w=5,is_fixed_width=FALSE,capital=100,is_return_b=FALSE,...){
	
	if(is_fixed_width){
		result = test.fixedWidthAnticor(r,max_w,capital,is_return_b,...);
		return(result);
	}else{
		l = list();
		asset_matrix_r = matrix(rep(NA,times=(max_w-1)*(ncol(r))),nrow=(max_w-1),ncol=ncol(r))
		for(i in 2:max_w){
			#set init_uniform_length of each sub anti-cor = init_uniform_length of anti-cor with max_w for applicability reason
			result = test.fixedWidthAnticor(r,i,capital,is_return_b,init_uniform_length = (2*max_w),...);
			l[[i-1]] = result;
		}
		
		for(i in 1:length(l)){
			total_asset = l[[i]][,"total_asset"];
			asset_matrix_r[i,] = getRelative(total_asset);
		}
		uniform_w_b = rep(1/(max_w-1),times=(max_w-1));
		asset = getCapitalValues(capital,asset_matrix_r,uniform_w_b);
		result = data.frame(total_asset=asset);
		
		if(is_return_b){
			l_b = lapply(l,function(data){
						data[,"total_asset"] * data[,c(2:ncol(data))];
					});
			total_b = Reduce("+",l_b);
			l_asset = lapply(l,function(data){
						data[,"total_asset"];
					});
			sum_total_asset = Reduce("+",l_asset);
			weighted_b = total_b / sum_total_asset;
			result = cbind(result,weighted_b);
		}
		
		return(result);
	}
	
}

#Should be called via test.basketAnticor(...)
#@return	data.frame, which has following column:
#			(if is_return_b == TRUE): c(total_asset,b_of_instrument_1,b_of_instrument_2,...,b_of_instrument_i)
#			or
#			(if is_return_b == FALSE): c(total_asset)
#			each row account for data in that period
test.fixedWidthAnticor<-function(r,w,capital,is_return_b=FALSE,update_interval=1,init_uniform_length=(2*w)){
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
	asset = getCapitalValues(capital,r[,1:init_uniform_length],b)
	
	if(is_return_b){
		#set uniform portfolio value to data from the begining period to a period before using anti-cor
		#skip the first column because that is total_asset column
		return.data[1:(2*w),2:(num_instrument+1)] = b[1];
	}
	
	#don't need to delay for the first update
	unupdate_days = update_interval;
	#this is an actual anti-cor portfolio(rebalancing daily with no delay interval)
	b_daily = b;
	#this portfolio doesn't perform rebalancing for specified interval
	b_interval = b;
	for(index in init_uniform_length:(ncol(r)-1)){
		
		unupdate_days = unupdate_days + 1;
		
		m_r0 = r[,((index - w + 1) - w):(index - w)]
		m_r1 = r[,(index - w + 1):index]
		
		b_daily = anticor.getNextPortfolio(w,m_r0,m_r1,b_daily,index);
		
		if(unupdate_days >= update_interval){
			b = b_daily;
			#adjust interval rebalancing portfolio to catch up with the daily rebalancing portfolio
			b_interval = b_daily;
			unupdate_days = 0;
		}else{
			#not rebalancing portfolio, just use portfolio that is affected by market prices in the previous day
			b = b_interval;
		}
		b[which(b<0)] = 0
		
		if(is_return_b){
			return.data[index,2:(num_instrument+1)] = b;
		}
		
		last_asset_value = asset[length(asset)]
		new_asset_value = getCapitalValues(last_asset_value,as.matrix(r[,index+1]),b)
		asset = c(asset,new_asset_value)
		
		#update portfolios
		b_daily = getAdjustedPortfolio(b_daily,r[,index+1]);
		b_interval = getAdjustedPortfolio(b_interval,r[,index+1]);
	}
	
	return.data[,"total_asset"] = asset;
	return(return.data);
}

test.testWithDB<-function(stock_names,from="2014-01-01",to="2016-01-01",max_w=5,is_fixed_width=FALSE,capital=100,is_return_b=FALSE,...){
	r = getRelativeDataMatrixFromDB(stock_names,from,to,column="CLOSE");
	
	if(!is.matrix(r)){
		print("can't generate relative-return matrix");
		return();
	}
	
	result = test.basketAnticor(r,max_w=max_w,is_fixed_width = is_fixed_width,capital,is_return_b,...);
	
	num_instrument = nrow(r);
	uniform_b = rep(1/num_instrument,times=num_instrument)
	
	asset = result[,"total_asset"];
	benchmark = getCapitalValues(capital,r,uniform_b);
	
	if(is_return_b){
		par(mfrow = c( 2, 1 ));
	}else{
		par(mfrow = c( 1, 1 ));
	}
	
	GF.generateBenchmarkGraph(asset,benchmark);
	
	if(is_return_b){
		GF.generatePortfolioCompositionPlot(result[,2:ncol(result)]);
	}
}

#verify the correctness of the implemented algorithm
#tested data is from this site: https://www.t6labs.com/article/a_winning_portfolio_selection_algorithm/
test.testWithFile<-function(max_w=6,is_fixed_width=TRUE,file_path="data//SPDR.csv",capital=100,...){
	r = read.csv(file_path,header=TRUE,colClasses=c(c("NULL"),rep("numeric",times=9)));
	r = t(r);
		
	for(row_index in 1:nrow(r)){
		r[row_index,] = getRelative(r[row_index,]);
	}
		
	result = test.basketAnticor(r,max_w,is_fixed_width,capital,...);
	
	num_instrument = nrow(r);
	uniform_b = rep(1/num_instrument,times=num_instrument)
	
	asset = result[,"total_asset"];
	benchmark = getCapitalValues(capital,r,uniform_b);
	
	GF.generateBenchmarkGraph(asset,benchmark);
}
