source("anticor.R");
source("custom_lib.R");

test.getDataMatrixFromDB <- function(stock_names,from="2014-01-01",to="2015-12-31",column="CLOSE"){
	dbConnection = createDbConnection();
	
	data = tryCatch({
				all_data = loadData(stock_names,from=from,to=to,dbConnection);
				all_data = fillMissingOHLCV(all_data);
				
				all_date = distinct(all_data,DATE)$DATE;
				all_date_count = length(all_date);
				
				data_matrix = matrix(0,nrow=length(stock_names),ncol=all_date_count);
				for(i in 1:(length(stock_names))){
					name = stock_names[i];
					data_matrix[i,] = filter(all_data,SYMBOL==name)[[column]];
				}
				
				data_matrix;
			}, error = function(e) {
				print("can't get relative matrix from DB");
				print(e);
				return(FALSE);
			}, finally = {
				closeDbConnection(dbConnection);
			});
	
	#if we get data with unequal length, r will be converted to list(instead of matrix)
	if(class(data) == "list"){
		print("some of the instruments have unequal data length");
		return(FALSE);
	}
	return(data);
}

test.getRelativeDataMatrixFromDB <- function(...){
	r = test.getDataMatrixFromDB(...);
		
	if(r == FALSE){
		print("can't extract relative data matrix");
		return();
	}
	
	for(i in 1:nrow(r)){
		r[i,] = getRelativeReturn(r[i,]);
	}
	
	return(r);
}

test.buyAndHoldAnticor<-function(r,max_w=5,is_fixed_width=FALSE,capital=100,is_return_b=FALSE,...){
	
	if(is_fixed_width){
		result = test.fixedWidthAnticor(r,max_w,capital,is_return_b);
		return(result);
	}else{
		l = list();
		asset_matrix_r = matrix(rep(NA,times=(max_w-1)*(ncol(r))),nrow=(max_w-1),ncol=ncol(r))
		for(i in 2:max_w){
			result = test.fixedWidthAnticor(r,i,capital,is_return_b);
			l[[i-1]] = result;
		}
		
		for(i in 1:length(l)){
			total_asset = l[[i]][,"total_asset"];
			asset_matrix_r[i,] = getRelativeReturn(total_asset);
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

test.testWithDB<-function(stock_names,from="2014-01-01",to="2015-12-31",max_w=5,is_fixed_width=FALSE,capital=100,is_return_b=FALSE,...){
	r = test.getRelativeDataMatrixFromDB(stock_names,from,to,column="CLOSE");
	
	if(!is.matrix(r)){
		print("can't generate relative-return matrix");
		return();
	}
	
	result = test.buyAndHoldAnticor(r,max_w=max_w,is_fixed_width = is_fixed_width,capital,is_return_b,...);
	
	num_instrument = nrow(r);
	uniform_b = rep(1/num_instrument,times=num_instrument)
	
	asset = result[,"total_asset"];
	benchmark = getCapitalValues(capital,r,uniform_b);
	
	if(is_return_b){
		par(mfrow = c( 2, 1 ));
	}else{
		par(mfrow = c( 1, 1 ));
	}
	
	test.generateBenchmarkGraph(asset,benchmark);
	
	if(is_return_b){
		test.generatePortfolioCompositionPlot(result[,2:ncol(result)]);
	}
}

#verify the correctness of the implemented algorithm
#tested data is from this site: https://www.t6labs.com/article/a_winning_portfolio_selection_algorithm/
test.testWithFile<-function(max_w=6,is_fixed_width=TRUE,file_path="SPDR.csv",capital=100,...){
	r = read.csv(file_path,header=TRUE,colClasses=c(c("NULL"),rep("numeric",times=9)));
	r = t(r);
		
	for(row_index in 1:nrow(r)){
		r[row_index,] = getRelativeReturn(r[row_index,]);
	}
		
	result = test.buyAndHoldAnticor(r,max_w,is_fixed_width,capital,...);
	
	num_instrument = nrow(r);
	uniform_b = rep(1/num_instrument,times=num_instrument)
	
	asset = result[,"total_asset"];
	benchmark = getCapitalValues(capital,r,uniform_b);
	
	test.generateBenchmarkGraph(asset,benchmark);
}

test.generateBenchmarkGraph <- function(asset,benchmark){
	asset = asset/asset[1]
	benchmark = benchmark/benchmark[1]
	
	min_value = min(min(asset),min(benchmark))
	max_value = max(max(asset),max(benchmark))
	
	plot(asset,type="l",col="blue",ylim=c(min_value,max_value))
	lines(benchmark,type="l",col="blue",lty=6)
}

#data - data.fram as returned from test.fixedWidthAnticor(...)
#       with column "total_asset" removed
test.generatePortfolioCompositionPlot <- function(data){
	data = as.matrix(t(data));
	#color index start from 2 because 1 is color black and it looks ugly...
	barplot(data,col=2:(nrow(data)+1));
}