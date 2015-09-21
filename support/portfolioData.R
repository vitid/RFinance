# Operations involve portfolio(portfolio adjustment, evaluate total asset, etc.)
# 
# Author: vitid
###############################################################################

# r     - matrix of daily return of m stocks when starts adopting portfolio b
#         this is a matrix with m x w dimension
#         where each row is correspond to a return of each stock
# b		- initialized vector of portfolios to use at w(1) (length m). noted that b is not
#		  rebalanced for subsequent w
# @return - vector of entire capital values for w period
getCapitalValues<-function(current_value,r,b){
	for(i in 1:nrow(r)){
		r[i,] = cumprod(r[i,]) * b[i];
	}
	values = current_value * colSums(r);
	return(values);
}

# Adjust current portfolio according to market's effect
# b		- vector of a current portfolio
# r		- matrix of relative returns (m x w dimension)
# @return - vector of adjusted portfolio when w periods have passed
getAdjustedPortfolio<-function(b,r){
	#convert r back from vector to matrix in case of r has only one column
	if(class(r) == "numeric"){
		r = as.matrix(r);
	}
	
	r_cumm_prod = apply(r,1,prod);
	
	adjust_b = (b * r_cumm_prod) / (b %*% r_cumm_prod);
	return(adjust_b);
}
