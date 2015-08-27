library(RMySQL)
library(dplyr)
library(quantmod)
library(tseries)
library(forecast)
source("custom_lib.R")

# w         - window size
# m_r0,m_r1 - matrix of daily return of m stocks in w period
#             this is a matrix with m x w dimension
#             where each row is correspond to a return of each stock
# b         - vector of portfolio of m stocks
# index     - index of current trading day
# @return   - vector of portfolio of m stocks of the next trading day
anticor.getNextPortfolio<-function(w,m_r0,m_r1,b,index){
    
    if(index < 2*w){
        return(b);
    }
    
    num_instrument = nrow(m_r0)
    
    last_r = t(m_r1)[ncol(m_r1),]
    
    m_r0 = log10(m_r0);
    m_r0 = t(m_r0);
    
    m_r1 = log10(m_r1);
    m_r1 = t(m_r1);
    
    mean_r0 = colMeans(m_r0)
    mean_r1 = colMeans(m_r1)
    
    sd_r0 = apply(m_r0,2,sd)
    sd_r1 = apply(m_r1,2,sd)
    
    cov_matrix = matrix(rep(NA,times=num_instrument*num_instrument ) ,nrow = num_instrument,ncol = num_instrument );
    corr_matrix = matrix(rep(NA,times=num_instrument*num_instrument ) ,nrow = num_instrument,ncol = num_instrument );

    for(j in 1:ncol(cov_matrix) ){
        for(i in 1:nrow(cov_matrix) ){
            cov_matrix[i,j] = (1/(w-1)) * ( (m_r0[,i]-mean_r0[i]) %*% (m_r1[,j]-mean_r1[j]));
        }
    }
    
    for(j in 1:ncol(corr_matrix) ){
        for(i in 1:nrow(corr_matrix) ){
			if(sd_r0[i]==0 || sd_r1[j]==0){
				corr_matrix[i,j] = 0;
			}else{
				corr_matrix[i,j] = cov_matrix[i,j]/(sd_r0[i]*sd_r1[j]);
			}
        }
    }

    claim = matrix(rep(0,times=num_instrument*num_instrument) ,nrow = num_instrument,ncol = num_instrument );
    
    for(j in 1:ncol(claim) ){
        for(i in 1:nrow(claim) ){
            if(mean_r1[i] >= mean_r1[j] && corr_matrix[i,j] > 0){
                claim[i,j] = claim[i,j] + corr_matrix[i,j]
                
                if(corr_matrix[i,i]<0){
                    claim[i,j] = claim[i,j] - corr_matrix[i,i]
                }
                if(corr_matrix[j,j]<0){
                    claim[i,j] = claim[i,j] - corr_matrix[j,j]
                }
            }
        }
    }
    
    #adjust b according to the last trading date
    adjust_b = c()
    for(i in 1:length(last_r) ){
        adjust_b[i] = b[i] * last_r[i]
    }
    adjust_b = adjust_b / (b %*% last_r)
    
    rowsum_claim = rowSums(claim)
    transfer = matrix(rep(0,times=num_instrument*num_instrument) ,nrow = num_instrument,ncol = num_instrument );
    
    for(j in 1:ncol(claim) ){
        for(i in 1:nrow(claim) ){
            if(claim[i,j] == 0){
                transfer[i,j] = 0
            }else
            {
                transfer[i,j] = adjust_b[i] * claim[i,j] / rowsum_claim[i]
            }
        }
    }
    
    next_b = adjust_b
    for(i in 1:length(next_b) ){
        next_b[i] = next_b[i] - rowSums(transfer)[i] + colSums(transfer)[i]
    }
    
    return(next_b);
}

# r     - matrix of daily return of m stocks when starts adopting portfolio b
#         this is a matrix with m x w dimension
#         where each row is correspond to a return of each stock
# b		- initialized vector of portfolios to use at w(1) (length m). noted that b is not
#		  rebalanced for subsequent w
# @return - vector of entire capital values for w period
getCapitalValues<-function(current_value,r,b){
	for(i in 1:nrow(r)){
		r[i,] = cumprod(r[i,]) * b[i]
	}
	values = current_value * colSums(r)
	return(values);
}

generatePrice<-function(n,mean,sd,p0){
	diff = rnorm(n,mean=mean,sd=sd)
	diff[1] = 0
	return(p0 + cumsum(diff));
}

getRelativeReturn<-function(x){
	lag_x = as.vector(Lag(x,1))
	relative = x / lag_x
	#no definition for the first sequence
	relative[1] = 1
	return(relative);
}
