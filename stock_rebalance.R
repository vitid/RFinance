source("custom_lib.R")
source("Stock_Analysis.R")

doRebalance<-function(ratio,r0,r1){
    if(length(r0)!=length(r1)){
        print("The length of r0 and r1 should be equal!!")
        return()
    }
    equity = rep(0,times=length(r0))
    for(i in 1:length(equity)){
        if(i==1){
            previous_equity = 1
        }else{
            previous_equity = equity[i-1]
        }
        equity[i] = (r0[i] * previous_equity * ratio[1]) + (r1[i] * previous_equity * ratio[2])
    }
    
    plot(equity,type="l",main = paste("(Re-balance)ratio: ",ratio[1]," - ",ratio[2],sep=""))
}

doBuyAndHold<-function(ratio,r1,r2){
    equity = (ratio[1] * cumprod(r1)) + (ratio[2]*cumprod(r2))
    plot(equity,type="l",main = paste("(Buy&Hold)ratio: ",ratio[1]," - ",ratio[2],sep=""))
}

diff1 = rep(x = c(1,-1),times = 50)
diff2 = Lag(diff1,1) * 1
diff1[1] = 1
diff2[1] = 1

close1 = cumsum(diff1)
close2 = cumsum(diff2)
close1 = close1+10
close2 = close2+10

# close1 = loadData("PTT","2011-01-01","2015-12-31");
# close2 = loadData("KBANK","2011-01-01","2015-12-31");
# close1 = close1$CLOSE
# close2 = close2$CLOSE

#close1 = c(1,1,1,1,1,1,1,1,1,1)
#close2 = c(1,0.5,1,0.5,1,0.5,1,0.5,1,0.5)

r1 = close1/Lag(close1,1)
r1[1] = 1
r2 = close2/Lag(close2,1)
r2[1] = 1

plot(close1,type="l")
plot(close2,type="l")

for(i in seq(from=1,to=0,by=-0.1)){
    ratio = c(i,1.0-i)
    doRebalance(ratio,r1,r2)
}

