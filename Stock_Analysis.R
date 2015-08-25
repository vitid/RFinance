source("custom_lib.R")

dbUsername = "root";
dbPassword = "settrade";
dbname = "stock_simulator";
host = "localhost";

myDb = dbConnect(MySQL(),user=dbUsername,password=dbPassword,dbname=dbname,host=host)

loadData<-function(symbol,from,to){
        data = dbSendQuery(myDb,paste("select * from stock_daily_info where symbol = '",symbol,"' and date >= '",from,"' and date<='",to,"'",sep=""))
        data = fetch(data,n=-1)
        return(data);
}

plotAutoPattern <- function(pattern,outcome,lag,responseData){
        formula = "as.factor(outcome) ~ "
        for(i in 1:lag){
                if(i!=1){
                        formula = paste(formula,"+"); 
                }
                formula = paste(formula," as.factor(Lag(pattern,",i,"))")
        }
        fit = (glm(as.formula(formula),family = "binomial"));
        
        prob = predict(fit,type="response");
        temp = cbind(c(rep(NA,times=lag),prob),responseData);
        plot(temp[1:nrow(temp),1],temp[1:nrow(temp),2]);
        abline(h=0);
        abline(v=0.4);
        abline(v=0.6);  
        title(paste("lag:",lag));
}

data = loadData("SET","2015-01-01","2015-12-31");
#OHLC
oh = log(data$OPEN) - log(data$HIGH);
ol = log(data$OPEN) - log(data$LOW);
oc = log(data$OPEN) - log(data$CLOSE);
hl = log(data$HIGH) - log(data$LOW);
hc = log(data$HIGH) - log(data$CLOSE);
lc = log(data$LOW)- log(data$CLOSE);

p_data = cbind(oh,ol,oc,hl,hc,lc)

kmodel = kmeans(p_data,centers=5);
plot(data$CLOSE,col=kmodel$cluster)

numLag = 1;
diff_set = getDiff(log(data$CLOSE),lag=numLag,na.rm = F);
plot(diff_set,col=kmodel$cluster[1:length(diff_set)]);
plot(kmodel$cluster[1:length(diff_set)],y=diff_set);

updown = getUpDown(diff_set);

for(i in 1:10){
        plotAutoPattern(kmodel$cluster,updown,i,diff_set);
}

