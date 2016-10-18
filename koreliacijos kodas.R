library("dplyr")
library("quantmod")
data=read.csv("WIKI_20161013.csv")
colnames(data)= c("Symbol","Date", "Open", "High", "Low", "Close", "Volume", "Ex-Dividend", "Split Ratio", "Adj. Open", "Adj. High", "Adj. Low", "Adj.Close", "Adj. Volume")
data=data %>% select(c(Symbol,Date,Adj.Close))


istraukimas = function(symbol){
  
  newdata = data[data$Symbol==symbol,-1]
  rownames(newdata) <- NULL
  colnames(newdata)=c("Date",symbol)
  return(newdata)
}

istraukimas2 = function(b){
  
a=seq(as.Date("2016-01-01"), as.Date("2017-01-01"), "day")
a=data.frame(a)
a[,1]=as.factor(a[,1])
colnames(a)="Date"

for (i in b){
  b=istraukimas(i)
  a=inner_join(a,b,by="Date")   
}
return(a)
}

portfelis = function(akcijos,svoriai){

p=istraukimas2(akcijos)
time=p[,1]
p2=data.frame(apply(p[,-1],2,diff))
p3=p[,-1]
p3=p3[-nrow(p3),]
santykis=p2/p3
santykis= santykis + 1 


portfelis = apply(santykis,1,function(i)svoriai*i)
portfelis = apply(portfelis,2,sum)
time=time[-1]
portfelis=data.frame(time,portfelis)
portf[,2]=as.numeric(portf[,2])
portf[,2]=portf[,-1]-1
return(portf)
}


apple = istraukimas("AAPL")
google = istraukimas("GOOG")

data$Date=as.Date(data$Date)
data=subset(data, Date > as.Date("2016-01-01"))

du=merge(apple, google, all=TRUE)
du=inner_join(apple,google,by="Date")
cor(du %>% select(c(Adj.Close.x,Adj.Close.y)))

write.csv(data, file = "akciju_kainos.csv",row.names=FALSE)


data=read.csv("akciju_kainos.csv")  





LMT= istraukimas("LMT")

akcijos=c("WMT","AMZN","FB","LMT")

svoriai = c(0.25,0.25,0.25,0.25)

portf=portfelis(akcijos,svoriai)




n=193
mass_istraukimas = function(n){
akcijos2=data[,1] %>%  unique       
tr=sapply(akcijos2,function(x) {ifelse((istraukimas(x) %>% nrow()) < n , FALSE, TRUE)})
akcijos3=akcijos2[which(tr)]
akcijos5=istraukimas2(akcijos3)
return(akcijos5)
}


p=akcijos5
time=p[,1]
p2=data.frame(apply(p[,-1],2,diff))
p3=p[,-1]
p3=p3[-nrow(p3),]
santykis_akciju=p2/p3
time=time[-1]
santykis_akciju=data.frame(time,santykis_akciju)


a=inner_join(portf,santykis_akciju,by="time") 
a=a[,-1]

b=apply(a[,-1],2,function(i)cor(a[,1],i))
d=data.frame (b[b<0])
c=data.frame(b)
colnames(c)=c("portfilis")

