library("dplyr")
library("quantmod")
library(data.table)
library(Quandl)
library("ggplot2")
Quandl.api_key("rCXPt32dg7QbyQyuZ4xx")

##duomenu sumazinimas ir irasymas
{

data=read.csv("WIKI_20161013.csv")
colnames(data)= c("Symbol","Date", "Open", "High", "Low", "Close", "Volume", "Ex-Dividend", "Split Ratio", "Adj. Open", "Adj. High", "Adj. Low", "Adj.Close", "Adj. Volume")
data=data %>% select(c(Symbol,Date,Adj.Close))
data$Date=as.Date(data$Date)
data2=subset(data, Date < as.Date("2016-01-01") &  Date >= as.Date("2015-01-01"))

write.csv(data2, file = "akciju_kainos2015.csv",row.names=FALSE)

}

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
portfelis [,2]=as.numeric(portfelis [,2])
portfelis [,2]=portfelis [,-1]-1
return(portfelis)
}





n=193
mass_istraukimas = function(n){
akcijos2=data[,1] %>%  unique       
tr=sapply(akcijos2,function(x) {ifelse((istraukimas(x) %>% nrow()) < n , FALSE, TRUE)})
akcijos3=akcijos2[which(tr)]
akcijos5=istraukimas2(akcijos3)
return(akcijos5)
}

p=mass_istraukimas(n)
write.csv(p, file = "n_193__2015.csv",row.names=FALSE)




data=read.csv("akciju_kainos.csv")  


LMT= istraukimas("LMT")

akcijos=c("WMT","AMZN","FB","LMT")

svoriai = c(0.25,0.25,0.25,0.25)

portf=portfelis(akcijos,svoriai)




p=read.csv("n_193__2016.csv")

time=p[,1]
p2=data.frame(apply(p[,-1],2,diff))
p3=p[,-1]
p3=p3[-nrow(p3),]
santykis_akciju=p2/p3
time=time[-1]
santykis_akciju=data.frame(time,santykis_akciju)


a=inner_join(portf,santykis_akciju,by="time") 


#g=lapply(names(a[,-(1:2)]),function(i)which(i %in% names(c[,-2])))



akc = melt(a[,c("time","portfelis")], id=c("time"))
ggplot(akc) + geom_line(aes(x=time, y=value, colour=variable,group=1)) +
  scale_colour_manual(values=c(2))+ylab("% pokytis")+
  guides(col=guide_legend(title="Kintamieji"))

akc = melt(a[,c("time","JONE")], id=c("time"))
ggplot(akc) + geom_line(aes(x=time, y=value, colour=variable,group=1)) +
  scale_colour_manual(values=c(1))+ylab("% pokytis")+
  guides(col=guide_legend(title="Kintamieji"))




b=a[,-1]





b=apply(b[,-1],2,function(i)cor(b[,1],i,method = "spearman"))
c=data.frame (b[b<0])
colnames(c)=c("portfilis")
setDT(c, keep.rownames = TRUE)[]

e=apply(b[,-1],2,function(i)cor.test(b[,1],i,method = "spearman")$p.value)
e=data.frame(e[e>0.05])
setDT(c, keep.rownames = TRUE)[]
c=data.frame(c)
setDT(e, keep.rownames = TRUE)[]
e=data.frame(e)


lapply(c[,1],function(i)i %in% e[,1])
b=apply(a[,-1],2,function(i)i-a[,1])



d=data.frame(cbind(a[,1],b))
akc = melt(d[,c("V1","ALR")], id=c("V1"))
ggplot(akc) + geom_line(aes(x=V1, y=value, colour=variable,group=1)) +
  scale_colour_manual(values=c(1:2))+ylab("% pokytis")+
  guides(col=guide_legend(title="Kintamieji"))







setDT(e, keep.rownames = TRUE)[]
setDT(c, keep.rownames = TRUE)[]
setDT(d, keep.rownames = TRUE)[]
du=inner_join(e,c,by="rn")
tri=anti_join(c,e,by="rn")
ketv=anti_join(d,e,by="rn")

tri=data.frame(tri)
penk=tri[which(tri[,2]<0.2),]

getSymbols("SPY",src="google")
SPY = SPY[,4]




data=read.csv("akcijos_geroje_lenteleje.csv")
a=rollapply(data[,-1], width = 30, FUN = mean, align = "left")
b=rollapply(data[,-1], width = 30, FUN = sd, align = "left")
newdata=data[-c(1:29),]
rownames(newdata)=NULL

mutate(data,naujas=NA)
