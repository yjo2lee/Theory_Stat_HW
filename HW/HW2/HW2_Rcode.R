mu<-0.05
sigma<-0.3
n<-5
i<-0.02
r_target<-0.04
a<-0.5
set.seed(1234)
column <- data.frame(matrix(rnorm(1000*20,mean=0,sd=1), 1000, 20))
column$V0<- rep( 1,nrow(column)) 
column$V1<-column$V0*(1+((1-a)*i)+a*(exp(mu+sigma*column$X1)-1))
# (1+((1-a)*i)+a*exp(mu+sigma*column[1,2])-1)  to check if the matrix is right

column$V2<-column$V1*(1+((1-a)*i)+a*(exp(mu+sigma*column$X2)-1))
column$V3<-column$V2*(1+((1-a)*i)+a*(exp(mu+sigma*column$X3)-1))
column$V4<-column$V3*(1+((1-a)*i)+a*(exp(mu+sigma*column$X4)-1))
column$V5<-column$V4*(1+((1-a)*i)+a*(exp(mu+sigma*column$X5)-1))
column$V6<-column$V5*(1+((1-a)*i)+a*(exp(mu+sigma*column$X6)-1))
column$V7<-column$V6*(1+((1-a)*i)+a*(exp(mu+sigma*column$X7)-1))
column$V8<-column$V7*(1+((1-a)*i)+a*(exp(mu+sigma*column$X8)-1))
column$V9<-column$V8*(1+((1-a)*i)+a*(exp(mu+sigma*column$X9)-1))
column$V10<-column$V9*(1+((1-a)*i)+a*(exp(mu+sigma*column$X10)-1))
column$V11<-column$V10*(1+((1-a)*i)+a*(exp(mu+sigma*column$X11)-1))
column$V12<-column$V11*(1+((1-a)*i)+a*(exp(mu+sigma*column$X12)-1))
column$V13<-column$V12*(1+((1-a)*i)+a*(exp(mu+sigma*column$X13)-1))
column$V14<-column$V13*(1+((1-a)*i)+a*(exp(mu+sigma*column$X14)-1))
column$V15<-column$V14*(1+((1-a)*i)+a*(exp(mu+sigma*column$X15)-1))
column$V16<-column$V15*(1+((1-a)*i)+a*(exp(mu+sigma*column$X16)-1))
column$V17<-column$V16*(1+((1-a)*i)+a*(exp(mu+sigma*column$X17)-1))
column$V18<-column$V17*(1+((1-a)*i)+a*(exp(mu+sigma*column$X18)-1))
column$V19<-column$V18*(1+((1-a)*i)+a*(exp(mu+sigma*column$X19)-1))
column$V20<-column$V19*(1+((1-a)*i)+a*(exp(mu+sigma*column$X20)-1))
n<-as.numeric(n)
rsfc<- mean(ifelse ((log(column[1:n,21:(21+n)]))/n>log(1+r_target),1,0))
rsfc #1-a)-목표달성확률

#1-b)
library(ggplot2)
library(reshape2)
df<-data.frame(t(column[1:5,22:31]))
df$t<-1:10
meltdf <- melt(df,id="t")
ggplot(meltdf,aes(x=t,y=value,colour=variable,group=variable)) + geom_line()+xlab("Vt")+ggtitle("Vt(a)의 시계열도표")

#1-c)
for (a in seq(0,1,0.1)) {
  mu<-0.05
  sigma<-0.3
  n<-5
  i<-0.02
  r_target<-0.04
  set.seed(1234)
  column <- data.frame(matrix(rnorm(1000*20,mean=0,sd=1), 1000, 20))
  column$V0<- rep( 1,nrow(column)) 
  column$V1<-column$V0*(1+((1-a)*i)+a*(exp(mu+sigma*column$X1)-1))
  column$V2<-column$V1*(1+((1-a)*i)+a*(exp(mu+sigma*column$X2)-1))
  column$V3<-column$V2*(1+((1-a)*i)+a*(exp(mu+sigma*column$X3)-1))
  column$V4<-column$V3*(1+((1-a)*i)+a*(exp(mu+sigma*column$X4)-1))
  column$V5<-column$V4*(1+((1-a)*i)+a*(exp(mu+sigma*column$X5)-1))
  column$V6<-column$V5*(1+((1-a)*i)+a*(exp(mu+sigma*column$X6)-1))
  column$V7<-column$V6*(1+((1-a)*i)+a*(exp(mu+sigma*column$X7)-1))
  column$V8<-column$V7*(1+((1-a)*i)+a*(exp(mu+sigma*column$X8)-1))
  column$V9<-column$V8*(1+((1-a)*i)+a*(exp(mu+sigma*column$X9)-1))
  column$V10<-column$V9*(1+((1-a)*i)+a*(exp(mu+sigma*column$X10)-1))
  column$V11<-column$V10*(1+((1-a)*i)+a*(exp(mu+sigma*column$X11)-1))
  column$V12<-column$V11*(1+((1-a)*i)+a*(exp(mu+sigma*column$X12)-1))
  column$V13<-column$V12*(1+((1-a)*i)+a*(exp(mu+sigma*column$X13)-1))
  column$V14<-column$V13*(1+((1-a)*i)+a*(exp(mu+sigma*column$X14)-1))
  column$V15<-column$V14*(1+((1-a)*i)+a*(exp(mu+sigma*column$X15)-1))
  column$V16<-column$V15*(1+((1-a)*i)+a*(exp(mu+sigma*column$X16)-1))
  column$V17<-column$V16*(1+((1-a)*i)+a*(exp(mu+sigma*column$X17)-1))
  column$V18<-column$V17*(1+((1-a)*i)+a*(exp(mu+sigma*column$X18)-1))
  column$V19<-column$V18*(1+((1-a)*i)+a*(exp(mu+sigma*column$X19)-1))
  column$V20<-column$V19*(1+((1-a)*i)+a*(exp(mu+sigma*column$X20)-1))
  n<-as.numeric(n)
  print(c(a,mean(ifelse ((log(column[1:n,21:(21+n)]))/n>log(1+r_target),1,0))))
}


#1-d)
library(dplyr)
prob<-data.frame(
  "a"=seq(0,1,0.1),
  "prob"=
  c(0,0.0333,0.1333,0.1667,0.2,0.2667,0.3,0.3334,0.3334,0.3334,0.3334)
  )
prob %>% slice(which.max(prob))

#2-a)
stock<-read.csv("stockdata.csv")
stock<-stock[-c(1,2,3),]
colnames(stock)<-c("time","stock")
#options(digits=9)
stock$stock<-gsub(",","",stock$stock) #값에 있는 , 제거
stock$stock<-as.numeric(as.character(stock$stock))
stock$stock_ln<-log(stock$stock)
stock$time<-ts(stock$time, frequency=12, start=c(2004,1))

ggplot(stock,aes(x=time,y=stock)) + geom_line()+xlab("time")+ggtitle("Pt의 시계열도표")
ggplot(stock,aes(x=time,y=stock_ln)) + geom_line()+xlab("time")+ggtitle("ln(Pt)의 시계열도표")

#2-b)
stock$diff<-c(0,diff(stock$stock_ln))
ggplot(stock, aes(x=diff)) + geom_histogram()+ggtitle("로그수익률 히스토그램")
ggplot(stock,aes(x=time,y=diff)) + geom_line()+xlab("time")+ggtitle("로그수익률 시계열도표")

#2-c)
qqnorm(stock$diff)
qqline(stock$diff)

#2-d)
mu<-(sum(stock$diff)/nrow(stock))*12
sigma<-var(stock$diff)*12
n<-10
i<-0.02
r_target<-0.04
mu
sigma

for (a in seq(0,1,0.1)) {
  mu<-(sum(stock$diff)/nrow(stock))*12
  sigma<-var(stock$diff)*12
  n<-10
  i<-0.02
  r_target<-0.04
  set.seed(1234)
  column <- data.frame(matrix(rnorm(1000*20,mean=0,sd=1), 1000, 20))
  column$V0<- rep( 1,nrow(column)) 
  column$V1<-column$V0*(1+((1-a)*i)+a*(exp(mu+sigma*column$X1)-1))
  column$V2<-column$V1*(1+((1-a)*i)+a*(exp(mu+sigma*column$X2)-1))
  column$V3<-column$V2*(1+((1-a)*i)+a*(exp(mu+sigma*column$X3)-1))
  column$V4<-column$V3*(1+((1-a)*i)+a*(exp(mu+sigma*column$X4)-1))
  column$V5<-column$V4*(1+((1-a)*i)+a*(exp(mu+sigma*column$X5)-1))
  column$V6<-column$V5*(1+((1-a)*i)+a*(exp(mu+sigma*column$X6)-1))
  column$V7<-column$V6*(1+((1-a)*i)+a*(exp(mu+sigma*column$X7)-1))
  column$V8<-column$V7*(1+((1-a)*i)+a*(exp(mu+sigma*column$X8)-1))
  column$V9<-column$V8*(1+((1-a)*i)+a*(exp(mu+sigma*column$X9)-1))
  column$V10<-column$V9*(1+((1-a)*i)+a*(exp(mu+sigma*column$X10)-1))
  column$V11<-column$V10*(1+((1-a)*i)+a*(exp(mu+sigma*column$X11)-1))
  column$V12<-column$V11*(1+((1-a)*i)+a*(exp(mu+sigma*column$X12)-1))
  column$V13<-column$V12*(1+((1-a)*i)+a*(exp(mu+sigma*column$X13)-1))
  column$V14<-column$V13*(1+((1-a)*i)+a*(exp(mu+sigma*column$X14)-1))
  column$V15<-column$V14*(1+((1-a)*i)+a*(exp(mu+sigma*column$X15)-1))
  column$V16<-column$V15*(1+((1-a)*i)+a*(exp(mu+sigma*column$X16)-1))
  column$V17<-column$V16*(1+((1-a)*i)+a*(exp(mu+sigma*column$X17)-1))
  column$V18<-column$V17*(1+((1-a)*i)+a*(exp(mu+sigma*column$X18)-1))
  column$V19<-column$V18*(1+((1-a)*i)+a*(exp(mu+sigma*column$X19)-1))
  column$V20<-column$V19*(1+((1-a)*i)+a*(exp(mu+sigma*column$X20)-1))
  n<-as.numeric(n)
  print(c(a,mean(ifelse ((log(column[1:n,21:(21+n)]))/n>log(1+r_target),1,0))))
}
prob2<-data.frame(
  "a"=seq(0,1,0.1),
  "prob"=
    c(0,0,0,0.0182,0.1182,0.2273,0.2909,0.3455,0.4,0.4364,0.4636)
)
prob2 %>% slice(which.max(prob))
