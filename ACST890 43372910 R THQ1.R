#Question 1#
p <- function(C,F,y,n){
  t <- seq(0.5,n,0.5)
  p <- sum(C*exp(-y*t))+F*exp(-y[2*n])
  return(p)
}
#some simple examples to check the function#
#use fixed interest rate to check the function#
#example: 2 years, each half year's interest rate is 0.1#
y <- rep(0.1,4)
p(10,100,y,2)
#use variable interest rate to check the function#
#example:2 years, each half year's interest rate is 0.1,0.15,0.2,0.1#
y <- c(0.1,0.15,0.2,0.1)
p(10,100,y,2)


#Question 3(a)#
dataset <- read.csv(file="singapore.economy.csv",header=T)
#Quesion 3(b)#
dataset <- na.omit(dataset)
#Question 3(c)#
gdp <- dataset$gdp
Time <- dataset$time
as.ts(Time)
plot(Time,gdp,type='l',xlab='Time',ylab='GDP(%)',main='Singapore GDP growth')
#Question 3(d)#
period <- dataset$period
period <- as.factor(period)
m <- aggregate(gdp,by=list(period),FUN=mean)
s <- aggregate(gdp,by=list(period),FUN=sd)
stat.table <- cbind(m,s[2])
colnames(stat.table) <- c("Period","Mean","Stand Deviation")
stat.table
#Question 3(e)#
pairs(dataset[,3:10])
#Question 3(f)#
exp <- dataset$exp
SLRM1 <- lm(gdp~exp)
summary(SLRM1)      
#Question 3(g)#
epg <- dataset$epg
hpr <- dataset$hpr
oil <- dataset$oil
gdpus <- dataset$gdpus
crd <- dataset$crd
SLRM2 <- lm(gdp~exp+epg+hpr+oil+gdpus+crd)
summary(SLRM2)
#Question 3(h)#
#5% quantile of gdp#
q <- quantile(gdp,probs=0.05)
#put state into dataset#
state <- factor(ifelse(gdp<q,"crisis","normal"))
dataset <- data.frame(cbind(dataset,state))
#fit LR model up to year 2007#
training.data <- dataset[dataset$time<2008,]
LRM1 <- glm(state~bci,family=binomial,data=training.data)
summary(LRM1)
#compute the confusion matrix#
testing.data <- dataset[dataset$time>=2008,]
probility <- predict(LRM1,newdata=testing.data,type="response")
contrasts(state)
predit.state <- ifelse(probility<0.05,"crisis","normal")
confusion.table <- table(predit.state,testing.data$state)
confusion.table

