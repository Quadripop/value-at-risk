library('openxlsx')
library('quantmod')
library("readxl")
library("tidyverse")
library(ggplot2)
library(lubridate)
library(gridExtra)
WTI<-read_excel("WTI.xlsx")
NG<-read_excel("NG.xlsx")
WTI$Dates<-as.Date(WTI$Dates,format="%m-%d-%y")
NG$Dates<-as.Date(NG$Dates,format="%m-%d-%y")
#computing daily returns for WTI
options(digits=5)
WTI<-WTI %>% mutate(daily_return=WTI$price/lag(WTI$price) -1) %>% 
  mutate(id=row_number())
WTI<-na.omit(WTI)
#computing daily returns for NG
NG<-NG %>% mutate(daily_return=NG$price/lag(NG$price) -1) %>% 
  mutate(id=row_number())
NG<-na.omit(NG)

#combining assets
portfolio_return<-merge(NG,WTI, by= "id")
#deleting duplicate dates columns
portfolio_return<-portfolio_return[,-5]
#renaming date column
names(portfolio_return)[2]<-"Dates"

#Adding combined weighted daily returns of portfolio

portfolio_return<-portfolio_return %>% 
  mutate(portfolioReturn= 0.5*daily_return.x+0.5*daily_return.y)
portfolio_return<- filter(portfolio_return,year(Dates)>1999)

#plotting the time series of portfolio daily returns
p1<-portfolio_return %>%ggplot()+geom_line(aes(Dates,portfolioReturn,colour="portfolio"))+
  geom_line(aes(Dates,daily_return.x,colour="NG"))+
  geom_line(aes(Dates,daily_return.y,colour="WTI"))+
  ylim(c(-0.2,0.2))+labs(title="Plots of daily returns",
                         x="Dates",
                         y="daily returns",
                         colour="Assets",)+
  theme_minimal()+theme(legend.position = "right")
# plot of timeseries of portfolio                      
p2<-portfolio_return %>% ggplot(aes(Dates,portfolioReturn))+geom_line()+
  labs(title="Time series of daily returns of portfolio")

#plot of time series of WTI
p3<-portfolio_return %>% ggplot(aes(Dates,daily_return.y,colour="red"))+geom_line()+
  labs(title="Time series of daily returns of WTI")

#plot of time series of NG
p4<-portfolio_return %>% ggplot(aes(Dates,daily_return.x,colour="blue"))+geom_line()+
  labs(title="Time series of daily returns of NG")
grid.arrange(p1,p2,p3,p4,nrow=2,ncol=2)
#plotting histogram of returns
par(mfrow=c(1,2))
hist(portfolio_return$portfolioReturn,breaks=50,main="Histogram of portfolio returns",
     xlim=c(-0.2,0.2));curve(80*dnorm(x,mean=mean(portfolio_return$portfolioReturn),
                                      sd=sd(portfolio_return$portfolioReturn)),
                             from=-0.2, to=0.2,add=TRUE,col="red")
plot(density(portfolio_return$portfolioReturn),main="density plot");
curve(dnorm(x,mean=mean(portfolio_return$portfolioReturn),
            
            sd=sd(portfolio_return$portfolioReturn)),
      
      from=-0.2, to=0.2,add=TRUE,col="red")
#checking autocorrelations
par(mfrow=c(2,2))

acf(portfolio_return$portfolioReturn, main="Return ACF")
pacf(portfolio_return$portfolioReturn,main="Return PACF")
acf(portfolio_return$portfolioReturn^2,main="squared return ACF")
pacf(portfolio_return$portfolioReturn^2,main="squared return PACF")

qqnorm(portfolio_return$portfolioReturn);qqline(portfolio_return$portfolioReturn)
#Estimating volatility
#Garch(1,1) 
par(mfrow=c(1,1))
library(rugarch)
#specifying garch(1,1) model
garch11.spec<-ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1)),
                         mean.model=list(armaOrder=c(0,0)))
#fitting garch(1,1) model
garch11.fit<-ugarchfit(spec=garch11.spec,data=portfolio_return$portfolioReturn)
coef(garch11.fit)

#Adding volatility estimates to portfolio return df
portfolio_return<-portfolio_return %>% 
  mutate(garchvol=garch11.fit@fit[["sigma"]]) 
#graphing vol
portfolio_return %>% ggplot(aes(Dates,garchvol))+geom_line()+
  ylim(c(0.01,0.08))

#plot acf
plot(garch11.fit, which = 3)

# Check autocorrelation in residuals
acf(residuals(garch11.fit, standardize = TRUE), main = "ACF of Standardized Residuals")



#egarch
egarch11.spec<-ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1)),
                          mean.model=list(armaOrder=c(0,0)))
egarch11.fit<-ugarchfit(spec=egarch11.spec,data=portfolio_return$portfolioReturn)
coef(egarch11.fit)



#Adding egarch volatility estimates to portfolio return df
portfolio_return<-portfolio_return %>% 
  mutate(egarchvol=egarch11.fit@fit[["sigma"]]) 

#graphing egarch vol

portfolio_return %>% ggplot(aes(Dates,egarchvol))+geom_line()+
  ylim(c(0.01,0.08))
#plot acf
plot(egarch11.fit, which = 3)

# Check autocorrelation in residuals
acf(residuals(egarch11.fit, standardize = TRUE), main = "ACF of Standardized Residuals")

#Tgarch

tgarch11.spec<-ugarchspec(variance.model=list(model="fGARCH",submodel="TGARCH",
                                              garchOrder=c(1,1)),mean.model=list(armaOrder=c(0,0)))

tgarch11.fit<-ugarchfit(spec=tgarch11.spec,data=portfolio_return$portfolioReturn)
coef(tgarch11.fit)


#Adding tgarch volatility estimates to portfolio return df
portfolio_return<-portfolio_return %>% 
  mutate(tgarchvol=tgarch11.fit@fit[["sigma"]]) 

#graphing tgarchvol
portfolio_return %>% ggplot(aes(Dates,tgarchvol))+geom_line()+
  ylim(c(0.01,0.08))

#graphing acf 

plot(tgarch11.fit, which = 3)

# Check autocorrelation in residuals
acf(residuals(tgarch11.fit, standardize = TRUE), main = "ACF of Standardized Residuals")

#model comparison using AIC and BIC
aic_garch11 <- infocriteria(garch11.fit)[1]
bic_garch11 <- infocriteria(garch11.fit)[2]

aic_egarch <- infocriteria(egarch11.fit)[1]
bic_egarch <- infocriteria(egarch11.fit)[2]

aic_tgarch<- infocriteria(tgarch11.fit)[1]
bic_tgarch<- infocriteria(tgarch11.fit)[2]



data.frame(Model = c("GARCH(1,1)", "EGARCH","TGARCH"),
           AIC = c(aic_garch11, aic_egarch,aic_tgarch),
           BIC = c(bic_garch11, bic_egarch,bic_tgarch))

#calculating 5% daily VAR tgarch

z_val<-qnorm(0.05,lower.tail=FALSE)
portfolio_value<-10e6

portfolio_return<- portfolio_return %>% 
  mutate(tgarchVar=-z_val*portfolio_return$tgarchvol) %>% 
  mutate(tgarch_var=tgarchVar*portfolio_value)
#write.csv(portfolio_return,"portfolio_VAR.csv")

#calculating 5% daily VAR egarch
portfolio_return<-portfolio_return %>% 
  mutate(egarchVar=-z_val* portfolio_return$egarchvol) %>% 
  mutate(egarch_var=egarchVar*portfolio_value)

#calculating 5% daily VAR garch11
portfolio_return<-portfolio_return %>% 
  mutate(garch_11_Var=-z_val* portfolio_return$garchvol) %>% 
  mutate(garch_11_var=garch_11_Var*portfolio_value)





#calculating 1% daily VAR
z_val<-qnorm(0.01,lower.tail=FALSE)
portfolio_value<-10e6

portfolio_return<- portfolio_return %>% 
  mutate(tgarchVar2=-z_val*portfolio_return$tgarchvol) %>% 
  mutate(tgarch_var2=tgarchVar2*portfolio_value)

#calculating 1% daily VAR egarch
portfolio_return<-portfolio_return %>% 
  mutate(egarchVar2=-z_val* portfolio_return$egarchvol) %>% 
  mutate(egarch_var2=egarchVar2*portfolio_value)


#calculating 1% daily VAR garch11
portfolio_return<-portfolio_return %>% 
  mutate(garch_11_Var2=-z_val* portfolio_return$garchvol) %>% 
  mutate(garch_11_var2=garch_11_Var2*portfolio_value)

#Adding actual loss to dataframe 
portfolio_return<-portfolio_return %>% mutate(actual_loss=portfolioReturn*portfolio_value)

# Bactesting using tgarch
exceptions_95_tgarch<-portfolio_return_segmented$actual_loss<portfolio_return$tgarch_var
exceedance_95<-sum(exceptions_95_tgarch)
exceptions_99_tgarch<-portfolio_return$actual_loss<portfolio_return$tgarch_var2
exceedance_99<-sum(exceptions_99_tgarch)

# Bactesting using egarch
exceptions_95_egarch<-portfolio_return$actual_loss<portfolio_return$egarch_var
exceedance2_95<-sum(exceptions_95_egarch)
exceptions_99_egarch<-portfolio_return$actual_loss<portfolio_return$egarch_var2
exceedance2_99<-sum(exceptions_99_egarch)

# Bactesting using garch(1,1)
exceptions_95_garch_11<-portfolio_return$actual_loss<portfolio_return$garch_11_var
exceedance3_95<-sum(exceptions_95_garch_11)
exceptions_99_garch_11<-portfolio_return$actual_loss<portfolio_return$garch_11_var2
exceedance3_99<-sum(exceptions_99_garch_11)

#plot of portfolio return and different garchVAR_95%
ggplot()+geom_line(data=portfolio_return,aes(Dates,tgarchVar,colour='TGARCH VAR'))+
  geom_line(data=portfolio_return,aes(Dates,portfolioReturn,colour='Portfolio return'))+
  geom_line(data=portfolio_return,aes(Dates,egarchVar,colour='EGARCH VAR'))+
  geom_line(data=portfolio_return,aes(Dates,garch_11_Var,colour='GARCH(1,1) VAR'))+
  labs(title="portfolio return and different garch 95% var",
       x="Dates",
       y="value",
       colour="Model")+
  theme_minimal()+
  theme(legend.position = "right") 

#plot of portfolio return and different garchVAR_99%
ggplot()+geom_line(data=portfolio_return,aes(Dates,tgarchVar2,colour='TGARCH VAR2'))+
  geom_line(data=portfolio_return,aes(Dates,portfolioReturn,colour='Portfolio return'))+
  geom_line(data=portfolio_return,aes(Dates,egarchVar2,colour='EGARCH VAR2'))+
  geom_line(data=portfolio_return,aes(Dates,garch_11_Var2,colour='GARCH(1,1) VAR2'))+
  labs(title="portfolio return and different garch 99% var",
       x="Dates",
       y="value",
       colour="Model")+
  theme_minimal()+
  theme(legend.position = "right") 

backtest_table<-matrix(c(295.5,exceedance3_95,exceedance2_95,exceedance_95,
                         59.1,exceedance3_99,exceedance2_99,exceedance_99),
                       nrow=2,byrow=TRUE)

rownames(backtest_table)<-c("5% VAR","1% VAR")
colnames(backtest_table)<-c("Expectations","Garch(1,1)",
                            "EGarch","TGarch")
knitr::kable(backtest_table)

#Kupeic test

kupeic_test <- function(T, x, sig_level) {
  # T: Total number of observations
  # x: Number of exceptions (actual losses > VaR)
  # sig_level: Confidence level of the VaR model (e.g., 0.99 for 99%)
  
  # Calculate observed and expected exception rates
  p_hat <- x/T     # Observed exception rate
  p <- 1 - sig_level   # Expected exception rate
  
  
  # Kupiec likelihood ratio test statistic
  LR <- -2 * ((T - x) * log((1 - p) / (1 - p_hat)) + x * log(p / p_hat))
  
  # P-value from the chi-squared distribution with 1 degree of freedom
  p_value <- 1- pchisq(LR, df = 1)
  
  # Return results
  return(list(LR_statistic = LR, p_value = p_value, exceedances = x, total_observations = T))
}

kp_test_tgarch_95<-kupeic_test(nrow(portfolio_return),exceedance_95,0.95)
kp_test_tgarch_99<-kupeic_test(nrow(portfolio_return),exceedance_99,0.99)
kp_test_egarch_95<-kupeic_test(nrow(portfolio_return),exceedance2_95,0.95)
kp_test_egarch_99<-kupeic_test(nrow(portfolio_return),exceedance2_99,0.99)
kp_test_garch_11_95<-kupeic_test(nrow(portfolio_return),exceedance3_95,0.95)
kp_test_garch_11_99<-kupeic_test(nrow(portfolio_return),exceedance3_99,0.99)

table<-matrix(c(kp_test_garch_11_95$total_observations,kp_test_garch_11_95$exceedances,
                kp_test_garch_11_95$LR_statistic,kp_test_garch_11_95$p_value,
                kp_test_egarch_95$total_observations,kp_test_egarch_95$exceedances,
                kp_test_egarch_95$LR_statistic,kp_test_egarch_95$p_value,
                kp_test_tgarch_95$total_observations,kp_test_tgarch_95$exceedances,
                kp_test_tgarch_95$LR_statistic,kp_test_tgarch_95$p_value,
                kp_test_garch_11_99$total_observations,kp_test_garch_11_99$exceedances,
                kp_test_garch_11_99$LR_statistic,kp_test_garch_11_99$p_value,
                kp_test_egarch_99$total_observations,kp_test_egarch_99$exceedances,
                kp_test_egarch_99$LR_statistic,kp_test_egarch_99$p_value,
                kp_test_tgarch_99$total_observations,kp_test_tgarch_99$exceedances,
                kp_test_tgarch_99$LR_statistic,kp_test_tgarch_99$p_value),
              nrow=6,ncol=4,byrow=TRUE)
colnames(table)<-c("Total observations","Exceedances","Kupeic Statistic","P-value")
rownames(table)<-c("Garch(1,1)_95%","EGARCH(1,1)_95%","TGARCH(1,1)_95%",
                   "Garch(1,1)_99%","EGARCH(1,1)_99%","TGARCH(1,1)_99%")
knitr::kable(table)


#graph of actual loss vs 95%VaR
portfolio_return<-portfolio_return %>% mutate(exceed=ifelse(actual_loss<garch_11_var,
                                                            actual_loss,NA))
portfolio_return<-portfolio_return %>% mutate(exceed2=ifelse(actual_loss<garch_11_var2,
                                                             actual_loss,NA)) 
portfolio_return2<-portfolio_return %>% filter(year(Dates)<2011)

portfolio_return2 %>% ggplot(aes(x=Dates))+geom_line(aes(y=actual_loss,colour="Actual loss"))+
  geom_line(aes(y=garch_11_var,colour="GARCH (1,1) 95%"))+
  geom_line(aes(y=garch_11_var2,colour="GARCH (1,1) 99%"))+
  geom_point(aes(y=exceed,color="var95%"))+
  geom_point(aes(y=exceed2,colour="var99%"))
labs(title = "Actual Loss vs GARCH(1,1) 95% & 99% VaR ",
     x = "Dates",
     y = "Loss",
     colour = "Legend") +
  theme_minimal() +
  theme(legend.position = "topleft")

#after 2011
portfolio_return3<-portfolio_return %>% filter(year(Dates)>2010)
portfolio_return3 %>% ggplot(aes(x=Dates))+geom_line(aes(y=actual_loss,colour="Actual loss"))+
  geom_line(aes(y=garch_11_var,colour="GARCH (1,1) 95%"))+
  geom_line(aes(y=garch_11_var2,colour="GARCH (1,1) 99%"))+
  geom_point(aes(y=exceed,color="var95%"))+
  geom_point(aes(y=exceed2,colour="var99%"))
labs(title = "Actual Loss vs GARCH(1,1) 95% & 99% VaR ",
     x = "Dates",
     y = "Loss",
     colour = "Legend") +
  theme_minimal() +
  theme(legend.position = "topleft")

portfolio_return %>% ggplot(aes(x=Dates))+
  geom_line(aes(y=(garchvol-min(garchvol))/(max(garchvol)-min(garchvol)),
                colour="Garch(1,1) vol"))+
  geom_line(aes(y=(egarchvol-min(egarchvol))/(max(egarchvol)-min(egarchvol)),
                colour="EGarch(1,1) vol"))+
  geom_line(aes(y=(tgarchvol-min(tgarchvol))/(max(tgarchvol)-min(tgarchvol)),
                colour="TGarch(1,1) vol"))+
  labs(title = "Plot of time series of  volatility ",
       x = "Dates",
       y = "Volatility",
       colour = "Legend") +
  theme_minimal() +
  theme(legend.position = "top")

