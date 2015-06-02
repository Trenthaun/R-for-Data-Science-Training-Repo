load("/Users/trenthaun/Documents/GitHub/R-for-Data-Science-Training-Repo/PUMS/psub.RData")
dtrain <- subset(psub, ORIGRANDGROUP >= 500)
dtest <- subset(psub, ORIGRANDGROUP < 500)

model <- lm(log(PINCP,base=10) ~ AGEP + SEX + COW + SCHL,data=dtrain)
dtest$predLogPINCP <-predict(model,newdata=dtest)
dtrain$predLogPINCP <- predict(model,newdata=dtrain)

# example 7.2 of section 7.1.3 
# (example 7.2 of section 7.1.3)  : Linear and logistic regression : Using linear regression : Making predictions 
# Title: Plotting log income as a function of predicted log income 

ggplot(data=dtest,aes(x=predLogPINCP,y=log(PINCP,base=10))) +
  geom_point(alpha=0.2,color="black") +
  geom_smooth(aes(x=predLogPINCP,
                  y=log(PINCP,base=10)),color="black") +
  geom_line(aes(x=log(PINCP,base=10),
                y=log(PINCP,base=10)),color="blue",linetype=2) +
  scale_x_continuous(limits=c(4,5)) +
  scale_y_continuous(limits=c(3.5,5.5))

# example 7.3 of section 7.1.3 
# (example 7.3 of section 7.1.3)  : Linear and logistic regression : Using linear regression : Making predictions 
# Title: Plotting residuals income as a function of predicted log income 

ggplot(data=dtest,aes(x=predLogPINCP,
                      y=predLogPINCP-log(PINCP,base=10))) +
  geom_point(alpha=0.2,color="black") +
  geom_smooth(aes(x=predLogPINCP,
                  y=predLogPINCP-log(PINCP,base=10)),
              color="black")

# example 7.4 of section 7.1.3 
# (example 7.4 of section 7.1.3)  : Linear and logistic regression : Using linear regression : Making predictions 
# Title: Computing R-squared 

rsq <- function(y,f) { 1 - sum((y-f)^2)/sum((y-mean(y))^2) }
rsq(log(dtrain$PINCP,base=10),predict(model,newdata=dtrain))
rsq(log(dtest$PINCP,base=10),predict(model,newdata=dtest))

# example 7.5 of section 7.1.3 
# (example 7.5 of section 7.1.3)  : Linear and logistic regression : Using linear regression : Making predictions 
# Title: Calculating root mean square error 

rmse <- function(y, f) { sqrt(mean( (y-f)^2 )) }
rmse(log(dtrain$PINCP,base=10),predict(model,newdata=dtrain))
rmse(log(dtest$PINCP,base=10),predict(model,newdata=dtest))
