library("rpart", lib.loc="/Library/Frameworks/R.framework/Versions/3.1/Resources/library")
load("~/Documents/GitHub/zmPDSwR/Statlog/GCDData.RData")

#decision tree test on credit data, from chapter 1 of RforDS

model <- rpart(Good.Loan ~
+ Duration.in.month
+ Installment.rate.in.percentage.of.disposable.income
+ Credit.amount
+ Other.installment.plans,
data=d,
control=rpart.control(maxdepth=4),
method="class")

#Function to conver to percent

percent <- function(x, digits = 2, format ="f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

#Create Confusion Matrix to validate model

resultframe <- data.frame(Good.Loan=creditdata$Good.Loan, pred=predict(model, type=class))

rtab <- table(resultframe)
rtab

#Overall Model Accuracy

percent(sum(diag(rtab))/sum(rtab))

#Model Precision (how many predicted to be bad actually did default?)

percent(sum(rtab[1,1])/sum(rtab[,1]))

#Model Recall, how many of the bad loans did the model find?

percent(sum(rtab[1,1])/sum(rtab[1,]))

#False Positive Rate, how many good apps were flagged bad?

percent(sum(rtab[2,1])/sum(rtab[2,]))
