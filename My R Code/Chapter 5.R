spamD <- read.table("~/Documents/GitHub/R-for-Data-Science-Training-Repo/Spambase/spamD.tsv",header=T,sep='\t')

#Create Train and Test groups:

#Ex: Logistic Regression Spam model

spamTrain <- subset(spamD, spamD$rgroup>=10)
spamTest <- subset(spamD, spamD$rgroup<10)
spamVars <- setdiff(colnames(spamD), list('rgroup','spam'))
spamFormula <- as.formula(paste('spam=="spam"', paste(spamVars, collapse=' + '),sep=' ~ '))
spamModel <- glm(spamFormula, family=binomial(link='logit'),data=spamTrain)

spamTrain$pred <- predict(spamModel,newdata=spamTrain,
                          type='response')
spamTest$pred <- predict(spamModel,newdata=spamTest,
                         type='response')
print(with(spamTest,table(y=spam,glmPred=pred>0.5)))

#sample the results:

sample <- spamTest[c(7,35,224,327),c('spam','pred')]
print(sample)

#the confusion matrix

cm <- table(truth=spamTest$spam, prediction=spamTest$pred>0.5)
print(cm)

#True negatives

cm[1,1]

#False negatives

cm[2,1]

#False positives

cm[1,2]

#True Positives

cm[2,2]

#Accuracy:
  
(cm[1,1]+cm[2,2])/sum(cm)

#Precision, how often a postive indication is correct

prec <- (cm[2,2])/(cm[2,2]+cm[1,2])

#Recall

rec <- cm[2,2]/(cm[2,2]+cm[2,1])

#F1, accounts for both

f1 <- (2*prec*rec)/(prec+rec)
f1

#Evaluating Scoring Models
#Scoring residuals

d <-data.frame(y=(1:10)^2,x=1:10)
model <- lm(y~x,data=d)
d$prediction <- predict(model,newdata=d)
library('ggplot2')
ggplot(data=d) + geom_point(aes(x=x,y=y)) +
  geom_line(aes(x=x,y=prediction),color='blue') +
  geom_segment(aes(x=x,y=prediction,yend=y,xend=x)) +
  scale_y_continuous('')

#Root Mean Square Error

sqrt(mean((d$prediction-d$y)^2))

#R Squared

1-sum((d$prediction-d$y)^2)/sum((mean(d$y)-d$y)^2)

#making a double density plot

ggplot(data=spamTest) + 
  geom_density(aes(x=pred,color=spam,linetype=spam))

#The ROC (receiver operating characteristics curve)

library ('ROCR')
eval <- prediction(spamTest$pred,spamTest$spam)
plot(performance(eval,"tpr","fpr"))
print(attributes(performance(eval,'auc'))$y.values[[1]])

#Log Likelihood


#Log likelihood the model assigns to the test data, want a number closer to 0

sum(ifelse(spamTest$spam=='spam',
           log(spamTest$pred),
           log(1-spamTest$pred)))

#Log likelihood rescaled by number of data points, rough average suprise per data point

sum(ifelse(spamTest$spam=='spam',
           log(spamTest$pred),
           log(1-spamTest$pred)))/dim(spamTest)[[1]]

#Test this against the null model of 180/458 (known spam divided by total pop)

pNull <- sum(ifelse(spamTest$spam=='spam',1,0))/dim(spamTest)[[1]]
sum(ifelse(spamTest$spam=='spam',1,0))*log(pNull) +
  sum(ifelse(spamTest$spam=='spam',0,1))*log(1-pNull)

#-134 is much better than null model of -306!

#Deviance with s=0, or pseudo R-squared:

1-(-2*(-134.9478))/(-2*(-306.8952))

#.56...good, but not great

#Entropy, given in bits per example

entropy <- function(x){
  xpos <- x[x>0]
  scaled <- xpos/sum(xpos)
  sum(-scaled*log(scaled,2))
}
print(entropy(table(spamTest$spam)))

#^lots of surprise is present!

#Conditional Entropy

conditionalEntropy <- function(t) {
  (sum(t[,1])*entropy(t[,1]) + sum(t[,2])*entropy(t[,2]))/sum(t)
}

#Not much surprise present given existing entropy...
print(conditionalEntropy(cm))

#Evaluating Cluster Models

#generate some random data on a plane

set.seed(32297)
d <-data.frame(x=runif(100),y=runif(100))
clus <- kmeans(d,centers=5)

d$cluster <- clus$cluster

#Plot your clusters

library('ggplot2');library('grDevices')
h <- do.call(rbind, lapply(unique(clus$cluster),
                           function(c){
                             f <- subset(d,cluster==c); f[chull(f),]
                           }))
ggplot() +
  geom_text(data=d,aes(label=cluster,x=x,y=y, color=cluster),size=3) +
  geom_polygon(data=h,aes(x=x,y=y,group=cluster,fill=as.factor(cluster)), alpha=0.4,linetype=0) +
  theme(legend.position="none")

#Evaluate:

table(d$cluster)

#Inter/Intra cluster distances.  Using provided code
#!! NEED to dig into this further, research functions, the code breaks



> library('reshape2')
> n <- dim(d)[[1]]
> pairs <- data.frame(
  ca = as.vector(outer(1:n,1:n,function(a,b) d[a,'cluster'])),
  cb = as.vector(outer(1:n,1:n,function(a,b) d[b,'cluster'])),
  dist = as.vector(outer(1:n,1:n,function(a,b)
    sqrt((d[a,'x']-d[b,'x'])^2 + (d[a,'y']-d[b,'y'])^2)))
)
> dcast(pairs,ca~cb,value.var='dist',mean)

