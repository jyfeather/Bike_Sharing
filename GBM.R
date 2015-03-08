rm(list=ls())
set.seed(2015)

####################
#     Input
####################
data.orig <- read.csv(file = "../../Data/hour.csv")

####################
#     Preprocessing
####################
datetime <- data.orig[,2]
data.orig <- data.orig[,-c(1,2)] # del inst & date
names <- c("season", "yr", "mnth", "hr", "holiday", "weekday", "workingday", "weathersit")
newnames <- c("season", "year", "month", "hour", "holiday", "weekday", "workingday", "weather", 
              "temp", "atemp", "humidity", "windspeed", "casual", "registered", "count")
data.orig[,names] <- lapply(data.orig[,names], factor)
colnames(data.orig) <- newnames
str(data.orig)

# training set & testing set separation
train.no <- sample(nrow(data.orig), round(2/3*nrow(data.orig)))
train.orig <- data.orig[train.no,]
test.orig <- data.orig[-train.no,]

####################
#     GBM
####################
library(gbm)
# registered
gbm.reg <-gbm(registered ~ .-casual-count,
                data=train.orig,
                var.monotone=NULL, 
                distribution="poisson",
                n.trees=1200, 
                shrinkage=0.1, 
                interaction.depth=3,
                bag.fraction = 0.5,
                train.fraction = 0.8,
                n.minobsinnode = 10,
                cv.folds =10,
                keep.data=TRUE,
                verbose=TRUE)
iter.reg <- gbm.perf(gbm.reg, method="cv")
summary(gbm.reg)
# casual
gbm.cas <-gbm(casual~ .-registered-count,
                data=train.orig,
                var.monotone=NULL, 
                distribution="poisson",
                n.trees=1200, 
                shrinkage=0.1, 
                interaction.depth=3,
                bag.fraction = 0.5,
                train.fraction = 0.8,
                n.minobsinnode = 10,
                cv.folds =10,
                keep.data=TRUE,
                verbose=TRUE)
iter.cas <- gbm.perf(gbm.cas, method="cv")
summary(gbm.cas)

####################
#     prediction
####################
pred.reg <- predict(gbm.reg, test.orig[,-c(13,14,15)], iter.reg, type = "response")
pred.reg <- round(abs(pred.reg))
pred.cas <- predict(gbm.cas, test.orig[,-c(13,14,15)], iter.cas, type = "response")
pred.cas <- round(abs(pred.cas))
pred <- pred.reg + pred.cas

####################
#     evaluation
####################
eval <- function(p, a) {
  return(sqrt(sum((log(p+1)-log(a+1))^2)/length(p)))
}
eval(pred, test.orig$count)
eval(pred.reg, test.orig$registered)
eval(pred.cas, test.orig$casual)

####################
#     visualization    
####################
library(ggplot2)
library(gridExtra)
# actual vs. prediction
viz.dat <- data.frame(cbind(predicted=pred, actual=test.orig$count))
ggplot(data = viz.dat, aes(x=actual, y=predicted)) +
  geom_point() + geom_abline(intercept = 0, slope = 1) +
  xlab("actual total rental count") + ylab("predicted total rental count") +
  ggtitle("Gradient Boosting Method Prediction") 

# relative importance
viz.reg <- relative.influence(gbm.reg, scale. = T)
viz.reg <- data.frame(keyName = names(viz.reg), value = viz.reg, row.names = NULL)
viz.reg$keyName <- factor(viz.reg$keyName, levels = viz.reg[order(viz.reg$value),"keyName"])
viz.cas <- relative.influence(gbm.cas, scale. = T)
viz.cas <- data.frame(keyName = names(viz.cas), value = viz.cas, row.names = NULL)
viz.cas$keyName <- factor(viz.cas$keyName, levels = viz.cas[order(viz.cas$value),"keyName"])
p.reg <- ggplot(data = viz.reg, aes(x=keyName, y=value)) + geom_histogram(stat="identity") +
  coord_flip() + xlab("") + ylab("Relative Importance") + ggtitle("Registered Rental Count Result")
p.cas <- ggplot(data = viz.cas, aes(x=keyName, y=value)) + geom_histogram(stat="identity") +
  coord_flip() + xlab("") + ylab("Relative Importance") + ggtitle("Casual Rental Count Result")
grid.arrange(p.reg, p.cas, ncol = 2)

####################
#     Kaggle
####################
test.kaggle <- data.orig[which(as.integer(substr(datetime,9,10)) >= 20),]
pred.reg <- predict(gbm.reg, test.kaggle[,-c(13,14,15)], iter.reg, type = "response")
pred.reg <- round(abs(pred.reg))
pred.cas <- predict(gbm.cas, test.kaggle[,-c(13,14,15)], iter.cas, type = "response")
pred.cas <- round(abs(pred.cas))
pred <- pred.reg + pred.cas
test <- read.csv(file = "../../Data/test.csv")
result <- cbind(datetime = as.character(test$datetime), count = pred)
write.csv(result, file = "../Results/test_result.csv", row.names = FALSE)
