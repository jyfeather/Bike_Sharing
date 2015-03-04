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
data.orig[,names] <- lapply(data.orig[,names], factor)
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
gbm.reg <-gbm(registered ~ .-casual-cnt,
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
# causal
gbm.cas <-gbm(casual~ .-registered-cnt,
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
eval(pred, test.orig$cnt)
eval(pred.reg, test.orig$registered)
eval(pred.cas, test.orig$casual)

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
