rm(list=ls())
set.seed(2015)

####################
#     Input
####################
train.orig <- read.csv(file = "../../Data/train.csv")
test.orig <- read.csv(file = "../../Data/test.csv")

####################
#     Preprocessing
####################
# time conversoin
train$datetime <- as.POSIXlt(train.orig$datetime)$hour
test$datetime <- as.POSIXlt(test.orig$datetime)$hour

####################
#     GBM
####################
library(gbm)
gbm_model <-gbm(train$count~.,
                data=train[,-c(9,10,11)], # registered,casual,count
                var.monotone=NULL, 
                distribution="gaussian",
                n.trees=1200, 
                shrinkage=0.1, 
                interaction.depth=3,
                bag.fraction = 0.5,
                train.fraction = 1,
                n.minobsinnode = 10,
                cv.folds =10,
                keep.data=TRUE,
                verbose=TRUE)

iter <- gbm.perf(gbm_model, method="cv")

####################
#     prediction
####################
pred = predict(gbm_model, test, iter, type="response")
result <- cbind(datetime = as.character(test.orig$datetime), count = round(abs(pred)))
write.csv(result, file = "../Results/test_result.csv", row.names = FALSE)
