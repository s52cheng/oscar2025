dat <- read.csv("/Users/ck/Downloads/oscars data 2025.csv")

head(dat)
dim(dat)


# should clean Age, Days, ...
# Age = NA/0 for movie
# other awards gdr-SAE, check

#View(dat[which(dat$Days>365),])
dim(dat[which(dat$Days>425),])
dat[dat$Days>425, c("Year", "Movie", "Days")]
dat[dat$Days>425 & dat$Year>2020, c("Year", "Movie", "Days")]

dat$Age[dat$PP==1]
dat[dat$PP==1 & dat$Age>0, c("Year", "Movie")]
dat[dat$PP==1 & dat$Age>0 & dat$Year>2020, c("Year", "Movie", "Age")]

dat$Movie[which(dat$PP==1 & dat$Age > 0)]

table(dat$Age[dat$PP==1])
summary(dat$Age[dat$DD==1])
summary(dat$Age[dat$MM==1])
summary(dat$Age[dat$FF==1])

# dat[dat$DD==1 & is.na(dat$Age), c("Year", "Movie", "Age")]
sum(dat$DD==1 & dat$Age==0)
dat[dat$DD==1 & dat$Age==0, c("Year", "Movie", "Age")]

# dat[dat$MM==1 & is.na(dat$Age), c("Year", "Movie", "Age")]
# dat[dat$FF==1 & is.na(dat$Age), c("Year", "Movie", "Age")]

# table(dat$Gdr)
sum(dat$Age==0 & !is.na(dat$Age))
sum(dat$Age==0 & !is.na(dat$Age) & dat$PP==1)
sum(dat$DD==1 & dat$Age==0)
sum(dat$DD==1)

dat$Age[dat$Age==0 & !is.na(dat$Age) & dat$PP==1] <- NA

# good to consider interaction in analysis
dat$PN <- dat$PP*dat$Nom 
dat$PD <- dat$PP*dat$Dir 
dat$DN <- dat$DD*dat$Nom 
dat$DP <- dat$DD*dat$Pic 
dat$DPrN <- dat$DD*dat$PrN 
dat$DPrW <- dat$DD*dat$PrW 
dat$MN <- dat$MM*dat$Nom 
dat$MP <- dat$MM*dat$Pic 
dat$MPrN <- dat$MM*dat$PrNl 
dat$MPrW <- dat$MM*dat$PrWl 
dat$FN <- dat$FF*dat$Nom 
dat$FP <- dat$FF*dat$Pic 
dat$FPrN <- dat$FF*dat$PrNl 
dat$FPrW <- dat$FF*dat$PrWl

names(dat)
dat$Ch[dat$Ch==2] <- 0
table(dat$Ch)


######
#library(dplyr)    # for general data wrangling needs

# Modeling packages
#library(gbm)      # for original implementation of regular and stochastic GBMs
#library(h2o)      # for a java-based implementation of GBM variants
library(xgboost)  # for fitting extreme gradient boosting

data.org <- dat

####################################################################
####################      Best picture     #########################
####################################################################
dat <- data.org[data.org$PP==1,]

### column 66 corresponds to the variable "Age"
## 2021
train <- list(data = dat[dat$Year<2021,c(4,13:65,67:102)], label = dat[dat$Year<2021,11])
test <- list(data = dat[dat$Year==2021,c(4,13:65,67:102)], label = dat[dat$Year==2021,11])

bstDense.PP1 <- xgboost(data = as.matrix(train$data), label = train$label, max.depth = 2,
                        eta = 0.03, nthread = 16, nrounds = 100, objective = "binary:logistic")
pred <- predict(bstDense.PP1, as.matrix(test$data))
pred.dat <- cbind(pred, dat[dat$Year==2021, c("PP", "DD", "MM", "FF", "Ch")])

pred.dat$Ch.pred <- 0
pred.dat$Ch.pred[which.max(pred.dat$pred)] <- 1
pred.dat

res <- NULL
res <- rbind(res, data.frame(Category = "Picture", Year = 2021,
                             Prediction = ifelse(pred.dat$Ch.pred[pred.dat$Ch ==1] == 1, "Correct", "Wrong")))
pdf(file = "Plot_PP_DD.pdf")
par(mfrow=c(3,2))
importance_matrix <- xgb.importance(model = bstDense.PP1)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix,
                    main = "Picture 2021, Model: <= 2020")


# 2022
train <- list(data = dat[dat$Year<2022,c(4,13:65,67:102)], label = dat[dat$Year<2022,11])
test <- list(data = dat[dat$Year==2022,c(4,13:65,67:102)], label = dat[dat$Year==2022,11])

bstDense.PP2 <- xgboost(data = as.matrix(train$data), label = train$label, max.depth = 2,
                        eta = 0.2, nthread = 16, nrounds = 100, objective = "binary:logistic")
pred <- predict(bstDense.PP2, as.matrix(test$data))
pred.dat <- cbind(pred, dat[dat$Year==2022, c("PP", "DD", "MM", "FF", "Ch")])

pred.dat$Ch.pred <- 0
pred.dat$Ch.pred[which.max(pred.dat$pred)] <- 1
pred.dat

res <- rbind(res, data.frame(Category = "Picture", Year = 2022,
                             Prediction = ifelse(pred.dat$Ch.pred[pred.dat$Ch ==1] == 1, "Correct", "Wrong")))

importance_matrix <- xgb.importance(model = bstDense.PP2)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix,
                    main = "Picture 2022, Model: <= 2021")

# 2023
train <- list(data = dat[dat$Year<2023,c(4,13:65,67:102)], label = dat[dat$Year<2023,11])
test <- list(data = dat[dat$Year==2023,c(4,13:65,67:102)], label = dat[dat$Year==2023,11])

bstDense.PP3 <- xgboost(data = as.matrix(train$data), label = train$label, max.depth = 2,
                        eta = 0.2, nthread = 16, nrounds = 100, objective = "binary:logistic")
pred <- predict(bstDense.PP3, as.matrix(test$data))
pred.dat <- cbind(pred, dat[dat$Year==2023, c("PP", "DD", "MM", "FF", "Ch")])

pred.dat$Ch.pred <- 0
pred.dat$Ch.pred[which.max(pred.dat$pred)] <- 1
pred.dat

res <- rbind(res, data.frame(Category = "Picture", Year = 2023,
                             Prediction = ifelse(pred.dat$Ch.pred[pred.dat$Ch ==1] == 1, "Correct", "Wrong")))


importance_matrix <- xgb.importance(model = bstDense.PP3)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix,
                    main = "Picture 2023, Model: <= 2022")

####################################################################
####################      Best director     #########################
####################################################################
dat <- data.org[data.org$DD==1,]

## 2021
train <- list(data = dat[dat$Year<2021,c(4,13:65,67:102)], label = dat[dat$Year<2021,11])
test <- list(data = dat[dat$Year==2021,c(4,13:65,67:102)], label = dat[dat$Year==2021,11])

bstDense.DD1 <- xgboost(data = as.matrix(train$data), label = train$label, max.depth = 2,
                        eta = .1, nthread = 16, nrounds = 100, objective = "binary:logistic")
pred <- predict(bstDense.DD1, as.matrix(test$data))
pred.dat <- cbind(pred, dat[dat$Year==2021, c("PP", "DD", "MM", "FF", "Ch")])

pred.dat$Ch.pred <- 0
pred.dat$Ch.pred[which.max(pred.dat$pred)] <- 1
pred.dat

res <- rbind(res, data.frame(Category = "Director", Year = 2021,
                             Prediction = ifelse(pred.dat$Ch.pred[pred.dat$Ch ==1] == 1, "Correct", "Wrong")))


importance_matrix <- xgb.importance(model = bstDense.DD1)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix,
                    main = "Director 2021, Model: <= 2020")

# 2022
train <- list(data = dat[dat$Year<2022,c(4,13:65,67:102)], label = dat[dat$Year<2022,11])
test <- list(data = dat[dat$Year==2022,c(4,13:65,67:102)], label = dat[dat$Year==2022,11])

bstDense.DD2 <- xgboost(data = as.matrix(train$data), label = train$label, max.depth = 2,
                        eta = .1, nthread = 16, nrounds = 100, objective = "binary:logistic")
pred <- predict(bstDense.DD2, as.matrix(test$data))
pred.dat <- cbind(pred, dat[dat$Year==2022, c("PP", "DD", "MM", "FF", "Ch")])

pred.dat$Ch.pred <- 0
pred.dat$Ch.pred[which.max(pred.dat$pred)] <- 1
pred.dat

res <- rbind(res, data.frame(Category = "Director", Year = 2022,
                             Prediction = ifelse(pred.dat$Ch.pred[pred.dat$Ch ==1] == 1, "Correct", "Wrong")))


importance_matrix <- xgb.importance(model = bstDense.DD2)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix,
                    main = "Director 2022, Model: <= 2021")


# 2023
train <- list(data = dat[dat$Year<2023,c(4,13:65,67:102)], label = dat[dat$Year<2023,11])
test <- list(data = dat[dat$Year==2023,c(4,13:65,67:102)], label = dat[dat$Year==2023,11])

bstDense.DD3 <- xgboost(data = as.matrix(train$data), label = train$label, max.depth = 2,
                        eta = .1, nthread = 16, nrounds = 100, objective = "binary:logistic")
pred <- predict(bstDense.DD3, as.matrix(test$data))
pred.dat <- cbind(pred, dat[dat$Year==2023, c("PP", "DD", "MM", "FF", "Ch")])

pred.dat$Ch.pred <- 0
pred.dat$Ch.pred[which.max(pred.dat$pred)] <- 1
pred.dat

res <- rbind(res, data.frame(Category = "Director", Year = 2023,
                             Prediction = ifelse(pred.dat$Ch.pred[pred.dat$Ch ==1] == 1, "Correct", "Wrong")))


importance_matrix <- xgb.importance(model = bstDense.DD3)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix,
                    main = "Director 2023, Model: <= 2022")

dev.off()

####################################################################
####################      Best male actor    #######################
####################################################################
dat <- data.org[data.org$MM==1,]

## 2021
train <- list(data = dat[dat$Year<2021,c(4,13:51,67:102)], label = dat[dat$Year<2021,11])
test <- list(data = dat[dat$Year==2021,c(4,13:51,67:102)], label = dat[dat$Year==2021,11])

bstDense.MM1 <- xgboost(data = as.matrix(train$data), label = train$label, max.depth = 2,
                        eta = .1, nthread = 16, nrounds = 100, objective = "binary:logistic")
pred <- predict(bstDense.MM1, as.matrix(test$data))
pred.dat <- cbind(pred, dat[dat$Year==2021, c("PP", "DD", "MM", "FF", "Ch")])

pred.dat$Ch.pred <- 0
pred.dat$Ch.pred[which.max(pred.dat$pred)] <- 1
pred.dat

res <- rbind(res, data.frame(Category = "Male lead", Year = 2021,
                             Prediction = ifelse(pred.dat$Ch.pred[pred.dat$Ch ==1] == 1, "Correct", "Wrong")))

pdf("Plot_MM_FF.pdf")
par(mfrow=c(3,2))
importance_matrix <- xgb.importance(model = bstDense.MM1)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix,
                    main = "Male lead 2021, Model: <= 2020")

# 2022
train <- list(data = dat[dat$Year<2022,c(4,13:51,67:102)], label = dat[dat$Year<2022,11])
test <- list(data = dat[dat$Year==2022,c(4,13:51,67:102)], label = dat[dat$Year==2022,11])

bstDense.MM2 <- xgboost(data = as.matrix(train$data), label = train$label, max.depth = 2,
                        eta = 1, nthread = 16, nrounds = 2, objective = "binary:logistic",
                        gamma = 0)
pred <- predict(bstDense.MM2, as.matrix(test$data))
pred.dat <- cbind(pred, dat[dat$Year==2022, c("PP", "DD", "MM", "FF", "Ch")])

pred.dat$Ch.pred <- 0
pred.dat$Ch.pred[which.max(pred.dat$pred)] <- 1
pred.dat

res <- rbind(res, data.frame(Category = "Male lead", Year = 2022,
                             Prediction = ifelse(pred.dat$Ch.pred[pred.dat$Ch ==1] == 1, "Correct", "Wrong")))


importance_matrix <- xgb.importance(model = bstDense.MM2)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix,
                    main = "Male lead 2022, Model: <= 2021")


# 2023
train <- list(data = dat[dat$Year<2023,c(4,13:51,67:102)], label = dat[dat$Year<2023,11])
test <- list(data = dat[dat$Year==2023,c(4,13:51,67:102)], label = dat[dat$Year==2023,11])

bstDense.MM3 <- xgboost(data = as.matrix(train$data), label = train$label, max.depth = 2,
                        eta = .1, nthread = 16, nrounds = 100, objective = "binary:logistic")
pred <- predict(bstDense.MM3, as.matrix(test$data))
pred.dat <- cbind(pred, dat[dat$Year==2023, c("PP", "DD", "MM", "FF", "Ch")])

pred.dat$Ch.pred <- 0
pred.dat$Ch.pred[which.max(pred.dat$pred)] <- 1
pred.dat

res <- rbind(res, data.frame(Category = "Male lead", Year = 2023,
                             Prediction = ifelse(pred.dat$Ch.pred[pred.dat$Ch ==1] == 1, "Correct", "Wrong")))


importance_matrix <- xgb.importance(model = bstDense.MM3)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix,
                    main = "Male lead 2023, Model: <= 2022")


####################################################################
###################      Best female actor    ######################
####################################################################
dat <- data.org[data.org$FF==1,]

## 2021
train <- list(data = dat[dat$Year<2021,c(4,13:51,67:102)], label = dat[dat$Year<2021,11])
test <- list(data = dat[dat$Year==2021,c(4,13:51,67:102)], label = dat[dat$Year==2021,11])

bstDense.FF1 <- xgboost(data = as.matrix(train$data), label = train$label, max.depth = 2,
                        eta = .1, nthread = 16, nrounds = 100, objective = "binary:logistic")
pred <- predict(bstDense.FF1, as.matrix(test$data))
pred.dat <- cbind(pred, dat[dat$Year==2021, c("PP", "DD", "MM", "FF", "Ch")])

pred.dat$Ch.pred <- 0
pred.dat$Ch.pred[which.max(pred.dat$pred)] <- 1
pred.dat

res <- rbind(res, data.frame(Category = "Female lead", Year = 2021,
                             Prediction = ifelse(pred.dat$Ch.pred[pred.dat$Ch ==1] == 1, "Correct", "Wrong")))

importance_matrix <- xgb.importance(model = bstDense.FF1)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix,
                    main = "Female lead 2021, Model: <= 2020")

# 2022
train <- list(data = dat[dat$Year<2022,c(4,13:51,67:102)], label = dat[dat$Year<2022,11])
test <- list(data = dat[dat$Year==2022,c(4,13:51,67:102)], label = dat[dat$Year==2022,11])

bstDense.FF2 <- xgboost(data = as.matrix(train$data), label = train$label, max.depth = 2,
                        eta = .1, nthread = 16, nrounds = 100, objective = "binary:logistic")
pred <- predict(bstDense.FF2, as.matrix(test$data))
pred.dat <- cbind(pred, dat[dat$Year==2022, c("PP", "DD", "MM", "FF", "Ch")])

pred.dat$Ch.pred <- 0
pred.dat$Ch.pred[which.max(pred.dat$pred)] <- 1
pred.dat

res <- rbind(res, data.frame(Category = "Female lead", Year = 2022,
                             Prediction = ifelse(pred.dat$Ch.pred[pred.dat$Ch ==1] == 1, "Correct", "Wrong")))


importance_matrix <- xgb.importance(model = bstDense.FF2)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix,
                    main = "Female lead 2022, Model: <= 2021")


# 2023
train <- list(data = dat[dat$Year<2023,c(4,13:51,67:102)], label = dat[dat$Year<2023,11])
test <- list(data = dat[dat$Year==2023,c(4,13:51,67:102)], label = dat[dat$Year==2023,11])

bstDense.FF3 <- xgboost(data = as.matrix(train$data), label = train$label, max.depth = 6,
                        eta = 0.1, nthread = 16, nrounds = 2, objective = "binary:logistic")
pred <- predict(bstDense.FF3, as.matrix(test$data))
pred.dat <- cbind(pred, dat[dat$Year==2023, c("PP", "DD", "MM", "FF", "Ch")])

pred.dat$Ch.pred <- 0
pred.dat$Ch.pred[which.max(pred.dat$pred)] <- 1
pred.dat

res <- rbind(res, data.frame(Category = "Female lead", Year = 2023,
                             Prediction = ifelse(pred.dat$Ch.pred[pred.dat$Ch ==1] == 1, "Correct", "Wrong")))


importance_matrix <- xgb.importance(model = bstDense.FF3)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix,
                    main = "Female lead 2023, Model: <= 2022")
dev.off()

res <- res[order(res$Year),]
res


#########################################################
#############     Predictions for 2024      ##############
#########################################################

####     Best picture
dat <- data.org[data.org$PP==1,]

### column 66 corresponds to the variable "Age"
## <=2020
test <- list(data = dat[dat$Year==2024,c(4,13:65,67:102)], label = dat[dat$Year==2024,11])
pred <- predict(bstDense.PP1, as.matrix(test$data))
pred.dat <- cbind(pred, dat[dat$Year==2024, c("PP", "DD", "MM", "FF", "Ch")])
pred.dat$Ch.pred <- 0
pred.dat$Ch.pred[which.max(pred.dat$pred)] <- 1
pred.dat

res <- NULL
res <- rbind(res, data.frame(data.org[as.numeric(row.names(pred.dat)[which.max(pred.dat$pred)]),
                                      c("Year", "Name", "Movie", "PP", "DD", "MM", "FF")],
                             prob = pred.dat$pred[which.max(pred.dat$pred)],
                             model = "year<=2020"))

## <=2021
test <- list(data = dat[dat$Year==2024,c(4,13:65,67:102)], label = dat[dat$Year==2024,11])
pred <- predict(bstDense.PP2, as.matrix(test$data))
pred.dat <- cbind(pred, dat[dat$Year==2024, c("PP", "DD", "MM", "FF", "Ch")])
pred.dat$Ch.pred <- 0
pred.dat$Ch.pred[which.max(pred.dat$pred)] <- 1
pred.dat

res <- rbind(res, data.frame(data.org[as.numeric(row.names(pred.dat)[which.max(pred.dat$pred)]),
                                      c("Year", "Name", "Movie", "PP", "DD", "MM", "FF")],
                             prob = pred.dat$pred[which.max(pred.dat$pred)],
                             model = "year<=2021"))

## <=2022
test <- list(data = dat[dat$Year==2024,c(4,13:65,67:102)], label = dat[dat$Year==2024,11])
pred <- predict(bstDense.PP3, as.matrix(test$data))
pred.dat <- cbind(pred, dat[dat$Year==2024, c("PP", "DD", "MM", "FF", "Ch")])
pred.dat$Ch.pred <- 0
pred.dat$Ch.pred[which.max(pred.dat$pred)] <- 1
pred.dat

res <- rbind(res, data.frame(data.org[as.numeric(row.names(pred.dat)[which.max(pred.dat$pred)]),
                                      c("Year", "Name", "Movie", "PP", "DD", "MM", "FF")],
                             prob = pred.dat$pred[which.max(pred.dat$pred)],
                             model = "year<=2022"))

####     Best director
dat <- data.org[data.org$DD==1,]

### column 66 corresponds to the variable "Age"
## <=2020
test <- list(data = dat[dat$Year==2024,c(4,13:65,67:102)], label = dat[dat$Year==2024,11])
pred <- predict(bstDense.DD1, as.matrix(test$data))
pred.dat <- cbind(pred, dat[dat$Year==2024, c("PP", "DD", "MM", "FF", "Ch")])
pred.dat$Ch.pred <- 0
pred.dat$Ch.pred[which.max(pred.dat$pred)] <- 1
pred.dat

res <- rbind(res, data.frame(data.org[as.numeric(row.names(pred.dat)[which.max(pred.dat$pred)]),
                                      c("Year", "Name", "Movie", "PP", "DD", "MM", "FF")],
                             prob = pred.dat$pred[which.max(pred.dat$pred)],
                             model = "year<=2020"))


## <=2021
test <- list(data = dat[dat$Year==2024,c(4,13:65,67:102)], label = dat[dat$Year==2024,11])
pred <- predict(bstDense.DD2, as.matrix(test$data))
pred.dat <- cbind(pred, dat[dat$Year==2024, c("PP", "DD", "MM", "FF", "Ch")])
pred.dat$Ch.pred <- 0
pred.dat$Ch.pred[which.max(pred.dat$pred)] <- 1
pred.dat

res <- rbind(res, data.frame(data.org[as.numeric(row.names(pred.dat)[which.max(pred.dat$pred)]),
                                      c("Year", "Name", "Movie", "PP", "DD", "MM", "FF")],
                             prob = pred.dat$pred[which.max(pred.dat$pred)],
                             model = "year<=2021"))


## <=2022
test <- list(data = dat[dat$Year==2024,c(4,13:65,67:102)], label = dat[dat$Year==2024,11])
pred <- predict(bstDense.DD3, as.matrix(test$data))
pred.dat <- cbind(pred, dat[dat$Year==2024, c("PP", "DD", "MM", "FF", "Ch")])
pred.dat$Ch.pred <- 0
pred.dat$Ch.pred[which.max(pred.dat$pred)] <- 1
pred.dat

res <- rbind(res, data.frame(data.org[as.numeric(row.names(pred.dat)[which.max(pred.dat$pred)]),
                                      c("Year", "Name", "Movie", "PP", "DD", "MM", "FF")],
                             prob = pred.dat$pred[which.max(pred.dat$pred)],
                             model = "year<=2022"))


####     Best male lead
dat <- data.org[data.org$MM==1,]

### column 66 corresponds to the variable "Age"
## <=2020
test <- list(data = dat[dat$Year==2024,c(4,13:51,67:102)], label = dat[dat$Year==2024,11])
pred <- predict(bstDense.MM1, as.matrix(test$data))
pred.dat <- cbind(pred, dat[dat$Year==2024, c("PP", "DD", "MM", "FF", "Ch", "Name")])
pred.dat$Ch.pred <- 0
pred.dat$Ch.pred[which.max(pred.dat$pred)] <- 1
pred.dat

res <- rbind(res, data.frame(data.org[as.numeric(row.names(pred.dat)[which.max(pred.dat$pred)]),
                                      c("Year", "Name", "Movie", "PP", "DD", "MM", "FF")],
                             prob = pred.dat$pred[which.max(pred.dat$pred)],
                             model = "year<=2020"))

## <=2021
test <- list(data = dat[dat$Year==2024,c(4,13:51,67:102)], label = dat[dat$Year==2024,11])
pred <- predict(bstDense.MM2, as.matrix(test$data))
pred.dat <- cbind(pred, dat[dat$Year==2024, c("PP", "DD", "MM", "FF", "Ch")])
pred.dat$Ch.pred <- 0
pred.dat$Ch.pred[which.max(pred.dat$pred)] <- 1
pred.dat

res <- rbind(res, data.frame(data.org[as.numeric(row.names(pred.dat)[which.max(pred.dat$pred)]),
                                      c("Year", "Name", "Movie", "PP", "DD", "MM", "FF")],
                             prob = pred.dat$pred[which.max(pred.dat$pred)],
                             model = "year<=2021"))

## <=2022
test <- list(data = dat[dat$Year==2024,c(4,13:51,67:102)], label = dat[dat$Year==2024,11])
pred <- predict(bstDense.MM3, as.matrix(test$data))
pred.dat <- cbind(pred, dat[dat$Year==2024, c("PP", "DD", "MM", "FF", "Ch")])
pred.dat$Ch.pred <- 0
pred.dat$Ch.pred[which.max(pred.dat$pred)] <- 1
pred.dat

res <- rbind(res, data.frame(data.org[as.numeric(row.names(pred.dat)[which.max(pred.dat$pred)]),
                                      c("Year", "Name", "Movie", "PP", "DD", "MM", "FF")],
                             prob = pred.dat$pred[which.max(pred.dat$pred)],
                             model = "year<=2022"))


####     Best female lead
dat <- data.org[data.org$FF==1,]

### column 66 corresponds to the variable "Age"
## <=2020
test <- list(data = dat[dat$Year==2024,c(4,13:51,67:102)], label = dat[dat$Year==2024,11])
pred <- predict(bstDense.FF1, as.matrix(test$data))
pred.dat <- cbind(pred, dat[dat$Year==2024, c("PP", "DD", "MM", "FF", "Ch")])
pred.dat$Ch.pred <- 0
pred.dat$Ch.pred[which.max(pred.dat$pred)] <- 1
pred.dat

res <- rbind(res, data.frame(data.org[as.numeric(row.names(pred.dat)[which.max(pred.dat$pred)]),
                                      c("Year", "Name", "Movie", "PP", "DD", "MM", "FF")],
                             prob = pred.dat$pred[which.max(pred.dat$pred)],
                             model = "year<=2020"))


## <=2021
pred <- predict(bstDense.FF2, as.matrix(test$data))
pred.dat <- cbind(pred, dat[dat$Year==2024, c("PP", "DD", "MM", "FF", "Ch")])
pred.dat$Ch.pred <- 0
pred.dat$Ch.pred[which.max(pred.dat$pred)] <- 1
pred.dat

res <- rbind(res, data.frame(data.org[as.numeric(row.names(pred.dat)[which.max(pred.dat$pred)]),
                                      c("Year", "Name", "Movie", "PP", "DD", "MM", "FF")],
                             prob = pred.dat$pred[which.max(pred.dat$pred)],
                             model = "year<=2021"))

## <=2022
pred <- predict(bstDense.FF3, as.matrix(test$data))
pred.dat <- cbind(pred, dat[dat$Year==2024, c("PP", "DD", "MM", "FF", "Ch")])
pred.dat$Ch.pred <- 0
pred.dat$Ch.pred[which.max(pred.dat$pred)] <- 1
pred.dat
res <- rbind(res, data.frame(data.org[as.numeric(row.names(pred.dat)[which.max(pred.dat$pred)]),
                                      c("Year", "Name", "Movie", "PP", "DD", "MM", "FF")],
                             prob = pred.dat$pred[which.max(pred.dat$pred)],
                             model = "year<=2022"))

res
### tie between the directors: 
### Jacques Audiard (Emilia Perez) and Brady Corbet (The Brutalist)




