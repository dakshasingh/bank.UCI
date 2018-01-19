setwd("C:/Users/sai/Desktop/kaggle")
library(readr)
bank_additional <- read_delim("bank-additional.csv", 
                                +     ";", escape_double = FALSE, trim_ws = TRUE)
Parsed with column specification:
  cols(
    .default = col_character(),
    age = col_integer(),
    duration = col_integer(),
    campaign = col_integer(),
    pdays = col_integer(),
    previous = col_integer(),
    emp.var.rate = col_double(),
    cons.price.idx = col_double(),
    cons.conf.idx = col_double(),
    euribor3m = col_double(),
    nr.employed = col_double()
  )
See spec(...) for full column specifications.
View(bank_additional)
str(bank_additional)
summary(bank_additional)
names(bank_additional)
rm(bank.additional)
#import dataset from the local machine, check seperated by semicolon and string as factors
bank.additional <- read.csv("C:/Users/sai/Desktop/kaggle/bank-additional.csv", sep=";")
View(bank.additional)
rm(bank_additional)
str(bank.additional)
summary(bank.additional)
bank.additional$default_transformed <- ifelse(bank.additional$default == "yes","unknown","bank.additional$default")
summary(bank.additional[,c("default", "default_transformed")])
bank.additional$default_transformed <- as.factor(ifelse(bank.additional$default == "yes","unknown","bank.additional$default"))
summary(bank.additional[,c("default", "default_transformed")])
bank.additional$default_transformed <- as.factor(ifelse(bank.additional$default == "yes","unknown",as.character(bank.additional$default)))
summary(bank.additional[,c("default", "default_transformed")])
bank.additional$default <- bank.additional$default_transformed
bank.additional$default_transformed <- NULL
summary(bank.additional[,c("pdays")])
hist(bank.additional$pdays)
boxplot(bank.additional$pdays)
summary(as.factor(bank.additional$pdays))
hist(bank.additional$pdays)
table(as.factor(bank.additional$pdays), bank.additional$y)
bank.additional$pdays <- as.factor(bank.additional$pdays)
summary(bank.additional)
names(bank.additional)
summary(bank.additional$previous)
summary(as.factor(bank.additional$previous))
table(bank.additional$previous, bank.additional$y)
bank.additional$previous <- as.factor(bank.additional$previous)
summary(bank.additional$previous)
summary(bank.additional)
bank.additional$emp.var.rate <- bank.additional$emp.var.rate - min(bank.additional$emp.var.rate)/max(bank.additional$emp.var.rate) - min(bank.additional$emp.var.rate)
bank.additional$cons.price.idx <- bank.additional$cons.price.idx - min(bank.additional$cons.price.idx)/max(bank.additional$cons.price.idx) - min(bank.additional$cons.price.idx)
bank.additional$cons.conf.idx <- bank.additional$cons.conf.idx - min(bank.additional$cons.conf.idx)/max(bank.additional$cons.conf.idx) - min(bank.additional$cons.conf.idx)
bank.additional$euribor3m <- bank.additional$euribor3m - min(bank.additional$euribor3m)/max(bank.additional$euribor3m) - min(bank.additional$euribor3m)
bank.additional$nr.employed <- bank.additional$nr.employed - min(bank.additional$nr.employed)/max(bank.additional$nr.employed) - min(bank.additional$nr.employed)
summary(bank.additional)
transformed.data <- model.matrix(y ~. , data = bank.additional)
transformed.data <- as.data.frame(model.matrix(y ~. , data = bank.additional))
names(transformed.data)
transformed.data$`(Intercept)` <- NULL
transformed.data$y <- bank.additional$y
names(bank.additional)
rows <- sample(1:nrow(transformed.data), 0.7 * nrow(transformed.data))
train.bank <- transformed.data[rows, ]
test.bank <- transformed.data[-rows, ]
model <- glm(formula = y ~. , family = binomial("logit"), data = train.bank)
summary(model)
model_optimise <- step(model, direction = "both")
model2 <- glm(formula = y ~ jobmanagement + jobtechnician + contacttelephone +
                monthdec + monthjun + monthmar + monthmay + duration + campaign + 
                pdays1 + pdays4 + pdays6 + pdays16 + pdays17 + previous1 + previous4 
              + poutcomesuccess + cons.price.idx + cons.conf.idx + euribor3m, family
               = binomial("logit"), data = train.bank)
summary(model2)
model2 <- glm(formula = y ~ jobtechnician + contacttelephone +
                monthdec + monthjun + monthmar + monthmay + duration + campaign + 
                pdays4 + pdays6  + previous1 + previous4 
              + poutcomesuccess + cons.price.idx + cons.conf.idx + euribor3m, family
              = binomial("logit"), data = train.bank)
summary(model2)
model2 <- glm(formula = y ~ contacttelephone + monthjun + monthmar + duration + 
                 previous1 
              + poutcomesuccess + cons.price.idx + cons.conf.idx + euribor3m, family
              = binomial("logit"), data = train.bank)
summary(model2)
install.packages("MKmisc")
library(MKmisc)
HLgof.test(fit = fitted(model), obs = ifelse(train.bank$y == "yes",1,0))
HLgof.test(fit = fitted(model_optimise), obs = ifelse(train.bank$y =="yes",1,0))
HLgof.test(fit = fitted(model2), obs = ifelse(train.bank$y == "yes",1,0))
rchisq(n = 1000, df = 10)
hist(rchisq(n = 1000, df = 10))
#generalisation of model, ROC tests// checking how the model behaves with new data
train.bank$predict <- predict.glm(model, newdata = train.bank, type = "response")
test.bank$predict <- predict.glm(model, newdata = test.bank, type = "response")
train.bank$predict.optimise <- predict.glm(model_optimise, newdata = train.bank, type = "response")
test.bank$predict.optimise <- predict.glm(model_optimise, newdata = test.bank, type = "response")
train.bank$predict.final <- predict.glm(model2, newdata = train.bank, type = "response")
test.bank$predict.final <- predict.glm(model2, newdata = test.bank, type = "response")
install.packages("pROC")
library(pROC)
roc(y ~ predict, data = train.bank)
plot(roc(y ~ predict, data = train.bank))
#environment changes for first model
roc(y ~ predict, data = test.bank)
plot(roc(y ~ predict, data = test.bank))
#second optimized model
roc(y ~ predict.optimise, data = train.bank)
plot(roc(y ~ predict.optimise, data = train.bank))
#environment changes in optimised model
roc(y ~ predict.optimise, data = test.bank)
plot(roc(y ~ predict.optimise, data = test.bank))
#final model
roc(y ~ predict.final, data = train.bank)
plot(roc(y ~ predict.final, data = train.bank))
#environment change in final model
roc(y ~ predict.final, data = test.bank)
plot(roc(y ~ predict.final, data = test.bank))
transformed.data$predict <- predict.glm(model2, newdata = transformed.data, type = "response")
plot(roc(y ~ predict, data = transformed.data))
roc(y ~ predict, data = transformed.data)
#creating flag, confusion matrix
table(transformed.data$y, transformed.data$predict)
table(transformed.data$y, transformed.data$predict <= 0.5)
