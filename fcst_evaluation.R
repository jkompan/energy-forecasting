
###################### Forecasting and evaluation ######################


# load estimated models
snaive1 <- readRDS("snaive1.rds")
arima1 <- readRDS("arima1.rds")
ets1 <- readRDS("ets1.rds")
RED <- readRDS("RED.rds")
SPRED <- readRDS("SPRED3.rds")

report(snaive1)
report(arima1)
report(ets1)
report(RED)
report(SPRED)

models <- list(snaive1,arima1,ets1,RED,SPRED)
modelnames <- c("SNAIVE","SARIMA","ETS","RED","SPRED")

summary(lm(log(Demand)~bs(Temperature,knots=c(16)),data=train_set))


### 1-step rolling forecasts without reestimation ###

# full set
acc1step <- data.frame(matrix(NA,length(models),6))
colnames(acc1step) <- c("RMSE","MAPE","RMSE-Summer","MAPE-Summer","RMSE-Winter","MAPE-Winter")
rownames(acc1step) <- modelnames
for (m in 1:length(models)){
    acc1step[m,1:2] <- models[[m]] %>% refit(test_set) %>% accuracy() %>% select(RMSE,MAPE)
}

# summer
test_summer <- test_set %>% filter(month(Time)>5 & month(Time)<10)
for (m in 1:length(models)){
    acc1step[m,3:4] <- models[[m]] %>% refit(test_summer) %>% accuracy() %>% select(RMSE,MAPE)
}

# winter
test_winter <- test_set %>% filter(month(Time)<4)
for (m in 1:length(models)){
    acc1step[m,5:6] <- models[[m]] %>% refit(test_winter) %>% accuracy() %>% select(RMSE,MAPE)
}
