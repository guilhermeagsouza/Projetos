new_data <- test %>% 
  dplyr::select(-id) %>% 
  mutate(year = lubridate::year(date), 
         month = as.numeric(lubridate::month(date)),
         weekdays = lubridate::wday(date))

new_data %<>% 
  inner_join(train %>% dplyr::select(-date, -sales, -year),
                         by = c("weekdays", "month", "store", "item")) %>% 
  distinct()

new_data %<>% dplyr::filter(store ==1 & item ==1)

train %<>% 
  dplyr::filter(store == 1 & item ==1)

ts_train2 <- ts(train[1:1826, 4])

xtrain2 <- ts(train[1:1826,5:8])
xtest2 <- new_data %>% 
  dplyr::select(weekdays, month, sales_week, sales_month) %>% 
  ts()

nnxreg_fit2 <- nnetar(ts_train2, xreg = xtrain2, lambda = 0, decay = 0, 
                     maxit = 1000) #Não faz o decaimento (decay)
nnxreg_fcast2 <- forecast(nnxreg_fit2,xreg = xtest2, h = 90)
#train %<>% mutate(weekdays = as.numeric(wday(date)), 
#                 month = as.numeric(month))

ts_train <- ts(train[1:ntrain,4])
ts_test <- ts(train[(ntrain+1):nrow(train),4])
xtrain <- train[1:ntrain,5:8]
xtest <- train[(ntrain+1):nrow(train),5:8]

#Sem regressores
decay <- 0
maxit <- 100
nn_fit <- nnetar(ts_train, decay=decay, maxit=maxit)
nn_fcast <- forecast(nn_fit, h=ntest)

plot(ts_train, main=paste("without xreg,", ", decay =", decay, 
                                          ", maxit =", maxit))
lines(nn_fcast$mean, col="blue")
lines((length(ts_train)+1):nrow(train), ts_test)

#Com regressores

nnxreg_fit <- nnetar(ts_train, xreg=xtrain, lambda = 0, decay = 0, 
                     maxit = 1000) #Não faz o decaimento (decay)
nnxreg_fcast <- forecast(nnxreg_fit,xreg=xtest, h=ntest)

plot(ts_train, main=paste("with xreg"))
lines(nnxreg_fcast$mean,col="blue")
lines((length(ts_train)+1):nrow(train), ts_test)

valor_teste <- data.frame(teste = ts_test, forecast = nnxreg_fcast$mean, 
                          forecast_round = ceiling(nnxreg_fcast$mean))
