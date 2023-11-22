# nbg draft script

# load libraries
library(data.table);library(readxl)
library(prophet);library(stringr);library(zoo)
options(scipen = 999)
#load  data 
# mcc volume sales
# mcc_raw <- read_xlsx("Volumes BDG 24 statistics projection file.xlsx",sheet = "Start of Month") 
mcc_raw <- read_xlsx("Adreas.xlsx",sheet = "Sheet1") 
setDT(mcc_raw)

# cpi
cpi <- read_xls("CPI_GDP_VOLUMES.xls",sheet = "CPI")
setDT(cpi)

# date operations 
cpi[,date:=paste0(Year,"-",str_pad(Month,2,side = "left",pad = 0))]
cpi[,date:=as.yearmon(date)]
cpi[,c("Month","Year"):=NULL]
setcolorder(cpi,c(2,1))
setnames(cpi,c("ds","y"))
cpi <- cpi[5:nrow(cpi),]

# tourism index
ti <- read_xls("CPI_GDP_VOLUMES.xls",sheet = "TI")
setDT(ti)

# date operations 
ti[,date:=paste0(Year,Quarter)]
ti[,date:=as.yearqtr(date)]
ti[,c("Year","Quarter"):=NULL]
setcolorder(ti,c(2,1))
setnames(ti,c("ds","y"))

# interpolation operations - linear
start_date <- as.Date(min(ti$ds))
end_date <- as.Date(max(ti$ds))
monthly_dates <- seq.Date(start_date, end_date, by = "month")

interpolated_values <- approx(
  x = as.Date(ti$ds),
  y = ti$y,
  xout = monthly_dates,
  method = "linear"
)

# plot(ti$y,type='l')
# plot(interpolated_values$y,type='l')
ti_interpolated <- data.frame(ds=interpolated_values$x,
                              y=interpolated_values$y)
ti_interpolated <- ti_interpolated[5:nrow(ti_interpolated),]

# predict external regressors

# cpi
c_f <- prophet(cpi)
future_r1 <- make_future_dataframe(c_f, periods = 16, freq = 'month')
forecast_r1 <- predict(c_f, future_r1)
tail(forecast_r1[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
plot(c_f, forecast_r1)
prophet_plot_components(c_f, forecast_r1)

# ti
ti_f <- prophet(ti_interpolated)
future_r2 <- make_future_dataframe(ti_f, periods = 20, freq = 'month')
forecast_r2 <- predict(ti_f, future_r2)
tail(forecast_r2[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
plot(ti_f, forecast_r2)
prophet_plot_components(ti_f, forecast_r2)


# select a sample mcc
ms_frame <- mcc_raw[,sum(Volumes),by=.(MCC)][order(-V1)]
ms_frame[,ms:=V1/sum(V1)]
ms_frame[,ms_cum:=cumsum(ms)]
id_run <- ms_frame[ms_cum<.90]$MCC


mcc_raw[,MCC := ifelse(MCC %chin% id_run,MCC,"Rest")]

list_of_preds <- list()
lp <- 1
for (i in unique(mcc_raw$MCC)){
  
  sample_set <- mcc_raw[MCC == i]
  cat(i,"\n")
  # date operations
  sample_set[,c("month","year"):=.(month(`Month`),year(`Month`))]
  roll_sample <- sample_set[,list(Volume=sum(Volumes)),by=.(month,year)]
  roll_sample[,date:=paste0(year,"-",str_pad(month,2,side = "left",pad = 0))]
  roll_sample[,date:=as.yearmon(date)]
  roll_sample[,c("month","year"):=NULL]
  setcolorder(roll_sample,c(2,1))
  setnames(roll_sample,c("ds","y"))
  
  # plot(roll_sample$y,type = "l")
  
  # add regressors
  # create set
  full_data <- cbind(roll_sample,
                     forecast_r1[1:nrow(roll_sample),c('yhat')],
                     forecast_r2[1:nrow(roll_sample),c('yhat')])
  mm <- prophet()
  mm <- add_regressor(mm,'V2')
  mm <- add_regressor(mm,'V3')
  mm <- fit.prophet(mm, full_data)
  
  
  # make predictions
  future <- make_future_dataframe(mm, periods = 15, freq = 'month')
  future$V2 <- forecast_r1$yhat
  future$V3 <- forecast_r2$yhat
  forecast <- predict(mm, future)
  forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')]
  plot(mm, forecast)
  prophet_plot_components(mm, forecast)
  
  predictions <- data.table(date = forecast$ds,
                            volume = forecast$yhat,
                            mcc = unique(sample_set$MCC))
  
  predictions[,type := ifelse(date < '2023-10-01',"Actual","Forecast")]
  list_of_preds[[lp]] <- predictions
  lp <- lp +1
}

final_pred <- rbindlist(list_of_preds)

fwrite(final_pred,"predictions_nbg.csv")
