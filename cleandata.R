## function to grab time series data

storepredict <- function(dataset, storeNumber, numberofperiods) {
  temp0 <-subset(dataset, dataset[[1]] == storeNumber, select=c(2:3))
  dates <- as.character.Date(temp0[[1]], format = "%Y%m%d")
  nextdate <- as.numeric(as.character.Date(max(dates),format = "%Y%m%d"))
  nextdate <- nextdate +7
  a <- c(0:(numberofperiods-1))
  a <- 7 * a
  weekNum <- as.Date.character(nextdate, format = "%Y%m%d") + a
  temp <- subset(temp0, select=c(2:2))
  rownames(temp) <- 1:nrow(temp)
  if (nrow(temp) < 8) {
    storeforecast <- 0
  } else {
 x <- ts(temp)
 xforecast <- HoltWinters(x, gamma = FALSE)
 temp2 <- forecast.HoltWinters(xforecast, h=numberofperiods)
 plot(temp2, main = paste(c("Forecast for ",storeNumber)))
 temp2 <- data.frame(temp2$mean,temp2$lower,temp2$upper)
 storeforecast <- data.frame(storeNumber,weekNum,temp2)}
 storeforecast
}