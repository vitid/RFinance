library(RMySQL)
library(dplyr)
library(quantmod)
library(tseries)
library(forecast)

setwd("F:\\Vitid\\github\\RFinance")

data = read.table("set_1_min.20150601_to20150715.csv",header = T,sep = ",")
data$Open = as.numeric(gsub(",","",data$Open))
data$Close = as.numeric(gsub(",","",data$Close))
data$High = as.numeric(gsub(",","",data$High))
data$Low = as.numeric(gsub(",","",data$Low))
data$Volume = as.numeric(gsub(",","",data$Volume))

band_oc = data$Close - data$Open
band_oc = band_oc[band_oc!=0]
