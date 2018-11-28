## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ------------------------------------------------------------------------
library(ggplot2)

## ------------------------------------------------------------------------
ex1 <- data.frame(stretch=c(46,54,48,50,44,42,52), distance=c(148,182,173,166,109,141,166))
ex1

ggplot(ex1, aes(distance, stretch)) + geom_point(col="blue", size=3)

## ------------------------------------------------------------------------
ex2 <- data.frame(year=c(1970:1979), snow.cover=c(6.5,12.0,14.9,10.0,10.7,7.9,21.9,12.5,14.5,9.2))
ex2

## ------------------------------------------------------------------------
ggplot(ex2, aes(snow.cover, year)) + geom_point(col="green", size=3)

## ------------------------------------------------------------------------
ggplot(ex2, aes(snow.cover)) + geom_histogram(fill="green", col="darkgreen", bins = 4)

## ------------------------------------------------------------------------
ex3 = read.csv("ex3.csv", na = "NR")
ex3

## ------------------------------------------------------------------------
fahrenheit_to_celsius = function(fahrenheit_value) {
  (fahrenheit_value-32) * 5/9
}

inches_to_mm = function(inches) {
  inches * 25.4
}

fahrenheit_columns_to_celsius = function(dataframe) {
  fahrenheit_columns = names(dataframe)[grepl("\\.F\\.", names(dataframe))]
  fahrenheit_indices = which(names(dataframe) %in% fahrenheit_columns)
  dataframe[,fahrenheit_indices] = apply(dataframe[,fahrenheit_indices], 2, fahrenheit_to_celsius)
  names(dataframe) = gsub("\\.F\\.", ".C.", names(dataframe))
  dataframe  
}

inches_columns_to_mm = function(dataframe) {
  inches_columns = names(dataframe)[grepl("\\.in\\.", names(dataframe))]
  inches_indices = which(names(dataframe) %in% inches_columns)
  dataframe[,inches_indices] = apply(dataframe[,inches_indices], 2, inches_to_mm)
  names(dataframe) = gsub("\\.in\\.", ".mm.", names(dataframe))
  dataframe  
}

ex3 = fahrenheit_columns_to_celsius(ex3)
ex3 = inches_columns_to_mm(ex3)
ex3

## ------------------------------------------------------------------------
ggplot(ex3, aes(Year, Warmest.Minimum.Temperature..C.)) + geom_line(col="red")

## ------------------------------------------------------------------------

ggplot(ex3, aes(x = Year)) + 
  geom_line(aes(y = Coldest.Minimum.Temperature..C., colour = "Coldest")) +
  geom_line(aes(y = Warmest.Minimum.Temperature..C., colour = "Warmest")) +
  scale_color_manual(name = "Temperature", values = c("blue", "red")) +
  labs(x = "Year", y = "Temperature (C)")
  

