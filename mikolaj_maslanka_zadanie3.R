install.packages("R6")
install.packages("tibble")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("munsell")
install.packages("labeling")
library(labeling)
library(munsell)
library(ggplot2)
library(R6)
library(tibble)
library(tidyverse)


DataObject <- R6Class("DataObject", list(
  data = NULL,
  org = NULL,
  initialize = function(d){
    self$data = d
    self$org = d
    invisible(self)
  },
  value = function(){
    invisible(self)
    return(self$data)
  },
  convert = function(){
    if(length(class(self$data)['tbl_df'])>0){
      self$data = as_data_frame(self$data)
    }
    else {
      self$data = as_tibble(self$data)
    }
    invisible(self)
  },
  op = function(f){
    self$data = f
    invisible(self)
  },
  reset = function(){
    self$data = self$org
  }
))

fileURL <- "http://michal.ramsza.org/lectures/2_r_programming/data/data_2.csv"
do = DataObject$new(d = read.csv(fileURL))
#do = DataObject$new(d = data_2)

class(do$value())
do$convert()
class(do$value())
do$value()

do$op(select(.data = do$value(),Mileage,Price,Brand))
do$value()
do$reset()
do$value()


do$op(select(.data=do$value(),Mileage, Price, Brand))
do$op(filter(.data=do$value(),Mileage < 50000))
do$op(filter(.data=do$value(),Brand %in% c("Honda", "Fiat")))


ggplot(data = (do$value()), mapping = aes(x = Price)) +
  geom_histogram(bins = 100, aes(fill = Brand, color = Brand, y = ..density..)) +
  geom_density(fill = "blue", alpha = 0.2) + 
  facet_wrap(~ Brand, nrow = 2)

