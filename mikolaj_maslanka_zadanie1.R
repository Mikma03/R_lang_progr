

install.packages(c("httr", "jsonlite","R6"))

library(R6)
library(httr)
library(jsonlite)

Currency <- R6Class("Currency", list(
  top = 1,
  currencies = c(),
  data = NULL,
  initialize = function(c,t){
    self$currencies = c(self$currencies,c)
    self$top = t
  },
  currencyList = function() {
    invisible(self)
    return(self$currencies)
  },
  currencyAdd = function(c){
    self$currencies = c(self$currencies,c)
    invisible(self)
  },
  currencyRemove = function(c){
    currencies = self$currencies[self$currencies!=c]
    invisible(self)
  },
  currencyRates = function(c){
    if(length(self$currencies[self$currencies==c])>0){
      url = paste('http://api.nbp.pl/api/exchangerates/rates/A/',c,'/last/',as.character(self$top),'?format=json',sep='')
      res = GET(url)
      self$data = fromJSON(rawToChar(res$content))
      invisible(self)
      return(self$data$rates)
    }
    else{
      invisible(self)
      return("brak wybranej waluty")
    }
  },
  chart = function(c,ylim,col,type,pch){
    self$currencyRates(c)
    plot(self$data$rates$mid~as.Date(self$data$rates$effectiveDate),type=type,
         xlab="Date",ylab="Price",col=col,pch=pch)
    invisible(self)
  },
  chartAdd = function(c,ylim,col,type,pch){
    par(new=TRUE)
    self$chart(c,ylim,col,type,pch)
    invisible(self)
  }
)
)

d = Currency$new("USD",20)
d$currencyRates("USD")
d$currencyAdd("GBP")
d$currencyRates("GBP")
d$currencyRates("KRA")
d$currencyList()
d$chart("USD",ylim=c(3,6),col='red',type='l',pch=4)
d$chartAdd("GBP",ylim=c(3,6),col='blue',type='l',pch=4)



