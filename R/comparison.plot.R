#' @title Comparing a financial parameter between two companies
#'
#' @description Compare any parameter on a financial statement graphically
#' @param A A \code{matrix} containing the financial statement from the fist company.
#' @param B A \code{matrix} containing the financial statement from the fist company.
#' @param x A \code{string} containing the variable within the financial statement that will be plotted.
#' @param ticker1 A \code{string} containing the stock ticker of the company in argument A.
#' @param ticker2 A \code{string} containing the stock ticker of the company in argument B.
#' @return A \code{plot} or \code{ggplot} object.
#' @author Ryan Voyack
#' @export
#' @examples
#' comparison.plot('Cash Only',create.data('wmt','Balance-Sheet'),create.data('aapl','Balance-Sheet'),'wmt','aapl')


comparison.plot <- function(x,A,B,ticker1,ticker2){

  y<-rownames(A)[x]

  z<-as.character(A[y,])
  z1<-as.character(B[y,])

  #gets rid of a pesky accounting input (negatives)
  z<-gsub("([)])","",z)
  z1<-gsub("([)])","",z1)


  ###  see if all the numbers in each of the two rows are all scaled by the same values
  #    this will be used in the conditional below
  #    this would be if values ended in B or M (to denote millions or billions of dollars) for example
  options(digits=5)
  yes<-as.character(c(substr(z,nchar(z),nchar(z))))
  yesyes<-as.character(c(substr(z1,nchar(z1),nchar(z1))))

  if((length(unique(yes))==1) & (length(unique(yesyes))==1)){
    if(unique(yes)=='-' & unique(yesyes)=="-")
      stop('Neither of the companies prompted have information for this variable on their Balance Sheets')
  }

  if(((length(unique(yes))==1) & (length(unique(yesyes))==1)) & unique(unique(yes)==unique(yesyes))){
    ###  if each variable set are all in the same denomination (such as millions) within and between each data set
    key<-substr(substr(z,nchar(z),nchar(z)),1,1)[1]
    if(key=="%"){
      legend<- sprintf("All values of '%s' are whole number percentages < 100", y)
    }else{
      legend<- sprintf("All values of '%s' are in %sillions of US dollars", y, key)
    }
    temp1 <- gsub("([M%B)])","",z)
    temp1 <- gsub("([-])","0",temp1)
    temp1 <- as.numeric(gsub("([(])","-",temp1))
    temp2 <- gsub("([M%B)])","",z1)
    temp2 <- gsub("([-])","0",temp2)
    temp2 <- as.numeric(gsub("([(])","-",temp2))

  }else if((((!(isTRUE(as.character(unique(yes)) != '-')) & length(unique(yes))==2))|((!isTRUE(as.character(unique(yesyes)) != '-')) & length(unique(yesyes))==2)) & (unique(unique(yes)==unique(yesyes))|unique(unique(yes)==rev(unique(yesyes))))){
    ###  if every variable is the same but there are some null entries
    i=1
    for(i in c(1:5)){
      if(!(yes[i]=='-')){
        #this is hard coded for the first company but this is okay considering the conditional
        key<-substr(substr(z,nchar(z),nchar(z)),1,1)[1]
      }
    }
    if(key=="%"){
      legend<- sprintf("All values of '%s' are percentages < 100", y)
      ###  if everything is a percentage
      ###  make everything (if all percentages) a numeric input type
      temp1 <- gsub("([M%B)])","",z)
      temp1 <- as.numeric(gsub("([-])","0",temp1))
      temp2 <- gsub("([M%B)])","",z1)
      temp2 <- as.numeric(gsub("([-])","0",temp2))
    }else{
      legend<- sprintf("All values of '%s' are in %sillions of US dollars", y, key)
      i=1
      for(i in c(1:length(nchar(z)))){
        #    this range argument is hard coded for both z and z1, this is obviously reasonable
        if(substr(z,nchar(z),nchar(z))[[i]]=="-"){
          z[i]<- gsub("([-])","0",z[i])
        }else if(substr(z,nchar(z),nchar(z))[[i]]=="B"){
          z[i]<- gsub("([B])","",z[i])
        }else if(substr(z,nchar(z),nchar(z))[[i]]=="K"){
          z[i]<- gsub("([K])","",z[i])
        }else if(substr(z,nchar(z),nchar(z))[[i]]=="M"){
          z[i]<- gsub("([M])","",z[i])
        }
        z[i]<- gsub("([(])","-",z[i])
        z[i]<-as.numeric(z[i])

        if(substr(z1,nchar(z1),nchar(z1))[[i]]=="-"){
          z1[i]<- gsub("([-])","0",z1[i])
        }else if(substr(z1,nchar(z1),nchar(z1))[[i]]=="B"){
          z1[i]<- gsub("([B])","",z1[i])
        }else if(substr(z1,nchar(z1),nchar(z1))[[i]]=="K"){
          z1[i]<- gsub("([K])","",z1[i])
        }else if(substr(z1,nchar(z1),nchar(z1))[[i]]=="M"){
          z1[i]<- gsub("([M])","",z1[i])
        }
        z1[i]<- gsub("([(])","-",z1[i])
        z1[i]<-as.numeric(z1[i])
      }
      temp1<-z
      temp2<-z1
    }
  }else{
    legend<- sprintf("All values of '%s' are in Millions of US dollars", y)
    i=1
    for(i in c(1:length(nchar(z)))){
      #    this range argument is hard coded for both z and z1, this is obviously reasonable
      if(substr(z,nchar(z),nchar(z))[[i]]=="-"){
        z[i]<- as.numeric(gsub("([-])","0",z[i]))
      }else if(substr(z,nchar(z),nchar(z))[[i]]=="M"){
        z[i]<- gsub("([M])","",z[i])
        z[i]<- as.numeric(gsub("([(])","-",z[i]))
      }else if(substr(z,nchar(z),nchar(z))[[i]]=="B"){
        z[i]<- gsub("([B])","",z[i])
        z[i]<- gsub("([(])","-",z[i])
        z[i]<-as.numeric(z[i])*1000
      }else if(substr(z,nchar(z),nchar(z))[[i]]=="K"){
        z[i]<- gsub("([K])","",z[i])
        z[i]<- gsub("([(])","-",z[i])
        z[i]<-as.numeric(z[i])/1000
      }

      if(substr(z1,nchar(z1),nchar(z1))[[i]]=="-"){
        z1[i]<- as.numeric(gsub("([-])","0",z1[i]))
      }else if(substr(z1,nchar(z1),nchar(z1))[[i]]=="M"){
        z1[i]<- gsub("([M])","",z1[i])
        z1[i]<- as.numeric(gsub("([(])","-",z1[i]))
      }else if(substr(z1,nchar(z1),nchar(z1))[[i]]=="B"){
        z1[i]<- gsub("([B])","",z1[i])
        z1[i]<- gsub("([(])","-",z1[i])
        z1[i]<-as.numeric(z1[i])*1000
      }else if(substr(z1,nchar(z1),nchar(z1))[[i]]=="K"){
        z1[i]<- gsub("([K])","",z1[i])
        z1[i]<- gsub("([(])","-",z1[i])
        z1[i]<-as.numeric(z1[i])/1000
      }
    }
    temp1<-z
    temp2<-z1
  }

  #now the function will graph the variable that the user prompted for
  years<-c("2013", "2014", "2015", "2016", "2017")
  data<-data.frame(years,temp1,temp2)

  #plot!
  plot <- plotly::plot_ly(data, x=~years,y=~y) %>%
    plotly::add_trace(y=~temp1, name=ticker1, type='scatter', mode='lines') %>%
    plotly::add_trace(y=~temp2, name=ticker2,  type='scatter', mode='lines') %>%
    plotly::layout(title = legend,
          xaxis = list(title = "Years"),
          yaxis = list(title = y))
}

#YES
