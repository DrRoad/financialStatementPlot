#' @title Import a financial statement
#'
#' @description Import a financial statement from the web (off of marketwatch.com). This function makes use of rvest and xml2.
#'     This version of the function is to be used practically; outside of the shiny app
#' @param char A \code{string} containing the formal stock ticker of a desired company
#' @param state A \code{numeric} containing the desired financial statement of the company
#'     1 for Income Statement, 2 for Balance sheet and 3 for statement of cash flows
#' @return A \code{list} containing the following attributes:
#' \describe{
#'      \item{visualizeData}{A matrix containing the financial statement}
#'      \item{variables}{A vector of all of the variables contained on the financial statement}
#' }
#' @author Ryan Voyack
#' @importFrom magrittr %>%
#' @export
#' @examples
#' create.data('wmt',2)[[1]] #imports Walmart's balance sheet with data from the last 5 years

createData <- function(char,state){
  if(state==1){
    c<-""
  }else if(state==2){
    c<-"balance-sheet"
  }else if(state==3){
    c<-"cash-flow"
  }else{
    stop("numeric value must be 1, 2 or 3. See ?createData for elaboration of values")
  }

  a<-'https://www.marketwatch.com/investing/stock/'
  b<-'/financials/'
  html <- paste(a,char,b,c,sep='')
  year17 <- xml2::read_html(html) %>%
    rvest::html_nodes(xpath='//tr//*[(((count(preceding-sibling::*) + 1) = 6) and parent::*)]') %>%
    rvest::html_text()
  year16 <- xml2::read_html(html) %>%
    rvest::html_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "valueCell", " " )) and (((count(preceding-sibling::*) + 1) = 5) and parent::*)] | //th[(((count(preceding-sibling::*) + 1) = 5) and parent::*)]') %>%
    rvest::html_text()
  year15 <- xml2::read_html(html) %>%
    rvest::html_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "valueCell", " " )) and (((count(preceding-sibling::*) + 1) = 4) and parent::*)] | //th[(((count(preceding-sibling::*) + 1) = 4) and parent::*)]') %>%
    rvest::html_text()
  year14 <- xml2::read_html(html) %>%
    rvest::html_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "valueCell", " " )) and (((count(preceding-sibling::*) + 1) = 3) and parent::*)] | //th[(((count(preceding-sibling::*) + 1) = 3) and parent::*)]') %>%
    rvest::html_text()
  year13 <- xml2::read_html(html) %>%
    rvest::html_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "valueCell", " " )) and (((count(preceding-sibling::*) + 1) = 2) and parent::*)] | //th[(((count(preceding-sibling::*) + 1) = 2) and parent::*)]') %>%
    rvest::html_text()

  variables <- xml2::read_html(html) %>%
    rvest::html_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "rowTitle", " " ))]') %>%
    rvest::html_text()
  #create matrix

  mylist<-c(year13[-1],year14[-1],year15[-1],year16[-1],year17[-1])

  fiscal.years<-c(year13[1],year14[1],year15[1],year16[1],year17[1])
  visualizeData<-matrix(mylist, ncol = 5)
  rownames(visualizeData)<-variables[-1]
  colnames(visualizeData)<-fiscal.years

  #create return
  returnList <- list(visualizeData, variables)
  return(returnList)
}
