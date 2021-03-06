% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/createData function.R
\name{createData}
\alias{createData}
\title{Import a financial statement}
\usage{
createData(char, state)
}
\arguments{
\item{char}{A \code{string} containing the formal stock ticker of a desired company}

\item{state}{A \code{numeric} containing the desired financial statement of the company
1 for Income Statement, 2 for Balance sheet and 3 for statement of cash flows}
}
\value{
A \code{list} containing the following attributes:
\describe{
     \item{visualizeData}{A matrix containing the financial statement}
     \item{variables}{A vector of all of the variables contained on the financial statement}
}
}
\description{
Import a financial statement from the web (off of marketwatch.com). This function makes use of rvest and xml2.
    This version of the function is to be used practically; outside of the shiny app
}
\examples{
create.data('wmt',2)[[1]] #imports Walmart's balance sheet with data from the last 5 years
}
\author{
Ryan Voyack
}
