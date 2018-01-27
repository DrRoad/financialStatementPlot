library(shiny)
library(DT)
library(plotly)
library(rvest)
  
#require create.data()
#require comparisonPlot(), or:
devtools::install_github("ryanvoyack/financialStatementPlot")
library(financialStatementPlot)
#######app code########
hello1<-c("\r\n                 Sales/Revenue\r\n            ","Sales Growth"," Cost of Goods Sold (COGS) incl. D&A","COGS excluding D&A","Depreciation & Amortization Expense","Depreciation","Amortization of Intangibles","COGS Growth"," Gross Income","Gross Income Growth","Gross Profit Margin",""," SG&A Expense","Research & Development","Other SG&A","SGA Growth","Other Operating Expense","Unusual Expense","EBIT after Unusual Expense","Non Operating Income/Expense","Non-Operating Interest Income","Equity in Affiliates (Pretax)"," Interest Expense","Interest Expense Growth","Gross Interest Expense","Interest Capitalized"," Pretax Income","Pretax Income Growth","Pretax Margin","Income Tax","Income Tax - Current Domestic","Income Tax - Current Foreign","Income Tax - Deferred Domestic","Income Tax - Deferred Foreign","Income Tax Credits","Equity in Affiliates","Other After Tax Income (Expense)","Consolidated Net Income","Minority Interest Expense"," Net Income","Net Income Growth","Net Margin Growth","Extraordinaries & Discontinued Operations","Extra Items & Gain/Loss Sale Of Assets","Cumulative Effect - Accounting Chg","Discontinued Operations","Net Income After Extraordinaries","Preferred Dividends","Net Income Available to Common"," EPS (Basic)","EPS (Basic) Growth","Basic Shares Outstanding"," EPS (Diluted)","EPS (Diluted) Growth","Diluted Shares Outstanding"," EBITDA","EBITDA Growth","EBITDA Margin")
hello2<-c(" Cash & Short Term Investments","Cash Only","Short-Term Investments","Cash & Short Term Investments Growth","Cash & ST Investments / Total Assets"," Total Accounts Receivable","Accounts Receivables, Net","Accounts Receivables, Gross","Bad Debt/Doubtful Accounts","Other Receivables","Accounts Receivable Growth","Accounts Receivable Turnover","Inventories","Finished Goods","Work in Progress","Raw Materials","Progress Payments & Other","Other Current Assets","Miscellaneous Current Assets","Total Current Assets","","Net Property, Plant & Equipment","Property, Plant & Equipment - Gross",
  "Buildings","Land & Improvements","Computer Software and Equipment","Other Property, Plant & Equipment","Accumulated Depreciation","Total Investments and Advances","Other Long-Term Investments","Long-Term Note Receivable","Intangible Assets","Net Goodwill","Net Other Intangibles","Other Assets","Tangible Other Assets"," Total Assets","Assets - Total - Growth","","ST Debt & Current Portion LT Debt","Short Term Debt","Current Portion of Long Term Debt"," Accounts Payable","Accounts Payable Growth","Income Tax Payable","Other Current Liabilities","Dividends Payable","Accrued Payroll","Miscellaneous Current Liabilities"," Total Current Liabilities","Long-Term Debt",
  "Long-Term Debt excl. Capitalized Leases","Non-Convertible Debt","Convertible Debt","Capitalized Lease Obligations","Provision for Risks & Charges","Deferred Taxes","Deferred Taxes - Credit","Deferred Taxes - Debit","Other Liabilities","Other Liabilities (excl. Deferred Income)","Deferred Income"," Total Liabilities","Non-Equity Reserves","Total Liabilities / Total Assets","Preferred Stock (Carrying Value)","Redeemable Preferred Stock","Non-Redeemable Preferred Stock"," Common Equity (Total)","Common Stock Par/Carry Value","Retained Earnings","ESOP Debt Guarantee","Cumulative Translation Adjustment/Unrealized For. Exch. Gain","Unrealized Gain/Loss Marketable Securities","Revaluation Reserves",
  "Treasury Stock","Common Equity / Total Assets"," Total Shareholders' Equity","Total Shareholders' Equity / Total Assets","Accumulated Minority Interest","Total Equity","Liabilities & Shareholders' Equity" )
hello3<- c(" Net Income before Extraordinaries","Net Income Growth","Depreciation, Depletion & Amortization","Depreciation and Depletion","Amortization of Intangible Assets","Deferred Taxes & Investment Tax Credit","Deferred Taxes","Investment Tax Credit","Other Funds","Funds from Operations","Extraordinaries","Changes in Working Capital","Receivables","Accounts Payable","Other Assets/Liabilities"," Net Operating Cash Flow","Net Operating Cash Flow Growth","Net Operating Cash Flow / Sales",""," Capital Expenditures","Capital Expenditures (Fixed Assets)","Capital Expenditures (Other Assets)","Capital Expenditures Growth","Capital Expenditures / Sales","Net Assets from Acquisitions","Sale of Fixed Assets & Businesses","Purchase/Sale of Investments","Purchase of Investments","Sale/Maturity of Investments","Other Uses","Other Sources"," Net Investing Cash Flow","Net Investing Cash Flow Growth","Net Investing Cash Flow / Sales","","Cash Dividends Paid - Total","Common Dividends","Preferred Dividends","Change in Capital Stock","Repurchase of Common & Preferred Stk.","Sale of Common & Preferred Stock","Proceeds from Stock Options","Other Proceeds from Sale of Stock","Issuance/Reduction of Debt, Net","Change in Current Debt","Change in Long-Term Debt","Issuance of Long-Term Debt","Reduction in Long-Term Debt","Other Funds","Other Uses","Other Sources"," Net Financing Cash Flow","Net Financing Cash Flow Growth","Net Financing Cash Flow / Sales","Exchange Rate Effect","Miscellaneous Funds","Net Change in Cash"," Free Cash Flow","Free Cash Flow Growth","Free Cash Flow Yield")

# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com


ui <- fluidPage(

  # Application title
  titlePanel("Financial statement variable comparing app"),
  # Sidebar with a slider input for number of bins

  sidebarLayout(
    sidebarPanel(

      h5('Please select the type of financial statement that you wish to compare'),
      selectInput("state", "Financial Statement", c("None", "Income-Statement", "Balance-Sheet", "Cash-Flows")),
      h5('Please enter the stock tickers of 2 companies that you wish to compare'),
      textInput('comp1', 'Company 1', value = "wmt", width = NULL, placeholder = NULL),
      textInput('comp2', 'Company 2', value = "aapl", width = NULL, placeholder = NULL),
      actionButton("Button", "import", icon = icon("line-chart")),
      uiOutput("statements")

      ),


    mainPanel(
      tabsetPanel(
        tabPanel("Financial Statement 1", DT::dataTableOutput('table1')),
        tabPanel("Financial Statement 2", DT::dataTableOutput('table2')),
        tabPanel("Comparing", plotly::plotlyOutput('comparison'), print("Hover over the plot to interact with specific values. Choose another variable in the drop-down bar to immediately render another plot"))
      )
    )
  )
)


server <- function(input, output) {

  output$statements <- renderUI({
    if(input$state == "Income-Statement"){
      selectInput("property", "What Would You Like To Compare:", hello1)
    }else if(input$state == "Balance-Sheet"){
      selectInput("property", "What Would You Like To Compare:", hello2)
    }else if(input$state == "Cash-Flows"){
      selectInput("property", "What Would You Like To Compare:", hello3)
    }
  })

  aa <- eventReactive(input$Button, valueExpr = {
    create.data(char = input$comp1, state = input$state)
  })
  bb <- eventReactive(input$Button, valueExpr = {
    create.data(char = input$comp2, state = input$state)[[1]]
  })
  output$comparison <- renderPlotly({
    comparison.plot(x = (grep(input$property, aa()[[2]][-1])), A = aa()[[1]], B = bb(), ticker1 = input$comp1, ticker2 = input$comp2)
  })
  output$table1 <- DT::renderDataTable({
    DT::datatable( aa()[[1]], rownames = TRUE)
  })
  output$table2 <- DT::renderDataTable({
    DT::datatable( bb(), rownames = TRUE)
  })
}
# Run the application
shinyApp(ui = ui, server = server)
