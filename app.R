library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(quantmod)
library(dygraphs)
library(dashboardthemes)
library(DT)
library(shinyjs)
library(tidyverse)

options(stringsAsFactors = FALSE)
stockList <- readRDS('./data/stockList.rds') %>% filter(symbol %in% names(readRDS('./data/xbrlData.rds')))

theme_blue_gradient <- shinyDashboardThemeDIY(
  
  ### general
  appFontFamily = "Arial"
  ,appFontColor = "#OD1540"
  ,primaryFontColor = "#OD1540"
  ,infoFontColor = "rgb(0,0,0)"
  ,successFontColor = "rgb(0,0,0)"
  ,warningFontColor = "rgb(0,0,0)"
  ,dangerFontColor = "rgb(0,0,0)"
  ,bodyBackColor = "rgb(248,248,248)"
  
  ### header
  ,logoBackColor = "#00a4e3"
  
  ,headerButtonBackColor = "#00a4e3"
  ,headerButtonIconColor = "rgb(75,75,75)"
  ,headerButtonBackColorHover = "rgb(210,210,210)"
  ,headerButtonIconColorHover = "rgb(0,0,0)"
  
  ,headerBackColor = "#ffffff"
  ,headerBoxShadowColor = "#00a4e3"
  ,headerBoxShadowSize = "2px 2px 2px"
  
  ### sidebar
  ,sidebarBackColor = "#ffffff"
  ,sidebarPadding = 0
  
  ,sidebarMenuBackColor = "transparent"
  ,sidebarMenuPadding = 0
  ,sidebarMenuBorderRadius = 0
  
  ,sidebarShadowRadius = "3px 5px 5px"
  ,sidebarShadowColor = "#aaaaaa"
  
  ,sidebarUserTextColor = "rgb(255,255,255)"
  
  ,sidebarSearchBackColor = "#00a4e3"
  ,sidebarSearchIconColor = "rgb(153,153,153)"
  ,sidebarSearchBorderColor = "#00a4e3"
  
  ,sidebarTabTextColor = "#OD1540"
  ,sidebarTabTextSize = 13
  ,sidebarTabBorderStyle = "none none solid none"
  ,sidebarTabBorderColor = "#00a4e3"
  ,sidebarTabBorderWidth = 1
  
  ,sidebarTabBackColorSelected = "#ffffff"
  ,sidebarTabTextColorSelected = "#00a4e3"
  ,sidebarTabRadiusSelected = "0px 20px 20px 0px"
  
  ,sidebarTabBackColorHover = "#00a4e3"
  ,sidebarTabTextColorHover = "#ffffff"
  ,sidebarTabBorderStyleHover = "none none solid none"
  ,sidebarTabBorderColorHover = "rgb(75,126,151)"
  ,sidebarTabBorderWidthHover = 1
  ,sidebarTabRadiusHover = "0px 20px 20px 0px"
  
  ### boxes
  ,boxBackColor = "rgb(255,255,255)"
  ,boxBorderRadius = 5
  ,boxShadowSize = "0px 1px 1px"
  ,boxShadowColor = "#00a4e3"
  ,boxTitleSize = 16
  ,boxDefaultColor = "#00a4e3"
  ,boxPrimaryColor = "#00a4e3"
  ,boxInfoColor = "#00a4e3"
  ,boxSuccessColor = "rgba(0,255,213,1)"
  ,boxWarningColor = "rgb(244,156,104)"
  ,boxDangerColor = "rgb(255,88,55)"
  
  ,tabBoxTabColor = "rgb(255,255,255)"
  ,tabBoxTabTextSize = 14
  ,tabBoxTabTextColor = "#OD1540"
  ,tabBoxTabTextColorSelected = "rgb(0,0,0)"
  ,tabBoxBackColor = "rgb(255,255,255)"
  ,tabBoxHighlightColor = "rgba(44,222,235,1)"
  ,tabBoxBorderRadius = 5
  
  ### inputs
  ,buttonBackColor = "rgb(245,245,245)"
  ,buttonTextColor = "#OD1540"
  ,buttonBorderColor = "rgb(200,200,200)"
  ,buttonBorderRadius = 5
  
  ,buttonBackColorHover = "rgb(235,235,235)"
  ,buttonTextColorHover = "rgb(100,100,100)"
  ,buttonBorderColorHover = "rgb(200,200,200)"
  
  ,textboxBackColor = "rgb(255,255,255)"
  ,textboxBorderColor = "rgb(200,200,200)"
  ,textboxBorderRadius = 5
  ,textboxBackColorSelect = "rgb(245,245,245)"
  ,textboxBorderColorSelect = "rgb(200,200,200)"
  
  ### tables
  ,tableBackColor = "rgb(255,255,255)"
  ,tableBorderColor = "rgb(240,240,240)"
  ,tableBorderTopSize = 1
  ,tableBorderRowSize = 1
  
)



  ui = dashboardPagePlus(
    
    header = dashboardHeaderPlus(
      title = 'E-N-Fin Explorer',
      enable_rightsidebar = FALSE,
      left_menu = tagList(
      selectizeInput('ticker', 'Company', 
                     choices = sort(unique(stockList$company)),
                     selected = 'Apache Corporation', multiple=FALSE)
      )
    ),
    sidebar = dashboardSidebar(
      br(),
      br(),
      br(),
      br(),
      
      sidebarMenu(
        id = 'tabs',
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dollar-sign")),
        menuItem("About", tabName = "packages", icon = icon("gears"))
      )
      
      
    ),
    body = dashboardBody(
      theme_blue_gradient,
      shinyjs::useShinyjs(),
      # infoBoxes
      tabItems(
        tabItem("dashboard",
          fluidRow(
            column(width = 12,
            boxPlus(
              title = "Candlestick Chart", 
              closable = FALSE, 
              width = 12,
              status = "info", 
              solidHeader = FALSE, 
              collapsible = TRUE,
              enable_dropdown = FALSE,
              dygraphs::dygraphOutput('stonks'),
              h6('Source: Quantmod/Yahoo Finance')
            )
          )
          ),
          fluidRow(
            column(width = 12,
                   boxPlus(
                     title = 'Bulk Financial Download - By Company',
                     closable = FALSE,
                     width = 12,
                     status = "info",
                     solidHeader = FALSE,
                     collapsible = TRUE,
                     enable_dropdown = FALSE,
                     fluidRow(
                       column(width = 3,
                     actionButton("Calculate", 
                                  label = ui <- fluidPage(
                                    tags$script(src = "https://www.paypalobjects.com/api/checkout.js "),
                                    tags$script("paypal.Button.render({
                                    // Configure environment
                                    env: 'sandbox',
                                    client: {
                                    sandbox: 'AUzCJ8LZWpfr-5q8RbpAbzgbnviGF01LygUugclYSUENDcN2SeN8n_hRjq8B7nFaEor1z4yWOlcgtbd_',
                                    production: 'demo_production_client_id'
                                    },
                                    // Customize button (optional)
                                    locale: 'en_US',
                                    style: {
                                    size: 'small',
                                    color: 'gold',
                                    shape: 'pill',
                                    },
                                    // Set up a payment
                                    payment: function (data, actions) {
                                    return actions.payment.create({
                                    transactions: [{
                                    amount: {
                                    total: '0.01',
                                    currency: 'USD'
                                    }
                                    }]
                                    });
                                    },
                                    // Execute the payment
                                    onAuthorize: function (data, actions) {
                                    return actions.payment.execute()
                                    .then(function () {
                                    // Show a confirmation message to the buyer
                                    window.alert('Thank you for your purchase!');
                                    });
                                    }
                                    }, '#Calculate');"),
                                    tags$div(id = "Calculate")))
                     ),
                     column(
                       width =3,
                     hidden(div(
                       id='actions',
                     downloadButton("downloadData", "Download")
                     ))
                     )
                     )
                   )
                   )
          ),
          fluidRow(
            column(width = 12,
                   boxPlus(
                     title = "Financial Statement Data", 
                     closable = FALSE, 
                     width = 12,
                     status = "info", 
                     solidHeader = FALSE, 
                     collapsible = TRUE,
                     enable_dropdown = FALSE,
                     fluidRow(
                       column(width = 2,
                        selectizeInput('period', 'Filing Period', choices = '')
                        ),
                       column(width = 2,
                              selectizeInput('type', 'Statement Type', choices = '')
                       ),
                     column(width = 4,
                          selectizeInput('table', 'Table', choices = '')  
                     
                      ),
                     column(width = 2,
                            selectizeInput('endDate', 'Date', choices = '')  
                            ),
                     column(width = 2,
                            selectizeInput('duration', 'Months', choices = '')
                            )
                   ),
                   fluidRow(
                     column(width =12,
                            DT::dataTableOutput('financials')
                            )
                   )
                   )
            )
          )
        ),
        tabItem(
          tabName = "packages",
          box(
            title = "Package List",
            status = "primary",
            width = NULL,
            userList(
              userListItem(
                src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", 
                user_name = "Shiny", 
                description = ""
              ),
              userListItem(
                src = "shinydb.png", 
                user_name = "Shiny Dashboard", 
                description = ""
              ),
              userListItem(
                src = "https://d33wubrfki0l68.cloudfront.net/071952491ec4a6a532a3f70ecfa2507af4d341f9/c167c/images/hex-dplyr.png", 
                user_name = "Tidyverse", 
                description = ""
              ),
              userListItem(
                src = "shinyjs.png", 
                user_name = "shinyjs", 
                description = "Dean Attali"
              ),
              userListItem(
                src = "shinydbplus.jpg", 
                user_name = "Shiny Dashboard+", 
                description = "Appsilon Data Science"
              ),
              userListItem(
                src = "quantmod.png", 
                user_name = "quantmod", 
                description = "Jeffrey A. Ryan"
              ),
              userListItem(
                src = "dbThems.png", 
                user_name = "Dashboard Themes", 
                description = "nik01010"
              ),
              userListItem(
                src = "dygraphs.png", 
                user_name = "dygraphs", 
                description = ""
              ),
              userListItem(
                src = "dt.png", 
                user_name = "DT", 
                description = ""
              )
            )
          )
        )
      )
    ),
    rightsidebar = rightSidebar(),
    footer = dashboardFooter(
      left_text = "E-N-Fin Explorer",
      right_text = "2020"
    ),
    title = "EnFinExplorer"
  )
  
  server = function(input, output, session) { 
    values <- reactiveValues()
    shinyjs::show('actions')
    stockInfo <- reactive(
      (stockList %>% filter(company%in% input$ticker))
    )
    observe(print(session$token))
    #observeEvent(input$Calculate, {
    #  print(input$Calculate == 0)
    #})
    
    #observe(print(input$Calculate))
    observeEvent(input$Calculate, {
      shinyjs::hide('Calculate')
      
      shinyjs::show('actions')
    })
    
    observeEvent(input$ticker, {
      
      shinyjs::hide('actions')
      shinyjs::show('Calculate')
    })
    
    observe({
      if(is.null(input$ticker)||input$ticker == ''){
        values$stock <- NULL
      } else {
        stock <- NULL
        rm(stock)
        #ticker <- input$operator
        ticker <- stockInfo()$symbol
        #print(ticker)
        stock <- getSymbols(ticker, src='yahoo', auto.assign = FALSE, setSymbolLookup('stock'))
        names(stock) <- c('Open', 'High', 'Low', 'Close', 'Volume', 'Adjusted')
        values$stock <- stock
      }
    })
    

    
    output$stonks <- dygraphs::renderDygraph({
      #print(input$dateRange)
      if(is.null(values$stock)){
        NULL
      } else {
      #print(head(tables()))
        dygraph(values$stock[,1:4]) %>%
          dyCandlestick()%>%
          dyAxis("y", label = "Share Price, US$") %>%
          dyRangeSelector(height = 20)
      }
      
    })
    
    tables <- reactive(readRDS('./data/xbrlData.rds')[[stockInfo()$symbol]] %>% mutate(Period = replace(Period, is.na(Period), 0)))
    
    observe(updateSelectizeInput(session, 'period', choices = unique(tables()$PERIOD)))
    observe(updateSelectizeInput(session, 'type', choices = unique((tables() %>% filter(PERIOD %in% input$period))$Type)))
    observe(updateSelectizeInput(session, 'table', choices = unique((tables() %>% filter(PERIOD %in% input$period) %>%
                                                                    filter(Type %in% input$type) )$Table)))
    observe(updateSelectizeInput(session, 'endDate', choices = unique((tables() %>% filter(PERIOD %in% input$period)%>%
                                                                         filter(Type %in% input$type) %>%
                                                                       filter(Table %in% input$table))$endDate)))
    observe(updateSelectizeInput(session, 'duration', choices = unique((tables() %>% filter(PERIOD %in% input$period)%>%
                                                                          filter(Type %in% input$type) %>%
                                                                       filter(Table %in% input$table) %>%
                                                                         filter(endDate %in% input$endDate))$Period)))
    
    tables1 <- reactive(tables() %>% filter(PERIOD %in% input$period) %>%
                          filter(Type %in% input$type) %>%
                          filter(Table %in% input$table) %>% filter(endDate %in% input$endDate) %>%
                          filter(Period %in% input$duration))
    

  
  output$financials <- DT::renderDataTable({
    if(is.null(input$duration)||input$duration == ''||
       is.null(input$period)||input$period == ''||
       is.null(input$table)||input$table == ''||
       is.null(input$type)||input$type == ''||
       is.null(input$endDate)||input$endDate == ''){
      NULL
    } else {
      DT::datatable(tables1() %>% arrange(Order) %>% select(Order, Label, Value = fact, Units) %>%
                      mutate(Order = seq(1, n(), 1)), rownames = FALSE,
                    extensions = c('Buttons', 'Scroller'), 
                    options = list(
                      dom = 'Bfrtip',
                      scrollX = TRUE,
                      scrollY = TRUE,
                      deferRender = TRUE,
                      paging = FALSE,
                      searching = FALSE,
                      buttons = c('copy', 'csv', 'excel')
                    ))
    }
    
  })
  
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(stockInfo()$symbol, "financials.csv", sep = "")
    },
    content = function(file) {
      write.csv(tables(), file, row.names = FALSE)
    }
  )
  

  
  }



shinyApp(ui, server)