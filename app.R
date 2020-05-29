library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(quantmod)
library(dygraphs)
library(dashboardthemes)
library(DT)
library(shinyjs)
library(tidyverse)
library(tidyquant)
library(billboarder)
#library(tuichartr)
library(stringr)
library(glue)

options(stringsAsFactors = FALSE)

stockListX <- rbind(tidyquant::tq_exchange('AMEX'),tidyquant::tq_exchange('NASDAQ'),tidyquant::tq_exchange('NYSE')) %>%
  filter(!duplicated(symbol)) %>% filter(!is.na(sector)) %>% filter(!is.na(industry)) %>% filter(!is.na(market.cap))

#print(head(stockListX))
stockList <- stockListX %>% filter(symbol %in% readRDS('./data/labelTicker.rds'))
#print(head(stockList))
cols <- c('#00a4e3', '#adafb2', '#a31c37', '#d26400', '#eaa814', '#5c1848', '#786592', '#ff4e50', '#027971', '#008542', '#5c6d00','#0D1540', '#06357a' )


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
      title = 'XBRL-Data',
      enable_rightsidebar = FALSE,
      left_menu = tagList(
        pickerInput(
          inputId = "ticker",
          label = "Company", 
          choices = sort(unique(stockList$company)),
          options = list(
            `live-search` = TRUE)
        )
      # selectizeInput('ticker', 'Company', 
      #                choices = sort(unique(stockList$company)),
      #                selected = 'Apache Corporation', multiple=FALSE)
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
        menuItem("Graphs", tabName = "graphs", icon = icon("chart-pie")),
        menuItem("About", tabName = "packages", icon = icon("info"))
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
                     title = 'Bulk Financial Download - All Filings (Selected Company)',
                     closable = FALSE,
                     width = 12,
                     status = "info",
                     solidHeader = FALSE,
                     collapsible = TRUE,
                     enable_dropdown = FALSE,
                     h6('Only one download allowed per session.  Simply refresh the app if another is desired.'),
                     fluidRow(
                       column(width = 3,
                              div(class="span6", align="center", #`data-display` = TRUE,
                                  div(tags$script(src = "https://www.paypalobjects.com/api/checkout.js "),
                                      #tags$script('<a id="Calculate">'),
                                      tags$a(id = 'Calculate',
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
                                    //window.alert('Thank you for your purchase!');
                                    var btn = document.createElement('BUTTON');   // Create a <button> element
                                    btn.innerHTML = 'Collect Data';                   // Insert text
                                    btn.className = 'btn btn-default shiny-download-link shiny-bound-output';
                                    btn.id = 'downloadData';
                                    btn.target = '_blank';
                                    document.getElementById('calculate').appendChild(btn);               // Append <button> to <body>
                                    });
                                    }
                                    }, '#calculate')")),
                                      
                                      tags$div(id = "calculate")))
                     ),
                      column(
                        width =3,
                      hidden(div(
                        id='actions',
                      downloadButton("downloadData1", "Download")
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
          tabName = 'graphs',
          fluidRow(
            column(width = 6,
                   boxPlus(
                     title = "Industry Market Cap", 
                     closable = FALSE, 
                     width = 12,
                     status = "info", 
                     solidHeader = FALSE, 
                     collapsible = TRUE,
                     enable_dropdown = FALSE,
                     h6('Exchanges: AMEX, NYSE, NYMEX'),
                     billboarder::billboarderOutput('mktCap')
                   )
            ),
            column(width =6,
                   boxPlus(
                     title = "Market Cap Comparables", 
                     closable = FALSE, 
                     width = 12,
                     status = "info", 
                     solidHeader = FALSE, 
                     collapsible = TRUE,
                     enable_dropdown = FALSE,
                     billboarderOutput('closest')
                   )
            )
          ),
          
          fluidRow(
            column(width = 12,
                   boxPlus(
                     title = "Current Assets/Liabilities", 
                     closable = FALSE, 
                     width = 12,
                     status = "info", 
                     solidHeader = FALSE, 
                     collapsible = TRUE,
                     enable_dropdown = FALSE,
                     billboarderOutput('currents')
                   )
            )

          ),
          
          fluidRow(
            column(width = 12,
                   boxPlus(
                     title = "Debt", 
                     closable = FALSE, 
                     width = 12,
                     status = "info", 
                     solidHeader = FALSE, 
                     collapsible = TRUE,
                     enable_dropdown = FALSE,
                     billboarderOutput('debt')
                   )
            )
            
          ),
          fluidRow(
            column(width = 12,
                   boxPlus(
                     title = "Annual Cash Flow Summary", 
                     closable = FALSE, 
                     width = 12,
                     status = "info", 
                     solidHeader = FALSE, 
                     collapsible = TRUE,
                     enable_dropdown = FALSE,
                     billboarderOutput('cf')
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
                src = "https://user-images.githubusercontent.com/5993637/39728796-935fde9e-520d-11e8-868f-85d7a132249c.png", 
                user_name = "Tidyverse", 
                description = ""
              ),
              userListItem(
                src = "shinyjs.png", 
                user_name = "shinyjs", 
                description = "Author: Dean Attali"
              ),
              userListItem(
                src = "shinydbplus.jpg", 
                user_name = "Shiny Dashboard+", 
                description = "Appsilon Data Science"
              ),
              userListItem(
                src = "quantmod.png", 
                user_name = "quantmod", 
                description = "Author: Jeffrey A. Ryan"
              ),
              userListItem(
                src = "dbThems.png", 
                user_name = "Dashboard Themes", 
                description = "Author: nik01010"
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
              ),
              userListItem(
                src = "tq.png", 
                user_name = "TidyQuant", 
                description = ""
              ),
              userListItem(
                src = "billboard.png", 
                user_name = "Billboarder", 
                description = "Author: Fanny Meyer"
              )
            )
          )
        )
      )
    ),
    rightsidebar = rightSidebar(),
    footer = dashboardFooter(
      left_text = "XBRL-Data",
      right_text = "2020"
    ),
    title = "XBRL-Data"
  )
  
  server = function(input, output, session) { 
    values <- reactiveValues()
    #shinyjs::show('actions')
    stockInfo <- reactive(
      (stockList %>% filter(company%in% input$ticker))
    )
    
    comps <- reactive(stockListX %>% 
                        filter(sector %in% stockInfo()$sector) %>%
                        filter(industry %in% stockInfo()$industry))
    

    
     observeEvent(input$ticker, {
       
       shinyjs::hide('downloadData')
       shinyjs::hide('actions')
       
    #   shinyjs::show('Calculate')
     })
     
     observeEvent(input$downloadData1, {
       shinyjs::hide('downloadData')
       
     })
    
    observe({
      if(is.null(input$ticker)||input$ticker == ''){
        values$stock <- NULL
      } else {
        stock <- NULL
        rm(stock)
        #print(head(comps()))
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
    
    tables <- reactive({
      txt1 <- glue::glue('./data/{stockInfo()$symbol}.rds')
      readRDS(txt1) %>% 
        #filter(ticker %in% stockInfo()$symbol) %>%
        mutate(Period = replace(Period, is.na(Period), 0))
      })
    
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
                          filter(Period %in% input$duration) %>%
                          mutate(Label = replace(Label, !is.na(arcrole), paste0('<b>',Label[!is.na(arcrole)],'</b>'))))
    

  
  output$financials <- DT::renderDataTable({
    if(is.null(input$duration)||input$duration == ''||
       is.null(input$period)||input$period == ''||
       is.null(input$table)||input$table == ''||
       is.null(input$type)||input$type == ''||
       is.null(input$endDate)||input$endDate == ''||
       is.null(tables1())||nrow(tables1()) == 0){
      NULL
    } else {
      DT::datatable(tables1() %>% arrange(Order) %>% select(Order, Label, Value = fact, Units) %>%
                      mutate(Order = seq(1, n(), 1)), rownames = FALSE, escape=FALSE,
                    extensions = c('Scroller'), 
                    options = list(
                      dom = 'Bfrtip',
                      scrollX = TRUE,
                      scrollY = TRUE,
                      deferRender = TRUE,
                      paging = FALSE,
                      searching = FALSE
                    ))
    }
    
  })
  
  
  output$downloadData1 <- downloadHandler(
    filename = function() {
      paste(stockInfo()$symbol, "financials.csv", sep = "")
    },
    content = function(file) {
      write.csv(tables(), file, row.names = FALSE)
    }
  )
  
  onclick('downloadData', shinyjs::show('actions'))
  onclick('calculate', shinyjs::hide('downloadData'))
  onclick('calculate', reset('downloadData'))
  onclick('downloadData1', shinyjs::hide('calculate'))
  
  output$mktCap <- billboarder::renderBillboarder({
    df <- comps() %>% select(company, market.cap) %>% 
      mutate(end = str_sub(market.cap, -1, -1),
             market.cap = as.numeric(str_sub(market.cap, 2, -2))) %>%
      mutate(market.cap = replace(market.cap, end == 'B', market.cap[end == 'B']*1000000000),
             market.cap = replace(market.cap, end == 'M', market.cap[end == 'M']*1000000)) %>%
      subset(select = -c(end)) %>% arrange(desc(market.cap))
    
    billboarder() %>%
      bb_piechart(data = df, bbaes(company, market.cap))%>%
      bb_color(palette = cols) %>%
  bb_labs(title = paste0(comps()$industry[1], ' Market Cap: $', round(sum(df$market.cap, na.rm=TRUE)/1000000000,2), ' Bn'),
          caption = "Data source: TidyQuant/State Street Global Advisors")%>%
      bb_legend(show = FALSE)
  }
    
  )
  
  output$closest <- renderBillboarder({
    df <- comps() %>% select(symbol, market.cap) %>%
      mutate(end = str_sub(market.cap, -1, -1),
             market.cap = as.numeric(str_sub(market.cap, 2, -2))) %>%
      mutate(market.cap = replace(market.cap, end == 'B', market.cap[end == 'B']*1000000000),
             market.cap = replace(market.cap, end == 'M', market.cap[end == 'M']*1000000)) %>%
      subset(select = -c(end)) %>% arrange(desc(market.cap))
    #print(head(df))
    count1 <- which(df$symbol == stockInfo()$symbol)
    if(count1 <= 3){
      df <- head(df, 7)
    } else if((nrow(df)-count1) <= 2){
      df <- tail(df, 7)
    } else {
      df <- df[(count1-3):(count1+3),]
    }
    df <- df %>% mutate(market.cap = round(market.cap/1000000, 2))
    txt1 <- stockInfo()$symbol

    # tuichart("column") %>% 
    #   add_data(df, aes(x = symbol, y = market.cap)) %>% 
    #   #tui_chart(title = "Comparable Companies by Market Cap") %>% 
    #   tui_yAxis(title = "US$Millions") %>% 
    #   tui_xAxis(title = "Data source: TidyQuant/State Street Global Advisors") %>% 
    #   tui_legend(visible = FALSE) %>% 
    #   tui_series(showLabel = TRUE)%>%
    #   tui_yAxis(
    #     max = max(df$market.cap*1.1) # y max value
    #   )
    
    billboarder() %>%
      bb_barchart(data = df, bbaes(x = symbol, y = market.cap)) %>%
      bb_y_grid(show = TRUE) %>%
      bb_y_axis(max = max(df$market.cap)*1.1,
                tick = list(format = suffix("$Ms")),
                label = list(text = "Market Cap, US$Millions", position = "outer-top")) %>% 
      bb_legend(show = FALSE) %>% 
      bb_labs(caption = "Data source: TidyQuant/State Street Global Advisors")
  }

  )
  
  output$currents <- renderBillboarder({
    cR <- tables() %>% filter(grepl('us-gaap_AssetsCurrent', Element)|grepl('us-gaap_LiabilitiesCurrent', Element)) %>%
      filter(!duplicated(paste0(fact, endDate, Element))) %>% 
      mutate(quarter = as.numeric(substr(PERIOD, 2, 2))*3, year = as.numeric(substr(PERIOD, 3, 7)) ) %>%
      mutate(date = as.POSIXct(paste0(quarter, '/01/', year), format = '%m/%d/%Y')) %>%
      group_by(Element, endDate) %>% filter(date == min(date)) %>%ungroup() %>%
      group_by(Element, endDate) %>% summarise(fact = mean(fact)) %>% arrange(Element, endDate) %>%
      mutate(fact = round(fact/1000000, 0)) %>% mutate(endDate = as.POSIXct(endDate, format = '%Y-%m-%d')) %>%
      mutate(endDate = as.Date(endDate))
    
    billboarder(data = cR) %>%
      bb_barchart(
        mapping = bbaes(x = endDate, y = fact, group = Element)
      ) %>%
      bb_color(palette = cols) %>%
      bb_y_grid(show = TRUE) %>%
      bb_y_axis(
        tick = list(format = suffix("$Ms")),
        label = list(text = "Current Assets/Libs in $Millions", position = "outer-top")
      ) %>%
      bb_legend(show = FALSE) %>%
      bb_labs(
        title = "",
        caption = "Data source: SEC XBRL"
      )%>%
      bb_x_axis(type = 'timeseries', label = list(rotate = 90), tick = list(format = "%Y-%m", fit = FALSE))
  
    
  })
  
  output$debt <- renderBillboarder({
    
    
      cR <- tables() %>% filter(Type == 'Statement') %>%
        filter(grepl('us-gaap_LongTermDebtCurrent', Element)|
                 grepl('us-gaap_LongTermDebtNoncurrent', Element)|
                 Element == 'us-gaap_LongTermDebt'|
                 Element == 'us-gaap_DebtCurrent'|
                 Element == 'us-gaap-DebtNoncurrent'|
                 Element == 'us-gaap_LongTermDebtAndCapitalLeaseObligations') %>%
        filter(!duplicated(paste0(fact, endDate, Element))) %>% 
        mutate(quarter = as.numeric(substr(PERIOD, 2, 2))*3, year = as.numeric(substr(PERIOD, 3, 7)) ) %>%
        mutate(date = as.POSIXct(paste0(quarter, '/01/', year), format = '%m/%d/%Y')) %>%
        group_by(Element, endDate) %>% filter(date == min(date)) %>%ungroup() %>%
        group_by(Element, endDate) %>% summarise(fact = mean(fact)) %>% arrange(Element, endDate) %>%
        mutate(fact = round(fact/1000000, 0)) %>% mutate(endDate = as.POSIXct(endDate, format = '%Y-%m-%d')) %>%
        mutate(endDate = as.Date(endDate))
      
      billboarder(data = cR) %>%
        bb_barchart(
          mapping = bbaes(x = endDate, y = fact, group = Element), stacked = FALSE
        ) %>%
        bb_color(palette = cols) %>%
        bb_y_grid(show = TRUE) %>%
        bb_y_axis(
          tick = list(format = suffix("$Ms")),
          label = list(text = "Long Term Debt in $Millions", position = "outer-top")
        ) %>%
        bb_legend(show = TRUE) %>%
        bb_labs(
          title = "",
          caption = "Data source: SEC XBRL"
        )%>%
        bb_x_axis(type = 'timeseries', label = list(rotate = 90), tick = list(format = "%Y-%m", fit = FALSE))
   
    # billboarder(data = cR) %>%
    #   bb_barchart(
    #     mapping = bbaes(x = endDate, y = fact, group = Element), stacked = TRUE
    #   ) %>%
    #   bb_color(palette = cols) %>%
    #   bb_y_grid(show = TRUE) %>%
    #   bb_y_axis(
    #     tick = list(format = suffix("$Ms")),
    #     label = list(text = "Long Term Debt in $Millions", position = "outer-top")
    #   ) %>%
    #   bb_legend(show = FALSE) %>%
    #   bb_labs(
    #     title = "",
    #     caption = "Data source: SEC XBRL"
    #   )%>%
    #   bb_x_axis(type = 'timeseries', label = list(rotate = 90), tick = list(format = "%Y-%m", fit = FALSE))
    # 
    
  })
  
  output$cf <- renderBillboarder({
    df1 <- tables() %>% filter(Type == 'Statement') %>% filter(!is.na(arcrole)) %>%
      filter(Period == 12) %>%
      filter(grepl('us-gaap_NetCashProvidedByUsedInOperatingActivities', Element)|
               grepl('us-gaap_NetCashProvidedByUsedInInvestingActivities', Element)|
               grepl('us-gaap_NetCashProvidedByUsedInFinancingActivities', Element)) %>%
      mutate(Element = replace(Element, grepl('Operating', Element), 'Operating Activities'),
             Element = replace(Element, grepl('Investing', Element), 'Investing Activities'),
             Element = replace(Element, grepl('Financing', Element), 'Financing Activities')) %>%
      filter(!duplicated(paste0(fact, endDate, Element))) %>% 
      mutate(quarter = as.numeric(substr(PERIOD, 2, 2))*3, year = as.numeric(substr(PERIOD, 3, 7)) ) %>%
      mutate(date = as.POSIXct(paste0(quarter, '/01/', year), format = '%m/%d/%Y')) %>%
      group_by(Element, endDate) %>% filter(date == min(date)) %>%ungroup() %>%
      group_by(Element, endDate) %>% summarise(fact = mean(fact)) %>% arrange(Element, endDate) %>%
      mutate(fact = round(fact/1000000, 0)) %>% filter(!duplicated(paste0(endDate, fact))) %>% arrange((Element), endDate) %>%
      group_by(endDate) %>% mutate(fact1 = cumsum(fact)) %>% ungroup()
    
    billboarder() %>% 
      bb_barchart(
        data = df1, 
        type = "area", 
        mapping = bbaes(x = endDate, y = fact, group = Element), stacked = FALSE
      ) %>% 
      bb_legend() %>% 
      bb_color(palette = cols) %>% 
      bb_y_axis(padding = 0) %>% 
      bb_labs(title = "",
              y = "Cash Flow, In US$Millions",
              caption = "Data source: SEC XBRL")%>%
      bb_x_axis(type = 'timeseries', label = list(rotate = 90), tick = list(format = "%Y-%m", fit = FALSE))
  })
  
  
  }



shinyApp(ui, server)