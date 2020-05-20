library(shiny)
library(gentelellaShiny)
library(shinyWidgets)
#library(noncensus)
library(openintro)
library(tidyverse)
#library(rdrop2)
library(dplyr)
library(shinyjs)
library(shinyBS)

library(DT)

options(shiny.maxRequestSize=30*1024^2)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv <- uniqv[!is.na(uniqv)]
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

saveData <- function(data) {
 saveRDS(data, 'responses/basedata.rds')
}

saveData1 <- function(data) {
 saveRDS(data, 'responses/basepdf.rds')
}

loadData <- function() {
  data <- readRDS('responses/basedata.rds')
  data
}

loadData1 <- function() {
 data <- readRDS('responses/basepdf.rds')
  data
}

countyList <- readRDS('data/countyList.rds')
#print(head(countyList))
secData <- readRDS('data/xbrlData.rds')

options(shiny.jquery.version=1)

shinyApp(
  ui = gentelellaPageCustom(
    title = "AFE Leaks",
    navbar = gentelellaNavbar(
    
    ),
    sidebar = gentelellaSidebar(
      site_title = shiny::HTML(paste(shiny::icon("gas-pump"),
                                     "AFE Leaks")),
      #uiOutput("profile"),
      sidebarDate(),
      sidebarMenu(
        sidebarItem(
          "AFE Upload",
          tabName = "tab1", 
          icon = tags$i(class = "fas fa-share"), 
          badgeName = "new",
          badgeStatus = "danger"
        ),
        sidebarItem(
          "AFE Viewer",
          tabName = "tab2", 
          icon = tags$i(class = "fas fa-archive")
        ),
        sidebarItem(
          "SEC Filings",
          tabName = "sec", 
          icon = tags$i(class = "fas fa-dollar-sign")
        )
      )
    ),
    body = gentelellaBody(
      useShinyjs(),
      tabItems(
        tabItem(
          tabName = "tab1",
          fluidRow(
            column(
              width = 4,
              align = "center",
              fileInput('file_input', 'Upload PDF', accept = c('.pdf')),
              selectInput('state', 'State', choices = state2abbr(state.name)),
              selectInput('county', 'County', choices = unique(countyList$county_name)),
              textInput('operator', "Operator Name"),
              textInput('API', label = "API Number"),
              textOutput('apiDouble'),
              textInput('well', label = "Well Name"),
              dateInput('apiDate', label ='AFE Date'),
              numericInput("preSpud", "Pre-Spud Capex, $", value = 0),
              numericInput("drill", "Drill Cost, $", value = 0),
              numericInput('complete', "Completion Cost, $", value = 0),
              numericInput('facilities', 'Facilities Cost, $', value = 0),
              numericInput('actual', 'Actual D&C+E, $', value = 0),
              numericInput('lateral', 'Lateral Length, ft', value = 0),
              bsButton('upload', 'Upload to Database', icon = icon('table'),
                       size = 'small', style = 'primary')
            ),
            column(
              width = 8,
              align = "center",
              uiOutput("pdfview")
            )
          ),
          fluidRow(
            h6('Disclaimer:  All data is crowdsourced.  We do not correct the data.
               We do not provide investment advice and any 
               investment decisions based on this data is purely at the risk of the individual
               or business using it.')
          )
        ),
        tabItem(
          tabName = "tab2",
          fluidRow(
            column(
              4,
              selectInput('state1', 'State', choices = state2abbr(state.name)),
              selectInput('county1', 'County', choices = unique(countyList$county_name)),
              selectInput('operator1', "Operator Name", choices = ''),
              selectInput('well1', label = "Well Name", choices = ''),
              selectInput('API1', label = "API Number", choices = ''),
              textOutput('apiDate1'),
              textOutput('preSpud1'),
              textOutput('drill1'),
              textOutput('complete1'),
              textOutput('facilities1'),
              textOutput('total1'),
              textOutput('actual1'),
              textOutput('lateral1')
            ),
            column(
              width = 8,
              align = "center",
              uiOutput("pdfview2")
            )
          ),
          fluidRow(
            h6('Disclaimer:  All data is crowdsourced.  We do not correct the data.
               We do not provide investment advice and any 
               investment decisions based on this data is purely at the risk of the individual
               or business using it.')
          )
        ),
        tabItem(
          tabName = 'sec',
          selectInput('ticker', 'Stock Ticker', choices = names(secData)),
          selectizeInput('period', 'Filing Period', choices = '', multiple = TRUE),
          selectizeInput('table', 'Table', choices = '', multiple = TRUE),
          
          selectizeInput('months', 'Time Period', choices = '', multiple = FALSE),
          DT::dataTableOutput('secFiling')
        )
      )
    ),
    footer = gentelellaFooter(
      leftText = 'Energy FinTwit',
      rightText = '2020'
    )
  ),
  server = function(input, output, session) {
    
    values <- reactiveValues()
    
    afeData <- loadData()
    values$afeData <- afeData
    #print(head(afeData))
    afePDF <- loadData1()
    values$afePDF <- loadData1()
    afePDF1 <- reactive(
      values$afePDF[[input$API1]]
    )
    
    #print(head(afePDF))
    output$apiDouble <- renderText({
      if(input$API %in% values$afeData$API){
        shinyjs::disable('upload')
        'Well is already in database'
        
      } 
    })
    
    observe({
     
      if(is.null(input$state)|is.null(input$county)|
         is.null(input$operator)|is.null(input$well)|
         is.null(input$API)|is.null(input$file_input)|
         is.null(input$apiDate)|input$state == ''|
         input$county == ''|input$operator == ''|
         input$well == ''|input$API == ''){
        shinyjs::disable('upload')
      } else {
        shinyjs::enable('upload')
      }
    })
    
    observeEvent(values$afeData, {
      updateSelectInput(session, 'state1', 'State', unique(values$afeData$State))
      updateSelectInput(session, 'county1', 'County', unique(values$afeData$County))
      updateSelectInput(session, 'operator1', 'Operator', unique(values$afeData$Operator))
      updateSelectInput(session, 'well1', 'Well', unique(values$afeData$Well))
      updateSelectInput(session, 'API1', 'API Number', unique(values$afeData$API))
    })
    
   
    observeEvent(input$state1, {
      #req(input$state)
      countyList1 <- countyList %>% filter(county_name %in% values$afeData$County) %>%
        filter(state %in% input$state1)
      #print(head(countyList1))
      updateSelectInput(session, 'county1', label = 'County', choices = unique(countyList1$county_name))
    })
    # 
    observeEvent(input$county1, {
      afeData1 <- values$afeData %>% filter(State %in% input$state1) %>%
        filter(County %in% input$county1)

      updateSelectInput(session, 'operator1', label = 'Operator', unique(afeData1$Operator))
      #updateSelectInput(session, 'well1', unique(afeData1$Well))
      #updateSelectInput(session, 'AFE1', unique(afeData1$API))

    })
    # 
    observeEvent(input$operator1, {
      afeData1 <- values$afeData %>% filter(State %in% input$state1) %>%
        filter(County %in% input$county1) %>% filter(Operator %in% input$operator1)

      #updateSelectInput(session, 'operator1', unique(afeData1$Operator))
      updateSelectInput(session, 'well1', 'Well', unique(afeData1$Well))
      #updateSelectInput(session, 'AFE1', unique(afeData1$API))

    })
    # 
    observeEvent(input$well1, {
      afeData1 <- values$afeData %>% filter(State %in% input$state1) %>%
        filter(County %in% input$county1) %>% filter(Operator %in% input$operator1) %>%
        filter(Well %in% input$well1)

      #updateSelectInput(session, 'operator1', unique(afeData1$Operator))
      #updateSelectInput(session, 'well1', unique(afeData1$Well))
      updateSelectInput(session, 'API1', 'API Number', unique(afeData1$API))

    })
    # 
    
    output$apiDate1 <- renderText({
      afeData1 <- values$afeData %>% filter(API %in% input$API1)
      paste0('AFE Date: ', afeData1$Date)
    })
    
    
    
    output$preSpud1 <- renderText({
      afeData1 <- values$afeData %>% filter(API %in% input$API1)
      paste0('Pre-Spud, $: ', afeData1$PreSpud)
    })

    output$drill1 <- renderText({
      afeData1 <- values$afeData %>% filter(API %in% input$API1)
      paste0('Drill, $: ', afeData1$Drill)
    })
    output$complete1 <- renderText({
      afeData1 <- values$afeData %>% filter(API %in% input$API1)
      paste0('Complete, $: ', afeData1$Complete)
    })

    output$facilities1 <- renderText({
      afeData1 <- values$afeData %>% filter(API %in% input$API1)
      paste0('Facilities, $: ', afeData1$Facilities)
    })
    output$total1 <- renderText({
      afeData1 <- values$afeData %>% filter(API %in% input$API1)
      paste0('Total Capex, $: ', afeData1$Total)
    })
    
    output$actual1 <- renderText({
      afeData1 <- values$afeData %>% filter(API %in% input$API1)
      paste0('Actual D&C+E, $: ', afeData1$Actual)
    })
    
    output$lateral1 <- renderText({
      afeData1 <- values$afeData %>% filter(API %in% input$API1)
      paste0('Lateral Length, ft: ', afeData1$Lateral)
    })
    
    observeEvent(input$upload, {
      shinyjs::disable('upload')
      updateButton(session, 'upload', label = 'Uploading....')
      df1 <- data.frame(State = input$state,
                        County = input$county,
                        Operator = input$operator,
                        API = input$API,
                        Well = input$well,
                        PreSpud = input$preSpud,
                        Complete = input$complete,
                        Drill = input$drill,
                        Facilities = input$facilities,
                        Total = input$preSpud + input$complete + input$drill + input$facilities,
                        Date = input$apiDate,
                        Actual = input$actual,
                        Lateral = input$lateral)
      
      values$afeData <- rbind(values$afeData, df1)
      saveRDS(values$afeData, 'responses/basedata.rds')

      afePDF1 <- list(values$test_file)
      names(afePDF1) <- input$API
      afeList <- append(values$afePDF, afePDF1)
      values$afePDF <- afeList
      saveRDS(values$afePDF, 'responses/basepdf.rds')
      
      #saveData(afeList)
      shinyjs::enable('upload')
      updateButton(session, 'upload', label = 'Upload to Database')
    })
    
    observe({
      #req(input$state)
      countyList1 <- countyList %>% filter(state %in% input$state)
      #print(head(countyList1))
      updateSelectInput(session, 'county', label = 'County', choices = unique(countyList1$county_name))
    })
    
    observe({
      req(input$file_input)
      test_file <- readBin(con=input$file_input$datapath,what = 'raw',n=input$file_input$size)
      values$test_file <- test_file
      writeBin(test_file,'www/myreport.pdf')
    })
    output$pdfview <- renderUI({
      if(is.null(input$file_input)){
        NULL
      } else {
      tags$iframe(style="height:600px; width:100%", src="myreport.pdf")
      }
    })
    
    observe({
      req(input$API1)
      #print(head(afePDF))
      test_file <- afePDF1() %>%
        writeBin('www/myreport1.pdf')
    })
    output$pdfview2 <- renderUI({
      if(is.null(input$API1)){
        NULL
      } else {
        tags$iframe(style="height:600px; width:100%", src="myreport1.pdf")
      }
    })
    
    secData1 <- reactive(secData[[input$ticker]] %>% mutate(Table = toupper(Table)) %>% mutate(Period = replace(Period, is.na(Period), 0)))
    
    #secDataX <- reactive(secData1() %>% filter(PERIOD %in% input$period) %>% filter(Table %in% input$table))
    
    observe(updateSelectizeInput(session, 'period', choices = sort(unique(secData1()$PERIOD))))
    
    observe(updateSelectizeInput(session, 'table', choices = sort(unique((secData1() %>% filter(PERIOD %in% input$period))$Table))))
    
    observe({
      #print(head(secDataX()))
      updateSelectizeInput(session, 'months', choices = as.character(sort(unique((secData1() %>% filter(Table %in% input$table) %>% filter(PERIOD %in% input$period))$Period))))
      
      })
    
    secData2 <- reactive(secData1() %>% filter(Table %in% input$table) %>% filter(PERIOD %in% input$period) %>% filter(as.character(Period) %in% input$months))
    
    output$secFiling <- DT::renderDataTable({
      
      #df1 <- secData2() %>% group_by(Element) %>% mutate(Label = getmode(Label)) %>% ungroup() %>%
      #  mutate(count = 1, count = replace(count, lag(subPlay, n=1L) == subPlay, 0)) %>% 
      #  mutate(count = cumsum(count)) %>% mutate(count1 = 0, count1 = replace(count1, lag(subPlay, n=1L)==subPlay, 0.1)) %>%
      #  group_by(count) %>% mutate(count1 = cumsum(count1)) %>% ungroup() %>% mutate(count = count+count1) %>% mutate(Order = count) 
      
      df1 <- secData2() %>% group_by(Element) %>% mutate(Label = getmode(Label)) %>% ungroup() %>%
        group_by(endDate, PERIOD) %>% mutate(count = 1, count = replace(count, lag(MainElement, n=1L) == MainElement, 0)) %>% 
        ungroup() %>%  group_by(endDate, PERIOD) %>% 
        mutate(count = cumsum(count), count1 = 0, count1 = replace(count1, lag(MainElement, n=1L)==MainElement, 0.1)) %>%
        ungroup() %>% group_by(count, endDate, PERIOD) %>% mutate(count1 = cumsum(count1)) %>% ungroup() %>% mutate(count = count+count1) %>% mutate(Order = count) %>%
        group_by(Element) %>% mutate(Order = mean(Order)) %>%
        subset(select = -c(Table, Period, PERIOD, ticker, count, count1)) %>% distinct() %>%
        group_by(Category, MainElement, Element, Label, Order, Units, endDate) %>% summarise(fact = mean(fact)) %>% ungroup() %>%
        distinct() %>%
        spread(endDate, fact)  %>% arrange(Order) %>% subset(select = -c(Order))
      
      DT::datatable(df1, rownames = FALSE,
                    extensions = c('Buttons', 'Scroller'),
                    options = list(
                      dom = 'Bfrtip',
                      scrollX = TRUE,
                      scrollY = FALSE,
                      deferRender = TRUE,
                      paging = FALSE,
                      searching = FALSE,
                      buttons = c(I('colvis'),'copy', 'csv', 'excel', 'pdf', 'print'))
                    ,
                    caption = htmltools::tags$caption(
                      style = 'caption-side: bottom; text-align: center;',
                      'Table: ', htmltools::em('SEC Filings')),
                    class = 'cell-border stripe')
    }
    )


  }
)