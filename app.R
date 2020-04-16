library(shiny)
library(shinyjs)
library(shinyWidgets)
library(argonR)
library(argonDash)
library(shinyBS)
library(highcharter)
library(rvest)
library(httr)
library(lubridate)
library(zoo)
library(tidyverse)
library(dplyr)
library(aRpsDCA)
library(scales)
library(DT)
library(jsonlite)
library(caret)
library(kernlab)
library(tableHTML)
library(akima)
library(maptools)
library(maps)
library(tools)
library(geosphere)
library(tigris)
library(openintro)
library(RColorBrewer)
library(rgdal)
library(leaflet)
library(leaflet.extras)
library(purrr)

options(shiny.maxRequestSize=30*1024^2)
options(stringsAsFactors = FALSE)
options(scipen = 999)

data1 <- readRDS('./data/operatorData.rds')
data1$qiOil[data1$oilEUR == 0] <- 0.001
data1$qiGas[data1$gasEUR == 0] <- 0.001
data2 <- readRDS('./data/pdp.rds')

opList <- readRDS('./data/opList.rds') #%>% filter(operator != 'Endeavor Energy Resources')
wellData <- readRDS('./data/assetSummary.rds')
costData <- readRDS('./data/costData.rds')
countyData <- readRDS('./data/countyData.rds')
prodData <- readRDS('./data/prodData.rds')
propUplift <- readRDS('./data/propUplift.rds')
acreageGEM <- readRDS('./data/acreageGEM.rds')


perfRisk <- data.frame(perf = c(0, 2500, 5000, 7500, 10000, 12500, 15000), risk = c(0, 0.55, 1, 1.45, 1.85, 2.2, 2.5))

perfUplift <- lm(perfRisk$risk ~ perfRisk$perf + I(perfRisk$perf**2))

prodData$oil[is.na(prodData$oil)] <- 0
prodData$gas[is.na(prodData$gas)] <- 0

op1 <- c("Antero Resources", "Apache", "Approach Resources", "Baytex Energy",
         "Bonanza Creek Energy", "BP", "Cabot Oil & Gas", "Callon Petroleum",
         "Centennial Resource", "Chaparral Energy", "Chesapeake Energy",
         "Chevron", "Cimarex Energy", "CNX Resources", "Comstock Resources",
         "Concho Resources", "ConocoPhillips", "Continental Resources",
         "Crescent Point", "Devon Energy", "Diamondback Energy Inc", "Eclipse Resources", 
         "Enerplus", "EOG Resources", "EP Energy", "EQT Corporation",
         "Equinor", "EXCO Resources", "Extraction Oil & Gas", "ExxonMobil",
         "Gulfport", "Halcon Resources", "Hess Corporation", "HighPoint Resources",
         "Jagged Peak Energy", "Laredo Petroleum", "Magnolia Oil & Gas",
         "Marathon Oil", "Matador Resources", "Murphy Oil", "Noble Energy",
         "Oasis Petroleum", "Occidental Petroleum", "Ovintiv", "Parsley Energy",
         "PDC Energy", "Penn Virginia Corporation", "Pioneer Natural Resources",
         "QEP Resources", "Range Resources Corp", "Repsol Oil & Gas Canada",
         "Sanchez Energy Corporation", "Seneca Resources", "Shell", "SilverBow Resources",
         "SM Energy", "Southwestern Energy", "Total", "Whiting Petroleum Corporation",
         "WPX Energy", "California Resources")
#op1 <- c('Hess Corporation')

IRRcalc <- function(cf, months){
  if(sum(cf) > 0){
    IRR1 <- 3
    loop <- 1
    while(sum(cf/((1+IRR1)^(months/12)))
          < 0){
      IRR1 <- IRR1 - 0.01
      loop = loop + 1
    }
    
  }else {
    IRR1 <- 0
  }
  return(IRR1)
}

crude <- 'https://www.eia.gov/dnav/pet/hist/RWTCD.htm'

webpage <- read_html(crude)
tbls_ls <- webpage %>%
  html_nodes('table') %>%
  .[6] %>%
  html_table(fill = TRUE)
wti1 <- tbls_ls[[1]]
wti1 <- wti1 %>% filter(!is.na(Mon))
wti1$Year <- word(wti1$`Week Of`,1)
wti1$Month <- substr(word(wti1$`Week Of`,2),1,3)
wti1$DATE <- as.POSIXct(paste0(wti1$Month, '-01-', wti1$Year), format = '%b-%d-%Y')
wti1 <- wti1[,2:ncol(wti1)]
wti1 <- wti1 %>% gather(Day, WTI, -c(Year, Month, DATE))
wti1 <-wti1 %>% group_by(DATE) %>% summarise(WTI = mean(WTI, na.rm=TRUE)) %>% ungroup()

crude <-'https://www.eia.gov/dnav/ng/hist/rngwhhdm.htm'
webpage <- read_html(crude)
tbls_ls <- webpage %>%
  html_nodes('table') %>%
  .[5] %>%
  html_table(fill = TRUE)
hh1 <- tbls_ls[[1]]
hh1 <- hh1 %>% filter(!is.na(Jan))

hh1 <- hh1 %>% gather(DATE, HH, -c(Year))
hh1 <- hh1 %>% mutate(DATE = paste0(DATE,'/01/', Year))
hh1$DATE <- as.POSIXct(hh1$DATE, format = '%b/%d/%Y')
hh1 <- hh1 %>% arrange(DATE) %>% select(DATE, HH)
wti1 <- wti1 %>% filter(DATE >= min(hh1$DATE))

css <- HTML(
  "#allTable > .dataTables_wrapper.no-footer > .dataTables_scroll > .dataTables_scrollBody {
        transform:rotateX(180deg);
    }
    #allTable > .dataTables_wrapper.no-footer > .dataTables_scroll > .dataTables_scrollBody table{
        transform:rotateX(180deg);
    }
  #devTable > .dataTables_wrapper.no-footer > .dataTables_scroll > .dataTables_scrollBody {
        transform:rotateX(180deg);
    }
    #devTable > .dataTables_wrapper.no-footer > .dataTables_scroll > .dataTables_scrollBody table{
        transform:rotateX(180deg);
    }
  #pdpTable > .dataTables_wrapper.no-footer > .dataTables_scroll > .dataTables_scrollBody {
        transform:rotateX(180deg);
    }
    #pdpTable > .dataTables_wrapper.no-footer > .dataTables_scroll > .dataTables_scrollBody table{
        transform:rotateX(180deg);
    }"
)


ui <- argonDashPage(
  header = argonDashHeader(
    gradient = FALSE,
    color = "secondary",
    separator = FALSE,
    separator_color = "default",
    argonRow(
      argonColumn(
        width = 3,
        pickerInput('operator', 'Operator', choices = op1, selected = 'Hess Corporation')
        ),
      argonColumn(
        width = 3,
        pickerInput('projectType', 'Project', choices = c('Woodmac', 'User')),
        bsButton('saveProject', 'Save Project', style = 'primary', size = 'small')
      ),
      argonColumn(
        width = 3,
        pickerInput('subPlayList', 'Active SubPlays', choices = ''),
        bsButton('remove', 'Remove SubPlay', style = 'danger', size = 'small')
        )
    )

  ),
  sidebar = argonDashSidebar(
    vertical = TRUE,
    skin = "light",
    background = "white",
    size = "md",
    side = "right",
    id = "my_sidebar",
    brand_url = "http://www.woodmac.com",
    brand_logo = "www.svg",
    useShinyjs(),
    textInput(
      inputId = 'wmacUser',
      label = 'Portal Login',
      'example@woodmac.com'
    ),
    passwordInput('wmacPass', 'Portal Password', 'password1'),
    bsButton('confirm', label = 'Login', style = 'primary', size = 'small'),
    hidden(div(
      id = 'playHide',
      argonSidebarHeader(title = "Gem Python"),
      
      argonSidebarMenu(
        argonSidebarItem(
          tabName = "price",
          icon = argonIcon(name = "money-coins", color = "primary"),
          "Pricing"
        ),
        argonSidebarItem(
          tabName = "dev",
          icon = argonIcon(name = "folder-17", color = "info"),
          "Inventory"
        ),
        argonSidebarItem(
          tabName = "pdp",
          icon = argonIcon(name = "sound-wave", color = "default"),
          "PDP Decline"
        ),
        argonSidebarItem(
          tabName = "schedule",
          icon = argonIcon(name = "curved-next", color = "green"),
          "Forecast"
        ),
        argonSidebarItem(
          tabName = "total",
          icon = argonIcon(name = "money-coins", color = "green"),
          "Summary CF"
        ),
        argonSidebarDivider(),
        argonSidebarHeader(title = "Lens Evaluator"),
        argonSidebarItem(
          tabName = "evaluator",
          icon = argonIcon(name = "folder-17", color = "default"),
          "Evaluator"
        ),
        argonSidebarItem(
          tabName = "map",
          icon = argonIcon(name = "map-big", color = "info"),
          "EUR Map"
        )
        
      )
    )
    )
    
  ),
  body = argonDashBody(
    useShinyjs(),
      argonTabItems(
        
        argonTabItem(
          tabName = 'price',
          argonRow(
            tags$style("[type = 'number'] {font-size:12px;height:14px;}"),
            tags$style(HTML(
              "label { font-size:90%; font-family:Arial; margin-bottom: 
              5px; }"
            )),
            argonColumn(
              width = 5,
              argonCard(
                title = 'Revenue Information',
                shadow = TRUE,
                border_level = 1,
                width = 12,
                argonRow(
                  argonColumn(
                    width = 2,
                    awesomeRadio('priceType', 'Price File', choices = c('Strip','Flat', 'Custom'), status = 'primary')
                  ),
                  argonColumn(
                    width = 5,
                    numericInput('wti1', 'Year 1 Oil Price, $/bbl', value = 35, min = 0),
                    numericInput('wti2', 'Year 2 Oil Price, $/bbl', value = 35, min = 0),
                    numericInput('wti3', 'Year 3 Oil Price, $/bbl', value = 35, min = 0),
                    numericInput('wti4', 'Year 4 Oil Price, $/bbl', value = 35, min = 0),
                    numericInput('wti5', 'Year 5 Oil Price, $/bbl', value = 35, min = 0),
                    numericInput('wti6', 'Year 6 Oil Price, $/bbl', value = 35, min = 0),
                    numericInput('wti7', 'Year 7 Oil Price, $/bbl', value = 35, min = 0),
                    numericInput('wti8', 'Year 8 Oil Price, $/bbl', value = 35, min = 0),
                    numericInput('wti9', 'Year 9 Oil Price, $/bbl', value = 35, min = 0),
                    numericInput('wti10', 'Year 10 Oil Price, $/bbl', value = 35, min = 0)
                    ),
                  argonColumn(
                    width = 5,
                    numericInput('hh1', 'Year 1 Gas Price, $/mcf', value = 2, min = 0),
                    numericInput('hh2', 'Year 2 Gas Price, $/mcf', value = 2, min = 0),
                    numericInput('hh3', 'Year 3 Gas Price, $/mcf', value = 2, min = 0),
                    numericInput('hh4', 'Year 4 Gas Price, $/mcf', value = 2, min = 0),
                    numericInput('hh5', 'Year 5 Gas Price, $/mcf', value = 2, min = 0),
                    numericInput('hh6', 'Year 6 Gas Price, $/mcf', value = 2, min = 0),
                    numericInput('hh7', 'Year 7 Gas Price, $/mcf', value = 2, min = 0),
                    numericInput('hh8', 'Year 8 Gas Price, $/mcf', value = 2, min = 0),
                    numericInput('hh9', 'Year 9 Gas Price, $/mcf', value = 2, min = 0),
                    numericInput('hh10', 'Year 10 Gas Price, $/mcf', value = 2, min = 0)
                  )
              )
              )
            ),
            argonColumn(
              width = 7,
              highchartOutput('prices')
            )
            
          )
        ),
        
        argonTabItem(
          tabName = 'dev',
          
          
          argonRow(
            
            argonColumn(
              width = 3,
              argonCard(
                title = 'Development Node',
                shadow = TRUE,
                border_level = 1,
                width = 12,
                #background_color = 'primary',
                
                textInput('subPlay', 'SubPlay Name', placeholder = 'SubPlay'),
                textInput('reservoir', 'Reservoir Name', placeholder = 'Reservoir')
              )
              ),
            argonColumn(
              width = 3,
                argonCard(
                  title = 'Inventory Information',
                  shadow = TRUE,
                  border_level = 1,
                  width = 12,
                  numericInput('wells', 'Net Remaining Wells',value = 100, min = 0),
                  numericInput('nri', 'Net Revenue Interest, %', value = 75, min = 0),
                  textOutput('acreageGEM')
                )
                
              ),
           

            argonColumn(
              width = 3,
              argonCard(
                title = 'Oil Type Curve Information',
                shadow = TRUE,
                border_level = 1,
                width = 12,
                #closable = TRUE,
                
                numericInput('oilEUR', 'Oil EUR/Well, mbbls', value = 100, min = 0, max = 10000),
                numericInput('curtailOil', 'Oil Curtailment, Months', value = 1, min = 0),
                numericInput('qiOil', 'Oil IP-30, bbl/d', value = 100, min = 0.001),
                numericInput('bOil', 'Oil B-Factor', value = 1, min = 0, max = 2),
                numericInput('DfOil', 'Oil Terminal Decline, %', min = 1, max = 30, value = 10)
                )
              ),
            argonColumn(
              width = 3,
              argonCard(
                title = 'Gas Type Curve Information',
                shadow = TRUE,
                border_level = 1,
                width = 12,
                numericInput('gasEUR', 'Wet Gas EUR/Well, mmcf', value = 1000, min = 0, max = 100000),
                numericInput('curtailGas', 'Gas Curtailment, Months', value = 1, min = 0),
                numericInput('qiGas', 'Gas IP-30, mcf/d', value = 1000, min = 0.001),
                numericInput('bGas', 'Gas B-Factor', value = 1, min = 0, max = 2),
                numericInput('DfGas', 'Gas Terminal Decline, %', min = 1, max = 30, value = 8),
                numericInput('shrink', 'Shrink (Percent Gas Sold), %',  value = 90, min = 0),
                textOutput('dryEUR'),
                br(),
                numericInput('nglYield', 'NGL Yield, (BBL/MMCF)', value = 90, min = 0),
                textOutput('nglEUR')
              )
            )
          ),
          argonRow(
            highchartOutput('tcPlot')
            ),
          argonRow(
            textOutput('npv10')
            ),
          argonRow(
            textOutput('irr')
          ),
          argonRow(
            argonColumn(
              width = 3,
              argonCard(
                title = 'Other Well Information',
                shadow = TRUE,
                border_level = 1,
                width = 12,
                numericInput('spudToProd', 'Spud to Production, months', value = 3, min = 1),
                numericInput('capex', 'Capex Per Well, $', value = 5000000, min = 0),
                numericInput('pna', 'Plugging & Abandonment, $', value = 40000, min = 0),
                numericInput('wellLife', 'Well Life, Years', value = 25, min = 10)
              ),
              
              
              bsButton("add", "Add To Development Program", style = 'primary', size = 'small')
            ),
            argonColumn(
              width = 3,
              argonCard(
                title = 'Differential Information',
                shadow = TRUE,
                border_level = 1,
                width = 12,
                #awesomeRadio('priceType', 'Price File', choices = c('Strip', 'Custom'), status = 'primary'),
                
                numericInput('oilDiff', 'Oil Differential, $/bbl', value = 1, min = 0),
                numericInput('gasDiff', 'Gas Differential, $/mcf', value = 0.3, min = 0),
                numericInput('btu', 'Gas BTU Factor, MMBTU/MCF', value = 1, min = 0),
                numericInput('nglDiff', 'NGL Differential, %WTI', value = 25, min = 0)
            )
            ),
            argonColumn(
              width = 3,
              argonCard(
                title = 'Expense Information',
                shadow = TRUE,
                border_level = 1,
                width = 12,
                #awesomeRadio('priceType', 'Price File', choices = c('Strip', 'Custom'), status = 'primary'),
                
               
                numericInput('fixed', 'Fixed Expense/Well, $/month', value = 10000, min = 0),
                numericInput('varExpGas', 'Variable Expense Gas, $/mcf', value = 0.3, min = 0),
                numericInput('varExpBOE', 'Variable Expense Liqiuds, $/boe', value = 6, min = 0)
               
              )
            ),
            argonColumn(
              width = 3,
              argonCard(
                title = 'Tax Information',
                shadow = TRUE,
                border_level = 1,
                width = 12,
                #awesomeRadio('priceType', 'Price File', choices = c('Strip', 'Custom'), status = 'primary'),
                
              
                numericInput('stxOil', 'Oil Severance Tax, % Revenue', value = 4.6, min = 0),
                numericInput('stxGas', 'Gas Severance Tax, % Revenue', value = 7.5, min = 0),
                numericInput('oilSTX', 'Oil Severance Per BBL', value = 0, min= 0),
                numericInput('gasSTX', 'Gas Severance Per MCF', value = 0, min = 0),
                numericInput('atx', 'Ad Valorem Tax, % Revenue', value = 2.5, min = 0)
              )
            )
            )
          
          
        ),
        
        argonTabItem(
          tabName = 'pdp',
          tags$head(tags$style(css)),
          #shinyWidgets::awesomeRadio('subPlayList', 'Active SubPlays', choices = '', status = 'primary'),
          argonRow(
            argonColumn(
              width = 12,
              argonCard(
                title = 'PDP Inputs',
                shadow = TRUE,
                border_level = 1,
                width = 12,
                argonRow(
                  argonColumn(
                    width = 3,
                    numericInput('pdpWells', label = 'PDP Wells', value = 0, min = 0)
                  ),
                  argonColumn(
                    width = 3,
                    numericInput('oilPDP', label = 'Last Year Oil, MBO/D', value = 0, min = 0)
                  ),
                  argonColumn(
                    width = 3,
                    numericInput('gasPDP', label = 'Last Year Gas, MMCF/D', value = 0, min = 0)
                  ),
                  argonColumn(
                    width = 3,
                    numericInput('nglPDP', label = 'Last Year NGL, MB/D', value = 0, min = 0)
                  )
                ),
                argonRow(
                  argonColumn(
                    width = 3,
                    br(),
                  ),
                  argonColumn(
                    width = 3,
                    numericInput('decl1oil', label = 'Year 1 Oil Decline (fraction)', value = 0, min = 0, max =1),
                    numericInput('decl2oil', label = 'Year 2 Oil Decline (fraction)', value = 0, min = 0, max =1),
                    numericInput('decl3oil', label = 'Year 3 Oil Decline (fraction)', value = 0, min = 0, max =1),
                    numericInput('decl4oil', label = 'Year 4 Oil Decline (fraction)', value = 0, min = 0, max =1),
                    numericInput('decl5oil', label = 'Year 5 Oil Decline (fraction)', value = 0, min = 0, max =1),
                    numericInput('decl6oil', label = 'Year 6 Oil Decline (fraction)', value = 0, min = 0, max =1),
                    numericInput('decl7oil', label = 'Year 7+ Oil Decline (fraction)', value = 0, min = 0, max =1)
                  ),
                  argonColumn(
                    width = 3,
                    numericInput('decl1gas', label = 'Year 1 Gas/NGL Decline (fraction)', value = 0, min = 0, max =1),
                    numericInput('decl2gas', label = 'Year 2 Gas/NGL Decline (fraction)', value = 0, min = 0, max =1),
                    numericInput('decl3gas', label = 'Year 3 Gas/NGL Decline (fraction)', value = 0, min = 0, max =1),
                    numericInput('decl4gas', label = 'Year 4 Gas/NGL Decline (fraction)', value = 0, min = 0, max =1),
                    numericInput('decl5gas', label = 'Year 5 Gas/NGL Decline (fraction)', value = 0, min = 0, max =1),
                    numericInput('decl6gas', label = 'Year 6 Gas/NGL Decline (fraction)', value = 0, min = 0, max =1),
                    numericInput('decl7gas', label = 'Year 7 Gas/NGL Decline (fraction)', value = 0, min = 0, max =1)
                  )
                )
                
              ),
              bsButton('addPDP', 'Save Change', style = 'primary', size = 'small')
            )
          ),
          argonRow(
            selectizeInput('selectedPDP', 'Graph Item', 
                           choices = c('Oil', 'Gas', 'NGL',
                                       'Sales Gas','Net Oil', 'Net Gas', 'Net NGL',
                                       'Expenses', 'Capex',  'Oil Revenue',
                                       'Gas Revenue', 'NGL Revenue', 'Revenue', 'Taxes', 'NOCF', 'FCF')),
            highchartOutput('pdpTotal')
            
          ),
          argonRow(
            numericInput('pdpDiscRate', 'Discount Rate, %', value = 10, min = 0),
            textOutput('pdpPV'),
            DT::dataTableOutput('pdpTable')
          )
        ),
        
        argonTabItem(
          tabName = 'schedule',
          tags$head(tags$style(css)),
          argonRow(
            argonColumn(
              width = 6,
              textOutput('subPlayIRR')
            )
            ),
          argonRow(
            argonColumn(
              width = 12,
              argonCard(
                title = 'Development Plan',
                shadow = TRUE,
                border_level = 1,
                width = 12,
                h5('Monthly Wells Drilled'),
                argonRow(
                  argonColumn(
                    width = 4,
                    numericInput('drill2020', '2020', value = 0, min = 0),
                    textOutput('remInv1'),
                    br(),
                    numericInput('drill2023', '2023', value = 0, min = 0),
                    textOutput('remInv4'),
                    br(),
                    numericInput('drill2026', '2026', value = 0, min = 0),
                    textOutput('remInv7'),
                    br(),
                    numericInput('drill2029', '2029', value = 0, min = 0),
                    textOutput('remInv10')
                              
                              ),
                  argonColumn(
                    width = 4,
                    numericInput('drill2021', '2021', value = 0, min = 0),
                    textOutput('remInv2'),
                    br(),
                    numericInput('drill2024', '2024', value = 0, min = 0),
                    textOutput('remInv5'),
                    br(),
                    numericInput('drill2027', '2027', value = 0, min = 0),
                    textOutput('remInv8'),
                    br(),
                    br(),
                    
                    bsButton('addFcst', 'Load Forecast', style = 'primary', size = 'small')
                    
                  ),
                  argonColumn(
                    width = 4,
                    numericInput('drill2022', '2022', value = 0, min = 0),
                    textOutput('remInv3'),
                    br(),
                    numericInput('drill2025', '2025', value = 0, min = 0),
                    textOutput('remInv6'),
                    br(),
                    numericInput('drill2028', '2028', value = 0, min = 0),
                    textOutput('remInv9')
                    
                  )
                  
                )
              )
              )
            ),
          
              argonRow(
                selectizeInput('selected', 'Graph Item', 
                               choices = c('Oil', 'Gas', 'NGL',
                                           'Sales Gas', 'Net Oil', 'Net Gas', 'Net NGL',
                                           'Expenses', 'Capex', 'Wells', 'Oil Revenue',
                                           'Gas Revenue', 'NGL Revenue', 'Revenue', 'Taxes', 'NOCF', 'FCF')),
                highchartOutput('devTotal')
              
              ),
              argonRow(
                numericInput('pudDiscRate', 'Discount Rate, %', value = 20, min = 0),
                textOutput('pudPV'),
                DT::dataTableOutput('devTable')
              )
            ),
        argonTabItem(
          tabName = 'total',
          tags$head(tags$style(css)),
          argonRow(
            selectizeInput('selectedAll', 'Graph Item', 
                           choices = c('Oil', 'Gas', 'NGL',
                                       'Sales Gas','Net Oil', 'Net Gas', 'Net NGL',
                                       'Expenses', 'Capex',  'Oil Revenue',
                                       'Gas Revenue', 'NGL Revenue', 'Revenue', 'Taxes', 'NOCF', 'FCF')),
            highchartOutput('allTotal')
            
          ),
          argonRow(
            numericInput('allDiscRate', 'Discount Rate, %', value = 10, min = 0),
            textOutput('allPV'),
            DT::dataTableOutput('allTable')
          )
        ),
        argonTabItem(
          tabName = 'evaluator',
          argonH1(display = 3, 'Remaining Inventory Estimator'),
          argonRow(
            
            
            argonColumn(
              width = 4,
              pickerInput('operator1', label = 'Operator', choices = sort(unique(opList$operator))),
              pickerInput('plays', 'Active Areas',choices =  sort(unique(opList$id)))
            ),
            argonColumn(
              width = 8,
              highchartOutput('acres_plot')
         
            )
          ),
          # argonRow(
          #   
          # ), 
          argonH1(display = 3, 'SubPlay Info'),
          argonRow(
            
            
            
            argonColumn(
              width = 6,
              radioButtons('subPlay1', label = 'Active Sub-Plays', choices = '')
            ),
            argonColumn(
              width = 6,
              highchartOutput('percentDev')
            )
          ),
          argonRow(
            highchartOutput('offsets_plot')
          ),
          argonRow(
            argonColumn(
              width = 12,
              argonCard(
                title = 'Sub-Play Assumptions',
                shadow = TRUE,
                border_level = 1,
                width = 12,
                awesomeRadio('econAbanS',
                             'Abandon at Economic Limit?',
                             choices = c('Yes', 'No'),
                             selected = 'Yes',
                             status = 'primary'),
                argonRow(
                  argonColumn(
                    width = 3,
                    numericInput('acres1', 'Acreage Estimate', value = 0),
                    numericInput('wellSpacing1', 'Wells Per Section', value = 0),
                    numericInput('pna1', 'P&A Per Well, $', value = 0),
                    numericInput('wiPDP1', 'PDP Working Interest, %', value = 100, min = 1, max = 100),
                    numericInput('nri1', 'Royalty (to 100% WI), %', value = 80, min = 1, max = 100)
                  ),
                  argonColumn(
                    width = 3,
                    numericInput('shrink1', 'Shrink (Fraction Retained)', value = 0),
                    numericInput('nglYield1', 'NGL Yield (BBL/MMCF)', value = 0),
                    numericInput('btu1', 'BTU Uplift', value = 0),
                    numericInput('gasDiff1', 'Gas Differential, $/MCF', value = 0),
                    numericInput('oilDiff1', 'Oil Differential, $/BBL', value = 0),
                    numericInput('nglDiff1', 'NGL Pricing, %WTI', value = 20)
                  ),
                  argonColumn(
                    width = 3,
                    numericInput('fixedCost1', 'Fixed Cost, $/mo', value = 0),
                    numericInput('varExpGas1', 'Variable Cost, $/mcf', value = 0),
                    numericInput('varExpBOE1', 'Variable Cost, $/boe', value = 0)
                  ),
                  argonColumn(
                    width = 3,
                    numericInput('stxOil1', 'Oil Severance, %', value = 4.6, min = 0),
                    numericInput('oilSTX1', 'Oil Severance, $/bbl', value = 0, min = 0),
                    numericInput('stxGas1', 'Gas Severance, %', value = 7.5, min = 0),
                    numericInput('gasSTX1', 'Gas Severance, $/mcf', value = 0, min = 0),
                    numericInput('atx1', 'Ad Valorem Tax, %', value = 2.5, min = 0)
                  )
                )
              )
              
              
            )
          ),
          
          argonRow(
            argonColumn(
              width = 12,
              argonCard(
                title = 'Well Design',
                shadow = TRUE,
                border_level = 1,
                width = 12,
                argonRow(
                  
                  
                  argonColumn(width = 4,
                              numericInput('perfSelect1', 'Average Lateral Length, ft', value = 7500)),
                  
                  argonColumn(width =4,
                              numericInput('ppfSelect1', 'Proppant Loading, lb/ft', value = 2000)),
                  
                  argonColumn(width =4,
                              numericInput('fpfSelect1', 'Fluid Loading, gal/ft', value = 2000))
                  
                )
              ),
              selectizeInput('wellFactor', '', choices = c('perf', 'ppf', 'fpf'), selected = 'perf'),
              highchartOutput('wellDesign'),
              br(),
              argonCard(
                title = 'Capex Calculator (Cost CoE)',
                shadow = TRUE,
                border_level = 1,
                width = 12,
                argonRow(
                  argonColumn(width = 3,
                              numericInput('capexStressor1', 'Capex Stress', value = 1, min = 0.1))
                ),
                argonRow(
                  
                  
                  argonColumn(width = 4,
                              numericInput('tvdSelect1', 'TVD', value = 7500, min = 1000, max = 20000),
                              numericInput('proppantPerStage1', 'Proppant Per Stage', value = 440000, min = 1),
                              numericInput('drillingFuelPerDay1', 'Drilling Fuel, $/Day', value = 4000, min = 1)),
                  argonColumn(width =4,
                              numericInput('drillSpeed1', 'Drill Speed, ft/day', value = 100, min = 1),
                              numericInput('padConstruction1', 'Pad Construction Per Well', value = 180000, min = 1),
                              numericInput('ancDrillPerDay1', 'Ancillary Drill Services, $/Day', value = 50000, min = 1)),
                  argonColumn(width =4,
                              numericInput('stagesPerDay1', 'Frac Stages per Day', value = 10, min = 1),
                              numericInput('drillingFluidsPerDay1', 'Drilling Fluids, $/Day', value = 4100, min = 1),
                              numericInput('chemsPerStage1', 'Chemical Cost, $/Stage', value = 6500, min = 1))
                ),
                
                argonRow(
                  
                  argonColumn(width = 4,
                              numericInput('pumpFuel1', 'Pumping Fuel, $/Stage', value = 5000, min = 1)),
                  argonColumn(width = 4,
                              numericInput('ancCompPerDay1', 'Ancillary Completion Services, $/Day', value = 50000, min = 1))
                ),
                h6('Sand Fractions: Should Add to 1'),
                argonRow(
                  
                  argonColumn(width = 3,
                              numericInput('brownSelect1', 'Brown Sand', value = 1, min = 0, max = 1)),
                  argonColumn(width = 3,
                              numericInput('ceramicSelect1', 'Ceramic', value = 0, min = 0, max = 1)),
                  argonColumn(width = 3,
                              numericInput('rcsSelect1', 'Resin-Coated', value = 0, min = 0, max= 1)),
                  argonColumn(width = 3,
                              numericInput('northernSelect1', 'Northern White', value = 0, min = 0, max = 1))
                )
              ),
              argonColumn(
                width = 12,
                center = TRUE,
                tableHTML_output('capexCalcs')
                ),
               
            ),
            argonColumn(
              width = 12,
              
              highchartOutput('costPerFt')
            )
            
          ),
          argonRow(
            argonColumn(
              width = 6,
              argonCard(
                title = 'Type Curve Design',
                shadow = TRUE,
                border_level = 1,
                width = 12,
                argonRow(
                  argonColumn(
                    width = 6,
                    pickerInput('operatorSelect',label = 'Select Operator(s)', choices = sort(unique(opList$operator)), 
                                multiple = TRUE),
                    pickerInput('selectYr', label = 'Select Year(s)', choices = '',  multiple = TRUE),
                    selectizeInput('productSelect1', 'Selected Product', choices = c('Oil', 'Gas'), multiple = FALSE),
                    numericInput('cutoff1', 'IRR Cutoff, %', value = 20, min = 0),
                    
                    numericInput('spudToProd1', 'Spud to First Production (Months)', value =3 , min = 1),
                    numericInput('prbRisking1', 'Probable Risking (0 to 1)', value = 0.75, min = 0, max = 1 ),
                    
                    numericInput('possRisking1', 'Possible Risking (0 to 1)', value = 0.5, min = 0, max = 1 )
                    
                  ),
                  
                  argonColumn(
                    width = 6,
                    
                    
                    numericInput('wellLifeS', 'Well Life (Years)', value = 25, min = 5, max = 50),
                    
                    numericInput('qiOilS', 'Oil IP, bbls/d', value = 1000, min = 0),
                    
                    numericInput('bOilS', 'Oil B-Factor', value = 1, min = 0, max = 2.5),
                    
                    numericInput('DiOilS', 'Di Oil', value = 0.9, min = 0.20000001, max = 0.999999),
                    
                    numericInput('DfOilS', 'Df Oil', value = 0.1, min = 0.01, max = 0.2),
                    
                    numericInput('curtailOilS', 'Oil Curtailment (months)', value = 0, min = 0),
                    
                    
                    numericInput('qiGasS', 'Gas IP, mcf/d',  value = 1000, min = 0),
                    
                    numericInput('bGasS', 'Gas B-Factor',  value = 1, min = 0, max = 2.5),
                    
                    numericInput('DiGasS', 'Di Gas', value = 0.9, min = 0.20000001, max = 0.999999),
                    
                    numericInput('DfGasS', 'Df Gas',  value = 0.08, min = 0.01, max = 0.2),
                    
                    numericInput('curtailGasS', 'Gas Curtailment (months)', value = 0, min = 0),
                    
                    actionButton('calcInv', 'Calculate Inventory')
                  )
                  
                )
              )
            ),
            argonColumn(
              width = 6,
              highchartOutput('spPlot1')
            )
            
          ),
          argonRow(
            argonColumn(
              width = 6,
              argonCard(
                title = 'Inventory Summary',
                shadow = TRUE,
                border_level = 1,
                width = 12,
                DT::dataTableOutput('wellCalcs')
              )
            ),
            argonColumn(
              width = 6,
              argonCard(
                title = 'GEM Decline Factors',
                shadow = TRUE,
                border_level = 1,
                width = 12,
                DT::dataTableOutput('wellCalcs1')
              )
            )
          )
        ),
        argonTabItem(
          tabName = 'map',
          awesomeRadio('mapMetric', label = 'Fluid Equivalent', choices = c('BOE', 'MCFE')),
          leafletOutput('map', height = 800),
          argonH1(display = 6, 'Sticks are Selected Operator/SubPlay Locations, Circles are that Operators Acreage within Subplay')
        )
      )
        
      
  )
)


server <- function(input, output, session) {
  values <- reactiveValues()
  
  shinyjs::disable('projectType')
  shinyjs::disable('saveProject')
  
 # observe({
 #   if(input$qiOil == 0){
 #     updateNumericInput(session, 'qiOil', value = 0.001)
 #   }
 #   
 #   if(input$qiGas == 0){
 #     updateNumericInput(session, 'qiGas', value = 0.001)
 #   }
 # })
  
  observeEvent(input$projectType, {
    check1 <-data1 %>% filter(operator == input$operator)
    if(input$projectType == 'Woodmac'){
      check1 <- check1 %>% filter(user == 'Woodmac')
    } else {
      check2 <- check1 %>% filter(user == input$wmacUser)
      check1 <- check1 %>% filter(user == 'Woodmac')
      check1 <- rbind(check2, check1) %>% 
        filter(!duplicated(paste0(operator, id)))
    }
    if(nrow(check1)==0){
      NULL
    } else {
      if(is.null(values$subPlayList)){
        
        values$subPlayList <- check1 %>% filter(operator == input$operator)
      } else {
        dataX <- check1 %>% filter(operator == input$operator)
        values$subPlayList <- dataX
      }
      updatePickerInput(session, 'subPlayList', label = 'Active SubPlays', choices = unique(values$subPlayList$id))
    }
    
    check1 <-data2 %>% filter(operator == input$operator)
    if(input$projectType == 'Woodmac'){
      check1 <- check1 %>% filter(user == 'Woodmac')
    } else {
      check2 <- check1 %>% filter(user == input$wmacUser)
      check1 <- check1 %>% filter(user == 'Woodmac')
      check1 <- rbind(check2, check1) %>% 
        filter(!duplicated(paste0(operator, id)))
    }
    if(nrow(check1)==0){
      NULL
    } else {
      if(is.null(values$pdpList)){
        
        values$pdpList <- check1 %>% filter(operator == input$operator)
      } else {
        dataX <- check1 %>% filter(operator == input$operator)
        values$pdpList <- dataX
      }
      
    }
  })
  
  
  observeEvent(input$saveProject, {
    updateButton(session, 'saveProject', label = 'Saving...', style = 'info')
    
    df1 <- values$subPlayList
    df2 <- readRDS('./data/operatorData.rds')
    df1 <- rbind(df1, df2) %>% filter(!duplicated(paste0(operator, id, user)))
    saveRDS(df1, 'data/operatorData.rds')
    
    df1 <- values$pdpList
    df2 <- readRDS('./data/pdp.rds')
    df1 <- rbind(df1, df2) %>% filter(!duplicated(paste0(operator, id, user)))
    saveRDS(df1, 'data/pdp.rds')
    Sys.sleep(2)
    updateButton(session, 'saveProject', label = 'Save Project', style = 'primary')
    
    
    
  })
  
  observeEvent(input$confirm, {
    updateButton(session, 'confirm', label = 'Confirming...', style = 'info')
    shinyjs::disable('confirm')
    values$authorization <- NULL
    values$clientID <- input$wmacUser
    values$clientSecret <- input$wmacPass
    clientID <- input$wmacUser
    clientSecret <- input$wmacPass
    authorization <- authenticate(clientID, clientSecret)
    values$authorization <- authorization
    
    url1 <- 'https://data.woodmac.com/query/l48/all/odata/attributes_horizontal_wells'
    basin <- 'Austin Chalk'
    basin1 <- paste("play_name eq '", basin, "'", sep = '')
    df <- data.frame()
    while(!is.null(url1)){
      wellInfo <- GET(url1, values$authorization,
                      add_headers(c('Accept'='application/json')),
                      query = list('$filter' = dput(basin1)))
      df1 <- content(wellInfo, as='text', encoding = 'UTF-8')[1]
      df1 <- fromJSON(df1, flatten = TRUE) %>% data.frame()
      if(nrow(df)==0){
        df <- df1
      } else {
        df <- df[,names(df1)]
        df <- rbind(df, df1)
      }
      rm(df1)
      url1 <- NULL
    }
    
    if(nrow(df) <= 1){
      updateButton(session, 'confirm', label = 'Failed!', style = 'danger')
    } else {
      updateButton(session, 'confirm', label = 'Success!', style = 'success')
      shinyjs::show('playHide')
      shinyjs::enable('projectType')
      shinyjs::enable('saveProject')
    }
    shinyjs::enable('confirm')
    
  })
  
  observeEvent(input$remove, {
    df <- values$subPlayList %>% filter(!id %in% input$subPlayList)
    values$subPlayList <- df
    df <- values$pdpList %>% filter(!id %in% input$subPlayList)
    values$pdpList <- df
    
    if(nrow(values$subPlayList)==0){
      shinyjs::disable('remove')
      shinyjs::hide('remove')
      updatePickerInput(session, 'subPlayList', label = 'Active SubPlays', choices = '')
    } else {
      shinyjs::enable('remove')
      shinyjs::show('remove')
      updatePickerInput(session, 'subPlayList', label = 'Active SubPlays',
                         choices = unique(values$subPlayList$id))
    }
  })
  
  observeEvent(input$add, {
    updateButton(session, 'add', label = 'Adding...')
    df <- data.frame(
      operator = input$operator,
      subPlay = input$subPlay,
      reservoir = input$reservoir,
      wells = input$wells,
      oilEUR = input$oilEUR,
      gasEUR = input$gasEUR,
      shrink = input$shrink,
      nglYield = input$nglYield,
      qiOil = input$qiOil,
      bOil = input$bOil,
      DiOil = values$DiOil,
      curtailOil = input$curtailOil,
      DfOil = input$DfOil,
      qiGas = input$qiGas,
      curtailGas = input$curtailGas,
      bGas = input$bGas,
      DiGas = values$DiGas,
      DfGas = input$DfGas,
      nri = input$nri,
      capex = input$capex,
      pna = input$pna,
      spudToProd = input$spudToProd,
      wellLife = input$wellLife,
      oilDiff = input$oilDiff,
      gasDiff = input$gasDiff,
      nglDiff = input$nglDiff,
      btu = input$btu,
      fixed = input$fixed,
      varExpGas = input$varExpGas,
      varExpBOE = input$varExpBOE,
      stxOil = input$stxOil,
      stxGas = input$stxGas,
      oilSTX = input$oilSTX,
      gasSTX = input$gasSTX,
      atx = input$atx,
      drill2020 = 0,
      drill2021 = 0,
      drill2022 =  0,
      drill2023 =  0,
      drill2024 =  0,
      drill2025 =  0,
      drill2026 =  0,
      drill2027 =  0,
      drill2028 = 0,
      drill2029 = 0,
      id = paste0(input$subPlay, '-', input$reservoir),
      user = input$wmacUser
    )
    
    if(is.null(values$subPlayList)){
      values$subPlayList <- df
    } else {
      df1 <- values$subPlayList
      df1 <- df1 %>% filter(!id %in% df$id)
      df1 <- rbind(df1, df)
      values$subPlayList <- df1
      #print(df1)
    }
    Sys.sleep(1)
    updatePickerInput(session, 'subPlayList', label = 'Active SubPlays',
                       choices = unique(values$subPlayList$id), 
                       selected = paste0(input$subPlay, '-', input$reservoir))
    updateButton(session, 'add', label = 'Add To Development Program')
  })
  
  output$acreageGEM <- renderText({
    df1 <- acreageGEM %>% filter(operator %in% input$operator) %>% filter(id %in% input$subPlayList)
    if(nrow(df1)==0){
      NULL
    } else {
      paste0('GEM Net Acreage: ', df1$acreage)
    }
  }
    
  )
  
  observe({
    if(is.null(values$pdpList)||nrow(values$pdpList)==0||is.null(values$price)){
      NULL
    } else {
      df1 <- values$pdpList
      df2 <- values$subPlayList
      price <- values$price
      price$Date <- paste0(year(price$DATE), '-01-', month(price$DATE))
      price$Date <- as.POSIXct(price$Date, format = '%Y-%d-%m')

      subPlay <- lapply(split(df1, df1[,'id']), function (dfy) tryCatch({
        dfx <- df2 %>% filter(id %in% dfy$id)
        df <- data.frame(years = seq(0, dfx$wellLife-1,1), Year = 2020) %>%
          mutate(Year = Year + years) %>% mutate(Oil = dfy$oilPDP*(1-dfy$decl1oil),
                                                 Gas = dfy$gasPDP*(1-dfy$decl1gas),
                                                 NGL = dfy$nglPDP*(1-dfy$decl1gas))

        df$Oil[2] <- df$Oil[1]*(1-dfy$decl2oil)
        df$Gas[2] <- df$Gas[1]*(1-dfy$decl2gas)
        df$NGL[2] <- df$NGL[1]*(1-dfy$decl2gas)

        df$Oil[3] <- df$Oil[2]*(1-dfy$decl3oil)
        df$Gas[3] <- df$Gas[2]*(1-dfy$decl3gas)
        df$NGL[3] <- df$NGL[2]*(1-dfy$decl3gas)

        df$Oil[4] <- df$Oil[3]*(1-dfy$decl4oil)
        df$Gas[4] <- df$Gas[3]*(1-dfy$decl4gas)
        df$NGL[4] <- df$NGL[3]*(1-dfy$decl4gas)

        df$Oil[5] <- df$Oil[4]*(1-dfy$decl5oil)
        df$Gas[5] <- df$Gas[4]*(1-dfy$decl5gas)
        df$NGL[5] <- df$NGL[4]*(1-dfy$decl5gas)

        df$Oil[6] <- df$Oil[5]*(1-dfy$decl6oil)
        df$Gas[6] <- df$Gas[5]*(1-dfy$decl6gas)
        df$NGL[6] <- df$NGL[5]*(1-dfy$decl6gas)

        i <- 7
        while(i <= nrow(df)){
          df$Oil[i] <- df$Oil[(i-1)]*(1-dfy$decl7oil)
          df$Gas[i] <- df$Gas[(i-1)]*(1-dfy$decl7gas)
          df$NGL[i] <- df$NGL[(i-1)]*(1-dfy$decl7gas)
          i <- i+1
        }

        dfD <- data.frame(months = seq(0, dfx$wellLife*12-1,1), Date = as.POSIXct('01-01-2020', format = '%m-%d-%Y')) %>%
          mutate(Date = Date %m+% months(months)) %>% select(Date) %>%
          mutate(Year = year(Date)) %>% left_join(df) %>%
          select(Date, Oil, SalesGas = Gas, NGL) %>%
          mutate(Gas = SalesGas/dfx$shrink*100) %>%
          mutate(days = days_in_month(Date)) %>%
          mutate(Oil = Oil*1000*days, Gas = Gas*1000*days, NGL = NGL *1000*days, SalesGas=SalesGas*1000*days) %>%
          select(Date, Oil, Gas, SalesGas, NGL) %>% mutate(wells = dfy$pdpWells) %>% mutate(months = seq(0, n()-1,1)) %>%
          mutate(wellDrop = wells[1]/(max(months)+1)*months) %>% mutate(wells = wells-wellDrop) %>%
          select(Date, Oil, Gas, SalesGas, NGL, wells)
        df <- dfD
        rm(dfD)


        df <- left_join(df, price[,c('Date', 'WTI', 'HH')])
        df$WTI <- na.locf(df$WTI)
        df$HH <- na.locf(df$HH)

        df$OilRevenue <- (df$Oil *(df$WTI-dfx$oilDiff))* dfx$nri/100
        df$GasRevenue <- (df$SalesGas*dfx$btu*(df$HH)-df$SalesGas*dfx$gasDiff)*dfx$nri/100
        df$NGLRevenue <- (df$NGL*df$WTI*dfx$nglDiff/100)*dfx$nri/100
        df$Revenue <- df$OilRevenue + df$GasRevenue + df$NGLRevenue
        df$expense <- dfx$fixed + dfx$varExpGas*df$Gas +
          dfx$varExpBOE*(df$Oil+df$NGL)
        df$tax <- df$OilRevenue*dfx$stxOil/100 + (df$GasRevenue+df$NGLRevenue)*dfx$stxGas/100 +
          df$Oil*dfx$nri/100*dfx$oilSTX + df$Gas*dfx$nri/100*dfx$gasSTX +
          df$Revenue*dfx$atx/100



        df$NOCF <- df$Revenue - df$expense - df$tax
        df$capex <- dfx$pna*(df$wells[1]-df$wells[2])

        df <- df %>% arrange(desc(Date))
        min1 <- which(df$NOCF >0)
        min1 <- min1[1]
        df <- df[min1:nrow(df),]
        df <- df %>% arrange(Date)

        df$netOil <- df$Oil*dfx$nri/100
        df$netGas <- df$SalesGas*dfx$nri/100
        df$netNGL <- df$NGL*dfx$nri/100

        df <- df %>% mutate(Months = seq(0, n()-1,1)) %>%
          select(Date, Oil, Gas, NGL, SalesGas, netOil, netGas, netNGL,expense, capex, WTI, HH, OilRevenue, GasRevenue, NGLRevenue, Revenue, tax, NOCF)



        df$FCF <- df$NOCF-df$capex

        df$id <- dfx$id
        df

        },
      error = function(e) {
        e
        NULL
      }))
      #print('1124')
      values$pdpFcst <- dplyr::bind_rows(subPlay)

    }
  })
  # 
  # 
  # 
  observeEvent(input$addPDP, {
    updateButton(session, 'addPDP', label = 'Calculating...', style = 'danger')
    df1 <- data.frame(
      operator = input$operator,
      id = input$subPlayList,
      subPlay = input$subPlay,
      reservoir = input$reservoir,
      oilPDP = input$oilPDP,
      gasPDP = input$gasPDP,
      nglPDP = input$nglPDP,
      pdpWells = input$pdpWells,
      decl1oil = input$decl1oil,
      decl2oil = input$decl2oil,
      decl3oil = input$decl3oil,
      decl4oil = input$decl4oil,
      decl5oil = input$decl5oil,
      decl6oil = input$decl6oil,
      decl7oil = input$decl7oil,
      decl1gas = input$decl1gas,
      decl2gas = input$decl2gas,
      decl3gas = input$decl3gas,
      decl4gas = input$decl4gas,
      decl5gas = input$decl5gas,
      decl6gas = input$decl6gas,
      decl7gas = input$decl7gas,
      user = input$wmacUser
    )

    if(nrow(values$pdpList)==0||is.null(values$pdpList)){
      values$pdpList <- df1
    } else {
      df2 <- values$pdpList %>% filter(!id %in% df1$id)
      values$pdpList <- rbind(df2,df1)
    }

    Sys.sleep(2)
    updateButton(session, 'addPDP', label = 'Save Change', style = 'primary')


  })
  # 
  observeEvent(input$operator, {
    check1 <-data1 %>% filter(operator == input$operator)
    if(input$projectType == 'Woodmac'){
      check1 <- check1 %>% filter(user == 'Woodmac')
    } else {
      check2 <- check1 %>% filter(user == input$wmacUser)
      check1 <- check1 %>% filter(user == 'Woodmac')
      check1 <- rbind(check2, check1) %>%
        filter(!duplicated(paste0(operator, id)))
    }
    if(nrow(check1)==0){
      NULL
    } else {
    if(is.null(values$subPlayList)){

      values$subPlayList <- check1 %>% filter(operator == input$operator)
    } else {
      dataX <- check1 %>% filter(operator == input$operator)
      values$subPlayList <- dataX
    }
      updatePickerInput(session, 'subPlayList', label = 'Active SubPlays', choices = unique(values$subPlayList$id))
    }

    check1 <-data2 %>% filter(operator == input$operator)
    if(input$projectType == 'Woodmac'){
      check1 <- check1 %>% filter(user == 'Woodmac')
    } else {
      check2 <- check1 %>% filter(user == input$wmacUser)
      check1 <- check1 %>% filter(user == 'Woodmac')
      check1 <- rbind(check2, check1) %>%
        filter(!duplicated(paste0(operator, id)))
    }
    if(nrow(check1)==0){
      NULL
    } else {
      if(is.null(values$pdpList)){

        values$pdpList <- check1 %>% filter(operator == input$operator)
      } else {
        dataX <- check1 %>% filter(operator == input$operator)
        values$pdpList <- dataX
      }

    }
  })
  # 
  output$dryEUR <- renderText(
    paste0('Dry Gas EUR: ', as.integer(input$gasEUR*input$shrink/100), ' mmcf')
  )

  output$nglEUR <- renderText(
    paste0('NGL EUR: ', as.integer(input$gasEUR*input$nglYield/1000), ' mb')
  )

  observe({
    if(is.null(values$subPlayList)||nrow(values$subPlayList)==0){
      shinyjs::hide('remove')
      shinyjs::disable('remove')
    } else{
      shinyjs::show('remove')
      shinyjs::enable('remove')
    }
  })

  observe({
    if(input$subPlay == ''|input$reservoir == ''){
      shinyjs::disable('add')
      #shinyjs::disable('remove')
    } else {
      shinyjs::enable('add')
      #shinyjs::enable('remove')
    }
  })
  # 
  observe({
    if(input$priceType == 'Strip'){
      shinyjs::hide('wti1')
      shinyjs::hide('hh1')
      shinyjs::hide('wti2')
      shinyjs::hide('hh2')
      shinyjs::hide('wti3')
      shinyjs::hide('hh3')
      shinyjs::hide('wti4')
      shinyjs::hide('hh4')
      shinyjs::hide('wti5')
      shinyjs::hide('hh5')
      shinyjs::hide('wti6')
      shinyjs::hide('hh6')
      shinyjs::hide('wti7')
      shinyjs::hide('hh7')
      shinyjs::hide('wti8')
      shinyjs::hide('hh8')
      shinyjs::hide('wti9')
      shinyjs::hide('hh9')
      shinyjs::hide('wti10')
      shinyjs::hide('hh10')

      #crude <-'https://www.eia.gov/dnav/pet/hist/LeafHandler.ashx?n=PET&s=RWTC&f=M'
      

      crude = 'https://quotes.wsj.com/futures/CRUDE%20OIL%20-%20ELECTRONIC/contracts'
      webpage <- read_html(crude)
      #tbls <- html_nodes(webpage, 'table')

      tbls_ls <- webpage %>%
        html_nodes('table') %>%
        .[1] %>%
        html_table(fill = TRUE)

      wti <- tbls_ls[[1]]

      crude = 'https://quotes.wsj.com/futures/NATURAL%20GAS/contracts'
      webpage <- read_html(crude)
      #tbls <- html_nodes(webpage, 'table')

      tbls_ls <- webpage %>%
        html_nodes('table') %>%
        .[1] %>%
        html_table(fill = TRUE)

      hh <- tbls_ls[[1]]


      rm(crude, webpage, tbls_ls)
      wti <- wti[,c('MONTH', 'SETTLEMENT')]
      hh <- hh[,c('MONTH', 'SETTLEMENT')]

      wti <- wti %>% filter(MONTH != 'Front Month')
      hh <- hh %>% filter(MONTH != 'Front Month')



      wti$YEAR <- substr(wti$MONTH, nchar(wti$MONTH)-3, nchar(wti$MONTH))
      wti$MONTH <- substr(wti$MONTH, nchar(wti$MONTH)-7, nchar(wti$MONTH)-5)


      hh$YEAR <- substr(hh$MONTH, nchar(hh$MONTH)-3, nchar(hh$MONTH))
      hh$MONTH <- substr(hh$MONTH, nchar(hh$MONTH)-7, nchar(hh$MONTH)-5)


      wti$DATE <- paste(wti$MONTH, '/01/', wti$YEAR, sep='')
      hh$DATE <- paste(hh$MONTH, '/01/', hh$YEAR, sep='')

      wti$DATE <- as.POSIXct(wti$DATE, format = '%b/%d/%Y')
      hh$DATE <- as.POSIXct(hh$DATE, format = '%b/%d/%Y')


      wti <- wti[,c('DATE', 'SETTLEMENT')]
      hh <- hh[,c('DATE', 'SETTLEMENT')]


      names(wti) <- c('DATE', 'WTI')
      names(hh) <- c('DATE', 'HH')

      date1 <- min(wti1$DATE)
      date2 <- max(hh$DATE)
      date3 <- data.frame(DATE = seq(0, 1000, 1))
      date3$DATE <- date1 %m+% months(date3$DATE)
      date3 <- date3 %>% filter(DATE <= date2)
      wti <- rbind(wti1, wti)
      wti <- merge(date3, wti, by='DATE', all.x=TRUE)

      hh <- rbind(hh1, hh)

      price <- merge(wti, hh, by='DATE', all.x=TRUE, all.y=TRUE)

      rm(wti, hh)


      if (is.na(price$WTI[1])) {
        price$WTI[1] <- price$WTI[2]
      }

      price <- price %>% group_by(DATE) %>% summarise(WTI = mean(WTI, na.rm=TRUE),
                                                      HH = mean(HH, na.rm=TRUE))

      price$WTI <- na.locf(price$WTI)
      price$HH <- na.locf(price$HH)

      price <- price %>% filter(year(DATE) >= 2015)

      values$price <- price


    } else if(input$priceType == 'Custom'){

      shinyjs::show('wti1')
      shinyjs::show('hh1')
      shinyjs::show('wti2')
      shinyjs::show('hh2')
      shinyjs::show('wti3')
      shinyjs::show('hh3')
      shinyjs::show('wti4')
      shinyjs::show('hh4')
      shinyjs::show('wti5')
      shinyjs::show('hh5')
      shinyjs::show('wti6')
      shinyjs::show('hh6')
      shinyjs::show('wti7')
      shinyjs::show('hh7')
      shinyjs::show('wti8')
      shinyjs::show('hh8')
      shinyjs::show('wti9')
      shinyjs::show('hh9')
      shinyjs::show('wti10')
      shinyjs::show('hh10')

      

      wti2 <- wti1 %>% filter(!is.na(WTI))
      hh2 <- hh1 %>% filter(!is.na(HH))

      df <- left_join(wti2, hh2)
      df1 <- data.frame(months = seq(1, 80*12, 1), DATE = max(df$DATE), WTI = NA, HH = NA)
      df1 <- df1 %>% mutate(DATE = DATE %m+% months(months))
      df1$WTI[year(df1$DATE) == 2020] <- priceValues()$wti1
      df1$WTI[year(df1$DATE) == 2021] <- priceValues()$wti2
      df1$WTI[year(df1$DATE) == 2022] <- priceValues()$wti3
      df1$WTI[year(df1$DATE) == 2023] <- priceValues()$wti4
      df1$WTI[year(df1$DATE) == 2024] <- priceValues()$wti5
      df1$WTI[year(df1$DATE) == 2025] <- priceValues()$wti6
      df1$WTI[year(df1$DATE) == 2026] <- priceValues()$wti7
      df1$WTI[year(df1$DATE) == 2027] <- priceValues()$wti8
      df1$WTI[year(df1$DATE) == 2028] <- priceValues()$wti9
      df1$WTI[year(df1$DATE) >= 2029] <- priceValues()$wti10
      df1$HH[year(df1$DATE) == 2020] <- priceValues()$hh1
      df1$HH[year(df1$DATE) == 2021] <- priceValues()$hh2
      df1$HH[year(df1$DATE) == 2022] <- priceValues()$hh3
      df1$HH[year(df1$DATE) == 2023] <- priceValues()$hh4
      df1$HH[year(df1$DATE) == 2024] <- priceValues()$hh5
      df1$HH[year(df1$DATE) == 2025] <- priceValues()$hh6
      df1$HH[year(df1$DATE) == 2026] <- priceValues()$hh7
      df1$HH[year(df1$DATE) == 2027] <- priceValues()$hh8
      df1$HH[year(df1$DATE) == 2028] <- priceValues()$hh9
      df1$HH[year(df1$DATE) >= 2029] <- priceValues()$hh10
      df1 <- subset(df1, select = -c(months))
      df <- rbind(df, df1)
      df <- df %>% filter(year(DATE) >= 2015)
      values$price <- df

    } else {
      shinyjs::show('wti1')
      shinyjs::show('hh1')
      shinyjs::hide('wti2')
      shinyjs::hide('hh2')
      shinyjs::hide('wti3')
      shinyjs::hide('hh3')
      shinyjs::hide('wti4')
      shinyjs::hide('hh4')
      shinyjs::hide('wti5')
      shinyjs::hide('hh5')
      shinyjs::hide('wti6')
      shinyjs::hide('hh6')
      shinyjs::hide('wti7')
      shinyjs::hide('hh7')
      shinyjs::hide('wti8')
      shinyjs::hide('hh8')
      shinyjs::hide('wti9')
      shinyjs::hide('hh9')
      shinyjs::hide('wti10')
      shinyjs::hide('hh10')
      
      
      
      wti2 <- wti1 %>% filter(!is.na(WTI))
      hh2 <- hh1 %>% filter(!is.na(HH))
      
      df <- left_join(wti2, hh2)
      df1 <- data.frame(months = seq(1, 80*12, 1), DATE = max(df$DATE), WTI = NA, HH = NA)
      df1 <- df1 %>% mutate(DATE = DATE %m+% months(months))
      df1$WTI <- priceValues()$wti1
      
      df1$HH <- priceValues()$hh1
      
      df1 <- subset(df1, select = -c(months))
      df <- rbind(df, df1)
      df <- df %>% filter(year(DATE) >= 2015)
      values$price <- df
      
    
    }
  })

  priceValues <- reactive({
    data.frame(
      Component = c('wti1', 'wti2',  'wti3', 'wti4', 'wti5',
                    'wti6', 'wti7', 'wti8', 'wti9', 'wti10',
                    'hh1', 'hh2',  'hh3', 'hh4', 'hh5',
                    'hh6', 'hh7', 'hh8', 'hh9', 'hh10'),
      
      Value = c(input$wti1, input$wti2, input$wti3, input$wti4, input$wti5,
                input$wti6, input$wti7, input$wti8, input$wti9, input$wti10,
                input$hh1, input$hh2, input$hh3, input$hh4, input$hh5,
                input$hh6, input$hh7, input$hh8, input$hh9, input$hh10),
      stringsAsFactors = FALSE) %>% spread(Component, Value)
    
  })

  output$prices <- renderHighchart({
    if(is.null(values$price)){
      NULL
    } else {
      price <- as.data.frame(values$price)
      price$DATE <- as.Date(price$DATE)
      cols <- c('#027971','#ff4e50', '#eaa814', '#5c1848', '#786592', '#ff4e50',  '#008542', '#5c6d00')

      highchart() %>%
        hc_colors(cols) %>%
        hc_xAxis(type = "datetime", dateTimeLabelFormats = list(day = '%b of %Y')) %>%
        hc_yAxis_multiples(list(title = list(text = "<b>WTI, $/bbl</b>"),opposite=FALSE),
                           list(title = list(text = "<b>Henry Hub, $/mcf</b>"),opposite=TRUE)) %>%
        hc_add_series(price, type = "line",
             hcaes(x = DATE, y = WTI), name = 'WTI') %>%
        hc_add_series(price, type = 'line',
                      hcaes(x=DATE, y = HH), name = 'HH', yAxis = 1) %>%
        hc_title(text = 'Price Forecast', align = 'left') %>%
        hc_subtitle(text = paste0('<i>',input$priceType,'</i>'), align = 'left') %>%
        hc_credits(enabled = TRUE, text = "Source: EIA/WSJ")
    }
  })
  # 
  declineValues <- reactive({
    data.frame(
      Component = c('qiOil', 'bOil',  'DfOil', 'curtailOil', 'oilEUR',
                    'qiGas', 'bGas', 'DfGas', 'curtailGas', 'gasEUR',
                    'wellLife'),

      Value = c(input$qiOil, input$bOil, input$DfOil, input$curtailOil, input$oilEUR,
                input$qiGas, input$bGas, input$DfGas, input$curtailGas, input$gasEUR,
                input$wellLife),
      stringsAsFactors = FALSE) %>% spread(Component, Value)

  })
  # 
  observe({
    if(is.na(input$gasEUR)||is.na(input$oilEUR)){
      NULL
    } else {
      if(input$gasEUR > 100000){
        updateNumericInput(session, 'gasEUR', value = 100000)
      }
  
      if(input$oilEUR > 10000){
        updateNumericInput(session, 'oilEUR', value = 10000)
      }
    }
  })
  # 
  expenseValues <- reactive({
    data.frame(
      Component = c('nri', 'spudToProd', 'capex', 'pna', 'oilDiff',
                    'gasDiff', 'nglDiff', 'btu', 'shrink', 'nglYield', 'stxOil',
                    'stxGas', 'oilSTX', 'gasSTX', 'atx', 'fixed',
                    'varExpGas',  'varExpBOE', 'pudDiscRate'),

      Value = c(input$nri, input$spudToProd, input$capex, input$pna, input$oilDiff,
                input$gasDiff, input$nglDiff, input$btu, input$shrink, input$nglYield, input$stxOil,
                input$stxGas, input$oilSTX, input$gasSTX, input$atx, input$fixed,
                input$varExpGas, input$varExpBOE, input$pudDiscRate
               ),
      stringsAsFactors = FALSE) %>% spread(Component, Value)

  })
  # 
  output$tcPlot <- renderHighchart({
    if((input$qiOil*input$oilEUR+input$gasEUR*input$qiGas==0)||
      (input$oilEUR/input$qiOil > 100)||(input$gasEUR/input$qiGas > 100)||
      (is.nan(input$oilEUR/input$qiOil) & is.nan(input$gasEUR/input$qiGas))){
      
      values$fcst <- NULL
    } else {
      DiOil <- 25
      oilEUR <- 0.1
      if(declineValues()$oilEUR == 0){
        DiOil <- 25
      } else {
        while(oilEUR <= declineValues()$oilEUR*1000){
          oilEUR <- sum(curtailed.q(arps.decline(
            declineValues()$qiOil*365, (DiOil), declineValues()$bOil, as.nominal(declineValues()$DfOil/100)),
            declineValues()$curtailOil/12.0, seq(0, declineValues()$wellLife-1/12, by = (1/12)))/12)
  
          DiOil <- DiOil -0.01
        }
      }
  
      DiGas<- 25
      gasEUR <- 0.1
      if(declineValues()$gasEUR == 0){
        DiGas <- 25
      } else {
        while(gasEUR <= declineValues()$gasEUR*1000){
          gasEUR <- sum(curtailed.q(arps.decline(
            declineValues()$qiGas*365, (DiGas), declineValues()$bGas, as.nominal(declineValues()$DfGas/100)),
            declineValues()$curtailGas/12.0, seq(0, declineValues()$wellLife-1/12, by = (1/12)))/12)
  
          DiGas <- DiGas -0.01
        }
      }
      values$DiOil <- DiOil
      values$DiGas <- DiGas
  
      oil <- curtailed.q(arps.decline(
        declineValues()$qiOil*365, (DiOil), declineValues()$bOil, as.nominal(declineValues()$DfOil/100)),
        declineValues()$curtailOil/12.0, seq(0, declineValues()$wellLife-1/12, by = (1/12)))/12
      gas <- curtailed.q(arps.decline(
        declineValues()$qiGas*365, (DiGas), declineValues()$bGas, as.nominal(declineValues()$DfGas/100)),
        declineValues()$curtailGas/12.0, seq(0, declineValues()$wellLife-1/12, by = (1/12)))/12
  
  
  
      df <- data.frame(Months = seq(1, declineValues()$wellLife*12, by = 1), Gas = gas, Oil = oil)
      rm(oil, gas)
  
        #df <- df %>% filter(Months <= (declineValues()$wellLife)*12) %>%filter(!duplicated(Months))
        df <- as.data.frame(df)
  
  
  
  
      #print(head(df))
      #values$oilEUR <- as.integer(sum(df$Oil)/1000)
      #values$gasEUR <- as.integer(sum(df$Gas)/1000)
      #print(head(df))
      values$fcst <- df
      df <- as.data.frame(df)
      df$Oil <- df$Oil/30.45
      df$Gas <- df$Gas/30.45
      df <- df %>% filter(Months <= 36)
      df$Oil <- as.integer(df$Oil)
      df$Gas <- as.integer(df$Gas)
      #print(head(df))
      #df <- df %>% gather(Component, Value, -c(Months))
      #df$Value <- df$Value/30.45
      #df <- df %>% filter((Months >= 1 & Months < 12)|Months %% 12 == 0)
  
      cols <- c('#027971','#ff4e50', '#eaa814', '#5c1848', '#786592', '#ff4e50',  '#008542', '#5c6d00')
      if(is.nan(input$oilEUR/input$qiOil)){
        highchart() %>%
          hc_colors(cols) %>%
          hc_xAxis(title = list(text = '<b>Months Producing</b>')) %>%
          hc_yAxis_multiples(list(title = list(text = "<b>Oil Production, bbl/d</b>"),opposite=FALSE),
                             list(title = list(text = "<b>Gas Production, mcf/d</b>"),opposite=TRUE)) %>%
          #hc_add_series(df, type = "line",
           #             hcaes(x = Months, y = Oil), name = 'Oil') %>%
          hc_add_series(df, type = 'line',
                        hcaes(x=Months, y = Gas), name = 'Gas', yAxis = 1) %>%
          hc_title(text = 'Type Curve Forecast', align = 'left') %>%
          #hc_subtitle(text = paste0('<i>','Oil EUR (mbo): ',input$oilEUR,' Gas EUR (mmcf): ', input$gasEUR, '</i>'), align = 'left') %>%
          hc_credits(enabled = TRUE, text = "Wood Mackenzie", src = "http://www.woodmac.com")
      } else if (is.nan(input$gasEUR/input$qiGas)){
        highchart() %>%
          hc_colors(cols) %>%
          hc_xAxis(title = list(text = '<b>Months Producing</b>')) %>%
          hc_yAxis_multiples(list(title = list(text = "<b>Oil Production, bbl/d</b>"),opposite=FALSE),
                             list(title = list(text = "<b>Gas Production, mcf/d</b>"),opposite=TRUE)) %>%
          hc_add_series(df, type = "line",
                        hcaes(x = Months, y = Oil), name = 'Oil') %>%
          #hc_add_series(df, type = 'line',
           #             hcaes(x=Months, y = Gas), name = 'Gas', yAxis = 1) %>%
          hc_title(text = 'Type Curve Forecast', align = 'left') %>%
          #hc_subtitle(text = paste0('<i>','Oil EUR (mbo): ',input$oilEUR,' Gas EUR (mmcf): ', input$gasEUR, '</i>'), align = 'left') %>%
          hc_credits(enabled = TRUE, text = "Wood Mackenzie", src = "http://www.woodmac.com")
      } else {
      highchart() %>%
        hc_colors(cols) %>%
        hc_xAxis(title = list(text = '<b>Months Producing</b>')) %>%
        hc_yAxis_multiples(list(title = list(text = "<b>Oil Production, bbl/d</b>"),opposite=FALSE),
                           list(title = list(text = "<b>Gas Production, mcf/d</b>"),opposite=TRUE)) %>%
        hc_add_series(df, type = "line",
                      hcaes(x = Months, y = Oil), name = 'Oil') %>%
        hc_add_series(df, type = 'line',
                      hcaes(x=Months, y = Gas), name = 'Gas', yAxis = 1) %>%
        hc_title(text = 'Type Curve Forecast', align = 'left') %>%
        #hc_subtitle(text = paste0('<i>','Oil EUR (mbo): ',input$oilEUR,' Gas EUR (mmcf): ', input$gasEUR, '</i>'), align = 'left') %>%
        hc_credits(enabled = TRUE, text = "Wood Mackenzie", src = "http://www.woodmac.com")
      }
      }


  })
  # 
  # 
  # 
  observe({
    if(is.null(values$fcst)||is.null(values$price)||(input$qiOil*input$oilEUR+input$gasEUR*input$qiGas==0)||
       (input$oilEUR/input$qiOil > 100)||(input$gasEUR/input$qiGas > 100)||
       (is.nan(input$oilEUR/input$qiOil))||(is.nan(input$gasEUR/input$qiGas))){
      NULL
    } else {

      df <- values$fcst
      
      #print('1644')
      df$Date <- paste0(year(today()), '-1-', month(today()))
      #print(head(df))
      #print('1647')
      df$Date <- as.POSIXct(df$Date, format = '%Y-%d-%m')
      #print(head(df))
      #print(expenseValues()$spudToProd)
      df$Months <- seq(1, nrow(df), 1)
      df$Date <- df$Date %m+% months(df$Months + expenseValues()$spudToProd)
      #print(head(df))
      price <- values$price
      price$Date <- paste0(year(price$DATE), '-01-', month(price$DATE))
      price$Date <- as.POSIXct(price$Date, format = '%Y-%d-%m')
      df <- left_join(df, price[,c('Date', 'WTI', 'HH')])
      df$WTI <- na.locf(df$WTI)
      df$HH <- na.locf(df$HH)
      
      df$NGL <- df$Gas*expenseValues()$nglYield/1000
      df$SalesGas <- df$Gas * expenseValues()$shrink/100
      df$OilRevenue <- (df$Oil *(df$WTI-expenseValues()$oilDiff))* expenseValues()$nri/100
      df$GasRevenue <- (df$SalesGas*expenseValues()$btu*(df$HH)-df$SalesGas*expenseValues()$gasDiff)*expenseValues()$nri/100
      df$NGLRevenue <- (df$NGL*df$WTI*expenseValues()$nglDiff/100)*expenseValues()$nri/100
      df$Revenue <- df$OilRevenue + df$GasRevenue + df$NGLRevenue
      df$expense <- expenseValues()$fixed + expenseValues()$varExpGas*df$Gas +
        expenseValues()$varExpBOE*(df$Oil+df$NGL)
      df$tax <- df$OilRevenue*expenseValues()$stxOil/100 + (df$GasRevenue+df$NGLRevenue)*expenseValues()$stxGas/100 +
        df$Oil*expenseValues()$nri/100*expenseValues()$oilSTX + df$Gas*expenseValues()$nri/100*expenseValues()$gasSTX +
        df$Revenue*expenseValues()$atx/100

      df$NOCF <- df$Revenue - df$expense - df$tax
      df$capex <- 0
      
      df <- df %>% arrange(desc(Date))
      #print('1712')
      min1 <- which(df$NOCF >0)
      min1 <- min1[1]
      if(is.na(min1)){
        output$npv10 <- renderText('Not Economic')
        
        output$irr <- renderText('Not Economic')
      } else {
        df <- df[min1:nrow(df),]
        df <- df %>% arrange(Date)
        df$capex[nrow(df)] <- expenseValues()$pna
  
  
        df1 <- df[1:expenseValues()$spudToProd,]
        df1$Gas <- 0
        df1$Oil <- 0
        df1$NGL <- 0
        df1$SalesGas <- 0
        df1$OilRevenue <- 0
        df1$GasRevenue <- 0
        df1$NGLRevenue <- 0
        df1$Revenue <- 0
        df1$expense <- 0
        df1$tax <- 0
        df1$NOCF <- 0
        df1$capex[1] <- expenseValues()$capex
  
        df <- rbind(df1, df) %>% select(Months, Oil, Gas, NGL, SalesGas, WTI, HH, OilRevenue, GasRevenue, NGLRevenue, Revenue, expense, tax, NOCF, capex)
        df$WTI[is.na(df$WTI)] <- mean(df$WTI, na.rm=TRUE)
        df$HH[is.na(df$HH)] <- mean(df$HH, na.rm=TRUE)
        df$capex[is.na(df$capex)] <- 0
        df$FCF <- df$NOCF-df$capex
        df$Months <- seq(0, nrow(df)-1, 1)
        
        df$pv10 <- df$FCF/1.1^(df$Months/12)
        #print(head(df))
        output$npv10 <- renderText(paste0('NPV-10, (US$ Thousands): ',dollar(as.integer(sum(df$pv10)/1000))))
        irr <- IRRcalc(df$FCF, df$Months)
        output$irr <- renderText(paste0('Type Curve IRR: ',percent(irr)))
      }

      #print(head(df))

    }
  })
  # 
  # 
  output$subPlayIRR <- renderText({
    if(is.null(values$subPlayList)||nrow(values$subPlayList)==0||is.null(values$price)){
      NULL
    } else {
      df1 <- values$subPlayList %>% filter(id %in% input$subPlayList)
      oil <- curtailed.q(arps.decline(
        df1$qiOil*365, (df1$DiOil), df1$bOil, as.nominal(df1$DfOil/100)),
        df1$curtailOil/12.0, seq(0, df1$wellLife-1/12, by = (1/12)))/12
      gas <- curtailed.q(arps.decline(
        df1$qiGas*365, (df1$DiGas), df1$bGas, as.nominal(df1$DfGas/100)),
        df1$curtailGas/12.0, seq(0, df1$wellLife-1/12, by = (1/12)))/12



      df <- data.frame(Months = seq(1, df1$wellLife*12, by = 1), Gas = gas, Oil = oil)
      rm(oil, gas)

      #df <- df %>% filter(Months <= (declineValues()$wellLife)*12) %>%filter(!duplicated(Months))
      df <- as.data.frame(df)
      df$Date <- paste0(year(today()), '-1-', month(today()))

      df$Date <- as.POSIXct(df$Date, format = '%Y-%d-%m')
      #print(head(df))
      df$Date <- df$Date %m+% months(df$Months + df1$spudToProd)
      price <- values$price
      price$Date <- paste0(year(price$DATE), '-01-', month(price$DATE))
      price$Date <- as.POSIXct(price$Date, format = '%Y-%d-%m')
      df <- left_join(df, price[,c('Date', 'WTI', 'HH')])
      df$WTI <- na.locf(df$WTI)
      df$HH <- na.locf(df$HH)
      df$NGL <- df$Gas*df1$nglYield/1000
      df$SalesGas <- df$Gas * df1$shrink/100
      df$OilRevenue <- (df$Oil *(df$WTI-df1$oilDiff))* df1$nri/100
      df$GasRevenue <- (df$SalesGas*df1$btu*(df$HH)-df$SalesGas*df1$gasDiff)*df1$nri/100
      df$NGLRevenue <- (df$NGL*df$WTI*df1$nglDiff/100)*df1$nri/100
      df$Revenue <- df$OilRevenue + df$GasRevenue + df$NGLRevenue
      df$expense <- df1$fixed + df1$varExpGas*df$Gas +
        df1$varExpBOE*(df$Oil+df$NGL)
      df$tax <- df$OilRevenue*df1$stxOil/100 + (df$GasRevenue+df$NGLRevenue)*df1$stxGas/100 +
        df$Oil*df1$nri/100*df1$oilSTX + df$Gas*df1$nri/100*df1$gasSTX +
        df$Revenue*df1$atx/100

      #print(head(df))

      df$NOCF <- df$Revenue - df$expense - df$tax
      df$capex <- 0

      df <- df %>% arrange(desc(Date))
      #print('1762')
      min1 <- which(df$NOCF >0)
      min1 <- min1[1]
      if(is.na(min1)){
        paste0('Uneconomic')
      } else {
        df <- df[min1:nrow(df),]
        df <- df %>% arrange(Date)
        df$capex[nrow(df)] <- df1$pna
  
  
        df2 <- df[1:df1$spudToProd,]
        df2$Gas <- 0
        df2$Oil <- 0
        df2$NGL <- 0
        df2$SalesGas <- 0
        df2$OilRevenue <- 0
        df2$GasRevenue <- 0
        df2$NGLRevenue <- 0
        df2$Revenue <- 0
        df2$expense <- 0
        df2$tax <- 0
        df2$NOCF <- 0
        df2$capex[1] <- df1$capex
  
        df <- rbind(df2, df) %>% select(Months, Oil, Gas, NGL, SalesGas, WTI, HH, OilRevenue, GasRevenue, NGLRevenue, Revenue, expense, tax, NOCF, capex)
        df$FCF <- df$NOCF-df$capex
        df$Months <- seq(0, nrow(df)-1, 1)
        #print(head(df))
        #df$pv10 <- df$FCF/1.1^(df$Months/12)
  
  
        irr <- IRRcalc(df$FCF, df$Months)
        paste0('IRR: ',percent(irr))
      }
    }

  })
  # 
  observeEvent(input$drill2020, {
    if(is.null(values$subPlayList)||nrow(values$subPlayList)==0||input$subPlayList == ''||is.na(input$drill2020)){
      NULL
    } else {
      wellCount <- values$subPlayList %>% filter(id %in% input$subPlayList) %>% filter(operator %in% input$operator)
      
      wellCount <- wellCount$wells
      #print(wellCount)
      df <- values$subPlayList %>% filter(!id %in% input$subPlayList)
      df1 <- values$subPlayList %>% filter(id %in% input$subPlayList)
      df1$user <- input$wmacUser
      df1$drill2020 <- input$drill2020
      #print(df1)
      values$subPlayList <- rbind(df, df1)
      output$remInv1 <- renderText(paste0('Remaining Wells: ',as.integer(wellCount -
                                                                           input$drill2020*12)))

      if(input$drill2020*12 >= wellCount){
        updateNumericInput(session, 'drill2020', value = wellCount/12)
        updateNumericInput(session, 'drill2021', value = 0)
        updateNumericInput(session, 'drill2022', value = 0)
        updateNumericInput(session, 'drill2023', value = 0)
        updateNumericInput(session, 'drill2024', value = 0)
        updateNumericInput(session, 'drill2025', value = 0)
        updateNumericInput(session, 'drill2026', value = 0)
        updateNumericInput(session, 'drill2027', value = 0)
        updateNumericInput(session, 'drill2028', value = 0)
        updateNumericInput(session, 'drill2029', value = 0)

      }
    }
  })
  # 
  observeEvent(input$drill2021, {
    if(is.null(values$subPlayList)||nrow(values$subPlayList)==0||input$subPlayList == ''||is.na(input$drill2021)){
      NULL
    } else {
      wellCount <- values$subPlayList %>% filter(id %in% input$subPlayList) %>% filter(operator %in% input$operator)
      wellCount <- wellCount$wells
      df <- values$subPlayList %>% filter(!id %in% input$subPlayList)
      df1 <- values$subPlayList %>% filter(id %in% input$subPlayList)
      df1$user <- input$wmacUser
      df1$drill2021 <- input$drill2021
      values$subPlayList <- rbind(df, df1)
      output$remInv2 <- renderText(paste0('Remaining Wells: ',as.integer(wellCount -
                                                                           input$drill2020*12-
                                                                           input$drill2021*12)))

      if(input$drill2020*12 + input$drill2021*12 >= wellCount){

        updateNumericInput(session, 'drill2021', value = (wellCount - input$drill2020*12)/12)
        updateNumericInput(session, 'drill2022', value = 0)
        updateNumericInput(session, 'drill2023', value = 0)
        updateNumericInput(session, 'drill2024', value = 0)
        updateNumericInput(session, 'drill2025', value = 0)
        updateNumericInput(session, 'drill2026', value = 0)
        updateNumericInput(session, 'drill2027', value = 0)
        updateNumericInput(session, 'drill2028', value = 0)
        updateNumericInput(session, 'drill2029', value = 0)

      }
    }
  })
  # 
  observeEvent(input$drill2022, {
    if(is.null(values$subPlayList)||nrow(values$subPlayList)==0||input$subPlayList == ''||is.na(input$drill2022)){
      NULL
    } else {
      wellCount <- values$subPlayList %>% filter(id %in% input$subPlayList) %>% filter(operator %in% input$operator)
      wellCount <- wellCount$wells
      df <- values$subPlayList %>% filter(!id %in% input$subPlayList)
      df1 <- values$subPlayList %>% filter(id %in% input$subPlayList)
      df1$user <- input$wmacUser
      df1$drill2022 <- input$drill2022
      values$subPlayList <- rbind(df, df1)
      output$remInv3 <- renderText(paste0('Remaining Wells: ',as.integer(wellCount -
                                                                           input$drill2020*12-
                                                                           input$drill2021*12-
                                                                           input$drill2022*12)))

      if(input$drill2020*12 + input$drill2021*12 + input$drill2022*12 >= wellCount){

        updateNumericInput(session, 'drill2022', value = (wellCount - input$drill2020*12 - input$drill2021*12)/12)
        updateNumericInput(session, 'drill2023', value = 0)
        updateNumericInput(session, 'drill2024', value = 0)
        updateNumericInput(session, 'drill2025', value = 0)
        updateNumericInput(session, 'drill2026', value = 0)
        updateNumericInput(session, 'drill2027', value = 0)
        updateNumericInput(session, 'drill2028', value = 0)
        updateNumericInput(session, 'drill2029', value = 0)

      }
    }
  })
  # 
  observeEvent(input$drill2023, {
    if(is.null(values$subPlayList)||nrow(values$subPlayList)==0||input$subPlayList == ''||is.na(input$drill2023)){
      NULL
    } else {
      wellCount <- values$subPlayList %>% filter(id %in% input$subPlayList) %>% filter(operator %in% input$operator)
      wellCount <- wellCount$wells
      df <- values$subPlayList %>% filter(!id %in% input$subPlayList)
      df1 <- values$subPlayList %>% filter(id %in% input$subPlayList)
      df1$user <- input$wmacUser
      df1$drill2023 <- input$drill2023
      values$subPlayList <- rbind(df, df1)
      output$remInv4 <- renderText(paste0('Remaining Wells: ',as.integer(wellCount -
                                                                           input$drill2020*12-
                                                                           input$drill2021*12-
                                                                           input$drill2022*12 -
                                                                           input$drill2023*12)))

      if(input$drill2020*12 + input$drill2021*12 +
         input$drill2022*12+ input$drill2023*12 >= wellCount){

        updateNumericInput(session, 'drill2023', value = (wellCount -
                             input$drill2020*12 - input$drill2021*12 - input$drill2022*12)/12)
        updateNumericInput(session, 'drill2024', value = 0)
        updateNumericInput(session, 'drill2025', value = 0)
        updateNumericInput(session, 'drill2026', value = 0)
        updateNumericInput(session, 'drill2027', value = 0)
        updateNumericInput(session, 'drill2028', value = 0)
        updateNumericInput(session, 'drill2029', value = 0)

      }
    }
  })
  # 
  observeEvent(input$drill2024, {
    if(is.null(values$subPlayList)||nrow(values$subPlayList)==0||input$subPlayList == ''||is.na(input$drill2024)){
      NULL
    } else {
      wellCount <- values$subPlayList %>% filter(id %in% input$subPlayList) %>% filter(operator %in% input$operator)
      wellCount <- wellCount$wells
      df <- values$subPlayList %>% filter(!id %in% input$subPlayList)
      df1 <- values$subPlayList %>% filter(id %in% input$subPlayList)
      df1$user <- input$wmacUser
      df1$drill2024 <- input$drill2024
      values$subPlayList <- rbind(df, df1)
      output$remInv5 <- renderText(paste0('Remaining Wells: ',as.integer(wellCount -
                                                                           input$drill2020*12-
                                                                           input$drill2021*12-
                                                                           input$drill2022*12 -
                                                                           input$drill2023*12 -
                                                                           input$drill2024*12)))

      if(input$drill2020*12 + input$drill2021*12 +
         input$drill2022*12+ input$drill2023*12 + input$drill2024*12 >= wellCount){

        updateNumericInput(session, 'drill2024', value = (wellCount - input$drill2020*12 -
                             input$drill2021*12 - input$drill2022*12 - input$drill2023*12)/12)
        updateNumericInput(session, 'drill2025', value = 0)
        updateNumericInput(session, 'drill2026', value = 0)
        updateNumericInput(session, 'drill2027', value = 0)
        updateNumericInput(session, 'drill2028', value = 0)
        updateNumericInput(session, 'drill2029', value = 0)

      }
    }
  })

  observeEvent(input$drill2025, {
    if(is.null(values$subPlayList)||nrow(values$subPlayList)==0||input$subPlayList == ''||is.na(input$drill2025)){
      NULL
    } else {
      wellCount <- values$subPlayList %>% filter(id %in% input$subPlayList) %>% filter(operator %in% input$operator)
      wellCount <- wellCount$wells
      df <- values$subPlayList %>% filter(!id %in% input$subPlayList)
      df1 <- values$subPlayList %>% filter(id %in% input$subPlayList)
      df1$user <- input$wmacUser
      df1$drill2025 <- input$drill2025
      values$subPlayList <- rbind(df, df1)
      output$remInv6 <- renderText(paste0('Remaining Wells: ',as.integer(wellCount -
                                                                           input$drill2020*12-
                                                                           input$drill2021*12-
                                                                           input$drill2022*12 -
                                                                           input$drill2023*12 -
                                                                           input$drill2024*12 -
                                                                           input$drill2025*12)))

      if(input$drill2020*12 + input$drill2021*12 +
         input$drill2022*12+ input$drill2023*12 +
         input$drill2024*12 + input$drill2025*12>= wellCount){

        updateNumericInput(session, 'drill2025', value = (wellCount -
                             input$drill2020*12 - input$drill2021*12 -
                             input$drill2022*12 - input$drill2023*12 -
                             input$drill2024*12)/12)
        updateNumericInput(session, 'drill2026', value = 0)
        updateNumericInput(session, 'drill2027', value = 0)
        updateNumericInput(session, 'drill2028', value = 0)
        updateNumericInput(session, 'drill2029', value = 0)

      }
    }
  })

  observeEvent(input$drill2026, {
    if(is.null(values$subPlayList)||nrow(values$subPlayList)==0||input$subPlayList == ''||is.na(input$drill2026)){
      NULL
    } else {
      wellCount <- values$subPlayList %>% filter(id %in% input$subPlayList) %>% filter(operator %in% input$operator)
      wellCount <- wellCount$wells
      df <- values$subPlayList %>% filter(!id %in% input$subPlayList)
      df1 <- values$subPlayList %>% filter(id %in% input$subPlayList)
      df1$user <- input$wmacUser
      df1$drill2026 <- input$drill2026
      values$subPlayList <- rbind(df, df1)
      output$remInv7 <- renderText(paste0('Remaining Wells: ',as.integer(wellCount -
                                                                           input$drill2020*12-
                                                                           input$drill2021*12-
                                                                           input$drill2022*12 -
                                                                           input$drill2023*12 -
                                                                           input$drill2024*12 -
                                                                           input$drill2025*12 -
                                                                           input$drill2026*12)))

      if(input$drill2020*12 + input$drill2021*12 +
         input$drill2022*12+ input$drill2023*12 +
         input$drill2024*12 + input$drill2025*12 +
         input$drill2026*12 >= wellCount){

        updateNumericInput(session, 'drill2026', value = (wellCount -
                             input$drill2020*12 - input$drill2021*12 -
                             input$drill2022*12 - input$drill2023*12 -
                             input$drill2024*12 - input$drill2025*12)/12)
        updateNumericInput(session, 'drill2027', value = 0)
        updateNumericInput(session, 'drill2028', value = 0)
        updateNumericInput(session, 'drill2029', value = 0)

      }
    }
  })

  observeEvent(input$drill2027, {
    if(is.null(values$subPlayList)||nrow(values$subPlayList)==0||input$subPlayList == ''||is.na(input$drill2027)){
      NULL
    } else {
      wellCount <- values$subPlayList %>% filter(id %in% input$subPlayList) %>% filter(operator %in% input$operator)
      wellCount <- wellCount$wells
      df <- values$subPlayList %>% filter(!id %in% input$subPlayList)
      df1 <- values$subPlayList %>% filter(id %in% input$subPlayList)
      df1$user <- input$wmacUser
      df1$drill2027 <- input$drill2027
      values$subPlayList <- rbind(df, df1)

      output$remInv8 <- renderText(paste0('Remaining Wells: ',as.integer(wellCount -
                                                                           input$drill2020*12-
                                                                           input$drill2021*12-
                                                                           input$drill2022*12 -
                                                                           input$drill2023*12 -
                                                                           input$drill2024*12 -
                                                                           input$drill2025*12 -
                                                                           input$drill2026*12 -
                                                                           input$drill2027*12)))

      if(input$drill2020*12 + input$drill2021*12 +
         input$drill2022*12 + input$drill2023*12 +
         input$drill2024*12 + input$drill2025*12 +
         input$drill2026*12 + input$drill2027*12 >= wellCount){

        updateNumericInput(session, 'drill2027', value = (wellCount -
                             input$drill2020*12 - input$drill2021*12 -
                             input$drill2022*12 - input$drill2023*12 -
                             input$drill2024*12 - input$drill2025*12 -
                             input$drill2026*12)/12)
        updateNumericInput(session, 'drill2028', value = 0)
        updateNumericInput(session, 'drill2029', value = 0)

      }
    }
  })

  observeEvent(input$drill2028, {
    if(is.null(values$subPlayList)||nrow(values$subPlayList)==0||input$subPlayList == ''||is.na(input$drill2028)){
      NULL
    } else {
      wellCount <- values$subPlayList %>% filter(id %in% input$subPlayList) %>% filter(operator %in% input$operator)
      wellCount <- wellCount$wells
      df <- values$subPlayList %>% filter(!id %in% input$subPlayList)
      df1 <- values$subPlayList %>% filter(id %in% input$subPlayList)
      df1$user <- input$wmacUser
      df1$drill2028 <- input$drill2028
      values$subPlayList <- rbind(df, df1)

      output$remInv9 <- renderText(paste0('Remaining Wells: ',as.integer(wellCount -
                                                                         input$drill2020*12-
                                                                           input$drill2021*12-
                                                                           input$drill2022*12 -
                                                                           input$drill2023*12 -
                                                                           input$drill2024*12 -
                                                                           input$drill2025*12 -
                                                                           input$drill2026*12 -
                                                                           input$drill2027*12 -
                                                                           input$drill2028*12)))

      if(input$drill2020*12 + input$drill2021*12 +
         input$drill2022*12+ input$drill2023*12 +
         input$drill2024*12 + input$drill2025*12 +
         input$drill2026*12 + input$drill2027*12 +
         input$drill2028*12 >= wellCount){

        updateNumericInput(session, 'drill2028', value = (wellCount -
                             input$drill2020*12 - input$drill2021*12 -
                             input$drill2022*12 - input$drill2023*12 -
                             input$drill2024*12 - input$drill2025*12 -
                             input$drill2026*12 - input$drill2027*12)/12)
        updateNumericInput(session, 'drill2029', value = 0)

      }
    }
  })

  observeEvent(input$drill2029, {
    if(is.null(values$subPlayList)||nrow(values$subPlayList)==0||input$subPlayList == ''||is.na(input$drill2029)){
      NULL
    } else {
      wellCount <- values$subPlayList %>% filter(id %in% input$subPlayList) %>% filter(operator %in% input$operator)
      wellCount <- wellCount$wells

      df <- values$subPlayList %>% filter(!id %in% input$subPlayList)
      df1 <- values$subPlayList %>% filter(id %in% input$subPlayList)
      df1$user <- input$wmacUser
      df1$drill2029 <- input$drill2029
      values$subPlayList <- rbind(df, df1)

      remWells <- wellCount -
        input$drill2020*12-
        input$drill2021*12-
        input$drill2022*12 -
        input$drill2023*12 -
        input$drill2024*12 -
        input$drill2025*12 -
        input$drill2026*12 -
        input$drill2027*12 -
        input$drill2028*12
      #print(remWells)
      #print(input$drill2029)
      output$remInv10 <- renderText(paste0('Remaining Months: ', as.integer(remWells/input$drill2029)))
    }
  })
  # 
  observeEvent(input$projectType, {
    if(is.null(values$subPlayList)||nrow(values$subPlayList)==0){
      NULL
    } else {
      df <- values$subPlayList %>% filter(id %in% input$subPlayList) %>%
        filter(operator %in% input$operator)
      updateNumericInput(session, 'drill2020', value = df$drill2020)
      updateNumericInput(session, 'drill2021', value = df$drill2021)
      updateNumericInput(session, 'drill2022', value = df$drill2022)
      updateNumericInput(session, 'drill2023', value = df$drill2023)
      updateNumericInput(session, 'drill2024', value = df$drill2024)
      updateNumericInput(session, 'drill2025', value = df$drill2025)
      updateNumericInput(session, 'drill2026', value = df$drill2026)
      updateNumericInput(session, 'drill2027', value = df$drill2027)
      updateNumericInput(session, 'drill2028', value = df$drill2028)
      updateNumericInput(session, 'drill2029', value = df$drill2029)
      updateTextInput(session, 'subPlay', value = df$subPlay)
      updateTextInput(session, 'reservoir', value = df$reservoir)
      updateNumericInput(session, 'wells', value = df$wells)
      updateNumericInput(session, 'oilEUR', value = df$oilEUR)
      updateNumericInput(session, 'shrink', value = df$shrink)
      updateNumericInput(session, 'nglYield', value = df$nglYield)
      updateNumericInput(session, 'qiOil', value = df$qiOil)
      updateNumericInput(session, 'bOil', value = df$bOil)
      updateNumericInput(session, 'DiOil', value = df$DiOil)
      updateNumericInput(session, 'curtailOil', value = df$curtailOil)
      updateNumericInput(session, 'DfOil', value = df$DfOil)
      updateNumericInput(session, 'qiGas', value = df$qiGas)
      updateNumericInput(session, 'curtailGas', value = df$curtailGas)
      updateNumericInput(session, 'gasEUR', value = df$gasEUR)
      updateNumericInput(session, 'bGas', value = df$bGas)
      updateNumericInput(session, 'DiGas', value = df$DiGas)
      updateNumericInput(session, 'DfGas', value = df$DfGas)
      updateNumericInput(session, 'nri', value = df$nri)
      updateNumericInput(session, 'capex', value = df$capex)
      updateNumericInput(session, 'pna', value = df$pna)
      updateNumericInput(session, 'spudToProd', value = df$spudToProd)
      updateNumericInput(session, 'wellLife', value = df$wellLife)
      updateNumericInput(session, 'oilDiff', value = df$oilDiff)
      updateNumericInput(session, 'gasDiff', value = df$gasDiff)
      updateNumericInput(session, 'nglDiff', value = df$nglDiff)
      updateNumericInput(session, 'btu', value = df$btu)
      updateNumericInput(session, 'fixed', value = df$fixed)
      updateNumericInput(session, 'varExpGas', value = df$varExpGas)
      updateNumericInput(session, 'varExpBOE', value = df$varExpBOE)
      updateNumericInput(session, 'stxOil', value = df$stxOil)
      updateNumericInput(session, 'stxGas', value = df$stxGas)
      updateNumericInput(session, 'oilSTX', value = df$oilSTX)
      updateNumericInput(session, 'gasSTX', value = df$gasSTX)
      updateNumericInput(session, 'atx', value = df$atx)

    }
    if(is.null(values$pdpList)||nrow(values$pdpList)==0){
      NULL
    } else {
      df <- values$pdpList %>% filter(operator %in% input$operator) %>%
        filter(id %in% input$subPlayList)
      updateNumericInput(session, 'oilPDP', value = df$oilPDP)
      updateNumericInput(session, 'gasPDP', value = df$gasPDP)
      updateNumericInput(session, 'nglPDP', value = df$nglPDP)
      updateNumericInput(session, 'pdpWells', value = df$pdpWells)
      updateNumericInput(session, 'decl1oil', value = df$decl1oil)
      updateNumericInput(session, 'decl2oil', value = df$decl2oil)
      updateNumericInput(session, 'decl3oil', value = df$decl3oil)
      updateNumericInput(session, 'decl4oil', value = df$decl4oil)
      updateNumericInput(session, 'decl5oil', value = df$decl5oil)
      updateNumericInput(session, 'decl6oil', value = df$decl6oil)
      updateNumericInput(session, 'decl7oil', value = df$decl7oil)
      updateNumericInput(session, 'decl1gas', value = df$decl1gas)
      updateNumericInput(session, 'decl2gas', value = df$decl2gas)
      updateNumericInput(session, 'decl3gas', value = df$decl3gas)
      updateNumericInput(session, 'decl4gas', value = df$decl4gas)
      updateNumericInput(session, 'decl5gas', value = df$decl5gas)
      updateNumericInput(session, 'decl6gas', value = df$decl6gas)
      updateNumericInput(session, 'decl7gas', value = df$decl7gas)
    }

  })
  # 
  observeEvent(input$subPlayList, {
    if(is.null(values$subPlayList)||nrow(values$subPlayList)==0){
      NULL
    } else {
      df <- values$subPlayList %>% filter(id %in% input$subPlayList) %>%
        filter(operator %in% input$operator)
      updateNumericInput(session, 'drill2020', value = df$drill2020)
      updateNumericInput(session, 'drill2021', value = df$drill2021)
      updateNumericInput(session, 'drill2022', value = df$drill2022)
      updateNumericInput(session, 'drill2023', value = df$drill2023)
      updateNumericInput(session, 'drill2024', value = df$drill2024)
      updateNumericInput(session, 'drill2025', value = df$drill2025)
      updateNumericInput(session, 'drill2026', value = df$drill2026)
      updateNumericInput(session, 'drill2027', value = df$drill2027)
      updateNumericInput(session, 'drill2028', value = df$drill2028)
      updateNumericInput(session, 'drill2029', value = df$drill2029)
      updateTextInput(session, 'subPlay', value = df$subPlay)
      updateTextInput(session, 'reservoir', value = df$reservoir)
      updateNumericInput(session, 'wells', value = df$wells)
      updateNumericInput(session, 'oilEUR', value = df$oilEUR)
      updateNumericInput(session, 'gasEUR', value = df$gasEUR)
      updateNumericInput(session, 'shrink', value = df$shrink)
      updateNumericInput(session, 'nglYield', value = df$nglYield)
      updateNumericInput(session, 'qiOil', value = df$qiOil)
      updateNumericInput(session, 'bOil', value = df$bOil)
      updateNumericInput(session, 'DiOil', value = df$DiOil)
      updateNumericInput(session, 'curtailOil', value = df$curtailOil)
      updateNumericInput(session, 'DfOil', value = df$DfOil)
      updateNumericInput(session, 'qiGas', value = df$qiGas)
      updateNumericInput(session, 'curtailGas', value = df$curtailGas)
      updateNumericInput(session, 'bGas', value = df$bGas)
      updateNumericInput(session, 'DiGas', value = df$DiGas)
      updateNumericInput(session, 'DfGas', value = df$DfGas)
      updateNumericInput(session, 'nri', value = df$nri)
      updateNumericInput(session, 'capex', value = df$capex)
      updateNumericInput(session, 'pna', value = df$pna)
      updateNumericInput(session, 'spudToProd', value = df$spudToProd)
      updateNumericInput(session, 'wellLife', value = df$wellLife)
      updateNumericInput(session, 'oilDiff', value = df$oilDiff)
      updateNumericInput(session, 'gasDiff', value = df$gasDiff)
      updateNumericInput(session, 'nglDiff', value = df$nglDiff)
      updateNumericInput(session, 'btu', value = df$btu)
      updateNumericInput(session, 'fixed', value = df$fixed)
      updateNumericInput(session, 'varExpGas', value = df$varExpGas)
      updateNumericInput(session, 'varExpBOE', value = df$varExpBOE)
      updateNumericInput(session, 'stxOil', value = df$stxOil)
      updateNumericInput(session, 'stxGas', value = df$stxGas)
      updateNumericInput(session, 'oilSTX', value = df$oilSTX)
      updateNumericInput(session, 'gasSTX', value = df$gasSTX)
      updateNumericInput(session, 'atx', value = df$atx)

    }
    if(is.null(values$pdpList)||nrow(values$pdpList)==0){
      NULL
    } else {
      df <- values$pdpList %>% filter(operator %in% input$operator) %>%
        filter(id %in% input$subPlayList)
      updateNumericInput(session, 'oilPDP', value = df$oilPDP)
      updateNumericInput(session, 'gasPDP', value = df$gasPDP)
      updateNumericInput(session, 'nglPDP', value = df$nglPDP)
      updateNumericInput(session, 'pdpWells', value = df$pdpWells)
      updateNumericInput(session, 'decl1oil', value = df$decl1oil)
      updateNumericInput(session, 'decl2oil', value = df$decl2oil)
      updateNumericInput(session, 'decl3oil', value = df$decl3oil)
      updateNumericInput(session, 'decl4oil', value = df$decl4oil)
      updateNumericInput(session, 'decl5oil', value = df$decl5oil)
      updateNumericInput(session, 'decl6oil', value = df$decl6oil)
      updateNumericInput(session, 'decl7oil', value = df$decl7oil)
      updateNumericInput(session, 'decl1gas', value = df$decl1gas)
      updateNumericInput(session, 'decl2gas', value = df$decl2gas)
      updateNumericInput(session, 'decl3gas', value = df$decl3gas)
      updateNumericInput(session, 'decl4gas', value = df$decl4gas)
      updateNumericInput(session, 'decl5gas', value = df$decl5gas)
      updateNumericInput(session, 'decl6gas', value = df$decl6gas)
      updateNumericInput(session, 'decl7gas', value = df$decl7gas)
    }

  })
  # 
  # observe({
  #   if(is.na(input$drill2020)||is.na(input$drill2021)||is.na(input$drill2022)||is.na(input$drill2023)||
  #      is.na(input$drill2024)||is.na(input$drill2025)||is.na(input$drill2026)||is.na(input$drill2027)||
  #      is.na(input$drill2028)||is.na(input$drill2029)){
  #     NULL
  #   } else {
  #     if(input$drill2020 + input$drill2021 + input$drill2022 +input$drill2023 + input$drill2024 +
  #        input$drill2025 + input$drill2026 + input$drill2027 + input$drill2028 + input$drill2029 == 0){
  #       shinyjs::disable('addFcst')
  #     } else {
  #       shinyjs::enable('addFcst')
  #     }
  #   }
  # })
  #   
  observeEvent(input$addFcst, {
    if(is.null(values$subPlayList)||nrow(values$subPlayList)==0||is.null(values$price)){
      NULL
    } else {
      updateButton(session, 'addFcst', 'Calculating...', style = 'danger')
      dfx <- values$subPlayList
      price <- values$price
      price$Date <- paste0(year(price$DATE), '-01-', month(price$DATE))
      price$Date <- as.POSIXct(price$Date, format = '%Y-%d-%m')
      #print(head(dfx))
      #print(head(price))

      econSummary <- lapply(split(dfx, dfx[,'id']), function (df) tryCatch({
        fcst <- data.frame(Months = seq(0, df$wellLife*12-1, 1)
                           )

        fcst$Oil <- curtailed.q(arps.decline(
          df$qiOil*365, (df$DiOil), df$bOil, as.nominal(df$DfOil/100)),
          df$curtailOil/12.0, seq(0, df$wellLife-1/12, by = (1/12)))/12
        fcst$Gas <- curtailed.q(arps.decline(
          df$qiGas*365, (df$DiGas), df$bGas, as.nominal(df$DfGas/100)),
          df$curtailGas/12.0, seq(0, df$wellLife-1/12, by = (1/12)))/12

        #print(head(df))
        #print(head(fcst))

        fcst$NGL <- fcst$Gas*df$nglYield/1000
        fcst$SalesGas <- fcst$Gas * df$shrink/100
        #print(head(fcst))
        fcst$expense <- df$fixed + df$varExpGas*fcst$Gas +
          df$varExpBOE*(fcst$Oil+fcst$NGL)
        #print(head(fcst))
        fcst$capex <- 0
        #print(head(fcst))
        df2 <- fcst[1:df$spudToProd,]
        df2$Gas <- 0
        df2$Oil <- 0
        df2$NGL <- 0
        df2$SalesGas <- 0

        df2$expense <- 0

        df2$capex[1] <- df$capex
        #print(head(df2))
        fcst <- rbind(df2, fcst) %>% select(Months, Oil, Gas, NGL, SalesGas, expense, capex)
        fcst$Months <- seq(0, nrow(fcst)-1, 1)

        fcst2 <- data.frame(Months = seq(0, df$wellLife*12-1, 1),
                            Date = as.POSIXct('01-01-2020', format = '%m-%d-%Y')) %>%
          mutate(Date = Date %m+% months(Months))

        #startDate <- fcst2$Date[1] %m+% months(-1*df$spudToProd)


        fcst2$wells <- 0
        fcst2$wells[1:12] <- df$drill2020
        fcst2$wells[13:24] <- df$drill2021
        fcst2$wells[25:36] <- df$drill2022
        fcst2$wells[37:48] <- df$drill2023
        fcst2$wells[49:60] <- df$drill2024
        fcst2$wells[61:72] <- df$drill2025
        fcst2$wells[73:84] <- df$drill2026
        fcst2$wells[85:96] <- df$drill2027
        fcst2$wells[97:108] <- df$drill2028
        fcst2$wells[109:nrow(fcst2)] <- df$drill2029

        fcst2 <- fcst2 %>% mutate(cumWells = cumsum(wells)) %>%
          filter(cumWells <= df$wells)

        fcst2 <- as.data.frame(fcst2)
        #print(head(fcst2))
        #print(head(fcst))

        subPlay <- lapply(split(fcst2, fcst2[,'Date']), function (dfy) tryCatch({

          df1 <- fcst %>% mutate(Date = dfy$Date %m+% months(-1*df$spudToProd)) %>% mutate(Date = Date %m+% months(Months)) %>%
            left_join(price[c('Date', 'WTI', 'HH')]) %>% mutate(WTI = na.locf(WTI), HH = na.locf(HH))
          df1$wells <- dfy$wells
          df1$Oil <- df1$Oil*dfy$wells
          df1$Gas <- df1$Gas*dfy$wells
          df1$NGL <- df1$NGL*dfy$wells
          df1$SalesGas <- df1$SalesGas*dfy$wells
          df1$expense <- df1$expense*dfy$wells
          df1$capex <- df1$capex*dfy$wells
          df1$netOil <- df1$Oil*df$nri/100
          df1$netGas <- df1$SalesGas*df$nri/100
          df1$netNGL <- df1$NGL*df$nri/100

          df1$OilRevenue <- (df1$Oil *(df1$WTI-df$oilDiff))* df$nri/100
          df1$GasRevenue <- (df1$SalesGas*df$btu*(df1$HH)-df1$SalesGas*df$gasDiff)*df$nri/100
          df1$NGLRevenue <- (df1$NGL*df1$WTI*df$nglDiff/100)*df$nri/100
          df1$Revenue <- df1$OilRevenue + df1$GasRevenue + df1$NGLRevenue

          df1$tax <- df1$OilRevenue*df$stxOil/100 + (df1$GasRevenue+df1$NGLRevenue)*df$stxGas/100 +
            df1$Oil*df$nri/100*df$oilSTX + df1$Gas*df$nri/100*df$gasSTX +
            df1$Revenue*df$atx/100
          df1$NOCF <- df1$Revenue - df1$expense - df1$tax
          df1 <- df1 %>% arrange(desc(Date))
          min1 <- which(df1$NOCF >0)
          min1 <- min1[1]
          df1 <- df1[min1:nrow(df1),]
          df1 <- df1 %>% arrange(Date)
          df1$capex[nrow(df1)] <- df$pna

          df1
        },
        error = function(e) {
          e
          NULL
        }))

        cfFcst <- dplyr::bind_rows(subPlay) %>% mutate(id = df$id)

        cfFcst

      },
      error = function(e) {
        e
        NULL
      }))

      cfSummary <- dplyr::bind_rows(econSummary)
      #print(head(cfSummary))
      cfSummary <-  as.data.frame(cfSummary %>% group_by(id, Date) %>% summarise(Oil = sum(Oil), Gas = sum(Gas),
                                                                                 NGL = sum(NGL),
                                                         SalesGas = sum(SalesGas), NetOil=sum(netOil),
                                                         NetGas = sum(netGas), NetNGL = sum(netNGL),
                                                         expense = sum(expense),
                                                         capex = sum(capex), WTI = mean(WTI), HH = mean(HH),
                                                         wells = sum(wells), OilRevenue = sum(OilRevenue),
                                                         GasRevenue = sum(GasRevenue), NGLRevenue=sum(NGLRevenue),
                                                         Revenue=sum(Revenue), tax = sum(tax), NOCF = sum(NOCF),
                                                         FCF = NOCF-capex) %>% arrange(id, Date))
      values$cfSummary <- cfSummary
      updateButton(session, 'addFcst', 'Load Forecast', style = 'primary')
    }
  })
  # 
  output$devTotal <- renderHighchart({
    if(is.null(values$cfSummary)){
      NULL
    } else {
      df1 <- values$cfSummary
      names(df1) <- c('id', 'Date', 'Oil', 'Gas', 'NGL', 'Sales Gas', 'Net Oil', 'Net Gas', 'Net NGL',
                      'Expenses', 'Capex','WTI', 'HH', 'Wells', 'Oil Revenue',
      'Gas Revenue', 'NGL Revenue', 'Revenue', 'Taxes', 'NOCF', 'FCF')

      df1 <- df1 %>% select(id, Date, input$selected) %>% filter(year(Date) >= 2020)
      names(df1) <- c('id', 'Date', 'metric')
      df1 <- df1 %>% group_by(Date) %>% summarise(metric = sum(metric, na.rm=TRUE))
      df1$metric <- as.integer(df1$metric)
      df1$Date <- as.Date(df1$Date)
      df1 <- as.data.frame(df1)
      #print(head(df1))

      if(input$selected == 'Oil'||input$selected == 'Net Oil'){
        cols <- c('#027971')
      } else if(input$selected == 'Gas'||input$selected == 'Net Gas'||input$selected == 'Sales Gas'){
        cols <- c('#ff4e50')
      } else {
        cols <- c('#eaa814')
      }
      #}
      #cols <- c('#027971','#ff4e50', '#eaa814', '#5c1848', '#786592', '#ff4e50',  '#008542', '#5c6d00')


      highchart() %>%
        hc_colors(cols) %>%
        hc_xAxis(type = 'datetime', title = list(text = '<b>Date</b>')) %>%
        hc_yAxis(title = list(text = "<b>Monthly Value in BBLS/MCF/$</b>"),opposite=FALSE) %>%
        hc_add_series(df1, type = "line",
                      hcaes(x = Date, y = metric), name = input$selected) %>%
        hc_title(text = 'Development Forecast', align = 'left') %>%
        hc_subtitle(text = 'Wood Mackenzie Corporate Service', align = 'left') %>%
        hc_credits(enabled = TRUE, text = "Wood Mackenzie", src = "http://www.woodmac.com")
    }


  })
  # 
  output$pdpTotal <- renderHighchart({
    if(is.null(values$pdpFcst)){
      NULL
    } else {
      df1 <- values$pdpFcst
      names(df1) <- c('Date', 'Oil', 'Gas', 'NGL', 'Sales Gas', 'Net Oil', 'Net Gas', 'Net NGL',
        'Expenses', 'Capex','WTI', 'HH', 'Oil Revenue',
        'Gas Revenue', 'NGL Revenue', 'Revenue', 'Taxes', 'NOCF', 'FCF', 'id')

      df1 <- df1 %>% select(id, Date, input$selectedPDP) %>% filter(year(Date) >= 2020)
      names(df1) <- c('id', 'Date', 'metric')
      df1 <- df1 %>% group_by(Date) %>% summarise(metric = sum(metric, na.rm=TRUE))
      df1$metric <- as.integer(df1$metric)
      df1$Date <- as.Date(df1$Date)
      df1 <- as.data.frame(df1)
      #print(head(df1))

      if(input$selectedPDP == 'Oil'||input$selectedPDP == 'Net Oil'){
        cols <- c('#027971')
      } else if(input$selectedPDP == 'Gas'||input$selectedPDP == 'Net Gas'||input$selectedPDP == 'Sales Gas'){
        cols <- c('#ff4e50')
      } else {
        cols <- c('#eaa814')
      }
      #}
      #cols <- c('#027971','#ff4e50', '#eaa814', '#5c1848', '#786592', '#ff4e50',  '#008542', '#5c6d00')


      highchart() %>%
        hc_colors(cols) %>%
        hc_xAxis(type = 'datetime', title = list(text = '<b>Date</b>')) %>%
        hc_yAxis(title = list(text = "<b>Monthly Value in BBLS/MCF/$</b>"),opposite=FALSE) %>%
        hc_add_series(df1, type = "line",
                      hcaes(x = Date, y = metric), name = input$selectedPDP) %>%
        hc_title(text = 'PDP Forecast', align = 'left') %>%
        hc_subtitle(text = 'Wood Mackenzie Corporate Service', align = 'left') %>%
        hc_credits(enabled = TRUE, text = "Wood Mackenzie", src = "http://www.woodmac.com")
    }


  })
  # 
  output$allTotal <- renderHighchart({
    if(is.null(values$cfSummary)||is.null(values$pdpFcst)){
      NULL
    } else {
      df1 <- values$cfSummary
      names(df1) <- c('id', 'Date', 'Oil', 'Gas', 'NGL', 'Sales Gas', 'Net Oil', 'Net Gas', 'Net NGL',
                      'Expenses', 'Capex','WTI', 'HH', 'Wells', 'Oil Revenue',
                      'Gas Revenue', 'NGL Revenue', 'Revenue', 'Taxes', 'NOCF', 'FCF')
      df1 <- subset(df1, select = -c(Wells))

      df2 <- values$pdpFcst
      names(df2) <- c('Date', 'Oil', 'Gas', 'NGL', 'Sales Gas', 'Net Oil', 'Net Gas', 'Net NGL',
                      'Expenses', 'Capex','WTI', 'HH', 'Oil Revenue',
                      'Gas Revenue', 'NGL Revenue', 'Revenue', 'Taxes', 'NOCF', 'FCF', 'id')
      df2 <- df2[,names(df1)]
      df1 <- rbind(df1, df2)

      df1 <- df1 %>% select(id, Date, input$selectedAll) %>% filter(year(Date) >= 2020)
      names(df1) <- c('id', 'Date', 'metric')
      df1 <- df1 %>% group_by(Date) %>% summarise(metric = sum(metric, na.rm=TRUE))
      df1$metric <- as.integer(df1$metric)
      df1$Date <- as.Date(df1$Date)
      df1 <- as.data.frame(df1)
      #print(head(df1))

      if(input$selectedAll == 'Oil'||input$selectedAll == 'Net Oil'){
        cols <- c('#027971')
      } else if(input$selectedAll == 'Gas'||input$selectedAll == 'Net Gas'||input$selectedAll == 'Sales Gas'){
        cols <- c('#ff4e50')
      } else {
        cols <- c('#eaa814')
      }
      #}
      #cols <- c('#027971','#ff4e50', '#eaa814', '#5c1848', '#786592', '#ff4e50',  '#008542', '#5c6d00')


      highchart() %>%
        hc_colors(cols) %>%
        hc_xAxis(type = 'datetime', title = list(text = '<b>Date</b>')) %>%
        hc_yAxis(title = list(text = "<b>Monthly Value in BBLS/MCF/$</b>"),opposite=FALSE) %>%
        hc_add_series(df1, type = "line",
                      hcaes(x = Date, y = metric), name = input$selectedAll) %>%
        hc_title(text = 'PDP+Dev Forecast', align = 'left') %>%
        hc_subtitle(text = 'Wood Mackenzie Corporate Service', align = 'left') %>%
        hc_credits(enabled = TRUE, text = "Wood Mackenzie", src = "http://www.woodmac.com")

    }
  })
  # 
  output$pdpPV <- renderText({
    if(is.null(values$pdpFcst)){
      NULL
    } else {
      df1 <- values$pdpFcst %>% filter(year(Date) > 2019)


      df1 <- df1[,c('Date', 'FCF')] %>% group_by(Date) %>% summarise_all(sum) %>% ungroup()
      df1$Months <- seq(0, nrow(df1)-1, 1)
      df1$PV <- df1$FCF/(1+input$pdpDiscRate/100)^(df1$Months/12)
      paste0('PV-',input$pdpDiscRate,': US ', dollar(as.integer(sum(df1$PV)/1000)), " thousands")
    }


  })
  # 
  output$pudPV <- renderText({
    if(is.null(values$cfSummary)){
      NULL
    } else {
      df1 <- values$cfSummary %>% filter(year(Date) > 2019)


      df1 <- df1[,c('Date', 'FCF')] %>% group_by(Date) %>% summarise_all(sum) %>% ungroup()
      df1$Months <- seq(0, nrow(df1)-1, 1)
      df1$PV <- df1$FCF/(1+input$pudDiscRate/100)^(df1$Months/12)
      paste0('PV-',input$pudDiscRate,': US ', dollar(as.integer(sum(df1$PV)/1000)), " thousands")
    }


  })

  output$allPV <- renderText({
    if(is.null(values$cfSummary)||is.null(values$pdpFcst)){
      NULL
    } else {
      df1 <- values$cfSummary %>% filter(year(Date) > 2019) %>% select(Date, FCF)
      df2 <- values$pdpFcst %>% filter(year(Date) > 2019) %>% select(Date, FCF)
      df1 <- rbind(df1, df2) %>% group_by(Date) %>% summarise_all(sum) %>% ungroup()
      df1$Months <- seq(0, nrow(df1)-1, 1)
      df1$PV <- df1$FCF/(1+input$allDiscRate/100)^(df1$Months/12)
      paste0('PV-',input$allDiscRate,': US ', dollar(as.integer(sum(df1$PV)/1000)), " thousands")
    }


  })


  output$allTable <- DT::renderDataTable({
    if(is.null(values$pdpFcst)||is.null(values$cfSummary)){
      NULL
    } else {
      df1 <- values$pdpFcst
      df2 <- values$cfSummary

      df1$Year <- year(df1$Date)
      df2$Year <- year(df2$Date)

      df1 <- df1 %>% group_by(Year) %>% summarise(Oil = as.integer(sum(Oil)/1000), Gas = as.integer(sum(Gas)/1000), NGL = as.integer(sum(NGL)/1000),
                                                  SalesGas = as.integer(sum(SalesGas)/1000), netOil = as.integer(sum(netOil)/1000),
                                                  netGas = as.integer(sum(netGas)/1000), netNGL = as.integer(sum(netNGL)/1000),
                                                  OilRevenue = as.integer(sum(OilRevenue)/1000),
                                                  GasRevenue = as.integer(sum(GasRevenue)/1000), NGLRevenue = as.integer(sum(NGLRevenue)/1000),
                                                  Revenue = as.integer(sum(Revenue)/1000), expense = as.integer(sum(expense)/1000), tax = as.integer(sum(tax)/1000),
                                                  NOCF = as.integer(sum(NOCF)/1000), capex = as.integer(sum(capex)/1000), FCF = as.integer(sum(FCF)/1000)) %>%
        ungroup() %>% arrange(Year) %>% filter(Year > 2019)

      df2 <- df2 %>% group_by(Year) %>% summarise(Oil = as.integer(sum(Oil)/1000), Gas = as.integer(sum(Gas)/1000), NGL = as.integer(sum(NGL)/1000),
                                                  SalesGas = as.integer(sum(SalesGas)/1000), netOil = as.integer(sum(NetOil)/1000),
                                                  netGas = as.integer(sum(NetGas)/1000), netNGL = as.integer(sum(NetNGL)/1000),
                                                  OilRevenue = as.integer(sum(OilRevenue)/1000),
                                                  GasRevenue = as.integer(sum(GasRevenue)/1000), NGLRevenue = as.integer(sum(NGLRevenue)/1000),
                                                  Revenue = as.integer(sum(Revenue)/1000), expense = as.integer(sum(expense)/1000), tax = as.integer(sum(tax)/1000),
                                                  NOCF = as.integer(sum(NOCF)/1000), capex = as.integer(sum(capex)/1000), FCF = as.integer(sum(FCF)/1000)) %>%
        ungroup() %>% arrange(Year) %>% filter(Year > 2019)
      df1 <- rbind(df1, df2)
      names(df1) <- c('Year', 'Oil, mbbls', 'Gas, mmcf', 'NGL, mbbls',
                      'Sales Gas, mmcf', 'Net Oil, mbbls', 'Net Gas, mmcf', 'Net NGL, mbbls',
                      'Oil Revenue, $000s', 'Gas Revenue, $000s',
                      'NGL Revenue, $000s', 'Total Revenue, $000s', 'Expenses, $000s', 'Taxes, $000s',
                      'NOCF, $000s', 'Capex, $000s', 'Free Cash Flow, $000s')
      df1 <- df1 %>% group_by(Year) %>% summarise_all(sum) %>% ungroup() %>% arrange(Year)

      DT::datatable(df1, rownames = FALSE,
                    extensions = c('Buttons', 'Scroller'),
                    options = list(
                      dom = 'Bfrtip',
                      scrollX = TRUE,
                      scrollY = FALSE,
                      deferRender = TRUE,
                      paging = FALSE,
                      searching = FALSE,
                      buttons = c(I('colvis'),'copy', 'csv', 'excel', 'pdf', 'print')
                    ))
    }
  })
  # 
  output$pdpTable <- DT::renderDataTable({
    if(is.null(values$pdpFcst)){
      NULL
    } else {
      df1 <- values$pdpFcst


      df1$Year <- year(df1$Date)

      df1 <- df1 %>% group_by(Year) %>% summarise(Oil = as.integer(sum(Oil)/1000), Gas = as.integer(sum(Gas)/1000), NGL = as.integer(sum(NGL)/1000),
                                                  SalesGas = as.integer(sum(SalesGas)/1000), netOil = as.integer(sum(netOil)/1000),
                                                  netGas = as.integer(sum(netGas)/1000), netNGL = as.integer(sum(netNGL)/1000),
                                                  OilRevenue = dollar(as.integer(sum(OilRevenue)/1000)),
                                                  GasRevenue = dollar(as.integer(sum(GasRevenue)/1000)), NGLRevenue = dollar(as.integer(sum(NGLRevenue)/1000)),
                                                  Revenue = dollar(as.integer(sum(Revenue)/1000)), expense = dollar(as.integer(sum(expense)/1000)), tax = dollar(as.integer(sum(tax)/1000)),
                                                  NOCF = dollar(as.integer(sum(NOCF)/1000)), capex = dollar(as.integer(sum(capex)/1000)), FCF = dollar(as.integer(sum(FCF)/1000))) %>%
        ungroup() %>% arrange(Year) %>% filter(Year > 2019)
      
      names(df1) <- c('Year', 'Oil, mbbls', 'Gas, mmcf', 'NGL, mbbls',
                      'Sales Gas, mmcf', 'Net Oil, mbbls', 'Net Gas, mmcf', 'Net NGL, mbbls',
                      'Oil Revenue, $000s', 'Gas Revenue, $000s',
                      'NGL Revenue, $000s', 'Total Revenue, $000s', 'Expenses, $000s', 'Taxes, $000s',
                      'NOCF, $000s', 'Capex, $000s', 'Free Cash Flow, $000s')

      DT::datatable(df1, rownames = FALSE,
                    extensions = c('Buttons', 'Scroller'),
                    options = list(
                      dom = 'Bfrtip',
                      scrollX = TRUE,
                      scrollY = FALSE,
                      deferRender = TRUE,
                      paging = FALSE,
                      searching = FALSE,
                      buttons = c(I('colvis'),'copy', 'csv', 'excel', 'pdf', 'print')
                    ))
    }
  })
  # 
  output$devTable <- DT::renderDataTable({
    if(is.null(values$cfSummary)){
      NULL
    } else {
      df1 <- values$cfSummary


      df1$Year <- year(df1$Date)

      df1 <- df1 %>% group_by(Year) %>% summarise(Oil = as.integer(sum(Oil)/1000), Gas = as.integer(sum(Gas)/1000), NGL = as.integer(sum(NGL)/1000),
                                                  SalesGas = as.integer(sum(SalesGas)/1000), netOil = as.integer(sum(NetOil)/1000),
                                                  netGas = as.integer(sum(NetGas)/1000), netNGL = as.integer(sum(NetNGL)/1000),
                                                  OilRevenue = dollar(as.integer(sum(OilRevenue)/1000)),
                                                  GasRevenue = dollar(as.integer(sum(GasRevenue)/1000)), NGLRevenue = dollar(as.integer(sum(NGLRevenue)/1000)),
                                                  Revenue = dollar(as.integer(sum(Revenue)/1000)), expense = dollar(as.integer(sum(expense)/1000)), tax = dollar(as.integer(sum(tax)/1000)),
                                                  NOCF = dollar(as.integer(sum(NOCF)/1000)), capex = dollar(as.integer(sum(capex)/1000)), FCF = dollar(as.integer(sum(FCF)/1000))) %>%
        ungroup() %>% arrange(Year) %>% filter(Year > 2019)

      names(df1) <- c('Year', 'Oil, mbbls', 'Gas, mmcf', 'NGL, mbbls',
                      'Sales Gas, mmcf', 'Net Oil, mbbls', 'Net Gas, mmcf', 'Net NGL, mbbls',
                      'Oil Revenue, $000s', 'Gas Revenue, $000s',
                      'NGL Revenue, $000s', 'Total Revenue, $000s', 'Expenses, $000s', 'Taxes, $000s',
                      'NOCF, $000s', 'Capex, $000s', 'Free Cash Flow, $000s')

      DT::datatable(df1, rownames = FALSE,
                    extensions = c('Buttons', 'Scroller'),
                    options = list(
                      dom = 'Bfrtip',
                      scrollX = TRUE,
                      scrollY = FALSE,
                      deferRender = TRUE,
                      paging = FALSE,
                      searching = FALSE,
                      buttons = c(I('colvis'),'copy', 'csv', 'excel', 'pdf', 'print')
                    ))
    }
  })
  
  observeEvent(input$operator1, {
    if(is.null(input$operator1)||input$operator1 == ''){
      NULL
    } else {
      df1 <- opList %>% filter(operator %in% input$operator1)
      updatePickerInput(session, 'plays',  choices = sort(unique(df1$id)), selected = sort(unique(df1$id))[1])
      # df1 <-leases() %>% filter(operator %in% input$operator1)
      # #print(head(df1))
      # if(nrow(df1)==0){
      #   df1 <-df() %>% filter(operator %in% input$operator1)
      # }
      # print(1)
      # updateRadioButtons(session, 'subPlay1', choices = sort(unique(df1$subPlay)))
      # print(1.1)
    }
  })
  
  observeEvent(input$plays, {
    if(is.null(input$plays)||input$plays == ''||is.null(input$operator1)||input$operator1 == ''){
      NULL
    } else {
      df1 <-leases() %>% filter(operator %in% input$operator1)
      #print(2.1)
      if(nrow(df1)==0){
        df1 <-df() %>% filter(operator %in% input$operator1)
      }
      #print(2.2)
      updateRadioButtons(session, 'subPlay1', choices = sort(unique(df1$subPlay)))
    }
  })
  
  leases <- reactive({
    if(is.null(input$plays)||input$plays == ''){
      NULL
    } else {
      #print(head(as.data.frame(wellData[[input$plays]]$leases)))
      as.data.frame(wellData[[input$plays]]$leases)
    }
  })
  
  df <- reactive({
    if(is.null(input$plays)||input$plays == ''){
      NULL
    } else {
      #print(head(as.data.frame(wellData[[input$plays]]$df)))
      as.data.frame(wellData[[input$plays]]$df)
    }
  })
  
  ogip <- reactive({
    if(is.null(input$plays)||input$plays == ''){
      NULL
    } else {
      #print(head(as.data.frame(wellData[[input$plays]]$ogip)))
      as.data.frame(wellData[[input$plays]]$ogip)
    }
  })
  
  df2 <- reactive(
    if(is.null(input$plays)||input$plays == ''){
      NULL
    } else {
      df() %>% filter(operator %in% input$operator1)
    }
  )
  
  output$acres_plot <- renderHighchart({
    if(is.null(input$plays)||input$plays == ''){
      NULL
    } else {
    df <- leases() %>% filter(operator %in% input$operator1) %>% filter(!duplicated(oneSecLocation)) %>%
      group_by(subPlay) %>% summarise(acres = n()*640) %>% ungroup() %>% select(subPlay, acres) %>% filter(!is.na(subPlay))
    
    print(head(df))
    if(nrow(df) == 0){
      NULL
    } else {
        totalAcres <- sum(df$acres)
        df <- df %>%
          mutate(acres = round(acres/sum(acres)*100,1))
        #print(head(df))
        cols <- c( '#00a4e3', '#adafb2','#0D1540','#06357a',  '#a31c37', '#d26400', '#eaa814',
                   '#5c1848', '#786592', '#ff4e50', '#027971', '#008542', '#5c6d00')
        highchart() %>%
          hc_add_series(df, 'pie', hcaes(name = subPlay, y = acres), name = 'Percent')  %>%
          hc_plotOptions(
            series = list(
              showInLegend = FALSE,
              pointFormat = "{point.y}%"
            )) %>%
          hc_title(
            text = paste0("Percent of Total Acreage By Subplay, Total Acres: <span style=\"color:#00a4e3\">",totalAcres,"</span>"),
            useHTML = TRUE) %>%
          hc_subtitle(text = '<a href="https://www.woodmac.com/research/lens/lens-direct/">Source: P2</a>', useHTML=TRUE, align = 'right') %>%
          hc_tooltip(table = TRUE, sort = TRUE) %>%
          #hc_xAxis(title = list(text = '<b>First Production Year</b>')) %>%
          #hc_yAxis(title = list(text = '<b>Six Month BOE Per Ft (20:1 Gas, 25% NGL)</b>')) %>%
          hc_credits(
            enabled = TRUE,
            text = "Powered by Highcharts",
            href = "https://www.highcharts.com/") %>%
          hc_colors(cols) %>%
          hc_exporting(enabled = TRUE, filename = 'acresBySubplay')
    }
    }
  })
  
  output$percentDev <- renderHighchart({
    df1 <- df() %>% filter(subPlay %in% input$subPlay1) %>% group_by(oneSecLocation) %>% summarise(wells = n()) %>% ungroup()
    if(nrow(df1) == 0){
      value1 <- 0
    } else {
      leases2 <- leases() %>% filter(operator %in% input$operator1) %>% filter(subPlay %in% input$subPlay1) %>% merge(df1, by='oneSecLocation', all.x=TRUE)
      #head(leases2)
      leases3 <- leases2 %>% filter(is.na(wells))
      leases2 <- leases2 %>% filter(!is.na(wells))
      
      leases3 <- leases3 %>% group_by(oneSecLocation, subPlay) %>% summarise(count=n()) %>% ungroup() %>% group_by(subPlay) %>% summarise(count = sum(count)) %>% ungroup()
      leases2 <- leases2 %>% group_by(oneSecLocation, subPlay) %>% summarise(count=n()) %>% ungroup() %>% group_by(subPlay) %>% summarise(count1 = sum(count)) %>% ungroup()
      leases2 <- merge(leases2, leases3, by='subPlay', all.x=TRUE)
      leases2$total <- leases2$count+leases2$count1
      leases2$percentDev <- leases2$count1/leases2$total*100
      value1 <- as.integer(leases2$percentDev)
      
    }
    cols <- c( '#00a4e3', '#adafb2','#0D1540','#06357a',  '#a31c37', '#d26400', '#eaa814',
               '#5c1848', '#786592', '#ff4e50', '#027971', '#008542', '#5c6d00')
    highchart() %>%
      hc_chart(type = 'solidgauge') %>%
      hc_add_series(data = value1, name = 'Percent Developed') %>%
      hc_pane(startAngle = -90,
              endAngle = 90,background= list(
                outerRadius = '100%',
                innerRadius = '60%',
                # backgroundColor = JS("Highcharts.Color('#9DFF02').setOpacity(0.1).get()"),
                shape="arc" ))%>%
      
      hc_title(
        text = "Developed Acreage Percent",
        useHTML = TRUE) %>%
      hc_subtitle(text = '<a href="https://www.woodmac.com/research/lens/lens-direct/">Source: Wood Mackenzie Lens Direct</a>', useHTML=TRUE, align = 'right') %>%
      hc_tooltip() %>%
      #hc_xAxis(title = list(text = '<b>First Production Year</b>')) %>%
      hc_yAxis(min = 0, max = 100) %>%
      hc_credits(
        enabled = TRUE,
        text = "Powered by Highcharts",
        href = "https://www.highcharts.com/") %>%
      hc_colors(cols)%>%
      hc_exporting(enabled = TRUE, filename = 'percentDeveloped')
    
    # billboarder() %>%
    #   bb_gaugechart(value = value1, color = "#00a4e3") %>%
    #   bb_legend(show = FALSE) %>% 
    #   bb_labs(title = "Percent of Acreage with at least 1 producing well",
    #           caption = "Data source: Lens and P2 Data")
  })
  
  observeEvent(input$subPlay1, {
    df2 <- leases() %>% filter(operator %in% input$operator1) %>% 
      filter(subPlay %in% input$subPlay1) %>% filter(!duplicated(oneSecLocation))
    #print(head(df1))
    updateNumericInput(session, 'acres1', value = nrow(df2)*640)
    df1 <- df() %>% filter(subPlay %in% input$subPlay1)
    #print(head(df1))
    updateNumericInput(session, 'wellSpacing1', value = as.integer(mean(df1$wellsPerSection))-1)
    updateNumericInput(session, 'shrink1', value = (mean(df1$shrink)))
    updateNumericInput(session, 'nglYield1', value = (mean(df1$nglYield)))
    updateNumericInput(session, 'fixedCost1', value = as.integer(mean(df1$finalFixed)))
    updateNumericInput(session, 'varExpGas1', value = (mean(df1$varExpGas)))
    updateNumericInput(session, 'varExpBOE1', value = (mean(df1$varExpBOE)))
    updateNumericInput(session, 'gasDiff1', value = (mean(df1$gasDiff)))
    updateNumericInput(session, 'oilDiff1', value = (mean(df1$oilDiff)))
    updateNumericInput(session, 'pna1', value = as.integer(mean(df1$pna)))
    updateNumericInput(session, 'btu1', value = (mean(df1$btu)))
    
    updateNumericInput(session, 'tvdSelect1', value = as.integer(mean(df2$tvd, na.rm=TRUE)))
    df3 <- df1 %>% filter(operator %in% input$operator1)
    df3 <- df3 %>%  filter(fp.year == max(df3$fp.year))
    if(nrow(df3) > 0){
      updateNumericInput(session, 'perfSelect1', value = as.integer(mean(df3$perf, na.rm=TRUE)))
      updateNumericInput(session, 'ppfSelect1', value = as.integer(mean(df3$ppf, na.rm=TRUE)))
      updateNumericInput(session, 'fpfSelect1', value = as.integer(mean(df3$fpf, na.rm=TRUE)))
    }
    
    shinyjs::disable('calcInv')
    values$qiGas <- NULL
    values$qiOil <- NULL
    
  })
  
  capexValues1 <- reactive({
    data.frame(
      Component = c('capexStressor', 'tvdSelect', 'drillSpeed', 'stagesPerDay', 'proppantPerStage', 'padConstruction',
                    'drillingFluidsPerDay', 'drillingFuelPerDay', 'ancDrillPerDay', 'chemsPerStage', 'pumpFuel', 'ancCompPerDay',
                    'brownSelect', 'ceramicSelect', 'rcsSelect', 'northernSelect', 'perfSelect', 'ppfSelect', 'fpfSelect'),
      Value = c(input$capexStressor1, input$tvdSelect1, input$drillSpeed1, input$stagesPerDay1, input$proppantPerStage1, input$padConstruction1,
                input$drillingFluidsPerDay1, input$drillingFuelPerDay1, input$ancDrillPerDay1, input$chemsPerStage1, input$pumpFuel1, input$ancCompPerDay1,
                input$brownSelect1, input$ceramicSelect1, input$rcsSelect1, input$northernSelect1, input$perfSelect1, input$ppfSelect1, input$fpfSelect1),
      stringsAsFactors = FALSE) %>% spread(Component, Value)
  })
  
  expenseValues1 <- reactive({
    data.frame(
      Component = c('acres', 'wellSpacing', 'pna', 'wiPDP', 'nri', 'shrink', 
                    'nglYield', 'btu', 'fixedCost', 'varExpGas', 'varExpBOE', 'stxOil',
                    'stxGas', 'oilSTX', 'gasSTX', 'atx', 'oilDiff', 'gasDiff', 'nglDiff'),
      
      Value = c(input$acres1, input$wellSpacing1, input$pna1, input$wiPDP1, input$nri1, input$shrink1,
                input$nglYield1, input$btu1, input$fixedCost1, input$varExpGas1, input$varExpBOE1,
                input$stxOil1, input$stxGas1, input$oilSTX1, input$gasSTX1, input$atx1,
                input$oilDiff1, input$gasDiff1, input$nglDiff1),
      stringsAsFactors = FALSE) %>% spread(Component, Value)
    
  })
  
  observe({
    #values$dd2 <- NULL
    if(is.null(input$subPlay1)){
      
      NULL
    } else {
      
      df <- df() %>% filter(subPlay %in% input$subPlay1)
      df <- as.data.frame(costData)%>%
        filter(subBasin %in% df$subBasin)
      
      x <- (as.integer(mean(df$drillSpeed, na.rm=TRUE)))
      y <- (as.integer(mean(df$stagesPerDay, na.rm=TRUE)))
      
      
      
      #names(df) <- 'x'
      
      # Can use character(0) to remove all choices
      if (is.null(x))
        x <- 620
      if (is.null(y))
        y <- 5
      
      
      
      # Can also set the label and select items
      updateNumericInput(session, "drillSpeed1", label = 'Drill Speed, ft/day',
                         x
      )
      
      updateNumericInput(session, "stagesPerDay1", label = 'Frac Stages per Day',
                         y
      )
      
     
    }
    
    
    
    
  })
  
  observeEvent(input$subPlay1, {
    if(is.null(input$subPlay1)||input$subPlay1 == ''){
      
      NULL
    } else {
      df1 <- df() %>% filter(subPlay %in% input$subPlay1)
      updatePickerInput(session, 'selectYr',  choices = sort(unique(as.character(df1$fp.year))), selected = sort(unique(as.character(df1$fp.year)))[1])
      updatePickerInput(session, 'operatorSelect',  choices = sort(unique(df1$operator)), selected  = sort(unique(df1$operator))[1])
    }
  })
  
  observeEvent(input$selectYr, {
    if(is.null(input$subPlay1)||input$subPlay1 == ''||is.null(input$selectYr)||input$selectYr == ''){

      NULL
    } else {
      df1 <- df() %>% filter(subPlay %in% input$subPlay1) %>% filter(as.character(fp.year) %in% (input$selectYr))
      #updatePickerInput(session, 'selectYr',  choices = sort(unique(as.character(df1$fp.year))), selected = sort(unique(as.character(df1$fp.year)))[1])
      updatePickerInput(session, 'operatorSelect',  choices = sort(unique(df1$operator)), selected  = sort(unique(df1$operator))[1])
    }
  })
  
  output$capexCalcs <- render_tableHTML({
    if(is.null(input$subPlay1)){
      
      
      NULL
    } else {
      #capexValues <- capexValues()
      #print(head(capexValues))
      df <- df() %>% filter(subPlay %in% input$subPlay1)
      df <- as.data.frame(costData)%>%
        filter(subBasin %in% df$subBasin)
      
      working1 <- df
      
      working1$md <- as.numeric(capexValues1()$tvdSelect) + as.numeric(capexValues1()$perfSelect)
      
      
      working1$cementing <- working1$md*8
      working1$proppant <- as.numeric(capexValues1()$ppfSelect)*as.numeric(capexValues1()$perfSelect)
      working1$fluid <- as.numeric(capexValues1()$fpfSelect)*as.numeric(capexValues1()$perfSelect)
      working1$proppantPerStage <- as.numeric(capexValues1()$proppantPerStage)
      working1$stages <- working1$proppant/working1$proppantPerStage
      working1$compDays <- working1$stages/(as.numeric(capexValues1()$stagesPerDay))
      working1$drillDays <- working1$md/as.numeric(capexValues1()$drillSpeed)
      
      working1 <- working1 %>% 
        mutate(drillCost = as.numeric(capexValues1()$tvdSelect)*sixteenCasingTVD*sixteenRate+
                 as.numeric(capexValues1()$tvdSelect)*thirteenCasingTVD*thirteenRate+
                 as.numeric(capexValues1()$tvdSelect)*nineCasingTVD*nineRate +
                 as.numeric(capexValues1()$tvdSelect)*sevenCasingTVD*sevenRate+
                 as.numeric(capexValues1()$tvdSelect)*fiveCasingTVD*fiveRate + 
                 md*fiveCasingTMD*fiveRate+md*fourCasingTMD*fourRate +
                 as.numeric(capexValues1()$tvdSelect)*tubingTVD*tubingRate+
                 as.numeric(capexValues1()$padConstruction)+
                 as.numeric(capexValues1()$drillingFluidsPerDay)*drillDays+
                 as.numeric(capexValues1()$drillingFuelPerDay)*drillDays+
                 as.numeric(capexValues1()$ancDrillPerDay)*drillDays+cementing+
                 rigRate*drillDays)
      
      working1$brownPercent <- as.numeric(capexValues1()$brownSelect)
      working1$ceramicPercent <- as.numeric(capexValues1()$ceramicSelect)
      working1$rcsPercent <- as.numeric(capexValues1()$rcsSelect)
      working1$northernPercent <- as.numeric(capexValues1()$northernSelect)
      
      working1 <- working1 %>%
        mutate(compCost = fleetRateComp*fleetRatePerHP*compDays+fluid*waterAcqRate/42 +
                 fluid*waterHaulingToRate/42+
                 fluid*waterTreatmentRate/42+
                 fluid*waterHaulingFromRate/42*waterFlowbackPercent +
                 fluid*waterDispRate*waterFlowbackPercent/42+
                 (brownPercent*sandRate+ceramicPercent*ceramicRate +
                    rcsPercent*rcsRate+northernPercent*northernRate)*proppant +
                 (brownPercent*sandTranspRate +ceramicPercent*ceramicTranspRate +
                    rcsPercent*rcsTranspRate+northernPercent*northernTranspRate)*proppant +
                 stages*as.numeric(capexValues1()$chemsPerStage)+
                 stages*as.numeric(capexValues1()$pumpFuel)+compDays*as.numeric(capexValues1()$ancCompPerDay))
      
      working1 <- working1 %>% mutate(facilities = (drillCost+compCost)/(1-eNt)*facil/2,
                                      equipment = (drillCost+compCost)/(1-eNt)*facil/2,
                                      pipeline = (drillCost+compCost)/(1-eNt)*tieIn)
      
      working1 <- working1 %>% mutate(dNc = drillCost+compCost, dcet = dNc + facilities + equipment + pipeline)
      working1$dNc <- working1$dNc*as.numeric(capexValues1()$capexStressor)
      working1$dcet <- working1$dcet*as.numeric(capexValues1()$capexStressor)
      working1 <- working1 %>% select(dNc, dcet)
      working1 <- as.data.frame(working1)
      working1$dNcPerFt <- dollar(as.integer(working1$dNc/as.numeric(capexValues1()$perfSelect)))
      working1$dcetPerFt <- dollar(as.integer(working1$dcet/as.numeric(capexValues1()$perfSelect)))
      working1$dNc <- dollar(as.integer(working1$dNc))
      working1$dcet <- dollar(as.integer(working1$dcet))
      
      names(working1) <- c('Drill & Complete, $', 'Drill, Complete, & Equip, $', 'D&C Per Ft, $/Ft', 'DC&E Per Ft, $/Ft')
      #print(head(working1))
      #working1
      table1 <- tableHTML(working1, rownames = FALSE, caption = 'Table: Cost Center of Excellence Calculation of Well Cost based on Lens Actuals') %>% 
        add_css_row(css = list(c('background-color', 'border'), c('white', '2px solid lightgray'))) %>%
        add_css_table(css = list('text-align', 'center'))
      
      #datatable((working1), rownames=FALSE, 
      #            escape = FALSE,extensions = c('Buttons', 'ColReorder', 'FixedHeader','KeyTable',  'Scroller'),
      #            options = list(dom = 'Bt', fixedHeader=TRUE, keys=TRUE,
      #                           deferRender = FALSE, scroller=TRUE,
      #                           scrollX='400px',
      #                           colReorder = TRUE,
      #                           buttons = c('copy')))
      
      table1
    }
  })
  
  output$wellDesign <- renderHighchart({
    df <- df2() %>% filter(subPlay %in% input$subPlay1)%>% 
      group_by(fp.year) %>% summarise(perf = mean(perf), ppf= mean(ppf), fpf=mean(fpf)) %>%
      ungroup() %>% gather(Component, Value, -c(fp.year)) %>% filter(Component == input$wellFactor) %>%
      arrange(fp.year) %>% select(fp.year, Value) %>% mutate(Value = as.integer(Value))
    if(nrow(df) == 0){
      NULL
    } else {
      if(input$wellFactor == 'perf'){
        # billboarder() %>%
        #   bb_barchart(data = df, color = "#00a4e3") %>%
        #   bb_y_grid(show = TRUE) %>%
        #   bb_y_axis(tick = list(format = suffix("ft")),
        #             label = list(text = "Lateral Length in Ft", position = "outer-top")) %>% 
        #   bb_legend(show = FALSE) %>% 
        #   bb_labs(title = "Average Lateral Length by Year",
        #           caption = "Data source: Lens Direct")
        cols <- c("#00a4e3")
        hchart(df, "column", hcaes(x = fp.year, y = Value), name = 'Lateral Length') %>%
          hc_title(
            text = "Average Lateral Length by Year",
            useHTML = TRUE) %>%
          hc_subtitle(text = '<a href="https://www.woodmac.com/research/lens/lens-direct/">Source: Wood Mackenzie Lens Direct</a>', useHTML=TRUE, align = 'right') %>%
          hc_tooltip(table = TRUE, sort = TRUE) %>%
          hc_xAxis(title = list(text = '<b>First Production Year</b>')) %>%
          hc_yAxis(title = list(text = '<b>Lateral Length, ft</b>')) %>%
          hc_credits(
            enabled = TRUE,
            text = "Powered by Highcharts",
            href = "https://www.highcharts.com/") %>%
          hc_colors(cols)%>%
          hc_exporting(enabled = TRUE, filename = 'perfData')
        
      } else if(input$wellFactor == 'ppf'){
        # billboarder() %>%
        #   bb_barchart(data = df, color = "#00a4e3") %>%
        #   bb_y_grid(show = TRUE) %>%
        #   bb_y_axis(tick = list(format = suffix("lb/ft")),
        #             label = list(text = "Proppant Loading, lb/ft", position = "outer-top")) %>% 
        #   bb_legend(show = FALSE) %>% 
        #   bb_labs(title = "Average Proppant Loading by Year",
        #           caption = "Data source: Lens Direct")
        
        cols <- c("#adafb2")
        hchart(df, "column", hcaes(x = fp.year, y = Value), name = 'Proppant Loading') %>%
          hc_title(
            text = "Average Proppant Loading by Year",
            useHTML = TRUE) %>%
          hc_subtitle(text = '<a href="https://www.woodmac.com/research/lens/lens-direct/">Source: Wood Mackenzie Lens Direct</a>', useHTML=TRUE, align = 'right') %>%
          hc_tooltip(table = TRUE, sort = TRUE) %>%
          hc_xAxis(title = list(text = '<b>First Production Year</b>')) %>%
          hc_yAxis(title = list(text = '<b>Proppant Loading, lb/ft</b>')) %>%
          hc_credits(
            enabled = TRUE,
            text = "Powered by Highcharts",
            href = "https://www.highcharts.com/") %>%
          hc_colors(cols)%>%
          hc_exporting(enabled = TRUE, filename = 'ppfData')
      } else {
        # billboarder() %>%
        #   bb_barchart(data = df, color = "#00a4e3") %>%
        #   bb_y_grid(show = TRUE) %>%
        #   bb_y_axis(tick = list(format = suffix("gal/ft")),
        #             label = list(text = "Fluid Loading, gal/ft", position = "outer-top")) %>% 
        #   bb_legend(show = FALSE) %>% 
        #   bb_labs(title = "Average Fluid Loading by Year",
        #           caption = "Data source: Lens Direct")
        
        cols <- c("#0D1540")
        hchart(df, "column", hcaes(x = fp.year, y = Value), name = 'Fluid Loading') %>%
          hc_title(
            text = "Average Fluid Loading by Year",
            useHTML = TRUE) %>%
          hc_subtitle(text = '<a href="https://www.woodmac.com/research/lens/lens-direct/">Source: Wood Mackenzie Lens Direct</a>', useHTML=TRUE, align = 'right') %>%
          hc_tooltip(table = TRUE, sort = TRUE) %>%
          hc_xAxis(title = list(text = '<b>First Production Year</b>')) %>%
          hc_yAxis(title = list(text = '<b>Fluid Loading, gal/ft</b>')) %>%
          hc_credits(
            enabled = TRUE,
            text = "Powered by Highcharts",
            href = "https://www.highcharts.com/") %>%
          hc_colors(cols)%>%
          hc_exporting(enabled = TRUE, filename = 'fpfData')
      }
    }
    
    
  })
  
  output$costPerFt <- renderHighchart({
    if(is.null(input$subPlay1)){
      
      
      NULL
    } else {
      #capexValues1 <- capexValues1()
      #print(head(capexValues1))
      df <- df() %>% filter(subPlay %in% input$subPlay1)
      df <- as.data.frame(costData)%>%
        filter(subBasin %in% df$subBasin)
      
      working1 <- df
      
      costCalc <- data.frame(perf = c(capexValues1()$perfSelect, 2500, 4500, 7500, 9000, 12500, 15000))
      costCalc <- costCalc %>% filter(!duplicated(perf))
      costCalc$count <- 1
      
      econSummary <- lapply(split(costCalc, costCalc[,'perf']), function (well) tryCatch({
        working1$md <- as.numeric(capexValues1()$tvdSelect) + as.numeric(well$perf)
        
        
        working1$cementing <- working1$md*8
        working1$proppant <- as.numeric(capexValues1()$ppfSelect)*as.numeric(well$perf)
        working1$fluid <- as.numeric(capexValues1()$fpfSelect)*as.numeric(well$perf)
        working1$proppantPerStage <- as.numeric(capexValues1()$proppantPerStage)
        working1$stages <- working1$proppant/working1$proppantPerStage
        working1$compDays <- working1$stages/(as.numeric(capexValues1()$stagesPerDay))
        working1$drillDays <- working1$md/as.numeric(capexValues1()$drillSpeed)
        
        working1 <- working1 %>% 
          mutate(drillCost = as.numeric(capexValues1()$tvdSelect)*sixteenCasingTVD*sixteenRate+
                   as.numeric(capexValues1()$tvdSelect)*thirteenCasingTVD*thirteenRate+
                   as.numeric(capexValues1()$tvdSelect)*nineCasingTVD*nineRate +
                   as.numeric(capexValues1()$tvdSelect)*sevenCasingTVD*sevenRate+
                   as.numeric(capexValues1()$tvdSelect)*fiveCasingTVD*fiveRate + 
                   md*fiveCasingTMD*fiveRate+md*fourCasingTMD*fourRate +
                   as.numeric(capexValues1()$tvdSelect)*tubingTVD*tubingRate+
                   as.numeric(capexValues1()$padConstruction)+
                   as.numeric(capexValues1()$drillingFluidsPerDay)*drillDays+
                   as.numeric(capexValues1()$drillingFuelPerDay)*drillDays+
                   as.numeric(capexValues1()$ancDrillPerDay)*drillDays+cementing+
                   rigRate*drillDays)
        
        working1$brownPercent <- as.numeric(capexValues1()$brownSelect)
        working1$ceramicPercent <- as.numeric(capexValues1()$ceramicSelect)
        working1$rcsPercent <- as.numeric(capexValues1()$rcsSelect)
        working1$northernPercent <- as.numeric(capexValues1()$northernSelect)
        
        working1 <- working1 %>%
          mutate(compCost = fleetRateComp*fleetRatePerHP*compDays+fluid*waterAcqRate/42 +
                   fluid*waterHaulingToRate/42+
                   fluid*waterTreatmentRate/42+
                   fluid*waterHaulingFromRate/42*waterFlowbackPercent +
                   fluid*waterDispRate*waterFlowbackPercent/42+
                   (brownPercent*sandRate+ceramicPercent*ceramicRate +
                      rcsPercent*rcsRate+northernPercent*northernRate)*proppant +
                   (brownPercent*sandTranspRate +ceramicPercent*ceramicTranspRate +
                      rcsPercent*rcsTranspRate+northernPercent*northernTranspRate)*proppant +
                   stages*as.numeric(capexValues1()$chemsPerStage)+
                   stages*as.numeric(capexValues1()$pumpFuel)+compDays*as.numeric(capexValues1()$ancCompPerDay))
        
        working1 <- working1 %>% mutate(facilities = (drillCost+compCost)/(1-eNt)*facil/2,
                                        equipment = (drillCost+compCost)/(1-eNt)*facil/2,
                                        pipeline = (drillCost+compCost)/(1-eNt)*tieIn)
        
        working1 <- working1 %>% mutate(dNc = drillCost+compCost, dcet = dNc + facilities + equipment + pipeline)
        working1$dNc <- working1$dNc*as.numeric(capexValues1()$capexStressor)
        working1$dcet <- working1$dcet*as.numeric(capexValues1()$capexStressor)
        working1 <- working1 %>% select(dNc, dcet)
        working1 <- as.data.frame(working1)
        #working1$dNcPerFt <- dollar(as.integer(working1$dNc/as.numeric(capexValues()$perfSelect)))
        working1$dcetPerFt <- (as.integer(working1$dcet/as.numeric(well$perf)))
        #working1$dNc <- dollar(as.integer(working1$dNc))
        #working1$dcet <- dollar(as.integer(working1$dcet))
        well$dcetPerFt <- working1$dcetPerFt
        well
      },
      error = function(e) {
        e
        NULL
      }))
      
      costCalc <- dplyr::bind_rows(econSummary)
      costCalc <- costCalc[,c('perf', 'dcetPerFt')]
      names(costCalc) <- c('Lateral', 'CapexPerFt')
      cols <- c('#06357a')
      hchart(costCalc, "line", hcaes(x = Lateral, y = CapexPerFt), name = 'Capex Per Ft') %>%
        hc_title(
          text = "Cost Per Ft vs Lateral Length",
          useHTML = TRUE) %>%
        hc_subtitle(text = '<a href="https://www.woodmac.com/">Source: Wood Mackenzie Cost Center of Excellence</a>', useHTML=TRUE, align = 'right') %>%
        hc_tooltip(table = TRUE, sort = TRUE) %>%
        hc_xAxis(title = list(text = '<b>Lateral Length, ft</b>')) %>%
        hc_yAxis(title = list(text = '<b>Capex/Ft, $/Ft</b>')) %>%
        hc_credits(
          enabled = TRUE,
          text = "Powered by Highcharts",
          href = "https://www.highcharts.com/") %>%
        hc_colors(cols)%>%
        hc_exporting(enabled = TRUE, filename = 'wellCostTrend')
      
      # billboarder() %>%
      #   bb_linechart(data = costCalc, type = 'spline', width =3, color = "#00a4e3") %>%
      #   bb_y_grid(show = TRUE) %>%
      #   bb_y_axis(tick = list(format = suffix("$/ft")),
      #             label = list(text = "Capex Per Ft, $/Ft", position = "outer-top")) %>% 
      #   bb_x_axis(tick = list(format = suffix("ft")),
      #             label = list(text = "Lateral Length, ft", position = "outer-top")) %>% 
      #   bb_legend(show = FALSE) %>% 
      #   bb_labs(title = "Cost Per Lateral Ft",
      #           caption = "Data source: Cost Center of Excellence")
    }
  })
  
  output$offsets_plot <- renderHighchart({
    df1 <- df2() %>% filter(subPlay %in% input$subPlay1)
    leases1 <- leases() %>%# filter(subPlay %in% input$subPlay) %>% 
      filter(possLocation %in% df1$possLocation) %>% filter(!duplicated(paste0(operator, oneSecLocation))) %>% 
      filter(!operator %in% input$operator1) %>%
      group_by(operator) %>% summarise(acres = sum(acres)) %>% ungroup() %>% arrange(desc(acres)) %>% top_n(15)
    if(nrow(leases1) == 0){
      NULL
    } else {
      cols <- c("#00a4e3")
      hchart(leases1, "column", hcaes(x = operator, y = acres), name = 'Offset Acreage') %>%
        hc_title(
          text = "Direct Offset Acreage by Operator",
          useHTML = TRUE) %>%
        hc_subtitle(text = '<a href="https://www.woodmac.com/research/lens/lens-direct/">Source: P2</a>', useHTML=TRUE, align = 'right') %>%
        hc_tooltip(table = TRUE, sort = TRUE) %>%
        hc_xAxis(title = list(text = '<b>Operator</b>')) %>%
        hc_yAxis(title = list(text = '<b>Acres</b>')) %>%
        hc_credits(
          enabled = TRUE,
          text = "Powered by Highcharts",
          href = "https://www.highcharts.com/") %>%
        hc_colors(cols)%>%
        hc_exporting(enabled = TRUE, filename = 'acreageOffsets')
    }
    
    # billboarder() %>%
    #   bb_barchart(data = leases3() %>% group_by(operator) %>% summarise(acres = sum(acres)) %>% ungroup() %>% arrange(desc(acres)) %>% top_n(15) %>% group_by(operator)) %>%
    #   bb_legend(show = FALSE) %>% 
    #   bb_x_axis(tick = list(rotate = 45)) %>%
    #   bb_labs(title = 'Acreage By Operator',
    #           caption = '')
  })
  
  declineValues1 <- reactive({
    data.frame(
      Component = c('qiOilS', 'bOilS', 'DiOilS', 'DfOilS', 'curtailOilS',
                    'qiGasS', 'bGasS', 'DiGasS', 'DfGasS', 'curtailGasS',
                    'wellLifeS', 'spudToProd', 'cutoff', 'prbRisking', 'possRisking'),
      
      Value = c(input$qiOilS, input$bOilS, input$DiOilS, input$DfOilS, input$curtailOilS,
                input$qiGasS, input$bGasS, input$DiGasS, input$DfGasS, input$curtailGasS,
                input$wellLifeS, input$spudToProd1, input$cutoff1, input$prbRisking1, input$possRisking1),
      stringsAsFactors = FALSE) %>% spread(Component, Value)
    
  })
  
  prodData1 <- reactive(
    prodData %>% filter(API %in% df()$API)
  )
  
  observe({
    if(is.null(input$subPlay1)||input$subPlay1 == ''||nrow(prodData1())==0){
      NULL
    } else {

      if(is.null(input$selectYr)||input$selectYr == ''){
        NULL
      } else {
        df <- as.data.frame(df()) %>%
          filter(subPlay %in% input$subPlay1) %>%
          filter(operator %in% input$operatorSelect) %>%
          filter(as.character(fp.year) %in% input$selectYr)

        #print(head(df))
      
        prod.data <- prodData1() %>% arrange(API, date) %>% filter(API %in% df$API)# %>% group_by(API) %>% mutate(month = cumsum(API/API))
        prod.data <- merge(prod.data, df[,c('API', 'perf', 'fp.year')], by='API', all.x=TRUE)
        
        #print(head(prod.data))
        #print(head(prod.data))
        if(nrow(prod.data) == 0){
          NULL
        } else {
          if(input$productSelect1 == 'Oil'){
            shinyjs::hide('qiGasS')
            shinyjs::hide('DiGasS')
            shinyjs::hide('bGasS')
            shinyjs::hide('DfGasS')
            shinyjs::hide('curtailGasS')
            shinyjs::show('qiOilS')
            shinyjs::show('DiOilS')
            shinyjs::show('bOilS')
            shinyjs::show('DfOilS')
            shinyjs::show('curtailOilS')
            prod.data <- prod.data %>% filter(oil > 0) %>% filter(!is.na(perf)) %>%
              mutate(oil = oil/perf*as.numeric(capexValues1()$perfSelect)) %>%
              group_by(API) %>% mutate(month=cumsum(API/API)) %>% ungroup()
            if(nrow(prod.data)==0){
              prod.data <- data.frame(fp.year = 2008, month = 1, oil = 0)
              cols <- c(  '#008542', '#ff4e50', '#5c6d00','#00a4e3', '#adafb2','#0D1540','#06357a',  '#a31c37', '#d26400', '#eaa814',
                          '#5c1848', '#786592', '#027971')
              
              
              
              p <- hchart(prod.data[,c('fp.year', 'month', 'oil')], type = 'line', hcaes(x = 'month', y='oil', group = 'fp.year')) %>%
                #hc_add_series(data = df$WTI, type = 'line') %>%
                #hc_add_series(data = meanProd[,c('month', 'oil')], type = 'line', hcaes(x = 'month', y='oil'),name = 'Mean') %>%
                hc_title(
                  text = "Oil Spaghetti Plot - Lateral Length Normalized",
                  useHTML = FALSE) %>%
                hc_subtitle(text = '<a href="www.woodmac.com">Source: Wood Mackenzie Lens Direct</a>', useHTML=TRUE, align = 'right') %>%
                #hc_tooltip(table = TRUE, sort = TRUE) %>%
                hc_xAxis(title = list(text = '<b>Months Producing</b>')) %>%
                hc_yAxis(title = list(text = '<b>Average Production: BBL/D</b>')) %>%
                hc_credits(
                  enabled = TRUE,
                  text = "Powered by Highcharts",
                  href = "https://www.highcharts.com/") %>%
                hc_colors(cols)
            } else {
              
            prod.data <- prod.data[,c('API', 'fp.year', 'month', 'oil')]
            values$life <- max(prod.data$month)
            meanProd <- prod.data %>% group_by(month) %>% summarise(oil = mean(oil, na.rm=TRUE)/30.45, count=n()) %>% ungroup()
            prod.data <- prod.data %>% group_by(fp.year, month) %>% summarise(oil = mean(oil, na.rm=TRUE)/30.45) %>% ungroup()
            #filter(count >= max(count)*0.4)
            #print(head(meanProd))
            cols <- c(  '#008542', '#ff4e50', '#5c6d00','#00a4e3', '#adafb2','#0D1540','#06357a',  '#a31c37', '#d26400', '#eaa814',
                        '#5c1848', '#786592', '#027971')

           

            p <- hchart(prod.data[,c('fp.year', 'month', 'oil')], type = 'line', hcaes(x = 'month', y='oil', group = 'fp.year')) %>%
              #hc_add_series(data = df$WTI, type = 'line') %>%
              hc_add_series(data = meanProd[,c('month', 'oil')], type = 'line', hcaes(x = 'month', y='oil'),name = 'Mean') %>%
              hc_title(
                text = "Oil Spaghetti Plot - Lateral Length Normalized",
                useHTML = FALSE) %>%
              hc_subtitle(text = '<a href="www.woodmac.com">Source: Wood Mackenzie Lens Direct</a>', useHTML=TRUE, align = 'right') %>%
              #hc_tooltip(table = TRUE, sort = TRUE) %>%
              hc_xAxis(title = list(text = '<b>Months Producing</b>')) %>%
              hc_yAxis(title = list(text = '<b>Average Production: BBL/D</b>')) %>%
              hc_credits(
                enabled = TRUE,
                text = "Powered by Highcharts",
                href = "https://www.highcharts.com/") %>%
              hc_colors(cols)
            }

           
          } else {
            shinyjs::show('qiGasS')
            shinyjs::show('DiGasS')
            shinyjs::show('bGasS')
            shinyjs::show('DfGasS')
            shinyjs::show('curtailGasS')
            shinyjs::hide('qiOilS')
            shinyjs::hide('DiOilS')
            shinyjs::hide('bOilS')
            shinyjs::hide('DfOilS')
            shinyjs::hide('curtailOilS')
            prod.data <- prod.data %>% filter(gas > 0) %>% filter(!is.na(perf)) %>%
              mutate(gas = gas/perf*as.numeric(capexValues1()$perfSelect)) %>%
              group_by(API) %>% mutate(month=cumsum(API/API)) %>% ungroup()
            if(nrow(prod.data)==0){
              prod.data <- data.frame(fp.year = 2008, month = 1, gas = 0)
              cols <- c(  '#008542', '#ff4e50', '#5c6d00','#00a4e3', '#adafb2','#0D1540','#06357a',  '#a31c37', '#d26400', '#eaa814',
                          '#5c1848', '#786592', '#027971')
              p <- hchart(prod.data[,c('fp.year', 'month', 'gas')], type = 'line', hcaes(x = 'month', y='gas', group = 'fp.year')) %>%
                #hc_add_series(data = df$WTI, type = 'line') %>%
                #hc_add_series(data = meanProd[,c('month', 'gas')], type = 'line', hcaes(x = 'month', y='gas'),name = 'Mean') %>%
                hc_subtitle(text = '<a href="www.woodmac.com">Source: Wood Mackenzie Lens Direct</a>', useHTML=TRUE, align = 'right') %>%
                #hc_tooltip(table = TRUE, sort = TRUE) %>%
                hc_xAxis(title = list(text = '<b>Months Producing</b>')) %>%
                hc_yAxis(title = list(text = '<b>Average Production: MCF/D</b>')) %>%
                hc_credits(
                  enabled = TRUE,
                  text = "Powered by Highcharts",
                  href = "https://www.highcharts.com/") %>%
                hc_colors(cols)
            } else {
            
            
              prod.data <- prod.data[,c('API', 'fp.year', 'month', 'gas')]
              values$life <- max(prod.data$month)
  
              meanProd <- prod.data %>% group_by(month) %>% summarise(gas = mean(gas, na.rm=TRUE)/30.45, count=n())# %>%
              prod.data <- prod.data %>% group_by(fp.year, month) %>% summarise(gas = mean(gas, na.rm=TRUE)/30.45) %>% ungroup()
              #filter(count >= max(count)*0.4)
              cols <- c(  '#008542', '#ff4e50', '#5c6d00','#00a4e3', '#adafb2','#0D1540','#06357a',  '#a31c37', '#d26400', '#eaa814',
                          '#5c1848', '#786592', '#027971')
              p <- hchart(prod.data[,c('fp.year', 'month', 'gas')], type = 'line', hcaes(x = 'month', y='gas', group = 'fp.year')) %>%
                #hc_add_series(data = df$WTI, type = 'line') %>%
                hc_add_series(data = meanProd[,c('month', 'gas')], type = 'line', hcaes(x = 'month', y='gas'),name = 'Mean') %>%
                hc_subtitle(text = '<a href="www.woodmac.com">Source: Wood Mackenzie Lens Direct</a>', useHTML=TRUE, align = 'right') %>%
                #hc_tooltip(table = TRUE, sort = TRUE) %>%
                hc_xAxis(title = list(text = '<b>Months Producing</b>')) %>%
                hc_yAxis(title = list(text = '<b>Average Production: MCF/D</b>')) %>%
                hc_credits(
                  enabled = TRUE,
                  text = "Powered by Highcharts",
                  href = "https://www.highcharts.com/") %>%
                hc_colors(cols)
          }
            # p <- plot_ly(prod.data, x=~month, y=~gas/30.45, group=~as.factor(API), color=I('gray'), type='scatter', mode='lines', name='Actuals') %>%
            #   #add_trace(data = fitGas, x=~month, y=~gasFCST/30.45, color=I('red'), name = 'Forecast', type='scatter', mode='lines')%>%
            #   add_trace(data = meanProd, x=~month, y=~gas/30.45, color=I('black'), name = 'Average', type='scatter', mode='lines')#%>%
            # #layout(title = title, yaxis = list(title='Daily Gas Rate, mcfd', type='log'), xaxis=  list(title = 'Month'))
          }
          values$p <- p
        }
      }
    }

  })
  # 
  # observeEvent(input$selectYr, {
  #   df1 <- df() %>% filter(subPlay %in% input$subPlay1) %>% filter(as.character(fp.year) %in% input$selectYr)
  #   updatePickerInput(session, 'operatorSelect', 'Select Operator(s)', choices = sort(unique(df1$operator)), selected =  sort(unique(df1$operator))[1])
  # 
  # })

  output$spPlot1 <- renderHighchart({
    if(is.null(values$p)||is.null(values$life)){
      NULL
    } else {

      if(input$productSelect1 == 'Oil'){
        #print(head(prod.data))
        #print(declineValues1())
        fitOil <- curtailed.q(arps.decline(
          as.numeric(declineValues1()$qiOilS)*365, as.nominal(as.numeric(declineValues1()$DiOilS)), as.numeric(declineValues1()$bOilS), as.nominal(as.numeric(declineValues1()$DfOilS))),
          as.numeric(declineValues1()$curtailOilS)/12.0,seq(0, declineValues1()$wellLifeS-1/12, by= (1/12)))/12

        fitOil <- as.data.frame(fitOil)
        
        #print(head(fitOil))
        #rm(fitOil)
        names(fitOil) <- c('oilFCST')
        if(declineValues1()$curtailOilS > 0){
          fitOil$oilFCST[1] <- fitOil$oilFCST[1]/2
        }

        fitOil <- fitOil %>% mutate(month = nrow(fitOil)) %>% mutate(month = cumsum(month/month))

        fcstOil <- sum(fitOil$oil)
        values$qiOil <- as.numeric(declineValues1()$qiOilS)/(fcstOil/1000)
        #print(values$qiOil)
        title <- paste('Forecast Oil EUR (MBO): ', as.integer(fcstOil/1000), sep='')
        #print(head(fitOil))
        #print(values$life)
        fitOil <- fitOil %>% filter(month <= values$life)
        fitOil$oilFCST <- fitOil$oilFCST/30.45
        
        p <- values$p %>%
          hc_add_series(data = fitOil[,c('month', 'oilFCST')], type = 'line', hcaes(x = 'month', y='oilFCST'),name = 'Forecast') %>%
          hc_title(
            text = title,
            useHTML = FALSE)%>%
          hc_exporting(enabled = TRUE, filename = 'oilTC')
        # p <- values$p %>%
        #   add_trace(data = fitOil, x=~month, y=~oilFCST/30.45, color=I('green'), name = 'Forecast', type='scatter', mode='lines') %>%
        #   #add_trace(data = meanProd, x=~month, y=~oil/30.45, color=I('black'), name = 'Average', type='scatter', mode='lines')%>%
        #   layout(title = title, yaxis = list(title='Daily Oil Rate, bopd', type='log'), xaxis=  list(title = 'Month'))
      } else {


        fitGas <- curtailed.q(arps.decline(
          as.numeric(declineValues1()$qiGasS)*365, as.nominal(as.numeric(declineValues1()$DiGasS)), as.numeric(declineValues1()$bGasS), as.nominal(as.numeric(declineValues1()$DfGasS))),
          as.numeric(declineValues1()$curtailGasS)/12.0,seq(0, declineValues1()$wellLifeS-1/12, by= (1/12)))/12

        fitGas <- as.data.frame(fitGas)
        #rm(fitOil)
        names(fitGas) <- c('gasFCST')
        if(declineValues1()$curtailGasS > 0){
          fitGas$gasFCST[1] <- fitGas$gasFCST[1]/2
        }
        fitGas <- fitGas %>% mutate(month = nrow(fitGas)) %>% mutate(month = cumsum(month/month))

        fcstGas <- sum(fitGas$gas)
        values$qiGas <- as.numeric(declineValues1()$qiGasS)/(fcstGas/1000)
        #print(values$qiGas)
        title <- paste('Forecast Gas EUR (MMCF): ', as.integer(fcstGas/1000), sep='')
        fitGas <- fitGas %>% filter(month <= values$life)

        fitGas$gasFCST <- fitGas$gasFCST/30.45
        p <- values$p %>%
          hc_add_series(data = fitGas[,c('month', 'gasFCST')], type = 'line', hcaes(x = 'month', y='gasFCST'),name = 'Forecast') %>%
          hc_title(
            text = title,
            useHTML = FALSE)%>%
          hc_exporting(enabled = TRUE, filename = 'gasTC')
        # p <- values$p %>%
        #   add_trace(data = fitGas, x=~month, y=~gasFCST/30.45, color=I('red'), name = 'Forecast', type='scatter', mode='lines')%>%
        #   #add_trace(data = meanProd, x=~month, y=~gas/30.45, color=I('black'), name = 'Average', type='scatter', mode='lines')%>%
        #   layout(title = title, yaxis = list(title='Daily Gas Rate, mcfd', type='log'), xaxis=  list(title = 'Month'))
        #
      }
      p
    }
  })
  # 
  
  observe({
    if(is.null(values$qiGas)|is.null(values$qiOil)){
      shinyjs::disable('calcInv')
    } else {
      shinyjs::enable('calcInv')
    }
  })
  
  observeEvent(input$calcInv, {
    if(is.null(input$subPlay1)){
      NULL
    }else {
      updateActionButton(session, "calcInv",
                         label = 'Calculating....')
      shinyjs::disable('calcInv')


      df <- leases() %>% filter(operator %in% input$operator1) %>% filter(subPlay %in% input$subPlay1) %>% filter(!duplicated(oneSecLocation))

      df3 <- df %>% group_by(operator) %>% summarise(acres = n()*640) %>% ungroup()
      df3 <- as.data.frame(df3)


      df1 <- df() %>% filter(oneSecLocation %in% df$oneSecLocation) %>%
        #filter(reservoir %in% input$reservoirSelect) %>%
        group_by(oneSecLocation) %>% summarise(wells=n(), perf = mean(perf, na.rm=TRUE),
                                               ppf =mean(ppf, na.rm=TRUE), fpf = mean(fpf, na.rm=TRUE))
      df <- merge(df, df1, by='oneSecLocation', all.x=TRUE)



      df$bOil <- as.numeric(declineValues1()$bOilS)
      df$bGas <- as.numeric(declineValues1()$bGasS)
      df$DfOil <- as.numeric(declineValues1()$DfOilS)
      df$DfGas <- as.numeric(declineValues1()$DfGasS)
      #rm(factors)
      df$oldPerf <- df$perf
      df$oldPPF <- df$ppf
      df$oldFPF <- df$fpf
      df$perf <- as.numeric(capexValues1()$perfSelect)
      df$ppf <- as.numeric(capexValues1()$ppfSelect)
      df$fpf <- as.numeric(capexValues1()$fpfSelect)



      df$wells[is.na(df$wells)] <- 0
      #print(head(df))

      qiOil <- as.numeric(values$qiOil)
      qiGas <- as.numeric(values$qiGas)




      df <- df %>% group_by(oneSecLocation) %>%
        mutate(risk1 = mean(risk, na.rm=TRUE), tvd1 = mean(tvd, na.rm=TRUE), gasFrac1 = mean(gasFrac, na.rm=TRUE))
      df$risk[is.na(df$risk)] <- df$risk1[is.na(df$risk)]
      df$tvd[is.na(df$tvd)] <- df$tvd1[is.na(df$tvd)]
      df$gasFrac[is.na(df$gasFrac)] <- df$gasFrac1[is.na(df$gasFrac)]
      df <- df %>% group_by(threeSecLocation) %>%
        mutate(risk1 = mean(risk, na.rm=TRUE), tvd1 = mean(tvd, na.rm=TRUE), gasFrac1 = mean(gasFrac, na.rm=TRUE))
      df$risk[is.na(df$risk)] <- df$risk1[is.na(df$risk)]
      df$tvd[is.na(df$tvd)] <- df$tvd1[is.na(df$tvd)]
      df$gasFrac[is.na(df$gasFrac)] <- df$gasFrac1[is.na(df$gasFrac)]
      df <- df %>% group_by(possLocation) %>%
        mutate(risk1 = mean(risk, na.rm=TRUE), tvd1 = mean(tvd, na.rm=TRUE), gasFrac1 = mean(gasFrac, na.rm=TRUE))
      df$risk[is.na(df$risk)] <- df$risk1[is.na(df$risk)]
      df$tvd[is.na(df$tvd)] <- df$tvd1[is.na(df$tvd)]
      df$gasFrac[is.na(df$gasFrac)] <- df$gasFrac1[is.na(df$gasFrac)]
      df <- df %>% group_by(twnRngLocation) %>%
        mutate(risk1 = mean(risk, na.rm=TRUE), tvd1 = mean(tvd, na.rm=TRUE), gasFrac1 = mean(gasFrac, na.rm=TRUE))
      df$risk[is.na(df$risk)] <- df$risk1[is.na(df$risk)]
      df$tvd[is.na(df$tvd)] <- df$tvd1[is.na(df$tvd)]
      df$gasFrac[is.na(df$gasFrac)] <- df$gasFrac1[is.na(df$gasFrac)]
      df$logPPF <- log(df$ppf)
      df <- as.data.frame(df)
      df <- subset(df, select=-c(risk1, tvd1, gasFrac1))
      df <- df %>% filter(!is.na(risk)) %>% filter(!is.na(tvd)) %>% filter(!is.na(gasFrac))
      df$md <- df$tvd + df$perf
      #df$bOil[is.na(df$bOil)] <- (as.numeric(declineValues()$bOilS))
      #df$bGas[is.na(df$bGas)] <- (as.numeric(declineValues()$bGasS))
      #df$DfOil[is.na(df$DfOil)] <- (as.numeric(declineValues()$DfOilS))
      #df$DfGas[is.na(df$DfGas)] <- (as.numeric(declineValues()$DfGasS))



      if(nrow(df) == 0) {
        df <- data.frame('Summary' = 'No Economic Locations')
      } else {

        x <- -0.2931903
        y <- -0.4256987
        z <- 0.2750021
        perfUplift <- perfUplift
        propUplift <- propUplift
        df$x <- x
        df$y <- y
        df$z <- z
        df$EUR <- exp(predict(propUplift, df))
        df$EUR <- df$EUR*df$risk
        df$perfRisk <- perfUplift$coefficients[1] +
          perfUplift$coefficients[2]*df$perf +
          perfUplift$coefficients[3]*(df$perf**2)
        df$EUR <- df$EUR*df$perfRisk
        df$oilEUR <- df$EUR*(1-df$gasFrac)
        df$gasEUR <- (df$EUR-df$oilEUR)*20

        pdpCount <- df() %>% #filter(subBasin %in% input$subBasinSelect) %>%
          filter(subPlay %in% input$subPlay1) %>%
          group_by(oneSecLocation) %>% summarise(pdpCount=n()) %>% ungroup()
        pudCount <-  df()%>% #filter(subBasin %in% input$subBasinSelect) %>%
          filter(subPlay %in% input$subPlay1) %>%
          group_by(twoSecLocation) %>% summarise(pudCount=n())%>% ungroup()
        prbCount <-  df() %>% #filter(subBasin %in% input$subBasinSelect) %>%
          filter(subPlay %in% input$subPlay1) %>%
          group_by(threeSecLocation) %>% summarise(prbCount = n()) %>% ungroup()
        possCount <-  df() %>% #filter(subBasin %in% input$subBasinSelect) %>%
          filter(subPlay %in% input$subPlay1) %>%
          group_by(possLocation) %>% summarise(possCount=n())

        df <- merge(df, pudCount, by=c('twoSecLocation'), all.x=TRUE)
        df <- merge(df, prbCount, by=c('threeSecLocation'), all.x=TRUE)
        df <- merge(df, possCount, by=c('possLocation'), all.x=TRUE)
        rm(pudCount, prbCount, possCount)
        df$rsvCat <- NA_character_
        df$rsvCat[!is.na(df$pudCount)] <- 'PUD'
        df$rsvCat[is.na(df$rsvCat) & !is.na(df$prbCount)] <- 'PRB'
        df$rsvCat[is.na(df$rsvCat) & !is.na(df$possCount)] <- 'POSS'
        df <- merge(df, pdpCount, by=c('oneSecLocation'), all.x=TRUE)
        rm(pdpCount)
        df$rsvCat[!is.na(df$pdpCount)] <- 'PUD'
        totalAcreage <- nrow(df)*640
        df <- df %>% filter(!is.na(rsvCat))

        df$oilEUR[df$rsvCat == 'PRB'] <- df$oilEUR[df$rsvCat == 'PRB']*as.numeric(declineValues1()$prbRisking)
        df$gasEUR[df$rsvCat == 'PRB'] <- df$gasEUR[df$rsvCat == 'PRB']*as.numeric(declineValues1()$prbRisking)
        df$oilEUR[df$rsvCat == 'POSS'] <- df$oilEUR[df$rsvCat == 'POSS']*as.numeric(declineValues1()$possRisking)
        df$gasEUR[df$rsvCat == 'POSS'] <- df$gasEUR[df$rsvCat == 'POSS']*as.numeric(declineValues1()$possRisking)
        df$qiOil <- df$oilEUR * qiOil
        df$qiGas <- df$gasEUR * qiGas
        df$oilEUR <-df$oilEUR*1000
        df$gasEUR <- df$gasEUR*1000

        df$wellsPerSection <- as.numeric(expenseValues1()$wellSpacing)
        df$remWells <- df$wellsPerSection - df$wells
        df$oldPerf[is.na(df$oldPerf)] <- as.numeric(capexValues1()$perfSelect)
        df$remWells <- df$remWells*5280/df$oldPerf

        df$remWells[df$remWells < 0] <- 0
        df$remWells <- as.integer(df$remWells)
        df$remWells[df$remWells > as.numeric(expenseValues1()$wellSpacing)] <- as.numeric(expenseValues1()$wellSpacing)
        #print('Good so far!')
        #subBasins <- values$df %>% filter(subPlay %in% input$subPlaySelect)
        df$subBasin <- df()$subBasin[1]
        #df$subBasin[df$subBasin == 'DJ'] <- 'Niobrara'
        #df$subBasin[df$subBasin == 'Powder'] <- 'Niobrara'

        #print(head(df))
        working1 <- merge(df, costData, by='subBasin', all.x=TRUE)

        working1$cementing <- working1$md*8
        working1$proppant <- working1$ppf*working1$perf
        working1$fluid <- working1$fpf*working1$perf
        working1$proppantPerStage <- as.numeric(capexValues1()$proppantPerStage)
        working1$stages <- working1$proppant/working1$proppantPerStage
        working1$compDays <- working1$stages/as.numeric(capexValues1()$stagesPerDay)
        working1$drillDays <- working1$md/as.numeric(capexValues1()$drillSpeed)
        working1 <- working1 %>%
          mutate(drillCost = tvd*sixteenCasingTVD*sixteenRate+
                   tvd*thirteenCasingTVD*thirteenRate+tvd*nineCasingTVD*nineRate +
                   tvd*sevenCasingTVD*sevenRate+tvd*fiveCasingTVD*fiveRate +
                   md*fiveCasingTMD*fiveRate+md*fourCasingTMD*fourRate +
                   tvd*tubingTVD*tubingRate+as.numeric(capexValues1()$padConstruction)+
                   as.numeric(capexValues1()$drillingFluidsPerDay)*drillDays+
                   as.numeric(capexValues1()$drillingFuelPerDay)*drillDays+
                   as.numeric(capexValues1()$ancDrillPerDay)*drillDays+cementing+rigRate*drillDays)

        working1$brownPercent <- as.numeric(capexValues1()$brownSelect)
        working1$ceramicPercent <- as.numeric(capexValues1()$ceramicSelect)
        working1$rcsPercent <- as.numeric(capexValues1()$rcsSelect)
        working1$northernPercent <- as.numeric(capexValues1()$northernSelect)

        working1 <- working1 %>%
          mutate(compCost = fleetRateComp*fleetRatePerHP*compDays+fluid*waterAcqRate/42 +
                   fluid*waterHaulingToRate/42+fluid*waterTreatmentRate/42+
                   fluid*waterHaulingFromRate/42*waterFlowbackPercent +
                   fluid*waterDispRate*waterFlowbackPercent/42+
                   (brownPercent*sandRate+ceramicPercent*ceramicRate +
                      rcsPercent*rcsRate+northernPercent*northernRate)*proppant +
                   (brownPercent*sandTranspRate +ceramicPercent*ceramicTranspRate +
                      rcsPercent*rcsTranspRate+northernPercent*northernTranspRate)*proppant +
                   stages*as.numeric(capexValues1()$chemsPerStage)+
                   stages*as.numeric(capexValues1()$pumpFuel)+compDays*as.numeric(capexValues1()$ancCompPerDay))

        working1 <- working1 %>% mutate(facilities = (drillCost+compCost)/(1-eNt)*facil/2,
                                        equipment = (drillCost+compCost)/(1-eNt)*facil/2,
                                        pipeline = (drillCost+compCost)/(1-eNt)*tieIn)

        working1 <- working1 %>% mutate(dNc = drillCost+compCost, dcet = dNc + facilities + equipment + pipeline)

        df$dNc <- working1$dNc*as.numeric(capexValues1()$capexStressor)
        df$dcet <- working1$dcet*as.numeric(capexValues1()$capexStressor)
        rm(working1)

        df <- as.data.frame(df)

        df$DiGas <-  declineValues1()$DiGasS

        df$DiOil <- declineValues1()$DiOilS




        #df <- bind_rows(econSummary)
        #df <- as.data.frame(df)
        #print(nrow(df))
        df <- df %>% filter(!duplicated(oneSecLocation))

        #print(nrow(df))
        #rm(econSummary)

        #print(head(df))
        #df <- as.data.frame(df)


        econSummary <- lapply(split(df, df[,'oneSecLocation']), function (df1) tryCatch({
          df2 <- df1
          fitOil <- curtailed.q(arps.decline(
            as.numeric(df1$qiOil)*365, as.nominal(as.numeric(df1$DiOil)), as.numeric(df1$bOil), as.nominal(as.numeric(df1$DfOil))),
            as.numeric(declineValues1()$curtailOilS)/12.0,seq(0, declineValues1()$wellLifeS-1/12, by= (1/12)))/12

          fit <- as.data.frame(fitOil)
          rm(fitOil)
          names(fit) <- c('oilFCST')
          #if(input$curtailOil1 > 0){
          #  fit$oilFCST[1] <- fit$oilFCST[1]/2
          #}


          fit <- fit %>% mutate(prodMonth = nrow(fit)) %>% mutate(prodMonth = cumsum(prodMonth/prodMonth))

          fitGas <- curtailed.q(arps.decline(
            as.numeric(df1$qiGas)*365, as.nominal(as.numeric(df1$DiGas)), as.numeric(df1$bGas), as.nominal(as.numeric(df1$DfGas))),
            as.numeric(declineValues1()$curtailGasS)/12.0,seq(0, declineValues1()$wellLifeS-1/12, by= (1/12)))/12
          names(fitGas) <- c('gasFCST')
          fit$gasFCST <- fitGas
          #if(input$curtailGas1 > 0){
          #  fit$gasFCST[1] <- fit$gasFCST[1]/2
          #}

          fit <- fit[,c('prodMonth', 'oilFCST', 'gasFCST')]
          names(fit) <- c('Months', 'Oil', 'Gas')
          fit$Water <- fit$Oil + fit$Gas/20
          df <- fit
          rm(fit)

          #print(head(df))
          #df$Oil <- df$Oil*expenseValues1()$wi/100
          #df$Gas <- df$Gas*expenseValues1()$wi/100
          #df$Water <- df$Water*expenseValues1()$wi/100
          df$Sales_Gas <- df$Gas*expenseValues1()$shrink
          df$NGL <- df$Gas*expenseValues1()$nglYield/1000
          #df$wi <- expenseValues1()$wi
          df1 <- data.frame(Months = seq(((declineValues1()$spudToProd-1)*-1), 0, 1), Oil = 0, Gas = 0, Water = 0)
          df$capex <- 0
          df1$capex <- 0
          df1$capex[1] <- df2$dcet
          #df1$capex[nrow(df1)] <- expenseValues1()$completeCost*expenseValues1()$wi/100

            prices <- values$price
            prices <- prices %>% filter(DATE >= today())
            prices <- prices[(declineValues1()$spudToProd):nrow(prices),]
            if(nrow(prices) > nrow(df)){
              prices <- prices[1:nrow(df),]
            }
            df$oilPrice <- NA
            df$oilPrice[1:nrow(prices)] <- prices$WTI
            df$gasPrice <- NA
            df$gasPrice[1:nrow(prices)] <- prices$HH
            rm(prices)
            df$oilPrice <- na.locf(df$oilPrice)
            df$gasPrice <- na.locf(df$gasPrice)
            df$nglPrice <- df$oilPrice*expenseValues1()$nglDiff/100

          #print(head(df))
          df$nri <- expenseValues1()$nri#*expenseValues1()$wi/100
          df$oilRev <- (df$oilPrice-expenseValues1()$oilDiff)*df$nri/100*df$Oil
          df$gasRev <- (df$gasPrice-expenseValues1()$gasDiff)*df$nri/100*df$Sales_Gas*expenseValues1()$btu
          df$nglRev <- (df$nglPrice)*df$nri/100*df$NGL
          df$rev <- df$oilRev+df$gasRev+df$nglRev
          #print(head(df))
          df$tax <- df$oilRev*expenseValues1()$stxOil/100 + (df$gasRev+df$nglRev)*expenseValues1()$stxGas/100 +
            df$Oil*df$nri/100*expenseValues1()$oilSTX + df$Gas*df$nri/100*expenseValues1()$gasSTX +
            df$rev*df$nri/100*expenseValues1()$atx/100
          #print(head(df))
          #print(expenseValues1()$fixedCost)
          #print(expenseValues1()$varExpGas)
          #print(expenseValues1()$varExpBOE)


          df$expense <- expenseValues1()$fixedCost + df$Gas*expenseValues1()$varExpGas +
            (df$Oil + df$NGL)*expenseValues1()$varExpBOE
          #print(head(df))
          df$nocf <- df$rev-df$tax-df$expense



          if(input$econAbanS == 'Yes'){
            df <- df[nrow(df):1,]
            df$cumNOCF <- cumsum(df$nocf)
            df$prev <- df$cumNOCF- dplyr::lag(df$cumNOCF, n = 1L)
            prev <- which(df$prev > 0)[1]
            df <- df[prev:nrow(df),]
            df <- df[nrow(df):1,]
            df <- subset(df, select = -c(cumNOCF, prev))
          }
          df <- as.data.frame(df)
          df$pna <- 0
          df$pna[nrow(df)] <- expenseValues1()$pna

          Missing <- setdiff(dput(names(df)), names(df1))  # Find names of missing columns
          df1[Missing] <- 0                    # Add them, filled with '0's
          df1 <- df1[,names(df)]

          df <- rbind(df1, df)
          df$fcf <- df$nocf - df$capex - df$pna
          df$Months <- seq(0,nrow(df)-1,1)





          df$pv10 <- df$fcf/(1.1^(df$Months/12))

          df2$IRR <- IRRcalc(df$fcf, df$Months)


          df2
        },
        error=function(e) {
          e
          NULL
        })

        )

        df <- bind_rows(econSummary)
        df <- df %>% filter(IRR >= (as.numeric(declineValues1()$cutoff)/100))
        #print(head(df))



        if(nrow(df) == 0){
          df <- data.frame('Summary' = 'No Economic Locations')

        } else {
          #print(head(df))
          df$oldPPF[is.na(df$oldPPF)] <- as.numeric(capexValues1()$ppfSelect)
          df$oldFPF[is.na(df$oldFPF)] <- as.numeric(capexValues1()$fpfSelect)
          df$wellSpacing <- 5280/(as.numeric(expenseValues1()$wellSpacing))
          df <- df[,c('subPlay', 'operator',
                      'wells', 'remWells', 'wellSpacing',
                      'qiOil', 'bOil', 'DiOil', 'DfOil', 'oilEUR',
                      'qiGas', 'bGas', 'DiGas', 'DfGas', 'gasEUR', 'dNc', 'dcet')]

          df$qiOilTotal <- df$qiOil*df$remWells
          df$qiGasTotal <- df$qiGas*df$remWells
          df$oilEURTotal <- df$oilEUR*df$remWells
          df$gasEURTotal <- df$gasEUR*df$remWells
          df$dNcTotal <- df$dNc*df$remWells
          df$dcetTotal <- df$dcet*df$remWells

          df <- df %>% filter(remWells > 0)
          viableAcreage <- nrow(df)*640/totalAcreage
          values$viableAcreage <- viableAcreage
          #print(viableAcreage)
          #print(summary(df))
          #print(head(df))
          if(nrow(df) == 0){
            df <- data.frame('Summary' = 'No Economic Locations')
          } else {
          
            df <- df %>% group_by(subPlay, operator) %>%
              summarise(wells = sum(wells), remWells = sum(remWells),
                        wellSpacing = mean(wellSpacing), qiOil = sum(qiOilTotal),
                        bOil = mean(bOil), DiOil = mean(DiOil), DfOil = mean(DfOil),
                        oilEUR = sum(oilEURTotal),
                        qiGas = sum(qiGasTotal), bGas = mean(bGas), DiGas = mean(DiGas),
                        DfGas = mean(DfGas), gasEUR = sum(gasEURTotal), dNc = sum(dNcTotal),
                        dcet = sum(dcetTotal)) %>% ungroup() %>%  mutate(qiOil = qiOil/remWells,
                                                                         qiGas = qiGas/remWells,
                                                                         oilEUR = oilEUR/remWells,
                                                                         gasEUR = gasEUR/remWells,
                                                                         dNc = dNc/remWells,
                                                                         dcet = dcet/remWells)
  
            #print(summary(df))
  
            df <- as.data.frame(df)
            #print(head(df))
  
            econSummary <- lapply(split(df, df[,'operator']), function (df1) tryCatch({
              df2 <- df1
              fitOil <- curtailed.q(arps.decline(
                as.numeric(df1$qiOil)*365, as.nominal(as.numeric(df1$DiOil)), as.numeric(df1$bOil), as.nominal(as.numeric(df1$DfOil))),
                as.numeric(declineValues1()$curtailOilS)/12.0,seq(0, declineValues1()$wellLifeS-1/12, by= (1/12)))/12
  
              fit <- as.data.frame(fitOil)
              rm(fitOil)
              names(fit) <- c('oilFCST')
              #if(input$curtailOil1 > 0){
              #  fit$oilFCST[1] <- fit$oilFCST[1]/2
              #}
  
  
              fit <- fit %>% mutate(prodMonth = nrow(fit)) %>% mutate(prodMonth = cumsum(prodMonth/prodMonth))
  
              fitGas <- curtailed.q(arps.decline(
                as.numeric(df1$qiGas)*365, as.nominal(as.numeric(df1$DiGas)), as.numeric(df1$bGas), as.nominal(as.numeric(df1$DfGas))),
                as.numeric(declineValues1()$curtailGasS)/12.0,seq(0, declineValues1()$wellLifeS-1/12, by= (1/12)))/12
              names(fitGas) <- c('gasFCST')
              fit$gasFCST <- fitGas
              #if(input$curtailGas1 > 0){
              #  fit$gasFCST[1] <- fit$gasFCST[1]/2
              #}
  
              fit <- fit[,c('prodMonth', 'oilFCST', 'gasFCST')]
              #names(fit) <- c('Months', 'Oil', 'Gas')
              names(fit) <- c('MONTH', 'OIL', 'GAS')
  
  
              fit3 <- fit[1:(nrow(fit)-1),]
              fit3 <- rbind(fit[1,], fit3)
              fit3$actOil <- fit$OIL
              fit3$actGas <- fit$GAS
  
              fit3$DiOil <- 1-(fit3$actOil/fit3$OIL)
              fit3$DiGas <- 1-(fit3$actGas/fit3$GAS)
  
              decl2Oil <- fit3$DiOil[2]
              decl2Gas <- fit3$DiGas[2]
              decl3Oil <- mean(fit3$DiOil[3:4])
              decl3Gas <- mean(fit3$DiGas[3:4])
              decl4Oil <- mean(fit3$DiOil[5:6])
              decl4Gas <- mean(fit3$DiGas[5:6])
              decl5Oil <- mean(fit3$DiOil[7:12])
              decl5Gas <- mean(fit3$DiGas[7:12])
              decl6Oil <- mean(fit3$DiOil[13:18])
              decl6Gas <- mean(fit3$DiGas[13:18])
              decl7Oil <- mean(fit3$DiOil[19:24])
              decl7Gas <- mean(fit3$DiGas[19:24])
              decl8Oil <- mean(fit3$DiOil[25:36])
              decl8Gas <- mean(fit3$DiGas[25:36])
  
              decl9Oil <- 1-sum(fit3$actOil[37:48])/sum(fit3$actOil[25:36])
              decl9Gas <- 1-sum(fit3$actGas[37:48])/sum(fit3$actGas[25:36])
  
              decl10Oil <- 1-sum(fit3$actOil[49:60])/sum(fit3$actOil[37:48])
              decl10Gas <- 1-sum(fit3$actGas[49:60])/sum(fit3$actGas[37:48])
  
              decl11Oil <- ((1-sum(fit3$actOil[61:72])/sum(fit3$actOil[49:60]))+
                              (1-sum(fit3$actOil[73:84])/sum(fit3$actOil[61:72]))+
                              (1-sum(fit3$actOil[85:96])/sum(fit3$actOil[73:84]))+
                              (1-sum(fit3$actOil[97:108])/sum(fit3$actOil[85:96]))+
                              (1-sum(fit3$actOil[109:120])/sum(fit3$actOil[97:108])))/5
  
              decl11Gas <- ((1-sum(fit3$actGas[61:72])/sum(fit3$actGas[49:60]))+
                              (1-sum(fit3$actGas[73:84])/sum(fit3$actGas[61:72]))+
                              (1-sum(fit3$actGas[85:96])/sum(fit3$actGas[73:84]))+
                              (1-sum(fit3$actGas[97:108])/sum(fit3$actGas[85:96]))+
                              (1-sum(fit3$actGas[109:120])/sum(fit3$actGas[97:108])))/5
  
  
              i <- 121
              j <- 132
              k <- as.integer((nrow(fit3) - j)/12)
              l <- 1
              dtx <- data.frame()
              while(l <= k){
                decl <- (1-sum(fit3$actOil[(i+12):(j+12)])/sum(fit3$actOil[(i):(j)]))
                dtx <- rbind(dtx, decl)
                l <- l+1
                i <- i+12
                j <- j+12
              }
              names(dtx) <- c('decl12Oil')
              decl12Oil <- mean(dtx$decl12Oil)
  
              i <- 121
              j <- 132
              k <- as.integer((nrow(fit3) - j)/12)
              l <- 1
              dtx <- data.frame()
              while(l <= k){
                decl <- (1-sum(fit3$actGas[(i+12):(j+12)])/sum(fit3$actGas[(i):(j)]))
                dtx <- rbind(dtx, decl)
                l <- l+1
                i <- i+12
                j <- j+12
              }
              names(dtx) <- c('decl12Gas')
              decl12Gas <- mean(dtx$decl12Gas)
  
              names(fit) <- c('Months', 'Oil', 'Gas')
  
              fit$Water <- fit$Oil + fit$Gas/20
              df <- fit
              rm(fit)
  
  
              #df$Oil <- df$Oil*expenseValues1()$wi/100
              #df$Gas <- df$Gas*expenseValues1()$wi/100
              #df$Water <- df$Water*expenseValues1()$wi/100
              df$Sales_Gas <- df$Gas*expenseValues1()$shrink
              df$NGL <- df$Gas*expenseValues1()$nglYield/1000
              #df$wi <- expenseValues1()$wi
              df1 <- data.frame(Months = seq(((declineValues1()$spudToProd-1)*-1), 0, 1), Oil = 0, Gas = 0, Water = 0)
              df$capex <- 0
              df1$capex <- 0
              df1$capex[1] <- df2$dcet
              #df1$capex[nrow(df1)] <- expenseValues1()$completeCost*expenseValues1()$wi/100
  
                prices <- values$price
                prices <- prices %>% filter(DATE >= today())
                prices <- prices[(declineValues1()$spudToProd):nrow(prices),]
                if(nrow(prices) > nrow(df)){
                  prices <- prices[1:nrow(df),]
                }
                df$oilPrice <- NA
                df$oilPrice[1:nrow(prices)] <- prices$WTI
                df$gasPrice <- NA
                df$gasPrice[1:nrow(prices)] <- prices$HH
                rm(prices)
                df$oilPrice <- na.locf(df$oilPrice)
                df$gasPrice <- na.locf(df$gasPrice)
                df$nglPrice <- df$oilPrice*expenseValues1()$nglDiff/100
  
              df$nri <- expenseValues1()$nri#*expenseValues1()$wi/100
              df$oilRev <- (df$oilPrice-expenseValues1()$oilDiff)*df$nri/100*df$Oil
              df$gasRev <- (df$gasPrice-expenseValues1()$gasDiff)*df$nri/100*df$Sales_Gas*expenseValues1()$btu
              df$nglRev <- (df$nglPrice)*df$nri/100*df$NGL
              df$rev <- df$oilRev+df$gasRev+df$nglRev
              df$tax <- df$oilRev*expenseValues1()$stxOil/100 + (df$gasRev+df$nglRev)*expenseValues1()$stxGas/100 +
                df$Oil*df$nri/100*expenseValues1()$oilSTX + df$Gas*df$nri/100*expenseValues1()$gasSTX +
                df$rev*df$nri/100*expenseValues1()$atx/100
  
              df$expense <- expenseValues1()$fixedCost + df$Gas*expenseValues1()$varExpGas +
                (df$Oil + df$NGL)*expenseValues1()$varExpBOE
  
  
              df$nocf <- df$rev-df$tax-df$expense
  
              # print(head(df))
  
              if(input$econAbanS == 'Yes'){
                df <- df[nrow(df):1,]
                df$cumNOCF <- cumsum(df$nocf)
                df$prev <- df$cumNOCF- dplyr::lag(df$cumNOCF, n = 1L)
                prev <- which(df$prev > 0)[1]
                df <- df[prev:nrow(df),]
                df <- df[nrow(df):1,]
                df <- subset(df, select = -c(cumNOCF, prev))
              }
              df <- as.data.frame(df)
              df$pna <- 0
              df$pna[nrow(df)] <- expenseValues1()$pna
  
              Missing <- setdiff(dput(names(df)), names(df1))  # Find names of missing columns
              df1[Missing] <- 0                    # Add them, filled with '0's
              df1 <- df1[,names(df)]
  
              df <- rbind(df1, df)
              df$fcf <- df$nocf - df$capex - df$pna
              df$Months <- seq(0,nrow(df)-1,1)
  
  
  
  
  
              df$pv10 <- df$fcf/(1.1^(df$Months/12))
  
              df2$IRR <- IRRcalc(df$fcf, df$Months)
  
              if(is.nan(decl2Oil)){
                decl2Oil <- 0
              }
              if(is.nan(decl3Oil)){
                decl3Oil <- 0
              }
              if(is.nan(decl4Oil)){
                decl4Oil <- 0
              }
              if(is.nan(decl5Oil)){
                decl5Oil <- 0
              }
              if(is.nan(decl6Oil)){
                decl6Oil <- 0
              }
              if(is.nan(decl7Oil)){
                decl7Oil <- 0
              }
              if(is.nan(decl8Oil)){
                decl8Oil <- 0
              }
              if(is.nan(decl9Oil)){
                decl9Oil <- 0
              }
              if(is.nan(decl10Oil)){
                decl10Oil <- 0
              }
              if(is.nan(decl11Oil)){
                decl11Oil <- 0
              }
              if(is.nan(decl12Oil)){
                decl12Oil <- 0
              }
  
              if(is.nan(decl2Gas)){
                decl2Gas <- 0
              }
              if(is.nan(decl3Gas)){
                decl3Gas <- 0
              }
              if(is.nan(decl4Gas)){
                decl4Gas <- 0
              }
              if(is.nan(decl5Gas)){
                decl5Gas <- 0
              }
              if(is.nan(decl6Gas)){
                decl6Gas <- 0
              }
              if(is.nan(decl7Gas)){
                decl7Gas <- 0
              }
              if(is.nan(decl8Gas)){
                decl8Gas <- 0
              }
              if(is.nan(decl9Gas)){
                decl9Gas <- 0
              }
              if(is.nan(decl10Gas)){
                decl10Gas <- 0
              }
              if(is.nan(decl11Gas)){
                decl11Gas <- 0
              }
              if(is.nan(decl12Gas)){
                decl12Gas <- 0
              }
  
              df2$decl2Oil <- percent(decl2Oil)
              df2$decl3Oil <- percent(decl3Oil)
              df2$decl4Oil <- percent(decl4Oil)
              df2$decl5Oil <- percent(decl5Oil)
              df2$decl6Oil <- percent(decl6Oil)
              df2$decl7Oil <- percent(decl7Oil)
              df2$decl8Oil <- percent(decl8Oil)
              df2$decl9Oil <- percent(decl9Oil)
              df2$decl10Oil <- percent(decl10Oil)
              df2$decl11Oil <- percent(decl11Oil)
              df2$decl12Oil <- percent(decl12Oil)
              df2$decl2Gas <- percent(decl2Gas)
              df2$decl3Gas <- percent(decl3Gas)
              df2$decl4Gas <- percent(decl4Gas)
              df2$decl5Gas <- percent(decl5Gas)
              df2$decl6Gas <- percent(decl6Gas)
              df2$decl7Gas <- percent(decl7Gas)
              df2$decl8Gas <- percent(decl8Gas)
              df2$decl9Gas <- percent(decl9Gas)
              df2$decl10Gas <- percent(decl10Gas)
              df2$decl11Gas <- percent(decl11Gas)
              df2$decl12Gas <- percent(decl12Gas)
              df2
            },
            error=function(e) {
              e
              NULL
            })
  
            )
  
            df <- bind_rows(econSummary)
            df <- as.data.frame(df)
            #print(head(df))
  
            df1 <- df[,c('decl2Oil', 'decl3Oil', 'decl4Oil',
                         'decl5Oil', 'decl6Oil', 'decl7Oil',
                         'decl8Oil', 'decl9Oil', 'decl10Oil',
                         'decl11Oil', 'decl12Oil')]
            df2 <- df[,c( 'decl2Gas', 'decl3Gas', 'decl4Gas',
                          'decl5Gas', 'decl6Gas', 'decl7Gas',
                          'decl8Gas', 'decl9Gas', 'decl10Gas',
                          'decl11Gas', 'decl12Gas')]
  
            df1 <- df1[1,]
            df2 <- df2[1,]
            names(df1) <- c('Month 2 Decl', 'Month 3-4 Decl',
                            'Month 5-6 Decl', 'Month 7-12 Decl',
                            'Month 13-18 Decl', 'Month 19-24 Decl',
                            'Month 25-36 Decl', 'Year 4 Decl',
                            'Year 5 Decl', 'Year 6-10 Decl', 'Year 11+ Decl')
  
            df1 <- t(df1)
  
            names(df2) <- c('Month 2 Decl', 'Month 3-4 Decl',
                            'Month 5-6 Decl', 'Month 7-12 Decl',
                            'Month 13-18 Decl', 'Month 19-24 Decl',
                            'Month 25-36 Decl', 'Year 4 Decl',
                            'Year 5 Decl', 'Year 6-10 Decl', 'Year 11+ Decl')
            df2 <- t(df2)
  
            df1 <- cbind(df1, df2)
            df1 <- as.data.frame(df1)
            names(df1) <- c('OIL', 'GAS')
            df1$TIME <- rownames(df1)
            df1 <- df1[,c('TIME', 'OIL', 'GAS')]
            #df1$ROW <- seq(1, 11, 1)
            values$wellDeclines <- df1
            # print((df1))
  
            df <- as.data.frame(df)
  
            df <- subset(df, select=-c(wells, decl2Oil, decl3Oil, decl4Oil,
                                       decl5Oil, decl6Oil, decl7Oil,
                                       decl8Oil, decl9Oil, decl10Oil,
                                       decl11Oil, decl12Oil,
                                       decl2Gas, decl3Gas, decl4Gas,
                                       decl5Gas, decl6Gas, decl7Gas,
                                       decl8Gas, decl9Gas, decl10Gas,
                                       decl11Gas, decl12Gas))
  
            df$wellSpacing <- as.integer(df$wellSpacing)
            df$qiOil <- as.integer(df$qiOil)
            df$bOil <- round(df$bOil, 2)
            #df$DiOil <- percent(round(as.effective(df$DiOil), 2))
            #df$DfOil <- percent(round(df$DfOil, 2))
            df$bGas <- round(df$bGas, 2)
            #df$DiGas <- percent(round(as.effective(df$DiGas), 2))
            #df$DfGas <- percent(round(df$DfGas, 2))
            df$oilEUR <- as.integer(df$oilEUR)
            df$gasEUR <- as.integer(df$gasEUR)
            df$qiGas <- as.integer(df$qiGas)
            df$dNcPerFt <- df$dNc/as.numeric(capexValues1()$perfSelect)
            df$dcetPerFt <- df$dcet/as.numeric(capexValues1()$perfSelect)
            #df$dNc <- dollar(df$dNc)
            #df$dcet <- dollar(df$dcet)
            df$dNcPerFt <- dollar(df$dNcPerFt)
            df$dcetPerFt <- dollar(df$dcetPerFt)
            df$IRR <- percent(df$IRR)
            #df$NPV <- dollar(df$NPV)
  
            #acres <- as.data.frame(values$leaseData) %>% filter(subPlay %in% input$subPlaySelect2)
            #acres <- acres$acreage
            #acres <- as.numeric(input$subPlayAcres)/acres
            #df$remWells <- as.integer(df$remWells*acres)
            df$id <- input$play
            #df <- merge(df, df3, by='operator', all.x=TRUE)
  
            df <- as.data.frame(df)
            df3 <- as.data.frame(df3)
            #print(head(df3))
  
            df$operator <- as.character(df$operator)
            df3$operator <- as.character(df3$operator)
            df <- merge(df, df3, by='operator', all.x=TRUE)
            #print(head(df))
            df$remWells <- as.integer(df$remWells/sum(df3$acres)*as.numeric(expenseValues1()$acres))
  
            df$acres <- as.integer(df$acres/sum(df3$acres)*as.numeric(expenseValues1()$acres))
            df$viableAcres <- percent(viableAcreage)
            #print(viableAcreage)
            #values$viableAcres <- viableAcreage
  
            df1 <- df() %>%
              filter(subPlay %in% input$subPlay1) %>%
              #filter(reservoir %in% input$reservoirSelect) %>%
              #filter(county %in% input$countySelect) %>%
              filter(operator %in% input$operator1) %>%
              group_by(operator) %>% summarise(oldWells=n(), oldPerf=as.integer(mean(perf)),
                                               oldPPF=as.integer(mean(ppf)), oldFPF = as.integer(mean(fpf))) %>% ungroup()
  
  
  
            df <- merge(df, df1, by='operator', all.x=TRUE)
            df$netWells <- as.integer(df$oldWells/sum(df3$acres)*as.numeric(expenseValues1()$acres))
            #df <- df %>% arrange(desc(acres))
            #print(head(df))
            #print(head(df))
            df$curtailOil <- declineValues1()$curtailOilS
            df$curtailGas <- declineValues1()$curtailGasS
  
            #print(head(df))
            df <- df[,c('subPlay', 'operator','acres', 'viableAcres', 'oldWells',  'oldPerf', 'oldPPF', 'oldFPF',
                        'wellSpacing', 'remWells', 'oilEUR', 'gasEUR', 'dNc', 'dcet', 'dNcPerFt', 'dcetPerFt',
                        'IRR', 'qiOil', 'DiOil', 'bOil', 'DfOil', 'curtailOil', 'qiGas', 'DiGas', 'bGas', 'DfGas', 'curtailGas')]
            values$wellCalc2 <- as.data.frame(df)
            names(df) <- c('Sub-Play', 'Operator',  'Acreage Estimate, acres', 'Viable Acreage, %',
                           'Historic Gross Wells', 'Historic Lateral Length, ft', 'Historic Proppant Loading, lb/ft',
                           'Historic Fluid Loading, gal/ft', 'Inter-well Spacing, ft', 'Remaining Inventory',
                           'Oil EUR, bbls', 'Gas EUR, mcf', 'D&C, $', 'DC&E, $', 'D&C/Ft', 'DC&E/Ft',  'IRR, %',
                           'Oil IP-30, bbl/d', 'Initial Decline Oil', 'b-Factor Oil', 'Terminal Decline Oil', 'Oil Curtailment, months',
                           'Gas IP-30, mcf/d', 'Initial Decline Gas', 'b-Factor Gas', 'Terminal Decline Gas', 'Gas Curtailment, months')
  
            #names(df) <- c('Sub Basin', 'Operator',  'Remaining Economic Wells', 'Well Spacing, ft', 'Oil IP-30, bbl/d', 'b-Factor Oil',
            #               'Initial Decline Oil', 'Terminal Decline Oil', 'Oil EUR, bbls', 'Wet Gas IP-30, mcf/d', 'b-Factor Gas',
            #               'Initial Decline Gas', 'Terminal Decline Gas', 'Gas EUR, mcf', 'Drill & Complete, $', 'Drill, Complete, and Equip, $','NPV, $', 'IRR, %',
            #               'D&C Per Ft, $', 'D&C+E Per Ft, $',
            #               'Reservoir', 'Gross Acreage Estimate, acres', 'Operator Wells in Selected Subplays', 'Operator Historical Lateral Length, ft',
            #               'Operator Historical Proppant Loading, lb/ft')
  
            #print(head(df))
          }
        }
      }
      values$wellCalc <- (df)
      updateActionButton(session, "calcInv",
                         label = 'CALCULATE')
      shinyjs::enable('calcInv')
    }
  })

  output$wellCalcs <- DT::renderDataTable({
    if(is.null(values$wellCalc)) {
      NULL
    } else {
      if(ncol(values$wellCalc) == 1){
        DT::datatable(values$wellCalc, rownames=TRUE,extensions = c('Buttons'),
                      options = list(dom='B', paging = FALSE,
                                     buttons = c('copy', 'csv', 'excel'),
                                     info = FALSE, ordering = FALSE),
                      caption = htmltools::tags$caption(
                        style = 'caption-side: bottom; text-align: center;',
                        'Table: ', htmltools::em('Remaining Economic Inventory Data')),
                      class = 'cell-border stripe')
      } else {
        declines <- values$wellCalc
        declines <- declines %>% mutate(PERF = as.numeric(capexValues1()$perfSelect),
                                        PPF = as.numeric(capexValues1()$ppfSelect),
                                        FPF = as.numeric(capexValues1()$fpfSelect))
        declines <- as.data.frame(t(declines))
        txt1 <- paste0(input$operator1,'/', input$plays,'/', input$subPlay1, collapse=' ')
        #print(txt1)
        names(declines) <- txt1


        expenseValues2 <- data.frame(
          Component = c('Fixed Expense/Month', 'Gas Variable Expense',
                        'Liquids Variable Expense','WTI Discount', 'Henry Hub Discount', 'NGL % WTI',
                        'Shrink',
                        'NGL Yield', 'BTU Uplift', 'Ad Val Tax Percent Revenue', 'Oil Severance Percent Revenue',
                        'Gas and NGL Severance Percent Revenue','Oil Severance Per BBL', 'Gas Severance Per MCF',
                        'Months Spud to Production', 'Net Revenue Interest', 'P & A'),
          Value = c(expenseValues1()$fixedCost,
                    expenseValues1()$varExpGas, expenseValues1()$varExpBOE,
                    expenseValues1()$oilDiff, expenseValues1()$gasDiff, expenseValues1()$nglDiff,
                    expenseValues1()$shrink, expenseValues1()$nglYield, expenseValues1()$btu,
                    expenseValues1()$atx, expenseValues1()$stxOil, expenseValues1()$stxGas,
                    expenseValues1()$oilSTX, expenseValues1()$gasSTX,
                    declineValues1()$spudToProd, expenseValues1()$nri,
                    expenseValues1()$pna))



        rownames(expenseValues2) <- expenseValues2$Component
        expenseValues2 <- subset(expenseValues2, select = -c(Component))
        #print(expenseValues2)
        #print(head(expenseValues1))
        #print(head(declines))
        names(expenseValues2) <- names(declines)
        declines <- rbind(as.data.frame(declines), as.data.frame(expenseValues2))
        #print(declines)

        values$declines <- declines

        DT::datatable(declines, rownames = TRUE,
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
                        'Table: ', htmltools::em('Remaining Economic Inventory Data')),
                      class = 'cell-border stripe')
      }
    }
  })
  
  
  output$wellCalcs1 <- DT::renderDataTable({
    if(is.null(values$wellDeclines)) {
      NULL
    } else {
      DT::datatable(values$wellDeclines, rownames=FALSE,extensions = c('Buttons'),
                    options = list(dom='B', paging = FALSE,
                                   buttons = c('copy', 'csv', 'excel'),
                                   info = FALSE, ordering = FALSE),
                    caption = htmltools::tags$caption(
                      style = 'caption-side: bottom; text-align: center;',
                      'Table: ', htmltools::em('Decline Inputs for GEM Python')),
                    class = 'cell-border stripe')
    }
  })
  
  
  latBinStart <- 24.116123
  longBinStart <- -121.496311
  
  fn <- function(x, y){
    distHaversine(c(x, y), c(x, latBinStart))*3.28084
    
  }
  fn1 <- function(x, y){
    distHaversine(c(x, y), c(longBinStart, y))*3.28084
  }
  
  fnx <- function(y){
    distHaversine(c(longBinStart, y), c(longBinStart, latBinStart))*3.28084
    
  }
  
  observe({
    if(is.null(input$subPlay1)||input$subPlay1 == ''){
      NULL
    } else {
      ogip1 <- ogip()
      ogip1 <- ogip1 %>% mutate(threeSecLocation = paste(round(distLat/(5280*3), digits=0),',', (round(distLong/(5280*3),digits=0)),sep=''),
                                twnRngLocation = paste(round(distLat/(5280*6), digits=0),',', (round(distLong/(5280*6),digits=0)),sep=''))
      
      df4 <-ogip1[,c('avLong', 'avLat', 'risk', 'twnRngLocation')] %>% group_by(twnRngLocation) %>% summarise_all(mean, na.rm=TRUE)
      df4 <- df4 %>% select(longitude = avLong, latitude = avLat, risk, twnRngLocation)
      #names(df4) <- c('longitude', 'latitude', 'risk', 'twnRngLocation')


      df5 <- df4 %>% group_by(twnRngLocation) %>% summarise(latitude = mean(latitude), longitude = mean(longitude))

      df51 <- df5 %>% mutate(latitude = latitude - 0.5, longitude = longitude - 0.5)
      df52 <- df5 %>% mutate(latitude = latitude + 0.5, longitude = longitude - 0.5)
      df53 <- df5 %>% mutate(latitude = latitude - 0.5, longitude = longitude + 0.5)
      df54 <- df5 %>% mutate(latitude = latitude + 0.5, longitude = longitude + 0.5)

      df51 <- rbind(df51, df52, df53, df54)

      k <- unique(df51$latitude)


      list1 <- mapply(fnx, k)
      list1 <- as.data.frame(list1)
      names(list1) <- 'distLat'
      list1$latitude <- k
      df51 <- merge(df51, list1, by='latitude', all.x=TRUE)
      #print(head(df51))

      df51 <- df51 %>%  mutate(distLong = pmap_dbl(list(x = longitude, y = latitude),fn1))
      df51 <- as.data.frame(df51)


      df51 <- df51 %>% mutate(twnRngLocation = paste(round(distLat/(5280*6), digits=0),',', (round(distLong/(5280*6),digits=0)),sep='')) %>%
        group_by(twnRngLocation) %>% summarise(latitude = mean(latitude), longitude = mean(longitude)) %>% filter(!twnRngLocation %in% df5$twnRngLocation)
      #print(head(df51))
      df5 <- df51
      df5$risk <- -0.1
      df5 <- df5[,c('longitude', 'latitude', 'risk')]
      #df4$risk <- -0.1

      #print(head(df5))
      #print(head(df4))
      df4 <- df5
      #df4 <- rbind(df4[,c('longitude', 'latitude', 'risk')], df5)
      df5 <- df4[1:4,]
      df5$latitude[1:2] <- max(ogip1$avLat)+.1
      df5$latitude[3:4] <- min(ogip1$avLat)-.1
      df5$longitude[2:3] <- max(ogip1$avLong)+.1
      df5$longitude[c(1,4)] <- min(ogip1$avLong)-.1
      df5$risk <- -0.1
      df4 <- rbind(df4, df5)
      names(df4) <- c('avLong', 'avLat', 'z')
      df4 <- df4 %>% distinct()
      df4 <- df4 %>% filter(!duplicated(paste0(avLong, avLat)))
      #print(head(df4))
      
      ogip1 <- ogip1 %>% group_by(threeSecLocation) %>% summarise(avLat =mean(avLat), avLong = mean(avLong), risk = mean(risk, na.rm=TRUE), gasFrac = mean(gasFrac, na.rm=TRUE))
      ogip1$perf <- as.numeric(capexValues1()$perfSelect)
      ogip1$ppf <- as.numeric(capexValues1()$ppfSelect)
      ogip1$fpf <- as.numeric(capexValues1()$fpfSelect)
      
  
      perfUplift <- perfUplift
      propUplift <- propUplift
      ogip1$x <- -0.2931903
      ogip1$y <-  -0.4256987
      ogip1$z <- 0.2750021
      ogip1$logPPF <- log(ogip1$ppf)
      ogip1$EUR <- exp(predict(propUplift, ogip1))
      ogip1$EUR <- ogip1$EUR*ogip1$risk
      ogip1$perfRisk <- perfUplift$coefficients[1] +
        perfUplift$coefficients[2]*ogip1$perf +
        perfUplift$coefficients[3]*(ogip1$perf**2)
      ogip1$EUR <- ogip1$EUR*ogip1$perfRisk
      ogip1$oilEUR <- ogip1$EUR*(1-ogip1$gasFrac)
      ogip1$gasEUR <- (ogip1$EUR-ogip1$oilEUR)*20
      ogip1$z <- ogip1$EUR
      #print(head(ogip1))
      df4 <- rbind(ogip1[,c('avLong', 'avLat', 'z')],df4)
      #df4 <- ogip1
      rm(ogip1)
      #print(head(df4))
      df4$z[df4$z > 1500] <- 1500
      df4 <- df4 %>% distinct()
      df4 <- df4  %>% filter(!duplicated(paste0(avLong, avLat)))
      values$df4 <- df4
      
      
      if(input$mapMetric == 'MCFE') {
        df4$z <- df4$z*20
      }
      
      
      df <- df4 %>% select(longitude = avLong, latitude=avLat, z)
      df <- df %>% arrange(longitude, latitude)
      df <- df %>% filter(!duplicated(paste0(longitude, latitude)))
      #print(head(df))
      d2d = interp(df$longitude, df$latitude, df$z)
      #contour(d2d$x, d2d$y, d2d$z)
      
      lines = contourLines(x=d2d$x, y=d2d$y, z=d2d$z, nlevels=8)
      
      d1 <- sapply(1:length(lines),function(i) Polygon(as.matrix(cbind(lines[[i]]$x,lines[[i]]$y))))
      d2 <- sapply(1:length(lines), function(i) Polygons(list(d1[[i]]),i))
      
      poly_data <- data.frame(Value = sapply(1:length(lines),function(i) lines[[i]]$level))
      
      dd2 <- SpatialPolygonsDataFrame(SpatialPolygons(d2),data=poly_data)
      proj4string(dd2) <- CRS("+proj=longlat +datum=WGS84")
      #values$dd2 <- dd2
      
      
      
      
      
      df <- df() %>% filter(subPlay %in% input$subPlay1) %>% filter(operator %in% input$operator1)
      #print(head(df))
      if(nrow(df)==0){
        LONGITUDE1 <- mean(df()$avLong)
        LATITUDE1 <- mean(df()$avLat)
      } else {
        LONGITUDE1 <- mean(df$avLong)
        LATITUDE1 <- mean(df$avLat)
      }
      
      values$LONGITUDE1 <- LONGITUDE1
      values$LATITUDE1 <- LATITUDE1
      #dd2 <- values$dd2
      
      factpal2 = colorFactor(rev(brewer.pal(n=11, name='Spectral')), dd2$Value)
      
      values$dd2 <- dd2
      values$factpal2 <- factpal2
      #dataMap <- wellData %>% filter(subBasin %in% dfy$subBasin) %>%
      #  filter(reservoir %in% dfy$reservoir)# %>% filter(subBasin %in% input$subBasinSelect)
      #dataMap$locationID <- as.character(dataMap$API)
      #dataMap$secondLocationID <- paste(as.character(dataMap$locationID), "_selectedLayer", sep="")
      #values$dataMap <- dataMap
      if(input$mapMetric == 'BOE'){
        
        
        values$map <- leaflet() %>% 
          addTiles(group = "OSM (default)") %>%
          addProviderTiles("Esri.WorldTopoMap", group = "ESRI") %>%
          addProviderTiles("Stamen.Toner", group = "Toner") %>%
          addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
          addLayersControl(baseGroups = c("OSM (default)", "ESRI", "Toner", "Toner Lite"),
                           options = layersControlOptions(collapsed = FALSE)) %>%
          #addCircles(data = dataMap, radius = 1, lat = dataMap$avLat, lng=dataMap$avLong, fillColor = NA, fillOpacity = 0.01,
          #           color = 'green', weight = 0.1, stroke = T, layerId = as.character(dataMap$locationID), highlightOptions = highlightOptions(
          #             color = 'mediumseagreen', opacity = 1, weight = 2, bringToFront = TRUE)) %>%
          addPolygons(
            data = dd2,
            stroke=FALSE, fillOpacity = 0.8, smoothFactor = 0.5,
            color='grey',
            fillColor = ~factpal2(Value),
            layerId=dd2@data$Value) %>%
          leaflet::addLegend(pal=factpal2, values=dd2$Value, opacity = 0.7, title='Normalized EUR per Well (20:1) MBOE', position = 'bottomright') %>%
          setView(lng = mean(LONGITUDE1),
                  lat = mean(LATITUDE1), zoom = 10)%>%
          addDrawToolbar(
            targetGroup='Selected',
            polylineOptions=FALSE,
            markerOptions = FALSE,
            polygonOptions = drawPolygonOptions(shapeOptions=drawShapeOptions(fillOpacity = 0
                                                                              ,color = 'white'
                                                                              ,weight = 3)),
            rectangleOptions = drawRectangleOptions(shapeOptions=drawShapeOptions(fillOpacity = 0
                                                                                  ,color = 'white'
                                                                                  ,weight = 3)),
            circleOptions = drawCircleOptions(shapeOptions = drawShapeOptions(fillOpacity = 0
                                                                              ,color = 'white'
                                                                              ,weight = 3)),
            editOptions = editToolbarOptions(edit = FALSE, selectedPathOptions = selectedPathOptions())) # %>%
        #addLayersControl(overlayGroups = c('draw'), options =
        #                  layersControlOptions(collapsed=FALSE))
      } else{
        
        values$map <- leaflet() %>% 
          addTiles(group = "OSM (default)") %>%
          addProviderTiles("Esri.WorldTopoMap", group = "ESRI") %>%
          addProviderTiles("Stamen.Toner", group = "Toner") %>%
          addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
          addLayersControl(baseGroups = c("OSM (default)", "ESRI", "Toner", "Toner Lite"),
                           options = layersControlOptions(collapsed = FALSE)) %>%
          #addCircles(data = dataMap, radius = 1, lat = dataMap$avLat, lng=dataMap$avLong, fillColor = NA, fillOpacity = 0.01,
          #           color = 'green', weight = 0.1, stroke = T, layerId = as.character(dataMap$locationID), highlightOptions = highlightOptions(
          #             color = 'mediumseagreen', opacity = 1, weight = 2, bringToFront = TRUE)) %>%
          addPolygons(
            data = dd2,
            stroke=FALSE, fillOpacity = 0.8, smoothFactor = 0.5,
            color='grey',
            fillColor = ~factpal2(Value),
            layerId=dd2@data$Value) %>%
          leaflet::addLegend(pal=factpal2, values=dd2$Value, opacity = 0.7, title='Normalized EUR per Well (20:1) MMCFE', position = 'bottomright') %>%
          setView(lng = mean(LONGITUDE1),
                  lat = mean(LATITUDE1), zoom = 10)%>%
          addDrawToolbar(
            targetGroup='Selected',
            polylineOptions=FALSE,
            markerOptions = FALSE,
            polygonOptions = drawPolygonOptions(shapeOptions=drawShapeOptions(fillOpacity = 0
                                                                              ,color = 'white'
                                                                              ,weight = 3)),
            rectangleOptions = drawRectangleOptions(shapeOptions=drawShapeOptions(fillOpacity = 0
                                                                                  ,color = 'white'
                                                                                  ,weight = 3)),
            circleOptions = drawCircleOptions(shapeOptions = drawShapeOptions(fillOpacity = 0
                                                                              ,color = 'white'
                                                                              ,weight = 3)),
            editOptions = editToolbarOptions(edit = FALSE, selectedPathOptions = selectedPathOptions())) # %>%
        #addLayersControl(overlayGroups = c('draw'), options =
        #                    layersControlOptions(collapsed=FALSE))
      }
      
      df <- df() 
      x <- paste0(levels(as.factor(df$state)))
      
      stateSelect <- state2abbr(x)
      
      stateSelect <- as.data.frame(stateSelect)
      
      stateSelect <- stateSelect %>% rowwise() %>% mutate(state1 = lookup_code(stateSelect))
      state1 <- stringr::str_sub(stateSelect$state1, -4, -3)
      
      leases1 <- leases() %>% filter(operator %in% input$operator1) %>% filter(subPlay %in% input$subPlay1) %>% filter(!duplicated(oneSecLocation))
      values$leases1 <- leases1
      me <- countyData
      me <- me[me@data$STATEFP %in% state1,]
      
      
      values$me <- me
      df <- df %>% filter(subPlay %in% input$subPlay1) %>% filter(operator %in% input$operator1)
      if(nrow(df) == 0){
        values$map1 <- values$map %>%
          addPolygons(
            data =me, 
            weight = 1,
            color = "black",
            fill = NA) %>%
          #addPolygons(data = df$geom) %>%
          addCircles(lng = leases1$long, lat = leases1$lat, color = 'yellow', radius = 804)
      } else {
        values$map1 <- values$map %>%
          addPolygons(
            data =me, 
            weight = 1,
            color = "black",
            fill = NA) %>%
          addCircles(lng = leases1$long, lat = leases1$lat, color = 'yellow', radius = 804) %>%
          addPolygons(data = df$geom, color = 'green')
      }
    }
    
  })
  
  observeEvent(input$mapMetric, {
    if(is.null(values$map)){
      NULL
    } else {
      df4 <- values$df4
      
      if(input$mapMetric == 'MCFE') {
        df4$z <- df4$z*20
      }
      
      
      df <- df4 %>% select(longitude = avLong, latitude=avLat, z)
      df <- df %>% arrange(longitude, latitude)
      d2d = interp(df$longitude, df$latitude, df$z)
      #contour(d2d$x, d2d$y, d2d$z)
      
      lines = contourLines(x=d2d$x, y=d2d$y, z=d2d$z, nlevels=8)
      
      d1 <- sapply(1:length(lines),function(i) Polygon(as.matrix(cbind(lines[[i]]$x,lines[[i]]$y))))
      d2 <- sapply(1:length(lines), function(i) Polygons(list(d1[[i]]),i))
      
      poly_data <- data.frame(Value = sapply(1:length(lines),function(i) lines[[i]]$level))
      
      dd2 <- SpatialPolygonsDataFrame(SpatialPolygons(d2),data=poly_data)
      proj4string(dd2) <- CRS("+proj=longlat +datum=WGS84")
      #values$dd2 <- dd2
      
      
      
      df <- df() %>% filter(subPlay %in% input$subPlay1) %>% filter(operator %in% input$operator1)
      
      
      LONGITUDE1 <- values$LONGITUDE1
      LATITUDE1 <- values$LATITUDE1 
      
      
      factpal2 = colorFactor(rev(brewer.pal(n=11, name='Spectral')), dd2$Value)
      
      values$dd2 <- dd2
      values$factpal2 <- factpal2
      #dataMap <- values$dataMap
      
      if(input$mapMetric == 'BOE'){
        values$map <- leaflet() %>% 
          addTiles(group = "OSM (default)") %>%
          addProviderTiles("Esri.WorldTopoMap", group = "ESRI") %>%
          addProviderTiles("Stamen.Toner", group = "Toner") %>%
          addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
          addLayersControl(baseGroups = c("OSM (default)", "ESRI", "Toner", "Toner Lite"),
                           options = layersControlOptions(collapsed = FALSE)) %>%
          #addCircles(data = dataMap, radius = 1, lat = dataMap$avLat, lng=dataMap$avLong, fillColor = NA, fillOpacity = 0.01,
          #           color = 'green', weight = 0.1, stroke = T, layerId = as.character(dataMap$locationID), highlightOptions = highlightOptions(
          #             color = 'mediumseagreen', opacity = 1, weight = 2, bringToFront = TRUE)) %>%
          addPolygons(
            data = dd2,
            stroke=FALSE, fillOpacity = 0.8, smoothFactor = 0.5,
            color='grey',
            fillColor = ~factpal2(Value),
            layerId=dd2@data$Value) %>%
          leaflet::addLegend(pal=factpal2, values=dd2$Value, opacity = 0.7, title='Normalized EUR per Well (20:1) MBOE', position = 'bottomright') %>%
          setView(lng = mean(LONGITUDE1),
                  lat = mean(LATITUDE1), zoom = 10)%>%
          addDrawToolbar(
            targetGroup='Selected',
            polylineOptions=FALSE,
            markerOptions = FALSE,
            polygonOptions = drawPolygonOptions(shapeOptions=drawShapeOptions(fillOpacity = 0
                                                                              ,color = 'white'
                                                                              ,weight = 3)),
            rectangleOptions = drawRectangleOptions(shapeOptions=drawShapeOptions(fillOpacity = 0
                                                                                  ,color = 'white'
                                                                                  ,weight = 3)),
            circleOptions = drawCircleOptions(shapeOptions = drawShapeOptions(fillOpacity = 0
                                                                              ,color = 'white'
                                                                              ,weight = 3)),
            editOptions = editToolbarOptions(edit = FALSE, selectedPathOptions = selectedPathOptions())) # %>%
        #addLayersControl(overlayGroups = c('draw'), options =
        #                  layersControlOptions(collapsed=FALSE))
      } else{
        values$map <- leaflet() %>% 
          addTiles(group = "OSM (default)") %>%
          addProviderTiles("Esri.WorldTopoMap", group = "ESRI") %>%
          addProviderTiles("Stamen.Toner", group = "Toner") %>%
          addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
          addLayersControl(baseGroups = c("OSM (default)", "ESRI", "Toner", "Toner Lite"),
                           options = layersControlOptions(collapsed = FALSE)) %>%
          #addCircles(data = dataMap, radius = 1, lat = dataMap$avLat, lng=dataMap$avLong, fillColor = NA, fillOpacity = 0.01,
          #           color = 'green', weight = 0.1, stroke = T, layerId = as.character(dataMap$locationID), highlightOptions = highlightOptions(
          #             color = 'mediumseagreen', opacity = 1, weight = 2, bringToFront = TRUE)) %>%
          addPolygons(
            data = dd2,
            stroke=FALSE, fillOpacity = 0.8, smoothFactor = 0.5,
            color='grey',
            fillColor = ~factpal2(Value),
            layerId=dd2@data$Value) %>%
          leaflet::addLegend(pal=factpal2, values=dd2$Value, opacity = 0.7, title='Normalized EUR per Well (20:1) MMCFE', position = 'bottomright') %>%
          setView(lng = mean(LONGITUDE1),
                  lat = mean(LATITUDE1), zoom = 10)%>%
          addDrawToolbar(
            targetGroup='Selected',
            polylineOptions=FALSE,
            markerOptions = FALSE,
            polygonOptions = drawPolygonOptions(shapeOptions=drawShapeOptions(fillOpacity = 0
                                                                              ,color = 'white'
                                                                              ,weight = 3)),
            rectangleOptions = drawRectangleOptions(shapeOptions=drawShapeOptions(fillOpacity = 0
                                                                                  ,color = 'white'
                                                                                  ,weight = 3)),
            circleOptions = drawCircleOptions(shapeOptions = drawShapeOptions(fillOpacity = 0
                                                                              ,color = 'white'
                                                                              ,weight = 3)),
            editOptions = editToolbarOptions(edit = FALSE, selectedPathOptions = selectedPathOptions())) # %>%
        #addLayersControl(overlayGroups = c('draw'), options =
        #                   layersControlOptions(collapsed=FALSE))
      }
      
      
      me <- values$me
      leases1 <- values$leases1
      
      df <- df() %>% filter(subPlay %in% input$subPlay1) %>% filter(operator %in% input$operator1)
      if(nrow(df) == 0){
        values$map1 <- values$map %>%
          addPolygons(
            data =me, 
            weight = 1,
            color = "black",
            fill = NA) %>%
          #addPolygons(data = df$geom) %>%
          addCircles(lng = leases1$long, lat = leases1$lat, color = 'yellow', radius = 804)
      } else {
        values$map1 <- values$map %>%
          addPolygons(
            data =me, 
            weight = 1,
            color = "black",
            fill = NA) %>%
          addCircles(lng = leases1$long, lat = leases1$lat, color = 'yellow', radius = 804) %>%
          addPolygons(data = df$geom, color = 'green')
      }
    }

    
    
  })

  output$map <- renderLeaflet({
    if(is.null(values$map)){
      NULL
    } else {
      values$map1
    }
  })
}

shinyApp(ui, server)