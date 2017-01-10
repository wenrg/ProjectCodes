#### global.R ####
# library packages
library(shiny)
library(shinythemes)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(xts)
library(dygraphs)
library(leaflet)
library(RColorBrewer)
library(zoo)
library(reshape2)
library(wordcloud)


# load data
# storm events data
load("data/LatLngTime.RData")
# fatality data
load("data/FatalLoc.RData")
# word cloud data
load("data/desc_txt.RData")


# data for shiny
LatLngTime = LatLngTime %>% 
  mutate(., EVENT_DATE = as.Date(BEGIN_DATE_TIME, format = '%d-%b-%y %H:%M:%S'))
LatLngTime$DATE_YM = as.yearmon(as.character(LatLngTime$YEARMONTH), format = '%Y%m')
# bar chart: storm type vs deaths
dths = LatLngTime %>% group_by(., EVENT_TYPE) %>%
  summarise(., DEATHS = sum(DEATHS_DIRECT + DEATHS_INDIRECT)) %>%
  arrange(., DEATHS)
# dygraph: time vs. event type
fata = LatLngTime %>% 
  select(., DEATHS_DIRECT, DEATHS_INDIRECT, DATE_YM) %>%
  group_by(., DATE_YM) %>% 
  summarise(., Direct = sum(DEATHS_DIRECT), Indirect = sum(DEATHS_INDIRECT))
fata_xt = xts(fata[, -1], order.by = fata$DATE_YM)
# dygraph: time vs. fatality
types = LatLngTime %>% group_by(., DATE_YM, EVENT_TYPE) %>% 
  summarise(., TYPE_NUM = n())
types_tran = dcast(types, DATE_YM ~ EVENT_TYPE, value.var = 'TYPE_NUM')
types_xt = xts(types_tran, types_tran$DATE_YM)

# tab/panel choices variable
top6 = c('Flash Flood' = 'Flash Flood',
         'Flood' = 'Flood',
         'Hail' = 'Hail',
         'Marine Thunderstorm Wind' = 'Marine Thunderstorm Wind',
         'Thunderstorm Wind' = 'Thunderstorm Wind',
         'Tornado' = 'Tornado')
loss = c('Deaths - Direct' = 'DEATHS_DIRECT',
         'Deaths - Indirect' = 'DEATHS_INDIRECT',
         'Injuries - Direct' = 'INJURIES_DIRECT',
         'Injuries - Indirect' = 'INJURIES_INDIRECT',
         'Damage Property' = 'DAMAGE_PROPERTY',
         'Damage Crops' = 'DAMAGE_CROPS')


# color pal
colpal = colorFactor(RColorBrewer::brewer.pal(6, 'Set1'),
                     domain = c('Flash Flood', 'Flood', 'Hail', 'Marine Thunderstorm Wind',
                                'Thunderstorm Wind', 'Tornado'))
                                
                                


#### server.R ####
function(input, output) {
  
  # user selected input from tab-1 absolutePanel
  points = reactive({
    LatLngTime %>% filter(., EVENT_TYPE %in% input$top6, YEAR %in% input$year)
  })
  
  bar = reactive({
    df = LatLngTime %>% filter(., EVENT_TYPE %in% input$top6, YEAR %in% input$year) %>%
      select_(.dots = c('EVENT_TYPE', yloss = input$loss))
    df %>% group_by(., EVENT_TYPE) %>% summarise(., counts = sum(yloss, na.rm = T))
  })
  
  # tab-1 output
  mymap = leaflet() %>% setView(lat = 39.82, lng = -98.58, 4) %>%
    addProviderTiles('CartoDB.Positron') %>%
    addTiles(urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
             attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>') %>%
    addLegend("topright", pal = colpal, values =c('Flash Flood', 'Flood', 'Hail', 
              'Marine Thunderstorm Wind', 'Thunderstorm Wind', 'Tornado'), title = "Storm Type",
              opacity = 0.5)
  output$map = renderLeaflet(mymap)
  
  observe({
    if (nrow(points()) == 0) {leafletProxy('map') %>% clearShapes()} 
    else {
      leafletProxy('map', data = points()) %>% clearShapes() %>%
        addCircles(lat = ~ LATITUDE, lng = ~ LONGITUDE,
                   radius = 500, stroke = F, fillOpacity = 0.5, 
                   color = ~ colpal(EVENT_TYPE), 
                   popup = ~ paste(sep = '<br/>','Event Type:', EVENT_TYPE))}
  })
  
  output$bar = renderPlot({
    ggplot(bar(), aes_string(x = 'EVENT_TYPE', y = 'counts')) +
      geom_bar(aes_string(fill = 'EVENT_TYPE'), stat = 'identity', width = 0.1, alpha = 0.6) + 
      theme_economist() +
      scale_fill_economist() +
      ggtitle('Loss vs. Storm Events') +
      ylab('Counts or Damage (million $)') +
      theme(legend.position = 'none', axis.title.x=element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  })

  
  
  
  
  # tab-2 output
  output$dygraph1 = renderDygraph({
    dygraph(types_xt, main = 'Storm Events in U.S. 2007 - 2016', group = 'SE') %>%
      dyOptions(colors = RColorBrewer::brewer.pal(6, 'Paired'), fillGraph = T, fillAlpha = 0.2,
                includeZero = T, axisLineColor = '#386cb0') %>%
      dyHighlight(highlightCircleSize = 2, 
                  highlightSeriesBackgroundAlpha = 1,
                  hideOnMouseOut = F,
                  highlightSeriesOpts = list(strokeWidth = 1)) %>%
      dyAxis('x', drawGrid = F, axisLabelColor = 'white',
             axisLabelFormatter = "function(y){return y.getFullYear()}") %>%
      dyAxis('y', label = 'Event Counts', gridLineWidth = 0.1, axisLabelColor = 'white') %>%
      dyLegend(width = 215, show = 'onmouseover', labelsSeparateLines = T) %>% 
      dyRangeSelector(height = 20, fillColor = '#bdc9e1', strokeColor = '') %>% 
      dyRoller(rollPeriod = 1)
  })
  
  output$dygraph2 = renderDygraph({
    dygraph(fata_xt, main = 'Fatality in Storm Events', group = 'SE') %>%
      dyOptions(colors = RColorBrewer::brewer.pal(2, 'Set2'), fillGraph = T, fillAlpha = 0.4,
                includeZero = T, axisLineColor = '#386cb0') %>%
      dyHighlight(highlightCircleSize = 2, 
                  highlightSeriesBackgroundAlpha = 1,
                  hideOnMouseOut = F,
                  highlightSeriesOpts = list(strokeWidth = 1)) %>%
      dyAxis('x', drawGrid = F, axisLabelColor = 'white',
             axisLabelFormatter = "function(y){return y.getFullYear()}") %>%
      dyAxis('y', label = 'Deaths', gridLineWidth = 0.1, axisLabelColor = 'white') %>%
      dyLegend(width = 80, show = 'onmouseover', labelsSeparateLines = T) %>% 
      dyRangeSelector(height = 20, fillColor = '#bdc9e1', strokeColor = '') %>% 
      dyRoller(rollPeriod = 1)
  })

  
  
  
  # tab-3 output
  output$barplt1 = renderPlot({
    ggplot(FatalLoc,
           aes(x = reorder(FATALITY_LOCATION, FATALITY_LOCATION, function(x)-length(x)))) +
      geom_bar(aes(fill = FATALITY_SEX), alpha = 0.7) +
      theme_economist() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
            legend.position = 'top') +
      scale_fill_economist(name = '', label = c('Female', 'Male', 'Unknown')) +
      guides(color = 'legend') +
      ggtitle('Fatality Location') +
      xlab('') +
      ylab('Deaths')
  })
  
  output$barplt2 = renderPlot({
    ggplot(dths, aes(x = reorder(EVENT_TYPE, -DEATHS), y = DEATHS)) +
      geom_bar(aes(fill = EVENT_TYPE), stat = 'identity', alpha = 0.7, width = 0.6) +
      theme_economist() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
            legend.position = 'none') +
      scale_fill_economist() +
      ggtitle('Storm Types vs. Fatality') +
      xlab('') +
      ylab('Deaths')
  })
  
  output$boxplt = renderPlot({
    ggplot(FatalLoc, aes(x = FATALITY_SEX, y = FATALITY_AGE)) +
      geom_boxplot(aes_string(fill = 'FATALITY_SEX'), color = 'lightgray', alpha = 0.7, width = 0.3) +                 
      theme_economist() +
      scale_fill_economist(guide = 'none') +
      theme(axis.title.x=element_blank()) +
      ylab('Age') +
      scale_x_discrete(labels = c('Female', 'Male', 'Unknown'))
  })
  
  
  # tab-4 wordcloud data
  descr = reactive({desc_txt})
  
  # tab-4 wordcloud output
  output$wordcloud = renderPlot({
    par(bg = "#2B3E4F") 
    wordcloud(words = descr()$word, freq = descr()$freq, min.freq = input$min_freq,
              max.words = input$max_wds, random.order = FALSE, rot.per = 0.35, 
              colors = brewer.pal(8, 'Blues'))
  })
  
}




#### ui.R ####
shinyUI(navbarPage('Storm Events in U.S. 2007 - 2016',
                   theme = shinythemes::shinytheme('superhero'),
                   
                   tabPanel('Storm Events on Map',
                            div(class="outer",
                                tags$head(
                                  includeCSS("styles.css"),
                                  includeScript("gomap.js")),
                            leafletOutput('map', width = '100%', height = '100%'), 
                            absolutePanel(id = 'map_options', fixed = F, draggable = F,
                                          top = 50, left = 50, right ='auto', bottom = 'auto',
                                          width = 280, height = 'auto',
                                          cursor = 'inherit',
                                          h3(img(src = 'cloud.png', height = 40, width = 40),
                                             'Storm & Loss'),
                                          checkboxGroupInput('top6',
                                                             h4('Select storm event type:'),
                                                             choices = top6,
                                                             selected = c('Tornado', 'Thunderstorm Wind')),
                                          selectInput('loss',
                                                      h4('Select loss type:'),
                                                      choices = loss,
                                                      selected = 'DAMAGE_PROPERTY'),
                                          sliderInput('year',h4('Year'),
                                                      min = 2007, max = 2016, value = 2015:2016)),
                            absolutePanel(id = 'box', fixed = F, draggable = T,
                                          top = 300, left = 'auto', right = 10, bottom = 'auto',
                                          width = 300, height = 'auto', style = 'opacity: 0.8',
                                          cursor = 'inherit',
                                          plotOutput('bar', height = 300))
                            )),
                   
                   tabPanel('Storms on Time Series',
                            fluidRow(
                              column(1),
                              column(3,
                                     h2(''),
                                     h4('Time Series'),
                                     br('Interactive graphs:'),
                                     br('- Move the cursor along with the lines, the corresponding storm types and numbers will be shown at the top right.'),
                                     br('- Drag the selectors at the bottom, either one of them, to change the time range.'),
                                     br('- Change the roll period number at the bottom left, the average number of storms will be changed based on the roll period(months) you choose.'),
                                     br(),
                                     br(),
                                     br(),
                                     br('The information shown in the graphs:'),
                                     br('- The number of every storm type happened in every month, and the corresponding number of deaths.'),
                                     br('- The trend of storms: the peak, the pattern, inference for future.'),
                                     br('- The possible relation between storms and deaths.'),
                                     br()),
                               column(7,
                                     h2(''),
                                     dygraphOutput('dygraph1'),
                                     hr(),
                                     dygraphOutput('dygraph2')),
                               column(1))
                            ),

                   tabPanel('Fatality',
                            fluidRow(
                              column(1),
                              column(6,
                                     h2('Fatality: Location, Gender, Age'),
                                     plotOutput('barplt1'),
                                     hr(),
                                     plotOutput('barplt2')),
                              column(4,
                                     br(),
                                     br(),
                                     br(),
                                     br(),
                                     plotOutput('boxplt', height = 180, width = 270),
                                     h4('Graphs can answer...'),
                                     br('- What locations are more dangerous during a storm?'),
                                     br('- What is the gender ratio in each location?'),
                                     br('- What is the age range of deaths?'),
                                     br('- Which storm type are more dangerous?'),
                                     br('- ...')),
                              column(1))
                            ),
                   
                   tabPanel('Word Cloud',   
                            fluidRow(
                              column(1),
                              column(3,
                                     h2('Word Cloud'),
                                     br(),
                                     br(),
                                     sliderInput('min_freq',
                                                 h4('Minimum Frequency:'),
                                                 min = 1500,  max = 120000, value = 20000),
                                     sliderInput('max_wds',
                                                 h4('Maximum Number of Words:'),
                                                 min = 1,  max = 1000,  value = 400)),
                              column(7,
                                     h3('What are the words used most often'),
                                     h3('in the storm narrative?'),
                                     plotOutput('wordcloud',height = 500)),
                              column(1))
                            ),
                   
                   tabPanel('About',
                            fluidRow(
                              column(1),
                              column(9,
                                     h2('About This Shiny App'),
                                     hr(),
                                     h3('Dataset:'),
                                     a('NOAA',href="http://gis.ncdc.noaa.gov/all-records/catalog/search/resource/details.page?id=gov.noaa.ncdc:C00510/", target = "_blank"),
                                     br(),
                                     h3('Codes:'),
                                     a('NYC Data Science Academy Blog',href="http://blog.nycdatascience.com/author/liwen/", target = "_blank"),
                                     br(),
                                     br()),
                              column(1)),
                            absolutePanel(id = 'ds', fixed = F, draggable = F,
                                          top = 300, left = 290, right = 'auto', bottom = 'auto',
                                          width = 600, height = 'auto', style = 'opacity: 0.8',
                                          cursor = 'inherit',
                                          img(src = "ds.png", width = 600))
                            )
                   
               
))
