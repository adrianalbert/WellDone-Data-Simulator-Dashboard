
###########################################
# server.r
# 
# MoMo data mock-up visualizations in R.
# This is the server file for RShiny.
# 
# Adrian Albert
# Last modified: June 2013.
###########################################

# ------------------------------------
# Initializations
# ------------------------------------

# rm(list = ls())

library(shiny)
library(datasets)
library(ggplot2) # load ggplot
library(reshape)
library(lubridate)
library(ggmap)
library(rCharts)
library(fields)

options(RCHART_WIDTH = 800)

source('~/Dropbox/WellDone/Test/shiny_test/multiplot.r')

# ------------------------------------------------
# Load & pre-process data (one-time operation)
# ------------------------------------------------

# ______________________________________
# Sensor consumption data

# load data
# this may be later replaced with another mechanism for passing data around
load('~/Dropbox/WellDone/Test/test_data/MoMo_test_data.RData')

# pre-compute general statistics
df.tot  = subset(sens, select = c('ID', 'Time.stamp', 'Flow', 'Events', 'Precipitation'))
df.tot$Flow.Event = df.tot$Flow / df.tot$Events
df.flow = melt(df.tot[,-c(3,5)], id.vars = c('ID', 'Time.stamp'))
df.prec = melt(df.tot[,-c(2,6)], id.vars = c('ID', 'Precipitation'))

# pre-format sensor markers
master = master[1:4,]
names(master)[c(3,4)] = c('lat', 'lon')

# pre-load static map of data sensors
center   = c(lat = mean(master[,'lon']), lon = mean(master[,'lat']))
map.sens = get_googlemap(center = center, zoom = 12,
                         markers = master[,c('lon','lat')],
                         maptype = 'roadmap')

# ______________________________________
# Weather data

load('~/Dropbox/WellDone/Test/weather/data/d4/formatted_data.RData')

stations = subset(stations, COUNTRY == 'ETHIOPIA')
center   = c(lont = mean(stations[,'LON']), lat = mean(stations[,'LAT']))
map.wthr = get_googlemap(center = center, zoom = 6,
                    markers = stations[,c('LON','LAT')],
                    maptype = 'roadmap')
                      
# pre-load static map of sensors & weather stations
center   = c(lat = mean(c(master[,'lon'], stations[,'LON'])), 
             lon = mean(c(master[,'lat'], stations[,'LAT'])))
map.tot  = get_googlemap(center = center, zoom = 6, maptype = 'roadmap')

# ------------------------------------
# Set up Shiny server logic
# ------------------------------------

# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {
  
  # Reactive expression to generate the requested distribution. This is 
  # called whenever the inputs change. The renderers defined 
  # below then all use the value computed from this expression
  data <- reactive({  
    tmp    = subset(sens, ID == as.numeric(input$ID))
    tmp$ID = NULL
    tmp
  })  
  
  # weather data closest to the selected sensor
  data.wthr <- reactive({  
    # get location of current sensor
    location = master[which(master$ID == as.numeric(input$ID)), c('lon', 'lat')]
    
    # find closest weather station
    distance = rdist.earth(location, stations[,c('LON', 'LAT')])
    idx.min  = which.min(distance)
    
    # get data corresponding to closest weather station
    USAF = stations$USAF[idx.min]
    tmp  = subset(weather, USAF == USAF & DateTime >= as.POSIXct('2013-05-01 00:00:00') )
    tmp$USAF = NULL
    return(tmp)    
  })  
  
  # prepare Latex/Sweave report for selected location
  output$prepareReportBtn <- downloadHandler(
    filename = function() { paste(input$ID, '.csv', sep='') },
    content = function(file) {
      write.csv(data(), file)
    }
  )
  
  # Raw data view: Generate an HTML table view of the data
  output$raw <- renderTable({
    as.data.frame(data())
  })
  
  # Summary data view
  output$summary <- renderTable({
    df = data()
    df$Time.stamp = as.POSIXct(df$Time.stamp)
    summary(df)
  })
  
  # Individual sensor tab: statistics on selected sensor
  output$sensor <- renderChart({
    # events plot
    df = subset(data(), select = c('Events', 'Flow', 'SMS.ID'))
    df$SMS.ID = as.factor(df$SMS.ID)
    df = droplevels(df)
    # it seems rPlot does not like variable names that contain "."
    names(df) = gsub('\\.', '', names(df))
    p <- rPlot(Flow ~ Events, data = df, color = 'SMSID', type = 'point')  
    p$addParams(width = 600, height = 300, dom = 'sensor',
                title = "Flow and Events")    
    p$guides(x = list(title = "Events"))
    p$guides(y = list(title = "Flow"))
    # Step 6. print the chart (just type p1 if you are using it in your R console)
    return(p)
  })
  
  # Map tab: location of sensors
  output$map.sens <- renderPlot({
    # get general location
    p   = ggmap(map.sens, extent = 'device')
    p   = p + geom_point(data = master, aes(x = lon, y = lat, size = Beneficiaries),  alpha = 0.6)
    p   = p + scale_size(range=c(4,10))
    p   = p + geom_text(data = master, aes(x = lon, y = lat, label = ID))
    print(p)
  })
  
  # Map tab: alerts
  output$map.alerts <- renderPlot({
    # get general location
    p   = ggmap(map.sens, extent = 'device')
    p   = p + geom_point(data = master, aes(x = lon, y = lat, size = Beneficiaries),  alpha = 0.6)
    p   = p + geom_point(data = master[1,], aes(x = lon, y = lat, size = Beneficiaries),  color = 'orange', size = 14, alpha=0.7)
    p   = p + scale_size(range=c(4,10))
    p   = p + geom_text(data = master, aes(x = lon, y = lat, label = ID))
    p   = p + ggtitle('Unusual Activity Detected') + xlab('lon') + ylab('lat')
    p   = p + geom_text(data = data.frame(lon = master[1,'lon']*0.999, lat = master[1,'lat']*0.999, message = 'Sensor Down'),
                        aes(x = lon, y = lat, label = message), color = 'red', size = 8)
    print(p)
  })
  
  # Map tab: location of weather sensors
  output$map.tot <- renderPlot({
    # get general location
    p   = ggmap(map.tot, extent = 'device')
    p   = p + geom_point(data = stations, aes(x = LON, y = LAT, color = ELEVATION),  size = 4)
    p   = p + scale_color_continuous(low = 'white', high = 'red')
    p   = p + geom_point(data = master, aes(x = lon, y = lat, size = Beneficiaries), color = 'black',  alpha = 0.8)
    p   = p + scale_size(range=c(4,10))
    p   = p + geom_text(data = master, aes(x = lon, y = lat, label = ID))
    p   = p + ggtitle('Weather Stations Location')
    
    p2  = ggmap(map.sens, extent = 'device')
    p2  = p2 + geom_point(data = master, aes(x = lon, y = lat, size = Beneficiaries),  alpha = 0.6)
    p2  = p2 + scale_size(range=c(4,10))
    p2  = p2 + geom_text(data = master, aes(x = lon, y = lat, label = ID))    
    p2  = p2 + ggtitle('MoMo Sensors Locations')
    
    print(multiplot(p,p2,cols=2))
  })
  
  # Map tab: time series of weather
  output$weather.series <- renderPlot({
    # get general location
    df  = melt(data.wthr(), id.vars = c('DateTime'))
    p   = ggplot(data = df, aes(DateTime, value, color = variable))
    p   = p + facet_wrap(~variable, ncol = 1, scales = 'free')
    p   = p + geom_point(size = 1.2) + geom_line()
    print(p)
  })
  
  # General overview tab: statistics on all sensors
  output$general <- renderPlot({
    # flow and events per SMS
    p <- ggplot(df.flow, aes(as.factor(Time.stamp), value, color = as.factor(ID)))
    p <- p + geom_boxplot()
    p <- p + facet_wrap(~variable, ncol = 2, scales = 'free')
    p <- p + ggtitle('Flow per Data Collection Event')
    p <- p + xlab('Time Data Received') + ylab('')
    
    q <- ggplot(df.prec, aes(as.factor(Precipitation), value))
    q <- q + geom_boxplot(aes(color = as.factor(ID)), size=2)
    q <- q + facet_wrap(~variable, ncol = 2, scales = 'free')
    q <- q + ggtitle('Flow per Event')
    q <- q + xlab('Events') + ylab('Flow')
      
    print(multiplot(p,q))
  })
  
})
