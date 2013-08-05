
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

rm(list = ls())

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

# load sites, monitors, data
sites    = read.csv('~/Dropbox/WellDone/WellDone-Data-Simulator-Dashboard/data/sites.csv')
monitors = read.csv('~/Dropbox/WellDone/WellDone-Data-Simulator-Dashboard/data/monitors.csv')
reports  = read.csv('~/Dropbox/WellDone/WellDone-Data-Simulator-Dashboard/data/reports.csv')
sites    = sites[,-1]
reports  = reports[,-1]
monitors = monitors[,-1]

# pre-load static map of data sensors
center   = c(lon = mean(monitors[,'Lon']), lat = mean(monitors[,'Lat']))
map.sens = get_googlemap(center = center, zoom = 5,
                         markers = monitors[,c('Lon','Lat')],
                         maptype = 'roadmap')
p   = ggmap(map.sens, extent = 'device')

# ______________________________________
# Weather data

load('/home/toni/Dropbox/WellDone/weather/data/d4/formatted_data.RData')

stations = subset(stations, COUNTRY == 'ETHIOPIA')
center   = c(lont = mean(stations[,'LON']), lat = mean(stations[,'LAT']))
map.wthr = get_googlemap(center = center, zoom = 5,
                    markers = stations[,c('LON','LAT')],
                    maptype = 'roadmap')
                      
# pre-load static map of sensors & weather stations
center   = c(lon = mean(c(monitors[,'Lon'], stations[,'LON'])), 
             lat = mean(c(monitors[,'Lat'], stations[,'LAT'])))
map.tot  = get_googlemap(center = center, zoom = 5, maptype = 'roadmap')

# ------------------------------------
# Set up Shiny server logic
# ------------------------------------

# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {
  
  # Reactive expression to generate the requested distribution. This is 
  # called whenever the inputs change. The renderers defined 
  # below then all use the value computed from this expression
  data <- reactive({  
    tmp    = subset(reports, MonitorID == as.numeric(input$ID))
    tmp
  })  
  
  # weather data closest to the selected sensor
  data.wthr <- reactive({  
    # get location of current sensor
    location = monitors[which(monitors$ID == as.numeric(input$ID)), c('Lon', 'Lat')]
    
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
    df$Time.stamp = as.POSIXct(df$Timestamp)
    summary(df)
  })
  
  # Individual sensor tab: statistics on selected sensor
  output$sensor <- renderChart({
    # events plot
    df = subset(data(), select = c('Timestamp', 'EventCount', 'HourlyPulses', 'CurrentHour'))
    df = droplevels(df)
    # it seems rPlot does not like variable names that contain "."
    names(df) = gsub('\\.', '', names(df))
    p <- rPlot(EventCount ~ Timestamp, data = df, color = 'CurrentHour', type = 'point')  
    p$addParams(width = 800, height = 500, dom = 'sensor',
                title = "Events History")    
    p$guides(x = list(title = "Time"))
    p$guides(y = list(title = "Events"))
    # Step 6. print the chart (just type p1 if you are using it in your R console)
    return(p)
  })
  
  # Map tab: location of sensors
  output$map.sens <- renderPlot({
    # get general location
    p   = ggmap(map.sens, extent = 'device')
    p   = p + geom_point(data = monitors, aes(x = Lon, y = Lat, size = Population),  alpha = 0.6)
    p   = p + scale_size(range=c(4,10))
    p   = p + geom_text(data = monitors, aes(x = Lon, y = Lat, label = ID))
    print(p)
  })
  
  # Map tab: alerts
  output$map.alerts <- renderPlot({
    # get general location
    p   = ggmap(map.sens, extent = 'device')
    p   = p + geom_point(data = monitors, aes(x = Lon, y = Lat, size = Population),  alpha = 0.6)
    p   = p + geom_point(data = monitors[1,], aes(x = Lon, y = Lat, size = Population),  color = 'orange', size = 14, alpha=0.7)
    p   = p + scale_size(range=c(4,10))
    p   = p + geom_text(data = monitors, aes(x = Lon, y = Lat, label = ID))
    p   = p + ggtitle('Unusual Activity Detected') + xlab('lon') + ylab('lat')
    p   = p + geom_text(data = data.frame(lon = monitors[1,'Lon']*0.999, lat = monitors[1,'Lat']*0.999, message = 'Sensor Down'),
                        aes(x = lon, y = lat, label = message), color = 'red', size = 8)
    print(p)
  })
  
  # Map tab: location of weather sensors
  output$map.tot <- renderPlot({
    # get general location
    p   = ggmap(map.tot, extent = 'device')
    p   = p + geom_point(data = stations, aes(x = LON, y = LAT, color = ELEVATION),  size = 4)
    p   = p + scale_color_continuous(low = 'white', high = 'red')
    p   = p + geom_point(data = monitors, aes(x = Lon, y = Lat, size = Population), color = 'black',  alpha = 0.8)
    p   = p + scale_size(range=c(4,10))
    p   = p + geom_text(data = monitors, aes(x = Lon, y = Lat, label = ID))
    p   = p + ggtitle('Weather Stations and Sensor Location')
    
    p2  = ggmap(map.sens, extent = 'device')
    p2  = p2 + geom_point(data = monitors, aes(x = Lon, y = Lat, size = Population),  alpha = 0.6)
    p2  = p2 + scale_size(range=c(4,10))
    p2  = p2 + geom_text(data = monitors, aes(x = Lon, y = Lat, label = ID))    
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
    p <- ggplot(reports, aes(as.factor(CurrentHour), BatteryVoltage))
    p <- p + geom_boxplot()
    # p <- p + facet_wrap(~variable, ncohl = 2, scales = 'free')
    p <- p + ggtitle('Monitor Health: Voltage')
    p <- p + xlab('Monitor ID') + ylab('')
    
    q <- ggplot(reports, aes(as.factor(CurrentHour), HourlyPulses))
    q <- q + geom_boxplot()
    # q <- q + facet_wrap(~variable, ncol = 2, scales = 'free')
    q <- q + ggtitle('Monitor Health: Pulses')
    q <- q + xlab('Monitor ID') + ylab('')
      
    print(multiplot(p,q))
  })
  
})
