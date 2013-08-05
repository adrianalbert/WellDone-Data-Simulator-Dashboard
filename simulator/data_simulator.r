###########################################
# data_simulator.r
# 
# MoMo data simulator.
# 
# Uses DB schema in db/schema.js.
# 
# Adrian Albert
# Last modified: July 2013.
###########################################

rm(list = ls())

library('lubridate')
library('RMySQL')
library('rjson')
library('RJSONIO')
library('ggmap')
library('reshape')
library('pls')
library('fields')

options(error = recover)

setwd('/home/toni/Dropbox/WellDone/WellDone-Data-Simulator-Dashboard/')
source('./utils/df2JSON.r')

# load a list of cities per country & geographic position
# cities.all   = read.csv('./metadata/worldcitiespop.txt', sep = ',')

# load weather data
weather      = read.csv('./data/weather_data.csv', sep = ',')
stations     = read.csv('./data/weather_stations.csv', sep = ',')
weather$DateTime = as.POSIXct(as.character(weather$DateTime))

# ---------------------------------------------
# sites: Generate a few random sites in Africa
# ---------------------------------------------

# get a list of countries in Africa & cities
country.name = c('Ethiopia', 'Eritrea', 'Somalia', 'Kenya', 'Uganda', 'Djibouti')
cities.all   = read.csv('./metadata/worldcities.csv')
countries    = read.csv('./metadata/Countries.txt')
cities.all   = merge(cities.all, subset(countries, select = c('CountryId', 'Country')), 
                     by.x = 'CountryID', by.y = 'CountryId')
cities.all   = subset(cities.all, select = c('City', 'Latitude', 'Longitude', 'Country'))
names(cities.all)[c(2,3)]  = c('Lat', 'Long')
cities.all$Country = as.character(cities.all$Country)
cities.all$City    = as.character(cities.all$City)
cities       = subset(cities.all, Country %in% country.name)
cities       = droplevels(cities)

# select a few cities at random 
set.seed(0)
site.ids = sample(1:nrow(cities), 5)
sites    = subset(cities[site.ids,], select = c('City', 'Country', 'Lat', 'Long'))
sites    = cbind(ID = 0:(nrow(sites)-1), sites)
rownames(sites) = NULL

# save to CSV
write.csv(sites, './data/sites.csv')

# save to JSON
json = df2JSON(sites, tofile = './data/sites.json')

# -------------------------------------------------
# monitors: Generate sensors in selected locations
# -------------------------------------------------

# random string to mimic GSMID
generateRandomString <- function(n=1, lenght=12)
{
  randomString = sapply(1:n, function(i) paste(sample(c(0:9, letters, LETTERS), lenght, replace=TRUE), collapse=""))  
  return(randomString)
}

monitors = list()
for (i in 1:nrow(sites)) {  
  
  cur_site   = sites[i,]  
  no.sensors = 1 + sample(5, 1)
  name       = paste(cur_site$City, 'Location', 0:(no.sensors-1))
  gsm.ids    = generateRandomString(no.sensors, 20)
  sens.lat   = cur_site$Lat + (-0.5 + runif(no.sensors)) * 0.01
  sens.lon   = cur_site$Long + (-0.5 + runif(no.sensors)) * 0.01
  monitors[[i]] = data.frame(Name   = name, 
                             Lat    = sens.lat, 
                             Lon    = sens.lon, 
                             GSMID  = gsm.ids, 
                             SiteID = cur_site$ID,
                             Population = 50 + sample(100, 1) * sample(10, 1))
}
monitors = do.call('rbind', monitors)
monitors = cbind(ID = 0:(nrow(monitors)-1), monitors)

# save to CSV
write.csv(monitors, './data/monitors.csv')

# save to JSON
json = df2JSON(monitors, tofile = './data/monitors.json')

# -------------------------------------------------
# aggregate_reports: Generate sensor readings
# -------------------------------------------------

# sensor readings happen once daily...
dates   = seq(as.POSIXct('2010-08-01'), as.POSIXct('2013-06-01'), by=3600*24)

# ...at random hours during the day, whenever sunlight provides enough power
hours   = sample(10:18, length(dates), replace=TRUE)
minutes = sample(0:59, length(dates), replace=TRUE)

times   = dates + hours * 3600 + minutes * 60

# function to get weather data for closest weather station to a given location
getWeatherData = function(location) {
  location = as.matrix(t(location))
  distance = rdist.earth(location, stations[,c('LON', 'LAT')])
  idx.min  = which.min(distance)
  
  # get data corresponding to closest weather station
  USAF = stations$USAF[idx.min]
  tmp  = subset(weather, USAF == USAF) # & DateTime >= as.POSIXct('2013-05-01 00:00:00') )
  tmp$USAF = NULL
  return(tmp)
}

# function to generate consumption readings at given monitor according to weather
generateEventData = function(SensorID, times, weather) {

  # compute daily min, max, mean for each covariate for each day
  weather$date = as.Date(weather$DateTime)
  wthr.avg  = aggregate(data = weather, cbind(Temperature.C, RelativeHumidity) ~ date, FUN = mean)
  wthr.max  = aggregate(data = weather, cbind(Temperature.C, RelativeHumidity) ~ date, FUN = max)
  wthr.min  = aggregate(data = weather, cbind(Temperature.C, RelativeHumidity) ~ date, FUN = min)  
  names(wthr.avg)[-1] = paste(names(wthr.avg)[-1], 'avg', sep='.')
  names(wthr.min) = paste(names(wthr.min), 'min', sep='.')
  names(wthr.max) = paste(names(wthr.max), 'max', sep='.')
  wthr.data       = cbind(wthr.avg, wthr.min[,-1], wthr.max[,-1])  
  wthr.std        = wthr.data
  wthr.std$Intercept = 1

  # only include weather data within timestamps
  wthr.std     = subset(wthr.std, date %in% as.Date(times))
  idx.times    = which(wthr.std$date %in% as.Date(times))
  times        = times[idx.times]
    
  # generate random sensitivity to weather
  beta1 = c(runif(1), runif(3)/20, -runif(3)/20)
  beta2 = c(runif(1), runif(3)/20, -runif(3)/20)
  beta3 = c(runif(1), runif(3)/20, -runif(3)/20)
  
  # apply linear model: event counts  
  EventCount = round(as.matrix(wthr.std[,names(wthr.std[,-1])]) %*% as.matrix(beta1))
  
  # Battery voltage
  BatteryVoltage = as.matrix(wthr.std[,names(wthr.std[,-1])]) %*% as.matrix(beta2)

  # Flow/pulses
  HourlyPulses = round(as.matrix(wthr.std[,names(wthr.std[,-1])]) %*% as.matrix(beta3))
  
  # current hour
  CurrentHour = hour(times)

  # format data for output  
  df = data.frame(Timestamp      = times,
                  MonitorID      = SensorID,
                  Raw            = NA,
                  BatteryVoltage = BatteryVoltage,
                  SensorType     = 0,
                  CurrentHour    = CurrentHour,
                  HourCount      = NA,
                  EventCount     = EventCount,
                  HourlyPulses   = HourlyPulses)
  return(df)
}

# for each sensor, generate data
Reports = list()
for (i in 1:nrow(monitors)) {
  
  cat(paste('Generating data for sensor', i, '\n'))
  
  m = monitors[i,]
  
  # get appropriate weather data
  cur.wthr = getWeatherData(c(lon = m$Lon, lat = m$Lat))
  
  # generate data
  Reports[[i]] = generateEventData(m$ID, times, cur.wthr)
}
Reports = do.call('rbind', Reports)

write.csv(Reports, './data/reports.csv')
json = df2JSON(Reports, './data/reports.json')
