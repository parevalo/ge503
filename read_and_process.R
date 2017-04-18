library(readxl)
library(rgdal)
library(ggplot2)
library(tidyr)
library(dplyr)
library(lubridate)
library(plotly)
library(RColorBrewer)
library(tseries)
library(forecast)

# PT: Precipitación (total en mm)
# BS: Brillo Solar (hours)
# EV: Evaporación (mm)
# HR: Humedad Relativa (%)
# NB: Cloud cover (okta) (Nubosidad)
# PR: Dew point (%) (punto de rocio)
# TS: Temperatura superficial (C)
# TV: Tensión de vapor (?) 

# NV: Niveles
# QL: Caudales
# VD_1: Wind direction 
# VD_2, VD_D: wind speed (m/s) AND/OR wind speed (three observations)
# SD and ST: Most likely sediments
# 
# Numbers
# 1: Medio
# 2: Máximos
# 3: Mínimos
# 4: Total
# 5: Número de días
# 8: Mínimos
# 9: Máxima en 24h

#IDEAS

#' 1) See if there are significant differences in surface temperature across stations
#' depending on surrounding land cover types and other environmental variables, like 
#' precipitation, evaporation and others?
#' 2) Explore effect of precip/temp/etc on EVI at this scale and see if there are
#' differences between land cover types. 


################# LOAD AND TIDY DATA
setwd("/media/paulo/785044BD504483BA/OneDrive/Spring_2017/GE503/project/data")
#setwd("C:/OneDrive/Spring_2017/GE503/project/data")

# Read SHP with stations and get station ID's
stations <- readOGR(".", "filtered_stations_amazon_WGS84")
amazon <- readOGR(".", "amazon_boundary_WGS84")
stations_geo = stations@data[c("CODIGO_CAT", "LATITUD", "LONGITUD", "ALTITUD")] 
colnames(stations_geo) = c("station", "lat", "long", "elev")
stations_geo$elev = as.numeric(gsub(".", "", as.character(stations_geo$elev), fixed = T))


# Function to read all sheets of excel file
read_excel_allsheets <- function(filename) {
  sheets <- readxl::excel_sheets(filename)
  x <-    lapply(sheets, function(X) readxl::read_excel(filename, sheet = X, na="NaN"))
  names(x) <- sheets
  x
}

# Read data
sdata = read_excel_allsheets("IDEAM_COLOMBIA.xlsx")

# Gather each variable into its own tibble
gather_fnc = function(x){gather(x, key="station", value="value", -Fecha)}
full_vars_list = lapply(sdata, FUN=gather_fnc)
full_vars_names = names(full_vars_list)

# Add var name as a new column on each tibble
for(i in 1:length(full_vars_names)){
  full_vars_list[[i]]$variable = full_vars_names[i]
}

# Vars we care about
var_list = c("EV_4", "PT_4", "TS_1", "TV_1", "BS_4", "HR_1", "NB_1", "PR_1")  

# Filter only the stations we need for each of those vars and discard the rest
# We need this to reduce the final dataframe and avoid memory problems
full_vars_stations = lapply(full_vars_list, FUN=filter, station %in% stations_geo$station)
def_vars_list = full_vars_stations[var_list]

# Merge them all into a single almost tidy table!
def_vars = do.call(rbind, def_vars_list)
def_vars = rename(def_vars, date = Fecha)

# Spread variables into columns to make it easier to plot vars against each other
# The function takes care of the times and stations! WOW
tidy_vars = spread(def_vars, variable, value)
rm(full_vars_list)

# Remove all data before 1980
def_vars = filter(def_vars, date > "1980-01-01")
tidy_vars = filter(tidy_vars, date > "1980-01-01")

# Merge other vars like latlong, elevation
def_vars = full_join(def_vars, stations_geo, by = "station")
tidy_vars = full_join(tidy_vars, stations_geo, by = "station")

# Calculate mean of stations and  filter only those that have observations for ALL variables of interest.
tidyvars_summary = tidy_vars %>%
  group_by(station) %>%
  summarise_each(funs(mean(.,na.rm=T))) %>%
  filter(complete.cases(.))


#options(tibble.print_max = Inf)
best_stations = tidyvars_summary$station

# Filter only the best stations
def_filtered = filter(def_vars, station %in% best_stations)
tidy_filtered = filter(tidy_vars, station %in% best_stations)

###############PLOTS

# Test plotting with first case
plot1 = ggplot(data=def_filtered) + geom_point(mapping = aes(x=date, y=value, colour=station, group=station)) + facet_wrap(~station, scales="free")
plot1
plot1 %+% filter(def_vars, variable == 'PT_4' & station == 31015010)
plot1 %+% filter(def_vars, variable == 'TS_1' & station == 31015010)
plot1 %+% filter(def_vars, variable == 'EV_4' & station == 31015010)

# Test plotting with tidy vars
plot1 = ggplot(data=tidy_filtered) + geom_line(mapping = aes(x=date, y=TS_1, group=station))+ facet_wrap(~station)
plot1

# Plot a single station
plot1 = ggplot(data=filter(tidy_filtered, station == 44035030)) + geom_line(mapping = aes(x=date, y=TS_1))
plot1

# Plot multiple varibles per station
plot2 = ggplot(data=filter(def_filtered, station == 31015010)) + geom_line(mapping = aes(x=date, y=value, color=variable, group=variable)) +facet_wrap(~station, scales="free")
plot2

# Or all variables for all stations...
plot2 = ggplot(data=def_filtered) + geom_line(mapping = aes(x=date, y=value, color=variable, group=variable)) +facet_wrap(~station, scales="free")
plot2

# Plot two or more specific vars
plot3 = ggplot(data=filter(tidy_vars, station == 44045010)) + geom_line(mapping = aes(x=date, y=PR_1)) + 
  geom_line(mapping = aes(x=date, y=TS_1))  
plot3


# Plot with those, this is the best wrap option
plot4 = ggplot(data=tidy_filtered) + geom_point(mapping = aes(x=TS_1, y=PR_1, group=station, color=station))+ facet_wrap(~station)
plot4
ggplotly(plot4)

 # Plot separately 
for(s in best_stations){
  print(s)
  pt = ggplot(data=filter(tidy_filtered, station == as.numeric(s))) + geom_point(mapping = aes(x=TS_1, y=PT_4)) + ggtitle(s)
  print(pt)
}


# Two variables clustered by station ina  single plot
plot6 = ggplot(data=tidy_filtered) + geom_point(mapping = aes(x=TS_1, y=PR_1, group=station, color=station))
plot6
ggplotly(plot6)

# Or Plot single pair of vars for all stations
ggplot(data=tidy_filtered) + geom_point(mapping = aes(x=TS_1, y=PT_4)) + facet_wrap(~station)

# Plot two variables clustered by lat/long
plot7 = ggplot(data=tidy_filtered) + geom_point(mapping = aes(x=TS_1, y=PR_1, color=long))
plot7

plot7 = ggplot(data=tidy_filtered) + geom_point(mapping = aes(x=TS_1, y=PR_1, group=long, colour=long)) 
  #scale_colour_gradient(low = "blue", high = "green")
plot7
ggplotly(plot7)

# Descriptive stats
plot8 = ggplot(data=tidy_filtered) + geom_boxplot(mapping = aes(x=station, y=BS_4))
plot8
ggplotly(plot8)
str(plot8)


# Seasonal plot
test = filter(tidy_filtered, station == 31015010) %>%
    mutate(month=format(date, "%m"), year=format(date, "%Y")) %>%  
      group_by(month, year) %>%
        summarise(total = sum(BS_1))
          

seasonal_plot = ggplot(test, aes(year, total)) + geom_point(data=test, mapping=aes(color=year)) + facet_grid(.~month) +
  theme(legend.title = element_blank(), axis.title.y=element_blank(),axis.text.x=element_blank()) + 
  xlab(paste("Years:",min(test$year),"to", max(test$year)))
seasonal_plot

# Line seasonal plot using ggseasonplot from the forecast package
ts_test = filter(tidy_filtered, station == 31015010)$TS_1
ts_test = ts(ts_test, start=c(1980,2), end=c(2014,11), frequency=12)
ts_plot = ggseasonplot(ts_test)
ggplotly(ts_plot)

#TODO
# Create summary statistics and map those
# Find "outlier" stations and outlier values

#Summary stats per station/variable
summary_stats = def_filtered %>%
  group_by(station, variable) %>%
    summarise(sum = sum(value, na.rm=T),
              avg = mean(value, na.rm=T),
              min = min(value, na.rm=T),
              max = max(value, na.rm=T),
              lat = mean(lat),
              long = mean(long),
              count = n())

summary_stats  

##### MAPS

# Map of stations
ggplot() +  geom_polygon(data=amazon, aes(x=long, y=lat, group=group)) + 
  geom_point(data=tidy_vars, aes(x=long, y=lat), color="red") 

# Plot stats!
ggplot() +  geom_polygon(data=amazon, aes(x=long, y=lat, group=group)) + 
    geom_point(data=filter(summary_stats, variable == "TS_1"), aes(x=long, y=lat, size=avg)) 



