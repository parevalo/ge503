library(tidyverse)
library(rgdal)
library(lubridate)
library(plotly)
library(RColorBrewer)
library(tseries)
library(forecast)
library(openair)


# PT: Precipitación (total en mm)
# BS: Brillo Solar (hours)
# EV: Evaporación (mm)
# HR: Humedad Relativa (%)
# NB: Cloud cover (okta) (Nubosidad)
# PR: Dew point (%) (punto de rocio)
# TS: Temperatura superficial (C)
# TV: Tensión de vapor (hPa) 

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
rm(sdata) 

# Add var name as a new column on each tibble
for(i in 1:length(full_vars_names)){
  full_vars_list[[i]]$variable = full_vars_names[i]
}

# Var codes we care about and their labels
var_list = c("PT_4", "PT_5", "PT_9", "BS_4", "EV_4", "HR_1", "NB_1", "PR_1", "TS_1", "TS_2", "TS_8", "TV_1")  
var_names = c("Total precipitation (mm)", "Precipitation (# of days)", "Max precipitation in 24 hrs", "Total solar brightness (hours)",
              "Total evaporation (mm)", "Median relative humidity (%)", "Median cloud cover (okta)", "Median dew point (%)", 
              "Median surface temperature (°C)", "Max surface temperature (°C)", "Min surface temperature (°C)", 
              "Median vapor pressure (hPa)")

# Filter only the stations we need for each of those vars and discard the rest
# We need this to reduce the final dataframe and avoid memory problems
full_vars_stations = lapply(full_vars_list, FUN=filter, station %in% stations_geo$station)
def_vars_list = full_vars_stations[var_list]

# Merge them all into a single almost tidy table!
def_vars = do.call(rbind, def_vars_list)
def_vars = rename(def_vars, date = Fecha)

# Remove all data before 1985 and outliers, and other transformations
def_vars = filter(def_vars, date > "1985-01-01")
def_vars = filter(def_vars, !(variable == 'EV_4' & value > 250)) 

# Add labels for stations per land cover type/transformation levels
low_transf = c(48015010, 31095010, 32155010, 42075010, 44135010, 47075010)
medium_transf = c(32035010, 32035020, 44115020, 47045010, 44055010)
other = c(44015040, 44015010) # Places with elevation 650 and 440, all the others are <300, just for reference
def_vars = def_vars %>% mutate(lc_change = ifelse(station %in% low_transf, "Low", ifelse(station %in% medium_transf, "Medium", "High")))
def_vars$lc_change = factor(def_vars$lc_change, levels = c("Low", "Medium", "High"), ordered = T)

# Spread variables into columns to make it easier to plot vars against each other
# The function takes care of the times and stations! WOW
tidy_vars = spread(def_vars, variable, value)
rm(full_vars_list)

# Merge other vars like latlong, elevation
def_vars = full_join(def_vars, stations_geo, by = "station")
tidy_vars = full_join(tidy_vars, stations_geo, by = "station")

# Calculate mean of stations and  filter only those that have observations for ALL variables of interest.
tidyvars_summary = tidy_vars %>%
  group_by(station) %>%
  summarise_each(funs(mean(.,na.rm=T)), -lc_change) %>%
  filter(complete.cases(.))


#options(tibble.print_max = Inf)
best_stations = tidyvars_summary$station

# Also filter stations in higher elevations 
best_stations = best_stations[!(best_stations %in% c(47015040, 47015080, 47015100, 44015030))]

# Filter only the best stations
def_filtered = filter(def_vars, station %in% best_stations)
tidy_filtered = filter(tidy_vars, station %in% best_stations)

# Filter stationsgeo and sort to lat or long
stations_lat = filter(stations_geo, station %in% best_stations) %>%
  arrange(., desc(lat))

stations_long = filter(stations_geo, station %in% best_stations) %>%
  arrange(., long)


# Create copies with stations ordered as lat or long. Maybe there is an easier way!?
def_filtered_lat = def_filtered
def_filtered_long = def_filtered
tidy_filtered_lat = tidy_filtered
tidy_filtered_long = tidy_filtered

# Convert stations to factors andr reorder levels to show stations in lat or long order
def_filtered_lat$station = factor(def_filtered$station, levels = stations_lat$station, ordered=TRUE)
def_filtered_long$station = factor(def_filtered$station, levels = stations_long$station, ordered=TRUE)

tidy_filtered_lat$station = factor(tidy_filtered$station, levels = stations_lat$station,  ordered = TRUE)
tidy_filtered_long$station = factor(tidy_filtered$station, levels = stations_long$station,  ordered = TRUE)

############### Test for trends with TheilSen from openair package
acf(filter(def_filtered, station == 44115020 &  variable == "TS")$value, na.action = na.pass)

# Use theilsen slope test, amore modern "version" of mann kendall.  Accounts for
# autocorrelation and it is strong to outliers.Run per station or pooled. If pooled, statistical positive trend. 
# Can i do that?

theil2 = TheilSen(testmk, pollutant = 'value', autocor = T, deseason = T, lab.cex = 2, xlab = "Year")
plot(theil2)

#Do it for all vars
vars_theil = list()
for(i in 1:length(var_list)){
  theil_subset = filter(def_filtered, variable == var_list[i])
  vars_theil[[i]] = TheilSen(theil_subset, pollutant = 'value', type='station', autocor = T, ylab=var_names[i], 
                             xlab = "Year")
}

# Save plots for each variable
for(i in 1:length(var_list)){
  png(paste0("../results/plots/theilsen/", var_list[i], "_stations.png"), width = 1920, height = 1080, res=120)
  plot(vars_theil[[i]])
  dev.off()
}

# Extract station, pstat and slope
cols=c("station", "p.stars", "slope")
slopes_per_station= data.frame()

for(i in 1:length(var_list)){
  # Retrieve results from test
  slope_info = vars_theil[[i]]$data$res2[cols]
  # Retrieve which rows have any value in the slope field and subset
  bool_ind = !is.na(slope_info$slope)
  slope_station = slope_info[bool_ind,]
  # Add new field with variable name
  slope_station$variable = var_list[i]
  # Bind iteratively
  slopes_per_station = rbind(slopes_per_station, slope_station)
}

slopes_per_station

###############PLOTS

# Test plotting with first case
# plot1 %+% filter(def_vars, variable == 'PT_4' & station == 31015010)
# plot1 %+% filter(def_vars, variable == 'TS_1' & station == 31015010)
# plot1 %+% filter(def_vars, variable == 'EV_4' & station == 31015010)

# Time vs single variable for all stations
plot = ggplot(data=tidy_filtered_lat) + geom_line(mapping = aes(x=date, y=TS_2, group=station, color=station))+ facet_wrap(~station) 
ggplotly(plot)

for(v in var_list){
  ggplot(data=tidy_filtered) + geom_line(mapping = aes(x=date, y=get(v), group=station))+ facet_wrap(~station) + ylab(v)
  ggsave(paste0("../results/plots/", v, "_per_station.png"), width = 20, height = 10, units = 'cm')
}

# Var1 vs var2
for(i in 1:length(var_list)){
  for(j in 1:length(var_list)){
    ggplot(data=tidy_filtered) + geom_line(mapping = aes(x=get(var_list[i]), y=get(var_list[j]), group=station))+ facet_wrap(~station) + 
      xlab(var_names[i]) + ylab(var_names[j])
    ggsave(paste0("../results/plots/var1_vs_var2_stations/", var_list[i], "_vs_", var_list[j], "_per_station.png"), width = 40, height = 22, units='cm')
  }
}


# Plot a single station
plot1 = ggplot(data=filter(tidy_filtered, station == 44035030)) + 
  geom_line(mapping = aes(x=date, y=TV_1))  +  geom_line(mapping = aes(x=date, y=PR_1))
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
plot4 = ggplot(data=tidy_filtered_long) + geom_point(mapping = aes(x=TS_1, y=PR_1, group=station, color=station))+ facet_wrap(~station)
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
plot7 = ggplot(data=tidy_filtered) + geom_point(mapping = aes(x=TS_1, y=PR_1, color=lat))
plot7

plot7 = ggplot(data=tidy_filtered) + geom_point(mapping = aes(x=TS_1, y=PR_1, group=long, colour=long)) 
  #scale_colour_gradient(low = "blue", high = "green")
plot7
ggplotly(plot7)


# Seasonal plot for a single station/var. Maybe do pooled for all stations?
seasonal_data = filter(def_filtered, station == 31015010 & variable == 'TS_1') %>%
  mutate(month=format(date, "%m"), year=format(date, "%Y")) %>%  
    group_by(month, year)
  
seasonal_plot = ggplot(seasonal_data, aes(year, value)) + geom_point(data=seasonal_data, mapping=aes(color=year)) + facet_grid(.~month) +
  theme(legend.title = element_blank(), axis.title.y=element_blank(),axis.text.x=element_blank()) + 
  xlab(paste("Years:",min(seasonal_data$year),"to", max(seasonal_data$year)))
seasonal_plot
ggplotly(seasonal_plot)
  

# Do for all variables (outer list), all stations(inner list)
stations_seasonal = list()
vars_seasonal = list()

for(i in 1:length(var_list)){
  # Calculate minmax for each variable, needed for axes
  mm = filter(def_filtered, variable == var_list[i]) %>%
    summarise(ymin = min(value, na.rm=T),
              ymax = max(value, na.rm=T))
  
  for(j in 1:length(best_stations)){
    # Subset by station and variable
    seasonal_data = filter(def_filtered, station == best_stations[j] & variable == var_list[i]) %>%
      mutate(month=format(date, "%m"), year=format(date, "%Y")) %>%  
      group_by(month, year)
    
    stations_seasonal[[j]] = ggplot(seasonal_data, aes(year, value)) + geom_point(data=seasonal_data, mapping=aes(color=year)) + 
      facet_grid(.~month) + ylim(mm$ymin, mm$ymax) + ggtitle(best_stations[j]) +
      theme(legend.position="none", axis.title.y=element_blank(),axis.title.x=element_blank(), axis.text.x=element_blank()) 
  }
  vars_seasonal[[i]] = stations_seasonal
}
  
# Now plot and save those complex figures
for(i in 1:length(var_list)){
  sss = grid.arrange(grobs=vars_seasonal[[i]], ncol=7, bottom="Month of year, from 1985 to 2014", left=var_names[i])
  ggsave(paste0("../results/plots/seasonal/", var_list[i], "_.png"), plot=sss,  width = 20, height = 10) 
}

# Line seasonal plot using ggseasonplot from the forecast package
ts_test = filter(tidy_filtered, station == 31095010)$PT_4
ts_test = ts(ts_test, start=c(1980,2), end=c(2014,11), frequency=12)
ts_plot = ggseasonplot(ts_test)
ts_plot
ggplotly(ts_plot)

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

# "Pooled trends"

pool = ggplot(data=tidy_filtered) + geom_point(mapping = aes(x=date, y=PT_9, group=station, color=station))
ggplotly(pool)

# Descriptive stats with boxplots
plot8 = ggplot(data=tidy_filtered) + geom_boxplot(mapping = aes(x=station, y=BS_4))
plot8
ggplotly(plot8)
str(plot8)

# Or even better with station Violin plots organized by lat or long, or lc_change classes
for(i in 1:length(var_list)){
  # ggplot(data=tidy_filtered_lat) + geom_violin(mapping = aes(x=station, y=get(var_list[i]))) + ylab(var_names[i]) + xlab("Station")
  # ggsave(paste0("../results/plots/violin/", var_list[i], "_stations_lat.png"), width = 20, height = 10)
  # ggplot(data=tidy_filtered_long) + geom_violin(mapping = aes(x=station, y=get(var_list[i]))) + ylab(var_names[i])+ xlab("Station")
  # ggsave(paste0("../results/plots/violin/", var_list[i], "_stations_long.png"), width = 20, height = 10) 
  ggplot(data=tidy_filtered_long) + geom_violin(mapping = aes(x=lc_change, y=get(var_list[i]))) + ylab(var_names[i])+ xlab("Transformation level")
  ggsave(paste0("../results/plots/violin/", var_list[i], "_lc_change.png"), width = 20, height = 10) 
}




##### MAPS

# Map of stations
ggplot() +  geom_polygon(data=amazon, aes(x=long, y=lat, group=group)) +geom_point(data=tidy_filtered, aes(x=long, y=lat,  color=lc_change)) 

# Plot stats!
ggplot() +  geom_polygon(data=amazon, aes(x=long, y=lat, group=group)) + 
    geom_point(data=filter(summary_stats, variable == "TS_1"), aes(x=long, y=lat, size=avg)) 


# Map trends, their magnitude and significance
# Get absolute value of slope to use in as size factor
slopes_per_station$abs_slope = abs(slopes_per_station$slope)

# Mostly interested in Evaporation, relative humidity, dew point and temperature(1, 2, 8), but let's plot and save
# everything
for(i in 1:length(var_list)){
  varsub = filter(slopes_per_station, variable == var_list[i])
  varsub$p.stars = factor(varsub$p.stars, levels = c("", "+", "*", "**", "***"), ordered = T)
  geo_varsub = inner_join(stations_lat, varsub, by = "station")
  
  # Plot trends, if I wanted to map size and slope together in a single legend, I'd use guid="legend" in
  # scale_colour_gradient2 element
  
  ggplot(data=geo_varsub, aes(x=long, y=lat)) +  geom_polygon(data=amazon, aes(x=long, y=lat, group=group)) + 
    geom_point(aes(colour=slope, size=abs_slope, shape=p.stars)) + scale_shape_manual(values=c(4, 3, 16, 17, 15)) +
    scale_colour_gradient2() + coord_fixed(1) +
    labs(title = var_names[i], color = "Slope", size="Magnitude", shape="Significance") +
    theme(plot.title = element_text(size = 12, face = "bold") , legend.title=element_text(size=14) , 
          legend.text=element_text(size=10))
  
  ggsave(paste0("../results/plots/mapped_trends/", var_list[i], "_trends.png"), width = 20, height = 10) 
}

##### NOTES
# QGIS expr to filter stations: "CODIGO_CAT" IN  ('31015010', '31095010', '32035010', '32035020', '32105070', '32155010', 
#'42075010', '44015010', '44015030', '44015040', '44035020', '44035030', '44045020', '44045030', '44055010', '44115020', 
# '44135010', '46015010', '46035010', '47015040', '47015080', '47015100', '47045010', '47075010', '48015010')

# stations with different patterns: 47015040, 47015080, 47015100, 44015030, all in higher elevations.
