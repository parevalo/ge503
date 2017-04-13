library(readxl)
library(rgdal)
library(ggplot2)
library(tidyr)
library(dplyr)

# PT: Precipitación (total en mm)
# NV: Niveles
# QL: Caudales
# BS: Brillo Solar (hours)
# EV: Evaporación (mm)
# HR: Humedad Relativa (%)
# NB: Cloud cover (okta) (Nubosidad)
# PR: Dew point (%) (punto de rocio)
# TS: Temperatura superficial (C)
# TV: Tensión de vapor (?) 
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


setwd("/media/paulo/785044BD504483BA/OneDrive/Spring_2017/GE503/project/data")
#setwd("C:/OneDrive/Spring_2017/GE503/project/data")

# Read SHP with stations and get station ID's
stations <- readOGR(".", "filtered_stations_amazon")
stations_id = stations$CODIGO_CAT

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
full_vars_stations = lapply(full_vars_list, FUN=filter, station %in% stations_id)
def_vars_list = full_vars_stations[var_list]

# Merge them all into a single tidy table!
def_vars = do.call(rbind, def_vars_list)
def_vars = rename(def_vars, date = Fecha)

# Test plotting
a = filter(def_vars, variable == 'EV_4')
plot1 = ggplot(data=def_vars) + geom_point(mapping = aes(x=date, y=variable, colour=station, group=station)) + facet_grid(station~.)
plot1
plot1 %+% filter(def_vars, variable == 'PT_4')


# Calculate things by groups or anything
test=ev %>%
      group_by(station) %>%
      summarise(avg = mean(ev, na.rm=TRUE),
                min = min(ev, na.rm=TRUE),
                max = max(ev, na.rm=TRUE),
                sd = sd(ev, na.rm=TRUE),
                total = n())



