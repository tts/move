library(tidyverse)
library(lubridate)
library(moveVis)
library(move)

aprildata <- "http://dev.hsl.fi/citybikes/od-trips-2019/2019-02.csv"
temp <- tempfile()
download.file(aprildata, temp)
data <- read_csv(temp)
unlink(temp)

bikestations <- "https://opendata.arcgis.com/datasets/1b492e1a580e4563907a6c7214698720_0.csv"
temp <- tempfile()
download.file(bikestations, temp)
stations <- read_csv(temp)
unlink(temp)

stations_cleaned <- stations %>% 
  select(X, Y, name, id)

kulosaari <- data %>% 
  filter(`Departure station name` == "Kulosaaren metroasema")

# Join departure station coords
kulo_dep <- left_join(kulosaari, stations_cleaned, by = c(`Departure station id`="id"))

# arrival
kulo_arr <- left_join(kulosaari, stations_cleaned, by = c(`Return station id`="id"))

dep <- kulo_dep %>% 
  select(Time = Departure, 
         StationID = `Departure station id`, 
         Station =`Departure station name`, X, Y) %>% 
  mutate(id = paste0("R",row_number())) %>% 
  head()

arr <- kulo_arr %>% 
  select(Time = Return, 
         StationID = `Return station id`, 
         Station =`Return station name`, X, Y) %>%   
  mutate(id = paste0("R", row_number())) %>% 
  head()

dep_arr <- rbind(dep, arr)

dep_arr$timestamp <- as.POSIXct(dep_arr$Time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

dep_arr <- dep_arr %>% 
  select(-Time, -Station, -StationID)

routes <- as.data.frame(dep_arr)

# df to move object
m <- df2move(routes,
        proj = CRS("+init=epsg:4326"), 
        x = "X",
        y = "Y",
        time = "timestamp",
        track_id = "id")

unique(unlist(timeLag(m, units = "secs")))

j.align <- align_move(m, res = 15, digit = 0, unit = "secs")

frames <- frames_spatial(j.align, path_colours = c("red", "green", "blue",
                                                   "yellow", "brown", "orange"),
                         map_service = "osm", map_type = "watercolor", alpha = 0.5) %>% 
  add_labels(x = "Longitude", y = "Latitude") %>% 
  add_northarrow() %>% 
  add_scalebar() %>% 
  add_timestamps(m, type = "label") %>% 
  add_progress()
