# ---- load packages ---- 
{
  library(ggplot2)
  library(dplyr)
  library(here)
  library(sf)
  library(qs)
}

# we can use functions from dplyr to extract the receiver array from the detection
# file 
# ---- bring in lake papineau ---- 

lake_papineau <- st_read(dsn = here("Shapefiles", 
                                    "lake_papineau_qc.shp"))
  
lake_papineau
# we are now going to transform it to have consisent prjcts 
# 


lake_papineau <- lake_papineau %>% 
st_transform(crs = 4326) # change projection from NAD83 to WGS84 using

# espg code 4326. Please google espg to learn more about projections and 
# espg codes. 

# ---- bring in the full detection file ---- 
dets <- qread(here("Data", 
                  "cleaned detection data", 
                  "kenauk smallmouth bass 2018 - 2020.qs")) 


glimpse(dets)
dets <- dets %>% 
  filter(passed_filter == 1) # remove false detections 



# --- view structuce of detection dataframe ----- 
glimpse(dets)

dets %>% 
  distinct(name, id, long, lat) %>% 
  arrange(name) %>% 
  ggplot() + 
  geom_point(size = 5, aes(x = long, y = lat), 
             shape = 21)
  
unique(dets$name)
# ---- extract receiver array ---- 
rec_array <- dets %>% 
  group_by(name) %>% # group by the receiver names
  summarise(
    mean_lat = mean(lat), # calculate the mean lat
    mean_lon= mean(long), # calculate the mean lon 
  ) %>% 
  ungroup()

rec_array 


dets %>% 
  distinct(name, lat_mean, long_mean) %>% 
  arrange(name) %>% 
  print(n = 23)
# ---- we can convert this to a sf object ---- 
# sf stands for simple features which is a package and framework
# that is geared towards spatial data, e.g. shapefiles/attribute tables 

# utm zone 18 n is 32618
# 
rec_array_sf <- rec_array %>% 
  st_as_sf(coords = c("mean_lon", "mean_lat"),
           crs = 4326
           ) # crs is the espg code 4326 which uses wgs84 projection


rec_array_sf %>% 
  ggplot() +
  # geom_sf(data = lake_papineau) +
  geom_sf() 


# ---- view both the receiver array and lake papineau using ggplot ---- 

# look at points 
ggplot() + 
  geom_sf(data = lake_papineau) + 
  geom_sf(data = rec_array_sf, size = 3)

# look at rec location names 
ggplot() + 
  geom_sf(data = lake_papineau) + 
  geom_sf_label(data = rec_array_sf, size = 3, aes(label = name))
