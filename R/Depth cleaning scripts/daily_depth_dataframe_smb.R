# --- bring in packages ----
{
  library(broom.mixed)
  library(data.table)
  library(dplyr)
  library(ggplot2)
  library(gratia)
  library(here)
  library(itsadug)
  library(mgcv)
  library(readr)
}

# ---- bring in data ----- 

smb <- read_rds(here("Data",
                     "Cleaned detection data",
                     "smb_telemetry_data.rds"))

glimpse(smb)

# ---- create daily summary dataframe grouped by day of year -----
smb_sum <- smb %>% 
  filter(passed_filter == 1 & 
           sensor_unit == "m" & 
           sensor_value > 0 & 
           sensor_value < 30) %>% 
  
  # summary(smb_sum$sensor_value)
  # 
  # smb_sum %>% 
  #   filter(sensor_value > 25) -> t
  # 
  # summary(t$sensor_value)
  
  group_by(
    species, floy_tag, doy, sensor_unit, fish_basin, month, season, year) %>% 
  summarise(
    mean_value = mean(sensor_value), 
    sd_value = sd(sensor_value), 
    sem = sd(sensor_value) / sqrt(n())
    # n_tag = n_distinct(floy_tag)
  ) %>% 
  ungroup()

glimpse(smb_sum)

# --- subset out just depth from summary dataframe 
# smb_sum <- smb_sum %>%
#   filter(mean_value > 0)
# 
# glimpse(smb_sum)
summary(smb_sum$mean_value)

# ---- create starting point for model to run ----
# we will use the minmum day of year as the starting point --- 
smb_sum <- smb_sum %>% 
  group_by(floy_tag) %>% 
  arrange(floy_tag, year, doy) %>% 
  mutate(start_event = if_else(doy == min(doy), true = TRUE, 
                               false = FALSE), 
         floy_tag = factor(floy_tag), 
         species = factor(species), 
         year = factor(year)) %>% 
  ungroup() %>% 
  arrange(doy, start_event)


smb_sum <- smb_sum %>% 
  mutate(
    fish_basin = factor(stringr::str_remove(fish_basin, " Basin"), 
                        levels = c("East", "West", "North")
    )
  )


write_rds(smb_sum, here::here("Data", 
                                "Daily SMB Depth Data", 
                                "daily_smb_depth_data.rds"))
