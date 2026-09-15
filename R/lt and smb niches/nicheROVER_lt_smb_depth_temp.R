# ---- load packages ----

library(data.table)
library(dplyr)
library(forcats)
library(ggplot2)
library(ggtext)
library(here)
library(lubridate)
library(nicheROVER)
library(purrr)
library(patchwork)
library(readr)
library(stringr)
library(tibble)
library(tidyr)


# ---- bring in clean downloaded ----
# 
# lt <- read_rds(here::here("Saved data", 
#                           "kenauk lake trout 2017 - 2020.rds"))
# 
#  
# smb <- read_rds(here::here("Saved data", 
#                           "kenauk smallmouth bass 2018 - 2020.rds"))
# 
# glimpse(lt)
# glimpse(smb)
# 
# 
# lt$species <- "lt"
# smb$species <- "smb"
# 
# 
# lt_smb <- bind_rows(lt, smb)
# 
# glimpse(lt_smb)
# 
# lt_smb <- lt_smb %>% 
#   mutate(
#     doy = yday(date)
#   )
# 
# # combined rds ----
# 
# write_rds(lt_smb, here("Saved Data", 
#                "lt_smb_telemetry_data.rds"))

lt_smb <- read_rds(here("Saved Data", 
                        "lt_smb_telemetry_data.rds"))

glimpse(lt_smb)
sum_sp <- lt_smb %>% 
  group_by(
    species, floy_tag, doy, sensor_unit, fish_basin, month, season, year) %>% 
  summarise(
    mean_value = mean(sensor_value), 
    # n_tag = n_distinct(floy_tag)
  ) %>% 
  ungroup()

sum_sp
write_rds(sum_sp, here("Saved Data", 
                       "daily_mean_sensor_sp.rds"))

sum_temp <- sum_sp %>% 
  filter(sensor_unit %in% c("°C", "m") & mean_value > 0) %>% 
  pivot_wider(id_cols = c(species, doy, fish_basin, month, season),
              names_from = sensor_unit, 
              values_from = mean_value
               ) %>% 
  rename(
    depth = m, 
    temp = `°C`
  ) %>% 
  filter(depth != is.na(depth) & temp != is.na(temp)) 
  

unique(is.na(sum_temp$temp))


nsample <- 1000

fish_par <- sum_temp %>% 
  split(.$species) %>% 
  map(~ select(., depth, temp)) %>% 
  map(~niw.post(nsample = nsample, X = .))


# ---- separate niw.post object into sigma and mu tibbles ----- 
df_sigma <- map(fish_par, pluck, 2) %>% 
  imap(~ as_tibble(.x) %>% 
         mutate( 
           metric = "sigma", 
           id = c("depth", "temp"),
           species = .y
         )
  ) %>%
  bind_rows() %>% 
  pivot_longer(cols = -c("id", "species", "metric"),
               names_to = "sensor_unit", 
               values_to = "post_sample"
  )  %>% 
  separate(sensor_unit, into = c("sensor", "sample_number"), sep = "\\.")

unique(df_sigma$id)


df_sigma_cn <- df_sigma %>% 
  filter(id != sensor)

df_sigma_wide <- df_sigma %>%
  select(id:post_sample) %>% 
  pivot_wider(names_from = id, 
              values_from = post_sample)

# df_sigma_n <- df_sigma %>% 
#   filter(id %in% "d15n" & isotopes %in% "d15n")

# ---- mu -----

df_mu <- map(fish_par, pluck, 1) %>% 
  imap(~ as_tibble(.x) %>% 
         mutate( 
           metric = "mu", 
           species = .y
         )
  ) %>%
  bind_rows() %>% 
  mutate(
    species = factor(species, 
                     levels = c("lt", "smb"))
  ) %>% 
  group_by(species) %>% 
  mutate(
    sample_number = 1:1000
  ) %>% 
  ungroup()
unique(df_mu$species)

unique(lt$sensor_unit)

glimpse(df_mu)

df_mu_long <- df_mu %>% 
  pivot_longer(cols = -c(metric, species, sample_number), 
               names_to = "sensor", 
               values_to = "mu_est") 

# ---- density plots ggplot ----

p <- ggplot(data = df_mu, aes(x = depth)) +
  geom_density(aes(fill = species), alpha = 0.35) + 
  scale_fill_viridis_d(begin = 0.25, end = 0.75, 
                       option = "G", name = "Species", 
                       labels = c("Lake Trout", "Smallmouth Bass")) + 
  # scale_y_continuous(breaks = seq(0, 2, 0.5), 
                     # limits = c(0, 3)
  # ) + 
  scale_x_continuous(breaks = seq(6.5, 10, 0.5),
                     limits = c(6.5, 10)) +
  theme_bw(base_size = 15) + 
  theme(
    panel.grid = element_blank(), 
    legend.position = c(0.15, 0.94), 
    legend.background = element_blank()
  ) + 
  labs(x = expression(paste(mu[depth (m)])),
       y = expression(paste("p(", mu[depth (m)], "| X)")))

# p
p1 <- ggplot(data = df_mu, aes(x = temp)) +
  geom_density(aes(fill = species), alpha = 0.5) +
  scale_fill_viridis_d(begin = 0.25, end = 0.75, 
                       option = "G", name = "Basin") + 
  scale_x_continuous(breaks = seq(4.5, 11.5, 1), 
                     limits = c(4.5, 11)) + 
  # scale_y_continuous(breaks = seq(0, 3, 0.5), 
  #                    limits = c(0, 3)
  # ) +  
  theme_bw(base_size = 15) + 
  theme(legend.position = "none", 
        panel.grid = element_blank(),   
        axis.title.x =  element_markdown(),
        axis.title.y =  element_markdown(), 
        ) + 
  labs(x = paste("\u00b5", "<sub>Temperature (</sub>", 
                 "<sub><sup>\U00B0</sup></sub>", "<sub>C)</sub>", sep = ""),
       y = paste("p(", "\u00b5", "<sub>Temperature (</sub>", 
                 "<sub><sup>\U00B0</sup></sub>", "<sub>C)</sub>", " | X)")
  )

# p1
p2 <- ggplot(data = df_sigma_cn, aes(x = post_sample)) +
  geom_density(aes(fill = species), alpha = 0.5) + 
  scale_fill_viridis_d(begin = 0.25, end = 0.75, 
                       option = "G", name = "Basin") + 
  scale_x_continuous(limits = c(-35, 15)) +
  # scale_y_continuous(breaks = seq(0, 2, 0.5), 
  #                    # limits = c(0, 3)
  # ) +  
  theme_bw(base_size = 15) + 
  theme(legend.position = "none",
        panel.grid = element_blank(), 
        axis.title.x =  element_markdown(),
        axis.title.y =  element_markdown()
        ) + 
  labs(
    x = paste("\U03A3 ",
       "<sub>", "Depth (m)", "</sub>", 
       "<sub> Temperature (</sub>", 
       "<sub><sup>\U00B0</sup></sub>", "<sub>C)</sub>", sep = ""),
    y = paste("p(", "\U03A3 ",
       "<sub>", "Depth (m)", "</sub>", 
       "<sub> Temperature (</sub>", 
       "<sub><sup>\U00B0</sup></sub>", "<sub>C)</sub>", "| X)", sep = "")
       )


# p2
p3 <- p + p1 + p2

# 
ggsave(filename = here("Plots",
                       "nicheROVER Habitat use",
                       "posterior_mean_density_habitat_use.png"),
       height = 8.5, width = 11 * 2, plot = p3)


