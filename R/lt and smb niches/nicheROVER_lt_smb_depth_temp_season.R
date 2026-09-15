# ---- load packages ----

library(data.table)
library(dplyr)
library(forcats)
library(ggplot2)
library(ggtext)
library(here)
library(lemon)
library(lubridate)
library(nicheROVER)
library(purrr)
library(patchwork)
library(readr)
library(stringr)
library(tibble)
library(tidyr)


# ---- bring in clean downloaded ----

sum_sp <- read_rds(here("Saved Data",
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

unique(sum_temp$season)

test <-  sum_temp %>% 
  group_split(species, season)

str(test)
fish_par <- sum_temp %>% 
  group_split(species, season) %>% 
  map(~ select(., depth, temp)) %>% 
  map(~niw.post(nsample = nsample, X = .))

unique(df_sigma$season)
# ---- separate niw.post object into sigma and mu tibbles ----- 
df_sigma <- map(fish_par, pluck, 2) %>% 
  imap(~ as_tibble(.x) %>% 
         mutate( 
           posterior = "sigma", 
           metric = c("depth", "temp"),
           species = .y
         )
  ) %>%
  bind_rows(.id = "season") %>% 
  pivot_longer(cols = -c("metric", "species", "season", "posterior"),
               names_to = "sensor_unit", 
               values_to = "post_sample"
  )  %>% 
  separate(sensor_unit, into = c("sensor", "sample_number"), sep = "\\.") %>% 
  mutate(
    species = case_when(
      species %in% c(1:4) ~ "Lake Trout", 
      species %in% c(5:8) ~ "Smallmouth Bass"
    ), 
    season = as.numeric(season),
    season = 
      factor(
        case_when(
          season %in% c(1, 5) ~ "Fall",
          season %in% c(2, 6) ~ "Winter",
          season %in% c(3, 7) ~ "Spring",
          season %in% c(4, 8) ~ "Summer"
        ), 
        levels = c("Fall", "Winter", "Spring", "Summer")
      )
  )
df_sigma
# unique(df_sigma$season)
# unique(df_sigma$species)


df_sigma_cn <- df_sigma %>% 
  filter(metric != sensor)

df_sigma_wide <- df_sigma %>%
  # select(id:post_sample) %>%
  pivot_wider(
    names_from = metric, 
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
  bind_rows(.id = "season") %>% 
  group_by(species) %>% 
  mutate(
    sample_number = 1:1000
  ) %>% 
  ungroup() %>% 
  mutate(
    species = case_when(
      species %in% c(1:4) ~ "Lake Trout", 
      species %in% c(5:8) ~ "Smallmouth Bass"), 
    season = 
      factor(
        case_when(
          season %in% c(1, 5) ~ "Fall",
          season %in% c(2, 6) ~ "Winter",
          season %in% c(3, 7) ~ "Spring",
          season %in% c(4, 8) ~ "Summer"
        ), 
        levels = c("Fall", "Winter", "Spring", "Summer")
      )
  )
unique(df_mu$species)



glimpse(df_mu)

df_mu_long <- df_mu %>% 
  pivot_longer(cols = -c(metric, species, season, sample_number), 
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
  # scale_x_continuous(breaks = seq(6.5, 10, 0.5),
  #                    limits = c(6.5, 10)) +
  theme_bw(base_size = 15) +
  facet_rep_wrap(.~ season, repeat.tick.labels = TRUE, scales = "free_x") +
  theme(
    panel.grid = element_blank(), 
    legend.position = c(0.15, 0.94), 
    legend.background = element_blank()
  ) + 
  labs(x = expression(paste(mu[depth (m)])),
       y = expression(paste("p(", mu[depth (m)], "| X)")))

p
p1 <- ggplot(data = df_mu, aes(x = temp)) +
  geom_density(aes(fill = species), alpha = 0.5) +
  scale_fill_viridis_d(begin = 0.25, end = 0.75, 
                       option = "G", name = "Basin") + 
  # scale_x_continuous(breaks = seq(4.5, 11.5, 1), 
  #                    limits = c(4.5, 11)) + 
  # scale_y_continuous(breaks = seq(0, 3, 0.5), 
  #                    limits = c(0, 3)
  # ) +  
  facet_rep_wrap(.~ season, repeat.tick.labels = TRUE, scales = "free_x") +
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

p1
p2 <- ggplot(data = df_sigma_cn, aes(x = post_sample)) +
  geom_density(aes(fill = species), alpha = 0.5) + 
  scale_fill_viridis_d(begin = 0.25, end = 0.75, 
                       option = "G", name = "Basin") + 
  # scale_x_continuous(limits = c(-35, 15)) +
  # scale_y_continuous(breaks = seq(0, 2, 0.5), 
  #                    # limits = c(0, 3)
  # ) +  
  facet_rep_wrap(.~ season, repeat.tick.labels = TRUE, scales = "free_x") + 
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


p2
p3 <- p + p1 + p2

p3
ggsave(filename = here("Plots",
                       "nicheROVER Habitat use",
                       "posterior_mean_density_habitat_use_season.png"),
       height = 8.5, width = 11 * 2, plot = p3)

# for loop over basins and sample number within basin to create each 
# sample numbers ellipi 

df_mu %>% 
  group_split(species, season)

i <- 1
j <- 1

p.ell <- 0.95
all_ellipses <- list()

for (i in 1:8) {
  
  sigma_group <- df_sigma_wide %>% 
    group_split(species, season) %>% 
    .[[i]]
  
  mu_group <- df_mu %>% 
    group_split(species, season) %>% 
    .[[i]]
  
  # a dummy variable to build in the loop
  ell <- NULL
  post.id <- NULL
  
  
  for(j in 1:length(unique(sigma_group$sample_number))) {
    
    sigma_ind <- sigma_group %>%
      filter(sample_number %in% sample_number[j]) %>% 
      select(depth, temp)
    Sigma <- as.matrix(sigma_ind, 2,2)
    row.names(Sigma) <- c("depth", "temp")
    
    Sigma
    mu <- mu_group %>%
      filter(sample_number %in% sample_number[j]) %>% 
      select(sample_number, depth, temp) %>% 
      pivot_longer(cols = -sample_number, 
                   names_to = "sensor", 
                   values_to = "mu") %>% 
      .$mu
    
    
    
    out <- ellipse::ellipse(Sigma, centre = mu , level = p.ell)
    
    ell <- rbind(ell, out)
    post.id <- c(post.id, rep(j, nrow(out)))
    
  }
  ell <- as.data.frame(ell)
  ell$rep <- post.id
  all_ellipses[[i]] <- ell
  message('Processing dataframe ', i, " of ", 8)
  
}


# combine ellipose list into dataframe and add basin names back in 
ellipse_df <- bind_rows(all_ellipses, .id = "season") %>% 
  mutate(
    species = case_when(
      season %in% c(1:4) ~ "Lake Trout", 
      season %in% c(5:8) ~ "Smallmouth Bass"), 
    season = 
      factor(
        case_when(
          season %in% c(1, 5) ~ "Fall",
          season %in% c(2, 6) ~ "Winter",
          season %in% c(3, 7) ~ "Spring",
          season %in% c(4, 8) ~ "Summer"
        ), 
        levels = c("Fall", "Winter", "Spring", "Summer")
      )
  ) %>% 
  as_tibble()


# select seasonal ellipse 
ellipse_df %>% 
  group_by(season, rep) %>% 
  nest() %>%
  group_by(season) %>% 
  slice_sample(n = 10, replace = TRUE) %>% 
  ungroup() %>% 
  unnest(cols = c(data)) -> random_ellipse 


random_ellipse

# ---- create niche plots -----
ggplot() + 
  geom_polygon(data = random_ellipse,
               mapping = aes(x = depth, y = temp,
                             group = interaction(rep, species),
                             color = species,
                             fill = NULL),
               fill = NA,
               linewidth = 0.5) + 
  
  scale_colour_viridis_d(begin = 0.25, end = 0.75, 
                         option = "G", name = "Basin", 
                         # alpha = 0.35
  ) + 
  facet_rep_wrap(.~ season, 
                 scales = "free_x", 
                 repeat.tick.labels = TRUE) +
  # scale_fill_viridis_d(begin = 0.25, end = 0.85, 
  #                      option = "D", name = "Basin") + 
  # scale_x_continuous(breaks = rev(seq(-20, -40, -2))) +
  # scale_y_continuous(breaks = seq(6, 16, 2)) +
  theme_bw(base_size = 15) +
  theme(axis.text = element_text(colour = "black"),
        panel.grid = element_blank(), 
        legend.position = "none", 
        legend.title.align = 0.5,
        legend.background = element_blank(),
        plot.tag.position  = c(0.95, 0.95)
  ) + 
  labs(
    x = "Depth (m)", 
    y = paste("Temperature(","\U00B0", "C)", sep = "")
  ) -> p4 
# p4

sum_temp %>% 
  filter(season == "Summer" & species == "smb")


ggplot() + 
  geom_density(data = sum_temp, aes(x = depth, 
                                    fill = species
                                    # colour = basin
  ),
  alpha = 0.35, 
  linewidth = 0.8) +
  facet_rep_wrap(.~ season, 
                 scales = "free_x", 
                 repeat.tick.labels = TRUE) +
  # scale_colour_viridis_d(begin = 0.25, end = 0.85, 
  #                      option = "D", name = "Basin") + 
  scale_fill_viridis_d(begin = 0.25, end = 0.75,
                       option = "G", name = "Basin") +
  # scale_x_continuous(breaks = seq(8, 16, 1), 
  #                    limits = c(8, 16)) +
  theme_bw(base_size = 15) +
  theme(axis.text = element_text(colour = "black"),
        panel.grid = element_blank(), 
        legend.position = "none", 
        legend.title.align = 0.5,
        legend.background = element_blank(),
        plot.tag.position  = c(0.95, 0.95)
  ) + 
  labs(x = "Depth Use (m)", 
       y = "Density") -> p5
# p5

ggplot() + 
  geom_density(data = sum_temp, aes(x = temp, 
                                    fill = species
                                    # colour = basin
  ), 
  alpha = 0.35, 
  linewidth = 0.8) +
  facet_rep_wrap(.~ season, 
                 scales = "free_x", 
                 repeat.tick.labels = TRUE) +
  # scale_colour_viridis_d(begin = 0.25, end = 0.85, 
  #                      option = "D", name = "Basin") + 
  scale_fill_viridis_d(begin = 0.25, end = 0.75,
                       option = "G", name = "Species", 
                       label = c("Lake Trout", 
                                 "Smallmouth Bass")) +
  # scale_x_continuous(breaks = rev(seq(-22, -34, -1)),
  #                    limits = rev(c(-22, -34)))  +
  theme_bw(base_size = 15) +
  theme(axis.text = element_text(colour = "black"),
        panel.grid = element_blank(), 
        legend.position = c(0.14, 0.90), 
        legend.title.align = 0.5,
        legend.background = element_blank(),
        plot.tag.position  = c(0.95, 0.95)
  ) + 
  labs(x =  paste("Temperature(","\U00B0", "C)", sep = ""), 
       y = "Density") -> p6

# p6

ggplot() + 
  geom_point(data = sum_temp, aes(x = depth, y = temp,
                                  # colour = basin, 
                                  fill = species, 
  ),
  shape = 21, colour = "black", 
  stroke = 0.8,
  size = 3, alpha = 0.70) +
  # scale_colour_viridis_d(begin = 0.25, end = 0.75, 
  #                        option = "D", name = "Basin") + 
  scale_fill_viridis_d(begin = 0.25, end = 0.75,
                       option = "G", name = "Basin") +
  # scale_x_continuous(breaks = rev(seq(-20, -39, -1))) +
  # scale_y_continuous(breaks = seq(5, 17, 1)) +
  theme_bw(base_size = 15) +
  theme(axis.text = element_text(colour = "black"),
        panel.grid = element_blank(), 
        legend.position = "none", 
        legend.title.align = 0.5,
        legend.background = element_blank(), 
        plot.tag.position  = c(0.95, 0.95)
  ) + 
  facet_rep_wrap(.~ season, 
                 scales = "free_x", 
                 repeat.tick.labels = TRUE) +
  labs(
    x = "Depth (m)", 
    y = paste("Temperature(","\U00B0", "C)", sep = "")
  ) -> p7

# p7

p8 <- p5 + p4 + p7 + p6 


# p8

ggsave(filename = here("Plots",
                       "nicheROVER Habitat use",
                       "habitat_use_species_season_ellispe.png"),
       height = 8.5 * 1.5, width = 11 * 1.5, plot = p8)
# ---- NICHE overlap calculation and plotting ----- 

# ---- calculate % overlap from posterior sampling ----
# Calculate how much each grouping overlaps 
over_stat <- overlap(fish_par, nreps = nsample, nprob = 1e3, 
                     alpha = c(0.95, 0.99))

# convert to dataframe to be able to plot and look at staistically 
over_stat_df <- over_stat %>% 
  as_tibble(rownames = "species_a") %>% 
  mutate(
    id = 1:nrow(.), 
    
    season_a = 
      factor(
        case_when(
          species_a %in% c(1, 5) ~ "Fall",
          species_a %in% c(2, 6) ~ "Winter",
          species_a %in% c(3, 7) ~ "Spring",
          species_a %in% c(4, 8) ~ "Summer"
        ), 
        levels = c("Fall", "Winter", "Spring", "Summer")
        
      ), 
    species_a = case_when(
      species_a %in% c(1:4) ~ "Lake Trout", 
      species_a %in% c(5:8) ~ "Smallmouth Bass"),
  ) %>% 
  pivot_longer(cols = -c(id, species_a, season_a), 
               names_to = "species_b", 
               values_to = "mc_nr")  %>% 
  separate(species_b, into = c("species_c", "sample_number", "percentage"),
           sep = "\\.") %>%
  select(-id) %>% 
  rename(species_b = species_c) %>%
  mutate(
    
    season_b = 
      factor(
        case_when(
          species_b %in% c(1, 5) ~ "Fall",
          species_b %in% c(2, 6) ~ "Winter",
          species_b %in% c(3, 7) ~ "Spring",
          species_b %in% c(4, 8) ~ "Summer"
        ), 
        levels = c("Fall", "Winter", "Spring", "Summer")
        
      ), 
    species_b = case_when(
      species_b %in% c(1:4) ~ "Lake Trout", 
      species_b %in% c(5:8) ~ "Smallmouth Bass"),
    mc_nr_perc = mc_nr * 100
  ) %>% 
  dplyr::select(species_a:species_b, season_b, sample_number:mc_nr, mc_nr_perc)

over_stat_df

# filter for just the 95% 
over_stat_df_95 <- over_stat_df %>% 
  filter(percentage %in% "95%")

# calculate the mean overlap for each basin 
over_mean <- apply(over_stat, c(1:2, 4), mean) * 100 %>% 
  round(2) 

# convert to dataframe to be able to plot and compare 
over_mean_df <- over_mean %>% 
  as_tibble(rownames = "species_a") %>% 
  mutate(
    id = 1:nrow(.), 
    season_a = 
      factor(
        case_when(
          species_a %in% c(1, 5) ~ "Fall",
          species_a %in% c(2, 6) ~ "Winter",
          species_a %in% c(3, 7) ~ "Spring",
          species_a %in% c(4, 8) ~ "Summer"
        ), 
        levels = c("Fall", "Winter", "Spring", "Summer")
        
      ), 
    species_a = case_when(
      species_a %in% c(1:4) ~ "Lake Trout", 
      species_a %in% c(5:8) ~ "Smallmouth Bass")
  ) %>% 
  pivot_longer(cols = -c(id, species_a, season_a), 
               names_to = "species_b", 
               values_to = "mean_mc_nr")  %>% 
  separate(species_b, into = c("species_c", "percentage"), 
           sep = "\\.") %>% 
  select(-id) %>% 
  rename(species_b = species_c) %>% 
  mutate(season_b = 
           factor(
             case_when(
               species_b %in% c(1, 5) ~ "Fall",
               species_b %in% c(2, 6) ~ "Winter",
               species_b %in% c(3, 7) ~ "Spring",
               species_b %in% c(4, 8) ~ "Summer"
             ), 
             levels = c("Fall", "Winter", "Spring", "Summer")
             
           ), 
         species_b = case_when(
           species_b %in% c(1:4) ~ "Lake Trout", 
           species_b %in% c(5:8) ~ "Smallmouth Bass"),
         # mc_nr_perc = mc_nr * 100
         )

# filter so that you have just the 95% 
over_mean_df_95 <- over_mean_df %>% 
  filter(percentage %in% "95%")

# calculate confidence intervals around the mean 
over_cred <- apply(over_stat * 100, c(1:2, 4), quantile, 
                   prob = c(0.025, 0.975), na.rm = TRUE)
# convert to dataframe for plotting 
over_cred_df <- over_cred %>% 
  as_tibble(rownames = "qual") %>% 
  mutate(
    id = 1:nrow(.),
    
  ) %>% 
  pivot_longer(cols = -c(id, qual), 
               names_to = "species_b", 
               values_to = "quantile_mc_nr") %>% 
  separate(species_b, into = c("species_a", "species_b", "percentage"), 
           sep = "\\.") %>% 
  select(species_a, species_b, percentage, qual, quantile_mc_nr) %>% 
  mutate(
    season_a = 
           factor(
             case_when(
               species_a %in% c(1, 5) ~ "Fall",
               species_a %in% c(2, 6) ~ "Winter",
               species_a %in% c(3, 7) ~ "Spring",
               species_a %in% c(4, 8) ~ "Summer"
             ), 
             levels = c("Fall", "Winter", "Spring", "Summer")
             
           ), 
         species_a = case_when(
           species_a %in% c(1:4) ~ "Lake Trout", 
           species_a %in% c(5:8) ~ "Smallmouth Bass"),
    
    season_b = 
           factor(
             case_when(
               species_b %in% c(1, 5) ~ "Fall",
               species_b %in% c(2, 6) ~ "Winter",
               species_b %in% c(3, 7) ~ "Spring",
               species_b %in% c(4, 8) ~ "Summer"
             ), 
             levels = c("Fall", "Winter", "Spring", "Summer")
             
           ), 
         species_b = case_when(
           species_b %in% c(1:4) ~ "Lake Trout", 
           species_b %in% c(5:8) ~ "Smallmouth Bass"),
  )
over_cred_df
# filter out just 95% 
over_cred_df_95 <- over_cred_df %>% 
  filter(percentage == "95%")


# ---- plot 95 % niche size  ----- 
# p9 <- ggplot(data = over_stat_df_95, aes(x = mc_nr_perc)) + 
#   geom_histogram(bins = 50, colour = "white", aes(fill = species_a)) + 
#   geom_vline(data = over_mean_df_95, aes(xintercept = mean_mc_nr), 
#              colour = "black", linewidth = 1) +
#   geom_vline(data = over_cred_df_95, aes(xintercept = quantile_mc_nr), 
#              colour = "black", linewidth = 1, linetype = 6) +
#   scale_fill_viridis_d(begin = 0.25, end = 0.75,
#                        alpha = 0.8,
#                        option = "D", name = "Basin") + 
#   ggh4x::facet_grid2(species_a ~ species_b, 
#                      independent = "y",
#                      scales = "free_y") + 
#   theme_bw(base_size = 15) + 
#   theme(
#     panel.grid = element_blank(), 
#     axis.text = element_text(colour = "black"), 
#     legend.position = c(0.75, 0.24)
#   ) + 
#   
#   labs(x = paste("Overlap Probability (%)", "\u2013", 
#                  "Niche Region Size: 95%"), 
#        y = "Frequency")
# 
# p9
# 
# ggsave(filename = here("Plots",
#                        "nicheROVER plots",
#                        "niche_percent_overlap_hist.png"), plot = p9,
#        height = 8.5, width = 11)

p10 <- ggplot(data = over_stat_df_95, aes(x = mc_nr_perc)) + 
  geom_density(aes(fill = species_a)) + 
  geom_vline(data = over_mean_df_95, aes(xintercept = mean_mc_nr), 
             colour = "black", linewidth = 1) +
  geom_vline(data = over_cred_df_95, aes(xintercept = quantile_mc_nr), 
             colour = "black", linewidth = 1, linetype = 6) +
  scale_fill_viridis_d(begin = 0.25, end = 0.75,
                       option = "D", name = "Basin", 
                       alpha = 0.35) + 
  ggh4x::facet_grid2(
    interaction(species_a, season_a) ~  
    interaction(species_b, season_b) , 
                     independent = "y",
                     scales = "free") + 
  theme_bw(base_size = 15) + 
  theme(
    panel.grid = element_blank(), 
    axis.text = element_text(colour = "black"), 
    legend.position = c(0.95, 0.06), 
    # strip.background = element_rect(fill = NA)
    strip.background = element_blank()
  ) + 
  
  labs(x = paste("Overlap Probability (%)", "\u2013", 
                 "Niche Region Size: 95%"), 
       y = "p(Percent Overlap | X)")

# p10

ggsave(filename = here("Plots",
                       "nicheROVER habitat use", 
                       "niche_percent_overlap_density_species_.png"), plot = p10,
       height = 8.5 * 1.5, width = 11 * 2)

