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

# ---- bring in data and the model for plotting ---- 
dat <- read_rds(here("Data",
                     "Daily SMB Depth Data", 
                     "daily_smb_depth_data.rds"))

m1 <- read_rds(here("model objects", 
                    "smb_gamm_depth.rds"))

glimspe(dat)
glimpse(m1)
# model family = Gamma(link = "log"), 

# ---- create predicted values from the model ---- 

# blank floy_tag and year to remove from model fit 
dat <- dat %>% 
  mutate(
    floy_tag = "a", 
    year = "b"
  )

# create model fit from predict
fit <- predict.gam(m1, newdata = dat, discrete = TRUE, 
                   exclude = c("s(floy_tag)", "s(year)"), se.fit = TRUE)


# ---- convert fit to preds dataframe that can be used for plotting ----
preds <- data.frame(dat, fit) %>% 
  mutate(
    lower = exp(1) ^ (fit - 1.96 * se.fit),
    upper = exp(1) ^ (fit + 1.96 * se.fit), 
    fit = exp(1) ^ fit, 
  ) %>% 
  as_tibble()

glimpse(preds)


# ---- create mean depth regardless of floy_tag ----
dat %>%
  group_by(doy, fish_basin) %>%
  summarise(mean_depth = mean(mean_value)) %>%
  ungroup() -> mean_depth

# ---- plotting prep -------
# ---- create monthly breaks ---- 

# create vector of the DOY that each Month starts 
month_doy <- preds %>%
  group_by(month) %>%
  summarise(first = first(doy),
            last = last(doy)) %>%
  ungroup() %>%
  arrange(first) %>% 
  mutate(
    month_abb = factor(month.abb,
                       levels = month.abb
    )) %>%
  arrange(month_abb) %>%
  .$first

month_doy

# create month labels 
month_label <- factor(month.abb, levels = month.abb)

month_label

# ---- create season bars ----

# figure out where your shading for summer and winter goes 

rect_winter <- tibble(
  season = "Winter",
  xmin = 1,
  xmax = 60,
  ymin = -Inf,
  ymax = Inf
)

rect_summer <- tibble(
  season = "Summer",
  xmin = 152,
  xmax = 244,
  ymin = -Inf,
  ymax = Inf
)

rect_winter_dec <- tibble(
  season = "Winter",
  xmin = 335,
  xmax = 365,
  ymin = -Inf,
  ymax = Inf
)

# ---- create plot ----- 
p <- ggplot(data = preds) +
  # turn this on and off if you want the bars 
  geom_rect(data = rect_summer, aes(xmin = xmin,
                                    xmax = xmax,
                                    ymin = ymin,
                                    ymax = ymax),
            fill = "grey80",
            alpha = 0.75,
            inherit.aes = FALSE) +
  geom_rect(data = rect_winter, aes(xmin = xmin,
                                    xmax = xmax,
                                    ymin = ymin,
                                    ymax = ymax),
            fill ="grey80",
            alpha = 0.75,
            inherit.aes = FALSE) +
  geom_rect(data = rect_winter_dec, aes(xmin = xmin,
                                        xmax = xmax,
                                        ymin = ymin,
                                        ymax = ymax),
            fill ="grey80",
            alpha = 0.75,
            inherit.aes = FALSE) +
  geom_text(
    aes(x = xmin + 33, y = 40, label = season),
    data = rect_summer,
    size = 5, vjust = 0, hjust = 0, check_overlap = TRUE) +
  geom_text(
    aes(x = xmin + 17.5, y = 40, label = season),
    data = rect_winter,
    size = 5, vjust = 0, hjust = 0, check_overlap = TRUE) +
  geom_text(
    aes(x = xmin + 5, y = 40, label = season),
    data = rect_winter_dec,
    size = 5, vjust = 0, hjust = 0, check_overlap = TRUE) + 
  # this section above can be turned on and off by commenting
  geom_point(data = mean_depth, aes(x = doy, y = mean_depth,
                                    colour = fish_basin), 
             alpha = 0.5, size = 3) +
  geom_hline(yintercept = 0, linetype = 2) + 
  geom_line(
    aes(x = doy, y = fit, colour = fish_basin), linewidth = 1) +
  geom_ribbon(
    aes(ymin = lower,
        ymax = upper,
        x = doy, y = fit,
        fill = fish_basin), alpha = 0.25) +
  scale_y_reverse(breaks = rev(seq(0, 40, 5))) +
  scale_x_continuous(breaks = month_doy,
                     label = month_label) +
  scale_colour_viridis_d(name = "Basin",
                         option = "G", begin = 0.35, end = 0.75) +
  scale_fill_viridis_d(name = "Basin",
                       option = "G", begin = 0.35, end = 0.75) +
  theme_bw(base_size = 15) +
  theme(panel.grid = element_blank(),
        axis.text = element_text(colour = "black"),
        legend.position = c(0.95, 0.86),
        legend.background = element_blank(),
        legend.title = element_text(hjust = 0.5),
        legend.text = element_text(hjust = 0.5)) +
  labs(x = "Date",
       y = "Depth Use (m)") 

# p

# ggsave(here("Plots", 
#             "Depth Daily GAMM", 
#             "smb_daily_depth_GAMM_no_bars.png"), plot = p, height = 7, 
#        width = 11)


  
ggsave(here("Plots", 
            "Depth Daily GAMM", 
            "smb_daily_depth_GAMM_bars.png"), plot = p, height = 7, 
       width = 11)



