library(tidyverse)
library(patchwork)
library(extrafont)




# 1. --- Load data --- 

edible_plants <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-02-03/edible_plants.csv')




# 2. --- Data cleaning ---


edible_plants_2 <- edible_plants %>% 
  mutate(across(where(is.character), tolower)) %>% 
  mutate(cultivation = case_when(cultivation == "brassicas" ~ "brassica",
                                 cultivation == "solanum" ~ "solanaceae",
                                 .default = cultivation),
         sunlight = case_when(sunlight == "full sun/partial shade/ full shade" ~ "full sun/partial shade/full shade",
                              .default = sunlight),
         nutrients = case_when(nutrients == "high potassium fertiliser every 2 weeks." ~ "high",
                               nutrients == "medium to high" ~ "medium",
                               .default = nutrients),
         temperature_class = case_when(temperature_class == "very hard" ~ "very hardy",
                                       temperature_class == "very tender" ~ "tender",
                                       .default = temperature_class)) %>% 
  select(taxonomic_name, common_name, cultivation, sunlight, water, preferred_ph_lower, preferred_ph_upper, nutrients, temperature_class)





# 3. --- Plotting ---


# 3.1 Water preferences 

water_preferences <- edible_plants_2 %>% 
  mutate(water_2 = factor(as.character(water), levels = c("very low", "low", "medium", "high", "very high")),
         water = case_when(water == "very low" ~ 1,
                           water == "low" ~ 2, 
                           water == "medium" ~ 3,
                           water == "high" ~ 4,
                           water == "very high" ~ 5)) %>%
  mutate(across(where(is.character), str_to_title)) %>%
  select(common_name, cultivation, water, water_2) %>% 
  filter(cultivation == "Brassica") %>% 
  ggplot() +
  geom_col(aes(x=str_wrap(common_name, 5), y=water, fill=water_2),
           alpha = 0.65,
           position = "dodge2",
           show.legend = TRUE) +
  coord_polar(clip = "off") +
  geom_segment(aes(x = str_wrap(common_name, 5),
                   y = 0,
                   xend = str_wrap(common_name, 5),
                   yend = 5),
               linetype = "dashed",
               linewidth = 0.5,
               color = "#D8D3CA") +
  geom_hline(data = data.frame(water = 1:5), aes(yintercept = water), linewidth=0.3, color="#5B5147") + 
  scale_y_continuous(limits = c(-0.5, 5.5), expand = c(0,0)) +
  scale_fill_manual(name = "Water requirement",
                    values = c("very low" = "#A65D45",
                               "low" = "#C58A5B",
                               "medium" = "#D5B982",
                               "high" = "#84966A",
                               "very high" = "#476B50"),
                    limits = c("very low", "low", "medium", "high", "very high"),
                    labels = c("very low" = "Very low", 
                               "low" = "Low",
                               "medium" = "Medium",
                               "high" = "High",
                               "very high" = "Very high"),
                    drop = FALSE)




water_preferences <- water_preferences +
  annotate(x = 0, y = 1.25, label = "Very low", geom = "text", size = 3, family = "Trebuchet MS", fontface = "bold") +
  annotate(x = 0, y = 2.25, label = "Low", geom = "text", size = 3, family = "Trebuchet MS", fontface = "bold") +
  annotate(x = 0, y = 3.25, label = "Medium", geom = "text", size = 3, family = "Trebuchet MS", fontface = "bold") +
  annotate(x = 0, y = 4.25, label = "High", geom = "text", size = 3, family = "Trebuchet MS", fontface = "bold") +
  annotate(x = 0, y = 5.25, label = "Very high", geom = "text", size = 3, family = "Trebuchet MS", fontface = "bold") +
  labs(title = "Water") +
  theme(
    # Fonts
    text = element_text(family = "Trebuchet MS"),
    plot.title = element_text(family = "Palatino Linotype", size = 20, face = "bold", color = "#A17D6B", hjust = 0.5, vjust = -1),
    
    # Axis options
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 8, color = "#5B5147"),
    
    # Grid options
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    
    # Background
    plot.background = element_rect(fill = "#F5F3EF", colour = NA),
    panel.background = element_rect(fill = "#F5F3EF", colour = NA),
    
    # Legend
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title.position = "top",
    legend.title = element_text(face = "bold", size = 10, hjust = 0.5),
    legend.background = element_rect(fill = "#F5F3EF", colour = NA),
    legend.key = element_rect(fill = "#F5F3EF", colour = NA),
    legend.text = element_text(size = 8, colour = "#5B5147"),
    legend.spacing.x = unit(0.35, "cm"),
    legend.key.width = unit(0.65, "cm"),
    legend.key.height = unit(0.35, "cm"),
    legend.box.spacing = unit(-0.3, "cm")
    )

water_preferences



# 3.2 Nutrient preferences 

nutrient_preferences <- edible_plants_2 %>% 
  mutate(nutrients_2 = factor(as.character(nutrients), levels = c("low", "medium", "high")),
         nutrients = case_when(nutrients == "low" ~ 1, 
                               nutrients == "medium" ~ 2,
                               nutrients == "high" ~ 3)) %>% 
  mutate(across(where(is.character), str_to_title)) %>%
  select(common_name, cultivation, nutrients, nutrients_2) %>% 
  filter(cultivation == "Brassica") %>% 
  ggplot() +
  geom_col(aes(x=str_wrap(common_name, 5), y=nutrients, fill=nutrients_2),
           alpha = 0.65,
           position = "dodge2",
           show.legend = TRUE) +
  coord_polar(clip = "off") +
  geom_segment(aes(x = str_wrap(common_name, 5),
                   y = 0,
                   xend = str_wrap(common_name, 5),
                   yend = 3),
               linetype = "dashed",
               linewidth = 0.5,
               color = "#D8D3CA") +
  geom_hline(data = data.frame(nutrients = 1:3), aes(yintercept = nutrients), linewidth=0.3, color="#5B5147") + 
  scale_y_continuous(limits = c(-0.5, 3.5), expand = c(0,0)) +
  scale_fill_manual(name = "Nutrient requirement",
                    values = c("low" = "#B16F56",
                               "medium" = "#A3A47A",
                               "high" = "#54745B"),
                    limits = c("low", "medium", "high"),
                    labels = c("low" = "Low",
                               "medium" = "Medium",
                               "high" = "High"),
                    drop = FALSE)



nutrient_preferences <- nutrient_preferences +
  annotate(x = 0, y = 1.25, label = "Low", geom = "text", size = 3, family = "Trebuchet MS", fontface = "bold") +
  annotate(x = 0, y = 2.25, label = "Medium", geom = "text", size = 3, family = "Trebuchet MS", fontface = "bold") +
  annotate(x = 0, y = 3.25, label = "High", geom = "text", size = 3, family = "Trebuchet MS", fontface = "bold") +
  labs(title = "Nutrients") +
  theme(
    # Fonts
    text = element_text(family = "Trebuchet MS"),
    plot.title = element_text(family = "Palatino Linotype", size = 20, face = "bold", color = "#A17D6B", hjust = 0.5, vjust = -1),
    
    # Axis options
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 8, color = "#5B5147"),
    
    # Grid options
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    
    # Background
    plot.background = element_rect(fill = "#F5F3EF", colour = NA),
    panel.background = element_rect(fill = "#F5F3EF", colour = NA),
    
    # Legend
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title.position = "top",
    legend.title = element_text(face = "bold", size = 10, hjust = 0.5),
    legend.background = element_rect(fill = "#F5F3EF", colour = NA),
    legend.key = element_rect(fill = "#F5F3EF", colour = NA),
    legend.text = element_text(size = 8, colour = "#5B5147"),
    legend.spacing.x = unit(0.35, "cm"),
    legend.key.width = unit(0.65, "cm"),
    legend.key.height = unit(0.35, "cm"),
    legend.box.spacing = unit(-0.3, "cm")
  )


nutrient_preferences



# 3.3 Temperature preferences 


temperature_preferences <- edible_plants_2 %>% 
  mutate(temperature_class_2 = factor(as.character(temperature_class), levels = c("tender", "half hardy", "hardy", "very hardy")),
         temperature_class = case_when(temperature_class == "tender" ~ 1, 
                               temperature_class == "half hardy" ~ 2,
                               temperature_class == "hardy" ~ 3,
                               temperature_class == "very hardy" ~ 4)) %>% 
  mutate(across(where(is.character), str_to_title)) %>%
  select(common_name, cultivation, temperature_class, temperature_class_2) %>% 
  filter(cultivation == "Brassica") %>% 
  ggplot() +
  geom_col(aes(x=str_wrap(common_name, 5), y=temperature_class, fill=temperature_class_2),
           alpha = 0.65,
           position = "dodge2",
           show.legend = TRUE) +
  coord_polar(clip = "off") +
  geom_segment(aes(x = str_wrap(common_name, 5),
                   y = 0,
                   xend = str_wrap(common_name, 5),
                   yend = 4),
               linetype = "dashed",
               linewidth = 0.5,
               color = "#D8D3CA") +
  geom_hline(data = data.frame(temperature_class = 1:4), aes(yintercept = temperature_class), linewidth=0.3, color="#5B5147") + 
  scale_y_continuous(limits = c(-0.5, 4.5), expand = c(0,0)) +
  scale_fill_manual(name = "Temperature class",
                    values = c("tender" = "#A65D45",
                               "half hardy" = "#D5A66F",
                               "hardy" = "#84966A",
                               "very hardy" = "#476B50"),
                    limits = c("tender", "half hardy", "hardy", "very hardy"),
                    labels = c("tender" = "Tender",
                               "half hardy" = "Half hardy",
                               "hardy" = "Hardy",
                               "very hardy" = "Very hardy"),
                    drop = FALSE)

temperature_preferences <- temperature_preferences + 
  annotate(x = 0, y = 1.25, label = "Tender", geom = "text", size = 3, family = "Trebuchet MS", fontface = "bold") +
  annotate(x = 0, y = 2.25, label = "Half hardy", geom = "text", size = 3, family = "Trebuchet MS", fontface = "bold") +
  annotate(x = 0, y = 3.25, label = "Hardy", geom = "text", size = 3, family = "Trebuchet MS", fontface = "bold") +
  annotate(x = 0, y = 4.25, label = "Very hardy", geom = "text", size = 3, family = "Trebuchet MS", fontface = "bold") +
  labs(title = "Temperature") +
  theme(
    # Fonts
    text = element_text(family = "Trebuchet MS"),
    plot.title = element_text(family = "Palatino Linotype", size = 20, face = "bold", color = "#A17D6B", hjust = 0.5, vjust = -1),
    
    # Axis options
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 8, color = "#5B5147"),
    
    # Grid options
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    
    # Background
    plot.background = element_rect(fill = "#F5F3EF", colour = NA),
    panel.background = element_rect(fill = "#F5F3EF", colour = NA),
    
    # Legend
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title.position = "top",
    legend.title = element_text(face = "bold", size = 10, hjust = 0.5),
    legend.background = element_rect(fill = "#F5F3EF", colour = NA),
    legend.key = element_rect(fill = "#F5F3EF", colour = NA),
    legend.text = element_text(size = 8, colour = "#5B5147"),
    legend.spacing.x = unit(0.35, "cm"),
    legend.key.width = unit(0.65, "cm"),
    legend.key.height = unit(0.35, "cm"),
    legend.box.spacing = unit(-0.3, "cm")
  )

temperature_preferences



# 3.4 pH preferences 

ph_preferences <- edible_plants_2 %>% 
  select(common_name, cultivation, preferred_ph_lower, preferred_ph_upper) %>% 
  mutate(across(where(is.character), str_to_title)) %>%
  filter(cultivation == "Brassica") %>% 
  ggplot() +
  geom_segment(aes(x=common_name, xend=common_name, y=preferred_ph_lower, yend=preferred_ph_upper), color="#D6D3CC", linewidth = 4) + 
  geom_point(aes(x=common_name, y=preferred_ph_lower), size=3.5, color="#B16F56", alpha = 0.7) +
  geom_point(aes(x=common_name, y=preferred_ph_upper), size=3.5, color="#54745B", alpha = 0.7) +
  geom_text(aes(x=common_name, y=preferred_ph_lower, label=preferred_ph_lower), nudge_y=-0.1, hjust=1, color="#B16F56", size=4, fontface="bold", alpha = 0.6) +
  geom_text(aes(x=common_name, y=preferred_ph_upper, label=preferred_ph_upper), nudge_y=0.1, hjust=0, color="#54745B", size=4, fontface="bold", alpha = 0.6) +
  coord_flip() + 
  ylim(4.5, 9) + 
  ylab("pH range") +
  labs(title = "pH") +
  theme(
    # Fonts
    text = element_text(family = "Trebuchet MS"),
    plot.title = element_text(family = "Palatino Linotype", size = 20, face = "bold", color = "#A17D6B", hjust = 0.5, vjust = 0.5),
    
    # Axis options
    axis.title.y = element_blank(),
    axis.title.x = element_text(color = "#5B5147", size = 10),
    axis.text.x = element_text(size = 8, color = "#5B5147"),
    axis.ticks.x = element_line(color = "#D8D3CA", linewidth=0.8),
    axis.ticks.y = element_line(color = "#D8D3CA", linewidth=0.8),
    axis.text.y = element_text(size = 8, color = "#5B5147", vjust=0.3),
    
    # Grid options
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_line(color="#D8D3CA", linewidth=0.7),

    
    # Background
    plot.background = element_rect(fill = "#F5F3EF", colour = NA),
    panel.background = element_rect(fill = "#F5F3EF", colour = NA),
    
    aspect.ratio = 0.7
  )


ph_preferences



# 3.5 Sunlight preferences 

sunlight_preferences <- edible_plants_2 %>% 
  select(common_name, cultivation, sunlight) %>% 
  mutate(across(where(is.character), str_to_title)) %>%
  mutate(full_sun = case_when(str_detect(sunlight, "Full Sun") == TRUE ~ 1, .default = 0),
         full_shade = case_when(str_detect(sunlight, "Full Shade") == TRUE ~ 1, .default = 0),
         partial_shade = case_when(str_detect(sunlight, "Partial Shade") == TRUE ~ 1, .default = 0)) %>%
  filter(cultivation == "Brassica") %>% 
  pivot_longer(cols = c(full_sun, full_shade, partial_shade),
               names_to = "sunlight_category",
               values_to = "suitable") %>% 
  ggplot(aes(sunlight_category, common_name, fill = factor(suitable))) +
  geom_tile(color = "#D8D3CA", linewidth = 0.5, alpha = 0.7) +
  scale_fill_manual(values = c("0" = "#B16F56", "1" = "#54745B"),
                    labels = c("Not suitable", "Suitable"),
                    name = "Sunlight") +
  scale_x_discrete(labels = c("full_sun" = "Full Sun",
                              "full_shade" = "Full Shade",
                              "partial_shade" = "Partial Shade")) +
  coord_fixed(ratio = 0.5) +
  labs(x = NULL, y = NULL) + 
  labs(title = "Sunlight") +
  theme(
    # Fonts
    text = element_text(family = "Trebuchet MS"),
    plot.title = element_text(family = "Palatino Linotype", size = 20, face = "bold", color = "#A17D6B", hjust = 0.5, vjust = 0.5),
    
    # Axis options
    axis.title.y = element_blank(),
    axis.title.x = element_text(color = "#5B5147"),
    axis.text.x = element_text(size = 8, color = "#5B5147", angle = 45, hjust = 1, vjust = 1),
    axis.ticks.x = element_line(color = "#D8D3CA", linewidth=0.8),
    axis.ticks.y = element_line(color = "#D8D3CA", linewidth=0.8),
    axis.text.y = element_text(size = 8, color = "#5B5147", vjust=0.3),
    
    # Grid options
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_line(color="#D8D3CA", linewidth=0.7),
    
    
    # Background
    plot.background = element_rect(fill = "#F5F3EF", colour = NA),
    panel.background = element_rect(fill = "#F5F3EF", colour = NA),
    
    # Legend
    legend.title.position = "top",
    legend.title = element_text(face = "bold", size = 10, hjust = 0.5),
    legend.background = element_rect(fill = "#F5F3EF", colour = NA),
    legend.key = element_rect(fill = "#F5F3EF", colour = NA),
    legend.text = element_text(size = 8, colour = "#5B5147"),
    legend.spacing.x = unit(0.35, "cm"),
    legend.key.width = unit(0.65, "cm"),
    legend.key.height = unit(0.35, "cm"),
    legend.box.spacing = unit(0.5, "cm")
  )


sunlight_preferences



# 3.6 Combine plots

final_plot <- (ph_preferences | water_preferences) / (temperature_preferences | nutrient_preferences | sunlight_preferences) +
  plot_annotation(
    title = "Growing preferences of edible plants",
    subtitle = "An overview of optimal growing conditions across different Brassica plants",
    caption = "Source: Tidy tuesday 2026-02-03 \n(https://github.com/rfordatascience/tidytuesday/tree/main/data/2026/2026-02-03)",
    theme = theme(
      plot.title = element_text(family = "Palatino Linotype", size = 32, face = "bold", color = "#829078", hjust = 0.5, margin = margin(b = 10)),
      plot.subtitle = element_text(size = 14, color="#A17D6B", hjust = 0.5, margin = margin(b = 2)),
      plot.caption = element_text(size = 10, hjust = 0, margin = margin(b = 2))
    )
  ) &
  theme(
    plot.background = element_rect(fill = "#F5F3EF", colour = NA),
    plot.margin = margin(12, 12, 12, 12, unit = "pt"),
    text = element_text(family = "Trebuchet MS", color = "#5B5147")
    )



ggsave(filename = "tidytuesday_20260203.png",
       plot = final_plot,
       width = 16,
       height = 12,
       units = "in",
       dpi = 300,
       bg = "#F5F3EF")


























