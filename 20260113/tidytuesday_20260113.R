library(tidyverse)
library(sf)
library(rnaturalearth)
library(scales)
library(RColorBrewer)
library(patchwork)
library(extrafont)



# 1. --- Load data --- 

africa <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-01-13/africa.csv')




# 2. --- Data cleaning ---

# Let's get a list of African countries that we shall use in our data cleaning
# process 

african_countries <- 
  rvest::read_html('https://www.worldometers.info/geography/how-many-countries-in-africa/') |> 
  rvest::html_table() |> 
  purrr::pluck(1) |> 
  pull('Country') %>% 
  c(., 'Ivory Coast', 'Cape Verde')

# Let's extract the table of interest from Wikipedia
dataset <- 
  rvest::read_html("https://en.wikipedia.org/wiki/Languages_of_Africa") |> 
  rvest::html_table() |> 
  purrr::pluck(5) |> 
  dplyr::select(
    language = Language,
    family = Family,
    native_speakers = `Native speakers within Africa (L1)`, 
    country = `Official status per country`
  ) 

# Clean up the family and native speaker columns, and extract the countries
# from the country column
dt <- dataset |> 
  mutate(
    family = str_split_i(family, " ", 1), 
    native_speakers = str_split_i(native_speakers, " ", 1),
    country = str_extract_all(
      country, str_c(african_countries, collapse = "|"), simplify = TRUE))

# From the country column, we "expand" the cells that have vectors, creating 
# a "wider" tibble
countries <- 
  dataset |> 
  select(country) |> 
  map(.f = function(x) str_extract_all(
    x, str_c(
      african_countries, collapse = "|"), 
    simplify = TRUE)) |> 
  as.data.frame()

# We binding columns between the two datasets, make the dataset longer, and 
# remove rows that have blank entries in the country column
africa <- bind_cols(dt, countries) |> 
  select(-country) |> 
  pivot_longer(
    cols = contains("country"),
    names_to = 'country_index', values_to = 'country') |> 
  mutate(
    country = na_if(country, "")) |> 
  drop_na()

# We then clean up the native_speakers column and prepare our final dataset 
# by removing the unwanted country_index column
africa <- africa |> 
  mutate(native_speakers = str_split_i(native_speakers, pattern = '\\[|-', i = 1), 
         native_speakers = str_remove_all(native_speakers, pattern = ','), 
         native_speakers = as.integer(native_speakers)) |> 
  filter(!is.na(native_speakers)) |> 
  select(-country_index)




# 3. --- Plotting ---

# Geographic data (filtered only on Africa)
africa_map <- ne_countries(scale = "medium", returnclass = "sf") %>% 
  filter(continent == "Africa")

# Change country names for left join
africa_map <- africa_map %>% 
  mutate(admin = case_when(admin == "United Republic of Tanzania" ~ "Tanzania", 
                           admin == "eSwatini" ~ "Eswatini",
                           admin == "Somaliland" ~ "Somalia",
                           admin == "Guinea-Bissau" ~ "Guinea",
                           admin == "Democratic Republic of the Congo" ~ "Congo",
                           admin == "Republic of the Congo" ~ "Congo",
                           admin == "Cabo Verde" ~ "Cape Verde",
                           .default = admin)) %>% 
  group_by(admin) %>%
  summarise(do_union = TRUE, .groups = "drop") 

# Join data together
africa_map <- left_join(africa_map, africa, by=c("admin" = "country"))


# Plot data on map
map <- africa_map %>%
  group_by(admin) %>%
  summarise(language = n()) %>%
  ungroup() %>%
  ggplot() +
  # Plot map
  geom_sf(aes(fill = language),
          color = "#A3B5B6",
          linewidth = 0.3) +
  # Color palette
  scale_fill_distiller(palette = "PuBuGn",
                       direction = 1,
                       na.value = "black",
                       name = "Number of\nlanguages") +
  # Make larger
  coord_sf(xlim = c(-18, 52),
           ylim = c(-36, 38),
           expand = FALSE) + 
  # Theme settings
  theme_minimal(base_family = "Trebuchet MS") + 
  theme( 
    # Background
    panel.background = element_rect(fill = "#DCE5E9", colour = NA),
    plot.background = element_rect(fill = "#DCE5E9", colour = NA),
    panel.grid = element_blank(),
    
    # Legend
    legend.position = "bottom",
    legend.title = element_text(size = 10, face = "bold", color = "#52616B", vjust = 5, margin = margin(r = 15)),
    legend.text = element_text(size = 8, color = "#52616B"),
    legend.key.width = unit(1, "cm"),
    legend.key.height = unit(0.25, "cm"),
    legend.spacing.x = unit(0.1, "cm"),
    
    # Layout
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.margin = margin(14, 14, 14, 14))


map


# Plot data as bar chart
bar <- africa_map %>%
  group_by(admin) %>%
  summarise(language = n()) %>%
  ungroup() %>% 
  arrange(desc(language)) %>% 
  slice(1:10) %>%
  ggplot(aes(language, reorder(admin, language))) + 
  geom_col(fill = "#A3B5B6",
           width = 0.7) + 
  # Add numbers at the end of the bars
  geom_text(aes(label = language),
            hjust = -0.4,
            fontface = "bold",
            color = "#A3B5B6",
            size = 6) + 
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.15))) + 
  labs(x = NULL,
       y = NULL,
       caption = "Top 10 countries by number of languages spoken") +
  theme_minimal(base_family = "Trebuchet MS") + 
  theme( 
    # Background
    panel.background = element_rect(fill = "#DCE5E9", colour = NA),
    plot.background = element_rect(fill = "#DCE5E9", colour = NA),
    panel.grid = element_blank(),
    
    # Remove x-axis elements
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank(),
    
    plot.caption = element_text(size = 10,
                                color = "#52616B",
                                face = "bold",
                                hjust = 0,
                                margin = margin(t = -28))
  )


bar



# Combine plots
map + bar + 
  plot_layout(widths = c(1, 1)) + 
  plot_annotation(title = "Language diversity across Africa",
                  subtitle = "How many languages are spoken in each country across Africa?",
                  caption = "Source: Tidy tuesday 2026-01-13\n(https://github.com/rfordatascience/tidytuesday/tree/main/data/2026/2026-01-13)",
                  theme = theme(
                    # Title
                    plot.title = element_text(family = "Georgia",
                                              size = 32, 
                                              face = "bold", 
                                              colour = "#263B45", 
                                              hjust = 0.5),
                    # Subtitle
                    plot.subtitle = element_text(family = "Trebuchet MS",
                                                 size = 15,
                                                 colour = "#52616B",
                                                 hjust = 0.5,
                                                 margin = margin(b = 15)),
                    # Caption
                    plot.caption = element_text(family = "Trebuchet MS",
                                                size = 9,  
                                                colour = "#263B45", 
                                                hjust = 0),
                    # Background
                    plot.background = element_rect(fill = "#DCE5E9",
                                                   colour = NA),
                    plot.margin = margin(15, 14, 14, 14))
  )

































