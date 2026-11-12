library(tidyverse)
library(plotly)
library(extrafont)
library(htmlwidgets)

# 1. --- Load data --- 

fiction_citations <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-11-10/fiction_citations.csv')
fiction_deaths <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-11-10/fiction_deaths.csv')
fiction_rankings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-11-10/fiction_rankings.csv')



# 2. --- Plot data --- 


# 2.1 ggplot
plot <- fiction_deaths %>% filter(undone == TRUE) %>% 
  group_by(franchise_slug) %>% 
  tally() %>% 
  inner_join(., fiction_deaths %>% filter(undone == TRUE) %>% 
               select(franchise_slug, victim) %>% 
               group_by(franchise_slug) %>%
               summarise(victim = paste(victim, collapse = ", "), .groups = "drop")) %>%
  mutate(franchise_slug = str_to_title(franchise_slug) %>% str_replace_all(., "-", " ")) %>%
  ggplot(aes(n, reorder(franchise_slug, n), text = victim)) + 
  geom_col(fill="#8447FF") +
  geom_text(aes(label = n), nudge_y=-0.1, nudge_x=0.4) +
  xlab("Deaths undone") +
  ylab("") +
  scale_x_continuous(expand = expansion(mult = c(0.015, 0.05))) +
  theme_minimal() +
  theme(
    legend.position = "none",
    
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    
    axis.text.x = element_blank(),
    axis.title = element_text(color = "#1B1725", family = "Open Sans Light", size = 13),
    axis.text.y = element_text(color = "#1B1725", family = "Open Sans", size = 15)

  ) 

plot

# 2.2 Interactive plot with tooltips

plotly <- ggplotly(plot, tooltip = "text",   width = 1200,
                   height = 800) %>% 
  layout(
    
    # Background
    paper_bgcolor = "#E5E9ED",
    plot_bgcolor = "#E5E9ED",
    
    # Title
    title = list(
      text = "Which franchises have a tendency \nto undo their deaths?",
      x = 0.4,
      xref = "paper",
      xanchor = "center",
      y = 0.94,
      yanchor = "top",
      font = list(family = "Libre Baskerville SemiBold", size = 36, color = "#1B1725")
      ),

    margin = list(t = 100, b = 70, l = 150, r = 40)
  
    )


# Identify which traces in plotly graph contain text labels (to change font, size, color of text labels)
text_traces <- which(vapply(plotly$x$data, function(x) "text" %in% x$mode, logical(1)))


plotly <- plotly %>% 
  style(textfont = list(family = "Libre Baskerville Semibold", size = 38, color = "#8447FF"),
        textposition = "middle right",
        traces = text_traces) %>% 
  onRender("
    function(el, x) {

      var barTrace = 0;
      var textTrace = 1;

      el.on('plotly_hover', function(data) {

        var pointNumber = data.points[0].pointNumber;

        // Change hovered bar color
        var barColors = Array(el.data[barTrace].x.length).fill('#8447FF');
        barColors[pointNumber] = '#FFC857';

        Plotly.restyle(el, {
          'marker.color': [barColors]
        }, [barTrace]);

        // Change corresponding number color
        var textColors = Array(el.data[textTrace].text.length).fill('#8447FF');
        textColors[pointNumber] = '#FFC857';

        Plotly.restyle(el, {
          'textfont.color': [textColors]
        }, [textTrace]);

      });

      el.on('plotly_unhover', function(data) {

        // Restore original bar colors
        Plotly.restyle(el, {
          'marker.color': '#8447FF'
        }, [barTrace]);

        // Restore original number colors
        Plotly.restyle(el, {
          'textfont.color': '#8447FF'
        }, [textTrace]);

      });

    }
  ")



plotly


saveWidget(plotly, "fiction_deaths.html", selfcontained = TRUE)


