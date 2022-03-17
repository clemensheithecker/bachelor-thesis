# Set working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Clear environment
rm(list = ls())


# Load libraries ----------------------------------------------------------

library(ggplot2)


# Standard theme ----------------------------------------------------------

theme_thesis <-
  function(...,
           base_size = 12,
           base_family = "Assistant") {
    theme(
      # Change aesthetics of lines
      line = element_line(
        color = "black",
        size = 0.5,
        linetype = 1,
        lineend = "butt"
      ),
      
      # Change aesthetics of rectangles
      rect = element_rect(
        fill = "white",
        color = "black",
        size = 0.5,
        linetype = 1
      ),
      
      # Change aesthetics of text
      text = element_text(
        family = base_family,
        face = "plain",
        color = "black",
        size = base_size,
        lineheight = 0.9,
        hjust = 0.5,
        vjust = 0.5,
        angle = 0,
        margin = margin(),
        debug = FALSE
      ),
      
      # Change aesthetics of axis titles
      axis.title = element_text(
        face = "plain",
        color = "black",
        size = base_size
      ),
      axis.title.x = element_text(margin = margin(t = 8)),
      axis.title.y = element_text(angle = 90, margin = margin(r = 8)),
      
      # Change aesthetics of axis text
      axis.text = element_text(
        face = "plain",
        color = "black",
        size = base_size
      ),
      
      # Change axis ticks color
      axis.ticks = element_line(color = "black"),
      
      # Change axis line color
      axis.line = element_line(color = "black"),
      
      # Remove the background color of the legend
      legend.background = element_rect(fill = NA),
      
      # Change legend container margins
      legend.margin = margin(
        t = 4,
        r = 4,
        b = 4,
        l = 4
      ),
      
      # Change legend margins
      legend.box.margin = margin(
        t = 4,
        r = 4,
        b = 4,
        l = 4
      ),
      
      # Remove the background color of the boxes in the legend
      legend.key = element_rect(fill = NA),
      
      # Change aesthetics of legend text
      legend.text = element_text(
        color = "black",
        size = base_size,
        margin = margin(r = 8)
      ),
      
      # Remove legend title
      legend.title = element_blank(),
      
      # Change the position of the legend
      legend.position = "bottom",
      
      # Change the panel background color
      panel.background = element_rect(fill = "white", color = NA),
      
      # Remove the panel border
      panel.border = element_blank(),
      
      # Change major grid line color to gray
      panel.grid.major =  element_line(color = "#D4D4D4"),
      
      # Drop horizontal major grid line
      panel.grid.major.x = element_blank(),
      
      # Drop minor grid lines
      panel.grid.minor = element_blank(),
      
      # Change the plot background color
      plot.background = element_rect(fill = "white", color = NA),
      
      # Make title bold and add space at the baseline
      plot.title = element_text(
        face = "bold",
        color = "black",
        size = base_size * 1.2,
        margin = margin(b = 4)
      ),
      
      # Adjust position of title
      plot.title.position = "plot",
      
      # Change aesthetics of plot subtitle
      plot.subtitle = element_text(size = base_size, margin = margin(b = 20)),
      
      # Adjust the margins of plot
      plot.margin = margin(24, 24, 24, 24),
      
      ...
    )
  }


# LaTeX theme -------------------------------------------------------------

theme_thesis_latex <- function() {
  # Update elements of "theme_thesis()"
  theme_thesis() +
    theme(
      # Reset to default font family
      text = element_text(family = ""),
      
      # Change color of axis titles to black
      axis.title = element_text(color = "black"),
      
      # Change color of axis text to black
      axis.text = element_text(color = "black"),
      
      # Change axis ticks color to black
      axis.ticks = element_line(color = "black"),
      
      # Change axis line color to black
      axis.line = element_line(color = "black"),
      
      # Add a border to the legend
      legend.background = element_rect(color = "black"),
      
      # Drop vertical major grid line
      panel.grid.major.y = element_blank(),
      
      # Add a border to the plot background
      plot.background = element_rect(color = "black")
    )
}
