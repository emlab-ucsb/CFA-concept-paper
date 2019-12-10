########################################################
#                    HELPER FUNCTIONS                  #
########################################################
# This script contains some helper functions to 
# streamline our work.
########################################################

#### MODEL STUFF ####
# The function below returns a panel figure
# with all state variables through time
fast_check <- function(r, K, X0, D, p, q, c, beta, L, alpha, mu, w, chi, years) {
  model(r, K, X0, D, p, q, c, beta, L, alpha, mu, w, chi, years) %>% 
    mutate_at(.vars = vars(X_r_vec, X_f_vec), .funs =  ~ . / (K / 2)) %>% 
    mutate(X_vec = X_vec / K) %>% 
    gather(variable, value, - time) %>% 
    ggplot(aes(x = time, y = value)) +
    geom_line() +
    facet_wrap(~variable, scale = "free_y")
}



#### FIGURE STUFF ####
# Plot theme function

plot_theme <- function (font_size = 10, font_family = "", line_size = 0.5) {
  
  half_line <- font_size / 2
  
  theme_grey(base_size = font_size, base_family = font_family) %+replace%
    theme(
      rect = element_rect(
        fill = "transparent",
        colour = NA,
        color = NA,
        size = 0,
        linetype = 0),
      text = element_text(
        family = font_family,
        face = "plain",
        colour = "black",
        size = font_size,
        hjust = 0.5,
        vjust = 0.5,
        angle = 0,
        lineheight = 0.9,
        margin = margin(),
        debug = FALSE),
      plot.title = element_text(
        family = font_family,
        face = "plain",
        colour = "black",
        size = rel(1.2),
        margin = margin(),
        debug = FALSE),
      axis.text = element_text(
        colour = "black",
        size = rel(0.8)),
      axis.ticks = element_line(
        colour = "black"),
      axis.line = element_line(
        colour = "black",
        size = line_size,
        lineend = "square"),
      axis.line.x = element_line(
        colour = "black",
        size = line_size,
        lineend = "square"),
      axis.line.y = element_line(
        colour = "black",
        size = line_size,
        lineend = "square"),
      legend.background = element_blank(),
      legend.key = element_blank(),
      legend.key.size = unit(1, "lines"),
      legend.spacing = unit(0.4, "cm"),
      legend.text = element_text(size = rel(0.8)),
      panel.background = element_blank(),
      strip.background = element_blank(),
      plot.background = element_blank(),
      complete = TRUE
    )
}




# Save function
# The function saves figures as png and pdf
lazy_ggsave <- function(plot, filename, width = 7, height = 5){
  # Save as png
  ggsave(plot = plot,
         here("results", "img", paste0(filename, ".png")),
         width = width,
         height = height)
  
  # Save as pdf
  ggsave(plot = plot,
         here("results", "img", paste0(filename, ".pdf")),
         width = width,
         height = height)
}
