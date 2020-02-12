########################################################
#                    HELPER FUNCTIONS                  #
########################################################
# This script contains some helper functions to 
# streamline our work.
########################################################

#### MODEL STUFF ####
# The function below returns a panel figure
# with all state variables through time
fast_check <- function(chi, r, K, X0, D, p, q, c, beta, L, alpha, mu, w, years, tolerance = 0.05) {
  model(
    chi = chi,
    r = r,
    K = K,
    X0 = X0,
    D = D,
    p = p,
    q = q,
    c = c,
    beta = beta,
    L = L,
    alpha = alpha,
    mu = mu,
    w = w,
    years = years,
    tolerance = tolerance
  ) %>%
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
      strip.text = element_text(
        hjust = 0,
        margin = margin(b = 10)),
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
      panel.background = element_rect(fill = "transparent", color = "transparent"),
      strip.background = element_rect(fill = "transparent", color = "transparent"),
      plot.background = element_rect(fill = "transparent", color = "transparent"),
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
         height = height,
         units = "cm")
  
  # Save as pdf
  ggsave(plot = plot,
         here("results", "img", paste0(filename, ".pdf")),
         width = width,
         height = height,
         units = "cm")
}
