lazy_ggsave <- function(plot, filename, width = 7, height = 5){
  # Save as png
  ggsave(plot = plot,
         here("tesults", "img", paste0(filename, ".tiff")),
         width = width,
         height = height)
  
  # Save as pdf
  ggsave(plot = plot,
         here("results", "img", paste0(filename, ".pdf")),
         width = width,
         height = height)
}
