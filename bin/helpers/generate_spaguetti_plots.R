generate_spaguetti_plot <- function(dataset = NULL, response = replicate_conc, font_size = 12, color_lines = F){
  
  spaguetti_plot = ggplot(dataset |> filter(!is.na({{response}})),
         aes(x = hours_before_freeze,
             y = {{response}})) +
    
    geom_path(aes(group = identifier,
                  color = identifier),
              position = position_dodge(0.3),
              alpha    = 0.3) +
    
    geom_point(aes(group = identifier,
                   fill  = identifier),
               position = position_dodge(0.3),
               alpha    = 0.3,
               shape    = 21,
               color    = "gray") +
    
    stat_summary(aes(group = 1),
                 geom      = "line",
                 fun       = "mean",
                 col       = "#680001",
                 linewidth = 1) +
    
    stat_summary(geom  = "point",
                 fun   = "mean",
                 col   = "#680001",
                 size  = 3,
                 shape = 21,
                 fill  = "#680001") +
    
    guides(color = "none", fill = "none") +
    
    theme_bw(base_size = font_size) +
    
    coord_flex_cart(bottom = capped_horizontal(capped = "both"), 
                    left   = capped_vertical(capped = "both")) +
    
    background_grid(major = "y") +
    
    theme(axis.line        = element_line(),
          panel.border     = element_blank(),
          panel.grid.major = element_line(linewidth = 0.25, 
                                          linetype = "dotted"),
          text = element_text(family = "serif"), 
          axis.title = element_text(family = "serif", size = 9)
          )
  
  if(color_lines == T){
    
    spaguetti_plot <- spaguetti_plot +
      
      scale_color_viridis(discrete = T, end = 0.7, option = "magma") +
      
      scale_fill_viridis(discrete = T, end = 0.7, option = "magma")
    
  } else {
    
    spaguetti_plot <- spaguetti_plot +
    
      scale_colour_grey(start = 0.4, end = 0.7) +
      
      scale_fill_grey(start = 0.4, end = 0.7) 
    
  }
  
  
  
}