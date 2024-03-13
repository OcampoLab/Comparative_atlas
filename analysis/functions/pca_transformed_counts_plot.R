# Function to make the PCA plot

# Required packages
require(tidyverse)
require(ggthemes)

pca_transformed_counts_plot <- function(pca_results_object, 
                                        metadata, 
                                        PCs, 
                                        color_by, 
                                        color_palette,
                                        color_column_name,
                                        color_column = FALSE 
                                        ){
  
  principal_components_df <- pca_results_object
  
  principal_components_df$color <- factor(metadata[, color_by], levels = unique(metadata[, color_by]))
  
  PCA_percentages <- pca_results_object %>%
    attributes() %>%
    .[["percentVar"]] %>%
    `*`(100) %>%
    round(1) %>%
    setNames(c("PC1", "PC2", "PC3", "PC4"))
  
  ggplot(data = principal_components_df,
         mapping = aes(x      = !!sym(PCs[1]),
                       y      = !!sym(PCs[2]),
                       label  = color,
                       fill   = color,
                       color  = color)) + 
    geom_point(shape = 21, size = 5, color = "white", show.legend = T) +
    xlab(paste(PCs[1], " (", PCA_percentages[PCs[1]], "%)", sep = "")) +
    ylab(paste(PCs[2], " (", PCA_percentages[PCs[2]], "%)", sep = "")) +
    theme_publication() +
    {if(!color_column) scale_fill_brewer(palette = color_palette)} +
    {if(color_column) scale_fill_identity(guide = "legend", labels = levels(metadata[,color_column_name, drop = T]))} +
    theme(
      legend.position = "bottom",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
}

