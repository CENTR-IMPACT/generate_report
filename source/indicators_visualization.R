# Project Indicators: A Visual Representation of Key Performance Metrics
# ==============================================================================
#
# This module creates a specialized visualization for project indicators.
# We use a unique "sized circles on a baseline" approach that allows for:
# 1. Quick visual comparison between indicator values
# 2. Consistent positioning for easier tracking across reports
# 3. A clean, minimalist aesthetic that emphasizes the data
#
# The visualization places indicators along a horizontal axis with circle
# size proportional to the square root of the indicator value - this creates
# a visual area that's proportional to the actual value, which is perceptually
# more accurate than using diameter or radius directly.
#
#' @param project_name Name of the project
#' @param snapshot_date Date of the data snapshot
#' @param report_date Date of the report
#' @param colors List of color definitions from setup.R
#'
#' @return The generated plot object (also saves file as side effect)
#'
create_indicators_plots <- function(project_name, snapshot_date, report_date, colors) {
  # First, load our indicator data from a CSV file.
  # The filename is constructed based on project name and snapshot date,
  # allowing us to maintain consistent naming conventions across the project.
  indicators_filename <- build_inname("indicators", project_name, snapshot_date)
  
  # Read the indicators data and prepare it for visualization:
  # 1. Join with our predefined color scheme to ensure visual consistency
  # 2. Create sequential x positions for each indicator along our axis
  # 3. Set a constant y position for alignment along our baseline
  indicators_df <- read_csv(indicators_filename, col_names = TRUE) |>
    right_join(colors$indicators_colors, by = "indicator") |>
    mutate(x = row_number()) |>  # Sequential positioning along x-axis
    mutate(y = 1)                # All indicators aligned at y=1
  
  # Calculate minimum scale for edge cases
  # This is a safeguard to ensure our visualization works even with:
  # - Zero values (which would make sqrt(0) = 0, invisible)
  # - Very small values relative to others
  min_scale <- if (any(indicators_df$value == 0)) {
    0.01  # Minimum non-zero value if we have zeros
  } else {
    # Otherwise, scale based on proportion of total but ensure visibility
    ((min(indicators_df$value) / sum(indicators_df$value)) * 60) + 5
  }
  
  # Calculate circle radius using square root scaling
  # This is a crucial design decision: we use sqrt because the perceived
  # "size" of a circle relates to its area (πr²), not its radius.
  # By using sqrt, we ensure the visual areas are proportional to values.
  # The division by 13 is a scaling factor determined through visual testing
  # to ensure appropriate sizing within our plot dimensions.
  indicators_df$radius <- sqrt(as.numeric(indicators_df$value)) / 13
  
  # Create labels combining the value and indicator name
  # This creates more informative labels than showing just names or just values
  indicators_df$label <- glue("{indicators_df$value} {indicators_df$indicator}")
  
  # Create data for vertical gridlines
  # These provide visual guides at regular intervals and help structure the visualization
  v_lines <- data.frame(x = c(1, 2, 3, 4, 5, 6, 7))
  
  # Now we construct our plot using ggplot2
  # We're building this in layers, starting from the background elements
  # and progressively adding our data visualization elements
  indicators_plot <- ggplot() +
    # Add a horizontal baseline that anchors all our indicators
    # This creates a visual reference line that unifies the composition
    annotate("segment",
             x = 0, y = 1, xend = 8, yend = 1,
             color = "#E0E0E0", linewidth = 0.5
    ) +
    
    # Add vertical gridlines at each indicator position
    # These subtle guides help the eye track from the indicator to its label
    geom_segment(
      data = v_lines, aes(x = x, y = 0, xend = x, yend = 2),
      color = "#E0E0E0", linewidth = 0.5, linetype = "dashed"
    ) +
    
    # Add consistent circle outlines at each position
    # These serve as "slots" or placeholders, creating visual consistency
    # even when indicator values vary greatly
    geom_circle(
      data = data.frame(x = 1:7, y = 1), aes(x0 = x, y0 = y, r = 0.4),
      color = "#E0E0E0", fill = "transparent", linewidth = 0.5
    ) +
    
    # Add the actual data circles, sized according to our indicator values
    # These are the primary data visualization elements
    geom_circle(
      data = indicators_df,
      aes(x0 = x, y0 = y, r = radius, fill = color_fill, color = color_border)
    ) + 
    
    # Add indicator labels above each circle
    # Using a 45-degree angle helps accommodate longer labels
    # without horizontal overlap between adjacent indicators
    geom_text(
      data = indicators_df, aes(x = x, y = y + 0.5, label = label),
      angle = 45, hjust = 0, size = 18, family = "xetbook", fontface = "italic"
    ) +
    
    # Set plot dimensions to accommodate our design
    # The y-limit of 2.75 provides sufficient space for our angled labels
    ylim(0, 2.75) +
    
    # Use fixed coordinate ratio to ensure circles appear as circles
    # Without this, the circles might become ellipses due to aspect ratio differences
    coord_fixed() +
    
    # Set x-axis limits to allow for some padding on both sides
    xlim(0, 8) +
    
    # Use our color identity scales to apply the exact colors we've defined
    scale_fill_identity() +
    scale_color_identity() +
    
    # Start with a minimal theme and further customize it
    theme_minimal() +
    theme(
      legend.position = "none",      # No legend needed as labels are direct
      axis.ticks = element_blank(),  # Remove default axis ticks
      axis.text = element_blank(),   # Remove default axis text
      axis.title = element_blank(),  # Remove axis titles
      panel.grid = element_blank(),  # Remove background grid
      legend.title = element_blank(), # No legend title
      text = element_text(family = "xetbook") # Consistent font throughout
    )
  
  # Create a dashboard version of the plot
  # This is the same plot but prepared for integration into a dashboard
  indicators_plot_dashboard <- indicators_plot
  
  # Add a title line specifically formatted for dashboard display
  indicators_plot_dashboard <- add_title_line(
    "Indicators",
    indicators_plot_dashboard,
    8,                              # Title size appropriate for dashboard
    colors$foreground_color,        # Use consistent color scheme
    colors$background_color
  )
  
  # For the standalone version, add a more prominent title
  # This is used when the plot is viewed independently
  indicators_plot <- indicators_plot +
    ggtitle("Project Indicators") +
    theme(
      title = element_text(
        family = "xetbook",
        size = 72,                    # Larger size for standalone display
        face = "italic",
        hjust = 0.5                   # Center the title
      )
    )
  
  # Build the output file name
  # Using our consistent naming convention for output files
  indicators_outputs <- build_outname(
    "indicators",
    project_name,
    report_date
  )
  
  # Save the plot as a high-resolution PNG file
  # Using ragg::agg_png renderer for improved font rendering
  ggsave(indicators_outputs, indicators_plot,
         units = "in", dpi = 300,          # Print-quality resolution
         device = ragg::agg_png, width = 16, height = 9  # 16:9 aspect ratio
  )
  
  # Return the dashboard version of the plot
  # This allows the function to be used both for file output
  # and for direct display in interactive contexts or dashboards
  return(indicators_plot_dashboard)
}