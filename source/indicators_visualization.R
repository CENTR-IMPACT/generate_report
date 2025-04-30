# Project Indicators

#' @param project_name Name of the project
#' @param snapshot_date Date of the data snapshot
#' @param report_date Date of the report
#' @param colors List of color definitions from setup.R
#'
#' @return The generated plot object (also saves file as side effect)
#'
create_indicators_plots <- function(project_name, snapshot_date, report_date, colors) {
  indicators_filename <- build_inname("indicators", project_name, snapshot_date)
  indicators_df <- read_csv(indicators_filename, col_names = TRUE) |>
    right_join(colors$indicators_colors, by = "indicator") |>
    mutate(x = row_number()) |>
    mutate(y = 1)

  min_scale <- if (any(indicators_df$value == 0)) {
    0.01
  } else {
    ((min(indicators_df$value) / sum(indicators_df$value)) * 60) + 5
  }

  indicators_df$radius <- sqrt(as.numeric(indicators_df$value)) / 13

  indicators_df$label <- glue("{indicators_df$value} {indicators_df$indicator}")

  v_lines <- data.frame(x = c(1, 2, 3, 4, 5, 6, 7))

  indicators_plot <- ggplot() +
    # Horizontal line
    annotate("segment",
      x = 0, y = 1, xend = 8, yend = 1,
      color = "#E0E0E0", linewidth = 0.5
    ) +

    # Vertical lines
    geom_segment(
      data = v_lines, aes(x = x, y = 0, xend = x, yend = 2),
      color = "#E0E0E0", linewidth = 0.5, linetype = "dashed"
    ) +

    # Circle outlines
    geom_circle(
      data = data.frame(x = 1:7, y = 1), aes(x0 = x, y0 = y, r = 0.4), # Explicitly create data here
      color = "#E0E0E0", fill = "transparent", linewidth = 0.5
    ) +

    # Data points using geom_circle
    geom_circle(
      data = indicators_df,
      aes(x0 = x, y0 = y, r = radius, fill = color_fill, color = color_border)
    ) + # Use border_color here
    geom_text(
      data = indicators_df, aes(x = x, y = y + 0.5, label = label),
      angle = 45, hjust = 0, size = 18, family = "xetbook", fontface = "italic"
    ) +
    ylim(0, 2.75) +
    coord_fixed() + # Important to ensure circles look circular
    xlim(0, 8) +
    scale_fill_identity() +
    scale_color_identity() + # Add this to use border_color directly
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      legend.title = element_blank(),
      text = element_text(family = "xetbook")
    )

  indicators_plot_dashboard <- indicators_plot
  indicators_plot_dashboard <- add_title_line(
      "Indicators",
      indicators_plot_dashboard,
      8,
      colors$foreground_color,
      colors$background_color
    )

  indicators_plot <- indicators_plot +
    ggtitle("Project Indicators") +
    theme(
      title = element_text(
        family = "xetbook",
        size = 72,
        face = "italic",
        hjust = 0.5
        )
    )
  
  # Build the output file name
  indicators_outputs <- build_outname(
    "indicators",
    project_name,
    report_date
  )

  # Save the plot as a PNG file
  ggsave(indicators_outputs, indicators_plot,
    units = "in", dpi = 300,
    device = ragg::agg_png, width = 16, height = 9
  )

  # Return the plot object for potential inline display
  return(indicators_plot_dashboard)
}
