#### Alignment Visualization - Streamlined Implementation

#' Create alignment plots for project data
#'
#' This function visualizes alignment data between researchers and partners,
#' calculating agreement scores and generating dashboard-style plots.
#'
#' @param project_name Name of the project
#' @param snapshot_date Date of data snapshot
#' @param report_date Date of report generation
#' @param colors Color scheme for visualization (must contain alignment_colors)
#'
#' @return A ggplot object representing the alignment dashboard
create_alignment_plots <- function(project_name, snapshot_date, report_date, colors) {
  # ----- [1] Data Import and Preparation -----
  
  # Load the alignment data with color information
  alignment_filename <- build_inname("alignment", project_name, snapshot_date)

  # Read data and join with color definitions in one step
  alignment_df <- read_csv(alignment_filename, col_names = TRUE) |>
    right_join(colors$alignment_colors, by = "alignment")
  
  # ----- [2] Calculate Summary Statistics -----
  
  # Calculate medians for researchers and partners in one step
  # Instead of separate dataframes, we'll use a single calculation with grouping
    medians <- alignment_df |>
      group_by(role, alignment) |>
      summarize(
        int_median = round(interp.median(rating, w = 1), 2),
        .groups = "drop"
      ) |>
      pivot_wider(
        names_from = role,
        values_from = int_median
      ) |>
    rowwise() |>
      mutate(
        overall = round(geometric.mean(c(researcher, partner)), 2)
      ) |>
      ungroup() |>
    left_join(distinct(select(alignment_df, alignment, color)), by = "alignment")
  
  # ----- [3] Calculate Inter-rater Agreement (ICC) -----
  
  # Calculate ICC score to measure alignment between researchers and partners
  icc_score <- icc(
    select(medians, researcher, partner),
    type = "agreement",
    model = "twoway",
    unit = "single"
  )
  
  alignment_score <- sprintf("%.2f", abs(icc_score$value))
  alignment_label <- bquote(S[a] == .(alignment_score))
  
  # ----- [4] Prepare Data for Plotting -----
  
  # Pivot the data longer for plotting
  plot_data <- medians |>
    pivot_longer(
      cols = c(researcher, partner, overall),
      names_to = "group",
      values_to = "value"
    ) |>
    # Rename groups for display
    mutate(
      group = case_when(
        group == "researcher" ~ "Researchers",
        group == "partner" ~ "Partners",
        .default = "Overall"
      )
    ) |>
    # Create ordered factor for consistent plotting
    mutate(group = factor(group, levels = c("Researchers", "Partners", "Overall"), ordered = TRUE))
  
  # Get first and last group for labels
  first_group <- levels(plot_data$group)[1]
  last_group <- levels(plot_data$group)[length(levels(plot_data$group))]
  
  # ----- [5] Create Visualization -----
  
  # Enable showtext for custom font rendering
  showtext_auto()
  
  # Create base plot
  alignment_plot <- ggplot(
    data = plot_data,
    aes(
      x = group,
      y = value,
      group = alignment,
      color = color
    )
  ) +
    # Add lines and points
    geom_line(linewidth = 0.5) +
    geom_point() +
    scale_x_discrete(position = "top") +
    scale_y_continuous(labels = label_number(accuracy = 0.01)) +
    scale_color_identity() +
    
    # Add left side labels
    geom_text_repel(
      data = filter(plot_data, group == first_group), 
      aes(label = alignment),
      family = "belltoposans",
      fontface = "italic",
      size = 14,
      nudge_x = -0.3,
      direction = "y",
      hjust = 1,
      segment.size = 0.25,
      segment.color = "#4A4A4A",
      box.padding = 0.5
    ) +
    
    # Add right side labels
    geom_text_repel(
      data = filter(plot_data, group == last_group),
      aes(label = alignment),
      family = "belltoposans",
      fontface = "italic",
      size = 14,
      nudge_x = 0.3,
      direction = "y",
      hjust = 0,
      segment.size = 0.25,
      segment.color = "#4A4A4A",
      box.padding = 0.5
    ) +
    
    # Add value labels
    geom_text(
      data = plot_data,
      aes(label = value),
      size = 10,
      nudge_y = 0.015,
      family = "belltoposans",
      fontface = "bold"
    ) +
    
    # Style the plot
    theme(
      legend.position = "none",
      panel.background = element_blank(),
      panel.border = element_blank(),
      plot.margin = margin(30, 30, 30, 30),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.text.y = element_text(size = 24, family = "belltoposans", face = "italic", color = "#4A4A4A"),
      axis.text.x = element_text(size = 48, family = "xetbook", face = "italic", color = "#4A4A4A"),
      panel.grid.major.y = element_line(linewidth = 0.25, color = "#E0E0E0"),
      panel.grid.major.x = element_blank(),
      text = element_text(family = "xetbook")
    )
  
  # Create dashboard version (might have different styling)
  alignment_plot_dashboard <- alignment_plot
  
  # Add alignment score to plots
  alignment_plot <- add_score(alignment_plot, alignment_label)
  alignment_plot_dashboard <- add_score(alignment_plot_dashboard, alignment_label)
  
  # Add title to dashboard plot
  alignment_plot_dashboard <- add_title_line(
    "Alignment",
    alignment_plot_dashboard,
    16,
    colors$foreground_color,
    colors$background_color
  )
  
  # ----- [6] Export Results -----
  
  # Save the plot as PNG
  alignment_outputs <- build_outname("alignment", project_name, report_date)
  ggsave(
    alignment_outputs, 
    alignment_plot, 
    units = "in", 
    dpi = 300,
    device = ragg::agg_png, 
    width = 16, 
    height = 9
  )
  
  # Export alignment data for further use
  alignment_export_df <- plot_data |>
    pivot_wider(
      id_cols = "alignment",
      names_from = "group",
      values_from = "value"
    ) |>
    #rename(Alignment = alignment)
    select(Alignment = alignment, Researchers, Partners, Overall)
  
  # Store results in global environment
#  assign("alignment_plot_dashboard", alignment_plot_dashboard, envir = .GlobalEnv)
  assign("alignment_df", alignment_export_df, envir = .GlobalEnv)
  assign("alignment_score", alignment_score, envir = .GlobalEnv)
  
  # Return the dashboard plot
  return(alignment_plot_dashboard)
}