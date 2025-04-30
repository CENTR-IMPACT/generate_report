################################################################################
# dynamics_visualization.R - Visualizing Project Dynamics for CEnTR-IMPACT
################################################################################
# 
# This script generates visualizations for the Dynamics component of 
# CEnTR-IMPACT reports. Project Dynamics represent how the various domains
# of a community-engaged research project interact and develop over time.
#
# The main domains measured in Project Dynamics are:
# - Contexts: Environmental and situational factors
# - Partnerships: Collaborative relationships between stakeholders
# - Research: Investigation and knowledge generation
# - Learning: Knowledge exchange and capacity building
# - Outcomes: Results and impacts
#
# The visualizations use a radar/spider chart approach to show the 
# relative strength of each domain, as well as the dimensions within them.
# A diversity score (Gini-Simpson index) is calculated to measure how
# balanced the development is across domains.
################################################################################

#' Create dynamics visualizations from project data
#' 
#' This function generates comprehensive visualizations of Project Dynamics
#' based on the CEnTR-IMPACT framework. It:
#' 1. Loads and processes dynamics data
#' 2. Calculates domain scores and diversity metrics
#' 3. Creates radar/spider visualizations
#' 4. Adds diversity scores and titles
#' 5. Saves the visualizations as image files
#' 
#' @param project_name Name of the project
#' @param snapshot_date Date of the data snapshot
#' @param report_date Date of the report
#' @param colors List of color definitions from setup.R
#' 
#' @return The generated plot object (also saves file as side effect)
create_dynamics_plots <- function(project_name, snapshot_date, report_date, colors) {
  
  # ============================================================================
  # STEP 1: LOAD AND PROCESS DATA
  # ============================================================================
  # We begin by loading the raw data from CSV and joining it with our color
  # definitions to ensure consistent visual encoding
  
  # Build the filename for the dynamics data file
  dynamics_filename <- build_inname("dynamics", project_name, snapshot_date)
  
  # Load the data and join with color definitions
  # - The data contains dimensions (specific metrics) grouped by domains
  # - Each dimension has a value between 0 and 1
  dynamics_df <- read_csv(dynamics_filename, col_names = TRUE) |>
    right_join(colors$dynamics_colors, by = "domain")
  
  # ============================================================================
  # STEP 2: CALCULATE DOMAIN SCORES
  # ============================================================================
  # For each domain, we calculate a composite score using the geometric mean
  # of its dimension values. The geometric mean is appropriate because:
  # - It handles values between 0 and 1 well
  # - It penalizes imbalances (one very low value will lower the overall score)
  # - It rewards balanced development across dimensions
  
  dynamics_df <- dynamics_df |>
    group_by(domain) |>
    mutate(domain_score = round(mean(dimension_value), 2)) |>
    ungroup()
  
  # Create a separate dataframe for domain-level information
  # This will be used for labels and overall domain representation
  domain_df <- dynamics_df |>
    distinct(domain, .keep_all = TRUE) |>
    select(domain, domain_score, color_text = color_border) |>
    mutate(domain_label = glue("{domain} ({domain_score})")) |>
    # Position labels at specific x-coordinates in the radar chart
    mutate(x = c(2.5, 6.5, 10.5, 14.5, 18.5)) |>
    # Convert domain to a factor for ordered plotting
    mutate(domain = factor(domain, levels = unique(domain), ordered = TRUE))
  
  # ============================================================================
  # STEP 3: CALCULATE BALANCE SCORE
  # ============================================================================
  # The 1 - Gini index measures how balanced the development is
  # across domains. A higher score indicates more equal development.
  
  dynamics_score <- calculate_gini(domain_df$domain_score)
  dynamics_label <- bquote(S[d] == .(dynamics_score))
  
  # Ensure dimensions are ordered consistently for visualization
  dynamics_df$dimension <- factor(dynamics_df$dimension,
                                  levels = unique(dynamics_df$dimension),
                                  ordered = TRUE
  )
  
  # ============================================================================
  # STEP 4: CREATE VISUALIZATION
  # ============================================================================
  # We create two versions of the visualization:
  # 1. A dashboard view showing domain scores
  # 2. A detailed view showing individual dimension values
  
  # Enable showtext for custom font rendering
  showtext_auto()
  
  # ----- BASE DASHBOARD PLOT -----
  # This creates the foundational radar chart showing domain scores
  dynamics_plot_dashboard <- ggplot() +
    # Draw the inner reference circles at 0.25, 0.5, 0.75, and 1.0
    geom_hline(yintercept = c(0.25, 0.5, 0.75, 1),
               color = "#E0E0E0", linewidth = 0.25) +
    # Add labels for the reference circles
    geom_text(
      data = data.frame(
        y = c(0.25, 0.5, 0.75, 1),
        label = c("0.25", "0.50", "0.75", "1.00")
      ),
      aes(x = 0.5, y = y, label = label),  # Position x at first category
      family = "belltoposans",
      fontface = "italic",
      size = 8,
      color = "#4A4A4A",
      hjust = 0.5,
      vjust = 0.5
    ) +
    # Add bars for each domain's score
    geom_bar(data = dynamics_df, aes(
      x = dimension,
      y = domain_score,
      fill = color_fill
    ),
    stat = "identity", width = 1) +
    # Set the y-axis to range from 0 to 1
    scale_y_continuous(
      limits = c(0, 1),
      breaks = c(0, 0.25, 0.5, 0.75, 1),
      labels = label_number(accuracy = 0.01)
    ) +
    # Use discrete x-axis for dimensions
    scale_x_discrete() +
    # Use the fill colors defined in the data
    scale_fill_identity() +
    # Convert to polar coordinates for radar chart appearance
    # Start at -pi/2 to put the first category at the top
    coord_polar(start = -pi/2) +
    # Add domain labels outside the radar chart
    geom_text(
      data = domain_df, aes(
        y = domain_score + 0.2,  # Position labels above the domain's score
        x = x,                   # Use pre-calculated x positions
        color = color_text,      # Use domain-specific colors
        label = domain_label     # Show domain name and score
      ),
      family = "xetbook", fontface = "italic", size = 14
    ) +
    # Use the text colors defined in the data
    scale_color_identity() +
    # Style the plot with a clean, minimal theme
    theme(
      panel.background = element_blank(),
      panel.border = element_blank(),
      axis.title = element_blank(),
      axis.ticks.y = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(linewidth = 0.5, color = "#E0E0E0", linetype = "dashed"),
      panel.grid.minor = element_blank(),
      text = element_text(family = "xetbook")
    )
  
  # ----- DETAILED PLOT -----
  # This enhances the dashboard plot by adding:
  # - Lines connecting the origin to each domain's score
  # - Points showing individual dimension values
  dynamics_plot <- dynamics_plot_dashboard +
    # Add lines from center to each domain's score
    geom_segment(
      data = dynamics_df, aes(
        x = dimension,           # Start at the domain's x position
        y = 0,           # Start at the center (0)
        xend = dimension,   # End at the domain's position
        yend = dimension_value, # End at the domain's score
        color = color_border   # Use domain-specific colors
      ),
      linewidth = 1
    ) +
    # Add points for individual dimension values
    geom_point(data = dynamics_df, aes(
      x = dimension, y = dimension_value, color = color_border
    ), size = 3) +
    # Show dimension labels on the x-axis
    theme(
      axis.text.x = element_text(size = 28, family = "belltoposans", face = "italic", color = "#4A4A4A"))
  
  # ============================================================================
  # STEP 5: ADD SCORES AND TITLES
  # ============================================================================
  # We add the balance score to each plot, as well as a title for context
  
  # Add the balance score to both plots
  dynamics_plot_dashboard <- add_score(
    dynamics_plot_dashboard,
    dynamics_label
  )
  
  dynamics_plot <- add_score(
    dynamics_plot,
    dynamics_label
  )
  
  # Add the title "Dynamics" to the dashboard plot
  dynamics_plot_dashboard <- add_title_line(
    "Dynamics",
    dynamics_plot_dashboard,
    16,
    colors$foreground_color,
    colors$background_color
  )
  
  # ============================================================================
  # STEP 6: SAVE OUTPUT
  # ============================================================================
  # Save the detailed plot to a file for inclusion in the report
  
  # Build the output filename
  dynamics_outputs <- build_outname("dynamics", project_name, report_date)
  
  # Save the plot as a PNG file
  ggsave(dynamics_outputs, dynamics_plot, units = "in", dpi = 300,
         device = ragg::agg_png, width = 9, height = 9)
  
  # Return the plot object for potential inline display
  return(dynamics_plot_dashboard)
}
