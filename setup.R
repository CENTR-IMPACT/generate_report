################################################################################
# setup.R - CEnTR-IMPACT Report Setup and Utility Functions
################################################################################
# 
# This script provides the foundational setup for CEnTR-IMPACT reports, including:
# - Loading required libraries
# - Setting up typography and fonts
# - Defining the color palettes and visual identity
# - Creating utility functions for file naming and data processing
# - Initializing the environment for report generation
#
# The goal is to centralize all setup and utility functions in one place,
# making the main Quarto document cleaner and more accessible to new users.
################################################################################

# ==============================================================================
# SECTION 1: LIBRARY LOADING
# ==============================================================================
# We load all the packages needed for the entire report process here.
# This centralizes dependencies and makes it easier to troubleshoot
# any package-related issues.

#' Load all required libraries for the CEnTR-IMPACT report
#'
#' This section loads packages for:
#' - Data visualization (ggplot2, cowplot, etc.)
#' - Data manipulation (dplyr, tidyr)
#' - Network analysis (igraph, centiserve)
#' - Typography and fonts (showtext)
#' - File paths and I/O operations (this.path, readr)
#' - Statistical analysis (irr, DescTools, psych)

# Visualization packages
library(ggplot2)      # The foundation of our visualization system
library(cowplot)      # For plot composition and layout
library(ggtext)       # For rich text in plots
library(showtext)     # For custom font handling
library(ggrepel)      # For non-overlapping text labels
library(grid)         # For low-level grid graphics
library(ragg)         # For high-quality raster graphics device
library(patchwork)    # For combining plots
library(scales)       # For scale transformations
library(ggforce)      # For circles

library(knitr)
library(kableExtra)

# Network analysis packages
library(igraph)       # For network/graph representation
library(centiserve)   # For centrality measures in networks

# File and path management
library(readr)        # For reading CSV files
library(this.path)    # For working with file paths

# Data manipulation
library(dplyr)        # For data transformation
library(tidyr)        # For data tidying
library(glue)         # For string interpolation

# Statistical analysis
library(irr)          # For inter-rater reliability metrics
library(DescTools)    # For descriptive statistics and diversity measures
library(psych)        # For psychological statistics and psychometric analysis


# ==============================================================================
# SECTION 2: TYPOGRAPHY SETUP
# ==============================================================================
# Typography is a key aspect of CEnTR-IMPACT reports. We use custom fonts
# to ensure consistent branding and readability across all visualizations.

#' Set up custom typography for CEnTR-IMPACT reports
#'
#' This function registers the required fonts with the R graphics system.
#' - XETBook is used as the main body font
#' - BellTopoSans is used for headings and labels
#'
#' @return NULL (fonts are registered as a side effect)
setup_typography <- function() {
  # Register XETBook font family (main text font)
  # This serif font provides a traditional, academic feel for body text
  font_add(
    family = "xetbook",
    regular = "fonts/XETBook/XETBook-Regular.otf",
    bold = "fonts/XETBook/XETBook-Bold.otf",
    italic = "fonts/XETBook/XETBook-Italic.otf",
    bolditalic = "fonts/XETBook/XETBook-BoldItalic.otf"
  )
  
  # Register BellTopoSans font family (headings and labels)
  # This sans-serif font provides clarity for headings and small text
  font_add(
    family = "belltoposans",
    regular = "fonts/BellTopoSans/BellTopoSans-Regular.otf",
    bold = "fonts/BellTopoSans/BellTopoSans-Bold.otf",
    italic = "fonts/BellTopoSans/BellTopoSans-Italic.otf",
    bolditalic = "fonts/BellTopoSans/BellTopoSans-BoldItalic.otf"
  )
  
  # Note: After registering fonts, they need to be activated with showtext_auto()
  # in the visualization functions that use them
}


# ==============================================================================
# SECTION 3: COLOR DEFINITIONS
# ==============================================================================
# Colors are carefully chosen to represent different aspects of the CEnTR-IMPACT
# framework. Each color has semantic meaning related to the concepts being measured.

#' Define color palettes and color assignments for CEnTR-IMPACT visualizations
#'
#' This function creates a comprehensive set of color definitions including:
#' - Main color palette for primary elements
#' - Fill palette for area fills (lighter versions of main colors)
#' - Background and foreground colors
#' - Color assignments for specific domains and concepts
#'
#' Each color is chosen with intention to represent specific qualities and
#' maintain visual harmony and accessibility.
#'
#' @return A list containing all color definitions and assignments
define_colors <- function() {
  # Create a container for all color-related objects
  result <- list()
  
  # ----- Main Color Palette -----
  # Each color in the main palette has specific semantic associations
  result$color_palette <- c(
    "#4E342E", # Espresso - A rich brown representing grounding and stability
    "#3B6B35", # Forest Green - Symbolizing growth and sustainability
    "#3F5E78", # Denim Blue - Conveying trust and reliability
    "#990000", # Deep Red - Representing impact and emphasis
    "#A64B42", # Rust - A warm earthy tone for connection to community
    "#E18B4E", # Burnt Orange - Signifying energy and engagement
    "#636b2f", # Olive Green - Representing harmony and balance
    "#1E325C", # Rainbow Indigo - For depth and insight
    "#E8D8C3", # Soft Beige - A neutral tone for background elements
    "#4A4A4A"  # Stone Gray - For text and details requiring neutrality
  )
  
  # ----- Fill Palette -----
  # Lighter versions of the main colors for area fills and backgrounds
  # These maintain the semantic associations while being less visually dominant
  result$fill_palette <- c(
    "#8e8380", # Espresso (lighter tint) - For filled areas requiring brown tones
    "#8ba086", # Forest Green (lighter tint) - For areas representing growth
    "#8c99a7", # Denim Blue (lighter tint) - For trustworthy background fills
    "#b26f6b", # Deep Red (lighter tint) - For emphasis without overwhelming
    "#bd908b", # Rust (lighter tint) - For community-related area fills
    "#f3d1b8", # Burnt Orange (lighter tint) - For engagement metrics fills
    "#9da183", # Olive Green (lighter tint) - For harmony-related fills
    "#6b7894", # Rainbow Indigo (lighter tint) - For insight-related fills
    "#f0e5d7", # Soft Beige (lighter tint) - For subtle background variation
    "#888888"  # Stone Gray (lighter tint) - For neutral fills and backgrounds
  )
  
  # ----- Overall Background and Foreground Colors -----
  # These are used consistently across all visualizations for unified design
  result$background_color <- "#F2ECD7" # A warm, paper-like tone that reduces eye strain
  result$foreground_color <- "#4A4A4A" # A soft black for better readability than pure black
  
  # ----- Project Dynamics Color Assignments -----
  # Map specific colors to each domain in the Dynamics framework
  # The domain names reflect key aspects of community-engaged research
  result$dynamics_colors <- data.frame(
    domain = c(
      "Contexts",     # The environmental and situational factors
      "Partnerships", # Collaborative relationships between stakeholders
      "Research",     # Investigation and knowledge generation activities
      "Learning",     # Knowledge exchange and capacity building
      "Outcomes"      # Results and impacts of the project
    ),
    # Border colors (from main palette) for outlines and emphasis
    color_border = c(
      result$color_palette[1], # Espresso for Contexts - grounding the project
      result$color_palette[2], # Forest Green for Partnerships - growing together
      result$color_palette[3], # Denim Blue for Research - trustworthy investigations
      result$color_palette[4], # Deep Red for Learning - highlighting knowledge transfer
      result$color_palette[5]  # Rust for Outcomes - lasting impact on communities
    ),
    # Fill colors (from fill palette) for area fills and backgrounds
    color_fill = c(
      result$fill_palette[1], # Lighter Espresso for Contexts fills
      result$fill_palette[2], # Lighter Forest Green for Partnerships fills
      result$fill_palette[3], # Lighter Denim Blue for Research fills
      result$fill_palette[4], # Lighter Deep Red for Learning fills
      result$fill_palette[5]  # Lighter Rust for Outcomes fills
    )
  )
  
  # ----- Project Alignment Color Assignments -----
  # Map specific colors to each alignment dimension
  result$alignment_colors <- data.frame(
    alignment = c(
      "Goals",       # Project objectives and aims
      "Values",      # Guiding principles and ethics
      "Roles",       # Team member functions and responsibilities
      "Resources",   # Materials, funding, and assets
      "Activities",  # Actions and initiatives undertaken
      "Empowerment", # Building capacity and agency
      "Outputs",     # Direct products and deliverables
      "Outcomes"     # Longer-term results and impacts
    ),
    # Single color for each alignment dimension (from main palette)
    color = c(
      result$color_palette[3], # Denim Blue for Goals - directing the vision
      result$color_palette[7], # Olive Green for Values - rooted in principles
      result$color_palette[8], # Rainbow Indigo for Roles - depth of contributions
      result$color_palette[6], # Burnt Orange for Resources - energizing the project
      result$color_palette[1], # Espresso for Activities - grounded in action
      result$color_palette[2], # Forest Green for Empowerment - growth of capacity
      result$color_palette[5], # Rust for Outputs - tangible products
      result$color_palette[4]  # Deep Red for Outcomes - ultimate impact
    )
  )
  
  # ----- Project Cascade Effects Color Assignments -----
  # Map specific colors to each cascade effect layer
  result$cascade_colors <- data.frame(
    layer_number = c(
      "1°", # Primary effects - direct impacts on immediate participants
      "2°", # Secondary effects - impacts on those connected to participants
      "3°"  # Tertiary effects - broader community and systemic impacts
    ),
    # Border colors for each layer (from main palette)
    color_border = c(
      result$color_palette[1], # Espresso for primary effects - solid foundation
      result$color_palette[2], # Forest Green for secondary growth effects
      result$color_palette[3]  # Denim Blue for far-reaching tertiary effects
    ),
    # Fill colors for each layer (from fill palette)
    color_fill = c(
      result$fill_palette[1], # Lighter Espresso for primary effect fills
      result$fill_palette[2], # Lighter Forest Green for secondary effect fills
      result$fill_palette[3]  # Lighter Denim Blue for tertiary effect fills
    )
  )
  
  # ----- Project Indicators Color Assignments -----
  # Map specific colors to each alignment dimension
  result$indicators_colors <- data.frame(
    indicator = c(
      "Community Partners",
      "Engagement Hours",
      "Individuals Served",
      "Infrastructure Tools",
      "Output Products",
      "Students Involved",
      "Successful Outcomes"
    ),
    color_border = c(result$color_palette[1],
                     result$color_palette[2],
                     result$color_palette[3],
                     result$color_palette[4],
                     result$color_palette[6],
                     result$color_palette[5],
                     result$color_palette[7]
    ),
    color_fill = c(result$fill_palette[1],
                   result$fill_palette[2],
                   result$fill_palette[3],
                   result$fill_palette[4],
                   result$fill_palette[6],
                   result$fill_palette[5],
                   result$fill_palette[7]
    )
  )
  
  # Return the complete collection of color definitions
  return(result)
}


# ==============================================================================
# SECTION 4: UTILITY FUNCTIONS
# ==============================================================================
# These functions handle common operations needed across different
# visualization and analysis scripts.

#' Build a standardized input filename for data files
#'
#' Creates a consistent file path for loading data files based on:
#' - The dataset name (e.g., "dynamics", "alignment")
#' - The project name
#' - The snapshot date
#'
#' @param dataset_name String identifying which dataset (e.g., "dynamics")
#' @param project_name String identifying the project
#' @param focus_date Date string for when the data was collected
#' @return A string containing the full file path
build_inname <- function(dataset_name, project_name, focus_date) {
  # Use glue for string interpolation
  return(glue("data/{project_name} - {dataset_name}_data - {focus_date}.csv"))
}

#' Build a standardized output filename for plot files
#'
#' Creates a consistent file path for saving visualization outputs based on:
#' - The dataset name (e.g., "dynamics", "alignment")
#' - The project name
#' - The report date
#'
#' @param dataset_name String identifying which dataset (e.g., "dynamics")
#' @param project_name String identifying the project
#' @param report_date Date string for when the report is generated
#' @return A string containing the full file path
build_outname <- function(dataset_name, project_name, report_date) {
  # Use glue for string interpolation
  return(glue("plots/{project_name} - {dataset_name}_plot - {report_date}.png"))
}

#' Calculate the Gini-Simpson diversity index
#'
#' The Gini-Simpson index measures the diversity of values in a distribution.
#' It ranges from 0 (no diversity) to 1 (maximum diversity).
#' In the CEnTR-IMPACT framework, higher diversity indicates more balanced
#' development across domains.
#'
#' @param metric_name String name of the metric (for reference)
#' @param category_values Vector of values to calculate diversity for
#' @param metric_label Expression or text to use in the visualization label
#' @return A list containing the score and a formatted expression for the label
calculate_gini <- function(category_values) {
  # Calculate the "balance" score using the Gini Index function
  balance_score <- sprintf("%.2f", round(1 - (Gini(category_values)), digits = 2))
  
  # Return both the raw score and the formatted label
  return(balance_score)
}

#' Add a score indicator to a plot
#'
#' Places a large, prominent score value in the upper right corner of a plot.
#' This provides immediate visual feedback on the overall metric.
#'
#' @param base_plot The ggplot object to add the score to
#' @param score_value The score value or expression to display
#' @return A modified plot with the score added
add_score <- function(base_plot, score_value) {
  # Enable showtext for custom font rendering
  showtext_auto()
  
  # Use ggdraw to overlay the text on the plot
  enhanced_plot <- ggdraw() +
    draw_plot(base_plot) +
    draw_grob(
      textGrob(
        label = score_value,
        x = 0.8, y = 0.92,  # Position in upper right quadrant
        hjust = 0.5, vjust = 0.5,
        gp = gpar(fontsize = 48, fontface = "bold", col = "#4A4A4A", fontfamily = "xetbook")
      )
    )
  
  return(enhanced_plot)
}

#' Add a title line to a plot
#'
#' Creates a title panel above the main plot and combines them.
#' This provides a consistent title treatment across visualizations.
#'
#' @param title_text The title text to display
#' @param main_plot The main plot to add the title to
#' @param height_ratio The ratio of main plot height to title height
#' @param foreground_color Color for the title text
#' @param background_color Color for the background
#' @return A combined plot with title and main content
add_title_line <- function(title_text, main_plot, height_ratio, foreground_color, background_color) {
  # Enable showtext for custom font rendering
  showtext_auto()
  
  # Create the title panel as a separate visualization
  title_data <- data.frame(
    x_pos = 1,
    y_pos = 1,
    label = glue("{title_text}")
  )
  
  # Define the title plot with appropriate styling
  title_plot <- ggplot(
    title_data,
    aes(
      x = x_pos,
      y = y_pos,
      label = label
    )
  ) +
    geom_richtext(
      fill = NA,
      label.color = NA,
      color = foreground_color,
      size = 24,
      family = "xetbook",
      fontface = "italic"
    ) +
    theme_light() +
    theme(
      legend.position = "none",
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "transparent"),
      plot.background = element_rect(fill = background_color, color = background_color)
    )
  
  # Combine the title and main plot with proper proportions using patchwork
  combined_plot <-
    title_plot /
    main_plot +
    plot_layout(heights = c(1, height_ratio))
  
  return(combined_plot)
}


# ==============================================================================
# SECTION 5: ENVIRONMENT INITIALIZATION
# ==============================================================================
# This section handles setting up the global environment for the report.

#' Initialize the environment for CEnTR-IMPACT report generation
#'
#' Sets up global variables, working directory, typography, and colors.
#' This function should be called at the beginning of the Quarto document.
#'
#' @param project_name Name of the project
#' @param snapshot_date Date of the data snapshot
#' @param report_date Date of the report
#' @return A list of colors and color assignments
init_environment <- function(project_name, snapshot_date, report_date) {
  # Set global variables for use across all scripts
  assign("project_name", project_name, envir = .GlobalEnv)
  assign("snapshot_date", snapshot_date, envir = .GlobalEnv)
  assign("report_date", report_date, envir = .GlobalEnv)
  
  # Set the working directory to the source file location if using this.path
  if(requireNamespace("this.path", quietly = TRUE)) {
    setwd(this.path::here())
  }
  
  # Setup typography
  setup_typography()
  
  # Define and return colors
  colors <- define_colors()
  return(colors)
}

source("source/dynamics_visualization.R")
source("source/alignment_visualization.R")
source("source/cascade_effects_visualization.R")
source("source/indicators_visualization.R")
source("source/dashboard_visualization.R")

# Initialize the environment with our parameters
colors <- init_environment(params$project_name, params$snapshot_date, params$report_date)
# Generate the dynamics plot
dynamics_plot_dashboard <- create_dynamics_plots(
  project_name,
  snapshot_date,
  report_date,
  colors
)

alignment_plot_dashboard <- create_alignment_plots(
  project_name,
  snapshot_date,
  report_date,
  colors
)

cascade_plot_dashboard <- create_cascade_plots(
  project_name,
  snapshot_date,
  report_date,
  colors
)

indicators_plot_dashboard <- create_indicators_plots(
  project_name,
  snapshot_date,
  report_date,
  colors
)
# 
# create_dashboard_plot(
#   project_name,
#   report_date,
#   dynamics_plot_dashboard,
#   alignment_plot_dashboard,
#   cascade_plot_dashboard,
#   indicators_plot_dashboard,
#   colors
# )
