# Project Cascade Effects: A Network Diffusion Visualization Tool
# ==============================================================================
#
# This module creates visualizations of network cascade effects - showing how influence 
# or information spreads through different layers of a network. The visualization uses
# a radial plot to display the strength of cascade effects at each layer of diffusion.
#
# OVERVIEW:
# This implementation models network diffusion by combining multiple network metrics:
# - Layer-Weighted Diffusion Degree: Measures how information spreads with layer-specific decay
# - Personalized PageRank: Captures influence flow from seed nodes
# - Gravitational Model: Accounts for node degree and network distance
#
# The final visualization presents a radial plot with each network layer's cascade effect,
# allowing for intuitive comparison of influence across different network depths.
#
#' @param project_name Name of the project
#' @param snapshot_date Date of the data snapshot
#' @param report_date Date of the report
#' @param colors List of color definitions from setup.R
#'
#' @return The generated plot object (also saves file as side effect)
create_cascade_plots <- function(project_name, snapshot_date, report_date, colors) {
  # SECTION 1: DATA LOADING AND PREPARATION
  # ---------------------------------------
  # Load cascade network data from CSV, constructed from project name and snapshot date
  cascade_filename <- build_inname("cascade_effects", project_name, snapshot_date)
#cascade_filename <- "data/Rising Waters, Resilient Communities - cascade_effects_data - 2025-04-30.csv"
  # Load network data and remove incomplete records to ensure analysis integrity
  network_df <- read_csv(cascade_filename, col_names = TRUE) |>
    na.omit()
  
  # SECTION 2: NETWORK STRUCTURE CREATION
  # -------------------------------------
  # Extract all unique nodes from both "from" and "to" columns to ensure complete network coverage
  all_nodes <- tibble(name = unique(c(network_df$from, network_df$to)))
  
  # Build a node→layer lookup table by extracting every node-layer pair
  # This identifies which network layer each node belongs to
  node_layer_lookup <- network_df |>
    select(name = from, layer) |>
    bind_rows(network_df |> select(name = to, layer)) |>
    distinct(name, layer)
  
  layer_count <- node_layer_lookup |>
    group_by(layer) |>
    summarize(count = n_distinct(name))

  total_count <- sum(layer_count$count)
  
  # Ensure every node is assigned a layer (unassigned nodes get Inf as default)
  node_layers <- all_nodes |>
    left_join(node_layer_lookup, by = "name") |>
    mutate(layer = replace_na(layer, Inf)) |>
    distinct(name, .keep_all = TRUE) |>
    mutate(
      # Apply layer-specific decay factor based on Leng et al., 2018
      layer_decay = case_when(
        layer == 1 ~ 0.9,
        layer == 2 ~ 0.5,
        layer == 3 ~ 0.45,
        TRUE ~ 0  # For any other layers
      ))
  
  # Construct directed graph from data frame
  # The graph preserves node attributes including layer information
  network_graph <- graph_from_data_frame(
    d = network_df |> select(from, to),
    directed = FALSE,
    vertices = node_layers
  )
  
  # SECTION 3: NETWORK METRICS CALCULATION
  # -------------------------------------
  
  # Extract layer information for each node
  layers <- V(network_graph)$layer

  alpha <- 0.4
  beta <- 0.3
  lambda <- 0.3
  
  network_matrix <- as_adjacency_matrix(network_graph, sparse = FALSE)
  
  # Calculate network metrics
  topology_efficiency <- global_efficiency(network_graph)
  topology_connectedness <- sna::connectedness(network_matrix)
  topology_hierarchy <- 1 - unname(sna::hierarchy(network_matrix))
  topology_lubness <- 1 - sna::lubness(network_matrix)
  local_community <- normalize(communitycent(network_graph))
  local_betweenness <- normalize(communibet(network_graph))
  local_crossclique <- normalize(crossclique(network_graph))
  local_clustcoef <- normalize(transitivity(network_graph, type = "local") %>%
    replace(is.nan(.), 0))
  global_eigenv <- normalize(igraph::eigen_centrality(network_graph)$vector)
  global_betweenness <- normalize(igraph::betweenness(network_graph))
  global_flow <- normalize(sna::flowbet(network_matrix))
  global_harmonic <- normalize(igraph::harmonic_centrality(network_graph))

  topology_score <- round((((
    topology_efficiency +
      topology_connectedness +
      topology_hierarchy +
      topology_lubness) / 4) *
    lambda), 2)

  # Create comprehensive data frame with all metrics and derived scores
  cascade_df <- node_layers |>
    right_join(layer_count, by = "layer") |>
    rename("gamma" = "layer_decay") |>
    arrange(name) |>
    mutate(
      # Store raw metrics for reference
      raw_knitting = gamma *
        alpha * local_community +
        beta * global_eigenv +
        topology_score,
      raw_bridging = gamma *
        alpha * local_crossclique +
        beta * global_betweenness +
        topology_score,
      raw_channeling = gamma *
        alpha * normalize(local_betweenness) +
        beta * normalize(global_flow) +
        topology_score,
      raw_reaching = gamma *
        alpha * normalize(local_clustcoef) +
        beta * normalize(global_harmonic) +
        topology_score,
      
      knitting = normalize(min_rank(raw_knitting)),
      bridging = normalize(min_rank(raw_bridging)),
      channeling = normalize(min_rank(raw_channeling)),
      reaching = normalize(min_rank(raw_reaching)),
      
      # Calculate composite rank combining all metrics
      composite_rank = ((knitting + bridging + channeling + reaching) / 4)
    ) |>

    # Calculate aggregate metrics for each layer
    group_by(layer) |>
    mutate(
      raw_count_for_this_degree = first(count),
      mu = calculate_proportional_multiplier(raw_count_for_this_degree, layer_count$count, min_multiplier = 0.75, max_multiplier = 1.0),
      layer_knitting = round(mean(knitting) * mu, 2),      # Average knitting score by layer
      layer_bridging = round(mean(bridging) * mu, 2),      # Average bridging score by layer
      layer_channeling = round(mean(channeling) * mu, 2),  # Average channeling score by layer
      layer_reaching = round(mean(reaching) * mu, 2),
      layer_score = round(mean(composite_rank) * mu, 2)  # Overall layer impact score
    ) |>
    ungroup()
  
  # Extract layer-level metrics for visualization
  # Join with color definitions for consistent visual styling
  cascade_df <- cascade_df |>
    distinct(layer, .keep_all = TRUE) |>
    mutate(layer_number = as.character(glue("{layer}°"))) |>
    select(layer, layer_number, count, gamma, layer_bridging, layer_channeling, layer_knitting, layer_reaching, layer_score) |>
    right_join(colors$cascade_colors, by = "layer_number")
  
  # SECTION 8: CASCADE SCORE CALCULATION
  # ----------------------------------
  # Calculate Gini-like coefficient as cascade score
  # Higher score indicates more concentrated influence in fewer layers
  cascade_score <- calculate_gini(cascade_df$layer_score)
  
  # Create mathematical notation for our score using bquote
  cascade_label <- bquote(S[c] == .(cascade_score))
  
  # SECTION 9: VISUALIZATION
  # ----------------------
  # Build a radial bar chart visualization showing cascade effects by layer
  cascade_plot <- ggplot(
    data = cascade_df,
    aes(
      y = layer_number,  # Each layer forms a segment of the radial plot
      x = layer_score,   # Bar length represents layer impact
      fill = color_fill  # Colors from predefined scheme
    )
  ) +
    # Create bars representing each layer's impact
    geom_bar(stat = "identity", width = 1) +
    
    # Set x-axis scale to start from 0 with a clean maximum value
    scale_x_continuous(
      limits = c(0, ceiling(max(cascade_df$layer_score)))#,
      #labels = cascade_df$layer_label
    ) +
    
    # Remove padding for cleaner visual appearance
    scale_y_discrete(expand = c(0, 0)) +
    
    # Apply color schemes defined in configuration
    scale_fill_identity() +
    scale_color_identity() +
    
    # Add text labels showing impact value for each layer
    geom_text(
      aes(
        x = layer_score,
        y = layer_number,
        fontface = "bold",
        color = colors$background_color,  # Use contrasting border color for visibility
        label = layer_score    # Display the actual impact value
      ),
      nudge_x = -0.05,  # Slight offset for readability
      size = 16         # Large enough for visibility in final output
    ) +
    
    # Transform from standard bar chart to radial plot
    coord_flip() +
    coord_radial(
      inner.radius = 0.25,     # Create empty center space
      r.axis.inside = TRUE,    # Place r-axis inside plot
      rotate.angle = FALSE,    # Don't rotate angle labels
      expand = FALSE,          # Don't expand beyond data range
      end = 1.75 * pi          # End at 1.75π radians for partial circle
    ) +
    
    # Start with clean black and white theme
    theme_bw() +
    
    # Apply custom styling for visualization clarity
    theme(
      panel.background = element_blank(),  # Remove panel background
      panel.border = element_blank(),      # Remove panel border
      axis.title = element_blank(),        # Remove axis titles
      axis.ticks.y = element_blank(),      # Remove y-axis ticks
      axis.ticks.x = element_blank(),      # Remove x-axis ticks
      # Apply consistent typography
      axis.text.y = element_text(size = 48, family = "xetbook", face = "bold.italic", color = "#4A4A4A"),
      axis.text.x = element_text(size = 24, family = "puritan", face = "italic", color = "#4A4A4A"),
      # Add subtle grid lines for reference
      panel.grid.major = element_line(linewidth = 0.5, color = "#E0E0E0"),
      # Set consistent font family throughout
      text = element_text(family = "xetbook")
    )
  
  # Add summary cascade score to the plot for quick interpretation
  cascade_plot <- add_score(cascade_plot, cascade_label)
  
  # Create dashboard version with title for integration into reporting systems
  cascade_plot_dashboard <- add_title_line(
    "Cascade Effects",
    cascade_plot,
    16,
    colors$foreground_color,
    colors$background_color
  )
  
  # SECTION 10: OUTPUT PREPARATION
  # ----------------------------
  # Prepare tabular data for export/reference
  cascade_df <- cascade_df |>
#    right_join(alternate_df, by = "layer_number") |>
    select(
      "Degree" = "layer_number",
      "People" = "count",
      "Bridging" = "layer_bridging",
      "Channeling" = "layer_channeling",
      "Knitting" = "layer_knitting",
      "Reaching" = "layer_reaching",
      "Score" = "layer_score"
    ) |>
    arrange(Degree)  # Sort by descending score for better readability
  
  # Make results available in global environment for potential further analysis
  assign("cascade_df", cascade_df, envir = .GlobalEnv)
  
  # SECTION 11: EXPORT AND RETURN
  # ---------------------------
  # Generate output filename for saving the visualization
  cascade_outputs <- build_outname("cascade_effects", project_name, report_date)
  
  # Save high-resolution version of the plot using ragg renderer for better typography
  ggsave(
    cascade_outputs, 
    cascade_plot,
    units = "in", 
    dpi = 300,
    device = ragg::agg_png, 
    width = 9, 
    height = 9
  )
  
  # Return the dashboard plot object for inline display in reports
  return(cascade_plot_dashboard)
}
