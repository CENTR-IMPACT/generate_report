# Project Cascade Effects

#' @param project_name Name of the project
#' @param snapshot_date Date of the data snapshot
#' @param report_date Date of the report
#' @param colors List of color definitions from setup.R
#'
#' @return The generated plot object (also saves file as side effect)
create_cascade_plots <- function(project_name, snapshot_date, report_date, colors) {
  cascade_filename <- build_inname("cascade_effects", project_name, snapshot_date)
  network_df <- read_csv(cascade_filename, col_names = TRUE) |>
    na.omit() |>
    mutate(layer_discount = case_when(
      layer == 1 ~ 0.110,
      layer == 2 ~ 0.051,
      layer == 3 ~ 0.049,
      TRUE ~ 0
    ))

  network_graph <- graph_from_data_frame(network_df, directed = TRUE)

  # Get all unique nodes from both "from" and "to" columns
  all_nodes <- unique(c(network_df$from, network_df$to))

  # Create node_df with all nodes
  node_df <- data.frame(node = all_nodes) |>
    left_join(
      network_df |> select(-from) |> distinct(to, .keep_all = TRUE),
      by = c("node" = "to")
    ) |>
    arrange(node) |>
    mutate(
      dd = diffusion.degree(network_graph),
      pr = page_rank(network_graph, directed = TRUE)$vector,
      node_mu = dd * pr
    )

  mu_df <- node_df |>
    group_by(layer) |>
    summarize(layer_mu = sum(node_mu) * first(layer_discount), .groups = "drop") |>
    mutate(layer_mu = round(layer_mu, 2)) |>
    ungroup() |>
    mutate(layer_number = glue("{layer}°")) |>
    right_join(colors$cascade_colors, by = "layer_number")

  cascade_score <- calculate_gini(mu_df$layer_mu)
  cascade_label <- bquote(S[c] == .(cascade_score))
  cascade_plot <- ggplot(
    data = mu_df,
    aes(
      y = layer_number,
      x = layer_mu,
      fill = color_fill
    )
  ) +
    geom_bar(stat = "identity", width = 1) +
    scale_x_continuous(
      limits = c(0, ceiling(max(mu_df$layer_mu))),
      labels = label_number(accuracy = 0.01)
    ) +
    scale_y_discrete(expand = c(0, 0)) +
    scale_fill_identity() +
    scale_color_identity() +
    geom_text(
      aes(
        x = layer_mu,
        y = layer_number,
        color = color_border,
        label = layer_mu
      ),
      nudge_x = 0.1,
      size = 12
    ) +
    coord_flip() +
    coord_radial(
      inner.radius = 0.18,
      r.axis.inside = TRUE,
      rotate.angle = FALSE,
      expand = FALSE,
      end = 1.75 * pi
    ) +
    theme_bw() +
    theme(
      panel.background = element_blank(),
      panel.border = element_blank(),
      axis.title = element_blank(),
      axis.ticks.y = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.y = element_text(size = 48, family = "xetbook", face = "italic", color = "#4A4A4A"),
      axis.text.x = element_text(size = 24, family = "puritan", face = "italic", color = "#4A4A4A"),
      panel.grid.major = element_line(linewidth = 0.5, color = "#E0E0E0"),
      text = element_text(family = "xetbook")
    )

  cascade_plot <- add_score(
    cascade_plot,
    cascade_label
  )

  cascade_plot_dashboard <- add_title_line(
    "Cascade Effects",
    cascade_plot,
    16,
    colors$foreground_color,
    colors$background_color
  )

  # Build the output filename
  cascade_outputs <- build_outname("cascade_effects", project_name, report_date)

  # Save the plot as a PNG file
  ggsave(cascade_outputs, cascade_plot,
    units = "in", dpi = 300,
    device = ragg::agg_png, width = 9, height = 9
  )

  # Return the plot object for potential inline display
  # return(
  #   list(
  #     plot = cascade_plot_dashboard,
  #     info = mu_df
  #     )
  # )
}
