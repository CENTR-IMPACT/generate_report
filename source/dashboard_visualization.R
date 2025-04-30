# Project Cascade Effects

#' @param project_name Name of the project
#' @param report_date Date of the report
#' 
#' @return The generated plot object (also saves file as side effect)
create_dashboard_plot <- function(project_name, report_date, dynamics_plot_dashboard,
                                  alignment_plot_dashboard, cascade_plot_dashboard,
                                  indicators_plot_dashboard, colors) {

  dashboard_top_row <- wrap_elements(dynamics_plot_dashboard) +
  wrap_elements(alignment_plot_dashboard) +
  wrap_elements(cascade_plot_dashboard) +
  plot_layout(widths = c(1, 1.5, 1))

dashboard_bottom_row <- indicators_plot_dashboard

# dashboard_title <- glue("CEnTR-IMPACT Report for {project_name}")

project_dashboard <-
  wrap_elements(dashboard_top_row) /
  wrap_elements(dashboard_bottom_row) +
  plot_layout(heights = c(4, 3)) #+
  # plot_annotation(
  #   title = dashboard_title,
  #   theme = theme(
  #     plot.title = element_text(
  #       size = 90,
  #       face = "bold",
  #       family = "xetbook",
  #       color = colors$foreground_color,
  #       hjust = 0.5
  #     ),
  #     plot.title.position = "plot",
  #   )
  # )
# Build the output filename
dashboard_outputs <- build_outname("dashboard", project_name, report_date)
ggsave(dashboard_outputs, project_dashboard, units = "in", dpi = 300,
       device = ragg::agg_png, width = 16, height = 10)
}
