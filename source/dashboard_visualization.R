#' # Project Cascade Effects
#' 
#' #' @param project_name Name of the project
#' #' @param report_date Date of the report
#' #' 
#' #' @return The generated plot object (also saves file as side effect)
#' create_dashboard_plot <- function(project_name, report_date, dynamics_plot_dashboard,
#'                                   alignment_plot_dashboard, cascade_plot_dashboard,
#'                                   indicators_plot_dashboard, colors) {
#' 
#'   dashboard_top_row <- wrap_elements(dynamics_plot_dashboard) +
#'   wrap_elements(alignment_plot_dashboard) +
#'   wrap_elements(cascade_plot_dashboard) +
#'   plot_layout(widths = c(1, 1.5, 1))
#' 
#' dashboard_bottom_row <- indicators_plot_dashboard
#' 
#' # dashboard_title <- glue("CEnTR-IMPACT Report for {project_name}")
#' 
#' project_dashboard <-
#'   wrap_elements(dashboard_top_row) /
#'   wrap_elements(dashboard_bottom_row) +
#'   plot_layout(heights = c(4, 3)) #+
#'   # plot_annotation(
#'   #   title = dashboard_title,
#'   #   theme = theme(
#'   #     plot.title = element_text(
#'   #       size = 90,
#'   #       face = "bold",
#'   #       family = "xetbook",
#'   #       color = colors$foreground_color,
#'   #       hjust = 0.5
#'   #     ),
#'   #     plot.title.position = "plot",
#'   #   )
#'   # )
#' # Build the output filename
#' dashboard_outputs <- build_outname("dashboard", project_name, report_date)
#' ggsave(dashboard_outputs, project_dashboard, units = "in", dpi = 300,
#'        device = ragg::agg_png, width = 16, height = 10)
#' }


create_dashboard_plot <- function(project_name, report_date, dynamics_plot_dashboard,
                                  alignment_plot_dashboard, cascade_plot_dashboard,
                                  indicators_plot_dashboard, colors) {
  
  # Safety converter function
  ensure_ggplot <- function(plot_obj) {
    if (!inherits(plot_obj, "gg") && !inherits(plot_obj, "patchwork")) {
      # If not a ggplot, create an empty plot with text showing the value
      warning("Converting non-ggplot object to ggplot: ", as.character(plot_obj))
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, 
                        label = paste("PLACEHOLDER:", as.character(plot_obj)),
                        size = 5) +
               theme_void())
    }
    return(plot_obj)
  }
  
  # Ensure all inputs are proper ggplot objects
  dynamics_plot_dashboard <- ensure_ggplot(dynamics_plot_dashboard)
  alignment_plot_dashboard <- ensure_ggplot(alignment_plot_dashboard)
  cascade_plot_dashboard <- ensure_ggplot(cascade_plot_dashboard)
  indicators_plot_dashboard <- ensure_ggplot(indicators_plot_dashboard)
  
  # Rest of your function...
  dashboard_bottom_row <- wrap_elements(dynamics_plot_dashboard) +
    wrap_elements(alignment_plot_dashboard) +
    wrap_elements(cascade_plot_dashboard) +
    plot_layout(widths = c(1, 1.5, 1))
  
  dashboard_top_row <- indicators_plot_dashboard
  
  project_dashboard <-
    wrap_elements(dashboard_top_row) /
    wrap_elements(dashboard_bottom_row) +
    plot_layout(heights = c(3, 4))
  
  # Build the output filename
  dashboard_outputs <- build_outname("dashboard", project_name, report_date)
  ggsave(dashboard_outputs, project_dashboard, units = "in", dpi = 300,
         device = ragg::agg_png, width = 16, height = 10)
}
