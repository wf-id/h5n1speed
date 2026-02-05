#' Generate the outbreak chart
#'
#' This function generates the outbreak chart from the data-raw/2025-11-21-h5n1-dairy.csv file.
#'
#' @param data The path to the data file.
#'
#' @return A ggplot object.
#' @export
#'

generate_outbreak_chart <- function(data = here::here("data-raw", "2025-11-21-h5n1-dairy.csv")) {
  data <- readr::read_csv(data)
  use_dat <- data |>
    tidyr::fill(Confirmed, .direction = "down") |>
    tidyr::fill(State, .direction = "down") |>
    dplyr::group_by(Confirmed) |>
    dplyr::summarise(
      n = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::mutate(Confirmed = lubridate::dmy(Confirmed)) |>
    dplyr::arrange(Confirmed, decreasing = TRUE) |>
    dplyr::mutate(cummulative = cumsum(n)) |>
    dplyr::mutate(l_cummulative = log(cummulative))
  sum(use_dat$n)
  min_date <- min(use_dat$Confirmed, na.rm = TRUE)
  cat("Minimum date: ", min_date, "\n")

  fig <- use_dat |>
    ggplot2::ggplot(ggplot2::aes(Confirmed, cummulative)) +
    ggplot2::geom_line(linewidth = 1.5) +
    ggplot2::scale_y_continuous(expand = c(0, 0), name = "Cumulative number of reported outbreaks (log scale)", transform = "log", breaks = c(1, 5, 10, 25, 50, 100, 500, 1000)) +
    ggplot2::scale_x_date(name = "Date of outbreak report", date_breaks = "3 month", date_labels = "%b %Y", expand = c(0, 15)) +
    ggplot2::theme_classic(base_size = 22) +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = ggplot2::rel(1.5)))
  cowplot::ggsave2(filename = here::here("manuscript", "figures", "outbreak.pdf"), fig, height = 10, width = 10)
}
# generate_outbreak_chart()
