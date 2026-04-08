save_count_plot <- function(data, category_col, count_col, title, path, fill, flip = TRUE) {
  plot_data <- data
  plot_data[[category_col]] <- factor(plot_data[[category_col]], levels = plot_data[[category_col]])
  max_count <- max(plot_data[[count_col]], na.rm = TRUE)
  label_padding <- max_count * 0.03 + 0.2
  axis_limit <- max_count + label_padding + 0.2
  break_by <- if (max_count <= 6) 1 else 2
  count_breaks <- seq(0, ceiling(axis_limit), by = break_by)

  plot <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = .data[[category_col]], y = .data[[count_col]])
  ) +
    ggplot2::geom_col(fill = fill, width = 0.75) +
    ggplot2::geom_text(
      ggplot2::aes(
        y = .data[[count_col]] + label_padding,
        label = .data[[count_col]]
      ),
      hjust = 0.5,
      vjust = if (flip) 0.5 else 0,
      size = 4.3
    ) +
    ggplot2::labs(
      title = title,
      x = NULL,
      y = "Papers"
    ) +
    ggplot2::scale_y_continuous(
      breaks = count_breaks,
      limits = c(0, axis_limit),
      expand = ggplot2::expansion(mult = c(0, 0))
    ) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 16),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 16), size = 14),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 10), size = 14),
      axis.text = ggplot2::element_text(size = 12),
      panel.grid.minor = ggplot2::element_blank()
    )

  if (flip) {
    plot <- plot +
      ggplot2::coord_flip(clip = "off")
  } else {
    plot <- plot
  }

  ggplot2::ggsave(project_path(path), plot = plot, width = 9, height = 6, dpi = 300)
}

build_location_map_layers <- function() {
  if (!suppressPackageStartupMessages(requireNamespace("sf", quietly = TRUE))) {
    stop("Package 'sf' is required to draw the location map.", call. = FALSE)
  }

  if (!suppressPackageStartupMessages(requireNamespace("rnaturalearth", quietly = TRUE))) {
    stop("Package 'rnaturalearth' is required to draw the location map.", call. = FALSE)
  }

  world_map <- suppressMessages(
    rnaturalearth::ne_countries(scale = "medium", type = "map_units", returnclass = "sf")
  )

  country_units <- world_map |>
    dplyr::filter(normalize_map_unit_name(geounit) == normalize_map_unit_name(admin)) |>
    dplyr::transmute(
      map_unit_level = "country",
      map_unit_key = normalize_map_unit_name(dplyr::coalesce(name_long, geounit, name, admin))
    )

  map_unit_subdivisions <- world_map |>
    dplyr::filter(normalize_map_unit_name(geounit) != normalize_map_unit_name(admin)) |>
    dplyr::transmute(
      map_unit_level = "subnational",
      map_unit_key = normalize_map_unit_name(dplyr::coalesce(name_long, geounit, name))
    )

  admin_one_units <- suppressMessages(rnaturalearth::ne_states(returnclass = "sf")) |>
    dplyr::filter(admin %in% c("United States of America", "Canada")) |>
    dplyr::transmute(
      map_unit_level = "subnational",
      map_unit_key = normalize_map_unit_name(dplyr::coalesce(woe_name, name, gn_name, name_en))
    )

  list(
    world = world_map,
    units = dplyr::bind_rows(country_units, map_unit_subdivisions, admin_one_units) |>
      dplyr::distinct(map_unit_level, map_unit_key, .keep_all = TRUE)
  )
}

save_location_map <- function(data, title, path) {
  map_layers <- build_location_map_layers()
  mapped_units <- dplyr::left_join(map_layers$units, data, by = c("map_unit_level", "map_unit_key"))
  matched_units <- mapped_units |>
    dplyr::filter(!is.na(n_papers))

  unmatched_units <- data |>
    dplyr::anti_join(
      sf::st_drop_geometry(map_layers$units),
      by = c("map_unit_level", "map_unit_key")
    )

  if (nrow(unmatched_units) > 0) {
    warning(
      "Location map units were not matched to Natural Earth polygons: ",
      paste(unmatched_units$map_unit_name, collapse = ", "),
      call. = FALSE
    )
  }

  max_count <- max(data$n_papers, na.rm = TRUE)
  legend_breaks <- seq(1, max_count, by = if (max_count <= 6) 1 else 2)
  region_xlim <- c(-170, 35)
  region_ylim <- c(28, 79)

  plot <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = map_layers$world,
      fill = "grey99",
      color = "grey74",
      linewidth = 0.24
    ) +
    ggplot2::geom_sf(
      data = matched_units |>
        dplyr::filter(map_unit_level == "country"),
      ggplot2::aes(fill = n_papers),
      color = "grey15",
      linewidth = 0.28
    ) +
    ggplot2::geom_sf(
      data = matched_units |>
        dplyr::filter(map_unit_level == "subnational"),
      ggplot2::aes(fill = n_papers),
      color = "black",
      linewidth = 0.34
    ) +
    ggplot2::geom_sf(
      data = map_layers$world,
      fill = NA,
      color = "grey76",
      linewidth = 0.18
    ) +
    ggplot2::scale_fill_gradient(
      name = "Papers",
      low = "grey80",
      high = "black",
      limits = c(0, max_count),
      breaks = legend_breaks,
      na.value = "white"
    ) +
    ggplot2::coord_sf(
      xlim = region_xlim,
      ylim = region_ylim,
      expand = FALSE
    ) +
    ggplot2::labs(title = title) +
    ggplot2::theme_void(base_size = 14) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 18, margin = ggplot2::margin(b = 14)),
      plot.title.position = "plot",
      legend.position = "right",
      legend.title = ggplot2::element_text(size = 14),
      legend.text = ggplot2::element_text(size = 12),
      legend.key.height = grid::unit(16, "pt"),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      legend.background = ggplot2::element_rect(fill = "white", color = NA),
      legend.key = ggplot2::element_rect(fill = "white", color = NA),
      plot.margin = ggplot2::margin(10, 14, 10, 14)
    )

  ggplot2::ggsave(project_path(path), plot = plot, width = 11, height = 7, dpi = 300)
}
