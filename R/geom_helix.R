#' A 'ggplot2' geom to draw alpha helices
#'
#' `geom_helix()` draws a geom representing an alpha helix in a protein
#' secondary structure diagram.
#'
#' The helix is drawn as a horizontal helix, 'wrapped around' the molecular
#' backbone.
#'
#' The range of the helix on the molecular backbone is set with the `xmin` and
#' `xmax` aesthetics. The molecular backbone that it is associated with is set
#' with the `y` aesthetic.
#'
#' @section Aesthetics:
#'
#' - xmin,xmax (required; start and end positions of the helix)
#' - y (required; the molecular backbone)
#' - alpha
#' - colour
#' - linetype
#' - linewidth
#'
#' @param mapping,data,stat,position,na.rm,show.legend,inherit.aes,... As
#' standard for ggplot2. inherit.aes is set to FALSE by default.
#' @param height `grid::unit()` opbject giving the height of the helix.
#' Defaults to 5 mm.
#'
#' @examples
#'
#' ggplot2::ggplot(subset(demolin_ss, type == "helix"), 
#'                 aes(xmin = start, xmax = end, y = 1)) +
#'   geom_helix(inherit.aes = TRUE)
#'
#' @seealso [geom_helix_label()]
#'
#' @export
geom_helix <- function(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = FALSE,
  height = grid::unit(5, "mm"),
  ...
) {

  ggplot2::layer(
    geom = GeomHelix, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      height = height,
      ...
    )
  )
}

#' GeomHelix
#' @noRd
GeomHelix <- ggplot2::ggproto("GeomHelix", ggplot2::Geom,
  required_aes = c("xmin", "xmax", "y"),
  default_aes = ggplot2::aes(
    forward = TRUE,
    alpha = 1,
    colour = "black",
    linetype = 1,
    linewidth = 0.3
  ),

  draw_key = function(data, params, size) {
    grid::rectGrob(
      width = grid::unit(1, "npc") - grid::unit(1, "mm"),
      height = grid::unit(1, "npc") - grid::unit(1, "mm"),
      gp = grid::gpar(
        col = ggplot2::alpha(data$colour, data$alpha),
        lty = data$linetype,
        lwd = data$linewidth * ggplot2::.pt
      )
    )
  },

  setup_data = function(data, params) {
    data
  },

  draw_panel = function(
    data,
    panel_scales,
    coord,
    height
  ) {

    # Detect coordinate system and transform values
    coord_system <- get_coord_system(coord)
    data <- data_to_grid(data, coord_system, panel_scales, coord)

    gt <- grid::gTree(
      data = data,
      cl = "helixtree",
      height = height,
      coord_system = coord_system
    )
    gt$name <- grid::grobName(gt, "geom_helix")
    gt
  }
)

#' @importFrom grid makeContent
#' @export
makeContent.helixtree <- function(x) {

  data <- x$data

  # Prepare grob for each helix
  grobs <- lapply(seq_len(nrow(data)), function(i) {

    helix <- data[i, ]

    # Set up geometry
    r <- ifelse(x$coord_system == "polar", helix$away, NA)

    # Helix parameters
    alongness <- abs(helix$along_max - helix$along_min)
    width_mm <- alongness_to_mm(alongness, x$coord_system, r)
    overlap <- 0.85
    radius_mm <- as.numeric(x$height) / 2
    radius_npc_x <- grid::convertWidth(x$height, "native", valueOnly = TRUE)
    radius_npc_y <- grid::convertHeight(x$height, "native", valueOnly = TRUE)
    turns <- width_mm / (overlap * radius_mm * 2)
    density <- 100 # Points per turn
    n <- floor(density * turns)

    # Generate points
    angles <- (0.5 * pi) - seq(0, by = pi / 100, length.out = n)
    alongs <- seq(
      min(c(helix$along_min, helix$along_max)),
      max(c(helix$along_min, helix$along_max)),
      length.out = n
    ) + (radius_npc_x * cos(angles))
    aways <- helix$away + (radius_npc_y * sin(angles))

    # Convert polyline into Cartesian coordinates within the grid viewport
    coords <- alaw_to_grid(alongs, aways, x$coord_system, r)

    # Create polyline grob
    pg <- grid::polylineGrob(
      x = coords$x,
      y = coords$y,
      gp = grid::gpar(
        col = ggplot2::alpha(helix$colour, helix$alpha),
        lty = helix$linetype,
        lwd = helix$linewidth * ggplot2::.pt
      )
    )

    # Return the polyline grob
    pg
  })

  class(grobs) <- "gList"
  grid::setChildren(x, grobs)
}
