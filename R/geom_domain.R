#' A 'ggplot2' geom to draw protein domains
#'
#' `geom_domain()` draws a geom representing a protein domain.
#'
#' The domain is drawn as rounded rectangle, sitting on the molecular
#' backbone.The rectangle has a fill which is light grey by default.
#'
#' The range of the domain on the molecular backbone is set with the `xmin` and
#' `xmax` aesthetics. The molecular backbone that it is associated with is set
#' with the `y` aesthetic.
#'
#' @section Aesthetics:
#'
#' - xmin,xmax (required; start and end positions of the domain)
#' - y (required; the molecular backbone)
#' - alpha
#' - colour
#' - linetype
#' - linewidth
#' - fill
#'
#' @param mapping,data,stat,position,na.rm,show.legend,inherit.aes,... As
#' standard for ggplot2. inherit.aes is set to FALSE by default.
#' @param height `grid::unit()` opbject giving the height of the rectangle
#' representing the domain. Defaults to 5 mm.
#'
#' @examples
#'
#' ggplot2::ggplot(demolin_domains, aes(xmin = start, xmax = end, y = 1)) +
#'   geom_domain(inherit.aes = TRUE)
#'
#' @seealso [geom_domain_label()]
#'
#' @export
geom_domain <- function(
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
    geom = GeomDomain, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      height = height,
      ...
    )
  )
}

#' GeomDomain
#' @noRd
GeomDomain <- ggplot2::ggproto("GeomDomain", ggplot2::Geom,
  required_aes = c("xmin", "xmax", "y"),
  default_aes = ggplot2::aes(
    forward = TRUE,
    alpha = 1,
    colour = "black",
    fill = "lightgrey",
    linetype = 1,
    linewidth = 0.3
  ),

  draw_key = function(data, params, size) {
    grid::rectGrob(
      width = grid::unit(1, "npc") - grid::unit(1, "mm"),
      height = grid::unit(1, "npc") - grid::unit(1, "mm"),
      gp = grid::gpar(
        col = data$colour,
        fill = ggplot2::alpha(data$fill, data$alpha),
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
      cl = "domaintree",
      height = height,
      coord_system = coord_system
    )
    gt$name <- grid::grobName(gt, "geom_domain")
    gt
  }
)

#' @importFrom grid makeContent
#' @export
makeContent.domaintree <- function(x) {

  data <- x$data

  # Prepare grob for each domain
  grobs <- lapply(seq_len(nrow(data)), function(i) {

    domain <- data[i, ]

    # Set up geometry. It's convenient to divide awayness by 2 here
    r <- ifelse(x$coord_system == "polar", domain$away, NA)
    awayness <- unit_to_alaw(x$height, "away", x$coord_system, r) / 2
    corner_points <- 20
    corner_r <- unit_to_alaw(grid::unit(1, "mm"), "along", x$coord_system, r)

    # Define polygon for the roundrect
    alongs <- c(
      domain$along_min + corner_r + corner_r * 
        cos(seq(pi, pi/2, length.out = corner_points)),
      domain$along_max - corner_r + corner_r * 
        cos(seq(pi/2, 0, length.out = corner_points)),
      domain$along_max - corner_r + corner_r * 
        cos(seq(0, -pi/2, length.out = corner_points)),
      domain$along_min + corner_r + corner_r * 
        cos(seq(-pi/2, -pi, length.out = corner_points))
    )
    aways <- c(domain$away + awayness - corner_r + corner_r * 
               sin(seq(pi, pi/2, length.out = corner_points)), 
               domain$away + awayness - corner_r + corner_r * 
                 sin(seq(pi/2, 0, length.out = corner_points)),
               domain$away - awayness + corner_r + corner_r * 
                 sin(seq(0, -pi/2, length.out = corner_points)), 
               domain$away - awayness + corner_r + corner_r * 
                 sin(seq(-pi/2, -pi, length.out = corner_points)))

    # If in polar coordinates, segment the polygon
    if (x$coord_system == "polar") {
      segmented <- segment_polargon(alongs, aways)
      alongs <- segmented$thetas
      aways <- segmented$rs
    }

    # Convert polygon into Cartesian coordinates within the grid viewport
    coords <- alaw_to_grid(alongs, aways, x$coord_system, r)

    # Create polygon grob
    pg <- grid::polygonGrob(
      x = coords$x,
      y = coords$y,
      gp = grid::gpar(
        fill = ggplot2::alpha(domain$fill, domain$alpha),
        col = ggplot2::alpha(domain$colour, domain$alpha),
        lty = domain$linetype,
        lwd = domain$linewidth * ggplot2::.pt
      )
    )

    # Return the polygon grob
    pg
  })

  class(grobs) <- "gList"
  grid::setChildren(x, grobs)
}
