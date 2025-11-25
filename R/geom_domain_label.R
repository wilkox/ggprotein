#' A 'ggplot2' geom to add labels to domains
#'
#' `geom_domain_label()` adds labels to domains drawn with
#' `geom_domain()`.
#'
#' Standard 'ggplot2' aesthetics for text are supported (see Aesthetics).
#'
#' @section Aesthetics:
#'
#' - xmin,xmax (required; start and end positions of the domain)
#' - y (required; the molecular backbone)
#' - label (required; the label text)
#' - colour
#' - linewidth
#' - alpha
#' - family
#' - fontface
#' - angle
#'
#' @param mapping,data,stat,position,na.rm,show.legend,inherit.aes,... As
#' standard for ggplot2. inherit.aes is set to FALSE by default.
#' @param place Where to draw the label, either 'inside' the domain (the
#' default) or 'above' it
#' @param align How the text label should be aligned relative to the ends of
#' the domain Default is 'centre'; other options are 'left' and 'right'.
#' @param height `grid::unit()` object giving the height of the label above the
#' molecular backbone, if place is 'outside'. Defaults to 4 mm.
#' @param label_height `grid::unit()` object giving the height of the label
#' text. Defaults to 3 mm.
#'
#' @examples
#'
#' ggplot2::ggplot(demolin_domains, aes(xmin = start, xmax = end, y = 1,
#'                                      label = name)) +
#'   geom_domain(inherit.aes = TRUE) +
#'   geom_domain_label(inherit.aes = TRUE)
#'
#' @seealso [geom_domain()]
#'
#' @export
geom_domain_label <- function(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  na.rm = FALSE,
  show.legend = FALSE,
  inherit.aes = FALSE,
  place = "inside",
  align = "centre",
  height = grid::unit(4, "mm"),
  label_height = grid::unit(3, "mm"),
  ...
) {

  # Check arguments
  check_arguments("place", place, c("inside", "above"),
                  "geom_domain_label")
  check_arguments("align", align, c("centre", "center", "middle", "left",
                                    "right"), "geom_domain_label")
  if (align %in% c("center", "middle")) align <- "centre"

  # Draw default labels
  if (place == "inside") {
    return(ggplot2::layer(
      data = data,
      mapping = mapping,
      stat = stat,
      geom = GeomInsideLabel,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(
        na.rm = na.rm,
        parent_geom = "geom_domain_label",
        label_height = label_height,
        place = align,
        ...
      )
    ))
  }

  # Draw above labels
  if (place == "above") {

    return(ggplot2::layer(
      data = data,
      mapping = mapping,
      stat = stat,
      geom = GeomAboveLabel,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(
        na.rm = na.rm,
        parent_geom = "geom_domain_label",
        height = height,
        place = align,
        label_height = label_height,
        ...
      )
    ))
  }
}
