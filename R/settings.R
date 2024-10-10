
initialize_settings <- list(
  arrange.ncol = 3,
  arrange.nrow = 2,
  axis.color = "#000000",
  axis.face = "plain",
  axis.font = "Arial",
  axis.scale = "free",
  axis.size = 12,
  background.border = FALSE,
  background.color = "#EDEDED",
  background.gridlines = FALSE,
  boxplot.alpha = 0.5,
  boxplot.line.color = "#BEBEBE",
  boxplot.fill.color = "#A4AEC4",
  axis.log.type = NULL,
  caption.color = "#000000",
  caption.face = "plain",
  caption.font = "Arial",
  caption.size = 10,
  certara.theme = TRUE,
  density.alpha = 0.5,
  density.line.color = "#BEBEBE",
  density.fill.color = "#A4AEC4",
  density.line.type = "blank",
  density.size = 1,
  facet.ncol = 3,
  facet.nrow = 2,
  guide.line.alpha = 0.5,
  guide.line.color = "#000000",
  guide.line.size = 1,
  guide.line.type = "dashed",
  guide.line.extra.y1 = 2,
  guide.line.extra.y2 = -2,
  hist.alpha = 0.5,
  hist.line.color = "#BEBEBE",
  hist.fill.color = "#A4AEC4",
  hist.line.type = "blank",
  hist.nbins = 8,
  hist.size = 1,
  indplots.legend.position = "bottom",
  indplots.line.color.IPRED = "#1B3152",
  indplots.line.color.PRED = "#DB4C1C",
  indplots.line.type.IPRED = "solid",
  indplots.line.type.PRED = "dashed",
  indplots.point.alpha.DV = 0.75,
  indplots.point.color.DV = "#A4AEC4",
  indplots.point.size.DV = 3,
  indplots.point.shape.DV = 16,
  legend.color = "#000000",
  legend.face = "plain",
  legend.font = "Arial",
  legend.size = 12,
  line.alpha = 50,
  line.color = "#000000",
  line.size = 1,
  line.type = "solid",
  outlier.alpha = 50,
  outlier.color = "#FF0000",
  outlier.shape = 1,
  outlier.size = 1,
  plot.distribution.type = "hr",
  plot.scatter.guide = TRUE,
  plot.scatter.smoothing = "loess",
  plot.scatter.type = "ps",
  point.alpha = 50,
  point.color = "#757D8F",
  point.shape = 16,
  point.size = 3,
  rug.color = "#BEBEBE",
  rug.line.size = 1,
  rug.sides = "b",
  smooth.line.alpha = 0.5,
  smooth.line.color = "#D63636",
  smooth.line.size = 1.2,
  smooth.line.type = "solid",
  smooth.span = 0.75,
  subtitle.color = "#000000",
  subtitle.face = "plain",
  subtitle.font = "Arial",
  subtitle.size = 14,
  text.default = TRUE,
  title.color = "#000000",
  title.face = "bold",
  title.font = "Arial",
  title.size = 18,
  vpc.axis.scale = "free",
  vpc.facet.category = TRUE,
  vpc.facet.quantile = FALSE,
  vpc.legend.position = "top",
  vpc.line.type.lo = "dashed",
  vpc.line.type.med = "solid",
  vpc.line.type.hi = "dashed",
  vpc.line.color.lo = "#D63636",
  vpc.line.color.med = "#3648D6",
  vpc.line.color.hi = "#D63636",
  vpc.line.type.prob0 = "dashed",
  vpc.line.type.prob1 = "solid",
  vpc.line.type.prob2 = "dashed",
  vpc.line.type.prob3 = "dotted",
  vpc.line.type.prob4 = "solid",
  vpc.line.color.prob0 = "#6DDA00",
  vpc.line.color.prob1 = "#3648D6",
  vpc.line.color.prob2 = "#D63636",
  vpc.line.color.prob3 = "#3648D6",
  vpc.line.color.prob4 = "#D63636",
  vpc.pi.alpha.fill = 0.1,
  vpc.point.alpha = 0.5,
  vpc.point.color = "#757D8F",
  vpc.point.shape = "circle-fill",
  vpc.point.size = 3,
  vpc.show.binning = FALSE,
  vpc.show.boundaries = TRUE,
  vpc.quantile.type = "Type 6"
)


update_settings_vpc <- function(settings, input) {
  settings$axis.color <- input$colorAxis
  settings$axis.face <-  input$faceAxis
  settings$axis.font <- input$fontAxis
  settings$axis.size <- input$sizeAxis
  settings$background.border <- input$isShowBorder
  settings$background.color <- input$colorBackground
  settings$background.gridlines <- input$showGridLines
  settings$certara.theme <- input$isCertaraTheme
  settings$vpc.axis.scale <- input$axisScales
  settings$vpc.legend.position <- input$legendPosition
  settings$vpc.pi.alpha.fill <- input$alphaFill
  settings$vpc.point.alpha <- input$alphaPoint
  settings$vpc.point.color <- input$colorPoint
  settings$vpc.point.shape <- input$shapePoint
  settings$vpc.point.size <- input$sizePoint
  settings$vpc.show.binning <- input$showBinning
  settings$vpc.show.boundaries <- input$showBoundaries
  settings$vpc.quantile.type <- input$quantileType

  return(settings)
}


update_settings_vpc_cat <- function(settings, input) {
  settings$vpc.facet.category <- input$facetQuantile
  settings$vpc.line.type.prob0 <- ifelse(is.null(input$typeLineprob0), "dashed", input$typeLineprob0)
  settings$vpc.line.type.prob1 <- ifelse(is.null(input$typeLineprob1), "solid", input$typeLineprob1)
  settings$vpc.line.type.prob2 <- ifelse(is.null(input$typeLineprob2), "dashed", input$typeLineprob2)
  settings$vpc.line.type.prob3 <- ifelse(is.null(input$typeLineprob3), "solid", input$typeLineprob3)
  settings$vpc.line.type.prob4 <- ifelse(is.null(input$typeLineprob4), "dashed", input$typeLineprob4)
  settings$vpc.line.color.prob0 <- ifelse(is.null(input$colorLineprob0), "#6DDA00", input$colorLineprob0)
  settings$vpc.line.color.prob1 <- ifelse(is.null(input$colorLineprob1), "#3648D6", input$colorLineprob1)
  settings$vpc.line.color.prob2 <- ifelse(is.null(input$colorLineprob2), "#D63636", input$colorLineprob2)
  settings$vpc.line.color.prob3 <- ifelse(is.null(input$colorLineprob3), "#3648D6", input$colorLineprob3)
  settings$vpc.line.color.prob4 <- ifelse(is.null(input$colorLineprob4), "#D63636", input$colorLineprob4)

  return(settings)
}


update_settings_vpc_cont <- function(settings, input) {
  settings$vpc.facet.quantile <- input$facetQuantile
  settings$vpc.line.type.lo <- ifelse(is.null(input$lineTypeLo), "dashed", input$lineTypeLo)
  settings$vpc.line.type.med <- ifelse(is.null(input$lineTypeMed), "solid", input$lineTypeMed)
  settings$vpc.line.type.hi <- ifelse(is.null(input$lineTypeHi), "dashed", input$lineTypeHi)
  settings$vpc.line.color.lo <- ifelse(is.null(input$colorLineLo), "#D63636", input$colorLineLo)
  settings$vpc.line.color.med <- ifelse(is.null(input$colorLineMed), "#3648D6", input$colorLineMed)
  settings$vpc.line.color.hi <- ifelse(is.null(input$colorLineHi), "#D63636", input$colorLineHi)

  return(settings)
}


#' A ggplot2 theme for Certara.
#'
#' @inheritParams ggplot2::theme_grey
#' @inheritParams ggplot2::continuous_scale
#' @param grid Which grid lines should appear? Horizontal only, both horizontal and vertical, or none (default).
#' \code{\link[ggplot2]{continuous_scale}()}.
#' @details There are 3 variants of the theme: no grid
#' \code{theme_certara()}, full grid \code{theme_certara(grid = "both")}, and
#' horizontal grid lines only \code{theme_certara(grid = "horizontal")}.
#' @return An object of class \code{\link[ggplot2]{theme}()}.
#' @export


theme_certara <- function(base_size = 11,
                          base_family = "",
                          base_line_size = base_size / 22,
                          base_rect_size = base_size / 22,
                          grid = c("none", "horizontal", "both")) {

  grid <- match.arg(grid)

  half_line <- base_size / 2

  theme_c <- theme(
    line = element_line(
      colour = "black",
      size = base_line_size,
      linetype = 1,
      lineend = "butt"
    ),

    rect = element_rect(
      fill = "white",
      colour = "black",
      size = base_rect_size,
      linetype = 1
    ),

    text = element_text(
      family = base_family,
      face = "plain",
      colour = "black",
      size = base_size,
      lineheight = 0.9,
      hjust = 0.5,
      vjust = 0.5,
      angle = 0,
      margin = margin(),
      debug = FALSE
    ),

    axis.line = element_blank(),
    axis.line.x = NULL,
    axis.line.y = NULL,

    axis.text = element_text(
      size = rel(0.9),
      colour = "grey40"
    ),

    axis.text.x = element_text(
      margin = margin(t = 0.8 * half_line / 2),
      vjust = 1
    ),

    axis.text.x.top = element_text(
      margin = margin(b = 0.8 * half_line / 2),
      vjust = 0
    ),

    axis.text.y = element_text(
      # angle=90,
      margin = margin(r = 0.8 * half_line / 2),
      hjust = 0.5
    ),

    axis.text.y.right = element_text(
      angle = -90,
      margin = margin(l = 0.8 * half_line / 2),
      hjust = 0.5
    ),

    axis.ticks = element_line(colour = "grey40", size = 0.3),

    axis.ticks.length = unit(half_line, "pt"),
    axis.ticks.length.x = unit(half_line, "pt"),
    axis.ticks.length.x.top = unit(half_line, "pt"),
    axis.ticks.length.x.bottom = unit(half_line, "pt"),
    axis.ticks.length.y = unit(half_line, "pt"),
    axis.ticks.length.y.left = unit(half_line, "pt"),
    axis.ticks.length.y.right = unit(half_line, "pt"),

    axis.title = element_text(colour = "grey40"),

    axis.title.x = element_text(
      margin = margin(t = half_line / 2),
      vjust = 1
    ),

    axis.title.x.top = element_text(
      margin = margin(b = half_line / 2),
      vjust = 0
    ),

    axis.title.y = element_text(
      angle = 90,
      margin = margin(r = half_line / 2),
      vjust = 1
    ),

    axis.title.y.right = element_text(
      angle = -90,
      margin = margin(l = half_line / 2),
      vjust = 0
    ),

    legend.background = element_rect(colour = NA),

    legend.spacing = unit(2 * half_line, "pt"),
    legend.spacing.x = unit(half_line, "pt"),
    legend.spacing.y = NULL,

    legend.margin = margin(half_line, half_line, half_line, half_line),

    legend.key = element_rect(
      fill = "white", colour = "NA"
    ),

    legend.key.size = unit(1.2, "lines"),
    legend.key.height = NULL,
    legend.key.width = NULL,

    legend.text = element_text(
      size = rel(0.9),
      margin = margin(r = 2 * half_line, unit = "pt")
    ),

    legend.text.align = NULL,

    legend.title = element_text(
      size = rel(0.9),
      hjust = 0
    ),

    legend.title.align = NULL,

    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.justification = "left",

    legend.box = NULL,
    legend.box.margin = margin(0, 0, 0, 0, "cm"),
    legend.box.background = element_blank(),
    legend.box.spacing = unit(2 * half_line, "pt"),

    panel.background = element_rect(
      fill = "white",
      colour = NA
    ),

    panel.border = element_rect(
      fill = NA,
      colour = "grey60",
      size = 0.3
    ),

    panel.grid = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),

    panel.spacing = unit(half_line, "pt"),
    panel.spacing.x = NULL,
    panel.spacing.y = NULL,

    panel.ontop = FALSE,

    strip.background = element_rect(
      fill = "grey90", colour = "grey20"
    ),

    strip.text = element_text(
      colour = "grey30",
      size = rel(0.8),
      margin = margin(0.3 * half_line, 0.3 * half_line, 0.5 * half_line, 0.3 * half_line)
    ),

    strip.text.x = NULL,
    strip.text.y = element_text(angle = -90),

    strip.placement = "inside",
    strip.placement.x = NULL,
    strip.placement.y = NULL,

    strip.switch.pad.grid = unit(half_line / 2, "pt"),
    strip.switch.pad.wrap = unit(half_line / 2, "pt"),

    plot.background = element_rect(colour = "white"),

    plot.title = element_text(
      size = rel(1.2),
      hjust = 0,
      vjust = 1,
      margin = margin(b = half_line)
    ),

    plot.subtitle = element_text(
      hjust = 0,
      vjust = 1,
      margin = margin(b = half_line)
    ),

    plot.caption = element_text(
      size = rel(0.8),
      hjust = 1,
      vjust = 1,
      margin = margin(t = half_line)
    ),

    plot.tag = element_text(
      size = rel(1.2),
      hjust = 0.5,
      vjust = 0.5
    ),

    plot.tag.position = "topleft",

    plot.margin = margin(half_line, half_line, half_line, half_line),

    complete = TRUE
  )

  if (grid == "horizontal") {
    theme_c <- theme_c %+replace% theme(
      panel.grid.major.y = element_line(colour = "grey90", size = 0.3),
      panel.grid.minor.y = element_line(colour = "grey90", size = 0.3)
    )
  } else if (grid == "both") {
    theme_c <- theme_c %+replace% theme(
      panel.grid.major = element_line(colour = "grey90", size = 0.3),
      panel.grid.minor = element_line(colour = "grey90", size = 0.3)
    )
  }

  theme_c
}




change_point_shape <- function(shape) {
  if (shape == "circle") {
    shape <- 1
  } else if (shape == "circle-fill") {
    shape <- 16
  } else if (shape == "square") {
    shape <- 0
  } else if (shape == "square-fill") {
    shape <- 15
  } else if (shape == "triangle") {
    shape <- 2
  } else {
    shape <- 17
  }
}

change_outlier_shape <- function(shape, display) {
  if (shape == "circle") {
    shape <- 1
  } else if (shape == "circle-fill") {
    shape <- 16
  } else if (shape == "square") {
    shape <- 0
  } else if (shape == "square-fill") {
    shape <- 15
  } else if (shape == "triangle") {
    shape <- 2
  } else {
    shape <- 17
  }
  if (!display) shape <- NA

  shape
}
