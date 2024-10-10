
get_plot <- function(vpc, reactiveStrat, input) {
  if (input$isStrat) {
    if (length(reactiveStrat$values) > 1) {
      facet_formula <- paste0("~", paste(reactiveStrat$values, collapse = " + "))
    } else if (length(reactiveStrat$values) == 1) {
      facet_formula <- paste0("~", reactiveStrat$values)
    } else {
      facet_formula <- ""
    }
  } else {
    facet_formula <- ""
  }

  if (input$typeVPC == TRUE) {
    g <- metaExpr({
      ggplot(
        ..(vpc)$stats, aes(x = xbin)
      )
    })
  } else {
    g <- metaExpr({
      ggplot(
        ..(vpc)$stats, aes(x = x)
      )
    })
  }

  if (facet_formula != "") {
    if (input$facetQuantile) {
      facet_formula <- as.formula(paste0("qname", facet_formula))
    } else {
      facet_formula <- as.formula(facet_formula)
    }
    if (input$facetScales == "free") {
      g <- metaExpr({
        ..(g) +
          facet_grid(..(facet_formula), scales = "free", as.table = FALSE)
      })
    } else {
      g <- metaExpr({
        ..(g) +
          facet_grid(..(facet_formula), scales = "fixed", as.table = FALSE)
      })
    }
  }

  if (facet_formula == "" && input$facetQuantile) {
    facet_formula <- as.formula(paste0("~", "qname"))
    if (input$facetScales == "free") {
      g <- metaExpr({
        ..(g) +
          facet_wrap(..(facet_formula), scales = "free", as.table = TRUE)
      })
    } else {
      g <- metaExpr({
        ..(g) +
          facet_wrap(..(facet_formula), scales = "fixed", as.table = TRUE)
      })
    }
  }

  g <- metaExpr({
    ..(g) +
      geom_ribbon(aes(ymin = lo, ymax = hi, fill = qname, col = qname, group = qname), alpha = ..(input$colorFill), col = NA) +
      geom_line(aes(y = md, col = qname, group = qname)) +
      geom_line(aes(y = y, linetype = qname), linewidth = 1)
  })


  isolate({
    if (input$isCensoring) {
      if (input$censorType == "Variable") {
        if (!is.null(reactiveStrat$values)) {
          g <- metaExpr({
            ..(g) +
              geom_hline(
                data = unique(..(vpc)$data[, .(LLOQ), by = eval(..(reactiveStrat$values))]),
                aes(yintercept = !!as.symbol(..(input$censorvar))), linetype = "dotted", size = 1
              ) +
              geom_text(
                data = unique(..(vpc)$data[, .(LLOQ), by = eval(..(reactiveStrat$values))]),
                aes(x = 10, y = LLOQ, label = paste("LLOQ", LLOQ, sep = "="), ), vjust = -1
              )
          })
        } else {
          g <- metaExpr({
            ..(g) +
              geom_hline(
                data = unique(..(vpc)$data[, .(LLOQ)]),
                aes(yintercept = !!as.symbol(..(input$censorvar))), linetype = "dotted", size = 1
              ) +
              geom_text(
                data = unique(..(vpc)$data[, .(LLOQ)]),
                aes(x = 10, y = LLOQ, label = paste("LLOQ", LLOQ, sep = "="), ), vjust = -1
              )
          })
        }
      } else {
        g <- metaExpr({
          ..(g) +
            geom_hline(aes(yintercept = ..(input$userLLOQ)), linetype = "dotted", size = 1) +
            geom_text(aes(x = 10, y = ..(input$userLLOQ), label = paste("LLOQ", ..(input$userLLOQ), sep = "="), ), vjust = -1)
        })
      }
    }


    if (input$isLogDV) {
      if (min(vpc$stats$lo) < 0) {
        g <- metaExpr({
          ..(g) + scale_y_continuous(trans = "log10", limits = c(0.1, max(..(vpc)$stats$hi)))
        })
      } else {
        g <- metaExpr({
          ..(g) + scale_y_log10()
        })
      }
    }
  })

  if (input$isLogX) {
    g <- metaExpr({
      ..(g) + scale_x_log10()
    })
  }

  g <- # isolate({
    metaExpr({
      ..(g) +
        scale_colour_manual(
          name = ..(paste0("Simulated Percentiles\nMedian (lines) ", input$ciUser * 100, "% CI (areas)")),
          breaks = ..(paste0("q", c(input$piUser_low, input$piUser_med, input$piUser_hi))),
          values = ..(c(input$colorLineHi, input$colorLineMed, input$colorLineLo)),
          labels = ..(paste0(c(input$piUser_low, input$piUser_med, input$piUser_hi) * 100, "%"))
        ) +
        scale_fill_manual(
          name = ..(paste0("Simulated Percentiles\nMedian (lines) ", input$ciUser * 100, "% CI (areas)")),
          breaks = ..(paste0("q", c(input$piUser_low, input$piUser_med, input$piUser_hi))),
          values = ..(c(input$colorLineHi, input$colorLineMed, input$colorLineLo)),
          labels = ..(paste0(c(input$piUser_low, input$piUser_med, input$piUser_hi) * 100, "%"))
        ) +
        scale_linetype_manual(
          name = ..(paste0("Observed Percentiles\nMedian (lines) ", input$ciUser * 100, "% CI (areas)")),
          breaks = ..(paste0("q", c(input$piUser_low, input$piUser_med, input$piUser_hi))),
          values = ..(c(input$lineTypeLo, input$lineTypeMed, input$lineTypeHi)),
          labels = ..(paste0(c(input$piUser_low, input$piUser_med, input$piUser_hi) * 100, "%"))
        ) +
        guides(
          fill = guide_legend(order = 2),
          colour = guide_legend(order = 2),
          linetype = guide_legend(order = 1)
        ) +
        theme_bw() +
        theme(
          legend.position = ..(input$legendPosition),
          legend.key.width = grid::unit(2, "cm")
        ) +
        labs(x = ..(input$xlabel), y = ..(input$ylabel))
    })
  # })

  if (!input$showStats && input$typeVPC == TRUE) {
    if (facet_formula != "") {
      if (input$facetScales == "free") {
        g <- isolate(metaExpr({
          ggplot2::ggplot(vpc$strat) +
            facet_wrap(..(facet_formula), scales = "free") +
            theme(
              plot.background = element_rect(fill = "white", colour = "red"),
              legend.position = ..(input$legendPosition),
              legend.key.width = grid::unit(2, "cm")
            ) +
            labs(x = ..(input$xlabel), y = ..(input$ylabel))
        }))
      } else {
        g <- isolate(metaExpr({
          ggplot2::ggplot(vpc$strat) +
            facet_grid(..(facet_formula)) +
            theme(
              plot.background = element_rect(fill = "white", colour = "red"),
              legend.position = ..(input$legendPosition),
              legend.key.width = grid::unit(2, "cm")
            ) +
            labs(x = ..(input$xlabel), y = ..(input$ylabel))
        }))
      }
    } else {
      g <- isolate(metaExpr({
        ggplot2::ggplot(vpc$strat) +
          theme(
            plot.background = element_rect(fill = "white", colour = "red"),
            legend.position = ..(input$legendPosition),
            legend.key.width = grid::unit(2, "cm")
          ) +
          labs(x = ..(input$xlabel), y = ..(input$ylabel))
      }))
    }
  }

  if (input$showPoints) {
    g <- isolate(metaExpr({
      ..(g) + ggplot2::geom_point(data = ..(vpc)$obs, ggplot2::aes(x = x, y = y), size = 1, alpha = 0.4, show.legend = F)
    }))
  } else {
    g
  }

  if (input$showBoundaries) {
    if (is.null(vpc$rqss.obs.fits)) {
      if (!is.null(vpc$strat)) {
        boundaries <- isolate(metaExpr({
          bininfo(..(vpc))[, .(x = sort(unique(c(xleft, xright)))), by = names(..(vpc)$strat)]
        }))
      } else {
        boundaries <- isolate(metaExpr({
          bininfo(..(vpc))[, .(x = sort(unique(c(xleft, xright))))]
        }))
      }
      if (input$showBinning) {
        g <- isolate(metaExpr({
          ..(g) + ggplot2::geom_vline(data = ..(boundaries), ggplot2::aes(xintercept = x), size = rel(0.5), col = "gray80") +
            ggplot2::theme(panel.grid = ggplot2::element_blank())
        }))
      }
      g <- isolate(metaExpr({
        ..(g) + ggplot2::geom_rug(data = ..(boundaries), ggplot2::aes(x = x), sides = "t", size = 1)
      }))
    }
  } else {
    g
  }

  g
}


font_to_family <- function(font_name){
  if(font_name == "Times New Roman"){
    font_family <- "serif"
  } else if (font_name == "Arial") {
    font_family <- "sans"
  } else {
    font_family <- "mono"
  }
  return(font_family)
}
