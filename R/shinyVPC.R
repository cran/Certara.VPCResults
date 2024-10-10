#' Shiny GUI to parameterize Visual Predictive Check (VPC)
#'
#' Use \code{tidyvpc} package to parameterize VPC from Shiny GUI and customize plot
#' using \code{ggplot2}. Code generation functionality allows you to reproduce \code{tidyvpc}
#' object and \code{ggplot2} plot in your local R session via R script saved to your working
#' directory.
#'
#'
#'
#' @param observed Observed input data.
#' @param simulated Simulated input data.
#' @param ObsName Optional character value specifiying the name of the observed dependent variable (e.g., ObsName).
#' @param vpc.type Character value specifying type of VPC.
#' @param tagged A list of tagged VPC's or model diagnostics created from previous \code{Certara.VPC} or \code{Certara.ModelResults} session.
#' @param settings A list of plot customization settings used in previous \code{Certara.VPC} or \code{Certara.ModelResults} session.
#' @param ... Arguments for initiation from Pirana.
#'
#' @examples
#' if (interactive()) {
#' vpcResultsUI(tidyvpc::obs_data, tidyvpc::sim_data)
#' }
#'
#' @return If \code{interactive()}, returns a list of tagged diagnostics from the Shiny application, otherwise returns \code{TRUE}.
#' @export
#'
vpcResultsUI <- function(observed, simulated, ObsName = NULL, vpc.type = c("continuous", "categorical"),
                         tagged = NULL, settings = NULL, ...) {

  arg_list <- as.list(substitute(list(...)))

  isPirana <- arg_list$pirana

  if (!is.null(isPirana)) {
    if(isPirana){
      pirana_obs_path <- eval(arg_list$pirana_obs_path, envir = vpc_results_env)
      pirana_sim_path <- eval(arg_list$pirana_sim_path, envir = vpc_results_env)
      if (grepl("dta", basename(pirana_obs_path))) {
        software <- "NONMEM"
      } else {
        software <- "NLME"
      }
    pirana <- TRUE
    } else {
      pirana <- FALSE
    }
  } else {
    pirana <- FALSE
  }

  observed_name <- deparse(substitute(observed))

  simulated_name <- deparse(substitute(simulated))

  if (!is.null(ObsName)) {
    check_data_ObsName(observed, simulated, ObsName)
    obs_dv <- ObsName
    observed <- observed[observed$ObsName == obs_dv, ]
    simulated <- simulated[simulated$OBSNAME == obs_dv, ]
  } else {
    obs_dv <- NULL
  }

  if (!is.null(observed$ID5) && !is.null(simulated$ID5)) {
    NLME_VPC <- TRUE
  } else {
    NLME_VPC <- FALSE
    NLME_VPC_PRED <- FALSE
  }
  # Sort data if NLME vpc output
  if (NLME_VPC) {
    observed <- observed %>%
      dplyr::arrange(ID1, ID2, ID3, ID4, ID5, IVAR)

    simulated <- simulated %>%
      dplyr::arrange(REPLICATE, ID1, ID2, ID3, ID4, ID5, IVAR)

    # Check for PRED in simulated data, and add to observed
    predcheck <- check_pred_sim(simulated)

    if (predcheck$hasPred) {
      observed$PRED <- predcheck$data$PRED
      simulated <- simulated[simulated$REPLICATE >= 0, ]
      NLME_VPC_PRED <- TRUE
    } else {
      NLME_VPC_PRED <- FALSE
    }

    #Check censoring
    hasLLOQ <- any(!is.na(observed$LLOQ))
    noMissingLLOQ <- all(!is.na(observed$LLOQ))

    if(hasLLOQ){
      if(!noMissingLLOQ){
      colLLOQ <- unique(na.omit(observed$LLOQ))
      if(length(colLLOQ) > 1) {
        stop("Cannot fill LLOQ column value in data. The LLOQ column has multiple values, including NA. Please replace NA
             values with their corresponding LLOQ values")
      } else {
        observed <-  observed %>%
          mutate(DV = as.numeric(ifelse(DV == "BLOQ", 0, DV)),
                 LLOQ = colLLOQ)
        }
      } else {
        observed <-  observed %>%
          mutate(DV = as.numeric(ifelse(DV == "BLOQ", 0, DV)))
      }
    }
  } else {
    hasLLOQ <- FALSE
    noMissingLLOQ <- FALSE
  }

  if (pirana && !NLME_VPC) {
    if (!is.null(observed$MDV))
    observed <- observed[observed$MDV == 0, ]
    if (!is.null(simulated$MDV))
    simulated <- simulated[simulated$MDV == 0, ]
  }



  if (is.null(settings)) {
    settings <- initialize_settings
  } else {
    settings <- modifyList(initialize_settings, settings)
  }


  tagged_vpc <- .run_shinyVPC(
    observed = observed, simulated = simulated, vpc.type = match.arg(vpc.type),
    tagged = tagged, settings = settings, pirana = pirana, pirana_observed_path = pirana_obs_path,
    pirana_simulated_path = pirana_sim_path, ObsName = obs_dv, observed_name = observed_name, simulated_name = simulated_name,
    NLME_VPC = NLME_VPC, NLME_VPC_PRED = NLME_VPC_PRED, hasLLOQ = hasLLOQ, noMissingLLOQ = noMissingLLOQ
  )

  if (interactive()) {
    return(invisible(tagged_vpc))
  } else {
    return(TRUE)
  }
}

pirana_run_vpc_results <- function(pirana_obs_path, pirana_sim_path, ObsName, vpc.type, settings_path = NULL, tagged_path = NULL) {

  stopifnot(file.exists(pirana_obs_path))
  stopifnot(file.exists(pirana_sim_path))

  if (grepl("dta", basename(pirana_obs_path))) {
    software <- "NONMEM"
  } else {
    software <- "NLME"
  }

  if (software == "NONMEM") {
    if (requireNamespace("xpose", quietly = TRUE)){
    observed <- xpose::read_nm_tables(file = pirana_obs_path)
    simulated <- xpose::read_nm_tables(file = pirana_sim_path)
    } else {
      stop("The xpose package is required to use VPCResults with NONMEM.")
    }
  } else {
    if(requireNamespace("data.table", quietly = TRUE)) {
      observed <- data.table::fread(file = pirana_obs_path, na.strings = c(".", "NA", ""))
    } else {
      observed <- read.csv(file = pirana_obs_path, na.strings = c(".", "NA", ""))
    }

    if(requireNamespace("data.table", quietly = TRUE)) {
      simulated <- data.table::fread(file = pirana_sim_path, na.strings = c(".", "NA", ""))
    } else {
      simulated <- read.csv(file = pirana_sim_path, na.strings = c(".", "NA", ""))
    }
  }

  if(!is.null(settings_path)){
    stopifnot(file.exists(settings_path))
    settings <- readRDS(settings_path)
  } else {
    settings <- NULL
  }

  if(!is.null(tagged_path)){
    stopifnot(file.exists(tagged_path))
    tagged <- readRDS(tagged_path)
  } else {
    tagged <- NULL
  }

  if (ObsName == "") {
    ObsName <- NULL
  }

  assign("pirana_obs_path", value = pirana_obs_path, envir = vpc_results_env)
  assign("pirana_sim_path", value = pirana_sim_path, envir = vpc_results_env)

  vpcResultsUI(observed, simulated, ObsName, vpc.type, settings = settings, tagged = tagged, pirana = TRUE, pirana_obs_path = pirana_obs_path, pirana_sim_path = pirana_sim_path)


}


#' @rawNamespace import(shiny, except = c(runExample, dataTableOutput, renderDataTable))
#' @import DT
#' @rawNamespace import(dplyr, except = c(between, first, last))
#' @import ggplot2
#' @import tidyvpc
#' @import bslib
#' @rawNamespace import(shinyjs, except = c(runExample))
#' @import sortable
#' @import shinymeta
#' @rawNamespace import(data.table, except = c(between, first, last))
#' @importFrom shinycssloaders withSpinner
#' @importFrom shinyWidgets chooseSliderSkin setSliderColor
#'
.run_shinyVPC <- function(observed = NULL, simulated = NULL, vpc.type = NULL,
                          tagged = NULL, settings = NULL, pirana = FALSE, pirana_observed_path = NULL,
                          pirana_simulated_path = NULL, ObsName = NULL, observed_name = NULL, simulated_name = NULL,
                          NLME_VPC = FALSE, NLME_VPC_PRED = FALSE, hasLLOQ = FALSE, noMissingLLOQ = FALSE) {

  server <- function(input, output, session) {

    # Initialize Settings ----
    if (!is.null(settings)) {
      settings_VPC_Results <- reactiveValues(values = settings)
    } else {
      settings_VPC_Results <- reactiveValues(values = initialize_settings)
    }

    if(hasLLOQ){
      updateCheckboxInput(session = session, inputId = "isCensoring", value = TRUE)
      updateSelectInput(session = session, inputId = "censorType", selected = "variable")
      updateSelectInput(session = session, inputId = "censorvar", selected = "LLOQ")
      shinyjs::disable("isCensoring")
    }

    if(NLME_VPC){
      updateSelectInput(session = session, inputId = "yvar", selected = "DV")
      updateSelectInput(session = session, inputId = "xvar", selected = "IVAR")
    }

    # Observer to update settings upon exit:
    # observe({
    #   update_settings_vpc(settings_VPC_Results$values, input)
    # })


    # Server UI ----
    # Save Plot Name
    output$userPlotName <- renderUI({
      textInput("plotName", label = "Name", value = paste("VPC Plot", input$buttonPlot, sep = " "), width = "100%")
    })
    outputOptions(output, "userPlotName", suspendWhenHidden = FALSE)



    # Validation ----
    # Validate PI low
    piValidation_low <- reactive({
      validate(
        need(input$piUser_low < 1 && input$piUser_low < input$piUser_med, "Error: Lower prediction interval must be between 0-1 and less than the values of 'Median' and 'Upper' inputs.")
      )
    })
    output$piValidation_low <- renderPrint({
      piValidation_low()
    })
    outputOptions(output, "piValidation_low", suspendWhenHidden = FALSE)

    # Validate PI med

    piValidation_med <- reactive({
      validate(
        need(input$piUser_med < 1 && input$piUser_med < input$piUser_hi, "Error: Median prediction interval must be between 0-1, greater than 'Lower' input, and less than 'Upper' input.")
      )
    })
    output$piValidation_med <- renderPrint({
      piValidation_med()
    })
    outputOptions(output, "piValidation_med", suspendWhenHidden = FALSE)


    # Validate PI hi

    piValidation_hi <- reactive({
      validate(
        need(input$piUser_hi < 1 && input$piUser_hi > input$piUser_low, "Error: Upper prediction interval must be between 0-1 and greater than the values of 'Lower'and 'Median' inputs.")
      )
    })
    output$piValidation_hi <- renderPrint({
      piValidation_hi()
    })
    outputOptions(output, "piValidation_hi", suspendWhenHidden = FALSE)


    # Validate CI Input
    ciValidation <- reactive({
      validate(
        need(input$ciUser < 1 && input$ciUser > 0, "Error: Confidence level must be between 0 and 1")
      )
    })

    output$ciValidation <- renderPrint({
      ciValidation()
    })

    outputOptions(output, "ciValidation", suspendWhenHidden = FALSE)


    # Remove Tagged Diagnostics ----
    # Add Confirmation Dialog
    observeEvent(input$removeTagged, {

      showModal(
        modalDialog(
          size = "m",
          title = "Remove Tagged VPC",
          easyClose = TRUE,

          div(
            style = "padding-top: 10px;",
            div(
              style = "display: inline-block;",
              actionButton("confirmRemoveTagged", label = "Confirm"),
            ),
            div(
              style = "display: inline-block;",
              actionButton("cancelRemoveTagged", label = "Cancel"),
            )
          ),

          footer = NULL
        )
      )
    })

    observeEvent(input$confirmRemoveTagged, {
      taggedDiagnostics$values[[input$selectDiagnostic]] <- NULL
      removeModal()
    })

    observeEvent(input$cancelRemoveTagged, {
      removeModal()
    })

    observe({
      if (length(names(taggedDiagnostics$values)) == 0) {
        shinyjs::hide("md_code")
      } else {
        shinyjs::show("md_code")
      }
    })
    # Validate Duplicated Tagged
    dupTaggedValidation <- eventReactive(list(input$plotName, input$open_savePlotModal), {
      validate(
        need(!(input$plotName %in% names(taggedDiagnostics$values)),
             "Warning: Tagged VPC name already exists and will be overwritten")
      )

    }, ignoreNULL = FALSE)

    output$userPlotDup <- renderPrint({
      dupTaggedValidation()
    })

    outputOptions(output, "userPlotDup", suspendWhenHidden = FALSE)


    # Toggle UI Visibility ----
    observe({
      toggleVisibility(selector = ".custom_plot_theme_inputs", condition = !input$isCertaraTheme)
      toggleVisibility(selector = ".custom_text_inputs", condition = !input$isDefaultText)
      toggleVisibility(selector = ".binning_ui_inputs", condition = plotAesthetics()$isBinning)
      toggleVisibility(selector = ".scales_layout", condition = !input$facetQuantile)
    })

    observe({
      if (input$showGridLines) {
        shinyjs::disable("showBinning")
      } else {
        shinyjs::enable("showBinning")
      }

      if (input$showBinning) {
        shinyjs::disable("showGridLines")
      } else {
        shinyjs::enable("showGridLines")
      }

      if (input$isCertaraTheme) {
        shinyjs::enable("showBinning")
      }
    })

    observe({
      if (length(names(taggedDiagnostics$values)) == 0) {
        shinyjs::hide("tidyvpc_code")
      } else {
        shinyjs::show("tidyvpc_code")
      }
    })
    # Categorical - disable SP inputs if stratification selected ----
    # Note: SP inputs by strata will be supported in a future release
    observeEvent(input$isStrat, {
      if (input$isStrat) {
        updateCheckboxInput(session = session, inputId = "isAutoOptimize", value = TRUE)
        shinyjs::disable("isAutoOptimize")
      } else {
        shinyjs::enable("isAutoOptimize")
      }
    })

    observeEvent(input$isAutoOptimize, {
      if (!input$isAutoOptimize) {
        updateCheckboxInput(session = session, inputId = "isStrat", value = FALSE)
        shinyjs::disable("isStrat")
      } else {
        shinyjs::enable("isStrat")
      }
    })



    output$categoricalSPInputs <- renderUI({
      levels <- levels(as.factor(observed[[input$yvar]]))

      ui <- tagList()

      for (i in seq_along(levels)) {
        lvl <- levels[[i]]

        ui[[lvl]] <- tagList(
          column(
            width = 10,
            numericInput(inputId = paste0("catSPprob", lvl), label = paste0("Smoothing Parameter, Category: ", lvl), min = 0.01, max = Inf, value = 3)
          )
        )
      }

      div(
        title = "Please enter a positive value",
        ui
      )
    })

    output$categoricalStyleOptions <- renderUI({
      req(vpc())

      levels <- levels(vpc()$stats$pname)

      ui <- tagList()

      for (i in seq_along(levels)) {
        lvl <- levels[[i]]

        ui[[lvl]] <- # tagList(
          column(
            width = 2,
            htmltools::h5(gsub("prob", "", paste0("Category: ", lvl))),
            selectInput(inputId = paste0("typeLine", lvl), label = "Line Type (Observed)", choices = c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash"), settings[[paste0("vpc.line.type.", lvl)]]),
            colourpicker::colourInput(paste0("colorLine", lvl), label = "Line/Interval Color (Simulated)", value = settings[[paste0("vpc.line.color.", lvl)]])
          )
        # )
      }

      ui
    })



    # VPC Inputs ----
    output$outstratvar <- renderPrint({
      cat(paste0("Stratification Formula: ~", paste(input$stratvar, collapse = " + ")))
    })

    reactiveStrat <- reactiveValues(values = c())

    vpc <- metaReactive2(varname = "vpc", {
      req(input$buttonPlot)

      errors <- c()

      isolate({
        if (input$isStrat && length(reactiveStrat[["values"]]) == 0) {
          errors <- c(errors, "Stratification selected but no stratification variables specified.")
        }

        if (vpc.type == "continuous") {
          if (input$isPred && input$predvar == "") {
            errors <- c(errors, "Prediction correction specified but no prediction variable specified.")
          }

          if (input$isCensoring) {
            if (input$censorType == "variable" && input$censorvar == "") {
              errors <- c(errors, "Censoring specified but no censoring variable selected.")
            }
            if (input$censorType == "value" && !is.numeric(input$userLLOQ)) {
              errors <- c(errors, "Censoring specified but no LLOQ provided.")
            }
          }
        }
      })

      if (length(errors) > 0) {
        showModal(
          modalDialog(
            title = "Error",
            size = "m",
            easyClose = TRUE,

            p(HTML(paste(errors, collapse = "<br><br>")), style = "color: red;"),

            footer = NULL
          )
        )

        return()
      }

      withProgress(message = "Generating VPC", value = 0, {
        isolate({
          userVPC <- get_vpc(observed, simulated, input, vpc.type, reactiveStrat)
        })
      })


      userVPC
    })

    # Try making this an event reactive for the style fields that should only be updated on plot button e.g. CI labels
    plotAesthetics <- eventReactive(input$buttonPlot, {
      if (is.null(input$stratvar)) {
        reactiveStrat$values <- c()
      } else {
        reactiveStrat$values <- unique(append(reactiveStrat$values, input$stratvar))
        ret <- reactiveStrat$values %in% input$stratvar
        reactiveStrat$values <- reactiveStrat$values[ret]
      }
      list(
        linetype = c(input$lineTypeLo, input$lineTypeMed, input$lineTypeHi),
        color = c(input$colorLineHi, input$colorLineMed, input$colorLineLo),
        color.fill = input$colorFill,
        pi.user = c(input$piUser_low, input$piUser_med, input$piUser_hi),
        # custom.theme = input$themeType,
        show.points = input$showPoints,
        # show.boundaries = input$showBoundaries,
        # show.stats = input$showStats,
        legend.position = input$legendPosition,
        facet.scales = input$facetScales,
        xlabel = input$xlabel,
        ylabel = input$ylabel,
        # qlabel = userQuantiles(),
        isStrat = input$isStrat,
        conf.level = input$ciUser,
        isBinning = !input$isBinless,
        isCensoring = input$isCensoring,
        censor.type = input$censorType,
        lloq = input$userLLOQ,
        lloq.var = input$censorvar,
        isPred = input$isPred
      )
    })




    # VPC Plot ----
    vpcPlot <- metaReactive2(varname = "vpcPlot", {
      if (vpc.type == "categorical") {
        req(vpc(), input$colorLineprob0) # We know at a minimun, this inptut should be rendered
      } else {
        req(vpc())
      }

      if (plotAesthetics()$isStrat == TRUE) {
        if (length(reactiveStrat$values) > 1) {
          strata <- paste(reactiveStrat$values, collapse = " + ")
          form <- formula(paste0("~", strata))
        } else if (length(reactiveStrat$values) == 1) {
          strata <- reactiveStrat$values
          form <- formula(paste0("~", strata))
        } else {
          form <- NULL
        }
      } else {
        form <- NULL
      }

      # Plot Categorical ----
      if (vpc.type == "categorical") {
        posProbLines <- grep(pattern = "typeLine", names(input))
        probLines <- c()
        for (i in posProbLines) {
          input_name <- names(input)[[i]]
          probLines <- c(probLines, input[[paste0(input_name)]])
        }

        posProbColors <- grep(pattern = "colorLine", names(input))
        probColors <- c()
        inputnamescolor <- c()
        for (i in posProbColors) {
          input_name <- names(input)[[i]]
          inputnamescolor <- c(inputnamescolor, input_name)
          probColors <- c(probColors, input[[paste0(input_name)]])
        }

        names(probColors) <- inputnamescolor

        probColors <- probColors[order(names(probColors))]

        probColors <- unname(probColors)

        if (plotAesthetics()$isBinning) {
          g <- metaExpr({
            ggplot(..(vpc())$stats, aes(x = xbin))
          })
        } else {
          g <- metaExpr({
            ggplot(..(vpc())$stats, aes(x = x))
          })
        }

        g <- metaExpr({
          ..(g) +
            geom_ribbon(aes(ymin = lo, ymax = hi, fill = pname, col = pname, group = pname), alpha = ..(input$alphaFill), col = NA) +
            geom_line(aes(y = md, col = pname, group = pname)) +
            geom_line(aes(y = y, linetype = pname), size = 1)
        })

        if (input$shapePoint != "none") {
          g <- metaExpr({
            ..(g) +
              geom_point(aes(y = y),
                colour = ..(input$colorPoint),
                size = ..(input$sizePoint),
                shape = ..(change_point_shape(input$shapePoint)),
                alpha = ..(input$alphaPoint)
              )
          })
        }


        if (input$isDefaultText) {
          g <- metaExpr({
            ..(g) +
              ylab(sprintf("Observed/Simulated probabilities and associated %s%% CI", 100 * ..(vpc())$conf.level)) +
              xlab("TIME")
          })
        } else {
          g <- metaExpr({
            ..(g) +
              labs(
                title = ..(input$textTitle),
                subtitle = ..(input$textSubtitle),
                caption = ..(input$textCaption)
              ) +
              ylab(..(input$ylab)) +
              xlab(..(input$xlab))
          })
        }


        g <- metaExpr({
          ..(g) +
            scale_colour_manual(
              name = sprintf("Simulated \nMedian (lines) %s%% CI (areas)", 100 * ..(vpc())$conf.level),
              breaks = levels(..(vpc())$stats$pname),
              values = ..(probColors),
              labels = levels(..(vpc())$stats$pname)
            ) +
            scale_fill_manual(
              name = sprintf("Simulated \nMedian (lines) %s%% CI (areas)", 100 * ..(vpc())$conf.level),
              breaks = levels(..(vpc())$stats$pname),
              values = ..(probColors),
              labels = levels(..(vpc())$stats$pname)
            ) +
            scale_linetype_manual(
              name = "Observed \nMedian (lines)",
              breaks = levels(..(vpc())$stats$pname),
              values = ..(probLines),
              labels = levels(..(vpc())$stats$pname)
            ) +
            guides(
              fill = guide_legend(order = 2),
              colour = guide_legend(order = 2),
              linetype = guide_legend(order = 1)
            )
        })



        if (input$facetQuantile) {
          if (!is.null(form)) {
            g <- metaExpr({
              ..(g) +
                facet_grid(as.formula(paste(paste0(names(..(vpc())$strat), collapse = " + "), "~", "pname", sep = " ")), scales = ..(input$axisScales), as.table = TRUE, labeller = label_both)
            })
          } else {
            g <- metaExpr({
              ..(g) +
                facet_grid(~pname, scales = ..(input$axisScales), labeller = label_both) # as.table = FALSE
            })
          }
        } else {
          if (!is.null(form)) {
            g <- metaExpr({
              ..(g) +
                facet_wrap(names(..(vpc())$strat), scales = ..(input$axisScales), labeller = label_both)
            })
          }
        }


        if (!is.null(g)) {
          if (input$isLogX) {
            g <- metaExpr({
              ..(g) +
                scale_x_log10(
                  breaks = scales::trans_breaks("log10", function(x) 10^x),
                  labels = scales::trans_format("log10", scales::math_format(10^.x))
                )
            })
          }
          if (input$isLogY) {
            g <- metaExpr({
              ..(g) +
                scale_y_log10(
                  breaks = scales::trans_breaks("log10", function(x) 10^x),
                  labels = scales::trans_format("log10", scales::math_format(10^.x))
                )
            })
          }
        }

        if (input$showBoundaries) {
          if (plotAesthetics()$isBinning) {
            if (plotAesthetics()$isStrat) {
              boundaries <- isolate(metaExpr({
                bininfo(..(vpc()))[, .(x = sort(unique(c(xleft, xright)))), by = names(..(vpc())$strat)]
              }))
            } else {
              boundaries <- isolate(metaExpr({
                bininfo(..(vpc()))[, .(x = sort(unique(c(xleft, xright))))]
              }))
            }
            if (input$showBinning) {
              g <- metaExpr({
                ..(g) +
                  geom_vline(data = ..(boundaries), aes(xintercept = x), size = rel(0.5), col = "gray80") +
                  theme(panel.grid = element_blank())
              })
            }
            g <- metaExpr({
              ..(g) +
                geom_rug(data = ..(boundaries), aes(x = x), sides = "t", size = 1)
            })
          }
        }
      } else {
        # Plot Continuous ----
        if (plotAesthetics()$isBinning) {
          g <- metaExpr({
            ggplot(..(vpc())$stats, aes(x = xbin))
          })
        } else {
          g <- metaExpr({
            ggplot(..(vpc())$stats, aes(x = x))
          })
        }

        g <- metaExpr({
          ..(g) +
            geom_ribbon(aes(ymin = lo, ymax = hi, fill = qname, col = qname, group = qname), alpha = ..(input$alphaFill), col = NA) +
            geom_line(aes(y = md, col = qname, group = qname)) +
            geom_line(aes(y = y, linetype = qname), size = 1)
        })


        if(input$shapePoint != "none"){
          if(plotAesthetics()$isPred){
            if(plotAesthetics()$isBinning){
              g <- metaExpr({
                ..(g) +
                  geom_point(
                    data = ..(vpc())$obs[!(blq|alq)], aes(x = x, y = ypc),
                    color = ..(input$colorPoint),
                    size = ..(input$sizePoint),
                    shape = ..(change_point_shape(input$shapePoint)),
                    alpha = ..(input$alphaPoint),
                    show.legend = FALSE
                  )
              })
            } else {
              g <- metaExpr({
                ..(g) +
                  geom_point(
                    data = ..(vpc())$obs[!(blq|alq)], aes(x = x, y = l.ypc),
                    color = ..(input$colorPoint),
                    size = ..(input$sizePoint),
                    shape = ..(change_point_shape(input$shapePoint)),
                    alpha = ..(input$alphaPoint),
                    show.legend = FALSE
                  )
              })
            }
          } else {
            g <- metaExpr({
              ..(g) +
                geom_point(
                  data = ..(vpc())$obs[!(blq|alq)], aes(x = x, y = y),
                  color = ..(input$colorPoint),
                  size = ..(input$sizePoint),
                  shape = ..(change_point_shape(input$shapePoint)),
                  alpha = ..(input$alphaPoint),
                  show.legend = FALSE
                )
            })
          }
        }



        if (plotAesthetics()$isCensoring) {

          if (plotAesthetics()$censor.type == "variable") {
            if(!is.null(reactiveStrat$values)) {
              g <- metaExpr({
                ..(g) +
                  geom_hline(data=unique(data.table::as.data.table(..(vpc())$data)[, .(LLOQ), by = eval(..(reactiveStrat$values))]),
                             aes(yintercept = !!as.symbol(..(input$censorvar))), linetype="dotted", size=1) +
                  geom_text(data=unique(data.table::as.data.table(..(vpc())$data)[, .(LLOQ), by = eval(..(reactiveStrat$values))]),
                            aes(x=10, y=LLOQ, label=paste("LLOQ", LLOQ, sep="="),), vjust=-1)
              })
            } else {
            g <- metaExpr({
              ..(g) +
                geom_hline(
                  #data = ..(vpc())$data,
                  data = data.frame(LLOQ = unique(data.table::as.data.table(..(vpc())$data)[, ..(vpc())$data[[..(paste0(plotAesthetics()$lloq.var))]]])),
                  aes(yintercept = LLOQ), linetype = "dotted", size = 1) +
                geom_text(
                  #data = ..(vpc())$data,
                  data = data.frame(LLOQ = unique(data.table::as.data.table(..(vpc())$data)[, ..(vpc())$data[[..(paste0(plotAesthetics()$lloq.var))]]])),
                  aes(x = 10, y = LLOQ, label = paste("LLOQ", LLOQ, sep = "="), ), vjust = -1)
            })
            }

          } else {
            g <- metaExpr({
              ..(g) +
                geom_hline(aes(yintercept = ..(plotAesthetics()$lloq)), linetype = "dotted", size = 1) +
                geom_text(aes(x = 10, y = ..(plotAesthetics()$lloq), label = paste("LLOQ", ..(plotAesthetics()$lloq), sep = "="), ), vjust = -1)
            })
          }
        }



        g <-
          metaExpr({
            ..(g) +
              scale_colour_manual(
                name = ..(paste0("Simulated Percentiles\nMedian (lines) ", plotAesthetics()$conf.level * 100, "% CI (areas)")),
                breaks = ..(paste0("q", plotAesthetics()$pi.user)),
                values = ..(c(input$colorLineLo, input$colorLineMed, input$colorLineHi)),
                labels = ..(paste0(plotAesthetics()$pi.user * 100, "%"))
              ) +
              scale_fill_manual(
                name = ..(paste0("Simulated Percentiles\nMedian (lines) ", plotAesthetics()$conf.level * 100, "% CI (areas)")),
                breaks = ..(paste0("q", plotAesthetics()$pi.user)),
                values = ..(c(input$colorLineLo, input$colorLineMed, input$colorLineHi)),
                labels = ..(paste0(plotAesthetics()$pi.user * 100, "%"))
              ) +
              scale_linetype_manual(
                name = ..(paste0("Observed Percentiles")),
                breaks = ..(paste0("q", plotAesthetics()$pi.user)),
                values = ..(c(input$lineTypeLo, input$lineTypeMed, input$lineTypeHi)),
                labels = ..(paste0(plotAesthetics()$pi.user * 100, "%"))
              ) +
              guides(
                fill = guide_legend(order = 2),
                colour = guide_legend(order = 2),
                linetype = guide_legend(order = 1)
              )
          })


        if (input$isLogX) {
          g <- metaExpr({
            ..(g) +
              scale_x_log10(
                breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))
              )
          })
        }

        if (input$isLogY) {
          g <- metaExpr({
            ..(g) +
              scale_y_log10(
                breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))
              )
          })
        }



        if (input$facetQuantile) {
          if (!is.null(form)) {
            g <- metaExpr({
              ..(g) +
                facet_grid(as.formula(paste(paste0(names(..(vpc())$strat), collapse = " + "), "~", "qname", sep = " ")), scales = ..(input$axisScales), as.table = TRUE, labeller = label_both)
            })
          } else {
            g <- metaExpr({
              ..(g) +
                facet_grid(~qname, scales = ..(input$axisScales), labeller = label_both) # as.table = FALSE
            })
          }
        } else {
          if (!is.null(form)) {
            g <- metaExpr({
              ..(g) +
                facet_wrap(names(..(vpc())$strat), scales = ..(input$axisScales), labeller = label_both)
            })
          }
        }





        if (input$showBoundaries) {
          if (plotAesthetics()$isBinning) {
            if (plotAesthetics()$isStrat) {
              boundaries <- isolate(metaExpr({
                bininfo(..(vpc()))[, .(x = sort(unique(c(xleft, xright)))), by = names(..(vpc())$strat)]
              }))
            } else {
              boundaries <- isolate(metaExpr({
                bininfo(..(vpc()))[, .(x = sort(unique(c(xleft, xright))))]
              }))
            }
            if (input$showBinning) {
              g <- metaExpr({
                ..(g) +
                  geom_vline(data = ..(boundaries), aes(xintercept = x), size = rel(0.5), col = "gray80")
              })
            }
            g <- metaExpr({
              ..(g) +
                geom_rug(data = ..(boundaries), aes(x = x), sides = "t", size = 1)
            })
          }
        }

        if (input$isDefaultText) {
          g <- metaExpr({
            ..(g) +
              ylab(sprintf("Observed/Simulated percentiles and associated %s%% CI", 100 * ..(vpc())$conf.level)) +
              xlab("TIME")
          })
        } else {
          g <- metaExpr({
            ..(g) +
              labs(
                title = ..(input$textTitle),
                subtitle = ..(input$textSubtitle),
                caption = ..(input$textCaption)
              ) +
              ylab(..(input$ylab)) +
              xlab(..(input$xlab))
          })
        }
      }


      if (input$isCertaraTheme) {
        g <- metaExpr({
          ..(g) +
            theme_certara()
        })
      } else {
        if (input$showGridLines && input$isShowBorder) {
          g <- metaExpr({
            ..(g) +
              theme(
                axis.title.x = element_text(
                  size = ..(input$sizeAxis), # Need UI inputs for axis size,color,family,face
                  color = ..(input$colorAxis),
                  family = ..(font_to_family(input$fontAxis)),
                  face = ..(input$faceAxis)
                ),
                axis.title.y = element_text(
                  size = ..(input$sizeAxis),
                  color = ..(input$colorAxis),
                  family = ..(font_to_family(input$fontAxis)),
                  face = ..(input$faceAxis)
                ),
                legend.title = element_text(
                  size = ..(input$sizeLegend),
                  color = ..(input$colorLegend),
                  family = ..(font_to_family(input$fontLegend)),
                  face = ..(input$faceLegend)
                ),
                plot.title = element_text(
                  size = ..(input$sizeTitle), # Consider making these nested under title
                  color = ..(input$colorTitle), # Can add plot options as argument to initialize app
                  family = ..(font_to_family(input$fontTitle)), # Make plot options a class
                  face = ..(input$faceTitle)
                ),
                plot.subtitle = element_text(
                  size = ..(input$sizeSubtitle),
                  color = ..(input$colorSubtitle),
                  family = ..(font_to_family(input$fontSubtitle)),
                  face = ..(input$faceSubtitle)
                ),
                plot.caption = element_text(
                  size = ..(input$sizeCaption),
                  color = ..(input$colorCaption),
                  family = ..(font_to_family(input$fontCaption)),
                  face = ..(input$faceCaption)
                ),
                panel.background = element_rect(fill = ..(input$colorBackground)),
                axis.line = element_line(colour = "black"),
                panel.border = element_rect(colour = "black", fill = NA, size = 1)
              )
          })
        } else if (!input$showGridLines && input$isShowBorder) {
          g <- metaExpr({
            ..(g) +
              theme(
                axis.title.x = element_text(
                  size = ..(input$sizeAxis), # Need UI inputs for axis size,color,family,face
                  color = ..(input$colorAxis),
                  family = ..(font_to_family(input$fontAxis)),
                  face = ..(input$faceAxis)
                ),
                axis.title.y = element_text(
                  size = ..(input$sizeAxis),
                  color = ..(input$colorAxis),
                  family = ..(font_to_family(input$fontAxis)),
                  face = ..(input$faceAxis)
                ),
                legend.title = element_text(
                  size = ..(input$sizeLegend),
                  color = ..(input$colorLegend),
                  family = ..(font_to_family(input$fontLegend)),
                  face = ..(input$faceLegend)
                ),
                plot.title = element_text(
                  size = ..(input$sizeTitle), # Consider making these nested under title
                  color = ..(input$colorTitle), # Can add plot options as argument to initialize app
                  family = ..(font_to_family(input$fontTitle)), # Make plot options a class
                  face = ..(input$faceTitle)
                ),
                plot.subtitle = element_text(
                  size = ..(input$sizeSubtitle),
                  color = ..(input$colorSubtitle),
                  family = ..(font_to_family(input$fontSubtitle)),
                  face = ..(input$faceSubtitle)
                ),
                plot.caption = element_text(
                  size = ..(input$sizeCaption),
                  color = ..(input$colorCaption),
                  family = ..(font_to_family(input$fontCaption)),
                  face = ..(input$faceCaption)
                ),
                panel.background = element_rect(fill = ..(input$colorBackground)),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.line = element_line(colour = "black"),
                panel.border = element_rect(colour = "black", fill = NA, size = 1)
              )
          })
        } else if (input$showGridLines && !input$isShowBorder) {
          g <- metaExpr({
            ..(g) +
              theme(
                axis.title.x = element_text(
                  size = ..(input$sizeAxis), # Need UI inputs for axis size,color,family,face
                  color = ..(input$colorAxis),
                  family = ..(font_to_family(input$fontAxis)),
                  face = ..(input$faceAxis)
                ),
                axis.title.y = element_text(
                  size = ..(input$sizeAxis),
                  color = ..(input$colorAxis),
                  family = ..(font_to_family(input$fontAxis)),
                  face = ..(input$faceAxis)
                ),
                legend.title = element_text(
                  size = ..(input$sizeLegend),
                  color = ..(input$colorLegend),
                  family = ..(font_to_family(input$fontLegend)),
                  face = ..(input$faceLegend)
                ),
                plot.title = element_text(
                  size = ..(input$sizeTitle), # Consider making these nested under title
                  color = ..(input$colorTitle), # Can add plot options as argument to initialize app
                  family = ..(font_to_family(input$fontTitle)), # Make plot options a class
                  face = ..(input$faceTitle)
                ),
                plot.subtitle = element_text(
                  size = ..(input$sizeSubtitle),
                  color = ..(input$colorSubtitle),
                  family = ..(font_to_family(input$fontSubtitle)),
                  face = ..(input$faceSubtitle)
                ),
                plot.caption = element_text(
                  size = ..(input$sizeCaption),
                  color = ..(input$colorCaption),
                  family = ..(font_to_family(input$fontCaption)),
                  face = ..(input$faceCaption)
                ),
                panel.background = element_rect(fill = ..(input$colorBackground))
              )
          })
        } else {
          g <- metaExpr({
            ..(g) +
              theme(
                axis.title.x = element_text(
                  size = ..(input$sizeAxis), # Need UI inputs for axis size,color,family,face
                  color = ..(input$colorAxis),
                  family = ..(font_to_family(input$fontAxis)),
                  face = ..(input$faceAxis)
                ),
                axis.title.y = element_text(
                  size = ..(input$sizeAxis),
                  color = ..(input$colorAxis),
                  family = ..(font_to_family(input$fontAxis)),
                  face = ..(input$faceAxis)
                ),
                legend.title = element_text(
                  size = ..(input$sizeLegend),
                  color = ..(input$colorLegend),
                  family = ..(font_to_family(input$fontLegend)),
                  face = ..(input$faceLegend)
                ),
                plot.title = element_text(
                  size = ..(input$sizeTitle), # Consider making these nested under title
                  color = ..(input$colorTitle), # Can add plot options as argument to initialize app
                  family = ..(font_to_family(input$fontTitle)), # Make plot options a class
                  face = ..(input$faceTitle)
                ),
                plot.subtitle = element_text(
                  size = ..(input$sizeSubtitle),
                  color = ..(input$colorSubtitle),
                  family = ..(font_to_family(input$fontSubtitle)),
                  face = ..(input$faceSubtitle)
                ),
                plot.caption = element_text(
                  size = ..(input$sizeCaption),
                  color = ..(input$colorCaption),
                  family = ..(font_to_family(input$fontCaption)),
                  face = ..(input$faceCaption)
                ),
                panel.background = element_rect(fill = ..(input$colorBackground)),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank()
              )
          })
        }
      }

      g <- metaExpr({
        ..(g) +
          theme(legend.position = ..(input$legendPosition))
      })



      g
    })



    # VPC Plot Output ----

    output$vpcPlotly <- plotly::renderPlotly({
      req(vpc())
      vpcPlot() %>%
        plotly::ggplotly(tooltip = c("x", "y", "ymin", "ymax", "col", "fill")) %>%
        plotly::layout(showlegend = FALSE)
      # plotly::layout(legend = list(x = 0.75, y = 0.9))
    })


    output$vpcPlot <- renderPlot({
      req(vpc())
      vpcPlot()
    })

    # Tagged Objects and R Code Gen ----
    if (!is.null(tagged)) {
      taggedDiagnostics <- reactiveValues(values = tagged)
    } else {
      taggedDiagnostics <- reactiveValues(values = list())
    }


    # Code Generation ----
    observeEvent(input$savePlot,
      {
        showNotification(
          "Generating Code",
          action = NULL,
          duration = 2,
          closeButton = TRUE,
          id = NULL,
          type = "message"
        )
        code <- expandChain(
          # quote({
          #   library(ggplot2)
          #   library(magrittr)
          #   library(tidyvpc)
          # }),
          vpcPlot()
        )


        code <- formatCode(code) %>%
          remove_rlang() %>%
          add_line_break() %>%
          replace_obs_sim_names(observed_name, simulated_name)


        if (pirana) {
          if (grepl("dta", basename(pirana_observed_path))) {
            software <- "NONMEM"
          } else {
            software <- "NLME"
          }
          if (software == "NONMEM" && !is.null(observed$MDV)) {
            hasMDV <- TRUE
          } else {
            hasMDV <- FALSE
          }
          init <- gen_init_pirana(ObsName, pirana_observed_path, pirana_simulated_path, observed_name, simulated_name, NLME_VPC_PRED, hasLLOQ, noMissingLLOQ, hasMDV, software)
        } else if (NLME_VPC) {
          if (!is.null(ObsName)) {
            init <- gen_init_filter_arrange(ObsName, observed_name, simulated_name, NLME_VPC_PRED, hasLLOQ, noMissingLLOQ)
          } else {
            init <- gen_init_arrange(ObsName, observed_name, simulated_name, NLME_VPC_PRED, hasLLOQ, noMissingLLOQ)
          }
        } else {
          init <- NULL
        }

        code <-
          c(paste0("# ", input$plotName, " ----"), init, code, "\n")

        # savedNames <- c(names(taggedDiagnostics$values), input$plotName)
        taggedDiagnostics$values[[trimws(paste0(input$plotName))]] <- taggedDiagnostics$values[[trimws(paste0(input$plotName))]] %>%
          update_tagged(
            vpc = vpc(),
            obj = vpcPlot(),
            type = "plot",
            code = code
          )
        removeModal()
        showNotification(
          "VPC Saved",
          action = NULL,
          duration = 3,
          closeButton = TRUE,
          id = NULL,
          type = "message"
        )
      },
      suspended = FALSE
    )


    observeEvent(input$selectDiagnostic, {
      shinyAce::updateAceEditor(
        session,
        "tidyvpc_code",
        mode = "r",
        tabSize = 4,
        useSoftTabs = FALSE,
        showInvisibles = FALSE,
        showLineNumbers = TRUE,
        value = paste0(unlist(taggedDiagnostics$values[[paste0(input$selectDiagnostic)]][["code"]]), collapse = "\n")
      )
    })



    # Preview Tagged Diagnostics ----
    output$myTaggedDiagnostics <- renderUI({
      if (length(names(taggedDiagnostics$values)) == 0) {
        selectDiagnostics <- tagList(
          div(
            style = "padding-left: 15px",
            h4("No Tagged Diagnostics")
          )
        )
      } else {
        selectDiagnostics <- tagList(
          fluidRow(style = "align-items: baseline",
            column(
              width = 5, style = "padding-left: 3rem; align-self: baseline;",
              selectInput(inputId = "selectDiagnostic", "Tagged Model Diagnostics", choices = names(taggedDiagnostics$values),
                          width = "auto")
            ),
            column(
              width = 1, style = "padding-top: 0.7rem; align-self: center;",
              actionLink(inputId = "removeTagged", icon = icon("trash"), label = "", style = "font-size: 22px;")
            ),
            column(
              width = 5, style = "align-self: baseline;",
              textInput(inputId = "nameTaggedScript", "R Script Name", value = "script", width = "125%")
            ),
            column(
              width = 1, style = "padding-top: 0.7rem; align-self: center;",
              downloadLink(outputId = "saveTaggedScript", label = list(icon("download")), style = "font-size: 22px; color: rgba(var(--bs-link-color-rgb));")
            )
          )
        )
      }

      selectDiagnostics
    })

    output$myTaggedPlots <- renderPlot({
      req(input$selectDiagnostic)
      taggedDiagnostics$values[[input$selectDiagnostic]][[2]]
    })


    # Save Script of Tagged Diagnostics ----
    output$saveTaggedScript <- downloadHandler(
      filename = function() {
        paste(input$nameTaggedScript, "R", sep = ".")
      },
      content = function(file) {
        # Copy the report file to a temporary directory before processing it
        tagged <- taggedDiagnostics$values

        libs <- c(
          "library(Certara.VPCResults)",
          "library(ggplot2)",
          "library(dplyr)",
          "library(magrittr)",
          "library(tidyvpc)\n"
        )

        libs <- paste0(libs, collapse = "\n")

        # if (pirana) {
        #   init <- gen_init_pirana(ObsName, pirana_observed_path, pirana_simulated_path, observed_name, simulated_name, NLME_VPC_PRED, hasLLOQ, noMissingLLOQ)
        # } else if (NLME_VPC) {
        #   if (!is.null(ObsName)) {
        #     init <- gen_init_filter_arrange(ObsName, observed_name, simulated_name, NLME_VPC_PRED, hasLLOQ, noMissingLLOQ)
        #   } else {
        #     init <- gen_init_arrange(ObsName, observed_name, simulated_name, NLME_VPC_PRED, hasLLOQ, noMissingLLOQ)
        #   }
        # } else {
        #   init <- NULL
        # }
        init <- NULL

        code <- lapply(tagged, function(x) x$code)

        if(pirana){
          if (grepl("dta", basename(pirana_observed_path))) {
            software <- "NONMEM"
          } else {
            software <- "NLME"
          }

          showModal(
            modalDialog(
              title = "File Saved",
              p("File has been saved to your downloads folder and is additionally available in ./pirana_scripts"),
              easyClose = TRUE,
              footer = NULL
            )
          )

          root <- get_dir_from_path(pirana_observed_path, software = software)
          pirana_scripts <- file.path(root, "pirana_scripts")

          if(!dir.exists(pirana_scripts)){
            dir.create(pirana_scripts)
          }
          writeLines(unlist(c(libs, init, code)), con = paste0(pirana_scripts, "/", input$nameTaggedScript,".R"))
        }

        writeLines(unlist(c(libs, init, code)), con = file)
      }
    )


    # Report Generation ----



    # Generate Rmd ----
    output$generateRmd <- downloadHandler(
      filename = function() {
        paste(input$reportName, "Rmd", sep = ".")
      },
      content = function(file) {

        rmd <- create_rmd_raw(
          title = input$reportName, objects = taggedDiagnostics$values[input$rank_list_2], orientation = input$pageLayout,
          marginLeft = input$marginLeft, marginRight = input$marginRight, marginTop = input$marginTop, marginBottom = input$marginBottom)

        if(pirana){
          if (grepl("dta", basename(pirana_observed_path))) {
            software <- "NONMEM"
          } else {
            software <- "NLME"
          }

          showModal(
            modalDialog(
              title = "File Saved",
              p("File has been saved to your downloads folder and is additionally available in ./pirana_scripts"),
              easyClose = TRUE,
              footer = NULL
            )
          )

          root <- get_dir_from_path(pirana_observed_path, software = software)
          pirana_scripts <- file.path(root, "pirana_scripts")

          if(!dir.exists(pirana_scripts)){
            dir.create(pirana_scripts)
          }
          filename <- gsub(":", "", input$reportName)
          writeLines(unlist(c(rmd)), con = paste0(pirana_scripts, "/", filename,".Rmd"))
        }


        writeLines(unlist(c(rmd)), con = file)
      }
    )

    output$selectReport <- renderUI({
      bucketReport <- tagList(
        fluidRow(
          style = "padding-left:25px; padding-right:25px;",
          sortable::bucket_list(
            header = NULL,
            group_name = "bucket_list_group",
            orientation = "horizontal",
            sortable::add_rank_list(
              text = "Tagged",
              labels = names(taggedDiagnostics$values),
              input_id = "rank_list_1"
            ),
            sortable::add_rank_list(
              text = "Report Output",
              labels = NULL,
              input_id = "rank_list_2"
            )
          )
        )
      )

      bucketReport
    })


    # Need function to dynamically generate r markdown doc chunks given n elements in input$ranklist2
    # or
    # edit r mardownk to take in plot list and plot all values in single code chunk



    output$generateReport <- downloadHandler(
      filename = function() {
        paste(input$reportName, input$fileType, sep = ".")
      },
      content = function(file) {
        # Copy the report file to a temporary directory before processing it
        create_rmd(
          title = input$reportName, objects = taggedDiagnostics$values[input$rank_list_2], orientation = input$pageLayout,
          marginLeft = input$marginLeft, marginRight = input$marginRight, marginTop = input$marginTop, marginBottom = input$marginBottom
        ) # if missing directory argument uses wd

        tempReport <- file.path(tempdir(), "report_template.Rmd")
        tempReportWord <- file.path(tempdir(), "report_template.docx")
        wordTemplate <- system.file("extdata", "report_template.docx", package = "Certara.VPCResults", mustWork = TRUE)

        file.copy("report_template.Rmd", tempReport, overwrite = TRUE)
        file.copy(wordTemplate, tempReportWord, overwrite = TRUE)


        # Set up parameters to pass to Rmd document
        params <- list(inputs = taggedDiagnostics$values[input$rank_list_2])

        if (!requireNamespace("rmarkdown", quietly = TRUE)) {
          stop("Please install the rmarkdown package to render the report.", call. = FALSE)
        }

        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in model results app).
        shiny::withProgress(
          message = paste0("Rendering ", input$reportName, ".", input$fileType),
          value = 0,
          {
            shiny::incProgress(3 / 10)
            rmarkdown::render(tempReport,
              output_file = file,
              output_format = report_render(input$fileType),
              params = params,
              envir = new.env(parent = globalenv())
            )
            shiny::incProgress(7 / 10)
            Sys.sleep(0.25)
            shiny::incProgress(10 / 10)
          }
        )
        file.remove("report_template.Rmd")
      }
    )

    if(pirana){
      shinyjs::hide("generateReport")
      shinyjs::show("generateReportPirana")
    }
    output$report_download_buttons <- renderUI({

      if(pirana){
        ui <- fluidRow(
          column(
            width = 4,
            actionLink("generateReportPirana", label = list("Download Report", HTML("&nbsp; &nbsp;"), icon("file-download")), style = "font-size: 18px; font-family:Segoe UI Light, Arial, sans-serif;")
          ),
          column(
            width = 4,
            offset = 3,
            downloadLink("generateRmd", label = list("Download RMarkdown", HTML("&nbsp; &nbsp;"), icon("file-code")), style = "font-size: 18px; font-family:Segoe UI Light, Arial, sans-serif;")
          )
        )
      } else {
        ui <- fluidRow(
          column(
            width = 4,
            downloadLink("generateReport", label = list("Download Report", HTML("&nbsp; &nbsp;"), icon("file-download")), style = "font-size: 18px; font-family:Segoe UI Light, Arial, sans-serif;")
          ),
          column(
            width = 4,
            offset = 3,
            downloadLink("generateRmd", label = list("Download RMarkdown", HTML("&nbsp; &nbsp;"), icon("file-code")), style = "font-size: 18px; font-family:Segoe UI Light, Arial, sans-serif;")
          )
        )
      }

      ui
    })

    observeEvent(input$generateReportPirana,{

      if (!requireNamespace("rmarkdown", quietly = TRUE)) {
        stop("Please install the rmarkdown package to render the report.", call. = FALSE)
      }

      filename <- paste(input$reportName, input$fileType, sep = ".")

      filename <- gsub(":", "", filename)

      create_rmd(title = input$reportName, objects = taggedDiagnostics$values[input$rank_list_2], orientation = input$pageLayout,
                 marginLeft = input$marginLeft, marginRight = input$marginRight, marginTop = input$marginTop, marginBottom = input$marginBottom) #if missing directory argument uses wd

      tempReport <- file.path(tempdir(), "report_template.Rmd")
      tempReportWord <- file.path(tempdir(), "report_template.docx")
      wordTemplate <- system.file("extdata", "report_template.docx", package = "Certara.VPCResults", mustWork = TRUE)

      file.copy("report_template.Rmd", tempReport, overwrite = TRUE)
      file.copy(wordTemplate, tempReportWord, overwrite = TRUE)


      # Set up parameters to pass to Rmd document
      params <- list(inputs = taggedDiagnostics$values[input$rank_list_2])
      if (grepl("dta", basename(pirana_observed_path))) {
        software <- "NONMEM"
      } else {
        software <- "NLME"
      }

      root <- get_dir_from_path(pirana_observed_path, software = software)
      pirana_reports <- file.path(root, "pirana_reports", "shiny")

      if(!dir.exists(pirana_reports)){
        dir.create(pirana_reports, recursive = TRUE)
      }
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in model results app).
      #NOTE Users should set this env variable from Windows if having issues with pandoc
      #Sys.setenv(RSTUDIO_PANDOC = "C:/Program Files/RStudio/bin/pandoc")
      shiny::withProgress(
        message = paste0("Rendering ", input$reportName, ".", input$fileType),
        value = 0,{
          shiny::incProgress(3/10)
          rmarkdown::render(tempReport, output_file = paste0(pirana_reports, "/", filename),
                            output_format = report_render(input$fileType),
                            params = params,
                            envir = new.env(parent = globalenv()))
          shiny::incProgress(7/10)
          Sys.sleep(0.25)
          shiny::incProgress(10/10)
        })
      file.remove("report_template.Rmd")

      showModal(
        modalDialog(
          title = "File Saved",
          p("Report has been saved to ./pirana_reports"),
          easyClose = TRUE,
          footer = NULL
        )
      )

    })

    #-----------------------------------------
    # Modal Behavior
    #-----------------------------------------
    observeEvent(input$open_savePlotModal, {
      showModal(
        modalDialog(
          size = "m",
          easyClose = TRUE,

          fluidRow(
            column(
              width = 12,
              uiOutput("userPlotName")
            )
          ),
          actionButton("savePlot", label = "Tag"),
          textOutput("userPlotDup"),

          footer = NULL
        )
      )
    })


    #----------------------------------------------------------------------------------------------------------------
    # Exit Behavior
    #----------------------------------------------------------------------------------------------------------------


    #   session$onSessionEnded(function() {
    #     # if the button SaveExit is not clicked and
    #     # the button Exit does not exist or is not clicked and
    #     if(!isolate(input$SaveExit) &&
    #        (is.null(isolate(input$Exit)) ||
    #         (!is.null(isolate(input$Exit)) && !isolate(input$Exit))))
    #       if(!is.null(baseModel)) {
    #         # there was an initial model
    #         warning("Shiny session has ended, the initial model is returned",
    #                 call. = FALSE, immediate. = TRUE)
    #         stopApp(baseModel)
    #       } else {
    #         # there was no initial model
    #         warning("Shiny session has ended, the resulted model is returned",
    #                 call. = FALSE, immediate. = TRUE)
    #         stopApp(isolate({model()}))
    #       }
    #   })
    # }

    observeEvent(input$exitShiny, {
      showModal(
        modalDialog(
          size = "m",
          title = "Exit VPC Results",
          easyClose = TRUE,

          fluidRow(
            div(style = "padding-top: 10px;"),
            column(
              width = 4,
              checkboxInput(inputId = "saveTaggedRds", label = "Save Tagged", value = TRUE)
            ),
            column(
              width = 4,
              checkboxInput(inputId = "saveSettingsRds", label = "Save Settings", value = TRUE)
            )
          ),
          div(
            style = "padding-top: 10px;",
            div(
              style = "display: inline-block;",
              actionButton("exitConfirm", "Exit")
            ),
            div(
              style = "display: inline-block;",
              actionButton("exitCancel", "Cancel")
            )
          ),

          footer = NULL
        )
      )
    })

    observeEvent(input$exitCancel, {
      removeModal(session)
    })

    observeEvent(input$exitConfirm, {
      # from the code in model results app).
      shiny::withProgress(
        message = paste0("Exiting VPC Results"),
        value = 0,
        {

          if(pirana){
            pirana_obs_path <- get("pirana_obs_path", envir = vpc_results_env)

            if (grepl("dta", basename(pirana_observed_path))) {
              software <- "NONMEM"
            } else {
              software <- "NLME"
            }
            root <- get_dir_from_path(pirana_observed_path, software = software)
            dir_out <- file.path(root, "pirana_shiny")
            runfolder <- paste0(get_dir_from_path(pirana_obs_path, "runfolder", software), "_")
            if(!dir.exists(dir_out)){
              dir.create(dir_out)
            }
          } else {
            dir_out <- "."
            runfolder <- NULL
          }

          if (input$saveSettingsRds) {
            settings_out <- update_settings_vpc(settings_VPC_Results$values, input)

            if(vpc.type == "categorical"){
              settings_out <- update_settings_vpc_cat(settings_out, input)
            } else {
              settings_out <- update_settings_vpc_cont(settings_out, input)
            }

            saveRDS(settings_out, file = paste0(dir_out, "/settings.Rds"))
            shiny::incProgress(3 / 10, message = "Settings saved")
            Sys.sleep(1)
          }



          if (input$saveTaggedRds) {
            shiny::incProgress(7 / 10, message = "Saving tagged objects")
            saveRDS(isolate({
              taggedDiagnostics$values
            }), file = paste0(dir_out, "/", runfolder, "tagged.Rds"))
            shiny::incProgress(9 / 10, message = "Tagged objects saved")
          }
        }
      )

      removeModal()
      message("Shiny session has ended")

      # we can implement a try for above code, if FALSE, don't stop app.

      session$sendCustomMessage(type = "shinymaterialJS", js$closewindow())
    })

    session$onSessionEnded(function() {
      stopApp(isolate({
        taggedDiagnostics$values
      }))
    })
  }

  ui <- generate_VPC_ui(settings = settings, observed = observed, simulated = simulated, vpc.type = vpc.type)

  runApp(
    shinyApp(ui = ui, server = server),
    launch.browser = TRUE
  )
}
