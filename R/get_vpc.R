

get_vpc <- function(observed, simulated, input, vpc.type, reactiveStrat) {
  if (!input$isAutoOptimize) {
    posProbSP <- grep(pattern = "catSPprob", names(input))
    probSP <- list()
    for (i in posProbSP) {
      input_name <- names(input)[[i]]
      lvl <- gsub("catSP", "", input_name)
      probSP[[lvl]] <- input[[paste0(input_name)]]
    }
  }

  if (input$isStrat == TRUE) {
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

  vpcUser <- metaExpr({
    observed(observed, x = !!rlang::sym(..(input$xvar)), y = !!rlang::sym(..(input$yvar)))
  })

  if (input$isDVDifferent) {
    vpcUser <- metaExpr({
      ..(vpcUser) %>%
        simulated(simulated, y = !!rlang::sym(..(input$yvar2)))
    })
  } else {
    vpcUser <- tryCatch(
      {
        metaExpr({
          ..(vpcUser) %>%
            simulated(simulated, y = !!rlang::sym(..(input$yvar)))
        })
      },
      error = function(e) {
        stop(paste0(input$yvar, " not found in simulated data. Please check the 'Specify simulated y-var' checkbox
                   to specify the simulated y-variable name."))
      }
    )
  }

  if (vpc.type == "continuous") {
    if (input$isCensoring) {
      if (input$censorType == "value") {
        vpcUser <- metaExpr({
          ..(vpcUser) %>%
            censoring(blq = !!rlang::sym(..(input$yvar)) < ..(input$userLLOQ), lloq = ..(input$userLLOQ))
        })
      } else {
        vpcUser <- metaExpr({
          ..(vpcUser) %>%
            censoring(blq = !!rlang::sym(..(input$yvar)) < !!rlang::sym(..(input$censorvar)), lloq = !!rlang::sym(..(input$censorvar)))
        })
      }
    }
  }

  if (!is.null(form)) {
    vpcUser <- metaExpr({
      ..(vpcUser) %>%
        stratify(..(form))
    })
  }


  if (vpc.type == "categorical") {
    if (input$isBinless == FALSE) {
      if (input$typeBinning == "x-variable") {
        vpcUser <- metaExpr({
          ..(vpcUser) %>%
            binning(bin = !!rlang::sym(..(input$xvar)))
        })
      } else if (input$typeBinning == "centers") {
        centers <- as.numeric(unlist(strsplit(input$centers, split = ",")))
        vpcUser <- metaExpr({
          ..(vpcUser) %>%
            binning(bin = "centers", centers = ..(centers))
        })
      } else if (input$typeBinning == "breaks") {
        breaks <- as.numeric(unlist(strsplit(input$breaks, split = ",")))
        vpcUser <- metaExpr({
          ..(vpcUser) %>%
            binning(bin = "breaks", breaks = ..(breaks))
        })
      } else if (input$typeBinning == "box") {
        vpcUser <- metaExpr({
          ..(vpcUser) %>%
            binning(bin = "box", iqr_mult = ..(input$iqr_mult))
        })
      } else if (input$typeBinning == "headtails") {
        vpcUser <- metaExpr({
          ..(vpcUser) %>%
            binning(bin = "headtails", thr = ..(input$thr))
        })
      } else {
        vpcUser <- metaExpr({
          ..(vpcUser) %>%
            binning(bin = ..(input$typeBinning), nbins = ..(input$nbins))
        })
      }
    } else {
      if (input$isAutoOptimize) {
        vpcUser <- metaExpr({
          ..(vpcUser) %>%
            binless(optimize = TRUE, interval = ..(c(input$smoothingIntervalLo, input$smoothingIntervalHi)))
        })
      } else {
        vpcUser <- metaExpr({
          ..(vpcUser) %>%
            binless(optimize = FALSE, sp = ..(probSP))
        })
      }
    }

    incProgress(amount = 0.5, message = "Computing VPC Stats")

    vpcUser <- metaExpr({
      ..(vpcUser) %>%
        vpcstats(
          conf.level = ..(input$ciUser),
          quantile.type = ..(as.numeric(gsub("Type ", "", input$quantileType))),
          vpc.type = "categorical"
        )
    })

    # Continuous vpc ----
  } else {
    # binning
    if (input$isBinless == FALSE) {
      if (input$typeBinning == "x-variable") {
        vpcUser <- metaExpr({
          ..(vpcUser) %>%
            binning(bin = !!rlang::sym(..(input$xvar)))
        })
      } else if (input$typeBinning == "centers") {
        centers <- as.numeric(unlist(strsplit(input$centers, split = ",")))
        vpcUser <- metaExpr({
          ..(vpcUser) %>%
            binning(bin = "centers", centers = ..(centers))
        })
      } else if (input$typeBinning == "breaks") {
        breaks <- as.numeric(unlist(strsplit(input$breaks, split = ",")))
        vpcUser <- metaExpr({
          ..(vpcUser) %>%
            binning(bin = "breaks", breaks = ..(breaks))
        })
      } else if (input$typeBinning == "box") {
        vpcUser <- metaExpr({
          ..(vpcUser) %>%
            binning(bin = "box", iqr_mult = ..(input$iqr_mult))
        })
      } else if (input$typeBinning == "headtails") {
        vpcUser <- metaExpr({
          ..(vpcUser) %>%
            binning(bin = "headtails", thr = ..(input$thr))
        })
      } else {
        vpcUser <- metaExpr({
          ..(vpcUser) %>%
            binning(bin = ..(input$typeBinning), nbins = ..(input$nbins))
        })
      }

      if (input$isPred) {
        if (input$log_dv) {
          vpcUser <- metaExpr({
            ..(vpcUser) %>%
              predcorrect(pred = !!rlang::sym(..(input$predvar)), log = TRUE)
          })
        } else {
          vpcUser <- metaExpr({
            ..(vpcUser) %>%
              predcorrect(pred = !!rlang::sym(..(input$predvar)))
          })
        }
      }
    } else {
      # binless
      if (input$isPred) {
        if (input$log_dv) {
          vpcUser <- metaExpr({
            ..(vpcUser) %>%
              predcorrect(pred = !!rlang::sym(..(input$predvar)), log = TRUE)
          })
        } else {
          vpcUser <- metaExpr({
            ..(vpcUser) %>%
              predcorrect(pred = !!rlang::sym(..(input$predvar)))
          })
        }

        if (input$isAutoOptimize) {
          vpcUser <- metaExpr({
            ..(vpcUser) %>%
              binless()
          })
        } else {
          vpcUser <- metaExpr({
            ..(vpcUser) %>%
              binless(optimize = FALSE, lambda = ..(c(input$lambdaLo, input$lambdaMed, input$lambdaHi)), span = ..(input$span))
          })
        }
      } else {
        if (input$isAutoOptimize) {
          vpcUser <- metaExpr({
            ..(vpcUser) %>%
              binless(optimize = TRUE, interval = ..(c(input$smoothingIntervalLo, input$smoothingIntervalHi)))
          })
        } else {
          vpcUser <- metaExpr({
            ..(vpcUser) %>%
              binless(optimize = FALSE, lambda = ..(c(input$lambdaLo, input$lambdaMed, input$lambdaHi)))
          })
        }
      }
    }
    incProgress(amount = 0.5, message = "Computing VPC Stats")

    vpcUser <- metaExpr({
      ..(vpcUser) %>%
        vpcstats(qpred = ..(c(input$piUser_low, input$piUser_med, input$piUser_hi)), conf.level = ..(input$ciUser), quantile.type = ..(as.numeric(gsub("Type ", "", input$quantileType))))
    })
  }

  incProgress(amount = 0.4, message = "VPC Finished")


  vpcUser
}
