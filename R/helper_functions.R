# helper functions

check_data_ObsName <- function(observed, simulated, ObsName) {
  stopifnot("ObsName" %in% colnames(observed))

  stopifnot("OBSNAME" %in% colnames(simulated))

  observed_ObsName <- unique(observed$ObsName)

  if(!(ObsName %in% observed_ObsName)){
    stop(paste0("The specified ObsName '", ObsName, "' does not exist in the observed data"))
  }

  simulated_ObsName <- unique(simulated$OBSNAME)

  if(!(ObsName %in% simulated_ObsName)){
    stop(paste0("The specified ObsName '", ObsName, "' does not exist in the simulated data"))
  }

}



update_tagged <- function(object, vpc, obj, type, code) {
  object$vpc <- vpc
  object$obj <- obj
  object$type <- type
  object$code <- code

  return(object)
}

update_settings_vpc <- function(settings, input) {
  settings$vpc.axis.scale <- input$axisScales
  settings$vpc.facet.category <- input$facetQuantile
  settings$vpc.facet.quantile <- input$facetQuantile
  settings$vpc.legend.position <- input$legendPosition
  settings$vpc.line.type.lo <- ifelse(is.null(input$lineTypeLo), "dashed", input$lineTypeLo)
  settings$vpc.line.type.med <- ifelse(is.null(input$lineTypeMed), "solid", input$lineTypeMed)
  settings$vpc.line.type.hi <- ifelse(is.null(input$lineTypeHi), "dashed", input$lineTypeHi)
  settings$vpc.line.color.lo <- ifelse(is.null(input$colorLineLo), "#D63636", input$colorLineLo)
  settings$vpc.line.color.med <- ifelse(is.null(input$colorLineMed), "#3648D6", input$colorLineMed)
  settings$vpc.line.color.hi <- ifelse(is.null(input$colorLineHi), "#D63636", input$colorLineHi)
  settings$vpc.line.type.prob0 <- ifelse(is.null(input$typeLineprob0), "dashed", input$typeLineprob0)
  settings$vpc.line.type.prob1 <- ifelse(is.null(input$typeLineprob1), "solid", input$typeLineprob1)
  settings$vpc.line.type.prob2 <- ifelse(is.null(input$typeLineprob2), "dashed", input$typeLineprob2)
  settings$vpc.line.color.prob0 <- ifelse(is.null(input$colorLineprob0), "#6DDA00", input$colorLineprob0)
  settings$vpc.line.color.prob1 <- ifelse(is.null(input$colorLineprob1), "#3648D6", input$colorLineprob1)
  settings$vpc.line.color.prob2 <- ifelse(is.null(input$colorLineprob2), "#D63636", input$colorLineprob2)
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


gen_init_pirana <- function(ObsName, pirana_observed_path, pirana_simulated_path, observed_name, simulated_name,
                            NLME_VPC_PRED = FALSE, hasLLOQ = FALSE, noMissingLLOQ = FALSE, hasMDV = FALSE, software = "NLME") {
  if (software == "NONMEM") {
    read_obs <- paste0(observed_name, " <- xpose::read_nm_tables(file = '", pirana_observed_path, "')\n")
    read_sim <- paste0(simulated_name, " <- xpose::read_nm_tables(file = '", pirana_simulated_path, "')\n")
    if (hasMDV) {
      filter_obs <- paste0(observed_name, " <- ", observed_name, " %>%\n\tfilter(MDV == 0)\n")
      filter_sim <- paste0(simulated_name, " <- ", simulated_name, " %>%\n\tfilter(MDV == 0)\n")
    } else {
      filter_obs <- NULL
      filter_sim <- NULL
    }
  } else {
  read_obs <- paste0(observed_name, " <- data.table::fread(file = '", pirana_observed_path, "')\n")
  filter_obs <- paste0(
    observed_name, " <- ", observed_name, " %>%\n\tfilter(ObsName == '", ObsName, "') %>%\n",
    "\tarrange(ID1, ID2, ID3, ID4, ID5, IVAR)\n"
  )

  read_sim <- paste0(simulated_name, " <- data.table::fread(file = '", pirana_simulated_path, "')\n")
  filter_sim <- paste0(
    simulated_name, " <- ", simulated_name, " %>%\n\tfilter(OBSNAME == '", ObsName, "') %>%\n",
    "\tarrange(REPLICATE, ID1, ID2, ID3, ID4, ID5, IVAR)\n"
  )
  }

  if (NLME_VPC_PRED) {
    add_pred <- paste0(
      "pred_col <- ", simulated_name, " %>%\n", "\tfilter(REPLICATE == 0) %>%\n",
      "\tpull(PRED)\n\n", observed_name, "$PRED <- pred_col\n"
    )
  } else {
    add_pred <- NULL
  }

  if(hasLLOQ){
    if(noMissingLLOQ){
      get_lloq <- NULL
      add_lloq <-  paste0(observed_name, " <- ", observed_name, " %>%\n\tmutate(DV = as.numeric(ifelse(DV == 'BLOQ', 0, DV)))")
    } else {
    get_lloq <- paste0("colLLOQ <- unique(na.omit(", observed_name, "$LLOQ))\n")
    add_lloq <-  paste0(observed_name, " <- ", observed_name, " %>%\n\tmutate(DV = as.numeric(ifelse(DV == 'BLOQ', 0, DV))) %>%\n\tmutate(LLOQ = colLLOQ)")
    }
  } else {
    get_lloq <- NULL
    add_lloq <- NULL
  }


  code <- c(read_obs, filter_obs, read_sim, filter_sim, add_pred, get_lloq, add_lloq)

  return(code)
}

gen_init_filter_arrange <- function(ObsName, observed_name, simulated_name, NLME_VPC_PRED, hasLLOQ, noMissingLLOQ) {
  filter_obs <- paste0(
    observed_name, " <- ", observed_name, " %>%\n\tfilter(ObsName == '", ObsName, "') %>%\n",
    "\tarrange(ID1, ID2, ID3, ID4, ID5, IVAR)\n"
  )

  filter_sim <- paste0(
    simulated_name, " <- ", simulated_name, " %>%\n\tfilter(OBSNAME == '", ObsName, "') %>%\n",
    "\tarrange(REPLICATE, ID1, ID2, ID3, ID4, ID5, IVAR)\n"
  )

  if (NLME_VPC_PRED) {
    add_pred <- paste0(
      "pred_col <- ", simulated_name, " %>%\n", "\tfilter(REPLICATE == 0) %>%\n",
      "\tpull(PRED)\n\n", observed_name, "$PRED <- pred_col\n"
    )
  } else {
    add_pred <- NULL
  }

  if(hasLLOQ){
    if(noMissingLLOQ){
      get_lloq <- NULL
      add_lloq <-  paste0(observed_name, " <- ", observed_name, " %>%\n\tmutate(DV = as.numeric(ifelse(DV == 'BLOQ', 0, DV)))")
    } else {
      get_lloq <- paste0("colLLOQ <- unique(na.omit(", observed_name, "$LLOQ))\n")
      add_lloq <-  paste0(observed_name, " <- ", observed_name, " %>%\n\tmutate(DV = as.numeric(ifelse(DV == 'BLOQ', 0, DV))) %>%\n\tmutate(LLOQ = colLLOQ)")
    }
  } else {
    get_lloq <- NULL
    add_lloq <- NULL
  }

  code <- c(filter_obs, filter_sim, add_pred, get_lloq, add_lloq)

  return(code)
}


gen_init_arrange <- function(ObsName, observed_name, simulated_name, NLME_VPC_PRED, hasLLOQ, noMissingLLOQ) {
  arrange_obs <- paste0(
    observed_name, " <- ", observed_name, " %>%\n",
    "\tarrange(ID1, ID2, ID3, ID4, ID5, IVAR)\n"
  )

  arrange_sim <- paste0(
    simulated_name, " <- ", simulated_name, " %>%\n",
    "\tarrange(REPLICATE, ID1, ID2, ID3, ID4, ID5, IVAR)\n"
  )

  if (NLME_VPC_PRED) {
    add_pred <- paste0(
      "pred_col <- ", simulated_name, " %>%\n", "\tfilter(REPLICATE == 0) %>%\n",
      "\tpull(PRED)\n\n", observed_name, "$PRED <- pred_col\n"
    )
  } else {
    add_pred <- NULL
  }

  if(hasLLOQ){
    if(noMissingLLOQ){
      get_lloq <- NULL
      add_lloq <-  paste0(observed_name, " <- ", observed_name, " %>%\n\tmutate(DV = as.numeric(ifelse(DV == 'BLOQ', 0, DV)))")
    } else {
      get_lloq <- paste0("colLLOQ <- unique(na.omit(", observed_name, "$LLOQ))\n")
      add_lloq <-  paste0(observed_name, " <- ", observed_name, " %>%\n\tmutate(DV = as.numeric(ifelse(DV == 'BLOQ', 0, DV))) %>%\n\tmutate(LLOQ = colLLOQ)")
    }
  } else {
    get_lloq <- NULL
    add_lloq <- NULL
  }

  code <- c(arrange_obs, arrange_sim, add_pred, get_lloq, add_lloq)

  return(code)
}


check_pred_sim <- function(simulated) {
  if (is.null(simulated$REPLICATE) || !is.numeric(simulated$PRED)) {
    ret <- list(
      hasPred = FALSE,
      data = NULL
    )
  } else {
    obs_pred <- simulated %>%
      dplyr::filter(REPLICATE == 0) %>%
      dplyr::select(PRED)

    if (nrow(obs_pred) == 0) {
      ret <- list(
        hasPred = FALSE,
        data = NULL
      )
    } else {
      ret <- list(
        hasPred = TRUE,
        data = obs_pred
      )
    }
  }

  return(ret)
}

get_dir_from_path <- function(path, type = c("rootdir", "runfolder"), software = "NLME"){

  type <- match.arg(type)

  if(type == "rootdir"){
  pos <- strsplit(path, split = "/")[[1]]

  if (software == "NLME") {
  dir_pos <- pos[1:(length(pos) - 2)]
  } else {
    dir_pos <- pos[1:(length(pos) - 3)]
  }

  ret <- paste0(dir_pos, collapse = "/")
  } else {
    pos <- strsplit(path, split = "/")[[1]]

    if (software == "NLME") {
    dir_pos <- pos[1:(length(pos) - 1)]
    } else {
      dir_pos <- pos[1:(length(pos) - 2)]
    }

    ret <- dir_pos[[length(dir_pos)]]
  }

  return(ret)

}

# Code Regex ----
# --------------------------------------------------------------------------------
#   !!rlang::sym             '!!rlang::sym'
# --------------------------------------------------------------------------------
#   \(                       '('
# --------------------------------------------------------------------------------
#   \"                       '"'
# --------------------------------------------------------------------------------
#   (                        group and capture to \1:
# --------------------------------------------------------------------------------
#     [\w\W]*?                 any character of: word characters (a-z,
#                              A-Z, 0-9, _), non-word characters (all
#                              but a-z, A-Z, 0-9, _) (0 or more times
#                              (matching the least amount possible))
# --------------------------------------------------------------------------------
#   )                        end of \1
# --------------------------------------------------------------------------------
#   "                        '"'
# --------------------------------------------------------------------------------
#   \)                       ')'


# remove_rlang <- function(code){
#   gsub("!!rlang::sym\\(\"(\\S+)\"\\)", "\\1", code)
#   } #alternative approach
remove_rlang <- function(code) {
  code <- gsub("!!rlang::sym\\(\"([\\w\\W]*?)\"\\)", "\\1", code, perl = TRUE)
}


add_line_break <- function(code) {
  code <- gsub("vpc <-", "\nvpc <-", code)
  code <- gsub("vpcPlot", "\nvpcPlot", code)
  return(code)
}

add_gg_line_break <- function(code) {
  code <- gsub("as.symbol\\(\"xpobj\"\\)", "xpobj", code)
  code <- gsub("\\+", "+ \n", code)
  code <- c(code, "\n")
  return(code)
}

replace_obs_sim_names <- function(code, observed_name, simulated_name) {
  code <- gsub("\\(observed,", paste0("(", observed_name, ","), code)
  code <- gsub("\\(simulated,", paste0("(", simulated_name, ","), code)
  return(code)
}
