# app_styling

styleColorInputsShape <-
  paste0(paste0(sapply(c(0:10), function(x)
    paste0("#colorLineprob", x)), collapse = ", "),
    "{
      border-radius: 25px;
    }",
    collapse = " ")

styleCSS <-
  ".certara-page {
    display: flex;
    flex-direction: column;
    min-height: 100vh;
  }

  body, .container-fluid {
    background-color: #616161;
    margin: 0;
    padding: 0;
  }

  .certara-header {
    display: flex;
    justify-content: space-between;
    align-items: center;
    padding: 0px 20px;
    background-color: #d3d3d3;
    border-bottom: 2px solid #CC0000;
  }

  .logo-title {
    display: flex;
    align-items: center;
  }

  .logo-title img {
    margin-right: 10px;
  }

  .certara-footer {
    display: flex;
    justify-content: space-between;
    align-items: center;
    left: 0;
    bottom: 0;
    width: 100%;
    height: 35px;
    background-color: #d3d3d3;
    border-top: 1px solid #CC0000;
    color: #000;
    text-align: left;
    z-index: 12;
  }

  h4 {
    color:black;
    font-size: 22px;
    font-family: Segoe UI Light, Arial, sans-serif;
  }

  h5 {
    color:black;
    font-size: 16px;
    font-family: Segoe UI Light, Arial, sans-serif;
  }

  h6 {
    color: #9e9e9e;
    font-size: 0.9rem;
    line-height: 9%;
  }

  .multi-input-with-checkbox {
    display: flex;
    align-items: baseline;
  }

  .multi-input-with-checkbox .col-sm-1,
  .multi-input-with-checkbox .col-sm-2,
  .multi-input-with-checkbox .col-sm-3 {
    align-self: flex-start;
  }

  .multi-input-with-checkbox .col-checkbox {
    align-self: flex-end;
    margin-bottom: 5px;
    padding-top: 2rem;
  }

  .style-tab {
    padding-left: 25px;
    padding-right: 25px;
    display: flex;
  }

  .style-column {
    padding-left: 5px;
    padding-right: 5px;
  }

  .style-column-cat {
    padding-left: 0px;
    padding-right: 0px;
  }

  .display-tab {
    padding-left: 50px;
    padding-right: 50px;
  }

  .shiny-output-error-validation {
    padding-top: 10px;
    color: #FF0000;
  }

  .shiny-input-container label {
    color: #9e9e9e;
    font-size: 0.7rem;
    line-height: 9%;
    margin: 0.366667rem 0 -0.54rem 0;
  }

  .shiny-input-container .control-label {
    line-height: initial;
  }

  .shiny-input-container .checkbox label {
    color: initial;
    font-size: initial;
    line-height: initial;
    margin: initial;
  }

  .save_btn,
  .shiny-download-link,
  #generateReportPirana {
    color: black;
    text-decoration: none;
    cursor: pointer;
  }

  .save_btn:hover
  .shiny-download-link:hover,
  #generateReportPirana {
    color: black;
    text-decoration: none;
  }

  .btn {
    color: #fff;
    background-color: #1d7eba;
    text-decoration: none;
    text-align: center;
    letter-spacing: .5px;
    -webkit-transition: background-color .2s ease-out;
    transition: background-color .2s ease-out;
    cursor: pointer;
  }

  .btn:hover {
    background-color: #008CBA;
    color: #fff;
  }

  #cancelRemoveTagged, #exitCancel {
    background-color: grey;
  }

  #categoricalStyleOptions {
    display: flex;
    flex-wrap: wrap;
    justify-content: left;
    gap: 10px;
  }

  .colourpicker-input-container {
      background-image: none;
      height: 2rem;
      margin-top: 0px;
  }

  .layout-tab {
    display: flex; padding-left: 50px; padding-right: 50px;
    gap: 0px;
  }

  .nav-item {
    flex: 1;
    text-align: center;
  }

  /* width */
    ::-webkit-scrollbar {
      width: 5px;
    }

  /* Track */
    ::-webkit-scrollbar-track {
      background: #f1f1f1;
    }

  /* Handle */
    ::-webkit-scrollbar-thumb {
      background: #888;
    }

  /* Handle on hover */
    ::-webkit-scrollbar-thumb:hover {
      background: #555;
    }

  /* Horizontal scolling for input dropdowns */
    ::-webkit-scrollbar-thumb {
      width: 3px;
      background: #888;
      border: 3px solid transparent;
      border-radius: 9px;
      background-clip: content-box;
    }

  #open_savePlotModal {
    position: fixed;
    bottom: 60px;
    right: 25px;
    height: 60px;
    width: 60px;
    border-radius: 50%;
    display: flex;
    align-items: center;
    justify-content: center;
  }

  .fa-tag {
    font-size: 40px;
    padding-top: 3px;
  }

  .bslib-sidebar-layout .sidebar {
    background-color: #fff;
  }

  .vpc-cols,
  .binning_ui_inputs,
  .custom_plot_theme_inputs {
    align-self: flex-start !important;
  }

  .bslib-full-screen-enter {
    bottom: var(--bslib-full-screen-enter-bottom);
    top: var(--bslib-full-screen-enter-top, 0.2rem);
  }
"

certara_header <- function(header_title) {
  div(class = "certara-header",
      div(class = "logo-title",
          tags$a(
            href = "https://www.certara.com",
            target = "_blank",
            class = "brand-logo",
            tags$img(src = "https://cdn.shortpixel.ai/spai/w_133+q_lossless+ret_img+to_webp/https://www.certara.com/app/uploads/2023/05/certara-logo-2023.png")
          ),
          h4(class = "header_title", header_title, style = "margin-top: 20px; font-family: Segoe UI !important")
      ),
      shiny::actionLink(inputId = "exitShiny",
                        label = "Save & Exit",
                        icon = icon("save"),
                        class = "save_btn"
      )
  )
}

certara_footer <- function() {
  div(class = "certara-footer",
      tags$p(style = 'margin: 0; padding-right: 5px; font-size:small',
             HTML("&nbsp;&nbsp;"),
             tags$a(href = 'https://www.certara.com/', target = '_blank', 'Home'),
             HTML("&nbsp;&nbsp;"),
             tags$a(href = 'https://certara.github.io/R-RsNLME-model-builder/index.html', target = '_blank', 'Help'),
             HTML("&nbsp;&nbsp;"),
             tags$a(href = 'https://certara.service-now.com/csm', target = '_blank', 'Support'),
             HTML("&nbsp;&nbsp;"),
             tags$a(href = 'https://www.certara.com/legal/privacy-center/', target = '_blank', 'Privacy Policy')
      ),
      tags$p(style = 'margin: 0; padding-right: 5px; font-size:small; text-align: right;',
             HTML("&#169; 2011-2024 Certara USA, Inc., All rights reserved. Version: 3.0.1")
      )
  )
}

hideTab <- function(inputId, target,
                    session = getDefaultReactiveDomain()) {
  force(target)
  inputId <- session$ns(inputId)

  callback <- function() {
    session$sendChangeTabVisibility(
      inputId = inputId,
      target = target,
      type = "hide"
    )
  }
  session$onFlush(callback, once = TRUE)
}

material_collapsible_item <- function(label, ..., icon = NULL, active = FALSE) {
  tags$li(
    tags$div(
      class = paste("collapsible-header", if (active) "active"),
      if (!is.null(icon)) {
        tags$i(class = "material-icons", icon)
      },
      label
    ),
    tags$div(
      class = "collapsible-body",
      tags$span(
        ...
      )
    )
  )
}

material_collapsible <- function(..., depth = NULL, color = NULL, type = NULL, active = TRUE) {
  tags$ul(
    class = paste(
      "collapsible",
      if (active) "active",
      if (!is.null(type)) type,
      if (!is.null(depth)) paste0("z-depth-", depth),
      if (!is.null(color)) color
    ),
    ...
  )
}


jsFunctions <- "shinyjs.closewindow = function() { window.close(); }"

#    $('li.tab a[href$=\"#tab2\"]').trigger('hide');
#    $('li.tab a[href$=\"#tab2\"]').trigger('hidden');


material_collapsible_item <- function(label, ..., icon = NULL, active = FALSE) {
  tags$li(
    tags$div(
      class = paste("collapsible-header", if (active) "active"),
      if (!is.null(icon)) {
        tags$i(class = "material-icons", icon)
      },
      label
    ),
    tags$div(
      class = "collapsible-body",
      tags$span(
        ...
      )
    )
  )
}

create_id <- function() {
  paste(format(as.hexmode(sample(256, 8, replace = TRUE) -
                            1), width = 2), collapse = "")
}

material_tabs <- function(tabs, color = NULL, icon = NULL) {
  material_tabs <- shiny::tagList()

  this_id <- paste0("tabs-id-", create_id())

  for (i in 1:length(tabs)) {
    material_tabs[[i]] <-
      shiny::tags$li(
        class = "tab",
        # shiny::tags$div(
        #   if (!is.null(icon[[i]]))
        #     tags$i(class = "material-icons", icon[[i]])
        # ),
        shiny::tags$a(
          class =
            paste0(
              ifelse(
                is.null(color),
                "",
                paste0(" ", color, "-text")
              )
            ),
          href = paste0("#", tabs[[i]]),
          tags$i(class = "material-icons", icon[[i]]),
          names(tabs)[[i]]
        )
      )
  }


  if (!is.null(color)) {
    tabs_style <-
      shiny::tagList(
        shiny::tags$head(
          shiny::tags$style(
            paste0(
              "
            #", this_id, " .indicator {
            position: absolute;
            bottom: 0;
            height: 2px;
            background-color: ", color, " !important;
            will-change: left, right;
            }
            #", this_id, " .tab a:focus, #", this_id, " .tab a:focus.active {
            background-color: ", paste0("rgba(", paste0(as.character(grDevices::col2rgb(color)[, 1]), collapse = ", "), ", 0.2)"), ";
            outline: none;
            }
            "
            )
          )
        )
      )
  } else {
    tabs_style <- shiny::tags$div()
  }

  shiny::tagList(
    shiny::tags$ul(
      id = this_id,
      class = "tabs tabs-fixed-width",
      material_tabs
    ),
    tabs_style
  )
}



#' Write code to R script from tagged diagnostics
#'
#' Use this function to write code to R script from diagnostics tagged in Certara's VPC Results Shiny Application.
#'
#' @param tagged List of tagged objects from returned from \code{VPCResultsUI()}.
#' @param file Character specifying path of output file. If missing, it will be saved as \code{code.R} in working directory.
#'
#' @examples
#' if (interactive()) {
#'  tagged_diagnostics <-
#'    vpcResultsUI(tidyvpc::obs_data, tidyvpc::sim_data)
#'
#'  write_code(tagged_diagnostics, "tagged_vpc.R")
#' }
#'
#' @return Returns \code{NULL} after writing to \code{file}.
#' @export

write_code <- function(tagged, file) {
  if (missing(file)) {
    stop("Missing \"file\" argument.")
  }

  lines <- vector(mode = "list", length = length(tagged))
  for (i in seq_along(tagged)) {
    lines[[i]] <- paste0(tagged[[i]]$code)
  }
  writeLines(unlist(lines), con = file)
}



toggleVisibility <- function(selector, condition) {
  if (condition) {
    shinyjs::show(selector = selector)
  } else {
    shinyjs::hide(selector = selector)
  }
}
