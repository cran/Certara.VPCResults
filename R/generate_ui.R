
generate_VPC_ui <- function(settings, observed, simulated, vpc.type) {
  jscode <- "shinyjs.closewindow = function() { window.close(); }"


  if (vpc.type == "continuous") {
    # Continuous ----
    ui <- tagList(

      ## 1.0 ShinyJS ----
      shinyjs::useShinyjs(),
      shinyjs::extendShinyjs(
        text = jscode,
        functions = c("closewindow")
      ),

      tags$head(tags$style(styleCSS)),
      shinyWidgets::chooseSliderSkin("Modern", color = "#0a7bc1"),

      ## 2.0 Header ----
      certara_header(header_title = "VPC Results"),

      ## 3.0 Page ----
      bslib::page_sidebar(
        window_title = "VPC Results",

        ## 4.0 Sidebar ----
        sidebar = bslib::sidebar(
          width = 300,
          open = TRUE,

          bslib::accordion(
            ### 5.1 Inputs ----
                bslib::accordion_panel(
                  title = shiny::HTML(paste0('<i class="fa-solid fa-pen"></i>&nbsp&nbsp; Inputs')),

              #### 5.1a Variables ----
              fluidRow(
                h5("Variables"),
                column(
                  width = 10,
                  selectInput(inputId = "yvar", "y-var", choices = c("", names(observed)), selected = NULL),
                  conditionalPanel(
                    "input.isDVDifferent == true",
                    selectInput(inputId = "yvar2", "y-var (simulated)", choices = c("", names(simulated)), selected = NULL)
                  ),
                  selectInput(inputId = "xvar", "x-var", choices = c("", names(observed)), selected = NULL),
                )
              ),
              fluidRow(
                checkboxInput(inputId = "isDVDifferent", label = "Specify simulated y-var", value = FALSE)
              ),

              #### 5.1b Options ----
                  fluidRow(
                    h5("Options"),
                    column(
                      width = 5,
                      checkboxInput(inputId = "isStrat", label = "Stratify")
                    ),
                    column(
                      width = 5,
                      checkboxInput(inputId = "isCensoring", label = "Censor")
                    )
                  ),
                  fluidRow(
                    column(
                      width = 10,
                      checkboxInput(inputId = "isPred", label = "Prediction Corrected", )
                    )
                  ),
                  fluidRow(
                    column(
                      width = 10,
                      conditionalPanel(
                        "input.isStrat == true",
                        div(style = "padding:5px;"),
                        h6("Stratify Options"),
                        selectInput(inputId = "stratvar", label = "Stratification Variable(s)", choices = names(observed), multiple = TRUE),
                        uiOutput("outstratvar")
                      ),
                      conditionalPanel(
                        "input.isCensoring == true",
                        div(style = "padding:10px;"),
                        h6("Censoring Options"),
                        selectInput(inputId = "censorType", label = "Censor", choices = c("variable", "value")),
                        conditionalPanel(
                          "input.censorType == 'variable'",
                          selectInput(inputId = "censorvar", label = "Censoring Variable (LLOQ)", choices = c("", names(observed)), multiple = FALSE)
                        ),
                        conditionalPanel(
                          "input.censorType == 'value'",
                          numericInput(inputId = "userLLOQ", "LLOQ", min = -Inf, max = Inf, value = 0)
                        )
                      ),
                      conditionalPanel(
                        "input.isPred == true",
                        div(style = "padding:5px;"),
                        h6("Prediction-Corrected Options"),
                        column(
                          width = 12,
                          selectInput(inputId = "predvar", label = "Prediction Variable", choices = c("", names(observed)), multiple = FALSE)
                        ),
                        column(
                          width = 6,
                          div(
                            title = "Check box if DV was fit in log scale",
                            checkboxInput(inputId = "log_dv", label = "Log y-var")
                          )
                        )
                      )
                    )
                  )
                ),

            ### 5.2 Quantiles ----
                bslib::accordion_panel(
                  title = shiny::HTML(paste0('<i class="fa-solid fa-bars"></i>&nbsp&nbsp; Quantiles')),
                  fluidRow(
                    column(
                      width = 10,
                      selectInput(inputId = "quantileType", label = "Quantile Type", choices = paste("Type", 1:7, sep = " "), selected = settings$vpc.quantile.type),
                      numericInput(inputId = "piUser_hi", label = "Upper", value = .95, min = 0.001, max = .999, step = .1),
                      uiOutput("piValidation_hi"),
                      numericInput(inputId = "piUser_med", label = "Median", value = .5, min = 0.001, max = .999, step = .1),
                      uiOutput("piValidation_med"),
                      numericInput(inputId = "piUser_low", label = "Lower", value = .05, min = 0.001, max = .999, step = .1),
                      uiOutput("piValidation_low")
                    )
                  )
                ),

            ### 5.3 Confidence Level ----
                bslib::accordion_panel(
                  title = shiny::HTML(paste0('<i class="fa-solid fa-bars"></i>&nbsp&nbsp; Confidence Level')),
                  fluidRow(
                    column(
                      width = 10,
                      numericInput(inputId = "ciUser", label = "Confidence Level", value = .95, min = 0.001, max = .999, step = .1),
                      uiOutput("ciValidation")
                    )
                  )
                ),

            ### 5.4 Controls ----
                bslib::accordion_panel(
                  title = shiny::HTML(paste0('<i class="fa-solid fa-sliders"></i>&nbsp&nbsp; Controls')),
                  fluidRow(
                    column(
                      width = 10,
                      checkboxInput(inputId = "isBinless", label = "Binless", value = FALSE)
                    )
                  ),
                  fluidRow(
                    column(
                      width = 10,
                      conditionalPanel(
                        "input.isBinless == false",
                        selectInput(inputId = "typeBinning",
                                    label = "Binning Type",
                                    choices = c("x-variable", "ntile", "pam", "sd", "equal", "pretty", "quantile",
                                      "kmeans", "jenks", "centers", "breaks", "headtails", "maximum", "box"
                                    ),
                                    selected = "pam"
                        )
                      ),
                      conditionalPanel(
                        "input.isBinless == true",
                        tags$div(
                          title = "Optimize smoothing parameters using AIC",
                          checkboxInput(inputId = "isAutoOptimize", label = "Optimize (AIC)", value = TRUE)
                        ),
                        div(style = "padding: 5px;")
                      )
                    )
                  ),
                  conditionalPanel(
                    "input.isBinless == false",
                    fluidRow(
                      column(
                        width = 10,
                        conditionalPanel(
                          "input.typeBinning != 'centers' &&
                           input.typeBinning != 'x-variable' &&
                           input.typeBinning != 'box' &&
                           input.typeBinning != 'headtails' &&
                           input.typeBinning != 'breaks' ",
                          numericInput(inputId = "nbins", "N Bins", value = 5, min = 1, max = 99)
                        ),
                        conditionalPanel(
                          "input.typeBinning == 'centers'",
                          textInput("centers", "Centers", value = NULL, placeholder = "0,3,5, ...")
                        ),
                        conditionalPanel(
                          "input.typeBinning == 'breaks'",
                          textInput("breaks", "Breaks", value = NULL, placeholder = "0,3,5, ...")
                        ),
                        conditionalPanel(
                          "input.typeBinning == 'box'",
                          numericInput(inputId = "iqr_mult", "IQR Multiplier", value = 1.5, min = 0, max = 5, step = 0.1)
                        ),
                        conditionalPanel(
                          "input.typeBinning == 'headtails'",
                          numericInput(inputId = "thr", "Threshold", value = 0.4, min = 0, max = 1, step = 0.05)
                        )
                      )
                    )
                  ),
                  conditionalPanel(
                    "input.isBinless == true",
                    fluidRow(style = "padding-left: 10px;",
                      conditionalPanel(
                        "input.isAutoOptimize == true",
                        fluidRow(style = "width: inherit;",
                          h6("Optimization Interval Range"),
                          column(
                            width = 5,
                            numericInput(inputId = "smoothingIntervalLo", "Lower", value = 0, min = 0, max = 1000)
                          ),
                          column(
                            width = 5,
                            numericInput(inputId = "smoothingIntervalHi", "Upper", value = 7, min = 1, max = 1000)
                          )
                        )
                      )
                    ),
                    conditionalPanel(
                      "input.isAutoOptimize == false",
                      conditionalPanel(
                        "input.isPred == true",
                        tags$h6("LOESS Smoothing Parameter"),
                        div(
                          numericInput(inputId = "span", label = "Span", min = 0, max = 1, value = .5, step = .1)
                        )
                      ),
                      div(
                        tags$h6("Lambda Smoothing Parameters"),
                        numericInput(inputId = "lambdaHi", label = "Lambda Upper Quantile", value = 3, min = 0, max = Inf, step = 1),
                        numericInput(inputId = "lambdaMed", label = "Lambda Median Quantile", value = 3, min = 0, max = Inf, step = 1),
                        numericInput(inputId = "lambdaLo", label = "Lambda Lower Quantile", value = 3, min = 0, max = Inf, step = 1)
                      )
                    )
                  )
                )
              ),

          ### 5.5 Plot Button ----
              fluidRow(
                column(style = "display: flex; justify-content: flex-end",
                  width = 12,
                  actionButton("buttonPlot", label = "Plot")
                )
              )
            ),


        ## 6.0 Main Body Card ----
            bslib::navset_card_underline(
              id = "maincard",
              title = NULL,

          ### 6.1 Preview Tab ----
              bslib::nav_panel(
                title = "Preview",
                bslib::card_body(
                  height = "100%",
                  div(
                    div(
                      style = "padding-left: 15px;  padding-bottom: 10px;",
                      checkboxInput(inputId = "isDynamic", label = "Interactive", value = FALSE)
                    ),

            #### 6.1a Plot Display ----
                    div(
                      style = "padding-left: 25px; margin-left: 25px;  padding-right: 25px; padding-bottom: 15px;",
                      conditionalPanel(
                      "input.isDynamic == false",
                        bslib::card(
                          style = "border: none;",
                          full_screen = TRUE,
                          shinyjqui::jqui_resizable(
                            plotOutput("vpcPlot")
                          )
                        )
                      ),
                      conditionalPanel(
                      "input.isDynamic == true",
                        bslib::card(
                          style = "border: none;",
                          full_screen = TRUE,
                          shinyjqui::jqui_resizable(
                            plotly::plotlyOutput("vpcPlotly")
                          )
                        )
                      )
                    ),

            #### 6.1b Plot Options Card ----
                    conditionalPanel(
                      "input.buttonPlot > 0",
                      bslib::navset_card_underline(
                        id = "plottabs",

              ##### Style Sub-Tab ----
                        bslib::nav_panel(
                          title = shiny::HTML(paste0('<i class="fa-solid fa-paint-roller"></i>&nbsp&nbsp; Style')),
                          bslib::card_body(
                            class = "style-tab",
                            fluidRow(style = "display:flex; padding-top: 10px;",
                              column(
                                width = 2, offset = 1, class = "style-column",
                                htmltools::h5("Lower Quantile"),
                                selectInput(inputId = "lineTypeLo", "Lower Quantile \n Line Type (Observed)", choices = c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash"), selected = settings$vpc.line.type.lo),
                                colourpicker::colourInput("colorLineLo", "Line/Interval Color (Simulated)", value = settings$vpc.line.color.lo),
                              ),
                              column(
                                width = 2, class = "style-column",
                                htmltools::h5("Median Quantile"),
                                selectInput(inputId = "lineTypeMed", "Line Type (Observed)", choices = c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash"), selected = settings$vpc.line.type.med),
                                colourpicker::colourInput("colorLineMed", "Line/Interval Color (Simulated)", value = settings$vpc.line.color.med),
                              ),
                              column(
                                width = 2, class = "style-column",
                                htmltools::h5("Upper Quantile"),
                                selectInput(inputId = "lineTypeHi", "Line Type (Observed)", choices = c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash"), selected = settings$vpc.line.type.hi),
                                colourpicker::colourInput("colorLineHi", "Line/Interval Color (Simulated)", value = settings$vpc.line.color.hi),
                              ),
                              column(
                                width = 2, class = "style-column",
                                htmltools::h5("Prediction Interval"),
                                numericInput(inputId = "alphaFill", "Transparency", value = settings$vpc.pi.alpha.fill, min = .01, max = .99, step = .01)
                              ),
                              column(
                                width = 2, class = "style-column",
                                htmltools::h5("Observed Data"),
                                selectInput(inputId = "shapePoint", "Shape", choices = c("none", "circle", "circle-fill", "square", "square-fill", "triangle", "triangle-fill"), selected = settings$vpc.point.shape),
                                numericInput(inputId = "sizePoint", "Size", min = 0, max = 10, step = .1, value = settings$vpc.point.size),
                                numericInput(inputId = "alphaPoint", "Transparency", value = settings$vpc.point.alpha, min = .01, max = .99, step = .01),
                                colourpicker::colourInput("colorPoint", "Color", value = settings$vpc.point.color),
                              )
                            )
                          )
                        ),

              ##### Layout Sub-Tab ----
                        bslib::nav_panel(
                          title = shiny::HTML(paste0('<i class="fa-solid fa-table-cells"></i>&nbsp&nbsp; Layout')),
                          bslib::card_body(
                            min_height = "240px",
                            class = "layout-tab",
                            fluidRow(class = "multi-input-with-checkbox",
                              column(
                                width = 2,
                                selectInput(inputId = "legendPosition", "Legend Position", choices = c("top", "bottom", "left", "right"), selected = settings$vpc.legend.position)
                              ),
                              column(class = "vpc-cols col-checkbox",
                                width = 2,
                                checkboxInput(inputId = "facetQuantile", "Facet Quantiles", value = settings$vpc.facet.quantile)
                              ),
                              column(class = "vpc-cols col-checkbox",
                                width = 2,
                                checkboxInput(inputId = "isLogY", "Log Y", value = FALSE)
                              ),
                              column(class = "vpc-cols col-checkbox",
                                width = 2,
                                checkboxInput(inputId = "isLogX", "Log X", value = FALSE)
                              )
                            ),
                            fluidRow(
                              class = "scales_layout",
                              column(
                                width = 2,
                                selectInput(inputId = "axisScales", "Axis Scales", choices = c("free", "fixed"), selected = settings$vpc.axis.scale)
                              )
                            )
                          )
                        ),

              ##### Display Sub-Tab ----
                        bslib::nav_panel(
                          title = shiny::HTML(paste0('<i class="fa-solid fa-eye-slash"></i>&nbsp&nbsp; Display')),
                          bslib::card_body(
                            class = "display-tab",
                            div(style = "padding: 5px;"),
                            fluidRow(class = "multi-input-with-checkbox",
                              column(
                                class = "vpc-cols col-checkbox",
                                width = 2,
                                checkboxInput(inputId = "isDefaultText", label = "Default Text", value = settings$text.default)
                              ),
                              column(
                                class = "vpc-cols col-checkbox",
                                width = 2,
                                checkboxInput(inputId = "isCertaraTheme", label = "Certara Theme", value = settings$certara.theme)
                              ),
                              column(
                                class = "binning_ui_inputs col-checkbox",
                                width = 2,
                                checkboxInput(inputId = "showBoundaries", label = "Bin Boundaries", value = settings$vpc.show.boundaries),
                                div(style = "padding:5px;"),
                                conditionalPanel(
                                  "input.showBoundaries == true",
                                  checkboxInput(inputId = "showBinning", label = "Bin Lines", value = settings$vpc.show.binning)
                                )
                              ),
                              column(
                                class = "vpc-cols col-checkbox",
                                width = 2,
                                conditionalPanel(
                                  "input.isCertaraTheme == false",
                                  checkboxInput(inputId = "isShowBorder", label = "Border", value = settings$background.border)
                                )
                              ),
                              column(
                                class = "vpc-cols col-checkbox",
                                width = 2,
                                conditionalPanel(
                                  "input.isCertaraTheme == false",
                                  checkboxInput(inputId = "showGridLines", "Grid Lines", value = settings$background.gridlines)
                                )
                              ),
                              column(
                                class = "vpc-cols",
                                width = 2, style = "margin-top: -7px;",
                                conditionalPanel(
                                  "input.isCertaraTheme == false",
                                  colourpicker::colourInput("colorBackground", "Plot Background Color", value = settings$background.color)
                                )
                              )
                            ),
                            fluidRow(
                              column(
                                width = 2,
                                div(
                                  class = "custom_text_inputs",
                                  textInput(inputId = "textTitle", "Title", value = ""),
                                  textInput(inputId = "textSubtitle", "Subtitle", value = ""),
                                  textInput(inputId = "textCaption", "Caption", value = ""),
                                  textInput(inputId = "xlab", "x-Label", value = ""),
                                  textInput(inputId = "ylab", "y-Label", value = "")
                                )
                              ),
                              column(class = "custom_plot_theme_inputs",
                                width = 2, style = "margin-left: 0rem; margin-right: 0rem;",
                                numericInput(inputId = "sizeTitle", "Title Size", min = 1, max = 30, value = settings$title.size, step = 1),
                                selectInput(inputId = "fontTitle", "Title Font", choices = c("Times New Roman", "Arial", "Courier New"), selected = settings$title.font),
                                selectInput(inputId = "faceTitle", "Title Face", choices = c("plain", "bold", "italic", "bold.italic"), selected = settings$title.face),
                                colourpicker::colourInput("colorTitle", "Title Font Color", value = settings$title.color)
                              ),
                              column(class = "custom_plot_theme_inputs",
                                width = 2, style = "margin-left: 0rem; margin-right:0rem;",
                                numericInput(inputId = "sizeSubtitle", "Subtitle Size", min = 1, max = 30, value = settings$subtitle.size, step = 1),
                                selectInput(inputId = "fontSubtitle", "Subtitle Font", choices = c("Times New Roman", "Arial", "Courier New"), selected = settings$subtitle.font),
                                selectInput(inputId = "faceSubtitle", "Subtitle Face", choices = c("plain", "bold", "italic", "bold.italic"), selected = settings$subtitle.face),
                                colourpicker::colourInput("colorSubtitle", "Subtitle Font Color", value = settings$subtitle.color)
                              ),
                              column(class = "custom_plot_theme_inputs",
                                width = 2, style = "margin-left: 0rem; margin-right: 0rem;",
                                numericInput(inputId = "sizeCaption", "Caption Size", min = 1, max = 30, value = settings$caption.size, step = 1),
                                selectInput(inputId = "fontCaption", "Caption Font", choices = c("Times New Roman", "Arial", "Courier New"), selected = settings$caption.font),
                                selectInput(inputId = "faceCaption", "Caption Face", choices = c("plain", "bold", "italic", "bold.italic"), selected = settings$caption.face),
                                colourpicker::colourInput("colorCaption", "Caption Font Color", value = settings$caption.color)
                              ),
                              column(class = "custom_plot_theme_inputs",
                                width = 2, style = "margin-left: 0rem; margin-right: 0rem;",
                                numericInput(inputId = "sizeAxis", "Axis Label Size", min = 1, max = 30, value = settings$axis.size, step = 1),
                                selectInput(inputId = "fontAxis", "Axis Label Font", choices = c("Times New Roman", "Arial", "Courier New"), selected = settings$axis.font),
                                selectInput(inputId = "faceAxis", "Axis Label Face", choices = c("plain", "bold", "italic", "bold.italic"), selected = settings$axis.face),
                                colourpicker::colourInput("colorAxis", "Axis Label Font Color", value = settings$axis.color)
                              ),
                              column(class = "custom_plot_theme_inputs",
                                width = 2, style = "margin-left: 0rem; margin-right: 0rem;",
                                numericInput(inputId = "sizeLegend", "Legend Size", min = 1, max = 30, value = settings$legend.size, step = 1),
                                selectInput(inputId = "fontLegend", "Legend Font", choices = c("Times New Roman", "Arial", "Courier New"), selected = settings$legend.font),
                                selectInput(inputId = "faceLegend", "Legend Face", choices = c("plain", "bold", "italic", "bold.italic"), selected = settings$legend.face),
                                colourpicker::colourInput("colorLegend", "Legend Font Color", value = settings$legend.color)
                              )
                            )
                          )
                        )
                      ),

            #### 6.1c Tag Button ----
                      fluidRow(
                        column(
                          width = 12,
                          conditionalPanel(
                            "input.buttonPlot > 0",
                            actionButton(inputId = "open_savePlotModal", label = NULL, icon = icon("tag"))
                          )
                        )
                      )
                    )
                  )
                )
              ),

          ### 6.2 Tagged Tab ----
              bslib::nav_panel(
                title = "Tagged",
                bslib::card_body(
                  uiOutput("myTaggedDiagnostics"),
                  div(
                    style = "padding-left: 25px; padding-right: 25px; padding-bottom: 15px;",
                    shinyjqui::jqui_resizable(
                      plotOutput("myTaggedPlots"),
                    )
                  ),
                  shinyAce::aceEditor(
                    outputId = "tidyvpc_code",
                    autoScrollEditorIntoView = TRUE,
                    minLines = 5,
                    maxLines = 35,
                    value = NULL,
                    readOnly = TRUE
                  )
                )
              ),

          ### 6.3 Report Tab ----
              bslib::nav_panel(
                title = "Report",
                bslib::card_body(
                  fluidRow(
                    style = "padding-left:25px; padding-right:25px;",
                    column(
                      width = 12,
                      fluidRow(
                        column(
                          width = 4,
                          textInput(inputId = "reportName", "Report Title:", value = paste0("Report_", format(Sys.time(), "%Y-%m-%d_%H:%M:%S")), width = "125%")
                        ),
                        column(
                          width = 2,
                          selectInput(inputId = "fileType", "File Type", choices = c("html", "pdf", "docx"))
                        ),
                        column(
                          width = 2,
                          conditionalPanel(
                            "input.fileType == 'pdf'",
                            selectInput(inputId = "pageLayout", "Page Layout", choices = c("Portrait", "Landscape"))
                          )
                        )
                      ),
                      conditionalPanel(
                        "input.fileType == 'pdf'",
                        fluidRow(
                          column(
                            width = 2,
                            numericInput(inputId = "marginLeft", "Margin Left (unit: cm)", min = 1, max = 10, value = 3, step = 1)
                          ),
                          column(
                            width = 2,
                            numericInput(inputId = "marginRight", "Margin Right (unit: cm)", min = 1, max = 10, value = 3, step = 1)
                          ),
                          column(
                            width = 2,
                            numericInput(inputId = "marginTop", "Margin Top (unit: cm)", min = 1, max = 10, value = 2, step = 1)
                          ),
                          column(
                            width = 2,
                            numericInput(inputId = "marginBottom", "Margin Bottom (unit: cm)", min = 1, max = 10, value = 2, step = 1)
                          )
                        )
                      )
                    )
                  ),
                  uiOutput("selectReport"),
                  fluidRow(
                    column(
                      width = 10, offset = 2,
                      uiOutput("report_download_buttons")
                    )
                  )
                )
              )
            )
      ),

      ## 7.0 Footer ----
      certara_footer()
    )



    # Categorical ----
  } else {
    ui <- tagList(

      ## 1.0 ShinyJS ----
      shinyjs::useShinyjs(),
      shinyjs::extendShinyjs(
        text = jsFunctions,
        functions = c("closewindow")
      ),

      tags$head(tags$style(styleCSS)),
      shinyWidgets::chooseSliderSkin("Modern", color = "#0a7bc1"),

      ## 2.0 Header ----
      certara_header(header_title = "VPC Results"),

      ## 3.0 Page ----
      bslib::page_sidebar(
        window_title = "VPC Results",

        ## 4.0 Sidebar ----
        sidebar = bslib::sidebar(
          width = 300,
          open = TRUE,

          bslib::accordion(
            ### 4.1 Inputs ----
            bslib::accordion_panel(
              title = shiny::HTML(paste0('<i class="fa-solid fa-pen"></i>&nbsp&nbsp; Inputs')),

              #### 4.1a Variables ----
              fluidRow(
                h5("Variables"),
                column(
                  width = 10,
                  selectInput(inputId = "yvar", "y-var", choices = c("", names(observed)), selected = NULL),
                  conditionalPanel(
                    "input.isDVDifferent == true",
                    selectInput(inputId = "yvar2", "y-var (simulated)", choices = c("", names(simulated)), selected = NULL)
                  ),
                  selectInput(inputId = "xvar", "x-var", choices = c("", names(observed)), selected = NULL),
                )
              ),
              fluidRow(
                checkboxInput(inputId = "isDVDifferent", label = "Specify simulated y-var", value = FALSE)
              ),

              #### 4.1b Options ----
              fluidRow(
                h5("Options"),
                column(
                  width = 6,
                  checkboxInput(inputId = "isStrat", label = "Stratify")
                )
              ),
              fluidRow(
                column(
                  width = 10,
                  conditionalPanel(
                    "input.isStrat == true",
                    selectInput(inputId = "stratvar", label = "Stratification Variable(s)", choices = names(observed), multiple = TRUE),
                    uiOutput("outstratvar")
                  )
                )
              )
            ),

            ### 4.2 Quantiles ----
            bslib::accordion_panel(
              title = shiny::HTML(paste0('<i class="fa-solid fa-bars"></i>&nbsp&nbsp; Quantiles')),
              fluidRow(
                column(
                  width = 10,
                  numericInput(inputId = "ciUser", label = "Confidence Level", value = .95, min = 0.001, max = .999, step = .01),
                  uiOutput("ciValidation"),
                  selectInput(inputId = "quantileType", label = "Quantile Type", choices = paste("Type", 1:7, sep = " "), selected = settings$vpc.quantile.type),
                )
              )
            ),

            ### 4.3 Controls ----
            bslib::accordion_panel(
              title = shiny::HTML(paste0('<i class="fa-solid fa-sliders"></i>&nbsp&nbsp; Controls')),
              fluidRow(
                column(
                  width = 10,
                  checkboxInput(inputId = "isBinless", label = "Binless", value = FALSE)
                )
              ),
              fluidRow(
                column(
                  width = 10,
                  conditionalPanel(
                    "input.isBinless == false",
                    selectInput(
                      inputId = "typeBinning",
                      label = "Binning Type",
                      choices = c("x-variable", "ntile", "pam", "sd", "equal", "pretty", "quantile",
                        "kmeans", "jenks", "centers", "breaks", "headtails", "maximum", "box"
                      ),
                      selected = "pam"
                    )
                  ),
                  conditionalPanel(
                    "input.isBinless == true",
                    tags$div(
                      title = "Optimize Smoothing Parameter using AIC",
                      checkboxInput(inputId = "isAutoOptimize", label = "Optimize (AIC)", value = TRUE)
                    )
                  )
                )
              ),
              conditionalPanel(
                "input.isBinless == false",
                fluidRow(
                  column(
                    width = 10,
                    conditionalPanel(
                      "input.typeBinning != 'centers' &&
                       input.typeBinning != 'x-variable' &&
                       input.typeBinning != 'box' &&
                       input.typeBinning != 'headtails' &&
                       input.typeBinning != 'breaks' ",
                      numericInput(inputId = "nbins", "N Bins", value = 5, min = 1, max = 99)
                    ),
                    conditionalPanel(
                      "input.typeBinning == 'centers'",
                      textInput("centers", "Centers", value = NULL, placeholder = "0,3,5, ...")
                    ),
                    conditionalPanel(
                      "input.typeBinning == 'breaks'",
                      textInput("breaks", "Breaks", value = NULL, placeholder = "0,3,5, ...")
                    ),
                    conditionalPanel(
                      "input.typeBinning == 'box'",
                      numericInput(inputId = "iqr_mult", "IQR Multiplier", value = 1.5, min = 0, max = 5, step = 0.1)
                    ),
                    conditionalPanel(
                      "input.typeBinning == 'headtails'",
                      numericInput(inputId = "thr", "Threshold", value = 0.4, min = 0, max = 1, step = 0.05)
                    )
                  )
                )
              ),
              conditionalPanel(
                "input.isBinless == true",
                fluidRow(style = "padding-left: 10px;",
                  conditionalPanel(
                    "input.isAutoOptimize == true",
                    fluidRow(style = "width: inherit;",
                      div(style = "padding-top: 10px;"),
                      h6("Optimization Interval Range"),
                      div(style = "padding-top: 5px;"),
                      column(
                        width = 5,
                        numericInput(inputId = "smoothingIntervalLo", "Lower", value = 0, min = 0, max = 1000)
                      ),
                      column(
                        width = 5,
                        numericInput(inputId = "smoothingIntervalHi", "Upper", value = 100, min = 1, max = 1000)
                      )
                    )
                  ),
                  conditionalPanel(
                    "input.isAutoOptimize == false",
                    uiOutput("categoricalSPInputs")
                  )
                )
              )
            )
          ),

          ### 4.4 Plot Button ----
          fluidRow(
            column(style = "display: flex; justify-content: flex-end",
              width = 12,
              actionButton("buttonPlot", label = "Plot")
            )
          )
        ),

        ## 5.0 Main Body Card ----
        bslib::navset_card_underline(
          id = "maincard",
          title = NULL,

          ### 5.1 Preview Tab ----
          bslib::nav_panel(
            title = "Preview",
            bslib::card_body(
              height = "100%",
              div(
                div(
                  style = "padding-left: 15px;  padding-bottom: 10px;",
                  checkboxInput(inputId = "isDynamic", label = "Interactive", value = FALSE)
                ),

                #### 5.1a Plot Display ----
              div(
                style = "padding-left: 25px; margin-left: 25px;  padding-right: 25px; padding-bottom: 15px;",
                conditionalPanel(
                  "input.isDynamic == false",
                  bslib::card(
                    style = "border: none;",
                    full_screen = TRUE,
                    shinyjqui::jqui_resizable(
                      plotOutput("vpcPlot")
                    )
                  )
                ),
                conditionalPanel(
                  "input.isDynamic == true",
                  bslib::card(
                    style = "border: none;",
                    full_screen = TRUE,
                    shinyjqui::jqui_resizable(
                      plotly::plotlyOutput("vpcPlotly")
                    )
                  )
                )
              ),

                #### 5.1b Plot Options Card ----
              conditionalPanel(
                "input.buttonPlot > 0",
                bslib::navset_card_underline(
                  id = "plottabs",

                    ##### Style Sub-Tab ----
                  bslib::nav_panel(
                    title = shiny::HTML(paste0('<i class="fa-solid fa-paint-roller"></i>&nbsp&nbsp; Style')),
                    bslib::card_body(
                      class = "style-tab",
                      column(width = 12,
                        fluidRow(
                          uiOutput("categoricalStyleOptions"),
                        ),
                        div(style = "padding: 5px;"),
                        fluidRow(style = "display: flex; justify-content: left; padding-left: calc(var(--bs-gutter-x)* .5); gap: 10px;",
                          column(
                            width = 2, class = "style-column-cat",
                            htmltools::h5("Prediction Interval"),
                            numericInput(inputId = "alphaFill", "Transparency", value = settings$vpc.pi.alpha.fill, min = .01, max = .99, step = .01)
                          ),
                          column(
                            width = 2, class = "style-column-cat",
                            htmltools::h5("Point Options"),
                            selectInput(inputId = "shapePoint", "Shape", choices = c("circle", "circle-fill", "square", "square-fill", "triangle", "triangle-fill"), selected = settings$vpc.point.shape),
                            numericInput(inputId = "sizePoint", "Size", min = 0, max = 10, step = .1, value = settings$vpc.point.size),
                            numericInput(inputId = "alphaPoint", "Transparency", value = settings$vpc.point.alpha, min = .01, max = .99, step = .01),
                            colourpicker::colourInput("colorPoint", "Color", value = settings$vpc.point.color)
                          )
                        )
                      )
                    )
                  ),

                    ##### Layout Sub-Tab ----
                  bslib::nav_panel(
                    title = shiny::HTML(paste0('<i class="fa-solid fa-table-cells"></i>&nbsp&nbsp; Layout')),
                    bslib::card_body(
                      class = "layout-tab",
                      min_height = "220px",
                      fluidRow(class = "multi-input-with-checkbox",
                        column(
                          width = 2,
                          selectInput(inputId = "legendPosition", "Legend Position", choices = c("top", "bottom", "left", "right"), selected = settings$vpc.legend.position)
                        ),
                        column(class = "vpc-cols col-checkbox",
                          width = 2,
                          checkboxInput(inputId = "facetQuantile", "Facet Categories", value = settings$vpc.facet.category)
                        ),
                        column(class = "vpc-cols col-checkbox",
                          width = 2,
                          checkboxInput(inputId = "isLogY", "Log Y", value = FALSE)
                        ),
                        column(class = "vpc-cols col-checkbox",
                          width = 2,
                          checkboxInput(inputId = "isLogX", "Log X", value = FALSE)
                        )
                      )
                    )
                  ),

                    ##### Display Sub-Tab ----
                  bslib::nav_panel(
                    title = shiny::HTML(paste0('<i class="fa-solid fa-eye-slash"></i>&nbsp&nbsp; Display')),
                    bslib::card_body(
                      class = "display-tab",
                      div(style = "padding: 5px;"),
                      fluidRow(class = "multi-input-with-checkbox",
                        column(
                          class = "vpc-cols col-checkbox",
                          width = 2,
                          checkboxInput(inputId = "isDefaultText", label = "Default Text", value = settings$text.default)
                        ),
                        column(
                          class = "vpc-cols col-checkbox",
                          width = 2,
                          checkboxInput(inputId = "isCertaraTheme", label = "Certara Theme", value = settings$certara.theme)
                        ),
                        column(
                          class = "binning_ui_inputs col-checkbox",
                          width = 2,
                          checkboxInput(inputId = "showBoundaries", label = "Bin Boundaries", value = settings$vpc.show.boundaries),
                          div(style = "padding:5px;"),
                          conditionalPanel(
                            "input.showBoundaries == true",
                            checkboxInput(inputId = "showBinning", label = "Bin Lines", value = settings$vpc.show.binning)
                          )
                        ),
                        column(
                          width = 2,
                          class = "custom_plot_theme_inputs col-checkbox",
                          checkboxInput(inputId = "isShowBorder", label = "Border", value = settings$background.border),
                        ),
                        column(
                          width = 2,
                          class = "custom_plot_theme_inputs col-checkbox",
                          checkboxInput(inputId = "showGridLines", "Grid Lines", value = settings$background.gridlines)
                        ),
                        column(style = "margin-top: -7px;",
                          width = 2,
                          class = "custom_plot_theme_inputs",
                          colourpicker::colourInput("colorBackground", "Plot Background Color", value = settings$background.color)
                        )
                      ),
                      fluidRow(
                        column(
                          width = 2,
                          div(
                            class = "custom_text_inputs",
                            textInput(inputId = "textTitle", "Title", value = ""),
                            textInput(inputId = "textSubtitle", "Subtitle", value = ""),
                            div(style = "3px;"),
                            textInput(inputId = "textCaption", "Caption", value = ""),
                            div(style = "3px;"),
                            textInput(inputId = "xlab", "x-Label", value = ""),
                            textInput(inputId = "ylab", "y-Label", value = "")
                          )
                        ),
                        column(class = "custom_plot_theme_inputs",
                          width = 2, style = "margin-left: 0rem; margin-right: 0rem;",
                          numericInput(inputId = "sizeTitle", "Title Size", min = 1, max = 30, value = settings$title.size, step = 1),
                          selectInput(inputId = "fontTitle", "Title Font", choices = c("Times New Roman", "Arial", "Courier New"), selected = settings$title.font),
                          selectInput(inputId = "faceTitle", "Title Face", choices = c("plain", "bold", "italic", "bold.italic"), selected = settings$title.face),
                          colourpicker::colourInput("colorTitle", "Title Font Color", value = settings$title.color)
                        ),
                        column(class = "custom_plot_theme_inputs",
                          width = 2, style = "margin-left: 0rem; margin-right:0rem;",
                          numericInput(inputId = "sizeSubtitle", "Subtitle Size", min = 1, max = 30, value = settings$subtitle.size, step = 1),
                          selectInput(inputId = "fontSubtitle", "Subtitle Font", choices = c("Times New Roman", "Arial", "Courier New"), selected = settings$subtitle.font),
                          selectInput(inputId = "faceSubtitle", "Subtitle Face", choices = c("plain", "bold", "italic", "bold.italic"), selected = settings$subtitle.face),
                          colourpicker::colourInput("colorSubtitle", "Subtitle Font Color", value = settings$subtitle.color)
                        ),
                        column(class = "custom_plot_theme_inputs",
                          width = 2, style = "margin-left: 0rem; margin-right: 0rem;",
                          numericInput(inputId = "sizeCaption", "Caption Size", min = 1, max = 30, value = settings$caption.size, step = 1),
                          selectInput(inputId = "fontCaption", "Caption Font", choices = c("Times New Roman", "Arial", "Courier New"), selected = settings$caption.font),
                          selectInput(inputId = "faceCaption", "Caption Face", choices = c("plain", "bold", "italic", "bold.italic"), selected = settings$caption.face),
                          colourpicker::colourInput("colorCaption", "Caption Font Color", value = settings$caption.color)
                        ),
                        column(class = "custom_plot_theme_inputs",
                          width = 2, style = "margin-left: 0rem; margin-right: 0rem;",
                          numericInput(inputId = "sizeAxis", "Axis Label Size", min = 1, max = 30, value = settings$axis.size, step = 1),
                          selectInput(inputId = "fontAxis", "Axis Label Font", choices = c("Times New Roman", "Arial", "Courier New"), selected = settings$axis.font),
                          selectInput(inputId = "faceAxis", "Axis Label Face", choices = c("plain", "bold", "italic", "bold.italic"), selected = settings$axis.face),
                          colourpicker::colourInput("colorAxis", "Axis Label Font Color", value = settings$axis.color)
                        ),
                        column(class = "custom_plot_theme_inputs",
                          width = 2, style = "margin-left: 0rem; margin-right: 0rem;",
                          numericInput(inputId = "sizeLegend", "Legend Size", min = 1, max = 30, value = settings$legend.size, step = 1),
                          selectInput(inputId = "fontLegend", "Legend Font", choices = c("Times New Roman", "Arial", "Courier New"), selected = settings$legend.font),
                          selectInput(inputId = "faceLegend", "Legend Face", choices = c("plain", "bold", "italic", "bold.italic"), selected = settings$legend.face),
                          colourpicker::colourInput("colorLegend", "Legend Font Color", value = settings$legend.color)
                        )
                      )
                    )
                  )
                ),

                  #### 5.1c Tag Button ----
                  fluidRow(
                    column(
                      width = 12,
                      conditionalPanel(
                        "input.buttonPlot > 0",
                        actionButton("open_savePlotModal", label = NULL, icon = icon("tag"))
                      )
                    )
                  )
                )
              )
            )
          ),

          ### 5.2 Tagged Tab ----
          bslib::nav_panel(
            title = "Tagged",
            bslib::card_body(
              uiOutput("myTaggedDiagnostics"),
              div(
                style = "padding-left: 25px; padding-right: 25px; padding-bottom: 15px;",
                shinyjqui::jqui_resizable(
                  plotOutput("myTaggedPlots"),
                )
              ),
              shinyAce::aceEditor(
                outputId = "tidyvpc_code",
                autoScrollEditorIntoView = TRUE,
                minLines = 5,
                maxLines = 35,
                value = NULL,
                readOnly = TRUE
              )
            )
          ),

          ### 5.3 Report Tab ----
          bslib::nav_panel(
            title = "Report",
            bslib::card_body(
              fluidRow(
                style = "padding-left:25px; padding-right:25px;",
                column(
                  width = 12,
                  fluidRow(
                    column(
                      width = 4,
                      textInput("reportName", "Report Title:", value = paste0("Report_", format(Sys.time(), "%Y-%m-%d_%H:%M:%S")), width = "125%")
                    ),
                    column(
                      width = 2,
                      selectInput(inputId = "fileType", "File Type", choices = c("html", "pdf", "docx"))
                    ),
                    column(
                      width = 2,
                      conditionalPanel(
                      "input.fileType == 'pdf'",
                        selectInput(inputId = "pageLayout", "Page Layout", choices = c("Portrait", "Landscape"))
                      )
                    )
                  ),
                  conditionalPanel(
                    "input.fileType == 'pdf'",
                    fluidRow(
                      column(
                        width = 2,
                        numericInput(inputId = "marginLeft", "Margin Left (unit: cm)", min = 1, max = 10, value = 3, step = 1)
                      ),
                      column(
                        width = 2,
                        numericInput(inputId = "marginRight", "Margin Right (unit: cm)", min = 1, max = 10, value = 3, step = 1)
                      ),
                      column(
                        width = 2,
                        numericInput(inputId = "marginTop", "Margin Top (unit: cm)", min = 1, max = 10, value = 2, step = 1)
                      ),
                      column(
                        width = 2,
                        numericInput(inputId = "marginBottom", "Margin Bottom (unit: cm)", min = 1, max = 10, value = 2, step = 1)
                      )
                    )
                  )
                )
              ),
              uiOutput("selectReport"),
              fluidRow(
                column(
                  width = 10, offset = 2,
                  uiOutput("report_download_buttons")
                )
              )
            )
          )
        )
      ),
      ## 6.0 Footer ----
      certara_footer()
    )
  }
  return(ui)
}
