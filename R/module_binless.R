# Module for binless inputs

binless_inputs_ui <- function(id) {
  ns <- NS(id)

  tagList(
    tags$h6("Additive Quantile Regression"),
    div(style = "padding:5px;"),
    sliderInput(inputId = ns("lambdaHi"), label = "Lambda Hi", value = 3, min = 0, max = 7, step = .01),
    sliderInput(inputId = ns("lambdaMed"), label = "Lambda Med", value = 3, min = 0, max = 7, step = .01),
    sliderInput(inputId = ns("lambdaLo"), label = "Lambda Lo", value = 3, min = 0, max = 7, step = .01),
    checkboxInput(inputId = "isLoessYPC", label = "Loess Prediction Corrected"),
    conditionalPanel(
      condition = "input.isLoessYPC == true",
      # condition = paste0("input.", ns("isLoessYPC")),
      tags$h6("LOESS"),
      div(style = "padding:5px;"),
      sliderInput(inputId = ns("span"), label = "Span", min = 0, max = 1, value = .5)
    )
    # )
  )
}


binless_inputs <- function(input, output, session) {
  session <- session$ns

  piUser <- reactive({
    c(input$piLo, input$piMed, input$piHi)
  })

  lamUser <- reactive({
    c(input$lambdaLo, input$lambdaMed, input$lambdaHi)
  })

  spanUser <- reactive({
    input$span
  })

  intervalUser <- reactive({
    input$interval
  })


  return(reactive({
    list(
      lamUser = lamUser(),
      spanUser = spanUser(),
      intervalUser = intervalUser()
    )
  }))
}
