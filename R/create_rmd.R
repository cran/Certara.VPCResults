create_rmdFile <- function(directory) {
  if (missing(directory)) {
    stop("Missing \"directory\" argument.")
  }

  directory <- gsub("\\", "/", directory, fixed = TRUE)

  rmdFile <- file.path(directory, "report_template.Rmd")
  file.create(rmdFile)

  return(rmdFile)
}

create_yaml <- function(title, orientation, marginLeft, marginRight, marginTop, marginBottom) { # add arguments for to create toc

  title <- paste0("title: \"", title, "\"")
  yamlBreak <- "---"
  html_theme <- "united"

  yamlHeaders <- c(
    title,
    "date: '`r format(Sys.time(), \"%m-%d-%y\")`'",
    "output:",
    "  word_document:",
    "    toc: true",
    "    toc_depth: 4",
    "    reference_docx: report_template.docx",
    "  html_document:",
    "    toc: true",
    "    toc_depth: 4",
    "    toc_float: true",
    "    keep_md: false",
    paste0("    theme: ", html_theme),
    "  pdf_document:",
    "    toc: true",
    "    number_sections: true",
    "    fig_caption: true",
    "params:",
    "  inputs: NA",
    paste0("classoption: ", orientation),
    paste0("geometry: ", paste0("left=", marginLeft, "cm,",
      "right=", marginRight, "cm,",
      "top=", marginTop, "cm,",
      "bottom=", marginBottom, "cm",
      collapse = ""
    ))
  )



  yamlHead <- c(yamlBreak, yamlHeaders, yamlBreak)

  return(yamlHead)
}

create_yaml_raw <- function(title, orientation, marginLeft, marginRight, marginTop, marginBottom) {
  title <- paste0("title: \"", title, "\"")
  yamlBreak <- "---"
  html_theme <- "united"
  reference_docx <- system.file(package = "Certara.VPCResults", "extdata", "report_template.docx")

  yamlHeaders <- c(
    title,
    "date: '`r format(Sys.time(), \"%m-%d-%y\")`'",
    "output:",
    "  word_document:",
    "    toc: true",
    "    toc_depth: 4",
    "    keep_md: false",
    paste0("    reference_docx: ", reference_docx),
    "  html_document:",
    "    toc: true",
    "    toc_depth: 4",
    "    toc_float: true",
    "    keep_md: false",
    paste0("    theme: ", html_theme),
    "  pdf_document:",
    "    toc: true",
    "    number_sections: true",
    "    fig_caption: true",
    "params:",
    "  inputs: NA",
    paste0("geometry: ", paste0("left=", marginLeft, "cm,",
      "right=", marginRight, "cm,",
      "top=", marginTop, "cm,",
      "bottom=", marginBottom, "cm",
      collapse = ""
    ))
  )


  yamlHead <- c(yamlBreak, yamlHeaders, yamlBreak)

  return(yamlHead)
}

create_rmd <- function(title, objects, orientation, marginLeft, marginRight, marginTop, marginBottom) {
  rmdFile <- create_rmdFile()

  rmd <- create_yaml(title, orientation = tolower(orientation), marginLeft, marginRight, marginTop, marginBottom) # need arguments for toc

  chunkNoIncl <- "\n```{r, include = FALSE}"
  chunkStart <- "\n```{r, echo = FALSE, message = FALSE, warning=FALSE}"
  chunkEnd <- "```\n"

  dep <- c("Certara.VPCResults", "ggplot2", "magrittr", "dplyr", "tidyvpc")

  libs <- paste0("library(", dep, ")")
  plotDims <- "knitr::opts_chunk$set(dpi = 300, fig.width = 10, fig.height = 5, message = FALSE, warning = FALSE)"

  rmd <- c(rmd, chunkNoIncl, libs, plotDims, chunkEnd)

  code_out <- list()
  for (obj in seq_along(objects)) {
    objname <- names(objects)[[obj]]
    header <- paste0("# ", objname)
    chunk <- paste0("knitr::knit_print(params$inputs$`", objname, "`[[2]])")
    code_out[[obj]] <- c(header, chunkStart, chunk, chunkEnd)
  }

  rmd <- c(rmd, code_out)

  writeLines(unlist(rmd), con = rmdFile)
}


create_rmd_raw <- function(title, objects, orientation, marginLeft, marginRight, marginTop, marginBottom) {
  rmd <- create_yaml_raw(title, orientation = tolower(orientation), marginLeft, marginRight, marginTop, marginBottom)

  chunkNoIncl <- "\n```{r, include = FALSE}"
  chunkStart <- "\n```{r, echo = FALSE, message = FALSE, warning=FALSE}"
  chunkEnd <- "```\n"

  dep <- c("Certara.VPCResults", "ggplot2", "magrittr", "dplyr", "tidyvpc")

  libs <- paste0("library(", dep, ")")
  plotDims <- "knitr::opts_chunk$set(dpi = 300, fig.width = 10, fig.height = 5, message = FALSE, warning = FALSE)"

  rmd <- c(rmd, chunkNoIncl, libs, plotDims, chunkEnd)

  code_out <- list()
  for (obj in seq_along(objects)) {
    header <- paste0("# ", names(objects)[[obj]])
    chunk <- objects[[obj]]$code

    code_out[[obj]] <- c(header, chunkStart, chunk, chunkEnd)
  }

  rmd <- c(rmd, code_out)


  return(rmd)
}

report_render <- function(type){
  if(type == "pdf"){
    render_fun <- "pdf_document"#rmarkdown::pdf_document(toc = TRUE)
  } else if (type == "docx"){
    render_fun <- "word_document"#rmarkdown::word_document(toc = TRUE, reference_docx = "report_template.docx")
  } else {
    render_fun <- "html_document"#rmarkdown::html_document(toc = TRUE)
  }
  return(render_fun)
}
