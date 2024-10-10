#' @importFrom stats as.formula formula na.omit
#' @importFrom utils read.csv modifyList
NULL

vpc_results_env <- new.env()

utils::globalVariables(
  c(
    ".",
    ".x",
    "DV",
    "ID1",
    "ID2",
    "ID3",
    "ID4",
    "ID5",
    "IVAR",
    "LLOQ",
    "PRED",
    "REPLICATE",
    "alq",
    "blq",
    "hi",
    "l.ypc",
    "lo",
    "md",
    "pirana_obs_path",
    "pname",
    "qname",
    "x",
    "xbin",
    "xleft",
    "xright",
    "y",
    "ypc"
  )
)
