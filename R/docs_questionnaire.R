#' Questionnaires used in the data collection of Brazil's censuses
#'
#' @description
#' Open on a browser the questionnaire used in the data collection of Brazil's
#' censuses
#'
#' @template year
#' @param type Character. The type of questionnaire, whether the one used in the
#'        sample component of the census, or on the universe component. Options
#'        currently include `c("sample")`.
#' @template showProgress
#' @template cache
#'
#' @return Opens a `.pdf` file on the browser
#' @export
#' @family Questionnaire
#' @examplesIf identical(tolower(Sys.getenv("NOT_CRAN")), "true")
#' library(censobr)
#'
#' # Open questionnaire on browser
#' questionnaire(year = 2010, type = 'sample', showProgress = FALSE)
#'
questionnaire <- function(year = NULL,
                               type = NULL,
                               showProgress = TRUE,
                               cache = TRUE){
  # year = 2010
  # type = 'sample'

  ### check inputs
  checkmate::assert_numeric(year)
  checkmate::assert_string(type)

  # data available for the years:
  years <- c(1970, 1980, 1991, 2000, 2010)
  if (isFALSE(year %in% years)) { stop(  paste0("Error: Questionnaire currently only available for the years: ",
                                              paste(years), collapse = " ")
                                        )}

  # data available for data sets:
  data_sets <- c('sample')
  if (isFALSE(type %in% data_sets)) { stop( paste0("Error: Questionnaire currently only available for the types: ",
                                              paste(data_sets, collapse = ", "))
                                            )}

  ### Get url
  fname <- paste0(year, '_questionnaire_', type, '.pdf')
  file_url <- paste0("https://github.com/ipeaGIT/censobr/releases/download/censo_docs/", fname)

  ### Download
  local_file <- download_file(file_url = file_url,
                              showProgress = showProgress,
                              cache = cache)
  # check if download worked
  if(is.null(local_file)) { return(NULL) }

  # open data dic on browser
  utils::browseURL(url = local_file)
}
