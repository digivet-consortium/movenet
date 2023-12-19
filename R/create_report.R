#' Create a network analysis report from data
#'
#' @param output output file name (and path)
#' @param output_format output format ("html_document", "pdf_document")
#' @param data movement data
#'
#' @importFrom rmarkdown render
#'
#' @keywords internal
create_report <- function(data, output, output_format = "html_document"){

  report_source <- "reports/static_network_analysis.rmd"
  #Have this vary according to an arg report_type

  #Parameters
  date <- Sys.Date()

  #Knit report
  render(input = report_source,
         output_format = output_format,
         output_file = output
         )

  }
