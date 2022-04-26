#' Reformats movement data from a delimited file into a common intermediate format (extracts and renames selected columns)
#'
#' @param move_data_file a delimited file with movement data
#'
#' @return reformatted movement data (selected & renamed columns)
#' @export
#'
#' @examples
reformat_move_data <- function(move_data_file){
  source("config.R") #better as argument to function, or turned into .yml file?
  varlist <- c(move_ID,origin_ID,dest_ID,dep_date,arr_date,nr_pigs)
  data.table::fread(move_data_file, select=varlist,
        col.names=c("move_ID","origin_ID","dest_ID","dep_date","arr_date","nr_pigs")
  )
  }

#Dates: Make date format consistent: YYYY-MM-DD , YYYYMMDD , separate columns?
