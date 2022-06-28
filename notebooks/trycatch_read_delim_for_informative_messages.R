load_all()
delim <- NULL
min_move_keys <- c("movenet.origin_ID", "movenet.dest_ID", "movenet.move_date", "movenet.nr_pigs")
minvars <- movenetenv$options$movement_data[min_move_keys]
extra <- movenetenv$options$movement_data[is.na(match(names(movenetenv$options$movement_data),c("movenet.origin_ID","movenet.dest_ID","movenet.move_date","movenet.nr_pigs")))]
minvar_coltypes <- list("c", "c", "c", "c")
names(minvar_coltypes) <- lapply(unname(minvars),FUN=as.name)
move_data_file<-"tests/testthat/test_input_files/ScotEID_testdata_colmissing.csv"

condition<-rlang::catch_cond(selected_data <- read_delim(move_data_file, delim = delim,
                                                         col_select = unname(unlist(c(minvars,extra))),
                                                         col_types = minvar_coltypes #this guesses column type when not specified, i.e. for extra variables
                                                         ))
condition$msg
