#Column headers associated with movement data in national database (ScotEID, or change as required)
#Can this be documented in similar Roxygen-like fashion?

#Movement identifier
move_ID <- "movement_reference" #sams_movement_reference_pigs
#Identifier of origin holding/establishment/..
origin_ID <- "departure_cph"
#Identifier of destination holding/establishment/..
dest_ID <- "dest_cph"
#Date of transport departure
dep_date <- "departure_date"
#Date of transport arrival -- need to make this optional for Danish data, can't simply copy departure datw
arr_date <- "arrival_date"
#Number of pigs transported
nr_pigs <- "qty_pigs"
