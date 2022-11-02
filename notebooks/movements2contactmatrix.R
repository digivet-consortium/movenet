load_all()
library(tidyverse)

movement_datafile <-
  "tests/testthat/test_input_files/sample_pigs_UK_with_dep_arr_dates.csv"
contactpars_outfile <-
  "tests/testthat/test_input_files/contact_matrix.rds"

anonymisation_m <-
  movement_datafile |>
  reformat_data("movement") |>
  anonymise("")

contact_matrix <-
  anonymisation_m$data %>%
    group_by(departure_cph, dest_cph) %>%
    summarise(freq=n()/365) %>%
    ungroup() %>%
    arrange(as.numeric(dest_cph)) %>%
    pivot_wider(names_from = dest_cph,
                values_from = freq) %>%
    arrange(as.numeric(departure_cph)) %>%
    select(-departure_cph)

saveRDS(contact_matrix,
        contactpars_outfile)
