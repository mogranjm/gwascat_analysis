get_current_gwasdata <- function(){

    require(dplyr)

    current_gwas_data <- gwascat::makeCurrentGwascat() %>%
        as_tibble()

    current_gwas_data %>%
        readr::write_csv(paste0('data/gwas_data_', Sys.Date(), '.csv')) %>%
        return()
}
