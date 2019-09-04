# This script assumes project data is stored on the same level as common project functions
refresh_gwas_dataset <- function(){

    if(!dir.exists("data")){dir.create("data")}

    this_months_gwas_datafile <- grep(pattern = paste0('gwas_data_',
                                                       substr(Sys.Date(), start = 1, stop = 8),
                                                       '*'),
                                      list.files("data"),
                                      value = T)

    if(length(this_months_gwas_datafile)>=1){

        print('Reading data file...')

        gwas_data <- readr::read_csv(
            file.path(
                "data",
                grep(pattern = paste0('gwas_data_',
                                      substr(Sys.Date(), start = 1, stop = 8),
                                      '*'),
                     rev(list.files("data")),
                     value = T)
            )
        )
    } else {
        source("scripts/functions/get_current_gwas_data.R")
        print('Downloading gwas catalog...')
        gwas_data <- get_current_gwasdata()
    }

    return(gwas_data)

}
