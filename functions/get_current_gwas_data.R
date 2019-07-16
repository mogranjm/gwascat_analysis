get_current_gwasdata <- function(new_varname='gwas_data'){
	require(dplyr)
	require(readr)
	require(gwascat)

	download_date <- Sys.Date()
	current_gwas_data <- gwascat::makeCurrentGwascat() %>%
			as_tibble()


	current_gwas_data %>%
		readr::write_csv(paste0(datadir, 'gwas_data_', download_date, '.csv'))

	assign(new_varname, current_gwas_data, envir=.GlobalEnv)
}
