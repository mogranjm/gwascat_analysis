source('functions/get_current_gwas_data.R')

reticulate::use_condaenv('gwas-biocenv')

this_months_gwas_datafile <- grep(pattern = paste0('gwas_data_', substr(Sys.Date(), start = 1, stop = 8), '*'),
								  list.files('data/'), value=TRUE)

if(length(this_months_gwas_datafile)>=1){

	print('Reading data file...')
	gwas_data <- suppressMessages(
		readr::read_csv(
			paste0('data/',
				   base::grep(paste0('gwas_data_',
				   				  substr(Sys.Date(), start = 1, stop = 7), '*'),
				   		   list.files('data/'), value=TRUE)))
	)
} else {
	print('Downloading gwas catalog...')
	get_current_gwasdata()
}

print('Data stored in variable: gwas_data')
