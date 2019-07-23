eutil_fetch_url_generator <- function(db, ids, rettype = NULL, retmode = NULL, api_key = NULL){

	# URL Generator for the NCBI entrez programming utility API

	# Args:
	# db - Name of Ensembl Database to be queried
	# eg:PubMed, PMC
	# ids - A single string containing a comma separated list of identifiers used to query the selected database
	# retmode - "Return mode" Format for the data query to be returned
	# rettype - "Return type"

	# Example output:
	# https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&id=25824743&retmode=xml
	# Returns an XML page containing data for the PubMed record with PUBMEDID #25824743

	# The base URL for the NCBI eutil tool
	url <- 'https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?'

	# Configure Database to query based on declared variable 'db'
	db_string <- paste0('db=', db)

	# Configure API query
	# Database ID string
	id_string <- paste0('&id=', ids)

	# API Return type
	if(!is.null(rettype)){
		rettype_string <- paste0('&rettype=', rettype)
	} else {
		rettype_string <- ""
	}

	# API Return mode
	if(!is.null(retmode)){
		retmode_string <- paste0('&retmode=', retmode)
	} else {
		retmode_string <- ""
	}

	# API Key
	if(!is.null(api_key)){
		api_string <- paste0('&api_key=', api_key)
	} else {
		print("Warning, no API key provided")
		api_string <- ""
	}

	# Concatenate API Query strings into URL
	GET_url <- paste0(url, db_string, id_string, rettype_string, retmode_string, api_string)

	return(GET_url)
}


eutil_link_url_generator <- function(eutil, dbfrom, db, ids){
	# URL Generator for the NCBI entrez programming utility API

	# Args:
	# dbfrom - Name of Ensembl Database to be referred to
	# db - Name of Ensembl Database to be queried
	# PubMed
	# ids - A single string containing a comma separated list of primary keys of records in the dbfrom database

	# Example output:
	# https://eutils.ncbi.nlm.nih.gov/entrez/eutils/elink.fcgi?dbfrom=pubmed&db=pmc&id=25824743
	# Returns an XML page containing PMC data related to the PubMed record with PUBMEDID #25824743

	# The base URL for the NCBI eutil tool
	url <- 'https://eutils.ncbi.nlm.nih.gov/entrez/eutils/elink.fcgi?'

	# Configure Database relationships
	# Define reference database
	dbfrom_string <- paste0('dbfrom=', dbfrom)

	# Define request database
	db_string <- paste0('&db=', db)

	# Configure reference database IDs string
	id_string <- paste0('&id=', ids)

	# Concatenate API Query strings into URL
	GET_url <- paste0(url, dbfrom_string, db_string, id_string)

	return(GET_url)
}

