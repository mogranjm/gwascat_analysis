library(dplyr)
library(xml2)
library(readr)

# Define project directories
projroot <- rprojroot::find_rstudio_root_file()
funcdir <- file.path(projroot, "functions")
datadir <- file.path(projroot, "data")

# Read cancer data
if(!exists("gwas_classified")){
    gwas_classified <- read_csv(file.path(datadir, "gwas_cancer_classifications.csv"))
}

# Get custom NCBI EUtil API functions
api_key <- "d96bb10ffdca4cfb6ad3c963ba2e9f155d09"
source(file.path(funcdir, "ncbi_eutil_functions.R"))

# Extract PUBMEDIDs for each paper to query NCBI
ids <- gwas_classified %>%
    distinct(PUBMEDID)

id_str <- ids %>%
    pull() %>%
    paste(collapse = ",")

# Construct query URL using NCBI EUtil API and query pubmed for publication data
url <- eutil_fetch_url_generator("pubmed", id_str, rettype = "xml", api_key = api_key)

xml <- read_xml(url)

# Extract DOI and Abstract text from query result
ids$doi <- xml_find_all(xml, '//ArticleId')[xml_attr(xml_find_all(xml, '//ArticleId'), "IdType")=="doi"] %>%
    xml_text

ids$abstract <- xml %>%
    xml_find_all("//Abstract") %>%
    xml_text() %>%
    trimws()

# Return extracted publication data to the original dataset and write to file.
gwas_classified <- gwas_classified %>%
    left_join(ids) %>%
    write_csv(file.path(projroot, "data/gwas_cancer_classifications.csv"))

rm(api_key)
rm(ids)
rm(id_str)
rm(url)
rm(xml)
rm(eutil_fetch_url_generator)
rm(eutil_link_url_generator)
