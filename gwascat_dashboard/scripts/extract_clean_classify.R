library(dplyr)

source('scripts/functions/refresh_gwas_dataset.R')
source('scripts/functions/extract_cancer_data.R')
source('scripts/functions/classify_cancers.R')
source('scripts/functions/filtering_join.R')
source('scripts/functions/melt_classes.R')

gwas <- refresh_gwas_dataset() #%>%

cancers <- gwas %>%
    extract_cancer_data() %>%
    classify_cancers()

final <- gwas %>%
    filtering_join(cancers) %>%
    melt_classes()

readr::write_csv(final, "data/classified_gwas_catalog.csv")
