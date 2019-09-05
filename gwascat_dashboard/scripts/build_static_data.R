library(dplyr)

# Open SQL connection to ensembl database
edb <- EnsDb.Hsapiens.v79::EnsDb.Hsapiens.v79

# Get Human chromosome lengths, write to CSV
ch_len <- ensembldb::select(
        edb,
        keys = ensembldb::keys(edb, keytype = "SEQNAME"),
        columns = c("SEQLENGTH","SEQNAME"),
        keytype = "SEQNAME"
    ) %>%
    filter(
        stringr::str_detect(SEQNAME, "^[0-9]+|^X$|^Y$")
    ) %>%
    arrange(
        SEQNAME
    ) %>%
    readr::write_csv("data/chromosome_lengths.csv")

# Get cancer classes, write to CSV
cancer_classes <- readr::read_csv("data/classified_gwas_catalog.csv") %>%
    count(cancer_class) %>%
    rename("value" = cancer_class, "frequency" = n) %>%
    filter(value != "total") %>%
    mutate(
        label = case_when(
            stringr::str_detect(value, "^cancer") ~ "Unspecified/Multiple",
            stringr::str_detect(value, "^skin") ~ "Skin (Non-Melanoma)",
            stringr::str_detect(value, "^head") ~ "Head/Neck",
            stringr::str_detect(value, "^multiple") ~ "Multiple Myeloma",
            stringr::str_detect(value, "^cns") ~ "CNS",
            TRUE ~ stringr::str_to_title(value)
        )
    ) %>%
    arrange(label)

    readr::write_csv(cancer_classes, "data/cancer_class_list.csv")
