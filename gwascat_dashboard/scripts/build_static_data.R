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
        stringr::str_detect(SEQNAME, "^[0-9]+|^X$|^Y$|^MT$")
    ) %>%
    mutate(
        SEQNAME = factor(SEQNAME, levels = c(seq(1,22), c("X", "Y","MT")))
    ) %>%
    arrange(
        SEQNAME
    ) %>%
    readr::write_csv("data/chromosome_lengths.csv")

# Get cancer classes, write to CSV
cancer_classes <- readr::read_csv("data/classified_gwas_catalog.csv") %>%
    count(cancer_class) %>%
    rename("value" = cancer_class, "frequency" = n)

cancer_classes %>%
    filter(value != "total") %>%
    readr::write_csv("data/cancer_class_list.csv")
