melt_classes <- function(data){

    require(dplyr)

    data <- data %>%
        tidyr::gather(cancer_class, tf, biliary:cancer_multiple_or_unspecified) %>%
        filter(tf==1) %>%
        select(
            -c(
                tf,
                traits,
                MAPPED_TRAIT.y
            )
        ) %>%
        distinct(PUBMEDID, SNPS, cancer_class, .keep_all = T) %>%
        rename(
            MAPPED_TRAIT = MAPPED_TRAIT.x
        ) %>%
        arrange(cancer_class)

    return(data)

}
