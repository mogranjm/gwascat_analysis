extract_cancer_data <- function(data){

    require(dplyr)
    require(stringr)

    cancer_data <- data %>%
        filter(
            str_detect(DISEASE.TRAIT, fixed("cancer", ignore_case = TRUE)) | str_detect(DISEASE.TRAIT, "oma([^\\w]|$)|leukemia"),
            !str_detect(DISEASE.TRAIT, fixed("glaucoma", ignore_case = TRUE)),
            !str_detect(DISEASE.TRAIT, fixed("tripanosoma", ignore_case = TRUE)),
            !str_detect(DISEASE.TRAIT, fixed("hematoma", ignore_case = TRUE))
        ) %>%
        arrange(desc(PUBMEDID))

    return(cancer_data)

}
