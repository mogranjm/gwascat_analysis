filtering_join <- function(primary_set, filter_set){

    require(dplyr)

    data <- primary_set %>%
        inner_join(filter_set, by = c("PUBMEDID","DISEASE.TRAIT")) %>%
        mutate(
            biliary           = if_else(is.na(biliary), FALSE, biliary),
            bladder           = if_else(is.na(bladder), FALSE, bladder),
            blood             = if_else(is.na(blood), FALSE, blood),
            # leukemia          = if_else(is.na(leukemia), FALSE, leukemia),
            # lymphoma          = if_else(is.na(lymphoma), FALSE, lymphoma),
            # multiple_myeloma  = if_else(is.na(multiple_myeloma), FALSE, multiple_myeloma),
            breast            = if_else(is.na(breast), FALSE, breast),
            cervical          = if_else(is.na(cervical), FALSE, cervical),
            cns               = if_else(is.na(cns), FALSE, cns),
            colorectal        = if_else(is.na(colorectal), FALSE, colorectal),
            endometrial       = if_else(is.na(endometrial), FALSE, endometrial),
            gastric           = if_else(is.na(gastric), FALSE, gastric),
            head_neck         = if_else(is.na(head_neck), FALSE, head_neck),
            # oesophageal       = if_else(is.na(oesophageal), FALSE, oesophageal),
            kidney            = if_else(is.na(kidney), FALSE, kidney),
            liver             = if_else(is.na(liver), FALSE, liver),
            lung              = if_else(is.na(lung), FALSE, lung),
            melanoma          = if_else(is.na(melanoma), FALSE, melanoma),
            neuroblastoma     = if_else(is.na(neuroblastoma), FALSE, neuroblastoma),
            ovarian           = if_else(is.na(ovarian), FALSE, ovarian),
            pancreatic        = if_else(is.na(pancreatic), FALSE, pancreatic),
            prostate          = if_else(is.na(prostate), FALSE, prostate),
            sarcoma           = if_else(is.na(sarcoma), FALSE, sarcoma),
            skin_non_melanoma = if_else(is.na(skin_non_melanoma), FALSE, skin_non_melanoma),
            thyroid           = if_else(is.na(thyroid), FALSE, thyroid),
            testicular        = if_else(is.na(testicular), FALSE, testicular),
            cancer_multiple_or_unspecified = if_else(is.na(cancer_multiple_or_unspecified), FALSE, cancer_multiple_or_unspecified),
        ) %>%
        distinct(PUBMEDID, SNPS,
                 biliary,
                 bladder,
                 blood,
                 # leukemia,
                 # lymphoma,
                 # multiple_myeloma,
                 breast,
                 cervical,
                 cns,
                 colorectal,
                 endometrial,
                 gastric,
                 head_neck,
                 # oesophageal,
                 kidney,
                 liver,
                 lung,
                 melanoma,
                 neuroblastoma,
                 ovarian,
                 pancreatic,
                 prostate,
                 sarcoma,
                 skin_non_melanoma,
                 thyroid,
                 testicular,
                 cancer_multiple_or_unspecified,
                 .keep_all = TRUE) %>%
        arrange(PUBMEDID)

    return(data)
}
