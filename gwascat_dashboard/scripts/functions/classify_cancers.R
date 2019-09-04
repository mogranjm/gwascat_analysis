
check_classifications <- function(data){

    require(dplyr)

    unclassified <- data %>%
        filter(total < 1)

    if (nrow(unclassified) > 1){

        assign("unclassified_records", unclassified, envir = .GlobalEnv)

        stop(nrow, "unclassified records detected, saved in 'unclassified_records'")
    }

    print("No unclassified records")
}

classify_cancers <- function(data){

    require(dplyr)
    require(stringr)

    exclude <- c(
        "response to",
        "exposure",
        "progression",
        "interaction",
        "measurement",
        "hypersensitivity",
        "hormone replacement therapy",
        "childhood",
        "induced",
        "sclerosing cholangitis",
        "infection",
        "survival",
        "adenoma",
        "smoking",
        "gastroesophageal reflux disease",
        "neuropathic pain",
        "non-alcoholic steatohepatitis",
        "primary ovarian insufficiency",
        "pain",
        "radiation"
    ) %>%
        paste(collapse="|")

    h_n <- c(
        "lung",
        "neck",
        "geal"
    ) %>%
    paste(collapse="|")

    gwas_cancers <- data %>%
    # gwas_cancers <- cancers %>%
        select(PUBMEDID, DISEASE.TRAIT, MAPPED_TRAIT) %>%
        mutate(
            traits =             tolower(MAPPED_TRAIT)# all lowercase,
        ) %>%
        filter(!str_detect(traits, exclude),
               !str_detect(tolower(DISEASE.TRAIT), exclude)) %>%
        mutate(
            biliary =            if_else(str_detect(traits, "gall"), TRUE, FALSE),
            bladder =            if_else(str_detect(traits, "(^| )bladder"), TRUE, FALSE),
            blood   =            if_else(str_detect(traits, "leuk|lymphom|waldenstrom|b-cell|myeloma"), TRUE, FALSE),
            # leukemia =           if_else(str_detect(traits, "leukemia"), TRUE, FALSE),
            # lymphoma =           if_else(str_detect(traits, "lymphom|waldenstrom|b-cell"), TRUE, FALSE),
            # multiple_myeloma =   if_else(str_detect(traits, "myeloma"), TRUE, FALSE),
            breast =             if_else(str_detect(traits, "breast"), TRUE, FALSE),
            cervical =           if_else(str_detect(traits, "cervical"), TRUE, FALSE),
            cns =                if_else(str_detect(traits, "glio|mening|central nervous system"), TRUE, FALSE),
            colorectal =         if_else(str_detect(traits, "colo"), TRUE, FALSE),
            endometrial =        if_else(str_detect(traits, "endometria"), TRUE, FALSE),
            gastric =            if_else(str_detect(traits, "gastric"), TRUE, FALSE),
            # head_neck =          if_else(str_detect(traits, "oral cavity|aryn|aerodigest"), TRUE, FALSE),
            # oesophageal =        if_else(str_detect(traits, "esoph"), TRUE, FALSE),
            head_neck =          if_else(
                                     str_detect(traits, "oral cavity|aryn|aerodigest|esoph") | (str_detect(traits, "squamous") & (str_detect(traits, h_n) | str_detect(tolower(DISEASE.TRAIT), h_n))),
                                     TRUE, FALSE
                                     ),
            kidney =             if_else(str_detect(traits, "renal"), TRUE, FALSE),
            liver =              if_else(str_detect(traits, "hepato"), TRUE, FALSE),
            # lung =               if_else(str_detect(traits, "lung"), TRUE, FALSE),
            lung =               if_else(
                                     str_detect(traits, "lung") & !str_detect(traits, "squamous"),
                                     TRUE, FALSE
                                     ),
            melanoma =           if_else(str_detect(traits, "(^|^-|\\s)melanoma"), TRUE, FALSE),
            neuroblastoma =      if_else(str_detect(traits, "neuroblastoma"), TRUE, FALSE),
            ovarian =            if_else(str_detect(traits, "ovar"), TRUE, FALSE),
            pancreatic =         if_else(str_detect(traits, "pancrea"), TRUE, FALSE),
            prostate =           if_else(str_detect(traits, "prostate"), TRUE, FALSE),
            sarcoma =            if_else(str_detect(traits, "sarcoma"), TRUE, FALSE),
            # skin_non_melanoma =  if_else(str_detect(traits, "skin|basal|keratinocyte|squamous"), TRUE, FALSE),
            skin_non_melanoma =  if_else(
                                     str_detect(traits, "skin|basal|keratinocyte|squamous") & !str_detect(traits, h_n) & !str_detect(tolower(DISEASE.TRAIT), h_n),
                                     TRUE, FALSE
                                     ),
            thyroid =            if_else(str_detect(traits, "thyroid"), TRUE, FALSE),
            testicular =         if_else(str_detect(traits, "testic"), TRUE, FALSE),
        ) %>%
        mutate(
            total =              rowSums(.[5:26]),
            cancer_multiple_or_unspecified = if_else(total>1, TRUE, FALSE)
        ) %>%
        mutate( # Exclude anything marked as being associated with multiple or unspecified cancer type as not being associated with a specific type
            biliary =            if_else(cancer_multiple_or_unspecified == TRUE, FALSE, biliary),
            bladder =            if_else(cancer_multiple_or_unspecified == TRUE, FALSE, bladder),
            blood   =            if_else(cancer_multiple_or_unspecified == TRUE, FALSE, blood),
            # leukemia =           if_else(cancer_multiple_or_unspecified == TRUE, FALSE, leukemia),
            # lymphoma =           if_else(cancer_multiple_or_unspecified == TRUE, FALSE, lymphoma),
            # multiple_myeloma =   if_else(cancer_multiple_or_unspecified == TRUE, FALSE, multiple_myeloma),
            breast =             if_else(cancer_multiple_or_unspecified == TRUE, FALSE, breast),
            cervical =           if_else(cancer_multiple_or_unspecified == TRUE, FALSE, cervical),
            cns =                if_else(cancer_multiple_or_unspecified == TRUE, FALSE, cns),
            colorectal =         if_else(cancer_multiple_or_unspecified == TRUE, FALSE, colorectal),
            endometrial =        if_else(cancer_multiple_or_unspecified == TRUE, FALSE, endometrial),
            gastric =            if_else(cancer_multiple_or_unspecified == TRUE, FALSE, gastric),
            head_neck =          if_else(cancer_multiple_or_unspecified == TRUE, FALSE, head_neck),
            # oesophageal =        if_else(cancer_multiple_or_unspecified == TRUE, FALSE, oesophageal),
            kidney =             if_else(cancer_multiple_or_unspecified == TRUE, FALSE, kidney),
            liver =              if_else(cancer_multiple_or_unspecified == TRUE, FALSE, liver),
            lung =               if_else(cancer_multiple_or_unspecified == TRUE, FALSE, lung),
            melanoma =           if_else(cancer_multiple_or_unspecified == TRUE, FALSE, melanoma),
            neuroblastoma =      if_else(cancer_multiple_or_unspecified == TRUE, FALSE, neuroblastoma),
            ovarian =            if_else(cancer_multiple_or_unspecified == TRUE, FALSE, ovarian),
            pancreatic =         if_else(cancer_multiple_or_unspecified == TRUE, FALSE, pancreatic),
            prostate =           if_else(cancer_multiple_or_unspecified == TRUE, FALSE, prostate),
            sarcoma =            if_else(cancer_multiple_or_unspecified == TRUE, FALSE, sarcoma),
            skin_non_melanoma =  if_else(cancer_multiple_or_unspecified == TRUE, FALSE, skin_non_melanoma),
            thyroid =            if_else(cancer_multiple_or_unspecified == TRUE, FALSE, thyroid),
        testicular =         if_else(cancer_multiple_or_unspecified == TRUE, FALSE, testicular),
        )

    check_classifications(gwas_cancers)

    return(gwas_cancers)

}

