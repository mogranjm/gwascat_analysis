library(gwascat)
library(dplyr)
library(stringr)

# Define project directories
projroot  <- rprojroot::find_rstudio_root_file()
funcdir   <- file.path(projroot, "functions")
datadir   <- file.path(projroot, "data")
reportdir <- file.path(projroot, "reporting")

# Get live GWAS data
source(file.path(funcdir, "refresh_gwas_dataset.R"), chdir = TRUE)

## Structure
# The GWAS Catalog is a large dataset (134,314x38) with records representing disease associated SNPs identified through Genome Wide Association Studies. Each table row represents a Single Nucleotide Polymorphism associated with a given disease trait. Lots of papers report multiple SNPs, so each paper tends to be replicated in a many-to-one table relationship style.
#
# Each record contains information about the SNP of interest:
#
# * Details of the publication in which it is reported
#     + Study Title,
#     + PubMed Id,    + Date Published,
#     + First Author,
#     + Journal me,
#     + NCBI Link,
# * Details of the condition it is associated with
#      + DISEASETRAIT,
#     + Risk allele, [SNP_ ID]-[Risk_Associated_Nucleotide]
#     + Associated Risk (Odds Ratio or Beta coefficient, including p-value & 95% CI)
#     + Sample Size, etc...
# * Details of the. genetic locus at which it resides
#     + SNP ID,
#     + Chromosome,
#     + Genomic Region, (p/q notation),
#     + Chromosomal Position, (Nucleotide index),
#     + Related Genes,
#     + Variant Context, (SNP Functional class - exon/missense/non-coding/etc)
#     + Platform (Genotyping platformI manufacturer used in Stage 1)
#
# ### Reducing the dataset
# First we need to reduce the full GWAS Catalog [data dictionary here](https://www.ebi.ac.uk/gwas/d.  ocs/fileheaders) down to include only cancer related records.
#
# Once this is done, each record will be assessed and categorised based on a thematic analysis of the study title.
#
# The intent here is to isolate only those records that have some kind of relevance to cancer. This will be done by filtering for cancer-like keywords in the DISEASE.TRAIT field.
#
# #### Positive RegEx matches (include)
# * "cancer"
#     + Match the pattern "cancer"
# * "oma([^\\w]|$)"
#     + Match the pattern "oma" only where it is not followed by a "word character" (i.e. ASCII letter, digit or underscore) OR where it is followed by a line ending. Effectively matches any word with the suffix "oma"
#     + Included: Melan**oma**, Carcin**oma**, etc...
#     + Not included: Chr**oma**tosis, Bi**oma**rker, S**oma**tic
#     + **NOTE:** *Need to validate that no cancer records are excluded*
#
# #### Negative RegEx matches (exclude)
# * "glaucoma"
#     + Exclude the pattern "glaucoma"
#     + Glaucoma is a degenerative eye syndrome characterised by vision loss as a result of damage to the optic nerve (not cancer).
# * "tripanosoma"
#     + Exclude the pattern "tripanosoma"
#     + Tripanosoma is a genus of parasitic protozoa (not cancer).
# * "hematoma"
#     + Exclude the pattern "hematoma"
#     + A hematoma is an abnormal collection of blood  external to the circulatory system (a more serious form of bruise) caused by the leakage of blood from a *large* blood vessel (not cancer).

# Extract cancer publications & SNPs from GWAS catalog
source(file.path(funcdir, "select_gwas_cancer.R"))
extract_cancer_data(gwas_data)

# Prepare titles for classification
cancer_classes <- gwas_cancer_data %>%
	dplyr::select(PUBMEDID, DISEASE.TRAIT) %>%
	mutate(
	    traits = tolower(DISEASE.TRAIT),                                 # all lowercase,
	    biliary =            if_else(str_detect(traits, "cholangio|gall"), TRUE, FALSE),     # exclude cholangiocarcinomas reported as a side effect of PSC
	    bladder =            if_else(str_detect(traits, "(^| )bladder"), TRUE, FALSE),
	    cns =                if_else(str_detect(traits, "glio|mening"), TRUE, FALSE),
	    breast =             if_else(str_detect(traits, "breast"), TRUE, FALSE),
	    cervical =           if_else(str_detect(traits, "cervical"), TRUE, FALSE),
	    colorectal =         if_else(str_detect(traits, "colo"), TRUE, FALSE),
	    endometrial =        if_else(str_detect(traits, "endometri"), TRUE, FALSE),
	    oesophageal =        if_else(str_detect(traits, "esoph"), TRUE, FALSE),
	    gastric =            if_else(str_detect(traits, "gastric"), TRUE, FALSE),
	    head_neck =          if_else(str_detect(traits, "oral cavity|aryn"), TRUE, FALSE),
	    kidney =             if_else(str_detect(traits, "renal"), TRUE, FALSE),
	    leukemia =           if_else(str_detect(traits, "leukemia"), TRUE, FALSE),
	    liver =              if_else(str_detect(traits, "hepato"), TRUE, FALSE),
	    lung =               if_else(str_detect(traits, "lung"), TRUE, FALSE),
	    lymphoma =           if_else(str_detect(traits, "lymphom"), TRUE, FALSE),
	    melanoma =           if_else(str_detect(traits, "(^|^-|\\s)melanoma"), TRUE, FALSE),
	    multiple_myeloma =   if_else(str_detect(traits, "myeloma"), TRUE, FALSE),
	    neuroblastoma =      if_else(str_detect(traits, "neuroblastoma"), TRUE, FALSE),
	    ovarian =            if_else(str_detect(traits, "ovarian"), TRUE, FALSE),
	    pancreatic =         if_else(str_detect(traits, "pancrea"), TRUE, FALSE),
	    prostate =           if_else(str_detect(traits, "prostate"), TRUE, FALSE),
	    sarcoma =            if_else(str_detect(traits, "sarcoma"), TRUE, FALSE),
	    skin_non_melanoma =  if_else(str_detect(traits, "skin|basal|keratinocyte|squamous"), TRUE, FALSE),
	    thyroid =            if_else(str_detect(traits, "thyroid"), TRUE, FALSE),
	    testicular =         if_else(str_detect(traits, "testic"), TRUE, FALSE),
	    benign =             if_else(str_detect(traits, "adenoma"), TRUE, FALSE),
	    cancer_unspecified = if_else(str_detect(traits, "^cancer|adenocarcinoma"), TRUE, FALSE)
	) %>%
    filter(!str_detect(traits, "body mass index|obesity|response|insulinoma|survival|pain|menopause|toxicity")) # Remove 28044437 BMI change in cancer patients

# Check whether any publications are not classified
unclassified <- cancer_classes %>%
    filter(
        benign == FALSE,
        biliary == FALSE,
        bladder == FALSE,
        cns == FALSE,
        breast == FALSE,
        cervical == FALSE,
        colorectal == FALSE,
        endometrial == FALSE,
        oesophageal == FALSE,
        gastric == FALSE,
        head_neck == FALSE,
        kidney == FALSE,
        leukemia == FALSE,
        liver == FALSE,
        lung == FALSE,
        lymphoma == FALSE,
        melanoma == FALSE,
        multiple_myeloma == FALSE,
        neuroblastoma == FALSE,
        ovarian == FALSE,
        pancreatic == FALSE,
        prostate == FALSE,
        sarcoma == FALSE,
        skin_non_melanoma == FALSE,
        thyroid == FALSE,
        testicular == FALSE,
        cancer_unspecified == FALSE,
    ) %>%
    distinct(DISEASE.TRAIT, .keep_all = TRUE)

if(nrow(unclassified)>=1){
    stop("There are unclassified GWAS Disease Traits")
} else {
    rm(unclassified)
    print("All GWAS Disease Traits classified")
}

# Generate empty df to store identified SNP records}
selected <- matrix(ncol=length(colnames(cancer_classes)),nrow=0) %>%
    as.data.frame() %>%
    setNames(colnames(cancer_classes))


    # Biliary tract (bile duct/gallbladder) SNPs}
    selected <- cancer_classes %>%
    	filter(
    	    biliary == TRUE,
    	    !str_detect(traits, "cholangitis"),
    	) %>%
        rbind(selected)

    # Bladder SNPs
    selected <- cancer_classes %>%
    	filter(
    	    bladder == TRUE,
     		!str_detect(traits, "smoking") # exclude publications on smoking interaction - one pub: 24662972
     	) %>%
        rbind(selected)

    # Breast SNPs
    chemotherapy <- "anastrozole|taxane|paclitaxel|trastuzumab|alopecia|bevacizumab"
    	# anastrozole - one publication:  30648747 (phamacokinetics/pharmacogenomics - SNP encodes anastrozole influx transporter)
    	# taxane      - one publication:  29278617
    	# paclitaxel  - two publications: 28763429, 24025145 (adverse effects: alopecia/change in LVEF)
    	# trastuzumab - one publication:  28763429 (adverse effects: change in LVEF)

    hormone_therapy <- "tamoxifen|hormone therapy"
    	# tamoxifen   - two publications: 30161160, 22180457

    radiation <- "childhood"
    	# childhood   - one publication:  29059430 (studied effect of chest-directed radiotherapy on female survivors of childhood cancer

    survival_time <- "free"
    	# free        - two publications: 27758888, 25867717 (Cancer-free interval post treatment)

    tumour_biochem <- "estradiol|estrone|androstene"
        # estradiol 23518928
    	# estrone     - one publication:  28429243 (hormone levels in resected early-stage tumours)
    	# androstene  - one publication:  28429243

    selected <- cancer_classes %>%
        filter(
            breast == TRUE,
            !str_detect(traits, "mortality"),          # exclude publications targeting post-diagnosis event (mortality)     - one publication:   30787463
            !str_detect(traits, chemotherapy),         # exclude publications targeting treatment response (chemotherapies)  - four publications: see exclusion terms
            !str_detect(traits, hormone_therapy),      # exclude publications targeting treatment response (hormone therapy) - two publications:  see exclusion terms
            !str_detect(traits, radiation),            # exclude publications targeting treatment exposure (radiation)       - one publication :  see exclusion terms
            !str_detect(traits, survival_time),        # exclude publications characterising recurrent disease               - two publications:  see exclusion terms
            !str_detect(traits, tumour_biochem),       # exclude publications characterising tumour environment              - one publication :  see exclusion terms
            !str_detect(traits, "survival|prognosis"), # exclude publications predicting post-diagnosis survival             - six publications:  29423119, 25964295, 25890600, 25867717, 25526632, 22232737, 23319801
        ) %>%
        rbind(selected)

    rm(
        "chemotherapy",
        "hormone_therapy",
        "radiation",
        "survival_time",
        "tumour_biochem"
       )

    # Cervical SNPs
    selected <- cancer_classes %>%
    	filter(
    	    cervical == TRUE,
    	    !str_detect(traits, "chemotherapy"),        # exclude publications targeting treatment response - one publication: 28120872
    	) %>%
        rbind(selected)

    # CNS (brain) SNPs
    selected <- cancer_classes %>%
    	filter(
    	    cns == TRUE,
     	 ) %>%
        rbind(selected)

    # Colorectal SNPs
    survival <- "metastasis|survival"
    	# metastasis - one publication:    30738427
    	# survival   - three publications: 26586795, 26222057, 25866641

    selected <- cancer_classes %>%
        filter(
            colorectal == TRUE,
            !str_detect(traits, survival),            # exclude publications targeting time to non-diagnosis event        - three publications: see exclusion terms
            !str_detect(traits, "toxicity"),          # exclude publications targeting adverse treatment response         - one publications:   30557370
            !str_detect(traits, "adenoma"),           # exclude publications targeting benign traits                      - one publication:    29228715
            !str_detect(traits, "cholangitis"),       # exclude publications targeting disease interaction                - one publication:    28779025
            !str_detect(traits, "interaction"),     # exclude publications targeting exposure factors                   - one publication:    27723779, 26766742, 25781442, 24743840
        ) %>%
        rbind(selected)

    rm("survival")

    # Endometrial SNPs
    selected <- cancer_classes %>%
        filter(
            endometrial == TRUE,
        ) %>%
        rbind(selected)

    # Gastric SNPs
    selected <- cancer_classes %>%
        filter(
            gastric == TRUE,
        ) %>%
        rbind(selected)

    # Head/neck SNPs
    selected <- cancer_classes %>%
        filter(
            head_neck == TRUE,
            !str_detect(traits, "radiation") # exclude adverse treatment effects - 1 pub 30299488
        ) %>%
        rbind(selected)

    # Kidney SNPs
    selected <- cancer_classes %>%
    	filter(
    	    kidney == TRUE,
     	) %>%
        rbind(selected)

    # Leukemia (blood) SNPs
    selected <- cancer_classes %>%
        filter(
            leukemia == TRUE,
            !str_detect(traits, "response|therapy|thiopurine|asparaginase|vincristine|methotrexate|kinetics")
        ) %>%
        rbind(selected)

    # Liver SNPs
    selected <- cancer_classes %>%
    	filter(
    	    liver == TRUE,
    	    !str_detect(traits, "response|survival|hepatitis")
    	) %>%
        rbind(selected)

    # Lung SNPs
    selected <- cancer_classes %>%
    	filter(
    	    lung == TRUE,
    	    !str_detect(traits, "smoker|interaction|platinum|irinotecan|survival|recurrence")
     	) %>%
        rbind(selected)

    # Lymphoma SNPs
    selected <- cancer_classes %>%
    	filter(
    	    lymphoma == TRUE,
    	    !str_detect(traits, "survival")
    	) %>%
        rbind(selected)

    # Multiple myeloma (blood) SNPs
    selected <- cancer_classes %>%
    	filter(
    	    multiple_myeloma == TRUE,
    	    !str_detect(traits, "survival|infection|neuropathy"),        # exclude publications targeting myeloma-secondary event - three publications: 28120872, 29594489, 28317148
    	) %>%
        rbind(selected)

    # Melanoma SNPs
    selected <- cancer_classes %>%
    	filter(
    	    melanoma == TRUE,
    	    # !str_detect(traits, "survival|infection|neuropathy"),        # exclude publications targeting myeloma-secondary event - three publications: 28120872, 29594489, 28317148
    	) %>%
        rbind(selected)

    # Neuroblastoma SNPs
    selected <- cancer_classes %>%
    	filter(
    	    neuroblastoma == TRUE,
     	) %>%
        rbind(selected)

    # Osophageal SNPs
    selected <- cancer_classes %>%
        filter(
            oesophageal == TRUE,
            !str_detect(traits, "interaction|survival")
        ) %>%
        rbind(selected)

    # Ovarian SNPs
    chemotherapy <- "carboplatin|paclitaxel"
    	# carboplatin - three publications 29367611 (pharmacokinetics), 28935272 (progression-free survival), 28935272 (treatment response)
    	# paclitaxel  - three publications 29367611 (pharmacokinetics), 28935272 (progression-free survival), 28935272 (treatment response)

    selected <- cancer_classes %>%
    	filter(
    	    ovarian == TRUE,
    	    !str_detect(traits, chemotherapy),        # exclude publications targeting treatment response         - one publication:  see exclusion list
    	) %>%
        rbind(selected)

    rm("chemotherapy")

    # Pancreatic SNPs
    selected <- cancer_classes %>%
    	filter(
    	    pancreatic == TRUE,
    	    !str_detect(traits, "gemcitabine|survival|pancreatitis")
     	) %>%
        rbind(selected)

    # Prostate SNPs
    prognosis <- "aggressiveness|survival"
    	# aggressiveness - one publication: 25939597
    	# survival       - one publication: 26307654

    treatment <- "treatment|docetaxel|radiotherapy"
    	# treatment  - one publication: 20932654
     	# docetaxel  - one publication: 27143689
        # radiotherapy - three publications: 27515689, 24974847, 23376709

     selected <- cancer_classes %>%
    	filter(
    	    prostate == TRUE,
    	    !str_detect(traits, 'benign'),          # exclude publications targeting benign traits             - one publication:    30410027
    	    !str_detect(traits, treatment),         # exclude publications targeting adverse treatment effects - five publications:    see exclusion terms
    	    !str_detect(traits, prognosis),         # exclude publications targeting prognosis                 - two publications:   see exclusion terms
    	) %>%
        rbind(selected)

    rm("prognosis",
       "treatment")

    # Sarcoma SNPs
    selected <- cancer_classes %>%
    	filter(
    	    sarcoma == TRUE,
    	    !str_detect(traits, "survival|metastasis")
     	) %>%
        rbind(selected)

    # Skin SNPs
    selected <- cancer_classes %>%
    	filter(
    	    skin_non_melanoma == TRUE,
    	    !str_detect(traits, "toxicity|pain|survival")
     	) %>%
        rbind(selected)

    # Thyroid SNPs
    selected <- cancer_classes %>%
    	filter(
    	    thyroid == TRUE,
     		!str_detect(traits, "radiation"), # exclude publications with additional exposure factors - one publication: 20350937
     	) %>%
        rbind(selected)

    # Testicular SNPs
    selected <- cancer_classes %>%
    	filter(
    	    testicular == TRUE,
     	) %>%
        rbind(selected)

    #"Cancer" SNPs}
    selected <- cancer_classes %>%
    	filter(
    	    cancer_unspecified == TRUE,
     	) %>%
        rbind(selected)

# Join classifications to main cancer dataset
gwas_classified <- selected %>%
	inner_join(gwas_cancer_data, by = c("PUBMEDID","DISEASE.TRAIT")) %>%
	mutate(
	    biliary = if_else(is.na(biliary), FALSE, biliary),
	    bladder = if_else(is.na(bladder), FALSE, bladder),
	    cns = if_else(is.na(cns), FALSE, cns),
	    breast = if_else(is.na(breast), FALSE, breast),
	    cervical = if_else(is.na(cervical), FALSE, cervical),
	    colorectal = if_else(is.na(colorectal), FALSE, colorectal),
	    endometrial = if_else(is.na(endometrial), FALSE, endometrial),
	    oesophageal = if_else(is.na(oesophageal), FALSE, oesophageal),
	    gastric = if_else(is.na(gastric), FALSE, gastric),
	    head_neck = if_else(is.na(head_neck), FALSE, head_neck),
	    kidney = if_else(is.na(kidney), FALSE, kidney),
	    leukemia = if_else(is.na(leukemia), FALSE, leukemia),
	    liver = if_else(is.na(liver), FALSE, liver),
	    lung = if_else(is.na(lung), FALSE, lung),
	    lymphoma = if_else(is.na(lymphoma), FALSE, lymphoma),
	    melanoma = if_else(is.na(melanoma), FALSE, melanoma),
	    multiple_myeloma = if_else(is.na(multiple_myeloma), FALSE, multiple_myeloma),
	    neuroblastoma = if_else(is.na(neuroblastoma), FALSE, neuroblastoma),
	    ovarian = if_else(is.na(ovarian), FALSE, ovarian),
	    pancreatic = if_else(is.na(pancreatic), FALSE, pancreatic),
	    prostate = if_else(is.na(prostate), FALSE, prostate),
	    sarcoma = if_else(is.na(sarcoma), FALSE, sarcoma),
	    skin_non_melanoma = if_else(is.na(skin_non_melanoma), FALSE, skin_non_melanoma),
	    thyroid = if_else(is.na(thyroid), FALSE, thyroid),
	    testicular = if_else(is.na(testicular), FALSE, testicular),
	    cancer_unspecified = if_else(is.na(cancer_unspecified), FALSE, cancer_unspecified),
	) %>%
    distinct(PUBMEDID, SNPS,
             biliary,
             bladder,
             breast,
             cervical,
             cns,
             colorectal,
             endometrial,
             gastric,
             head_neck,
             kidney,
             leukemia,
             liver,
             lung,
             lymphoma,
             melanoma,
             multiple_myeloma,
             neuroblastoma,
             oesophageal,
             ovarian,
             pancreatic,
             prostate,
             sarcoma,
             skin_non_melanoma,
             thyroid,
             testicular,
             cancer_unspecified,
             .keep_all = TRUE) %>%
	arrange(PUBMEDID)

# Clean and export classified dataset
gwas_classified <- gwas_classified %>%
    tidyr::gather(cancer_class, tf, biliary:cancer_unspecified) %>%
    filter(tf==1) %>%
    dplyr::select(-c(tf, traits, start:strand)) %>%
    arrange(cancer_class) %>%
    write_csv(file.path(projroot, "data/gwas_cancer_classifications.csv"))

rm(this_months_gwas_datafile)
rm(gwas_data)
rm(gwas_cancer_data)
rm(cancer_classes)
rm(selected)
rm(extract_cancer_data)
rm(get_current_gwasdata)
