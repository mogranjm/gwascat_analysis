extract_cancer_data <- function(data, new_varname='gwas_cancer_data'){
	require(dplyr)
	require(stringr)

	cancer_data <- data %>%
		dplyr::filter(
			stringr::str_detect(DISEASE.TRAIT, stringr::fixed("cancer", ignore_case = TRUE)) | stringr::str_detect(DISEASE.TRAIT, "oma[^\\w]"),
			!stringr::str_detect(DISEASE.TRAIT, stringr::fixed("glaucoma", ignore_case = TRUE)),
			!stringr::str_detect(DISEASE.TRAIT, stringr::fixed("tripanosoma", ignore_case = TRUE)),
			!stringr::str_detect(DISEASE.TRAIT, stringr::fixed("hematoma", ignore_case = TRUE))
		) %>%
		dplyr::arrange(dplyr::desc(PUBMEDID))

	assign(new_varname, cancer_data, envir=.GlobalEnv)
}

# #### Positive RegEx matches (include)
# * "cancer"
# + Match the pattern "cancer"
# * "oma[^\\w]"
# + Match the pattern "oma" only where it is not followed by a "word character" (i.e. ASCII letter, digit or underscore). Effectively matches any word with the suffix "oma"
# + Included: Melan**oma**, Carcin**oma**, etc...
# + Not included: Chr**oma**tosis, Bi**oma**rker, S**oma**tic
# + **NOTE:** *Need to validate that no cancer records are excluded*
# 	```{r RegEx include}
# data <- gwas_data %>%
# 	as_tibble() %>%
# 	filter(
# 		str_detect(DISEASE.TRAIT, fixed("cancer", ignore_case = TRUE)) | str_detect(DISEASE.TRAIT, "oma[^\\w]")
# 	)
# ```
#
# #### Negative RegEx matches (exclude)
# * "glaucoma"
# + Exclude the pattern "glaucoma"
# + Glaucoma is a degenerative eye syndrome characterised by vision loss as a result of damage to the optic nerve (not cancer).
# * "tripanosoma"
# + Exclude the pattern "tripanosoma"
# + Tripanosoma is a genus of parasitic protozoa (not cancer).
# * "hematoma"
# + Exclude the pattern "hematoma"
# + A hematoma is an abnormal collection of blood  external to the circulatory system (a more serious form of bruise) caused by the leakage of blood from a *large* blood vessel (not cancer).
#
# ```{r RegEx exclude}
# data <- filter(
# 	data,
# 	!str_detect(DISEASE.TRAIT, fixed("glaucoma", ignore_case = TRUE)),
# 	!str_detect(DISEASE.TRAIT, fixed("tripanosoma", ignore_case = TRUE)),
# 	!str_detect(DISEASE.TRAIT, fixed("hematoma", ignore_case = TRUE))
# ) %>%
# 	arrange(desc(PUBMEDID))
# ```
