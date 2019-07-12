
| Cancer Stream | Publications                                          | SNPs                                            |
	|-----          |-----                                                  |-----                                            |
	| Blood         | `r blood_pubs %>% distinct(PUBMEDID) %>% nrow()`      | `r blood_pubs %>% distinct(SNPS) %>% nrow()`    |
	| Breast        | `r breast_pubs %>% distinct(PUBMEDID) %>% nrow()`     | `r breast_pubs %>% distinct(SNPS) %>% nrow()`   |
	| Cervical      | `r cervical_pubs %>% distinct(PUBMEDID) %>% nrow()`   | `r cervical_pubs %>% distinct(SNPS) %>% nrow()` |
	| Colorectal    | `r crc_pubs %>% distinct(PUBMEDID) %>% nrow()`        | `r crc_pubs %>% distinct(SNPS) %>% nrow()`      |
	| Ovarian       | `r ovarian_pubs %>% distinct(PUBMEDID) %>% nrow()`    | `r ovarian_pubs %>% distinct(SNPS) %>% nrow()`  |
	| Prostate      | `r prostate_pubs %>% distinct(PUBMEDID) %>% nrow()`   | `r prostate_pubs %>% distinct(SNPS) %>% nrow()` |
	| Thyroid       | `r thyroid_pubs %>% distinct(PUBMEDID) %>% nrow()`    | `r thyroid_pubs %>% distinct(SNPS) %>% nrow()`  |
