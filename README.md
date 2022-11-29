# Mizzou Hair Shedding Project

The code and analyses here are associated with "Genomic loci involved in sensing environmental cues and metabolism affect seasonal coat shedding in *Bos taurus* and *Bos indicus* cattle" (Durbin et al., 2022). Private and identifiable information including pedigrees and registration numbers has been removed. Contact Harly Durbin Rowan at harlyjaned@gmail.com or Jared Decker at deckerje@missouri.edu with questions. 

## Data import, joining, and cleaning

### Phenotypic data

The code here is associated with *"Phenotypes"* heading of the Materials and methods section. 

Run `source_functions/import_joint_clean.R` to generate `data/derived_data/cleaned.rds`. 

* Takes a "blacklist" of farm ID/year combinations to ignore as arguments. I.e., to ignore PVF 2019 data and WAA 2018 data, `Rscript --vanilla source_functions/import_join_clean.R "PVF_2019" "WAA_2018"`
* Imports `source_functions/first_clean.R` to iteratively filter & tidy data on a farm-by-farm basis. 
    + `source_functions/iterative_id_search.R` used to match up animals to Lab IDs using different combinations of Mizzou Hair Shedding Project identifiers and Animal Table columns
    + When an animal matches up to more than one lab ID, keeps the most recent one (unless the most recent Lab ID comes from the summer 2020 ASA/RAAA genotype share)
    + Thompson Research Farm (farm ID UMCT) data cleaned in a different way using `data/derived_data/import_join_clean/umct_id_key.csv`
* University of Arkansas (farm IDs BAT, SAV) data cleaned separately in `source_functions/clean_arkansas.R` and imported from `data/derived_data/import_join_clean/ua_clean.Rds`
* Uses `source_functions/impute_age.R` to impute missing ages when ages in other years were recorded
    + When DOB available, "age class" is (n*365d)-90d to ((n+1)*365d)-90d, where n is the age classification and d is days. This means that animals that aren't actually one year of age can still be classified as yearlings and so on. 
    + Scores on animals less than 275 days (i.e., 365-90) old removed
* Miscellaneous data cleaning:
    + All records from animals with differing sexes across multiple years removed
    + All punctuation and spaces removed from animal IDs
    + "AMGV" prefix added to all American Gelbvieh Association registration numbers to match Animal Table
    + Coat colors codes, calving season codes, toxic fescue grazing status codes, and breed codes standardized
    
Final cleaned data stored in `data/derived_data/import_join_clean/cleaned.rds`. Anonymized version stored in `data/derived_data/data_submission/anonymized.csv`.
    
#### `cleaned.rds` and  column descriptions

* `year`: Data recording year ranging from 2012-2020
* `farm_id`: 3-digit identifier used in UMAG Tissue Table for farm or ranch where scores was collected
* `breed_code`: 2-4 digit identifier used in UMAG Animal Table, reported by breeder. One of:

| `breed_code` | Breed                         | Breed association                            |
|--------------|-------------------------------|----------------------------------------------|
| AN           | Angus                         | American Angus Association                   |
| ANR          | Red Angus                     | Red Angus Association of America             |
| BG           | Brangus, including UltraBlack | International Brangus Breeders Association   |
| CHA          | Charolais                     | American International Charolais Association |
| CHIA         | Chianina                      | American Chianina Association                |
| CROS         | Mixed/crossbred cattle        | Non-registered crossbred cattle              |
| GEL          | Gelbvieh, including Balancers | American Gelbvieh Association                |
| HFD          | Hereford                      | American Hereford Association                |
| MAAN         | Maine-Anjou                   | American Maine-Anjou Association             |
| SH           | Shorthorn                     | American Shorthorn Association               |
| SIM          | Simmental, including SimAngus | American Simmental Association               |
| SIMB         | Simbrah                       | American Simmental Association               |

* `registration_number`: Registration number associated with `breed_code`, can be `NA`
* `animal_id`: Unique identification for the animal, which must remain the same across years. Ear tag, tattoo, freeze brand, or other herd ID used when collecting the hair score. No limit on length
* `sex`: M or F
* `color`: Breeder-reported coat color. One of:

| `color`          |
|------------------|
| BLACK            |
| BLACK ROAN       |
| BLACK WHITE FACE |
| BRINDLE          |
| BROWN            |
| GREY             |
| RED              |
| RED ROAN         |
| RED WHITE FACE   |
| WHITE            |
| YELLOW           |

* `Lab_ID`: Identifier used to match animal to UMAG Animal Table
* `date_score_recorded`: Breeder-reported date when hair shedding score was collected, formatted as YYYY-MM-DD. This in NOT the date DNA sample was collected.
* `hair_score`: Integer between 1-5
* `age`: Integer between 1-21
* `calving_season`: Breeder-reported season when last calf was born. One of `SPRING` or `FALL` for females, `NA` for bulls. June 30 is the cut-off for spring and fall calving.
* `toxic_fescue`: Was the animal grazed on toxic fescue during the spring of the recording year? One of `TRUE` or `FALSE`
* `comment`: Breeder-reported comments about the animal, including additional information about breed makeup. Comments could include descriptions such as muddy, long rear hooves, lost tail switch, black hided cattle with brown backs, etc. Producers can also note any feed supplements the animals were given.
* `barcode`: Barcode of blood card, hair card, or TSU submitted for genotyping through the project
* `sold`: Has the animal been sold, died, or left the herd? Retroactively updated for previous years once breeder indicates scores will no longer be collected on the animal
* `dob`: Date of birth, formatted as YYYY-MM-DD

A high level data summary including the grouped phenotype tallies mentioned in the *"Phenotypes"* heading can be found in `html/data_summary.html`.

### Genotypic data

The code here is associated with *"Genotypes & imputation"* heading of the Materials and methods section.

* Parentage conflicts removed, genotypes QC'd, and genotypes reformatted using `source_functions/blupf90_geno_format.snakefile`
* [Link to Zenodo genotypes]

### Pedigree & pedigree-based breed composition

The code here is associated with *"Generation of the pedigree and genomic relatedness matrices"* heading of the Materials and methods section. For privacy reasons, actual pedigrees and identifiers are not included.

* Multi-breed 3-generation pedigree construction in `notebooks/3gen.Rmd` using data contributed by breed associations in `data/raw_data/3gen`
    * Parentage for genotyped animals verified using seekparentf90 in `source_functions/seekparentf90.snakefile`/`notebooks/seekparentf90.Rmd`, discordant parents set to missing in the pedigree
* Pedigree-based breed composition data for cross-bred but registered animals cleaned in `source_functions/breed_key.R` and `source_functions/rhf_breed_comp.R` using data in `data/raw_data/breed_key`, output in `data/derived_data/breed_key/breed_key.rds`

### Environmental data

The code here is associated with *"The effects of temperature and photoperiod"* heading of the Materials and methods section. For privacy reasons, the coordinates associated with each farm used to extract weather data are not included.

* Herd addresses converted to geographic coordinates in `source_functions/coord_key.R`, output in `data/derived_data/environmental_data/coord_key.csv`. 
* Weather data for unique score dates at unique coordinates gathered using DarkSky API in `source_functions/darksky.R`, output in `data/derived_data/environmental_data/weather.rds`

## Analyses

### Variance component estimation

The code here is associated with the *"Estimation of breeding values and genetic parameters"*, *"The effects of temperature and photoperiod"*, and *"Recommendations for genetic evaluations"* headings of the Materials and methods section. Descriptions of models tested and associated "model IDs" can be found in `notebooks/model_key.Rmd`, including full models, breed-specific models, and models aimed at evaluating the effect of the environment.

* AIREML variance component estimation workflow for all models in `source_functions/aireml_varcomp.snakefile`
    + Calls `source_functions/setup.aireml_varcomp.{model ID}.R` for data formatting and setup, including contemporary group assignment
* Variance components of breed-specific models analyzed in `notebooks/breeds.Rmd`
* Results of models evaluating the effects of temperature and photoperiod analyzed in `notebooks/environmental_data.Rmd` with rendered results in `html/environmental_data.html`
    + Analysis of calving season, toxic fescue grazing status, and age group BLUEs in `notebooks/misc_blues.Rmd`

### Model evaluation

The code here is associated with the *"Evaluation of breeding values"* heading of the Materials and methods section. 

* Eigensoft smartPCA workflow in `source_functions/smartpca.snakefile`. Results sanity checked in `notebooks/pca.Rmd`
* Prediction model evaluation performed using `source_functions/estimate_bias.snakefile`
* Results analyzed using `notebooks/estimate_bias.Rmd` with rendered results for each model evaluated in `html/estimate_bias.*.html`

### GWAA

#### Using de-regressed EBVs

The code here is associated with the *"Deregression of breeding values and single-SNP regression"* heading of the Materials and methods section. 

* SNP1101 setup and workflow in `source_functions/snp1101.snakefile`
    + Calls `source_functions/setup.snp1101.R` to calculate accuracy of and de-regress breeding
* Results of full models analyzed in `notebooks/general_gwas.Rmd` with rendered results in `html/general_gwas.html`
* Results of breed-specific models analyzed in `notebooks/breeds.Rmd`
    
#### Meta-analysis of year-specific GWAA

The code here is associated with the *"GWAA meta-analysis"* and *"Conditional & joint association analysis"* headings of the Materials and methods section. 

* GCTA-MLMA, COJO setup and workflow in `source_functions/gcta_gwas.years.snakefile`
* Results analyzed in `notebooks/cojo.Rmd`
* Rendered results can be found in `html/cojo.html`

#### GxE GWAA

The code here is associated with the *"Genotype-by-environment interaction GWAA"* heading of the Materials and methods section. 

* GEMMA GxE GWAS setup and workflow in `source_functions/gxe_gwas.snakefile`
    + Calls `source_functions/setup.gxe_gwas.R` to pre-adjust phenotypes by subtracting contemporary group effect estimated using the `gxe_gwas` model (see `notebooks/model_key.Rmd`) and randomly select one score per year per animal 

### Annotation and enrichment

* Annotation and enrichment analyses performed in `notebooks/annotation.Rmd`

### Miscellaneous

* Reference hair shedding score photos in `reference_photos/`