## Data and code for "No support for honest signalling of male quality in zebra finch song"

When using this content **PLEASE CITE** the paper (link will be added soon)) and [this repository](https://github.com/MartinBulla/rebuttal_alam_2024).


[Data](Data/):
- [Fig_4c.csv](Data/Fig_4c.csv): source data provided by [Alam et al. (2024, *Nature*)](https://doi.org/10.1038/s41586-024-07207-4)
- [rebuttal_fig_2.csv](Data/rebuttal_fig_2.csv): source data provided by [Alam et al. (2024, *Nature*)](https://doi.org/10.1038/s41586-024-07207-4) and data measured by us from their Extended Data Fig. 6 (green versus black bars during playback)

[R](R/)-scripts used in the analysis:
- [Fig_point_1.r](R/Fig_point_1.r) uses [Fig_4c.csv](Data/Fig_4c.csv) and generates our [Fig. 1](Output/Fig_point_1.png); columns: *tutor* - unique tutor ID, *pupil* - unique pupil ID for the given tutor, *tutor_path* - song path length of the tutor, *pupil_path* - song path length of the pupil
- [Fig_point_2.r](R/Fig_point_2.r) uses [rebuttal_fig_2.csv](Data/rebuttal_fig_2.csv) and generates our [Fig. 2](Output/Fig_point_2.png); columns: *trial_id* - unique trial ID, *side_bias* - was the long-path song during the trial played in the *preferred arm* or *non-preferred arm* by the female during the baseline period?, *song_pair* - unique song-pair ID; *long* - path length of the long path song, *short* - path length of the short path song, *pre* - the percentage of pre-trial time spent in the arm where long path song would be played during the trial, *trial* -  the percentage of trial time spent in the arm with long path song, *post* - the percentage of post-trial time spent in the arm where long path song was played during the trial, *trial_long* - trial time spent in the arm with long path song, *trial_short* - trial time spent in the arm with short path song

[Output](Output/): contains the generated figures in PNG format

[LICENSE](LICENSE): terms of reuse

***

### Session Information
*The figures were generated using the below indicated version of R and its related packages, running on the below indicated macOS. The installation of R and related R-packages takes only a few mintues. How to install R and R-packages is described [here](https://rstudio-education.github.io/hopr/starting.html) and [here](https://rstudio-education.github.io/hopr/packages2.html#installing-packages)*

**R version 4.2.0** (2022-04-22)  
Platform: x86_64-apple-darwin17.0 (64-bit)  
Running under: **macOS Mojave 10.14.6**  

**Matrix products:** default  
**BLAS:   /Library/Frameworks/R.framework/Versions/4.2/Resources/lib/libRblas.0.dylib**  
**LAPACK:** /Library/Frameworks/R.framework/Versions/4.2/Resources/lib/libRlapack.dylib  

**locale:** en_GB.UTF-8/en_GB.UTF-8/en_GB.UTF-8/C/en_GB.UTF-8/en_GB.UTF-8

**attached base packages:** grid, stats, graphics, grDevices, utils, datasets, methods, base     

**other attached packages:**
patchwork_1.1.3, ggpubr_0.6.0, ggpmisc_0.5.4-1, ggpp_0.5.5, ggplot2_3.5.1, data.table_1.14.8

**loaded via a namespace (and not attached):** pillar_1.9.0, compiler_4.2.0, tools_4.2.0, lifecycle_1.0.4, tibble_3.2.1, gtable_0.3.4, lattice_0.22-5, pkgconfig_2.0.3, rlang_1.1.2, Matrix_1.6-2, cli_3.6.1, polynom_1.4-1, SparseM_1.81, withr_2.5.2, dplyr_1.1.3, generics_0.1.3, vctrs_0.6.4, MatrixModels_0.5-3, tidyselect_1.2.0, glue_1.6.2, R6_2.5.1, rstatix_0.7.2, fansi_1.0.5, survival_3.5-7, carData_3.0-5, car_3.1-2, tidyr_1.3.0, purrr_1.0.2, magrittr_2.0.3, backports_1.4.1, scales_1.3.0, MASS_7.3-60, splines_4.2.0, abind_1.4-5, colorspace_2.1-0, ggsignif_0.6.4    

### Demo & Reproduction Instructions
We do not provide any demo data, because the two *csv* files used in the analyses are small and already contain all the data used in the analyses. 

To generate Fig. 1 and/or Fig. 2, download the [two csv files](Data/) into *Data* folder of your project's root directory and create a folder *Output*. Open *R* software and the *Fig_point_1.r* or  *Fig_point_2.r* R-script, install the R-packages indicated under the tools by running *install.packages(c('data.table','ggplot2','ggpmisc','ggpubr','grid','patchwork')* and run the script. The pngs of the figures will be generated in *Output*.