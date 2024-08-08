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