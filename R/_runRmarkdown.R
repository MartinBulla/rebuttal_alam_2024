require(rmarkdown)

rmarkdown::render('R/SI.R', output_dir = 'Output',  output_file = 'SI.html'); file.copy(from = 'Output/SI.html', to = 'index.html', overwrite = TRUE)


rmarkdown::render('R/rev_Point_2.R', output_dir = 'Output',  output_file = 'Rev_Point_2.html')
rmarkdown::render('R/rev_ED_Fig_1_kdistances.R', output_dir = 'Output',  output_file = 'Rev_ED_Fig_1_kdistance.html')
rmarkdown::render('R/rev_ED_Fig_1.R', output_dir = 'Output',  output_file = 'Rev_ED_Fig_1_distance.html')