Prior to executing the write_up.Rmd file, preproc1.R, preproc2.R and models.R have to be executed in this respective order.

preproc1.R and preproc2.R load the data into R, parse column names, perform some preprocessing steps (detailed in the script) and save the resulting files.  

models.R builds random forest and generalized boosting models (using gbm_grid.R).



