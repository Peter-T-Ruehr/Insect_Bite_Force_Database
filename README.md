# R Code for the creation of the insect bite force database

This repository contains the R code that was used to create the tables and figures of the article describing the insect bite force database published under

Rühr et al. (**in review**): A bite force database of 654 insect species.

## Instructions
1. Open the R project 'Bite_Force_Database.Rproj'. We have used RStudio (1) to run R (2)
2. Open the R script 'Bite_Force_Database.R'
3. Run the script

The script contains comments that explain the code step-by-step.

The Zenodo folder that is automatically downloaded by lines 58 ff. is available at [doi.org/10.5281/zenodo.5782923](https://www.doi.org/10.5281/zenodo.5782923). Note that the Zenodo data must be unzipped after download so that all files are located in the same folder as the 'Bite_Force_Database.Rproj' file.

During review,

We provided example code for those steps that have been performed once and need manual input from the user. Howeverm such code is commented out, so that the code results in identical tables and figures as published in the article.

The Zenodo repository already contains all files that are produced by this script (mainly PDFs with plotted figures and the final database Excel sheet 'iBite_table.xlsx'). Running this script will overwrite some of those files. However, the file content will be identical.

## References
(1) https://www.rstudio.com/
(2) R Core Team. R: A language and environment for statistical computing. R Foundation for Statistical Computing. (R Foundation for Statistical Computing, 2021).
