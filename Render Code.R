# Code to render the .Rmd file to the readme.md
library(rmarkdown)
rmarkdown::render("~/ST558/Project_2/Project-2-558/Project 2.Rmd",
                  output_format = "github_document",
                  output_file = "README.md")


# Updated render function for automation of all 6 channel documents
apply(reports, MARGIN=1,
      FUN = function(x){
        render(input = "~/ST558/Project_2/Project-2-558/Project 2.Rmd", output_file= x[[1]], params = x[[2]])
      })
