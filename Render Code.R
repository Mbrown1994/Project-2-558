# Code to render the .Rmd file to the readme.md
library(rmarkdown)
rmarkdown::render("~/Desktop/R Programming/iCloud Drive (Archive)/Project-2-558/Project-2-558/Project 2.Rmd",
                  output_format = "github_document",
                  output_file = "README.md")


## Automation

# Removing Null values
channelIDs <- c("Entertainment", "Business", "Tech", "Lifestyle", "World", "Social Media")

# Create file names
output_file <- paste0(channelIDs, ".md")

# Commented out for right now
# Create a list for each channel with just the channel name
params= lapply(channelIDs, FUN = function(x){list(channel=x)})  

# Put into a data frame  
reports <- tibble(output_file, params)  

# Rename channelid columns to params  
colnames(reports) <- c("output_file", "params")


# Updated render function for automation of all 6 channel documents
apply(reports, MARGIN=1,
      FUN = function(x){
        render(input = "Project2.Rmd", output_file= x[[1]], params = x[[2]])
      })

