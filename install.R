install.packages('RSQLite')
install.packages('DBI')
install.packages('ggplot2')
install.packages('DT')
install.packages('janitor')
install.packages('plotly')
install.packages('readr')
install.packages('dplyr')
install.packages('lubridate')
require(devtools)
install_version("rmarkdown", version = "2.13", repos = "https://cran.r-project.org")
install_version("tidyverse", version = "1.3.1", repos = "https://cran.r-project.org")
update.packages(ask = FALSE, checkBuilt = TRUE)
packages = c("tidyverse","readr","ggplot2","dplyr","lubridate","plotly","DT","janitor","htmlTable","kableExtra","questionr","wrapr","zoo")
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)
