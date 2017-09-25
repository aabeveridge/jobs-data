library(httr)
library(jsonlite)

##
## Attaching package: 'jsonlite'
##
## The following object is masked from 'package:utils':
##
##     View

library(lubridate)

options(stringsAsFactors = FALSE)

url  <- "https://api.jobs.com"
path <- "search/jobs"

raw.result <- GET(url = url, path = path)
