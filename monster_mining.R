#####################################
# Text Mining Code for IEEE Article #
#####################################

library(rvest)
library(tm)
library(httr)

################################################
# Scrape data for job title, location, and URL #
# from the search results page on monster      #
################################################

#Calculates the time for the entire data collection
collection_time <- system.time({

  # The range provided in the for loop determines how any pages
  # of monster results will be mined for data. In this case it
  # is set at 1:3, so it will mine the first 3 pages of monster
  # search results.
  for (i in 1:4) {

    # The URL ends with "page=", as the code below shows. This
    # allows R to use the paste() function to add new page
    # numbers to the end of this URL programatically
    monster <- read_html(paste0("https://www.monster.com/jobs/search/?q=technical-writer&intcid=skr_navigation_nhpso_searchMain&page=", i))

    # The code below is for the first page only
    if(i == 1) {

      # Collecting job title from first page
      jobTitle <- monster %>%
      html_nodes(".primary .jobTitle span") %>%
      html_text()

      # Collecting location from first page
      location <- monster %>%
      html_nodes(".job-specs-location a") %>%
      html_text()

      # Collecting URL for the job descriptions on first page
      jobUrl <- monster %>%
      html_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "primary", " " ))]//*[contains(concat( " ", @class, " " ), concat( " ", "jobTitle", " " ))]//@href') %>%
      html_text()
    }

    # The code below is for all remaining pages being mined
    else {

      # Collecting job title data and appending to the dataset
      jobTitle <- append(jobTitle, monster %>%
        html_nodes(".primary .jobTitle span") %>%
        html_text())

      # Collecting location data and appending to dataset
      location <- append(location, monster %>%
        html_nodes(".job-specs-location a") %>%
        # html_nodes(".job-specs-location a") %>%
        html_text())

      # Collecting URL data and appending to dataset
      jobUrl <- append(jobUrl, monster %>%
        html_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "primary", " " ))]//*[contains(concat( " ", @class, " " ), concat( " ", "jobTitle", " " ))]//@href') %>%
        html_text())
    }
  }
#End time calculation
})

# Removes carriage returns from location and job title data
location <- gsub("\r?\n|\r", " ", location)

# Changes data type for URLs to character
jobUrl <- as.character(jobUrl)

#################################################
# Extracting job description text from the URLs #
# listed on the monster search results page     #
#################################################

# Calculates the time it takes to extract detailed job descriptions
# from the URLs extracted from the monster search results
description_time <- system.time({

  # Loops over the URLs extracted from the monster search and then pulls the job descriptions from those URLs
  description <- c()
  for (i in 1:length(jobUrl)) {

    # Tests to see if the URL is still available--provides basic error handling for
    # deleted web pages in the monster search results
    if (http_error(jobUrl[i])) {
      description <- append(description, "bad_url")
    }

    # Checks to see if a web page is the traditional monster page,
    # or if it is an external page leaeding to a company's own
    # HR page for providing the job descriptions. If the page
    # is not a monster page, then it is coded as "non-monster-url".
    # Otherwise, the text from the job description is collected.
    else {
      d <- read_html(jobUrl[i]) %>%
      html_nodes("#JobBody .card-content") %>%
      html_text()
      if (i == 1) {
        if (length(d) == 0) {
          description <- "non_monster_url"
        }
        else {
          description <- d
        }
      }
      else {
        if (length(d) == 0) {
          description <- append(description, "non-monster-url")
        }
        else {
          description <- append(description, d)
        }
      }
    }
  }

#End time calculation
})

##################################################
# Counting the software and coding proficiencies #
# listed in the job description text             #
##################################################

skillList <- c("acrobat", "adobe creative", "robohelp", "cms", "dms", "database", "sql", "mysql", "oracle", "dita", "dreamweaver", "flash", "framemaker", "html", "css", "javascript", "jquery", "illustrator", "fireworks", "indesign", "mad cap flare", "microsoft sharepoint", "office", "photoshop", "visio", "xml")

# Sets corpus variable to the description texts pulled from URLs
corpus <- description

# Removes carrige returns from descriptions
corpus <- gsub("\r?\n|\r", " ", corpus)

# Replace punctuation with space
corpus <- gsub("[[:punct:]]", " ", corpus)

# Forces corpus to lowercase
corpus <- tolower(corpus)

# Removes any remaining whitespace
corpus <- stripWhitespace(corpus)

# this loop counts each time that a word from skillList appears in the job description
# and then add adds that data to that job description in a new column
techSkills <- c()
for (desc in 1:length(corpus)) {

  d1 <- c()

  #iterates through each job description, looking for matches to skillList terms
  for (skill in 1:length(skillList)) {

    #This checks to see if the d1 vector is empty, if so, it adds "none_listed" to the row matching that description
    if (length(d1) < 1 & skill == length(skillList)) {
      d1 <- "none_listed"
    }

    #If skill matches a term in the description, then it appends that term to new vector
    else if (grepl(skillList[skill], corpus[desc]) > 0) {
      d1 <- c(d1, paste0(skillList[skill]))
    }
  }
  d1 <- paste(d1, collapse=" ")
  techSkills <- append(techSkills, d1)
  rm(d1)
}

#Create dataset
monster <- data.frame(jobTitle, location, jobUrl, description, techSkills)

#Collection data for analysis
write.csv(monster, file="proof_data.csv")

#End time calculation
})
