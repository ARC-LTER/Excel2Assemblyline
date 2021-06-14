#' @title Find the NFS Award title and PI given a award number. 
#' Requires rvest, stringr
#' @param National Science Foundation award number. 
#' returns a tibble 
#
library(rvest)
library(stringr)

get_funding <- function(awardnumber) {
  awardurl <-
    paste0("https://nsf.gov/awardsearch/showAward?AWD_ID=",
           awardnumber)
  awardpage <- read_html(awardurl)
  title <- awardpage %>%
    rvest::html_nodes(".pageheadline") %>%
    rvest::html_text(trim = T) %>%
    .[1]
  pi_name <- awardpage %>%
    rvest::html_nodes(".tabletext2") %>%
    rvest::html_nodes("li")%>%
    rvest::html_text(trim = T) %>%
    grep("(Principal Investigator)", ., value = T, fixed = T) %>%
    stringr::word(., 1:2,sep = "[\n \t]+") %>%
    str_trim()
  givenName <- pi_name[1]
  surName <- pi_name[2]
  role <- "PI"
  fundingAgency <- if (!is.na(surName)) {
    "NSF"
  } else {
    NA
  }
  return(as_tibble(
    list(
      givenName = givenName,
      surName = surName,
      role = role,
      projectTitle = title,
      fundingAgency = fundingAgency,
      fundingNumber = awardnumber
    )
  ))
}
