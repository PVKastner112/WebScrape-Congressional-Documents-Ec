
#Load packages
library(rvest)
library(tidyverse)
library(dplyr)
library(stringr)
library(purrr)

#1. Document IDs
#Function to extract document ID (acta number)

get_id<- function(x){
  file_page <- read_html(x)
  doc_id<- file_page %>% 
    html_nodes("span+ .field li:nth-child(2) a")%>% 
    html_text() %>% 
    paste(collapse = ",")
  return(doc_id)
}

#Get a list of document IDs for all the documents in the list
document_list<-data.frame()
for (page_result in seq(from = 1, to = 3, by=1)) {
  link<-paste0("http://archivo.asambleanacional.gob.ec/index.php/search/advanced?page=",page_result,"&f=&so0=and&sq0=%222019-2021%22&sf0=title&so1=and&sq1=%22ACTAS+ASAMBLEA+NACIONAL%22&sf1=title&limit=100&sort=alphabetic")
  webpage_code<-read_html(link)
  webpage_code %>% html_nodes(".title a")
  file_url<- webpage_code %>% 
    html_nodes(".title a") %>% 
    html_attr("href") %>% 
    paste("http://archivo.asambleanacional.gob.ec", ., sep = "")
  doc_id <- sapply(file_url, FUN = get_id, USE.NAMES = FALSE)
  document_list<- rbind(document_list, data.frame(doc_id, stringsAsFactors = FALSE))
  print(paste("Page : ", page_result))
}


#2. Download documents
#Download function + error message
download_PDFs <- function(url) {
  tryCatch(
    {
      page <- read_html(url)
      raw_list <- page %>%
        html_nodes("a") %>%
        html_attr("href") %>%
        str_subset("\\.pdf")
      
      walk2(raw_list, basename(raw_list), download.file, mode = "wb")
    },
    error = function(e) {
      message(paste("! Download from", url, "failed:", conditionMessage(e)))
    }
  )
}


# Function to get URLs for each page 
get_links <- function(pageNumber) {
  website <- paste0("http://archivo.asambleanacional.gob.ec/index.php/search/advanced?page=", pageNumber, "&f=&so0=and&sq0=%222019-2021%22&sf0=title&so1=and&sq1=%22ACTAS+ASAMBLEA+NACIONAL%22&sf1=title&limit=100&sort=alphabetic")
  website <- website %>% read_html()
  a_elements <- website %>% html_elements(css = ".title a")
  links <- a_elements %>%
    html_attr(name = "href") %>% 
    paste("http://archivo.asambleanacional.gob.ec", ., sep = "")
  
  return(links)
}

# Download documents for multiple pages
page_numbers <- 1:3
links <- map(page_numbers, getLinks)
urls <- unlist(links)

walk(urls, download_PDFs)


