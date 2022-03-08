#' ---
#' title: "Read an Excel file with metadata and output templates for EMLassemblyline"
#' author: "Jim Laundre"
#' date: "2021-04-28"
#' output: html_notebook
#' ---
#' 
## ----setup, include=FALSE---------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

#' 
#' ## Required packages and functions.
#' 
## ---------------------------------------------------------------------------------------------------------------------------------
# -- REQUIRED PACKAGES -------------------------------------------------------
  # To install a local copy of EMLassemblyline if there are any code edits:
  # install.packages("~/Documents/GitHub/EMLassemblyline",
  #   repos = NULL,
  #   type = "source")
  
  library(tidyverse)
  library(lubridate)
  library (janitor)
  library(EMLassemblyline)
  
  #-- FUNCTIONS ---------------------------------------------------------------
  
  source("./R/get_excel_meta.R")
  source("./R/lookup_nfs_info.R")


#' 
#' ## Site information
#' 
#' Edit the information below to match your site information.
#' 
## ---------------------------------------------------------------------------------------------------------------------------------
  # -- Site  info ----
  
  site_name <- "ARCTIC LTER"
  site_email <- "arc_im@mbl.edu"
  user_id <- "ARC"
  user_domain <- "EDI"
  

#' 
#' ## Excel file name and define paths
#' 
#' For metadata templates, data, and EML. Two path schemes are available:
#' 
#' -   Scheme 1 - single directory for a data set.
#' -   Scheme 2 - data objects organized under a sub-directory. Within the sub-directory a template directory for each Excel file is created for the templates files.
#' 
#' Change "path_scheme" to 1 02 to match the path scheme used.
#' 
## ---------------------------------------------------------------------------------------------------------------------------------
  
  # Define paths for metadata templates, data, and EML. Edit path_scheme to 
  # choose between two different schemes. 
  path_scheme <- 2

  # -- Choose Excel File ----

  wb_full_name <- rstudioapi::selectFile()
  wb_dir <- dirname(wb_full_name)
  wb_name <- basename(wb_full_name)

  # - Path Scheme #1 organization. ----
  #   Single directory for each data package. Data package directories
  #   need to exist with the Excel and CSV files in "/data_objects".
  if (path_scheme == 1) {  
    path_data_package <- dirname(wb_dir)
    path_templates <- paste0(path_data_package, "/metadata_templates/")
    path_data <- paste0(path_data_package, "/data_objects")
    path_eml <- paste0(path_data_package, "/eml")
    
    # Create directories if they doesn't exist.
    if (!dir.exists(path_templates))
      dir.create(path_templates, recursive = T)
    if (!dir.exists(path_eml))
      dir.create(path_eml)
    
  } else {
    
    # - Path Scheme #2 organization. ----
    #    Data objects organized under sub-directories (wb_dir) with the data table
    #    file name used a sub-directory for the template files.
    path_templates <-
      paste0(wb_dir,"/",
             tools::file_path_sans_ext(wb_name),"_EAL_templates")
    path_data <- wb_dir
    path_eml <- wb_dir
    if (!dir.exists(path_templates))
      dir.create(path_templates)
  }

#' 
#' ## EMLassemblyline arguments 
#' 
#' The function "get_excel_meta", given site name, Excel file name and path, returns a named two element list ("dataset_info" and "attributes_txt"). "dataset_info" has all the information needed for EMLassemblyline::make_eml, i.e. arguments and templates (except attributes). "attributes_txt" has the data tables attribute information. The list object "EMLal_arguments*"* is populated with the required parameters needed for EMLassemblyline::make_eml.
#' 
#' Currently only one data table csv file described in the excel metadata workbook is processed. 
#' 
## ---------------------------------------------------------------------------------------------------------------------------------
  # -- Get excel metadata information ------------------------------
  #    A named two element list is returned - "dset_info"  "attributes_txt"
  #    Currently only one data table and the excel metadata workbook 
  #    are processed.  
  #    TODO extend this to several data tables and other entities
  
  excel_meta_info <- get_excel_meta(path_data,wb_name,site_name)
    
  EMLal_arguments <-  excel_meta_info$dset_info %>%
    select(
      package.id = metacat_package_id,
      year_released_to_public,
      dataset.title = dataset_title,
      pubDate = year_released_to_public,
      beginning_date,
      end_date,
      data.table = data_file_name,
      non_tabluar_files,
      files_descriptions
    ) %>%
    .[1, ] %>%
    as.list()%>%
    list_modify(
      path = path_templates,
      data.path = path_data,
      eml.path = path_eml, 
      temporal.coverage = c(.$beginning_date,.$end_date),
      data.table.name = c(str_replace(.$data.table, "\\.", "_")),
      data.table.description = c(.$dataset.title),
      data.table.url = "",
      data.table.quote.character = "\"",
      other.entity = str_trim(na.omit(c(wb_name,
                       stringr::str_split(.$non_tabluar_files,";",simplify=T)))),
      other.entity.name = str_trim(na.omit(c(str_replace(wb_name, "\\.","_"),
                            stringr::str_split(.$non_tabluar_files,";",simplify=T)))),
      other.entity.description = c(paste(
        "Excel file with metadata and data sheets:", .$dataset.title),
        na.omit(stringr::str_split(.$files_descriptions,";",simplify=T)
      )),
      other.entity.url = "",
      user.id = user_id,
      user.domain = user_domain,
    ) 
  # -- Maintenance.description--using info in 'log of changes'--------------
  #    It's multiple rows of information between maintenance_description
  #    and before name_of_data_sheet. Currently EML_assemblyline does not  
  #    support any formatting of the maintenance element.
  
  maintenance_index<-match(c("maintenance_description","name_of_data_sheet"),
                           names(excel_meta_info$dset_info))
  
  EMLal_arguments$maintenance.description <-
    excel_meta_info$dset_info[1,maintenance_index[1]:(maintenance_index[2]-1)] %>%
    .[!is.na(.)] %>%
    paste(., sep= "; ", collapse= "; ")

#' 
#' ## Personnel
#' 
#' Personnel.txt describes the personnel and funding sources involved in the creation of the data.
#' 
## ---------------------------------------------------------------------------------------------------------------------------------
  # -- Personnel.txt --------------
  #    Use information from excel file and/or look up the NSF grant titles based on   #    award numbers.
  #    TODO Need to look up information that's not in the excel file; either from 
  #      Drupal site or a look up table, e.g a txt or excel file.  Where 
  #      appropriate add that information back into the Excel file.
  
personnel <- excel_meta_info$dset_info %>%
  rename(
    givenName = first_name,
    surName = last_name,
    organizationName = organization,
    electronicMailAddress = email,
    userId = orcid_id
    ) %>%
  select(givenName,
         surName,
         organizationName,
         electronicMailAddress,
         userId,
         role) %>%
  filter(!is.na(role)) %>%
  mutate(middleInitial = NA, .after = givenName) %>%
  mutate(
    role = case_when(
      str_trim(role) == 'Owner' ~ 'creator',
      str_trim(role) == 'Associated Researcher' ~ 'associatedParty',
      TRUE ~ role
    )
  )
  # -- Project funding ------
  #    Look up NSF award information if PI name is NA
  #    If lter funding is set to Yes, the information in lter_funding data frame
  #    is added to project_funding.
  #    TO DO add a time out for slow web response when doing funding lookup.
  #          and check if the lter_funding.RDS is present 
  
project_funding <- excel_meta_info$dset_info %>%
  select(pi_first_name,
         pi_last_name,
         title_of_grant,
         funding_agency,
         funding_number) %>%
  filter(!is.na(funding_number)) %>%
  rename(
    givenName = pi_first_name,
    surName = pi_last_name,
    projectTitle = title_of_grant,
    fundingAgency = funding_agency,
    fundingNumber = funding_number
  ) %>%
  mutate(role = "PI")
# If no surname try to look up funding information from NSF site
funding_lookup <- project_funding %>%
  filter(is.na(surName)) %>%
  {
    map_df(.$fundingNumber, get_funding)
  }
# Add in any information found
if (!is_empty(funding_lookup)) {
  project_funding <- project_funding %>%
    filter(!is.na(surName)) %>%
    union(., funding_lookup) %>%
    unique()
}

if (excel_meta_info$dset_info$lter_funding_yes_no[1] == "Yes" |
    nrow(project_funding) == 0) {
  lter_funding <- readRDS("lter_funding.RDS")
  project_funding <- project_funding %>%
    union(., lter_funding) %>%
    unique()
}

if (nrow(project_funding) == 0)
  warning("No project funding!! ")

personnel.txt <- personnel %>%
  full_join(., as.data.frame(project_funding), copy = T) %>%
  select(
    givenName,
    middleInitial,
    surName,
    organizationName,
    electronicMailAddress,
    userId,
    role,
    projectTitle,
    fundingAgency,
    fundingNumber
  ) %>%  # Add creator as a contact
  add_row(filter(., .$role == "creator") %>% mutate(role = "contact")) %>%
  filter(!is.na(.$surName)) %>%  #TODO: add a warning about removing incomplete row
  add_row(
    givenName = "Information Manger",
    organizationName = site_name,
    electronicMailAddress = site_email,
    role = "contact"
  )

write.table(
  personnel.txt,
  file = paste0(path_templates, "/personnel.txt"),
  sep = "\t ",
  row.names = F,
  quote = F,
  na = ""
)


#' 
#' ## Abstract, method and protocol
#' 
#' The abstract, method and protocols text from the metadata sheet are saved as text files.
#' 
## ---------------------------------------------------------------------------------------------------------------------------------
  
  # -- Abstract, method and protocol from the text boxes--------------
  #
  write_lines(excel_meta_info$dset_info$abstract[1],
              paste0(path_templates,"/abstract.txt"))
  write_lines(excel_meta_info$dset_info$methods[1],
              paste0(path_templates,"/methods.txt"))
  write_lines(excel_meta_info$dset_info$protocol_document[1],
              paste0(path_templates,"/protocol.txt")) 

#' 
#' ## Geographic coverage
#' 
## ---------------------------------------------------------------------------------------------------------------------------------
# -- Geographic coverage ------------------------------------
geo_coverage <-  excel_meta_info$dset_info %>%
  rename(
    geographicDescription = geographic_description,
    northBoundingCoordinate = north_bounding_coordinate,
    southBoundingCoordinate = south_bounding_coordinate,
    eastBoundingCoordinate = east_bounding_coordinate,
    westBoundingCoordinate = west_bounding_coordinate
  ) %>%
  filter(!is.na(geographicDescription),!geographicDescription == "Enter Description") %>%
  mutate(
    geographicDescription = paste(location_name, geographicDescription),
    northBoundingCoordinate = coalesce(northBoundingCoordinate, latitude),
    southBoundingCoordinate = coalesce(southBoundingCoordinate, latitude),
    eastBoundingCoordinate = coalesce(eastBoundingCoordinate, longitude),
    westBoundingCoordinate = coalesce(westBoundingCoordinate, longitude)
  ) %>%
  select(
    geographicDescription,
    northBoundingCoordinate,
    southBoundingCoordinate,
    eastBoundingCoordinate,
    westBoundingCoordinate
  )
write.table(
  geo_coverage,
  paste0(path_templates, "/geographic_coverage.txt"),
  sep = "\t",
  row.names = F,
  quote = F
)

#' 
#' ## Keywords
#' 
## ---------------------------------------------------------------------------------------------------------------------------------
  # -- Keywords  --------------------------------------------
  #
keywords <- excel_meta_info$dset_info %>%
  filter(!is.na(keyword_information)) %>%
  rename(keyword = keywords, keywordThesaurus = keyword_information) %>%
  mutate(
    keywordThesaurus = case_when(
      keywordThesaurus == "LTER Keywords" ~ "https://vocab.lternet.edu/",
      keywordThesaurus == "Arctic LTER Vocabulary" ~ "Arctic LTER",
      keywordThesaurus == "Core"  ~ "https://vocab.lternet.edu/",
      TRUE ~ ""
    )
  ) %>%
  select(keyword, keywordThesaurus) %>%
  filter(!(keywordThesaurus == "")) %>%
  separate_rows(keyword, sep = ",")
write.table(
  keywords,
  paste0(path_templates, "/keywords.txt"),
  sep = "\t",
  row.names = F,
  quote = F
)

#' 
#' ## Taxonomic coverage
#' 
#' The metadata organisms studied should already have been checked using some authority. EMLassemblylin::template_taxonomic_coverage will check the names using the authority specified and output the taxonomic_coverage.txt file.
#' 
## ---------------------------------------------------------------------------------------------------------------------------------
 # -- Taxonomic coverage ----------------------------------
  #    The metadata organisms studied should already have been checked.
  #    EMLassemblylin::template_taxonomic_coverage will check the names
  #    using the authority specified, i.e.
  # id        authority                         resolve_sci_taxa resolve_comm_taxa
  # 3  Integrated Taxonomic Information System (ITIS)  supported         supported
  # 9  World Register of Marine Species (WORMS)        supported     not supported
  # 11 Global Biodiversity Information Facility (GBIF) supported     not supported
  #165 Tropicos - Missouri Botanical Garden            supported     not supported
  
  taxon_coverage <- excel_meta_info$dset_info %>% 
    filter(!is.na(organisms_studied),!grepl("Example:",organisms_studied)) %>%
    select(organisms_studied)%>%
    separate_rows(organisms_studied,sep = "[,;]")
  if(length(taxon_coverage$organisms_studied))  {
    write_csv(taxon_coverage,paste0(path_data,"/taxon.txt"))
  
    # Use EMLassemblyline to validate taxa. Outputs taxonomic_coverage.txt
    EMLassemblyline::template_taxonomic_coverage(
    path = path_templates, 
    data.path = path_data,
    taxa.table = "taxon.txt",
    taxa.col = "organisms_studied",
    taxa.name.type = "scientific",
    taxa.authority = 3)
    # Remove temporary taxon.txt file
    if (file.exists(paste0(path_data,"/taxon.txt"))) {
      #Delete file if it exists
      file.remove(paste0(path_data,"/taxon.txt"))
    }
  }

#' 
#' ## Attributes
#' 
#' Files describing the columns of a data table (classes, units, datetime formats, missing value codes) are save in the template directory. Any custom units, i.e. those not standard EML units, need to be described in ListUnitDictionary.csv.
#' 
## ---------------------------------------------------------------------------------------------------------------------------------
 # -- Attribute -----------------------------------
  #    TODO need to rewrite this for multiple data tables
  
  # Check for category variables. If there are code definitions then it's
  # a categorical class
  
cat_vars <- excel_meta_info$attributes_txt %>%
  filter(class == "categorical") %>%
  select(attributeName, code = code_info) %>%
  separate_rows(code, sep = ";") %>%
  separate(code, sep = "=", into = c("code", "definition"))

write.table(
  select(excel_meta_info$attributes,-code_info),
  paste0(
    path_templates,
    "/attributes_",
    tools::file_path_sans_ext(wb_name),
    ".txt"
  ),
  sep = "\t",
  row.names = F,
  na = "",
  quote = F,
  fileEncoding = "UTF-8"
)
write.table(
  cat_vars,
  paste0(
    path_templates,
    "/catvars_",
    tools::file_path_sans_ext(wb_name),
    ".txt"
  ),
  sep = "\t",
  row.names = F,
  na = "",
  quote = F
)

# -- Custom Units -----------------
#    TO DO Check if ListUnitDictionary.csv exist. If not then create it.
unit_lookup <- as.data.frame(read_csv("ListUnitDictionary.csv", ))
custom_units.txt <- excel_meta_info$attributes %>%
  select(unit) %>%
  distinct() %>%
  filter(!is.na(unit)) %>%
  rename(id = unit) %>%
  filter(!id %in% EML::get_unitList()$units$id) %>%
  left_join(unit_lookup, by = "id") %>%
  select(id, unitType, parentSI, multiplierToSI, description)

# Check for units not in ListUnitDictionary.csv
undefined_units <-
  anti_join(custom_units.txt, unit_lookup, by = "id")

if (!nrow(undefined_units) == 0) {
  warning("Units not in unit lookup table.")
  undefined_units
}

write.table(
  custom_units.txt,
  paste0(path_templates, "/custom_units.txt"),
  sep = "\t",
  row.names = F,
  na = "",
  quote = F
)
  

#' 
#' ## Intellectual Rights
#' 
#' Describes how the data may be used. Releasing without restriction ([CC0](https://creativecommons.org/publicdomain/zero/1.0/)) or with minimal attribution ([CC BY](https://creativecommons.org/licenses/by/4.0/)) maximizes value and future use.
#' 
## ---------------------------------------------------------------------------------------------------------------------------------
 # -- Intellectual Rights -----
  
  copy_success <- if (excel_meta_info$dset_info$intellectual_rights[1] =="CCBY") {
    file.copy( system.file(
      "/templates/intellectual_rights_ccby4.0.txt", package = "EMLassemblyline"),
      paste0(path_templates,"/intellectual_rights.txt"))
  }else{
    file.copy( system.file(
      "/templates/intellectual_rights_cc0.txt", package = "EMLassemblyline"),
      paste0(path_templates,"/intellectual_rights.txt"))    
  }
if (!copy_success)warning("File not copied. Check if it already exists..")

#' 
#' ## Data files
#' 
#' The data tables files should be saved from Excel as csv files in the data object directory. If there are spaces in the column headings (not the best practice) the column names will need to be quoted.
#' 
#' Run the following code chunk to add the quotes.
#' 
## ---------------------------------------------------------------------------------------------------------------------------------
  # -- Data file ---- 
  #    For csv files saved from excel the header row will not be quoted even if 
  #    there are spaces in the names. The following code will read the .csv file 
  #    in, add quotes and write it out. To disable this set quote_header to False
  
  quote_header <- TRUE
  if (quote_header) {
    t <- read_lines(paste0(wb_dir, "/", excel_meta_info$dset_info$data_file_name[1]))
    t[1] <- t[1] %>%
      str_replace_all("\\p{quotation mark}", replacement = "") %>%
      str_split("[\",]") %>%
      `[[`(1) %>% shQuote() %>% str_flatten(collapse = ",")
    
    write_lines(t,paste0(wb_dir, "/", excel_meta_info$dset_info$data_file_name[1]))
    rm(t)
  }

#' 
#' ## Create EML
#' 
#' Once metadata templates are complete call this function to create the EML. If the eml passes the eml can be evaluated/uploaded to EDI data portal. Note currently the data objects need to be manually specified. If there are URLs for the data objects then edit the URLs in the "Full parameter listing.."
#' 
#' #### Compact statement
#' 
## ---------------------------------------------------------------------------------------------------------------------------------
# Compact statement for creating EML
do.call(make_eml, EMLal_arguments[names(EMLal_arguments) %in% names(formals(make_eml))])

#' 
#' #### Full parameter listing of make_eml
#' 
## ----eval=FALSE-------------------------------------------------------------------------------------------------------------------
##   # -- Create EML-------
##   #    Once metadata templates are complete call this function to create
##   #    the EML.  If the eml passes the eml can be evaluated/uploaded to EDI data
##   #    portal.  Note currently the data objects need to be manually specified.
##   #    If there are URLs for them then edit the URLs below.
## 
##   EMLassemblyline::make_eml(
##     path = path_templates,
##     data.path = path_data,
##     eml.path = path_eml,
##     dataset.title = EMLal_arguments$dataset.title,
##     temporal.coverage = EMLal_arguments$temporal.coverage,
##     maintenance.description = c(EMLal_arguments$maintenance.description),
##     data.table = EMLal_arguments$data.table,
##     data.table.name = EMLal_arguments$data.table.name,
##     data.table.description = EMLal_arguments$data.table.description,
##     data.table.url = EMLal_arguments$data.table.url,
##     data.table.quote.character =EMLal_arguments$data.table.quote.character,
##     other.entity = EMLal_arguments$other.entity,
##     other.entity.name = EMLal_arguments$other.entity.name,
##     other.entity.description = EMLal_arguments$other.entity.description,
##     other.entity.url = EMLal_arguments$other.entity.url,
##     user.id = EMLal_arguments$user.id,
##     user.domain = EMLal_arguments$user.domain,
##     package.id = EMLal_arguments$package.id)

#' 
#' #### Keep Publication Data
#' 
#' If only minor changes in the metadata run the following to keep the original publishing date otherwise he publication date of the data set will be the date this script runs.
#' 
## ----eval=FALSE-------------------------------------------------------------------------------------------------------------------
##    y<-EML::read_eml(paste0(path_eml,"/",EMLal_arguments$package.id,".xml"))
##    y[["dataset"]][["pubDate"]] <- EMLal_arguments$pubDate
##    EML::write_eml(y,paste0(path_eml,"/",EMLal_arguments$package.id,".xml"))
## 

