#' @title Read excel metadata file and parse metadata.
#' 

#


get_excel_meta <- function(path_data=path_data,wb_name=wb_name, site_name) {
  
  library(openxlsx)
  library(tidyverse)
  
  #-- FUNCTIONS ---------------------------------------------------------------
  # Functions for excel to data frames
  
  # Transpose a data frame (df) using the first column as names. Requires tidyr
  #
  transpose_df <- function(df) {
    df %>%
      pivot_longer(-1) %>%
      pivot_wider(
        names_from = 1,
        names_repair = "unique",
        values_from = 3
      )
  }
  
  # Extract text box names and text from an Excel file. Must be .xlsx
  # The file is renamed and unzipped. xml2 package is used to read the file
  # and extract the data.
  #
  get_excel_textboxs <- function(file_dir, excel_file_name)
  {
    
    library(stringr)
    library(xml2)
    
    dir.create(tmp <- tempfile())
    unzip(zipfile = paste0(file_dir, "\\", excel_file_name),
          exdir = tmp)
    drawing_files <- list.files(
      path = paste0(tmp, "\\xl\\drawings\\"),
      pattern = "\\.xml",
      full.names = T
    )
    t_boxes <- map(drawing_files, function (y) {
      read_xml(y) %>%
        xml_find_all(., ".//xdr:twoCellAnchor", xml_ns(.)) %>%
        map_dfr(., function(x) {
          tbox_name <-
            xml_find_all(x, ".//xdr:cNvPr", xml_ns(x)) %>% xml_attr(., "name")
          tbox_body <-
            xml_find_all(x, ".//xdr:txBody", xml_ns(x)) %>%
            xml_children(.) %>%
            #xml_find_all(., ".//a:endParaRPr", xml_ns(.)) %>%   # find the paragraphs breaks
            xml_text(., trim = T) %>%
            paste(., collapse = "\n\n") # paste to one string separated by 2 returns
                                        # which make_eml.R parses into a <para>
          tibble(tbox_name = tbox_name, tbox_body = tbox_body)
        })
    })
    t_boxes <- do.call(rbind, lapply(t_boxes, data.frame))
    
    #' Clean up tmp files
    unlink(tmp, recursive = T)
    return(t_boxes)
  }
  
  #-- Get workbook and read in the Metadata work sheet-------------------------
  #   The metadata sheet should have the word "metadata" in the first cell, e.g. 
  #   "Metadata Template - ARCTIC LTER". This identifies the correct worksheet.
  
  wb <- paste0(path_data,"/",wb_name)
  wb_sheets <- getSheetNames(wb)

  for (i in wb_sheets) {
    cell1 <-grepl("metadata",read.xlsx(wb,sheet = i,colNames = F,rows=1, cols = 1)[1], ignore.case = T)
    if (length(cell1)==0) cell1 <-FALSE
    if (cell1) {
      meta_df <- as_tibble(read.xlsx(wb, sheet = i, detectDates = F)) 
     break
    }
  }
  #meta_df <- as_tibble(read.xlsx(wb, sheet = results[1], detectDates = F))
  test <-  tryCatch (  # Test if a sheet was found
    !nrow(meta_df),
    error = function(e) {
      print("No MetaData Sheet found! If metadata sheet is present check site name.")
    }
  )
  
  #-- Create row index from first column for referencing the info.-----
  # TO DO: Do checks on the metadata information.  Currently an excel macro is used
  #        to check variable names, units, sites and personnel
  # 
  names(meta_df)[1] <- "meta_variables"
  row_index <- rowid_to_column(meta_df)  %>%
    select(meta_variables, rowid) %>%
    filter(!is.na(.[1])) %>%
    transpose_df() %>%
    clean_names()
  
  
  # -- Data set information from MetaData sheet----------------
  #' 
  #'   The first column of meta_df is used as column names when
  #'   transposing the data frame. Note that NA in the the first column need to 
  #'   be replaced with a unique value for the transposed column names.
  #'   row_index data frame is used for selecting the rows to extract.
  #'   Dates are not detected. Using openxlsx convert_to_date. Need to check 
  #'   if this works for Mac excel
   
  
  dset_info <- meta_df %>%
    mutate(meta_variables = if_else(
      is.na(meta_variables),
      paste0("row_", row_number()),
      meta_variables
    )) %>%
    transpose_df() %>% 
    clean_names() %>%
    mutate(
      beginning_date = convert_to_date(beginning_date),
      end_date = convert_to_date(end_date))
  
  # -- Abstract, method and protocol from the text boxes--------------
  # Uses custom function: get_excel_textboxs
  
  text_boxes <- get_excel_textboxs(path_data, wb_name) %>%
    filter(tbox_name %in% c("abstract", "method", "protocol1")) %>%
    transpose_df()
  
  dset_info[1,] <- dset_info[1,] %>% 
    mutate(abstract = text_boxes$abstract,
           methods = text_boxes$method,
           protocol_document = text_boxes$protocol1)
  
  dset_info[1,] <- dset_info[1,] %>% 
    mutate(abstract = text_boxes$abstract,
           methods = text_boxes$method,
           protocol_document = paste(url_of_online_protocol,
                                     text_boxes$protocol1, sep = " "))
  
  
  # -- Get attributes ------
  #    TODO need to rewrite this for multiple data tables
  
  attrib_txt_names <-c("attributeName","attributeDefinition","class","unit",
                       "dateTimeFormatString","code_info","missingValueCode",
                       "missingValueCodeExplanation")
  
  attributes <- tail(meta_df, -row_index$variable_name) %>%
    filter(!is.na(.$meta_variables)) %>%
    select(1:8) %>%
    set_names(attrib_txt_names) %>%
    mutate(
      attributeName = str_trim(attributeName),
      class = case_when(
        str_trim(class) == 'datetime' ~ 'Date',
        str_trim(class) == 'text' ~ 'character',
        str_trim(class) == 'number' ~ 'numeric',
        TRUE ~ class
      ),
      class = ifelse(is.na(code_info),class,"categorical" )
    ) %>%
    separate(missingValueCode,
             c("missingValueCode", "missingValueCodeExplanation"),
             sep = "[=|]") %>%
    mutate(
      attributeDefinition =
        stringr::str_replace_all(attributeDefinition,"[\r\n\t]", " ")
    )
  # The above str_replace_all() will remove any line endings within the definition  
  # since fread in template_arguments is used with quotes = "" and will separated 
  # the text. Any extra tabs will confuse read_tbl() since is expects the fields 
  # to be tab delimited. Error msg: "enc2utf8(d[, k]) : argument is not a character vector"
  
  return(list(dset_info = dset_info,attributes_txt = attributes))
  
}
