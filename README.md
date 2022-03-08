# Excel2Assemblyline

R program to create template files for [EDIorg/EMLAssemblyline](https://github.com/EDIorg/EMLAssemblyline) from an Excel metadata template workbook (MetadataBlank.xlsx).

Using the Excel metadata template workbook, researchers can easily create metadata to publish data online.  The template workbook features one metadata worksheet for documenting a dataset and a sheet for data.  The metadata worksheet has extensive comments and dropdown lists to facilitate filling in required metadata.  Once completed, the R script *excel_2_EMLassemblyline.Rmd* builds the require templates and run EMLAssemblyline::make_eml to create an EML file. This file can then be uploaded to the [Environmental Data Initiative (EDI)](https://environmentaldatainitiative.org/edi/) data repository.

In using EDI's  [EMLAssemblyline](https://github.com/EDIorg/EMLAssemblyline) to create EML metadata this R script leverages an R program curated at the network level.

An overview and tutorial of the Excel Metadata sheet is forthcoming.



### Features

- Excel2Assemblyline uses an R notebook (Excel2Assemblyline.Rmd) format.  The notebook format uses code chucks which can to be run individually or run all at once. Currently the html file created does not make full use of R notebook capabilities. A notebook could be tailor to produce a formatted report on the data set.

- Choose between two different data package organization schemes.  

  - Scheme 1 - single directory for a data set, e.g. 2012-2017MAT89lgcover, with sub-directories as follows
    - **data_object**s A directory of data and other digital objects to be packaged (e.g. data files, scripts, .zip files, etc.).
    - **metadata_templates** A directory of EAL template files.
    - **eml** A directory of EML files created by EAL.
  - Scheme 2 - data objects organized under a single directory. Within this directory a template directory for each Excel file is created for the template files. This data object directory can be part of tree of project directories.

- Do an online searched for National Science Foundation (NSF) award numbers to lookup authors and titles

- Use EMLassemblylin::template_taxonomic_coverage to check the taxon names using a specified authority.

- Calls EMLassemblyline::make_eml to create and validate the EML file.

  

### Usage

- Download or clone the excel_2_Assemblyline files to a directory. 
- Edit the Excel metadata workbook replacing the site name and the research sites in the **DropDownLists(Do NOT Edit)** worksheet with your research sites' information.
- Open Excel2Assemblyline.Rproj and then Excel2Assemblyline.Rmd.
- Edit the Excel2Assemblyline.Rmd file sections: **Site Information** and **Keywords**, replacing site specific information.
- Decide on organization scheme.
- Fill out the Excel metadata template file. For an example open 
- data/terrestrial/2008_2010-2020MAT06lgcover.xls]
- Save the data as a csv file using the name entered in the metadata sheet.
- Run each chunk sequentially or all at once.

Addition comments are in the code and can be viewed as an html document - excel_2_EMLassemblyline.nb.html.

