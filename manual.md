## Manual

This script was created for use at the UNC Metabolomics and Proteomics Core to converts output from proteomics software into an Excel (.xlsx) format for our collaborators. 

Currently, there are 3 options for table conversion, Maxquant results that are further processed with Perseus **(MQ-Perseus)**, **Byos**, and **Spectronaut**.

### MQ-Perseus
The MQ-Perseus option is specifically designed for AP-MS projects. It combines the input of unimputed and imputed data, from Persues exported as text files. The final Excel spreadsheet consists of a "Proteins" tab that contains all identified (unimputed) proteins. The second tab contains proteins that were first, filtered (usually for 50% quant values or a fraction of quant values per condition) then imputed protein results. Subsequent tabs are created, one for each comparison performed in Perseus (t-test), which is then filtered for a p-value < 0.05. There is an option to filter the comparison tabs by both p-value and a log2FC cutoff of +/- 1. The comparison tabs are sorted by log2FC, while the "Proteins" and "Imputed" tabs are sorted by summed intensity column (calculated from the non-log2 abundance values).


Required columns:  "Majority protein IDs", "Protein names", "Fasta headers", "Gene names", "Gene name", "Potential contaminant", "Peptides", "Razor + unique peptides", "Unique peptides"


### Byos
The Byos converter is used for creating Excel spreadsheets for intact ZipChip results. At the core these are mostly single proteins or antibodies that collaborators would like to confirm the mass of or identify modifications on. 
This option is designed to read in a multi-tabbed Excel spreadsheet, containing the exported data table of individual samples from Byos. This converter requires Mass and Intesity columns along with the expected mass. The expected mass can be passed to R by either typing them in under the "Optional" box, each separated by only a comma. Each sample/tab should have a single expected mass associated with it. The other option is to use an Excel spreadsheet, where the first column is the sammple name and the second column is the expected mass, this is best used for projects with many samples. These expected masses are used to calulcated the delta mass. The script does this by substracting the identified mass from the expected, and from the Byos caluclated expected mass. Due to differences in how the collaborator and Byos calucaltes the mass of protein these values are often different. 
For QC of intact ZipChip data we use the NIST mAB standard. We provide the results of this standard for each project we do. The first tab must be the table from the NIST mAB results, as this is treated differently than the actual samples. Each tab should be labeled with the sample information or unique identifier. The output is an Excel spreadsheet with the expected masses and delta expected masses from both the user provided and byos provided values. Along with the intensity and local relative intensity. 


Required columns: "Name", "Mass", "Expected mass"(only for NIST), and "Intensity"


### Spectronaut
Spectronaut exports the quantitative and statistical values in separate files. This converter is used to combine them into a single Excel (xlsx) spreadsheet. 
The Spectronaut converter reads in the tsv output Report and Candidate spreadsheets, merges the two, and then creates additional tabs for each comparison. The script can process both Protein and PTM related data. The SpN .rs schema files used to export the data are included in the repo. The comparison groups can be choosen once the candidate file is uploaded. There is also an option to upload an additoinal file from SpN that contains the sample information (ConditionSetup.tsv), this will automatically label the quant columns.
For PTM data, modifications can be selected which will be filtered for in the data. Providing the ability to filter out Oxidation or CarbC peptides, for example.
The comparison tabs are filtered by for significant hits (< 0.05) using either the p-value or q-value. The Candidate spreadsheet is in the "long" data format and should be exported with all filters removed. 


