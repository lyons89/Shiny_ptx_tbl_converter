## Manual

Currently, there are 3 options for table conversion, Maxquant results that are further processed with Perseus **(MQ-Perseus)**, **Byos**, and **Spectronaut**.

### MQ-Perseus
The MQ-Perseus option is specifically designed for AP-MS projects. It combines and converts the input of unimputed and imputed data, from Persues, as text files. The unimputed data does not need to contain any statistical data. A summed intensity column is calculated from the non-log2 data, which is then used to sort the proteins in descending order. Each comparison that was performed in Perseus will be identified and will be presented on a single Excel spreadsheet tab after being filtered for either value alone or in conjunction with a log2FC value. Currently, the p-value is set to 0.05, and log2FC is greater than or less than 1.

Required columns:  "Majority protein IDs", "Protein names", "Fasta headers", "Gene names", "Gene name", "Potential contaminant", "Peptides", "Razor + unique peptides", "Unique peptides"


### Byos
The Byos converter is designed to read in a multi-tabbed Excel spreadsheet, containing the exported data table of individual samples from Byos. This converter requires Mass and Intesity columns along with the expected mass and creates multiple new columns such as delta mass from expected, (based on client expected and Byos expected), delta mass from the most intense, and local relative intensity. The first tab must be the table from the NIST mAB results, as this is treated differently than the actual samples. The tabs should be labeled with sample information. For the project samples the user needs to input the "Expected masses", what the client expects the masses to be. This can be done in two different ways. The first is by typing in the expected masses in the text box, only separating masses by a comma. The expectation is that each sample only contains a single expected mass. The other option is to use an Excel spreadsheet, where the first column is the same name and the second column is the expected mass, this is best used for projects with many samples. In cases in which the client is looking for multiple masses within a single sample, the easiest way to achieve this is by treating it as two samples by copying the data table onto 2 tabs, one with expected sequence A and the other tab for expected sequence B. The output is the same Excel spreadsheet except with the added columns and cleaned-up column names. 


Required columns: "Name", "Mass", "Expected mass"(only for NIST), and "Intensity"


### Spectronaut
The Spectronaut converter reads in the xls output Report and Candidate spreadsheets, merges the two, and then creates additional tabs for each comparison detected in the Candidate comparison column. These comparison tabs are then filtered by for significant hits by the users choosing. The Report should exported using our standard "PC protein Report schema.rs" method. The Candidate spreadsheet is in the "long" data format and should be exported with filters removed. The stats value to filter the data on can be selected from either p-value or q-value, and the cutoff can be selected with the standard being 0.05. 

Required columns: "PG.ProteinGroups", "PG.ProteinNames", "PG.Genes", "PG.ProteinDescriptions", "PG.FASTAHeader" 
