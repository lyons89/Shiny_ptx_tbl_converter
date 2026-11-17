# Proteomics Table Converter

A Shiny application that imports output tables from several proteomics search and analysis workflows, standardizes and cleans selected columns, and exports the results as a multi-sheet Excel workbook. It supports Spectronaut, Byos, several Perseus-compatible formats, and an MIBS-SpN workflow.

The app is intended to make common result tables easier to inspect and share. It does not rerun database searches, statistical tests, or peptide/protein identification; it reformats and combines data already produced by the selected software.

## Features

- Supports these input workflows:
  - **Spectronaut** protein and PTM results
  - **Byos** preprocessed Excel workbooks, with expected masses entered manually or uploaded from Excel
  - **MQ-Perseus**
  - **FP-Perseus**
  - **PD-Perseus**
  - **SpN-Perseus**
  - **MIBS-SpN**
- Imports tab-delimited Perseus files while skipping leading `#!` metadata lines.
- Selects useful protein, abundance, identification, annotation, and statistical columns, where present in the input.
- Rounds selected abundance and fold-change values for readability and sorts many result tables by summed abundance or quantity.
- Renames comparison statistics to more compact labels such as `Log2FC`, `p-value`, and `q-value`.
- For Spectronaut, allows the user to choose protein or PTM data, select comparisons, choose a quantity transformation, and choose p-value or q-value filtering for comparison result tabs.
- For MIBS-SpN, creates protein, QC, and kinase-focused sheets using a supplied kinase database or a description-based kinase flag.
- Previews a selected result sheet in the app and exports the complete result as an `.xlsx` workbook.

## Requirements

### R packages

The script loads these packages:

- `shiny`
- `tidyverse`
- `openxlsx`
- `DT`
- `markdown`
- `vroom`

Install them from an R console if needed:

```r
install.packages(c("shiny", "tidyverse", "openxlsx", "DT", "markdown", "vroom"))
```

### App files

The UI includes a **Manual** tab that loads `manual.md` from the app directory. Include that file when running or deploying the app, or remove/replace the `includeMarkdown("manual.md")` line if you do not need the tab.

### Input files

Use exports with the column names and structure expected by the selected workflow. The app selects columns by their names, so changing software export settings or renaming columns can affect conversion. Spectronaut inputs are read as tab-delimited text by `vroom`; use tab-separated exports even though the upload control also lists `.xls` as an accepted extension.

## Run the app

Save `app.R` and `manual.md` in the same directory, set that directory as the R working directory, and run:

```r
shiny::runApp()
```

Alternatively, open `app.R` in RStudio and select **Run App**.

## Basic workflow

1. Choose the relevant **Search Engine**.
2. Upload the required input file or files shown for that workflow.
3. Set any workflow-specific options, such as Spectronaut comparisons, quantity transformation, or expected masses for Byos.
4. Select the output sheet to preview from **Select tab to view**.
5. Enter an optional export file-name label.
6. Click **Convert** to run the conversion and display the selected result table.
7. Click **Download** to save all generated sheets in an Excel workbook.

The download name uses the current date and the entered label, in the form `YYYYMMDD_<label>_Results.xlsx`.

## Example operations

### Example: Spectronaut protein results

1. Select **Spectronaut**.
2. Upload the Candidate Stats file and the Report Quant file as tab-delimited exports.
3. Choose **Protein** for Data Type.
4. Select one or more comparisons in **Choose which comparisons to keep**.
5. Choose whether to output non-transformed quantities, Log2 quantities, or both. If using a condition setup file, select **Yes** and upload a TSV with `Run Label`, `Condition`, and `Replicate` columns.
6. Choose p-value or q-value as the statistic used to filter comparison-specific result tabs.
7. Click **Convert**, inspect the `Proteins` or comparison tab, and download the workbook.

The Spectronaut workbook is intended to contain a summary-statistics sheet, a combined protein table, and a result sheet for each selected comparison.

### Example: Perseus input

1. Select the appropriate Perseus workflow, such as **MQ-Perseus** or **PD-Perseus**.
2. Upload both the **UNIMPUTED** and **IMPUTED** tab-delimited Perseus files.
3. Click **Convert**.
4. Preview the `Unimputed` or `Imputed` sheet, then download the workbook.

For **SpN-Perseus**, also upload the Spectronaut condition TSV. The converter uses the condition file to rename quantity and run-evidence columns where the run labels match.

### Example: Byos expected masses

1. Select **Byos** and upload the preprocessed `.xlsx` workbook.
2. Choose **Manual** expected masses and enter comma-separated values, or choose the uploaded Excel option and provide an expected-mass workbook.
3. Click **Convert** to create the NIST/reference sheet and sample sheets with measured mass, expected mass, mass differences, intensity, and relative intensity columns.
4. Preview a sheet and download the workbook.

For manual entry, provide expected masses in the same order as the sample worksheets in the Byos workbook, separated by commas.

## What each workflow does

### Spectronaut

Combines selected comparison statistics from the Candidate Stats input with protein- or PTM-level quantities from the Report Quant input. It can optionally rename run quantity columns using a condition setup TSV, create log2 quantities, calculate replicate CV tables, and generate comparison-specific tabs using the selected p-value or q-value threshold. Protein/PTM annotations and quantity columns are retained when they match the expected Spectronaut column names.

### Byos

Reads the workbook's sheets, treats the first sheet as a reference/NIST-style table, and processes subsequent sample sheets. It calculates mass differences from the Byos expected mass and, when supplied, the user-provided expected mass; it also calculates difference from the most intense peak and local relative intensity.

### MQ-Perseus

Cleans MaxQuant/Perseus-style protein tables, retains selected identification and statistical columns, calculates a summed LFQ intensity from the LFQ columns, and sorts by that summed intensity. It exports unimputed and imputed tables and creates filtered comparison tables from the imputed data.

### FP-Perseus

Cleans the supported Perseus-format protein table, retains selected protein, MaxLFQ, spectral-count, and statistical columns, calculates summed intensity, flags likely contaminants from organism/indistinguishable-protein fields, and standardizes comparison-statistic names.

### PD-Perseus

Cleans supported Proteome Discoverer/Perseus-format tables, retains selected annotation, abundance, and statistical fields, extracts `GN=` values from descriptions into a Gene column, calculates summed abundance, and sorts by it.

### SpN-Perseus

Cleans Perseus tables containing Spectronaut-derived quantities and uses the condition TSV to rename quantity and run-evidence columns to condition/replicate labels.

### MIBS-SpN

Uses a Spectronaut Report Quant file, a condition TSV, and a kinase database workbook to create a protein table, a per-sample QC summary, and a kinase-focused table. If **None** is selected for kinase species, the code flags proteins whose descriptions contain the word “kinase.”

## Output workbook

The app writes the generated result list to an Excel `.xlsx` workbook. Depending on the selected workflow, sheets may include `Unimputed`, `Imputed`, `Proteins`, `QC`, `Kinases`, `summary_stats`, Byos source-sheet names, or comparison-specific tables. The table shown in the Shiny preview is only the currently selected sheet; the downloaded workbook contains the generated sheets for that conversion.

## Important implementation notes

- The script raises Shiny's request-size setting to approximately 1,000 MiB. Hosting platforms, reverse proxies, or server configuration may impose a lower upload limit.
- Column selection relies on expected export column names. Check that your input files are exported from compatible software versions and with the expected fields.
- For Spectronaut, the source reads the stats and quant files as tab-separated data. A file extension alone does not make an Excel workbook a valid TSV input.
- The Spectronaut CV calculation uses Log2 quantity columns. Choose a transformation and provide/verify condition labels so that the expected Log2 columns are available; otherwise, the CV step may not work as intended.
- The MIBS-SpN interface offers kinase-species choices, but the current conversion code joins the kinase database using the `Human.Uniprot` field whenever a species other than `None` is selected. Verify or adapt this join if using another species.
- The app formats existing results; verify the exported workbook and analysis choices before using it for downstream interpretation or publication.
