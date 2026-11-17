# Proteomics Table Converter Manual

## Purpose

This Shiny app converts selected proteomics search and analysis outputs into cleaner, multi-sheet Excel workbooks for review and collaboration. It is designed for data that have already been processed by the source software: it does not perform database searching, protein identification, or statistical testing.

The app currently offers these modes: **Spectronaut**, **Byos**, **MQ-Perseus**, **FP-Perseus**, **PD-Perseus**, **SpN-Perseus**, and **MIBS-SpN**. These modes use different input formats and do not all create the same types of output sheets. Follow the relevant section below and inspect the exported workbook before using it downstream.

## Start the app

Place `app.R` and this `manual.md` in the same folder. In R or RStudio, set that folder as the working directory and run:

```r
shiny::runApp()
```

The app uses the R packages `shiny`, `tidyverse`, `openxlsx`, `DT`, `markdown`, and `vroom`. Install any missing packages with:

```r
install.packages(c("shiny", "tidyverse", "openxlsx", "DT", "markdown", "vroom"))
```

The **Manual** tab in the app displays this file. Keep the filename `manual.md` unless you also change the `includeMarkdown("manual.md")` line in `app.R`.

## General workflow

1. In **Select Search Engine Used**, choose the mode that matches your input data.
2. Upload the file or files requested for that mode. Use the expected export format and column names described below.
3. Choose any available options, such as Spectronaut data type, comparisons, quantity transformation, or Byos expected-mass source.
4. Click **Convert**. The app displays the sheet selected in **Select tab to view**.
5. Review the preview and use the sheet selector to inspect other generated sheets.
6. Enter an optional label in **Export file name** and click **Download** to export the generated workbook.

The workbook filename is based on the download date and the entered label, in the form `YYYYMMDD_<label>_Results.xlsx`. The workbook contains the sheets generated for the selected conversion; the preview shows only one sheet at a time.

## Spectronaut

### Purpose and inputs

Spectronaut exports quantitative results and comparison statistics separately. This mode combines a **Candidate Stats** file with a **Report Quant** file and creates an Excel workbook containing a protein- or PTM-level table and comparison result sheets.

Upload:

- A tab-delimited Candidate Stats export in the long format expected by the app. It should contain comparison, group, fold-change, p-value, and q-value fields, including columns such as `Comparison (group1/group2)`, `Group`, `AVG Log2 Ratio`, `Pvalue`, and `Qvalue`.
- A tab-delimited Report Quant export containing the relevant protein- or PTM-level annotations and quantity columns.

Select the comparisons to include after the Candidate Stats file has been read. Export the Candidate Stats data with the desired filters removed; the app itself applies the selected-comparison and validity logic when converting.

### Options

- **Data Type:** Choose **Protein** or **PTM**. For PTM data, choose the modification titles to retain.
- **Import SpN condition setup file:** Optionally upload a tab-delimited condition file. It must have columns named `Run Label`, `Condition`, and `Replicate`. The run labels are used to match and rename quantity columns; spelling must match the labels embedded in the Report Quant column names.
- **How do you want the sample quantity values:** Choose `non-transformed`, `Log2`, or `Both`.
- **Stats value to filter on:** Choose p-value or q-value for the comparison-specific sheets. The app uses a threshold of `< 0.05` for those sheets.

### Output and processing

The workbook is assembled from selected comparison statistics and Report Quant annotations and quantities. Depending on the selection, it includes a summary-statistics sheet, a combined `Proteins` table, and a sheet for each selected comparison. The comparison sheets are filtered by the selected p-value or q-value threshold and sorted by log2 fold change. The combined table is generally sorted by summed quantity. When a condition file is supplied, quantity columns can be relabeled using the condition and replicate information.

The app also attempts to calculate replicate CV values from Log2 quantity columns. For this calculation to be meaningful, check that the data transformation and condition/replicate labels yield the intended Log2 quantity columns. Review the output and any app errors if using the `non-transformed` option.

## MQ-Perseus

### Purpose and inputs

This mode is intended for MaxQuant protein results processed in Perseus, particularly AP-MS workflows. Upload both:

- An **unimputed** Perseus text export.
- An **imputed** Perseus text export.

The importer reads tab-delimited text and skips leading metadata rows beginning with `#!`. Use Perseus exports with the column names expected by the converter. Key protein columns include:

- `Majority protein IDs`
- `Protein names`
- `Fasta headers`
- `Gene names`
- `Gene name`
- `Potential contaminant`
- `Peptides`
- `Razor + unique peptides`
- `Unique peptides`

The converter also uses LFQ intensity, comparison difference, p-value, q-value, and significance columns when present.

### Output and processing

The converter selects protein annotation, peptide, comparison-statistic, LFQ intensity, and MS/MS count columns; rounds selected values; calculates a summed LFQ intensity from the LFQ intensity columns; and sorts the protein tables by that sum. It is intended to provide an unimputed protein table, an imputed table, and comparison-specific tables filtered at p-value `< 0.05` and sorted by fold change.

**Current-code note:** The script currently defines the Perseus sheet-name list as only `Unimputed` and `Imputed`, while the MQ-Perseus conversion code also attempts to append comparison tables. As a result, the MQ-Perseus conversion/export may fail when comparison tables are present. If that happens, update the sheet-name generation in `app.R` so there is one valid, unique worksheet name per output table before relying on this feature.

### Perseus condition names

Avoid underscores (`_`) in condition names used to create Perseus comparisons. Perseus uses underscores to separate comparison group names, so an underscore within a single condition name can make comparison parsing ambiguous.

## FP-Perseus

Upload the unimputed and imputed tab-delimited Perseus text exports. The importer skips leading `#!` metadata rows. The converter selects supported protein identification, annotation, MaxLFQ intensity, spectral-count, and comparison-statistic columns; calculates summed LFQ intensity; flags likely contaminants using organism and indistinguishable-protein fields; standardizes some comparison-statistic labels; and sorts by summed intensity.

The current conversion code returns the cleaned `Unimputed` and `Imputed` sheets. Although the interface displays a p-value/q-value selector for this mode, the current FP-Perseus conversion code does not use it to create filtered comparison sheets.

## PD-Perseus

Upload the unimputed and imputed tab-delimited Perseus text exports. The converter selects supported Proteome Discoverer/Perseus annotation, abundance, and comparison-statistic columns; extracts gene identifiers from `GN=` entries in descriptions when available; calculates summed normalized abundance; and sorts by that value.

The current conversion code returns the cleaned `Unimputed` and `Imputed` sheets. The p-value/q-value control shown in the interface is not currently used to filter or create comparison sheets in this mode.

## SpN-Perseus

Upload the unimputed and imputed Perseus text exports, plus a tab-delimited condition setup file with these columns:

- `Run Label`
- `Condition`
- `Replicate`

The converter cleans supported Spectronaut-derived Perseus columns and uses matching run labels to rename quantity and run-evidence columns to condition/replicate labels. Ensure that labels in the condition file match the names embedded in the Perseus columns.

The current conversion code returns `Unimputed` and `Imputed` sheets. The p-value/q-value selector shown in the interface is not currently used to create filtered comparison sheets in this mode.

## Byos

### Purpose and inputs

This mode prepares intact-protein/antibody results, such as ZipChip output, for review. Upload a multi-sheet `.xlsx` workbook exported or preprocessed from Byos. The **first worksheet must be the NIST mAb reference result**; subsequent worksheets are treated as samples.

The first worksheet must contain these columns:

- `Name`
- `Mass`
- `Expected mass`
- `Intensity`

Each sample worksheet must provide the columns selected by the app: `Name`, `Mass`, `Expected mass`, and `Intensity`.

### Expected-mass options

Choose one source of expected masses:

- **Manual:** Enter comma-separated expected masses in the text field. Values must be in the same order as the sample worksheets (excluding the first NIST/reference worksheet), with one value per sample sheet.
- **xlsx:** Upload an Excel workbook containing expected masses. The app reads the values from the second column of the first worksheet, in row order. The current code does not match values by sample name, so make sure the order and count correspond to the Byos sample worksheets.

### Output and calculations

The NIST/reference sheet is sorted by intensity and reports measured mass, Byos expected mass, delta mass from the Byos expected mass, delta mass from the most intense peak, intensity, and local relative intensity.

For each sample, the output includes measured mass, the user-supplied expected mass, delta mass from that user-supplied mass, Byos expected mass, delta mass from the Byos expected mass, delta mass from the most intense peak, intensity, and local relative intensity. Delta mass is calculated as measured mass minus the corresponding expected mass. Local relative intensity is scaled to the most intense peak in that worksheet.

## MIBS-SpN

### Purpose and inputs

This mode summarizes Spectronaut protein quantities for MIBS-SpN analysis. Upload:

- A tab-delimited Spectronaut Report Quant file.
- A tab-delimited condition setup file with `Run Label`, `Condition`, and `Replicate` columns.
- A kinase database workbook (`.xlsx`).

The condition file is used to rename sample quantity columns. Ensure run labels match the Report Quant column names and that there is one entry per sample quantity column.

### Kinase selection and output

The app populates the species choices from the kinase database workbook's column names (starting with its fourth column), with an additional `None` choice. It creates three sheets:

- **Proteins:** cleaned protein annotations, summed quantity, and log2 sample quantities.
- **QC:** per-sample summed kinase quantity, total quantity, and percent enrichment.
- **Kinases:** proteins identified as kinases using the selected database field, or by searching protein descriptions for the word `kinase` when `None` is selected.

**Current-code note:** The current database-join code uses the `Human.Uniprot` column whenever a database species is selected, even though the interface presents multiple species choices. Confirm that the selected database structure and species match this join, or adjust the code before interpreting non-human selections. A kinase database workbook is currently required even when `None` is selected.

## Input and output considerations

- Column names are significant. The converter selects columns by expected names, so renamed columns or materially different export formats may result in missing fields or conversion errors.
- Perseus inputs must be tab-delimited text files. Spectronaut Report Quant, Candidate Stats, and condition files are also read as tab-delimited text; a spreadsheet extension does not convert an Excel workbook into a TSV.
- The app allows large uploads by setting Shiny's request limit to about 1,000 MiB, but the hosting server or reverse proxy may impose a lower limit.
- Excel worksheet names must be unique and within Excel's naming limits. If comparisons generate duplicate or long sheet names, adjust them before export.
- Always verify the generated workbook, sample labels, transformations, filters, and statistical thresholds before downstream analysis or sharing. The app is a formatting and conversion utility, not a substitute for reviewing the underlying analysis.
