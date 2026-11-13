# ADEC context, README checklist plan, book and docs

Moved verbatim from AGENTS.md on 2026-11-13. Read when relevant; core memory is AGENTS.md.

## ADEC and Integrated Report Context (2026-11-09)

User-stated pathway to ADEC: (1) KWF submits data to EPA CDX (WQX); (2) ADEC accesses the data from WQP. KWF does not submit to ADEC separately. Links the user wants considered: https://integrated-report-adec.hub.arcgis.com/ (JS-rendered; contents not read) and https://dec.alaska.gov/water/water-quality/ambient-water-quality-data (403 on direct fetch; read via search snippet only).

- ADEC's ambient data page: WQX is the submission route to EPA, WQP the retrieval route. ADEC keeps its own database (AWQMS) and pushes from it to WQX; ADEC also takes voluntary submissions through the Water Quality Data Manager (Amber Bethe, 907-269-7955, as listed on the page).
- Integrated Report: ADEC assesses waters against standards every two years (CWA 303(d) and 305(b)) and issues a call for data (2026 report: closed 2025-02-28; next cycle dates unconfirmed). It also accepts data continuously.
- CALM (2020 edition text in `other/documents/md/`; a newer edition may exist): QAPPs, lab transmittals, COCs, calibration records and lab qualifications should be available on request. Data qualification rules (data levels, minimum sample size, confidence level) decide whether data are used.
- Implications: because ADEC reads from WQP, the quality of the CDX upload (IDs, fractions, units, status codes, no duplicates) is what ADEC sees; fixes after upload mean delete and re-upload. The assessed output and the 42-question checklist serve as the QA/QC documentation ADEC may request. Open: whether any KWF data also reaches ADEC by another route (duplicate risk); whether the assessed flags should align with CALM data qualification rules.
- Status (2026-11-09): the user was asked to paste or save the Integrated Report hub content (data submission requirements, Kenai assessment units) because it cannot be read here; and whether to align assessed flags with CALM rules is undecided. No code changed for this context.

**README checklist plan (items to include when README is rewritten):**

1. Copy the template to a new repo; put only that year's raw lab files in `data/`.
2. Edit `config.yml` (dates, file paths, trip blank sites, TSS LOD/LOQ, FC and TSS run and receipt date/time from the COCs).
3. Add new SGS duplicate/blank ID rows to `sgs_site_names_matching_table.xlsx` as needed; fill `year_files/` (flag decisions, YSI corrections, trip blank crews).
4. Run ingest, contract checks, QA/QC document, WQX format and CDX export; resolve all guard failures.
5. Upload to CDX via WQX Web (`cdx_upload.qmd`); verify in WQP once indexed (days, not 24 to 48 hours).
6. Retain the supporting documents for ADEC requests (COCs, lab reports, calibration records, QAPP version, completed checklist).
7. Note for reviewers: ADEC obtains the data from WQP after the CDX upload; no separate ADEC submission is made. Contact the ADEC Water Quality Data Manager for questions or to confirm this.
8. Report repo (`kenai-river-wqx`) reads the data from WQP after indexing.

## Book and Docs (2026-10-28)

`quarto render` ran clean. `intro.qmd` was rewritten (purpose, single-year workflow, four pipeline parts, bare-ID rule). `summary.qmd` is a seven-step finish-a-year checklist (not a book chapter; can feed the README checklist). Book title no longer says "2014 - 2025"; `date: today` is set in `_quarto.yml`. Output stays `output/report`; whether it is published (GitHub Pages) is open. Not checked: whether Part D wrote files or skipped.


## ADEC QA page review (2026-11-13)

Documents saved in `other/documents/` (text copies in `other/documents/md/`: `ADEC-QAPP-guidance-2026-update.md`, `ADEC-QAPP-template-2026.md`, `ADEC-DOW-QMP-2025-2029.md`). Our QAPP is v3 (2023) with April 2024 addendum; the ADEC guidance (05-22-26) and template follow EPA 2025 QAPP guidance (26 sections).

Open gaps, not yet acted on:
1. **RPD rule (Q19).** ADEC 2026 guidance: skip RPD when either result is below LOQ and the other is above LOQ but under 2x LOQ; the pair fails when either is below LOQ and the other is at least 2x LOQ. Our QAPP v3 says RPD needs one or both results above LOQ and one or both at least 2x LOQ. The old Q19 code applies neither screen (it computes RPD on any pair with two numeric results). Decide which rule governs, then implement it in `qaqc.qmd`.
2. **Method blank rule** (no target analyte at or above the reporting limit) and **LCS** ("laboratory in-house control limits") match `assess_lab_qc()`. The ADEC guidance does not mention LLQC, so it does not close the LLQC open task.
3. **Usability and rejection.** ADEC separates verification, validation and usability; rejection is reserved for the project team; any use of data that missed QC criteria must be explained in the final report; all qualifiers used must be defined in the QAPP. The report should include a data usability statement and a qualifier definitions list.
4. **Completeness goal** in the ADEC example is 80-90%; check the project QAPP's own goal before Q25/Q26 are ported.
5. ADEC's guidance calls a trip blank "also known as a field blank"; we code them as separate Activity Types.
6. The QMP says AWQMS data goes to WQP annually; KWF data reaches WQP via CDX (unchanged).
