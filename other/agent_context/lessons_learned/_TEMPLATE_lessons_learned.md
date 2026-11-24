# {YEAR} Lessons Learned

Copy this file to `{YEAR}_lessons_learned.md` in the year's repo, fill it in as the year's QA/QC and CDX upload proceed, and finish it before the year is closed out. Then open the main template repo and ask the assistant to apply it (see `other/agent_context/lessons_learned/README.md`).

Write for a reader who has not seen this year's work. Describe what happened and what the template should do differently; do not prescribe code unless you already know the fix. Remove this paragraph and the field guide below once the file is filled in.

## Year summary

| Field | Value |
|----|----|
| Project year | {YEAR} |
| Year repo | {URL} |
| Template commit this year started from | {short SHA} |
| Labs and delivery formats | {e.g. SGS xlsx, FC and TSS from City of Kenai workbook, YSI workbook} |
| CDX upload date(s) and outcome | {accepted / partial / rejected; import or batch ID} |
| Completed by / date | {name, YYYY-MM-DD} |

## Entries

One block per lesson. Copy the block as many times as needed.

### L{YEAR}-01: {short title}

| Field | Value |
|----|----|
| Date | {YYYY-MM-DD} |
| Stage | {ingest / contract check / QA/QC question number / format_wqx / apply_qaqc_flags / CDX export / WQX import / report / docs} |
| Type | {bug / format change / new lab or method / new QC rule / QAPP interpretation / WQX or CDX behavior / process / documentation} |
| Scope | {year-specific only / likely general / unsure} |
| Severity | {blocked work / needed a workaround / minor} |
| Files affected | {paths in the year repo, if known} |

**What happened:** {facts: input, error message, unexpected output, counts as test-fixture or year values}

**Workaround used this year:** {what you did in the year repo, or "none"}

**Requested template change:** {what the template should do next year, or "decide"}

**Evidence:** {file, log line, WQX import message, email, or QAPP section}

**Status in template:** {leave as "open"; the assistant updates it to "done (commit)", "declined (reason)" or "deferred (reason)"}

## Reference data and config changes

List edits to `reference/`, `config.yml` keys or `year_files/` that the year needed and that other years will likely need too (new analytes, methods, sites, lookup rows, qualifier codes).

| File   | Change   | Why      |
|--------|----------|----------|
| {path} | {change} | {reason} |

## Open questions for the QAPP or for agencies

Items where the pipeline interpolated or guessed. These feed `other/agent_context/topics/qapp_revision_notes.md`.

- {question, who could answer it}

## Field guide

- **Scope** decides routing. "Year-specific only" entries are recorded in the template's log but cause no code change. "Likely general" entries should lead to a change or an explicit decision.
- **Stage** uses the same names as the pipeline run order: combine_dat, assess_lab_qc, assign_qc_status, format_wqx, apply_qaqc_flags, generate_cdx_export.
- Never record year-specific values (row counts, site counts, dates) as expectations. Give them as examples from this year only.
- Do not paste secrets (CDX credentials, tokens).
