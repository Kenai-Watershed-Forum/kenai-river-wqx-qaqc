# Lessons learned workflow

Each project year is run in its own copy of this template. Each year teaches something (a new lab format, a WQX import rule, a QC edge case). This folder is how those lessons flow back into the template.

## In the year repo

1. At the start of the year, copy `_TEMPLATE_lessons_learned.md` to `other/agent_context/lessons_learned/{YEAR}_lessons_learned.md`.
2. Add an entry whenever you work around a template problem, make a decision the template did not anticipate, or get a surprise from WQX/CDX. A short entry written when it happens is better than a complete one written later.
3. After the CDX upload is verified on the Water Quality Portal, complete the year summary and the open questions section, and commit the file.

## In the template repo

1. Open the main template repo (not the year repo).
2. Copy or reference the year's file, for example by placing it in `other/agent_context/lessons_learned/received/{YEAR}_lessons_learned.md`, and ask the assistant: "Apply `other/agent_context/lessons_learned/received/{YEAR}_lessons_learned.md` to the template."
3. The assistant should work through the file in this order, presenting one decision at a time:
   1. Triage every entry: code change, reference-table or config change, documentation change, QAPP note, or no action.
   2. Check each "likely general" entry against the current template, since it may already be fixed.
   3. Propose the changes, wait for approval, then implement with tests where the change touches a function.
   4. Record each QAPP interpolation as a row in `other/agent_context/topics/qapp_revision_notes.md`.
   5. Update the `Status in template` field of every entry in the received file, and move that file to `other/agent_context/lessons_learned/applied/` with the template commit SHA noted in its year summary.
4. Run the tests and the full pipeline on the example data before committing.

## Rules

- The template never records year-specific values as expectations; lessons that are year-specific stay in the log.
- Entries are not deleted. A declined or deferred lesson keeps its reason so that it is not re-litigated.
- Conventions that emerge from several years (for example a standard way to log a lab re-run) get promoted from entries into the template's documentation, and the entry links to where.

## Folders

| Folder | Contents |
|----|----|
| `received/` | Year files waiting to be applied (create when needed) |
| `applied/` | Year files after their entries have been triaged, with final statuses |
