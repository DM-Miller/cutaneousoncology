# Confirmation workflow event scoping — DRY RUN

**No existing files were moved, edited, renamed, or deleted.**

Shared engine: `workflows/journal_club/shared/presenter_confirmation`
DAROMUN workspace: `workflows/journal_club/events/2026-09-25_daromun/presenter_confirmation`

| Source | Destination | Source exists | Destination exists | Planned action |
|---|---|---:|---:|---|
| `workflows/journal_club/shared/presenter_confirmation/config/event.csv` | `workflows/journal_club/events/2026-09-25_daromun/presenter_confirmation/config/event.csv` | TRUE | FALSE | MOVE/COPY INTO EVENT |
| `workflows/journal_club/shared/presenter_confirmation/config/sections.csv` | `workflows/journal_club/events/2026-09-25_daromun/presenter_confirmation/config/sections.csv` | TRUE | FALSE | MOVE/COPY INTO EVENT |
| `workflows/journal_club/shared/presenter_confirmation/data/raw_private` | `workflows/journal_club/events/2026-09-25_daromun/presenter_confirmation/data/raw_private` | TRUE | FALSE | MOVE/COPY INTO EVENT |
| `workflows/journal_club/shared/presenter_confirmation/data/working_private` | `workflows/journal_club/events/2026-09-25_daromun/presenter_confirmation/data/working_private` | FALSE | FALSE | CREATE EMPTY EVENT LOCATION |
| `workflows/journal_club/shared/presenter_confirmation/data/processed_private` | `workflows/journal_club/events/2026-09-25_daromun/presenter_confirmation/data/processed_private` | FALSE | FALSE | CREATE EMPTY EVENT LOCATION |
| `workflows/journal_club/shared/presenter_confirmation/outputs` | `workflows/journal_club/events/2026-09-25_daromun/presenter_confirmation/outputs` | FALSE | FALSE | CREATE EMPTY EVENT LOCATION |

## Shared items that remain shared

- `R/` reusable workflow scripts
- `templates/` mail-merge and input templates
- `public/` recipient-facing confirmation page
- `netlify/` serverless token lookup
- `netlify.toml` deployment configuration
- `data/netlify_private/assignments.generated.json` current deployment cache only

## Event-specific items

- Event and section configuration
- Raw RSVP and confirmation exports
- Working and finalized assignments
- Token ledger and canonical event lookup JSON
- Mail merge and status/coverage reports
