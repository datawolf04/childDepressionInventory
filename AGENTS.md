# AGENTS.md — CDI Project

## What this repo is

Documentation and R scoring code for the Children's Depression Inventory (CDI / CDI 2). Three flat files at root, no build system.

## Files

- `cdiAnnBib.md` — Annotated bibliography (7 academic sources, APA 7th)
- `cdiScoringGuide.md` — Scoring guide for clinicians (total, scale, subscale, cutoff scores)
- `cdi_scoring.R` — R scoring implementation (tidyverse)

## Code notes

- `cdi_scoring.R` depends on tidyverse (`library(tidyverse)`); requires R ≥ 4.0
- Item-to-subscale mapping for the original 27-item CDI is coded directly (per Jelínek et al., 2021)
- CDI-2 item assignments are **not** hardcoded — user supplies a custom scoring key (item-to-subscale mapping is proprietary in the MHS manual)
- Test the file: `R --no-save --no-restore -e 'source("cdi_scoring.R")'`
- Two core functions: `score_cdi()` and `flag_cdi_patterns()`
- No test suite, no CI, no package structure — this is a standalone script

## Conventions

- Use tidyverse pipe (`%>%`) and dplyr verbs consistently for any R additions
- APA 7th citations for any new bibliography entries
- Markdown `.md` files use standard GFM with `---` section breaks
