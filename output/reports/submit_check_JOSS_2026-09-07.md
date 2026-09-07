# Submission Check Report — JOSS — 2026-09-07

Paper: inst/paper/paper.md (+ paper.bib, figures/)
Journal: JOSS (Journal of Open Source Software)
Run by: submit-check skill (adapted — JOSS uses its own review checklist, not the geo-journal word-limit template)

Note: JOSS has no word/figure-count limit and no Methods/Results/Discussion structure requirement — it reviews the **paper** (Summary, Statement of Need, refs) against brevity/scope norms, and the **repository** (license, tests, docs, community guidelines, authorship). Checklist below follows JOSS's actual review criteria (https://joss.readthedocs.io/en/latest/review_criteria.html), not the generic template's geo/word-limit items.

## Summary
- Total items checked: 16
- PASS: 10
- FAIL: 2
- WARN: 4

**Overall verdict: NOT READY (two quick fixes, then ready)**

## Failed Items (must fix before submission)

| Section | Item | Issue |
|---|---|---|
| Content | Orphan table reference | `paper.md` line 105 says "(\autoref{fig:strategies} and Table 1)" but no Table 1 exists anywhere in the paper — either add the table or remove "and Table 1" |
| Figures | Unreferenced figure label | `\label{fig:exposure}` (line 93, exposure/covariate layer figure) is never cited via `\autoref` in the text — add `\autoref{fig:exposure}` to the sentence describing that layer (line 91) |

## Warnings (review before submission)

| Section | Item | Note |
|---|---|---|
| Figures | `fig:map` under-cited | Line 113 cites `\autoref{fig:trend}-\autoref{fig:dlnm}` as a range, but `fig:map` (the incidence map, line 117) sits between them and is never explicitly `\autoref`'d — a range implies 3 consecutive figures are all covered, but only the endpoints render as links. Cite it explicitly, e.g. "\autoref{fig:trend}, \autoref{fig:map} and \autoref{fig:dlnm}" |
| Authorship | DESCRIPTION vs. paper author mismatch | `paper.md` lists 9 authors; `DESCRIPTION`'s `Authors@R` lists only 3 (Anjos, Menezes, Faria) with roles `aut`/`ctb`. JOSS reviewers check that paper authors are substantive contributors — align the package `Authors@R` (roles/list) with the paper's author list, or add a short justification if some are domain/data contributors rather than code contributors |
| Repository | No `CITATION.cff` | Not required by JOSS but expected practice for citable software; recommend adding one (can be generated from DESCRIPTION) |
| Repository | No root-level `CONTRIBUTING.md` | Community guidelines exist as `contributing.Rmd` vignettes (en/es/pt) but JOSS reviewers usually look for a discoverable root `CONTRIBUTING.md` or a README link to it — add a one-line pointer in `README.md` if not already there |

## Passed Items

- **Summary** section present, clear, states what the software does (not just theory) — PASS
- **Statement of Need** present, articulates the concrete integration problem and cites supporting literature — PASS
- **State of the field** section correctly differentiates from `microdatasus`, `datasus`, `geobr`, `censobr`, `dlnm`, `mvmeta`, and a comparable Python framework — PASS
- All in-text citation keys (`@bell2008`, `@gasparrini2011`, etc. — 22 keys) resolve to entries in `paper.bib`; no missing references — PASS (verified via key cross-check)
- `paper.bib` has 7 unused entries (armstrong2006, confalonieri2009, funk2026, gelaro2017, inness2019, landau2021, munozsabater2021) — harmless (JOSS doesn't require zero unused refs), not flagged as FAIL, but consider pruning for tidiness
- Pandoc + citeproc render of `paper.md` against `paper.bib` completes with 0 errors — bibliography syntax and YAML front matter are valid
- All 9 image files referenced in `paper.md` exist on disk under `inst/paper/figures/` — PASS
- License: `MIT + file LICENSE`, OSI-approved, present at repo root (`LICENSE`, `LICENSE.md`) — PASS
- Automated tests exist (`tests/testthat/`, 3 test files) with `testthat (>= 3.0.0)` configured in DESCRIPTION — PASS (JOSS requires evidence of testing, not full coverage)
- **AI usage disclosure** section present, specific, and appropriately scoped (per current JOSS policy) — PASS
- Documentation: README (4 languages), pkgdown site, tutorials in 3 languages linked from the paper — PASS

## Recommended Fixes

1. Fix the orphan "Table 1" reference in `paper.md` line 105 — either add a real Table 1 summarizing the ten temporal strategies (referenced content already exists in prose at line 107, could be tabulated), or drop "and Table 1" from the sentence.
2. Add `\autoref{fig:exposure}` to the sentence on line 91 describing the exposure/covariate integration layer.
3. Change the figure range on line 113 to explicitly name all three figures (trend, map, dlnm) instead of a two-endpoint range.
4. Reconcile `DESCRIPTION`'s `Authors@R` list with the 9 authors credited in `paper.md` (or document why the package author list is narrower).
5. Optional: add a `CITATION.cff` and a root-visible `CONTRIBUTING.md` (or link to the existing `contributing.Rmd` vignette from `README.md`).

Once items 1–2 (FAIL) are fixed, the paper is ready for JOSS pre-review submission; items 3–6 are worth doing but won't block review.
