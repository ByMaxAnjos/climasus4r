# Revision Log — climasus4r JOSS paper

Applied in response to `REVIEW.md`.

| # | Review point | Action taken |
|---|---|---|
| M1 | Case-study data falsely described as real SIM-DO/INMET data | Verified provenance by reproducing the seeded synthetic generator in `scripts/modelagem_01_dlnm.R` and matching it exactly (2170/2170 values) against `dados/mod_dlnm_fit.rds`. Rewrote the validation section to state explicitly that the bundled tutorial uses a seeded synthetic exposure series (`exemplo_minimo = TRUE`) and demonstrates pipeline wiring, not a detected effect. Reframed the section around specification equivalence to peer-reviewed studies, with the bundled run used only as an execution/convergence check. |
| M2 | Unexplained discrepancy between single-station (RR 0.735) and pooled (RR 1.05) São Paulo estimates | Dropped the single-station number from `paper.md`; retained both numbers in `validation.md` with an explicit note on why they differ (different reference temperature, single-station vs. pooled/shrunk BLUP). |
| M3 | `sus_mod_burden()` cited without checking its own demo output | Removed `sus_mod_burden()` from the validated-decomposition claim; the sentence now names only `sus_mod_af()`, which was checked. The implausible `mod_burden.rds` demo output is not cited anywhere in the manuscript. |
| m1 | Research-results-style validation section too long for JOSS norms | Moved the full comparator table, both validation figures' detail, and provenance discussion to `paper/validation.md`; `paper.md` now carries two tight paragraphs and one figure. |
| m2 | Ten strategies as dense prose | Added a strategy/window/rationale table directly above the existing flowchart figure. |
| m3 | Unreferenced figure | Merged the exposure-response panel into the referenced pooled-cities figure (`patchwork`, two panels, one file); removed the orphaned file. |
| m4 | Affiliation uncertainty | Not silently resolved — flagged to the author in the final summary; placeholder affiliations from `DESCRIPTION` email domains are marked for confirmation before submission. |
| m5 | No test suite (JOSS functionality requirement) | Out of scope for the manuscript; flagged to the author as a pre-submission blocker. |
| m6 | "Rises" claim contradicted by flat point estimates | Reworded to state a flat central estimate (RR ≈ 1.01 at both P75 and P99) with widening confidence intervals at higher percentiles. |

## Language and tone pass (separate from the peer-review pass)

After the corrections above, `paper.md` and `validation.md` were re-read specifically for generic/AI-sounding phrasing and trimmed:

- Removed stock intensifiers and hedge-filler ("comprehensive", "robust", "seamless", "leverage", "cutting-edge", "state-of-the-art", "streamline") wherever they appeared without a specific referent.
- Replaced vague claims ("significantly reduces effort", "powerful framework") with the specific mechanism being described (what the function does, what object it returns, what it is compared against).
- Cut sentences that restated the section heading in different words.
- Kept technical terms (DLNM, BLUP, crossbasis, quasi-Poisson) as-is; these are precise, not filler.
