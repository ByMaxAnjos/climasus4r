# Peer Review — climasus4r (JOSS submission)

Reviewer perspective: JOSS editor/reviewer checklist plus a domain reviewer for environmental epidemiology methods (equivalent standard to IJE/Lancet Planetary Health methods sections).

## Major issues

**M1. Provenance of the validation case study was overstated.**
The initial draft stated the bundled tutorial case study was "drawn from real SIM-DO microdata... joined to real INMET station temperature." Tracing `scripts/modelagem_01_dlnm.R` and `scripts/tutorial_07_clima_estacoes.R` shows both mortality and temperature series in the bundled `.rds` artifacts are seeded synthetic fallbacks (`exemplo_minimo = TRUE`), used because the bundle must build without a live network call. Reporting this as real data would misrepresent the evidence to readers and reviewers who cannot verify it themselves; a paper's validation section is exactly where this kind of error is most costly.
*Disposition: must fix before submission.*

**M2. Internal inconsistency between two RR estimates for the same city.**
The single-station DLNM fit (RR = 0.735 at P75) and the six-city pooled BLUP for São Paulo (RR = 1.05) were both presented without explaining they come from different models with different reference temperatures. A careful reader would flag this as either an error or cherry-picking.
*Disposition: must fix — state which artifact each number comes from, or drop one.*

**M3. `sus_mod_burden()` cited as validated without checking its output.**
The bundled `mod_burden.rds` demo contains an implausible result (São Paulo attributable number = −1145.9, `pct_of_total` = 1092.5%), consistent with a bug or a scaling artifact in that specific demo run. The draft's claim that `sus_mod_af()` *and* `sus_mod_burden()` both "implement the same decomposition" as the cited studies should not extend to a function whose own demo output was not sanity-checked.
*Disposition: must fix — narrow the claim to `sus_mod_af()`, or verify `sus_mod_burden()` separately and report the fix.*

## Minor issues

**m1.** JOSS explicitly discourages research-results-style validation sections ("JOSS is not a venue for research results"). A 7-row comparison table plus two validation figures in the main manuscript will likely draw an editor comment.
*Disposition: relocate full crosswalk and figures to a companion `validation.md`; keep 1-2 tight paragraphs plus one figure in `paper.md`.*

**m2.** The ten temporal-alignment strategies — the feature the user asked to be foregrounded — were presented as one dense paragraph rather than a scannable table.
*Disposition: add a strategy/window/rationale table alongside the existing flowchart.*

**m3.** A generated figure (`validation_exposure_response.png`) was never referenced in the text.
*Disposition: merge into the referenced pooled-cities figure or remove.*

**m4.** Author affiliations are inferred from email domains in `DESCRIPTION` (`ufjf.br`, `unesp.br`, `usp.br`), which may not reflect current institutional affiliation (the session's own user-email metadata uses a different domain for the corresponding author). JOSS requires accurate affiliations and ORCIDs at submission.
*Disposition: flag explicitly to the author for confirmation; do not silently guess.*

**m5.** `CLAUDE.md` states there is no test suite yet. JOSS review requires "an automated test suite" as a functionality criterion — this is a submission blocker independent of the manuscript text.
*Disposition: out of scope for the manuscript itself; flagged to the author as a pre-submission action item.*

**m6.** Sentence in the validation section claimed the pooled RR "rises" with temperature, but the reported point estimates were flat (1.01 at both P75 and P99) with only the confidence interval widening — the sentence did not match the numbers it cited.
*Disposition: reworded to describe a flat central estimate with widening uncertainty.*

## What was already solid

- The three-stage architecture description (Preparation / Integration / Analysis) accurately reflects the `R/` directory and `CLAUDE.md`'s documented pipeline stages; no changes needed.
- The ten-strategy list in `sus_climate_aggregate()`'s Roxygen documentation matched the `match.arg()` call and the `switch()` dispatch in the function body exactly — no invented strategies, no omissions.
- Citations to the DLNM methodology papers (Gasparrini 2010, 2011, 2012/2013; Bhaskaran 2013) are standard, high-confidence, foundational references appropriate for this context.
- The peer-reviewed comparators (Bell 2008, Son 2016, Gasparrini 2015, Zhao 2021, Geirinhas 2021, Lee 2021) were independently retrieved and sourced with DOIs, and numbers not independently confirmed against a primary table (a secondary-sourced Brazil-specific AF% from Gasparrini 2015) were explicitly flagged and excluded rather than reported unverified.

## Verdict

Reject-and-resubmit on M1-M3 (data provenance, internal consistency, unverified function claim). Once these three are corrected, the manuscript is at or near JOSS-acceptable quality for a domain-specific software paper — see `REVISION_LOG.md` for the corrections applied.
