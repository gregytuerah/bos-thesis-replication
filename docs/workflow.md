# Replication Workflow

## Empirical Sequence

1. `Code/01_Identification.R` harmonizes IFLS roster records across waves,
   identifies origin households and siblings, constructs potential BOS-exposed
   years, and constructs schooling outcomes.
2. `Code/01b_Controls.R` adds location, expenditure, family composition, and
   parental-education measures.
3. `Code/02_Balance.R` produces baseline balance diagnostics by exposure
   cohort.
4. `Code/03_Regression.R` estimates the principal household fixed-effects
   models for senior-high attainment and completed years of schooling.
5. `Code/04_HTE-Robustness.R` produces heterogeneity analyses, sensitivity
   estimates, and the heterogeneity figure.
6. `Code/05_Manuscript_Figures.R` produces the analytic-sample origin map and
   BOS implementation-intensity map reported in the manuscript.

## Design Summary

- BOS began in 2005.
- Potential exposure is measured as the number of school-age years after BOS
  rollout for children born in the analytic cohorts.
- Lower-exposure cohort: birth years 1988--1992.
- Higher-exposure cohort: birth years 1994--1997.
- The 1993 transition cohort is excluded.
- Intensity is measured using the 2007 province share of sampled IFLS schools
  reporting positive BOS-related local operational funding.
- The preferred model interacts exposure years with a binary above-median
  intensity indicator.
- The primary estimation uses one child observation from the 2014 wave with
  origin-household fixed effects and household-clustered standard errors.

`table1_BOS_allocation.tex` and `appendix_variable_construction.tex` are
manuscript-source tables describing policy background and variable definitions;
they are not regression outputs.

The principal reported estimates and sample sizes are summarized in
`docs/expected_results.md` for comparison after running the scripts.
