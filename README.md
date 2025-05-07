# Meta-analysis of Physical Activity Effects on Cognitive Function in the Elderly 

This repository contains materials related to my Master's thesis in Biostatistic, conducted at the University of Milan-Bicocca. The project focuses on a meta analysis of randomized controlled trials (RCTs) investigating the effects of physical activity on cognitive and partially cognitive outcomes in elderly hospitalized patients. 

## Abstract 

This meta-analysis investigates the effects of physical activity on cognitve function (primary outcome) and partially cognitive function (secondary outcomes) in hospitalized elderly individuals. We included 29 RCTs involving 4.574 patients aged 60 or older. Studies assessed cognitive outcomes using standardized tools such as MMSE, MoCA, Logical Memory Test and GDS. Statistical analyses were conducted using R (version 4.3.2), applying fixed- and random-effects models depending on heterogeneity. Meta-regressions and influence analyses were performed to assess robustness and identify potential effects modifiers. Results show that physical activity leads to statistically significant improvements in MMSE, LM, Gait Speed, and SPPB. Despite limitations related to database scope and variability in cognitive assessment tools, this work supports the integration of structured physical activity programs in the geriatric care to enhance cognitive and overall health. 

## Repository Contents

- R_meta_code to do models and regression
- Inclusion/exclusion analyses 
- Mesh Terms in PubMed  

## Methods 

- Systematic literature search using Pubmed and MeSH terms
- Inclusion of RCTs following PRISMA guidelines
- Data extraction for primary and secondary outcomes
- Statistical analysis using R 4.3.2 with 'meta' packages, in specifc 'metainf', 'metareg'
- Heterogenity measured using I^2, random/fixed effects model applied accordingly
- Meta-regression and influence analyses to explore source of variability
- Funnel Plot to see Pubblication Bias

## Main Outcomes 
**Congnitive (primary)**; 
- MMSE
- MoCA
- Logical Memory Test

**Partially Cognitive (secondary)**
- GDS
- GS
- SPPB

## Key Findings

- Significant improvement in MMSE, LM, GS and SPPB in intervention groups
- Non-significant improvement in MoCA and GDS
- High heterogenity handled with meta-regression and robust models

## Tools and Technologies 

- **R** (version 4.3.2)
- Packages: 'meta' 'dplyr' 'ggplot2'

## Author 

**Alessia Bacchiani** 
University of Milano-Bicocca
Email: alessiabacchiani@hotmail.it
