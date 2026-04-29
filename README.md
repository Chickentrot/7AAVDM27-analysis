# 7AAVDM27 – Social Media Travel Content & Beach Destination Intention

**Module:** 7AAVDM27 Quantitative Research Methods — KCL MSc Digital Economy

---

## Research Question

Is self-reported consumption of social media travel content about beach destinations associated with young people's intention to visit those destinations, and does personality (openness, extraversion, agreeableness, conscientiousness, neuroticism) predict travel intention?

---

## Hypotheses

H1–H3 are directional (positive association predicted from prior literature). H4–H6 are exploratory two-tailed tests — no strong directional prior exists in the travel-personality literature for these traits.

| # | Hypothesis | Type |
|---|------------|------|
| H1 | Higher social media travel content consumption is positively correlated with intention to visit beach destinations | Directional (positive) |
| H2 | Higher openness to experience is positively correlated with intention to visit beach destinations | Directional (positive) |
| H3 | Higher extraversion is positively correlated with intention to visit beach destinations | Directional (positive) |
| H4 | Agreeableness is associated with intention to visit beach destinations | Exploratory (two-tailed) |
| H5 | Conscientiousness is associated with intention to visit beach destinations | Exploratory (two-tailed) |
| H6 | Neuroticism is associated with intention to visit beach destinations | Exploratory (two-tailed) |

---

## Variable Codebook

### Likert items (1 = Strongly Disagree, 5 = Strongly Agree unless noted)

| Variable | Item | Scale | Notes |
|----------|------|-------|-------|
| Q1 | How often do you view beach destination content on social media? | 1=Never, 2=Rarely, 3=Sometimes, 4=Often, 5=Very often / Always | Ordinal frequency; SMuse composite |
| Q2 | I actively engage with travel content on social media | 1–5 | SMuse composite |
| Q3 | I frequently see social media travel content about beach destinations | 1–5 | SMuse composite |
| Q4 | I follow travel influencers or travel-related accounts | 1–5 | SMuse composite |
| Q5 | Social media travel content increases my interest in travelling to beach destinations | 1–5 | TravelIntent |
| Q6 | Social media travel content increases my desire to travel to beach destinations | 1–5 | TravelIntent |
| Q7 | I am likely to visit a beach destination I see on social media | 1–5 | TravelIntent |
| Q8 | I would consider booking a beach trip based on social media content | 1–5 | TravelIntent |
| Q9 | Social media content makes beach destinations seem more appealing | 1–5 | TravelIntent |
| Q10 | I see myself as someone who has an active imagination | 1–5 | Openness — normal scoring |
| Q11 | I see myself as someone who has few artistic interests | 1–5 | Openness — **REVERSE SCORED** (Q11_r = 6 − Q11) |
| Q12 | I see myself as someone who is outgoing, sociable | 1–5 | Extraversion — normal scoring |
| Q13 | I see myself as someone who is reserved | 1–5 | Extraversion — **REVERSE SCORED** (Q13_r = 6 − Q13) |
| Q14 | I see myself as someone who is generally trusting | 1–5 | Agreeableness — normal scoring |
| Q15 | I see myself as someone who tends to find fault with others | 1–5 | Agreeableness — **REVERSE SCORED** (Q15_r = 6 − Q15) |
| Q16 | I see myself as someone who does a thorough job | 1–5 | Conscientiousness — normal scoring |
| Q17 | I see myself as someone who tends to be lazy | 1–5 | Conscientiousness — **REVERSE SCORED** (Q17_r = 6 − Q17) |
| Q18 | I see myself as someone who worries a lot | 1–5 | Neuroticism — normal scoring |
| Q19 | I see myself as someone who is relaxed and handles stress well | 1–5 | Neuroticism — **REVERSE SCORED** (Q19_r = 6 − Q19) |

### Demographic items

| Variable | Item | Categories |
|----------|------|------------|
| Q20 | Age | 1=18–20, 2=21–23, 3=24–26, 4=27–29, 5=30+ |
| Q21 | Gender | 1=Male, 2=Female, 3=Prefer not to say |
| Q22 | Occupation (first selection if multi-select) | 1=Student, 2=Full-time employed, 3=Part-time employed, 4=Freelancer/self-employed, 5=Unemployed, 6=Prefer not to say |
| Q23 | Travel frequency (leisure trips per year) | 1=Never, 2=1–2 times, 3=3–5 times, 4=6 or more |
| Q24 | Have you ever visited a beach destination influenced by content you saw on social media? | 1=Yes, 2=No |

### Composite Scales

| Composite | Items | n items |
|-----------|-------|---------|
| **SMuse** | Q1, Q2, Q3, Q4 | 4 |
| **TravelIntent** | Q5, Q6, Q7, Q8, Q9 | 5 |
| **Openness** | Q10, Q11_r | 2 |
| **Extraversion** | Q12, Q13_r | 2 |
| **Agreeableness** | Q14, Q15_r | 2 |
| **Conscientiousness** | Q16, Q17_r | 2 |
| **Neuroticism** | Q18, Q19_r | 2 |

> Note: 2-item composites are common in short BFI measures but may produce lower Cronbach's α. Values below 0.70 should be interpreted with caution.

---

## How to Run

### 1. Install R packages (once)

```r
install.packages(c("tidyverse", "psych", "corrplot"))
```

`analysis.R` checks for and installs missing packages automatically on startup.

### 2. Prepare data

The script expects `data/data.csv` in the project root. The committed file contains the real survey responses (n=109) collected via Microsoft Forms. Likert text responses ("Strongly disagree" → 1, etc.) are recoded numerically before commit; demographics are recoded per the codebook above. To rerun on a refreshed export, replace `data/data.csv` keeping column order Q1–Q24.

### 3. Run the analysis

From the `7AAVDM27-analysis/` directory:

```bash
Rscript analysis.R
```

Or open `analysis.R` in RStudio and click **Source**.

### 4. Outputs

| File | Description |
|------|-------------|
| `outputs/descriptives.csv` | Mean, SD, min, max, n for all 7 composites |
| `outputs/hypothesis_results.csv` | APA-format correlation results for H1–H6 |
| `outputs/correlation_matrix.png` | Heatmap of all composite inter-correlations |
| `outputs/H1_scatterplot.png` | SMuse vs TravelIntent |
| `outputs/H2_scatterplot.png` | Openness vs TravelIntent |
| `outputs/H3_scatterplot.png` | Extraversion vs TravelIntent |
| `outputs/H4_scatterplot.png` | Agreeableness vs TravelIntent |
| `outputs/H5_scatterplot.png` | Conscientiousness vs TravelIntent |
| `outputs/H6_scatterplot.png` | Neuroticism vs TravelIntent |

---

## Analysis Decisions

- **Reverse scoring:** Q11, Q13, Q15, Q17, Q19 are negatively-worded BFI-2 items. Reversed with `6 − score` before composites are computed.
- **Correlation method (Spearman throughout):** All six hypothesis tests use Spearman's rho. Composites are means of ordinal Likert items, so the resulting scale is at best interval-like and parametric assumptions are not guaranteed even when Shapiro-Wilk does not reject normality. Spearman is the standard recommendation for Likert-derived composites (Field, 2018). Using a single consistent method also avoids post-hoc method-switching that inflates Type I error. Shapiro-Wilk results are reported in the script output for transparency only and do **not** drive method selection.
- **Multiple comparisons (Holm correction):** Six simultaneous tests are conducted (H1–H6). Holm's sequential Bonferroni correction is applied to control the family-wise error rate. Both raw and Holm-adjusted p-values are reported in `outputs/hypothesis_results.csv`.
- **Reliability — multi-item scales:** Cronbach's α for SMuse (4 items) and TravelIntent (5 items). Threshold α > 0.70.
- **Reliability — 2-item BFI scales:** Spearman-Brown (SB) coefficient is used instead of α for the five 2-item Big Five composites. SB = (2·r) / (1 + r), where r is the inter-item Pearson correlation. Threshold SB > 0.70. Note that α is mathematically identical to SB for 2-item scales only when items have equal variance, which is not guaranteed; SB is the more appropriate measure (Eisinga, Grotenhuis, & Pelzer, 2013).
- **Missing data:** Composites are set to NA if a respondent has more than one missing item in the underlying scale (rather than imputing from partial items).
