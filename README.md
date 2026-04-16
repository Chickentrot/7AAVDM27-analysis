# 7AAVDM27 – Social Media Travel Content & Beach Destination Intention

**Module:** 7AAVDM27 Quantitative Research Methods — KCL MSc Digital Economy

---

## Research Question

Is self-reported consumption of social media travel content about beach destinations associated with young people's intention to visit those destinations, and does personality (openness, extraversion, agreeableness, conscientiousness, neuroticism) predict travel intention?

---

## Hypotheses

| # | Hypothesis |
|---|------------|
| H1 | Higher social media travel content consumption is positively correlated with intention to visit beach destinations |
| H2 | Higher openness to experience is positively correlated with intention to visit beach destinations |
| H3 | Higher extraversion is positively correlated with intention to visit beach destinations |
| H4 | Higher agreeableness is positively correlated with intention to visit beach destinations |
| H5 | Higher conscientiousness is positively correlated with intention to visit beach destinations |
| H6 | Higher neuroticism is negatively correlated with intention to visit beach destinations |

---

## Variable Codebook

### Likert items (1 = Strongly Disagree, 5 = Strongly Agree unless noted)

| Variable | Item | Scale | Notes |
|----------|------|-------|-------|
| Q1 | How often do you view travel content on social media? | 1=Never … 5=Very often | Ordinal; SMuse composite |
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
| Q17 | I see myself as someone who tends to be disorganised | 1–5 | Conscientiousness — **REVERSE SCORED** (Q17_r = 6 − Q17) |
| Q18 | I see myself as someone who worries a lot | 1–5 | Neuroticism — normal scoring |
| Q19 | I see myself as someone who is relaxed and handles stress well | 1–5 | Neuroticism — **REVERSE SCORED** (Q19_r = 6 − Q19) |

### Demographic items

| Variable | Item | Categories |
|----------|------|------------|
| Q20 | Age | 1=16–18, 2=19–21, 3=22–24, 4=25–27, 5=28+ |
| Q21 | Gender | 1=Male, 2=Female, 3=Prefer not to say |
| Q22 | Occupation | 1=Undergraduate student, 2=Postgraduate student, 3=Employed full-time, 4=Employed part-time, 5=Self-employed, 6=Other |
| Q23 | Travel frequency (holidays per year) | 1=Never, 2=Once, 3=2–3 times, 4=4 or more |
| Q24 | Have you ever visited a destination after seeing it on social media? | 1=Yes, 2=No |

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

The script expects `data/data.csv` in the project root. A synthetic dummy dataset (n=120) is included for testing. **When real survey data is ready, replace `data/data.csv` with the exported CSV keeping the same column names Q1–Q24.**

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
- **Normality:** Shapiro-Wilk on each composite. If either variable in a pair is non-normal (p < .05), Spearman's rho is used; otherwise Pearson's r. With Likert-scale composites, Spearman is typical.
- **Reliability:** Cronbach's α reported per composite. Threshold α > 0.70. 2-item scales may fall below this; flag and interpret accordingly.
- **Dummy data:** Synthetic data generated with `data/generate_data.R` (seed=42). H1 r≈.47, H2 r≈.45, H3 r≈.17 (borderline), H4–H6 non-significant. Replace with real data before drawing conclusions.
