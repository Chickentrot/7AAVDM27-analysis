# 7AAVDM27 – Social Media Travel Content & Beach Destination Intention

**Module:** 7AAVDM27 Quantitative Research Methods — KCL MSc Digital Economy

---

## Research Question

Is self-reported consumption of social media travel content about beach destinations associated with young people's intention to visit those destinations, and does personality (openness, extraversion) predict travel intention?

---

## Hypotheses

| # | Hypothesis | Expected direction |
|---|------------|--------------------|
| H1 | Higher social media travel content consumption is positively correlated with intention to visit beach destinations | Positive, significant |
| H2 | Higher openness to experience is positively correlated with intention to visit beach destinations | Positive, significant |
| H3 | Higher extraversion is positively correlated with intention to visit beach destinations | Positive (exploratory) |

---

## Variable Codebook

| Variable | Survey item | Scale | Notes |
|----------|-------------|-------|-------|
| Q1 | How often do you view travel content on social media? | 1=Never … 5=Very often | Ordinal; included in SMuse composite |
| Q2 | I actively engage with travel content | 1–5 Likert | |
| Q3 | I frequently see social media travel content about beach destinations | 1–5 Likert | |
| Q4 | I follow travel influencers or travel-related accounts | 1–5 Likert | |
| Q5 | Social media travel content increases my interest in travelling to beach destinations | 1–5 Likert | |
| Q6 | Social media travel content increases my desire to travel to beach destinations | 1–5 Likert | |
| Q7 | I am likely to visit a beach destination I see on social media | 1–5 Likert | |
| Q8 | I would consider booking a beach trip based on social media content | 1–5 Likert | |
| Q9 | I see myself as someone who has an active imagination | 1–5 Likert | Openness item, normal scoring |
| Q10 | I see myself as someone who has few artistic interests | 1–5 Likert | **REVERSE SCORED** — negatively worded; script computes Q10_r = 6 − Q10 |
| Q11 | I see myself as someone who is outgoing, sociable | 1–5 Likert | Extraversion item, normal scoring |
| Q12 | I see myself as someone who is reserved | 1–5 Likert | **REVERSE SCORED** — negatively worded; script computes Q12_r = 6 − Q12 |
| Q13 | Age category | 1=18–20, 2=21–23, 3=24–26, 4=27+ | |
| Q14 | Gender | 1=Male, 2=Female, 3=Prefer not to say | |
| Q15 | Student status | 1=Yes, 2=No | |

### Composite Scales

| Composite | Items | Purpose |
|-----------|-------|---------|
| **SMuse** | Q1, Q2, Q3, Q4 | Social media travel content consumption |
| **TravelIntent** | Q5, Q6, Q7, Q8 | Intention to visit beach destinations |
| **Openness** | Q9, Q10_r | Openness to experience (BFI-derived) |
| **Extraversion** | Q11, Q12_r | Extraversion (BFI-derived) |

---

## How to Run

### 1. Install R packages (once)

```r
install.packages(c("tidyverse", "psych", "corrplot"))
```

Or let `analysis.R` install them automatically — the script checks and installs missing packages on startup.

### 2. Prepare data

The script expects `data/data.csv` relative to the project root. A dummy dataset (n=120) is included for testing. **When real survey data is available, replace `data/data.csv` with the exported CSV from Microsoft Forms/Qualtrics, keeping the same column names (Q1–Q15).**

### 3. Run the analysis

From the `7AAVDM27-analysis/` directory:

```bash
Rscript analysis.R
```

Or open `analysis.R` in RStudio and click **Source**.

### 4. Outputs

| File | Description |
|------|-------------|
| `outputs/descriptives.csv` | Mean, SD, min, max, n for all composites |
| `outputs/hypothesis_results.csv` | APA-format correlation results for H1–H3 |
| `outputs/correlation_matrix.png` | Heatmap of inter-composite correlations |
| `outputs/H1_scatterplot.png` | SMuse vs TravelIntent with regression line |
| `outputs/H2_scatterplot.png` | Openness vs TravelIntent |
| `outputs/H3_scatterplot.png` | Extraversion vs TravelIntent |

---

## Notes on Analysis Decisions

- **Reverse scoring:** Q10 and Q12 are negatively-worded BFI items. They are reversed (6 − score) before composite calculation so that all items point in the same direction.
- **Normality:** Shapiro-Wilk is run on each composite. If p < .05 for either variable in a pair, Spearman's rho is used for that hypothesis; otherwise Pearson's r.
- **Reliability:** Cronbach's alpha is reported for each composite. α > 0.70 is the conventional threshold for acceptable internal consistency.
- **Dummy data:** The included `data.csv` was generated with a fixed seed to produce H1 r ≈ 0.45 (significant), H2 r ≈ 0.35 (significant), H3 r ≈ 0.10 (non-significant). Replace with real data before drawing conclusions.
