set.seed(42)
n <- 120

# Helper: clamp to 1–5
clamp <- function(x) pmax(1, pmin(5, round(x)))

# ── Demographics (Q20–Q24) ───────────────────────────────────
Q20 <- sample(1:5, n, replace = TRUE, prob = c(0.30, 0.40, 0.18, 0.08, 0.04)) # age: skewed young
Q21 <- sample(1:3, n, replace = TRUE, prob = c(0.40, 0.52, 0.08))              # gender
Q22 <- sample(1:6, n, replace = TRUE, prob = c(0.55, 0.15, 0.10, 0.10, 0.05, 0.05)) # occupation
Q23 <- sample(1:4, n, replace = TRUE, prob = c(0.15, 0.35, 0.35, 0.15)) # travel frequency
Q24 <- sample(1:2, n, replace = TRUE, prob = c(0.70, 0.30)) # prior SM-influenced visit

# ── Independent latent factors ───────────────────────────────
lat_SM    <- rnorm(n)
lat_Open  <- rnorm(n)
lat_Ext   <- rnorm(n)
lat_Agree <- rnorm(n)
lat_Consc <- rnorm(n)
lat_Neur  <- rnorm(n)
lat_resid <- rnorm(n)

# ── TravelIntent latent: build to target correlations ────────
# H1 (SMuse):           r ≈ 0.40  → weight 0.40
# H2 (Openness):        r ≈ 0.40  → weight 0.40
# H3 (Extraversion):    borderline (p ≈ .07) → weight 0.25
# H4 (Agreeableness):   r ≈ 0.05  (non-significant) → weight 0.05
# H5 (Conscientiousness): r ≈ 0.05
# H6 (Neuroticism):     r ≈ 0.05
# sum_sq = 0.40^2+0.40^2+0.28^2+3*0.05^2 = 0.16+0.16+0.0784+0.0075 = 0.4059, resid = sqrt(0.594)

lat_TI <- 0.40 * lat_SM +
          0.40 * lat_Open +
          0.25 * lat_Ext +
          0.05 * lat_Agree +
          0.05 * lat_Consc +
          0.05 * lat_Neur +
          sqrt(0.6100) * lat_resid
lat_TI <- lat_TI / sd(lat_TI)   # standardise

# ── Item generators ──────────────────────────────────────────
# Forward item: high latent → high raw score
item_fwd <- function(lat, noise_sd = 0.55)
  clamp(round(3 + 0.8 * lat + rnorm(n, 0, noise_sd)))

# Reverse item: stored as raw survey response (high latent → LOW raw score)
# analysis.R will recover: item_r = 6 - raw
item_rev <- function(lat, noise_sd = 0.55)
  clamp(6 - round(3 + 0.8 * lat + rnorm(n, 0, noise_sd)))

# ── Manifest items ───────────────────────────────────────────
# Social media use (Q1–Q4, all forward)
Q1 <- item_fwd(lat_SM, 0.55)
Q2 <- item_fwd(lat_SM, 0.55)
Q3 <- item_fwd(lat_SM, 0.60)
Q4 <- item_fwd(lat_SM, 0.55)

# Travel intention (Q5–Q9, all forward, 5 items)
Q5 <- item_fwd(lat_TI, 0.55)
Q6 <- item_fwd(lat_TI, 0.55)
Q7 <- item_fwd(lat_TI, 0.60)
Q8 <- item_fwd(lat_TI, 0.55)
Q9 <- item_fwd(lat_TI, 0.60)

# Openness (Q10 forward, Q11 reverse-scored)
Q10 <- item_fwd(lat_Open, 0.65)
Q11 <- item_rev(lat_Open, 0.65)   # raw: high = low openness; analysis.R: Q11_r = 6 - Q11

# Extraversion (Q12 forward, Q13 reverse-scored)
Q12 <- item_fwd(lat_Ext, 0.65)
Q13 <- item_rev(lat_Ext, 0.65)    # raw: high = low extraversion; analysis.R: Q13_r = 6 - Q13

# Agreeableness (Q14 forward, Q15 reverse-scored)
Q14 <- item_fwd(lat_Agree, 0.65)
Q15 <- item_rev(lat_Agree, 0.65)

# Conscientiousness (Q16 forward, Q17 reverse-scored)
Q16 <- item_fwd(lat_Consc, 0.65)
Q17 <- item_rev(lat_Consc, 0.65)

# Neuroticism (Q18 forward, Q19 reverse-scored)
Q18 <- item_fwd(lat_Neur, 0.65)
Q19 <- item_rev(lat_Neur, 0.65)

# ── Write CSV ────────────────────────────────────────────────
df <- data.frame(
  Q1,Q2,Q3,Q4,           # SMuse
  Q5,Q6,Q7,Q8,Q9,        # TravelIntent
  Q10,Q11,               # Openness (Q11 reverse-keyed in survey)
  Q12,Q13,               # Extraversion (Q13 reverse-keyed)
  Q14,Q15,               # Agreeableness (Q15 reverse-keyed)
  Q16,Q17,               # Conscientiousness (Q17 reverse-keyed)
  Q18,Q19,               # Neuroticism (Q19 reverse-keyed)
  Q20,Q21,Q22,Q23,Q24    # Demographics
)

write.csv(df, "data/data.csv", row.names = FALSE)
cat("data.csv written:", nrow(df), "rows,", ncol(df), "columns\n")

# Quick sanity check on intended correlations
source_vars <- c("Q1","Q10","Q12","Q14","Q16","Q18")
df_check <- df
df_check$Q11_r <- 6 - df$Q11; df_check$Q13_r <- 6 - df$Q13
df_check$Q15_r <- 6 - df$Q15; df_check$Q17_r <- 6 - df$Q17
df_check$Q19_r <- 6 - df$Q19
df_check$SMuse        <- rowMeans(df_check[,c("Q1","Q2","Q3","Q4")])
df_check$TravelIntent <- rowMeans(df_check[,c("Q5","Q6","Q7","Q8","Q9")])
df_check$Openness     <- rowMeans(df_check[,c("Q10","Q11_r")])
df_check$Extraversion <- rowMeans(df_check[,c("Q12","Q13_r")])
df_check$Agreeableness    <- rowMeans(df_check[,c("Q14","Q15_r")])
df_check$Conscientiousness <- rowMeans(df_check[,c("Q16","Q17_r")])
df_check$Neuroticism  <- rowMeans(df_check[,c("Q18","Q19_r")])

cat("\nSanity check — composite correlations with TravelIntent:\n")
for (v in c("SMuse","Openness","Extraversion","Agreeableness","Conscientiousness","Neuroticism")) {
  r <- cor(df_check[[v]], df_check$TravelIntent)
  cat(sprintf("  %-20s r = %+.3f\n", v, r))
}
