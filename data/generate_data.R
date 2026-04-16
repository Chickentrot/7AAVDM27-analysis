set.seed(42)
n <- 120

# Helper: clamp to 1–5
clamp <- function(x) pmax(1, pmin(5, round(x)))

# Demographics
Q13 <- sample(1:4, n, replace = TRUE, prob = c(0.20, 0.45, 0.25, 0.10))
Q14 <- sample(1:3, n, replace = TRUE, prob = c(0.40, 0.52, 0.08))
Q15 <- sample(1:2, n, replace = TRUE, prob = c(0.90, 0.10))

# Latent factors (standardised)
lat_SM   <- rnorm(n)        # social media use latent
lat_TI   <- rnorm(n)        # travel intention latent
lat_Open <- rnorm(n)        # openness latent
lat_Ext  <- rnorm(n)        # extraversion latent

# H1: r ≈ 0.45 between SMuse and TravelIntent
lat_TI <- 0.45 * lat_SM + sqrt(1 - 0.45^2) * lat_TI

# H2: r ≈ 0.35 between Openness and TravelIntent (partially through lat_TI)
lat_TI <- lat_TI + 0.20 * lat_Open   # modest additional boost
lat_TI <- lat_TI / sd(lat_TI)         # re-standardise

# H3: extraversion near-zero correlation with TravelIntent (r ≈ 0.10)
lat_TI <- lat_TI + 0.10 * lat_Ext
lat_TI <- lat_TI / sd(lat_TI)

# Manifest items: scale latent to ~mean 3, sd 0.8
make_items <- function(lat, n_items, noise_sd = 0.6) {
  lapply(1:n_items, function(i) clamp(round(3 + 0.8 * lat + rnorm(n, 0, noise_sd))))
}

SM_items      <- make_items(lat_SM,   4, 0.55)
TI_items      <- make_items(lat_TI,   4, 0.55)
Open_items_raw <- make_items(lat_Open, 2, 0.65)
Ext_items_raw  <- make_items(lat_Ext,  2, 0.65)

Q1  <- SM_items[[1]];  Q2  <- SM_items[[2]]
Q3  <- SM_items[[3]];  Q4  <- SM_items[[4]]
Q5  <- TI_items[[1]];  Q6  <- TI_items[[2]]
Q7  <- TI_items[[3]];  Q8  <- TI_items[[4]]
Q9  <- Open_items_raw[[1]]
# Q10 stored as ALREADY reverse-keyed (so high = high openness)
# But in raw survey Q10 is negatively-worded → store raw (1–5) and let analysis.R reverse it
Q10 <- clamp(6 - Open_items_raw[[2]])  # store as raw survey response (low raw = high openness)
Q11 <- Ext_items_raw[[1]]
Q12 <- clamp(6 - Ext_items_raw[[2]])   # store as raw survey response

df <- data.frame(Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9,Q10,Q11,Q12,Q13,Q14,Q15)
write.csv(df, "data/data.csv", row.names = FALSE)
cat("data.csv written:", nrow(df), "rows\n")
