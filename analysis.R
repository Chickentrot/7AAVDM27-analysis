# ============================================================
#  7AAVDM27 Quantitative Research Analysis
#  Social Media Travel Content & Beach Destination Intention
#  KCL MSc Digital Economy
# ============================================================

# ── Section 1: Setup ─────────────────────────────────────────
required_packages <- c("tidyverse", "psych", "corrplot")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if (length(new_packages)) install.packages(new_packages, repos = "https://cran.rstudio.com/")

library(tidyverse)
library(psych)
library(corrplot)

dir.create("outputs", showWarnings = FALSE)

df <- read_csv("data/data.csv", show_col_types = FALSE)
cat("Data loaded:", nrow(df), "rows,", ncol(df), "columns\n\n")


# ── Section 2: Reverse Scoring ───────────────────────────────
# Five items are negatively worded in the BFI-2 short form:
#   Q11 — "has few artistic interests"          → low raw = high Openness
#   Q13 — "is reserved"                         → low raw = high Extraversion
#   Q15 — "starts arguments with others"        → low raw = high Agreeableness
#   Q17 — "tends to be disorganised"            → low raw = high Conscientiousness
#   Q19 — "is relaxed, handles stress well"     → low raw = high Neuroticism
# Reversing with (6 - score) so all items point in the same direction as the trait.
df <- df %>%
  mutate(
    Q11_r = 6 - Q11,
    Q13_r = 6 - Q13,
    Q15_r = 6 - Q15,
    Q17_r = 6 - Q17,
    Q19_r = 6 - Q19
  )
cat("Reverse scoring applied: Q11_r, Q13_r, Q15_r, Q17_r, Q19_r = 6 - raw\n\n")


# ── Section 3: Composite Scores ──────────────────────────────
df <- df %>%
  mutate(
    SMuse             = rowMeans(select(., Q1, Q2, Q3, Q4),              na.rm = TRUE),
    TravelIntent      = rowMeans(select(., Q5, Q6, Q7, Q8, Q9),          na.rm = TRUE),
    Openness          = rowMeans(select(., Q10, Q11_r),                   na.rm = TRUE),
    Extraversion      = rowMeans(select(., Q12, Q13_r),                   na.rm = TRUE),
    Agreeableness     = rowMeans(select(., Q14, Q15_r),                   na.rm = TRUE),
    Conscientiousness = rowMeans(select(., Q16, Q17_r),                   na.rm = TRUE),
    Neuroticism       = rowMeans(select(., Q18, Q19_r),                   na.rm = TRUE)
  )

composites <- c("SMuse", "TravelIntent", "Openness", "Extraversion",
                "Agreeableness", "Conscientiousness", "Neuroticism")

cat("Composites created:\n")
cat("  SMuse             — Q1, Q2, Q3, Q4\n")
cat("  TravelIntent      — Q5, Q6, Q7, Q8, Q9\n")
cat("  Openness          — Q10, Q11_r\n")
cat("  Extraversion      — Q12, Q13_r\n")
cat("  Agreeableness     — Q14, Q15_r\n")
cat("  Conscientiousness — Q16, Q17_r\n")
cat("  Neuroticism       — Q18, Q19_r\n\n")


# ── Section 4: Descriptive Statistics ────────────────────────
desc_stats <- df %>%
  select(all_of(composites)) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Value") %>%
  group_by(Variable) %>%
  summarise(
    n    = n(),
    Mean = round(mean(Value, na.rm = TRUE), 2),
    SD   = round(sd(Value,   na.rm = TRUE), 2),
    Min  = round(min(Value,  na.rm = TRUE), 2),
    Max  = round(max(Value,  na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  arrange(match(Variable, composites))

cat("── Descriptive Statistics ──────────────────────────────\n")
print(as.data.frame(desc_stats), row.names = FALSE)
cat("\n")

write_csv(desc_stats, "outputs/descriptives.csv")
cat("Descriptives exported to outputs/descriptives.csv\n\n")


# ── Section 5: Normality Tests (Shapiro-Wilk) ────────────────
cat("── Shapiro-Wilk Normality Tests ────────────────────────\n")

sw_results <- setNames(
  lapply(composites, function(var) {
    test <- shapiro.test(df[[var]])
    cat(sprintf("  %-20s W = %.4f, p = %.4f  →  %s\n",
                var, test$statistic, test$p.value,
                ifelse(test$p.value >= 0.05, "Normal (Pearson OK)", "Non-normal (use Spearman)")))
    list(var = var, W = test$statistic, p = test$p.value, normal = test$p.value >= 0.05)
  }),
  composites
)
cat("\n")

# Choose Pearson or Spearman per hypothesis based on normality of both variables
cor_method <- function(x_var) {
  ifelse(sw_results[[x_var]]$normal & sw_results[["TravelIntent"]]$normal,
         "pearson", "spearman")
}

methods <- setNames(lapply(composites[composites != "TravelIntent"], cor_method),
                    composites[composites != "TravelIntent"])

cat("  Correlation methods selected:\n")
for (h in seq_along(methods)) {
  cat(sprintf("    H%d (%s): %s\n", h, names(methods)[h], toupper(methods[[h]])))
}
cat("\n")


# ── Section 6: Correlation Tests & APA Reporting ─────────────

apa_report <- function(test, hypothesis, method) {
  r      <- as.numeric(test$estimate)
  p      <- test$p.value
  df_val <- if (!is.null(test$parameter)) as.integer(test$parameter) else NA_integer_
  ci     <- test$conf.int

  r_label <- ifelse(method == "pearson", "r", "r_s")
  p_str   <- ifelse(p < .001, "< .001", sprintf("= .%03d", round(p * 1000)))
  r_fmt   <- function(v) ifelse(v >= 0,
                                 paste0( ".", sprintf("%02d", abs(round(v * 100)))),
                                 paste0("-.", sprintf("%02d", abs(round(v * 100)))))

  if (!is.null(ci) && !anyNA(ci)) {
    apa <- sprintf("%s(%d) = %s, p %s, 95%% CI [%s, %s]",
                   r_label, df_val, r_fmt(r), p_str, r_fmt(ci[1]), r_fmt(ci[2]))
  } else {
    apa <- sprintf("%s = %s, p %s (95%% CI not available for Spearman)",
                   r_label, r_fmt(r), p_str)
  }

  sig <- ifelse(p < .05, "SIGNIFICANT", "non-significant")
  cat(sprintf("  %s:\n    %s  [%s]\n\n", hypothesis, apa, sig))

  data.frame(
    Hypothesis  = hypothesis,
    Method      = toupper(method),
    r           = round(r, 3),
    df          = df_val,
    p           = round(p, 4),
    CI_lower    = if (!is.null(ci) && !anyNA(ci)) round(ci[1], 3) else NA_real_,
    CI_upper    = if (!is.null(ci) && !anyNA(ci)) round(ci[2], 3) else NA_real_,
    Significant = p < .05,
    stringsAsFactors = FALSE
  )
}

cat("── Hypothesis Tests ────────────────────────────────────\n\n")

hyp_pairs <- list(
  list(x = "SMuse",             label = "H1 (SMuse → TravelIntent)"),
  list(x = "Openness",          label = "H2 (Openness → TravelIntent)"),
  list(x = "Extraversion",      label = "H3 (Extraversion → TravelIntent)"),
  list(x = "Agreeableness",     label = "H4 (Agreeableness → TravelIntent)"),
  list(x = "Conscientiousness", label = "H5 (Conscientiousness → TravelIntent)"),
  list(x = "Neuroticism",       label = "H6 (Neuroticism → TravelIntent)")
)

test_results <- list()
result_rows  <- list()

for (hp in hyp_pairs) {
  m <- methods[[hp$x]]
  t <- cor.test(df[[hp$x]], df$TravelIntent, method = m)
  test_results[[hp$x]] <- t
  result_rows[[hp$x]]  <- apa_report(t, hp$label, m)
}

results_table <- bind_rows(result_rows)
write_csv(results_table, "outputs/hypothesis_results.csv")
cat("Results exported to outputs/hypothesis_results.csv\n\n")


# ── Section 7: Visualisations ────────────────────────────────

# 7a — Correlation matrix heatmap (all 7 composites)
cor_mat <- cor(df[, composites], use = "complete.obs")

png("outputs/correlation_matrix.png", width = 820, height = 720, res = 110)
corrplot(cor_mat,
         method      = "color",
         type        = "upper",
         addCoef.col = "black",
         number.cex  = 0.75,
         tl.col      = "black",
         tl.srt      = 45,
         col         = colorRampPalette(c("#4575b4", "white", "#d73027"))(200),
         title       = "Correlation Matrix — Composite Scales",
         mar         = c(0, 0, 2, 0))
dev.off()
cat("Saved: outputs/correlation_matrix.png\n")

# Shared minimal theme
theme_study <- theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 13, hjust = 0.5),
    axis.title       = element_text(size = 11),
    panel.grid.minor = element_blank()
  )

# Helper: scatterplot with regression line + r/p annotation
scatter_plot <- function(data, x_var, y_var, x_lab, y_lab, title, r_val, p_val) {
  p_str <- ifelse(p_val < .001, "< .001", paste0("= ", sprintf("%.3f", p_val)))
  label <- sprintf("r = %.2f, p %s", r_val, p_str)

  ggplot(data, aes(x = .data[[x_var]], y = .data[[y_var]])) +
    geom_jitter(alpha = 0.45, width = 0.08, height = 0.08,
                colour = "#2c7bb6", size = 1.8) +
    geom_smooth(method = "lm", se = TRUE, colour = "#d73027",
                fill = "#f4a582", linewidth = 0.9) +
    annotate("text", x = Inf, y = -Inf, label = label,
             hjust = 1.1, vjust = -0.8, size = 3.8, colour = "grey30") +
    labs(title = title, x = x_lab, y = y_lab) +
    scale_x_continuous(breaks = 1:5) +
    scale_y_continuous(breaks = 1:5) +
    coord_cartesian(xlim = c(0.7, 5.3), ylim = c(0.7, 5.3)) +
    theme_study
}

scatter_specs <- list(
  list(x = "SMuse",             x_lab = "Social Media Use (SMuse)",
       title = "H1: Social Media Use and Travel Intention",      file = "H1_scatterplot.png"),
  list(x = "Openness",          x_lab = "Openness to Experience",
       title = "H2: Openness and Travel Intention",              file = "H2_scatterplot.png"),
  list(x = "Extraversion",      x_lab = "Extraversion",
       title = "H3: Extraversion and Travel Intention",          file = "H3_scatterplot.png"),
  list(x = "Agreeableness",     x_lab = "Agreeableness",
       title = "H4: Agreeableness and Travel Intention",         file = "H4_scatterplot.png"),
  list(x = "Conscientiousness", x_lab = "Conscientiousness",
       title = "H5: Conscientiousness and Travel Intention",     file = "H5_scatterplot.png"),
  list(x = "Neuroticism",       x_lab = "Neuroticism",
       title = "H6: Neuroticism and Travel Intention",           file = "H6_scatterplot.png")
)

for (sp in scatter_specs) {
  t    <- test_results[[sp$x]]
  plot <- scatter_plot(df, sp$x, "TravelIntent",
                       sp$x_lab, "Travel Intention (TravelIntent)",
                       sp$title,
                       as.numeric(t$estimate), t$p.value)
  out_path <- file.path("outputs", sp$file)
  ggsave(out_path, plot, width = 6, height = 5, dpi = 150)
  cat("Saved:", out_path, "\n")
}
cat("\n")


# ── Section 8: Reliability (Cronbach's Alpha) ────────────────
cat("── Cronbach's Alpha (α > 0.70 acceptable) ──────────────\n")

alpha_scales <- list(
  SMuse             = df %>% select(Q1, Q2, Q3, Q4),
  TravelIntent      = df %>% select(Q5, Q6, Q7, Q8, Q9),
  Openness          = df %>% select(Q10, Q11_r),
  Extraversion      = df %>% select(Q12, Q13_r),
  Agreeableness     = df %>% select(Q14, Q15_r),
  Conscientiousness = df %>% select(Q16, Q17_r),
  Neuroticism       = df %>% select(Q18, Q19_r)
)

for (scale_name in names(alpha_scales)) {
  a         <- psych::alpha(alpha_scales[[scale_name]], warnings = FALSE)
  alpha_val <- a$total$raw_alpha
  flag      <- ifelse(alpha_val >= 0.70, "✓ acceptable", "✗ below threshold — interpret with caution")
  cat(sprintf("  %-20s α = %.3f  %s\n", scale_name, alpha_val, flag))
}

cat("\n── Analysis complete. All outputs saved to outputs/ ────\n")
