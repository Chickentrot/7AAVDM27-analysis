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

# Ensure outputs/ directory exists
dir.create("outputs", showWarnings = FALSE)

# Load data
df <- read_csv("data/data.csv", show_col_types = FALSE)
cat("Data loaded:", nrow(df), "rows,", ncol(df), "columns\n\n")


# ── Section 2: Reverse Scoring ───────────────────────────────
# Q10 ("I have few artistic interests") and Q12 ("I am reserved") are
# negatively-worded BFI items. On a 1–5 scale, a high raw score means
# LOW openness / LOW extraversion. We reverse them so that higher scores
# consistently indicate MORE of the trait across all items.
df <- df %>%
  mutate(
    Q10_r = 6 - Q10,   # reverse: now high = many artistic interests (high Openness)
    Q12_r = 6 - Q12    # reverse: now high = outgoing/sociable (high Extraversion)
  )
cat("Reverse scoring applied: Q10_r = 6 - Q10, Q12_r = 6 - Q12\n\n")


# ── Section 3: Composite Scores ──────────────────────────────
df <- df %>%
  mutate(
    SMuse       = rowMeans(select(., Q1, Q2, Q3, Q4),         na.rm = TRUE),
    TravelIntent = rowMeans(select(., Q5, Q6, Q7, Q8),        na.rm = TRUE),
    Openness    = rowMeans(select(., Q9, Q10_r),               na.rm = TRUE),
    Extraversion = rowMeans(select(., Q11, Q12_r),             na.rm = TRUE)
  )
cat("Composites created: SMuse (Q1–Q4), TravelIntent (Q5–Q8),",
    "Openness (Q9, Q10_r), Extraversion (Q11, Q12_r)\n\n")


# ── Section 4: Descriptive Statistics ────────────────────────
composites <- c("SMuse", "TravelIntent", "Openness", "Extraversion")

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

sw_results <- lapply(composites, function(var) {
  test <- shapiro.test(df[[var]])
  cat(sprintf("  %-14s W = %.4f, p = %.4f  →  %s\n",
              var,
              test$statistic,
              test$p.value,
              ifelse(test$p.value >= 0.05, "Normal (Pearson OK)", "Non-normal (use Spearman)")))
  list(var = var, W = test$statistic, p = test$p.value,
       normal = test$p.value >= 0.05)
})
names(sw_results) <- composites
cat("\n")

# Decide method per hypothesis
method_H1 <- ifelse(sw_results[["SMuse"]]$normal & sw_results[["TravelIntent"]]$normal,
                    "pearson", "spearman")
method_H2 <- ifelse(sw_results[["Openness"]]$normal & sw_results[["TravelIntent"]]$normal,
                    "pearson", "spearman")
method_H3 <- ifelse(sw_results[["Extraversion"]]$normal & sw_results[["TravelIntent"]]$normal,
                    "pearson", "spearman")

cat(sprintf("  H1 method: %s\n", toupper(method_H1)))
cat(sprintf("  H2 method: %s\n", toupper(method_H2)))
cat(sprintf("  H3 method: %s\n\n", toupper(method_H3)))


# ── Section 6: Correlation Tests (APA Reporting) ─────────────

apa_report <- function(test, hypothesis, method) {
  r   <- test$estimate
  p   <- test$p.value
  df_val <- ifelse(!is.null(test$parameter), test$parameter, NA)
  ci  <- test$conf.int  # available for Pearson only

  r_label  <- ifelse(method == "pearson", "r", "r_s")
  p_str    <- ifelse(p < .001, "< .001", sprintf("= .%03d", round(p * 1000)))
  r_str    <- sprintf(".%02d", abs(round(r * 100)))
  r_signed <- ifelse(r >= 0, paste0(".", abs(round(r * 100))),
                              paste0("-.", abs(round(r * 100))))

  if (!is.null(ci) && !anyNA(ci)) {
    ci_str <- sprintf("[%s, %s]",
                      ifelse(ci[1] >= 0,
                             paste0(" .", sprintf("%02d", abs(round(ci[1]*100)))),
                             paste0("-.", sprintf("%02d", abs(round(ci[1]*100))))),
                      ifelse(ci[2] >= 0,
                             paste0(".", sprintf("%02d", abs(round(ci[2]*100)))),
                             paste0("-.", sprintf("%02d", abs(round(ci[2]*100))))))
    apa <- sprintf("%s(%d) = %s, p %s, 95%% CI %s",
                   r_label,
                   as.integer(df_val),
                   ifelse(r >= 0, paste0(".", sprintf("%02d", abs(round(r*100)))),
                                  paste0("-.", sprintf("%02d", abs(round(r*100))))),
                   p_str,
                   ci_str)
  } else {
    apa <- sprintf("%s = %s, p %s (95%% CI not available for Spearman)",
                   r_label,
                   ifelse(r >= 0, paste0(".", sprintf("%02d", abs(round(r*100)))),
                                  paste0("-.", sprintf("%02d", abs(round(r*100))))),
                   p_str)
  }

  sig <- ifelse(p < .05, "SIGNIFICANT", "non-significant")
  cat(sprintf("  %s: %s  [%s]\n\n", hypothesis, apa, sig))

  data.frame(
    Hypothesis = hypothesis,
    Method     = toupper(method),
    r          = round(r, 3),
    df         = ifelse(!is.na(df_val), as.integer(df_val), NA),
    p          = round(p, 4),
    CI_lower   = ifelse(!is.null(ci) && !anyNA(ci), round(ci[1], 3), NA),
    CI_upper   = ifelse(!is.null(ci) && !anyNA(ci), round(ci[2], 3), NA),
    Significant = p < .05,
    stringsAsFactors = FALSE
  )
}

cat("── Hypothesis Tests ────────────────────────────────────\n")

test_H1 <- cor.test(df$SMuse,       df$TravelIntent, method = method_H1)
test_H2 <- cor.test(df$Openness,    df$TravelIntent, method = method_H2)
test_H3 <- cor.test(df$Extraversion, df$TravelIntent, method = method_H3)

res_H1 <- apa_report(test_H1, "H1 (SMuse → TravelIntent)",        method_H1)
res_H2 <- apa_report(test_H2, "H2 (Openness → TravelIntent)",     method_H2)
res_H3 <- apa_report(test_H3, "H3 (Extraversion → TravelIntent)", method_H3)

results_table <- bind_rows(res_H1, res_H2, res_H3)
write_csv(results_table, "outputs/hypothesis_results.csv")
cat("Results exported to outputs/hypothesis_results.csv\n\n")


# ── Section 7: Visualisations ────────────────────────────────

# 7a — Correlation matrix heatmap
comp_matrix <- df %>% select(SMuse, TravelIntent, Openness, Extraversion)
cor_mat <- cor(comp_matrix, use = "complete.obs")

png("outputs/correlation_matrix.png", width = 700, height = 600, res = 110)
corrplot(cor_mat,
         method   = "color",
         type     = "upper",
         addCoef.col = "black",
         tl.col   = "black",
         tl.srt   = 45,
         col      = colorRampPalette(c("#4575b4", "white", "#d73027"))(200),
         title    = "Correlation Matrix — Composite Scales",
         mar      = c(0, 0, 2, 0))
dev.off()
cat("Saved: outputs/correlation_matrix.png\n")

# Shared minimal theme
theme_study <- theme_minimal(base_size = 12) +
  theme(
    plot.title   = element_text(face = "bold", size = 13, hjust = 0.5),
    axis.title   = element_text(size = 11),
    panel.grid.minor = element_blank()
  )

# Helper: scatterplot with regression line + correlation annotation
scatter_plot <- function(data, x_var, y_var, x_lab, y_lab, title, r_val, p_val) {
  p_str <- ifelse(p_val < .001, "< .001", paste0("= ", round(p_val, 3)))
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

p_H1 <- scatter_plot(df, "SMuse", "TravelIntent",
                     "Social Media Use (SMuse)",
                     "Travel Intention (TravelIntent)",
                     "H1: Social Media Use and Travel Intention",
                     test_H1$estimate, test_H1$p.value)
ggsave("outputs/H1_scatterplot.png", p_H1, width = 6, height = 5, dpi = 150)
cat("Saved: outputs/H1_scatterplot.png\n")

p_H2 <- scatter_plot(df, "Openness", "TravelIntent",
                     "Openness to Experience (Openness)",
                     "Travel Intention (TravelIntent)",
                     "H2: Openness and Travel Intention",
                     test_H2$estimate, test_H2$p.value)
ggsave("outputs/H2_scatterplot.png", p_H2, width = 6, height = 5, dpi = 150)
cat("Saved: outputs/H2_scatterplot.png\n")

p_H3 <- scatter_plot(df, "Extraversion", "TravelIntent",
                     "Extraversion",
                     "Travel Intention (TravelIntent)",
                     "H3: Extraversion and Travel Intention",
                     test_H3$estimate, test_H3$p.value)
ggsave("outputs/H3_scatterplot.png", p_H3, width = 6, height = 5, dpi = 150)
cat("Saved: outputs/H3_scatterplot.png\n\n")


# ── Section 8: Reliability (Cronbach's Alpha) ────────────────
cat("── Cronbach's Alpha (α > 0.70 acceptable) ──────────────\n")

alpha_scales <- list(
  SMuse        = df %>% select(Q1, Q2, Q3, Q4),
  TravelIntent = df %>% select(Q5, Q6, Q7, Q8),
  Openness     = df %>% select(Q9, Q10_r),
  Extraversion = df %>% select(Q11, Q12_r)
)

for (scale_name in names(alpha_scales)) {
  a <- psych::alpha(alpha_scales[[scale_name]], warnings = FALSE)
  alpha_val <- a$total$raw_alpha
  flag <- ifelse(alpha_val >= 0.70, "✓ acceptable", "✗ below threshold")
  cat(sprintf("  %-14s α = %.3f  %s\n", scale_name, alpha_val, flag))
}

cat("\n── Analysis complete. All outputs saved to outputs/ ────\n")
