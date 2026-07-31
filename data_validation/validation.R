# =========================================================
# POLARIS vs Web: agreement analysis
# Columns expected:
# data, source, query, collected_at, api_total, web_total
# =========================================================

pkgs <- c("ggplot2", "dplyr", "readr", "tibble")
new_pkgs <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(new_pkgs) > 0) install.packages(new_pkgs, dependencies = TRUE)

library(ggplot2)
library(dplyr)
library(readr)
library(tibble)

# ---------------------------------------------------------
# 1) LOAD DATA
# ---------------------------------------------------------
df <- read_csv("C:/Users/ldoprado/OneDrive - Arizona State University/Documents/Phd/Polaris/Validation/data_validation2/Validation_2.csv")

# If needed, inspect:
# str(df)
# head(df)

# ---------------------------------------------------------
# 2) CLEANING
# ---------------------------------------------------------
df2 <- df %>%
  mutate(
    base_dados = as.character(base_dados),
    grupo_pasta = as.character(grupo_pasta),
    query = as.character(query),
    api_total = as.numeric(api_total),
    web_total = as.numeric(web_total)
  ) %>%
  filter(!is.na(api_total), !is.na(web_total))

# ---------------------------------------------------------
# 3) BASIC METRICS
# ---------------------------------------------------------
cat("\n=============================\n")
cat("OVERALL SUMMARY\n")
cat("=============================\n")

cor_orig <- cor(df2$web_total, df2$api_total, method = "pearson")
lm_orig  <- lm(api_total ~ web_total, data = df2)
r2_orig  <- summary(lm_orig)$r.squared

cat(sprintf("Pearson correlation (original scale): %.4f\n", cor_orig))
cat(sprintf("R-squared (original scale): %.4f\n", r2_orig))

if (nrow(df_log) > 1) {
  cor_log <- cor(log10(df_log$web_total), log10(df_log$api_total), method = "pearson")
  lm_log  <- lm(log10(api_total) ~ log10(web_total), data = df_log)
  r2_log  <- summary(lm_log)$r.squared
  
  cat(sprintf("Pearson correlation (log10 scale): %.4f\n", cor_log))
  cat(sprintf("R-squared (log10 scale): %.4f\n", r2_log))
} else {
  cor_log <- NA
  r2_log <- NA
  cat("Not enough positive values for log10 analysis.\n")
}

# ---------------------------------------------------------
# 4) BLAND-ALTMAN (RAW SCALE)
# ---------------------------------------------------------
df_ba <- df2 %>%
  mutate(
    mean_methods = (api_total + web_total) / 2,
    diff_raw     = api_total - web_total,
    pct_diff     = 100 * (api_total - web_total) / web_total
  )

bias_raw  <- mean(df_ba$diff_raw)
sd_raw    <- sd(df_ba$diff_raw)
loa_low   <- bias_raw - 1.96 * sd_raw
loa_high  <- bias_raw + 1.96 * sd_raw

cat("\n=============================\n")
cat("BLAND-ALTMAN (RAW SCALE)\n")
cat("=============================\n")
cat(sprintf("Bias (mean difference): %.4f\n", bias_raw))
cat(sprintf("SD of differences: %.4f\n", sd_raw))
cat(sprintf("Lower LoA: %.4f\n", loa_low))
cat(sprintf("Upper LoA: %.4f\n", loa_high))

# ---------------------------------------------------------
# 5) BLAND-ALTMAN (LOG10 SCALE)
# ---------------------------------------------------------
if (nrow(df_log) > 1) {
  df_log_ba <- df_log %>%
    mutate(
      mean_log = (log10(api_total) + log10(web_total)) / 2,
      diff_log = log10(api_total) - log10(web_total)
    )
  
  bias_log <- mean(df_log_ba$diff_log)
  sd_log   <- sd(df_log_ba$diff_log)
  loa_low_log  <- bias_log - 1.96 * sd_log
  loa_high_log <- bias_log + 1.96 * sd_log
  
  cat("\n=============================\n")
  cat("BLAND-ALTMAN (LOG10 SCALE)\n")
  cat("=============================\n")
  cat(sprintf("Bias (mean log difference): %.6f\n", bias_log))
  cat(sprintf("SD of log differences: %.6f\n", sd_log))
  cat(sprintf("Lower LoA: %.6f\n", loa_low_log))
  cat(sprintf("Upper LoA: %.6f\n", loa_high_log))
}

# ---------------------------------------------------------
# 6) PROPORTIONAL DEVIATIONS
# ---------------------------------------------------------
cat("\n=============================\n")
cat("PROPORTIONAL DEVIATIONS\n")
cat("=============================\n")

prop_summary <- df_ba %>%
  summarise(
    mean_pct   = mean(pct_diff),
    median_pct = median(pct_diff),
    sd_pct     = sd(pct_diff),
    q1_pct     = quantile(pct_diff, 0.25),
    q3_pct     = quantile(pct_diff, 0.75),
    min_pct    = min(pct_diff),
    max_pct    = max(pct_diff)
  )

print(prop_summary)

# ---------------------------------------------------------
# 7) PLOTS - OVERALL
# ---------------------------------------------------------
p_scatter <- ggplot(df2, aes(x = web_total, y = api_total)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "POLARIS/API vs Web",
    subtitle = sprintf("R² = %.4f | Pearson r = %.4f", r2_orig, cor_orig),
    x = "Web total",
    y = "API total"
  ) +
  theme_minimal()

print(p_scatter)

p_ba_raw <- ggplot(df_ba, aes(x = mean_methods, y = diff_raw)) +
  geom_point(alpha = 0.7) +
  geom_hline(yintercept = bias_raw) +
  geom_hline(yintercept = loa_low, linetype = "dashed") +
  geom_hline(yintercept = loa_high, linetype = "dashed") +
  labs(
    title = "Bland-Altman Plot (Raw Scale)",
    subtitle = sprintf("Bias = %.2f | LoA = [%.2f, %.2f]", bias_raw, loa_low, loa_high),
    x = "Mean of the two methods",
    y = "API total - Web total"
  ) +
  theme_minimal()

print(p_ba_raw)

if (nrow(df_log) > 1) {
  p_ba_log <- ggplot(df_log_ba, aes(x = mean_log, y = diff_log)) +
    geom_point(alpha = 0.7) +
    geom_hline(yintercept = bias_log) +
    geom_hline(yintercept = loa_low_log, linetype = "dashed") +
    geom_hline(yintercept = loa_high_log, linetype = "dashed") +
    labs(
      title = "Bland-Altman Plot (log10 scale)",
      subtitle = sprintf("Bias = %.4f | LoA = [%.4f, %.4f]", bias_log, loa_low_log, loa_high_log),
      x = "Mean of log10 values",
      y = "log10(API total) - log10(Web total)"
    ) +
    theme_minimal()
  
  print(p_ba_log)
}

p_pct <- ggplot(df_ba, aes(x = pct_diff)) +
  geom_histogram(bins = 30) +
  geom_vline(xintercept = mean(df_ba$pct_diff)) +
  geom_vline(xintercept = median(df_ba$pct_diff), linetype = "dotted") +
  labs(
    title = "Distribution of Proportional Deviations",
    subtitle = sprintf("Mean = %.2f%% | Median = %.2f%%", mean(df_ba$pct_diff), median(df_ba$pct_diff)),
    x = "Proportional deviation (%)",
    y = "Count"
  ) +
  theme_minimal()

print(p_pct)

# ---------------------------------------------------------
# 8) OPTIONAL: ANALYSIS BY SOURCE
# ---------------------------------------------------------
source_summary <- df2 %>%
  group_by(base_dados) %>%
  summarise(
    n = n(),
    pearson_r = cor(web_total, api_total, method = "pearson"),
    r2 = summary(lm(api_total ~ web_total))$r.squared,
    mean_diff = mean(api_total - web_total),
    median_pct_diff = median(100 * (api_total - web_total) / web_total),
    .groups = "drop"
  )

print(source_summary)

# Optional save
# write.csv(source_summary, "source_summary.csv", row.names = FALSE)

library(dplyr)
library(ggplot2)
library(patchwork)
library(ggplot2)
library(dplyr)
library(gridExtra)

#=========================================================
# Data preparation
#=========================================================

df_ba <- df2 %>%
  mutate(
    mean_methods = (api_total + web_total)/2,
    diff_raw     = api_total - web_total,
    pct_diff     = 100*(api_total - web_total)/web_total
  )

bias <- mean(df_ba$diff_raw)
loa.low  <- bias - 1.96*sd(df_ba$diff_raw)
loa.high <- bias + 1.96*sd(df_ba$diff_raw)

#=========================================================
# Figure A - Scatter plot
#=========================================================

p1 <- ggplot(df2,
             aes(web_total,
                 api_total)) +
  
  geom_point(size=2.8,
             alpha=.8,
             color="#8C1D40") +
  
  geom_abline(intercept=0,
              slope=1,
              linetype=2,
              linewidth=.8,
              color="grey40") +
  
  geom_smooth(method="lm",
              se=FALSE,
              linewidth=.9,
              color="#FFC627") +
  
  labs(
    title="(A) Agreement between POLARIS and Web Interface",
    x="Records retrieved from Web interface",
    y="Records retrieved by POLARIS"
  )+
  
  theme_bw(base_size=12)

#=========================================================
# Figure B - Bland Altman
#=========================================================

p2 <- ggplot(df_ba,
             aes(mean_methods,
                 diff_raw))+
  
  geom_point(size=2.6,
             alpha=.8,
             color="#8C1D40")+
  
  geom_hline(yintercept=bias,
             linewidth=.8)+
  
  geom_hline(yintercept=loa.low,
             linetype=2)+
  
  geom_hline(yintercept=loa.high,
             linetype=2)+
  
  annotate("text",
           x=max(df_ba$mean_methods),
           y=bias,
           label="Bias",
           hjust=1.1)+
  
  labs(
    title="(B) Bland–Altman agreement",
    x="Mean of Web and POLARIS",
    y="Difference (POLARIS − Web)"
  )+
  
  theme_bw(base_size=12)

#=========================================================
# Figure C - Relative deviations
#=========================================================

p3 <- ggplot(df_ba,
             aes(pct_diff))+
  
  geom_histogram(
    bins=20,
    fill="#8C1D40",
    color="white"
  )+
  
  geom_vline(
    xintercept=mean(df_ba$pct_diff),
    linewidth=.8
  )+
  
  geom_vline(
    xintercept=median(df_ba$pct_diff),
    linetype=2,
    linewidth=.8
  )+
  
  labs(
    title="(C) Distribution of proportional deviations",
    x="Relative difference (%)",
    y="Frequency"
  )+
  
  theme_bw(base_size=12)

#=========================================================
# Figure D - Relative deviations by database
#=========================================================

p4 <- ggplot(df_ba,
             aes(base_dados,
                 pct_diff,
                 fill=base_dados))+
  
  geom_boxplot(alpha=.8,
               outlier.shape=16)+
  
  labs(
    title="(D) Relative deviations by database",
    x="Database",
    y="Relative difference (%)"
  )+
  
  theme_bw(base_size=12)+
  
  theme(
    legend.position="none"
  )

#=========================================================
# Final figure
#=========================================================

grid.arrange(
  p1,
  p2,
  p3,
  p4,
  ncol=2
)

# Optional:
# ggsave("Figure_agreement_POLARIS.png",
#        width=12,
#        height=9,
#        dpi=600)
# =========================================================
# Assume df2 already exists with:
# base_dados, api_total, web_total
# =========================================================

# Keep only valid rows
df_fig <- df2 %>%
  mutate(
    api_total  = as.numeric(api_total),
    web_total  = as.numeric(web_total),
    base_dados = as.character(base_dados)
  ) %>%
  filter(!is.na(api_total), !is.na(web_total))

# Positive values for log analysis
df_log <- df_fig %>%
  filter(api_total > 0, web_total > 0) %>%
  mutate(
    mean_log = (log10(api_total) + log10(web_total)) / 2,
    diff_log = log10(api_total) - log10(web_total),
    pct_diff = 100 * (api_total - web_total) / web_total
  )

# For scatter and proportional deviations
df_fig <- df_fig %>%
  mutate(
    pct_diff = 100 * (api_total - web_total) / web_total
  )

# Agreement stats
bias_log <- mean(df_log$diff_log)
sd_log   <- sd(df_log$diff_log)
loa_low_log  <- bias_log - 1.96 * sd_log
loa_high_log <- bias_log + 1.96 * sd_log

# =========================================================
# (A) Scatter plot with identity line + regression line
# =========================================================
p1 <- ggplot(df_fig, aes(x = web_total, y = api_total)) +
  geom_point(alpha = 0.8, size = 2.6) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", linewidth = 0.8, color = "grey40") +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.9) +
  labs(
    title = "(A) POLARIS vs Web",
    x = "Web total",
    y = "POLARIS total"
  ) +
  theme_bw(base_size = 12)

# =========================================================
# (B) Bland–Altman on log10 scale
# =========================================================
p2 <- ggplot(df_log, aes(x = mean_log, y = diff_log)) +
  geom_point(alpha = 0.8, size = 2.6) +
  geom_hline(yintercept = bias_log, linewidth = 0.8) +
  geom_hline(yintercept = loa_low_log, linetype = "dashed", linewidth = 0.8) +
  geom_hline(yintercept = loa_high_log, linetype = "dashed", linewidth = 0.8) +
  labs(
    title = "(B) Bland–Altman (log10 scale)",
    x = "Mean of log10(POLARIS, Web)",
    y = "log10(POLARIS) - log10(Web)"
  ) +
  theme_bw(base_size = 12)

# =========================================================
# (C) Distribution of proportional deviations
# =========================================================
p3 <- ggplot(df_fig, aes(x = pct_diff)) +
  geom_histogram(bins = 20, color = "white") +
  geom_vline(xintercept = mean(df_fig$pct_diff, na.rm = TRUE), linewidth = 0.8) +
  geom_vline(xintercept = median(df_fig$pct_diff, na.rm = TRUE), linetype = "dashed", linewidth = 0.8) +
  labs(
    title = "(C) Proportional deviations",
    x = "Relative difference (%)",
    y = "Frequency"
  ) +
  theme_bw(base_size = 12)

# =========================================================
# (D) Proportional deviations by database
# =========================================================
p4 <- ggplot(df_fig, aes(x = base_dados, y = pct_diff, fill = base_dados)) +
  geom_boxplot(alpha = 0.8, outlier.shape = 16) +
  labs(
    title = "(D) Deviations by database",
    x = "Database",
    y = "Relative difference (%)"
  ) +
  theme_bw(base_size = 12) +
  theme(legend.position = "none")

# =========================================================
# Combine plots
# =========================================================
final_fig <- (p1 | p2) / (p3 | p4)

print(final_fig)

# Optional export
ggsave(
  filename = "POLARIS_agreement_figure.png",
  plot = final_fig,
  width = 13,
  height = 10,
  dpi = 600
)