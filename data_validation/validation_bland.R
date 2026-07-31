# =========================================================
# POLARIS vs Web - Agreement analysis with improved figures
# =========================================================

# ---------------------------------------------------------
# 0) PACKAGES
# ---------------------------------------------------------
pkgs <- c("ggplot2", "dplyr", "readr", "patchwork", "scales")
new_pkgs <- pkgs[!pkgs %in% installed.packages()[, "Package"]]

if (length(new_pkgs) > 0) {
  install.packages(new_pkgs, dependencies = TRUE)
}

library(ggplot2)
library(dplyr)
library(readr)
library(patchwork)
library(scales)

# ---------------------------------------------------------
# 1) SETTINGS
# ---------------------------------------------------------
file_path <- "C:/Users/ldoprado/OneDrive - Arizona State University/Documents/Phd/Polaris/Validation/data_validation2/Validation_2.csv"

main_color   <- "#2C7FB8"
accent_color  <- "#D95F0E"
bias_color    <- "#B2182B"
grid_color    <- "grey85"
border_color  <- "grey35"

# ---------------------------------------------------------
# 2) CUSTOM THEME
# ---------------------------------------------------------
theme_agreement <- function(base_size = 13) {
  theme_classic(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", size = base_size + 1, color = "black"),
      plot.subtitle = element_text(size = base_size - 1, color = "grey30"),
      axis.title = element_text(face = "bold", color = "black"),
      axis.text = element_text(color = "black"),
      axis.line = element_line(color = border_color, linewidth = 0.4),
      axis.ticks = element_line(color = border_color, linewidth = 0.4),
      panel.border = element_rect(color = border_color, fill = NA, linewidth = 0.5),
      panel.grid.major.y = element_line(color = grid_color, linewidth = 0.3),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "none",
      plot.margin = margin(10, 12, 10, 12)
    )
}

# ---------------------------------------------------------
# 3) LOAD DATA
# ---------------------------------------------------------
df <- read_csv(file_path, show_col_types = FALSE)

# ---------------------------------------------------------
# 4) CLEANING
# ---------------------------------------------------------
df2 <- df %>%
  mutate(
    base_dados = as.character(base_dados),
    grupo_pasta = as.character(grupo_pasta),
    query = as.character(query),
    api_total = suppressWarnings(as.numeric(api_total)),
    web_total = suppressWarnings(as.numeric(web_total))
  ) %>%
  filter(!is.na(api_total), !is.na(web_total))

# ---------------------------------------------------------
# 5) DATA FOR ANALYSES
# ---------------------------------------------------------
# Raw Bland-Altman data
df_ba <- df2 %>%
  mutate(
    mean_methods = (api_total + web_total) / 2,
    diff_raw     = api_total - web_total,
    pct_diff     = if_else(web_total != 0, 100 * (api_total - web_total) / web_total, NA_real_)
  ) %>%
  filter(!is.na(pct_diff))

# Log10 data: only positive values
df_log <- df2 %>%
  filter(api_total > 0, web_total > 0) %>%
  mutate(
    mean_log = (log10(api_total) + log10(web_total)) / 2,
    diff_log = log10(api_total) - log10(web_total)
  )

# ---------------------------------------------------------
# 6) BASIC METRICS
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
# 7) BLAND-ALTMAN (RAW SCALE)
# ---------------------------------------------------------
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
# 8) BLAND-ALTMAN (LOG10 SCALE)
# ---------------------------------------------------------
if (nrow(df_log) > 1) {
  bias_log <- mean(df_log$diff_log)
  sd_log   <- sd(df_log$diff_log)
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
# 9) PROPORTIONAL DEVIATIONS
# ---------------------------------------------------------
cat("\n=============================\n")
cat("PROPORTIONAL DEVIATIONS\n")
cat("=============================\n")

prop_summary <- df_ba %>%
  summarise(
    mean_pct   = mean(pct_diff, na.rm = TRUE),
    median_pct = median(pct_diff, na.rm = TRUE),
    sd_pct     = sd(pct_diff, na.rm = TRUE),
    q1_pct     = quantile(pct_diff, 0.25, na.rm = TRUE),
    q3_pct     = quantile(pct_diff, 0.75, na.rm = TRUE),
    min_pct    = min(pct_diff, na.rm = TRUE),
    max_pct    = max(pct_diff, na.rm = TRUE)
  )

print(prop_summary)

# ---------------------------------------------------------
# 10) OPTIONAL: SUMMARY BY SOURCE
# ---------------------------------------------------------
source_summary <- df2 %>%
  group_by(base_dados) %>%
  summarise(
    n = n(),
    pearson_r = cor(web_total, api_total, method = "pearson"),
    r2 = summary(lm(api_total ~ web_total))$r.squared,
    mean_diff = mean(api_total - web_total),
    median_pct_diff = median(100 * (api_total - web_total) / web_total, na.rm = TRUE),
    .groups = "drop"
  )

print(source_summary)

# ---------------------------------------------------------
# 11) FIGURE A - SCATTER PLOT
# ---------------------------------------------------------
x_text <- min(df2$web_total, na.rm = TRUE)
y_text <- max(df2$api_total, na.rm = TRUE)

p1 <- ggplot(df2, aes(x = web_total, y = api_total)) +
  geom_point(
    shape = 21,
    size = 2.8,
    stroke = 0.35,
    fill = main_color,
    color = "black",
    alpha = 0.85
  ) +
  geom_abline(
    intercept = 0,
    slope = 1,
    linetype = "dashed",
    linewidth = 0.7,
    color = "grey45"
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    linewidth = 0.95,
    color = accent_color,
    fill = "grey80"
  ) +
  annotate(
    "text",
    x = x_text,
    y = y_text,
    label = sprintf("R² = %.3f\nr = %.3f", r2_orig, cor_orig),
    hjust = 0,
    vjust = 1.1,
    size = 4.0,
    fontface = "bold",
    color = "grey20"
  ) +
  labs(
    title = "(A) Agreement between POLARIS and Web",
    subtitle = "Scatter plot with identity line and linear fit",
    x = "Web total",
    y = "POLARIS total"
  ) +
  theme_agreement()

# ---------------------------------------------------------
# 12) FIGURE B - BLAND-ALTMAN (RAW SCALE)
# ---------------------------------------------------------
p2 <- ggplot(df_ba, aes(mean_methods, diff_raw)) +
  geom_point(
    shape = 21,
    size = 2.6,
    stroke = 0.35,
    fill = main_color,
    color = "black",
    alpha = 0.8
  ) +
  geom_hline(yintercept = bias_raw, linewidth = 0.9, color = bias_color) +
  geom_hline(yintercept = loa_low, linetype = 2, linewidth = 0.8, color = "grey40") +
  geom_hline(yintercept = loa_high, linetype = 2, linewidth = 0.8, color = "grey40") +
  annotate(
    "text",
    x = max(df_ba$mean_methods, na.rm = TRUE) * 0.89,
    y = loa_high -10,
    label = sprintf("LoA: [%.2f, %.2f]", loa_low, loa_high),
    hjust = 0.5,
    vjust = 1,
    size = 3.6,
    color = "grey25"
  ) +
  annotate(
    "text",
    x = max(df_ba$mean_methods, na.rm = TRUE) * 0.92,
    y = bias_raw +10,
    label = sprintf("Bias = %.2f", bias_raw),
    hjust = 0.5,
    vjust = -0.7,
    size = 4.0,
    fontface = "bold",
    color = bias_color
  ) +
  labs(
    title = "(B) Bland–Altman agreement",
    subtitle = "Raw scale",
    x = "Mean of Web and POLARIS",
    y = "Difference (POLARIS - Web)"
  ) +
  scale_x_continuous(expand = expansion(mult = c(0.03, 0.08))) +
  coord_cartesian(clip = "off") +
  theme_agreement() +
  theme(
    plot.margin = margin(10, 18, 10, 10)
  )
# ---------------------------------------------------------
# 13) FIGURE C - DISTRIBUTION OF PROPORTIONAL DEVIATIONS
# ---------------------------------------------------------
p3 <- ggplot(df_ba, aes(x = pct_diff)) +
  geom_histogram(
    bins = 24,
    fill = main_color,
    color = "white",
    alpha = 0.9
  ) +
  geom_density(
    aes(y = after_stat(count)),
    linewidth = 0.9,
    color = accent_color,
    alpha = 0.4
  ) +
  geom_vline(
    xintercept = mean(df_ba$pct_diff, na.rm = TRUE),
    linewidth = 0.9,
    color = bias_color
  ) +
  geom_vline(
    xintercept = median(df_ba$pct_diff, na.rm = TRUE),
    linetype = "dashed",
    linewidth = 0.8,
    color = "grey40"
  ) +
  labs(
    title = "(C) Distribution of proportional deviations",
    subtitle = sprintf("Mean = %.2f%% | Median = %.2f%%",
                       mean(df_ba$pct_diff, na.rm = TRUE),
                       median(df_ba$pct_diff, na.rm = TRUE)),
    x = "Relative difference (%)",
    y = "Count"
  ) +
  theme_agreement()

# ---------------------------------------------------------
# 14) FIGURE D - DEVIATIONS BY DATABASE
# ---------------------------------------------------------
df_ba <- df_ba %>%
  mutate(base_dados = factor(base_dados))

p4 <- ggplot(df_ba, aes(x = reorder(base_dados, pct_diff, FUN = median), y = pct_diff)) +
  geom_boxplot(
    width = 0.55,
    outlier.shape = NA,
    fill = main_color,
    alpha = 0.45,
    color = border_color
  ) +
  geom_jitter(
    width = 0.14,
    alpha = 0.55,
    size = 1.8,
    color = accent_color
  ) +
  labs(
    title = "(D) Relative deviations by database",
    subtitle = "Boxplot with individual observations",
    x = "Database",
    y = "Relative difference (%)"
  ) +
  theme_agreement() +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1)
  )

# ---------------------------------------------------------
# 15) OPTIONAL: LOG-SCALE BLAND-ALTMAN
# ---------------------------------------------------------
if (nrow(df_log) > 1) {
  x_log_text <- max(df_log$mean_log, na.rm = TRUE)
  
  p5 <- ggplot(df_log, aes(x = mean_log, y = diff_log)) +
    geom_point(
      shape = 21,
      size = 2.8,
      stroke = 0.35,
      fill = main_color,
      color = "black",
      alpha = 0.8
    ) +
    geom_hline(
      yintercept = bias_log,
      linewidth = 0.9,
      color = bias_color
    ) +
    geom_hline(
      yintercept = loa_low_log,
      linetype = "dashed",
      linewidth = 0.8,
      color = "grey40"
    ) +
    geom_hline(
      yintercept = loa_high_log,
      linetype = "dashed",
      linewidth = 0.8,
      color = "grey40"
    ) +
    annotate(
      "text",
      x = x_log_text,
      y = bias_log,
      label = sprintf("Bias = %.4f", bias_log),
      hjust = 1.05,
      vjust = -0.7,
      size = 4.0,
      fontface = "bold",
      color = bias_color
    ) +
    labs(
      title = "(E) Bland–Altman agreement",
      subtitle = "Log10 scale",
      x = "Mean of log10(POLARIS, Web)",
      y = "log10(POLARIS) - log10(Web)"
    ) +
    theme_agreement()
}

# ---------------------------------------------------------
# 16) COMBINE FIGURES
# ---------------------------------------------------------
if (nrow(df_log) > 1) {
  final_fig <- (p1 | p2) / (p3 | p4) / p5
} else {
  final_fig <- (p1 | p2) / (p3 | p4)
}

print(final_fig)

# ---------------------------------------------------------
# 17) EXPORT
# ---------------------------------------------------------
ggsave(
  filename = "POLARIS_agreement_figure.png",
  plot = final_fig,
  width = 11,
  height = ifelse(nrow(df_log) > 1, 11, 8),
  dpi = 300
)

# Optional: save source summary
# write.csv(source_summary, "source_summary.csv", row.names = FALSE)