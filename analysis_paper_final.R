# ============================================================================
# Empirical Analysis: Impact of U.S. Foreign Policy on HKEX-listed Firms
# Difference-in-Differences model
# Outcome: log(winsorized monthly trading volume)
# Treatment: firms with documented U.S. trade exposure (us_trade = 1)
# Treatment onset: July 2019 (HKHRDA introduction)
# ============================================================================

library(readxl)
library(dplyr)
library(lubridate)
library(fixest)
library(ggplot2)
library(tidyr)
library(broom)

# ============================================================================
# Data preparation
# ============================================================================
excel_file <- "C:/Users/Dmitriy/Desktop/paper/Companies_v5.xlsx"

# Monthly volume panel
df_vol <- read_excel(excel_file, sheet = "forR_m") %>%
  mutate(month = dmy(month), year = year(month))

winsorize <- function(x, lo = 0.01, hi = 0.99) {
  q <- quantile(x, c(lo, hi), na.rm = TRUE)
  pmax(pmin(x, q[2]), q[1])
}

df_vol <- df_vol %>%
  mutate(
    value_w   = winsorize(value),
    log_vol   = log(value_w + 1),
    treated   = as.integer(us_trade == 1),
    post      = as.integer(month >= ymd("2019-07-01")),
    is_fin    = as.integer(sector == "Financials"),
    month_fct = as.factor(format(month, "%Y-%m"))
  )

# Annual aggregation
df_yr <- df_vol %>%
  group_by(company, year, us_trade, loc_China, sector) %>%
  summarise(log_ann_vol = log(sum(value_w, na.rm = TRUE) + 1), .groups = "drop") %>%
  mutate(
    treated  = as.integer(us_trade == 1),
    yr_fct   = as.factor(year),
    is_fin   = as.integer(sector == "Financials")
  )

# Monthly price panel (robustness)
df_price <- read_excel(excel_file, sheet = "forR_price_m") %>%
  mutate(
    month     = dmy(month),
    year      = year(month),
    treated   = as.integer(us_trade == 1),
    post      = as.integer(month >= ymd("2019-07-01")),
    is_fin    = as.integer(sector == "Financials"),
    month_fct = as.factor(format(month, "%Y-%m")),
    log_price = log(avg_price)
  )

df_price_yr <- df_price %>%
  group_by(company, year, us_trade, loc_China, sector) %>%
  summarise(log_ann_price = log(mean(avg_price, na.rm = TRUE)), .groups = "drop") %>%
  mutate(treated = as.integer(us_trade == 1), yr_fct = as.factor(year),
         is_fin = as.integer(sector == "Financials"))

# Extraction helper
get_effects <- function(model, ref = 2018) {
  tidy(model) %>%
    filter(grepl("yr_fct::.*:treated", term)) %>%
    mutate(year = as.numeric(gsub("yr_fct::([0-9]+):treated", "\\1", term)),
           ci_lo = estimate - 1.96 * std.error,
           ci_hi = estimate + 1.96 * std.error) %>%
    bind_rows(data.frame(year = ref, estimate = 0, std.error = 0,
                         p.value = NA, ci_lo = 0, ci_hi = 0)) %>%
    arrange(year)
}

# ============================================================================
# Diagnostics
# ============================================================================

pre_bal <- df_yr %>% filter(year %in% c(2017, 2018)) %>%
  group_by(company, treated) %>% summarise(v = mean(log_ann_vol), .groups = "drop")
bal_test <- t.test(v ~ treated, data = pre_bal)

# ============================================================================
# CASE STUDY: Year-by-year treatment effects
# ============================================================================
m_event <- feols(log_ann_vol ~ i(yr_fct, treated, ref = "2018") | company + year,
                 data = df_yr, cluster = ~company)

effects <- get_effects(m_event) %>% mutate(pct = (exp(estimate) - 1) * 100)


cat("\n--- Case Study: yearly effects (Table for Section 3.2) ---\n")
print(effects %>% select(year, estimate, std.error, p.value, pct) %>%
        mutate(across(where(is.numeric), ~round(., 4))))

# Pooled ATT
m_att <- feols(log_vol ~ i(treated, post, ref = 0) | company + month_fct,
               data = df_vol, cluster = ~company)
att <- tidy(m_att) %>% filter(grepl("treated::1", term))


cat(sprintf("\n--- Pooled ATT: %.4f (SE = %.4f, p = %.4f) → %.1f%% ---\n",
            att$estimate, att$std.error, att$p.value, (exp(att$estimate) - 1) * 100))


m_att_esc <- feols(log_vol ~ i(treated, post, ref = 0) | company + month_fct,
                   data = filter(df_vol, year <= 2023), cluster = ~company)
att_esc <- tidy(m_att_esc) %>% filter(grepl("treated::1", term))

cat(sprintf("--- Escalation ATT: %.4f (SE = %.4f, p = %.4f) → %.1f%% ---\n",
            att_esc$estimate, att_esc$std.error, att_esc$p.value,
            (exp(att_esc$estimate) - 1) * 100))

# Trend tests
td <- effects %>% filter(year >= 2019, std.error > 0)
lm_full <- lm(estimate ~ year, data = td, weights = 1 / std.error^2)
lm_esc  <- lm(estimate ~ year, data = filter(td, year <= 2023),
               weights = 1 / filter(td, year <= 2023)$std.error^2)
lm_rev  <- lm(estimate ~ year, data = filter(td, year >= 2023),
               weights = 1 / filter(td, year >= 2023)$std.error^2)

cat("\n--- Trend tests (report in H3 section) ---\n")
cat(sprintf("Full period:  slope = %.4f, p = %.4f\n",
            coef(lm_full)["year"], summary(lm_full)$coefficients["year", "Pr(>|t|)"]))
cat(sprintf("Escalation:   slope = %.4f, p = %.4f\n",
            coef(lm_esc)["year"], summary(lm_esc)$coefficients["year", "Pr(>|t|)"]))
cat(sprintf("Reversal:     slope = %.4f, p = %.4f\n",
            coef(lm_rev)["year"], summary(lm_rev)$coefficients["year", "Pr(>|t|)"]))

# --- Figure 1: Event study plot ---
fig1 <- ggplot(effects, aes(x = year, y = estimate)) +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), alpha = 0.14, fill = "#8B0000") +
  geom_line(color = "#8B0000", linewidth = 0.7) +
  geom_point(size = 2.8, color = "#8B0000") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_vline(xintercept = 2018.5, linetype = "dotted", color = "grey50") +
  scale_x_continuous(breaks = effects$year) +
  labs(x = "Year", y = "Treatment effect (log scale)") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        plot.margin = margin(10, 15, 10, 10))
print(fig1)

# ============================================================================
# HYPOTHESIS 1: China-located firms more resistant
# ============================================================================
m_h1 <- feols(log_ann_vol ~ i(yr_fct, treated, ref = "2018") +
                i(yr_fct, treated, ref = "2018"):loc_China | company + year,
              data = df_yr, cluster = ~company)

h1_coefs <- tidy(m_h1) %>%
  filter(grepl(":loc_China", term)) %>%
  mutate(year = as.numeric(gsub(".*yr_fct::([0-9]+).*", "\\1", term)),
         sig  = case_when(p.value < 0.01 ~ "***", p.value < 0.05 ~ "**",
                          p.value < 0.10 ~ "*", TRUE ~ ""))

#H1 triple interaction table
cat("\n--- H1: Triple interaction coefficients (Table) ---\n")
print(h1_coefs %>% filter(year >= 2019) %>%
        select(year, estimate, std.error, p.value, sig) %>%
        mutate(across(where(is.numeric), ~round(., 4))))

# Pooled H1
m_h1_pool <- feols(log_vol ~ i(treated, post, ref = 0) +
                     i(treated, post, ref = 0):loc_China | company + month_fct,
                   data = df_vol, cluster = ~company)
h1_pool <- tidy(m_h1_pool) %>% filter(grepl(":loc_China", term))

cat(sprintf("\n--- H1 pooled: %.4f (SE = %.4f, p = %.4f) → %.1f%% ---\n",
            h1_pool$estimate, h1_pool$std.error, h1_pool$p.value,
            (exp(h1_pool$estimate) - 1) * 100))

# Subgroup averages
m_china    <- feols(log_ann_vol ~ i(yr_fct, treated, ref = "2018") | company + year,
                    data = filter(df_yr, loc_China == 1), cluster = ~company)
m_nonchina <- feols(log_ann_vol ~ i(yr_fct, treated, ref = "2018") | company + year,
                    data = filter(df_yr, loc_China == 0), cluster = ~company)

avg_ch <- tidy(m_china) %>% filter(grepl("yr_fct", term)) %>%
  mutate(y = as.numeric(gsub(".*::([0-9]+).*", "\\1", term))) %>%
  filter(y >= 2019) %>% summarise(m = mean(estimate))
avg_nc <- tidy(m_nonchina) %>% filter(grepl("yr_fct", term)) %>%
  mutate(y = as.numeric(gsub(".*::([0-9]+).*", "\\1", term))) %>%
  filter(y >= 2019) %>% summarise(m = mean(estimate))

cat(sprintf("--- China-located avg effect: %.4f (%.1f%%)\n", avg_ch$m, (exp(avg_ch$m)-1)*100))
cat(sprintf("--- Non-China avg effect:     %.4f (%.1f%%)\n", avg_nc$m, (exp(avg_nc$m)-1)*100))

# --- Figure 2: H1 plot ---
eff_h1 <- bind_rows(
  get_effects(m_china) %>% mutate(group = "China-located firms"),
  get_effects(m_nonchina) %>% mutate(group = "Non-China firms"))

fig2 <- ggplot(eff_h1, aes(x = year, y = estimate, color = group, fill = group)) +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), alpha = 0.10, color = NA) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 2.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_vline(xintercept = 2018.5, linetype = "dotted", color = "grey50") +
  scale_color_manual(values = c("China-located firms" = "#B22222",
                                 "Non-China firms" = "#1F4E79")) +
  scale_fill_manual(values = c("China-located firms" = "#B22222",
                                "Non-China firms" = "#1F4E79")) +
  scale_x_continuous(breaks = unique(eff_h1$year)) +
  labs(x = "Year", y = "Treatment effect (log scale)", color = NULL, fill = NULL) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        legend.position = c(0.25, 0.20),
        legend.background = element_rect(fill = "white", color = NA),
        plot.margin = margin(10, 15, 10, 10))
print(fig2)

# ============================================================================
# HYPOTHESIS 2: Financial sector more sensitive
# ============================================================================
m_h2 <- feols(log_ann_vol ~ i(yr_fct, treated, ref = "2018") +
                i(yr_fct, treated, ref = "2018"):is_fin | company + year,
              data = df_yr, cluster = ~company)

h2_coefs <- tidy(m_h2) %>%
  filter(grepl(":is_fin", term)) %>%
  mutate(year = as.numeric(gsub(".*yr_fct::([0-9]+).*", "\\1", term)),
         sig  = case_when(p.value < 0.01 ~ "***", p.value < 0.05 ~ "**",
                          p.value < 0.10 ~ "*", TRUE ~ ""))

#H2 triple interaction table
cat("\n--- H2: Triple interaction coefficients (Table) ---\n")
print(h2_coefs %>% filter(year >= 2019) %>%
        select(year, estimate, std.error, p.value, sig) %>%
        mutate(across(where(is.numeric), ~round(., 4))))

# Pooled H2
m_h2_pool <- feols(log_vol ~ i(treated, post, ref = 0) +
                     i(treated, post, ref = 0):is_fin | company + month_fct,
                   data = df_vol, cluster = ~company)
h2_pool <- tidy(m_h2_pool) %>% filter(grepl(":is_fin", term))

cat(sprintf("\n--- H2 pooled: %.4f (SE = %.4f, p = %.4f) → %.1f%% ---\n",
            h2_pool$estimate, h2_pool$std.error, h2_pool$p.value,
            (exp(h2_pool$estimate) - 1) * 100))

# Subgroup averages
m_fin  <- feols(log_ann_vol ~ i(yr_fct, treated, ref = "2018") | company + year,
                data = filter(df_yr, is_fin == 1), cluster = ~company)
m_nfin <- feols(log_ann_vol ~ i(yr_fct, treated, ref = "2018") | company + year,
                data = filter(df_yr, is_fin == 0), cluster = ~company)

avg_f <- tidy(m_fin) %>% filter(grepl("yr_fct", term)) %>%
  mutate(y = as.numeric(gsub(".*::([0-9]+).*", "\\1", term))) %>%
  filter(y >= 2019) %>% summarise(m = mean(estimate))
avg_nf <- tidy(m_nfin) %>% filter(grepl("yr_fct", term)) %>%
  mutate(y = as.numeric(gsub(".*::([0-9]+).*", "\\1", term))) %>%
  filter(y >= 2019) %>% summarise(m = mean(estimate))

cat(sprintf("--- Financial avg effect:     %.4f (%.1f%%)\n", avg_f$m, (exp(avg_f$m)-1)*100))
cat(sprintf("--- Non-Financial avg effect: %.4f (%.1f%%)\n", avg_nf$m, (exp(avg_nf$m)-1)*100))

# --- Figure 3: H2 plot ---
eff_h2 <- bind_rows(
  get_effects(m_fin) %>% mutate(group = "Financial sector"),
  get_effects(m_nfin) %>% mutate(group = "Non-financial sectors"))

fig3 <- ggplot(eff_h2, aes(x = year, y = estimate, color = group, fill = group)) +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), alpha = 0.10, color = NA) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 2.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_vline(xintercept = 2018.5, linetype = "dotted", color = "grey50") +
  scale_color_manual(values = c("Financial sector" = "#6A0DAD",
                                 "Non-financial sectors" = "#228B22")) +
  scale_fill_manual(values = c("Financial sector" = "#6A0DAD",
                                "Non-financial sectors" = "#228B22")) +
  scale_x_continuous(breaks = unique(eff_h2$year)) +
  labs(x = "Year", y = "Treatment effect (log scale)", color = NULL, fill = NULL) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        legend.position = c(0.25, 0.20),
        legend.background = element_rect(fill = "white", color = NA),
        plot.margin = margin(10, 15, 10, 10))
print(fig3)

# ============================================================================
# HYPOTHESIS 4: Event-specific DiD
# ============================================================================
events <- data.frame(
  date_text = c("01.07.2019", "01.11.2019", "01.05.2020", "01.07.2020",
                "01.08.2020", "01.01.2021", "01.04.2025", "01.05.2025",
                "01.07.2025", "01.08.2025", "01.11.2025"),
  label = c("HKHRDA intro", "HKHRDA signed", "Status revocation",
            "EO 13936", "Made-in-China rule", "Inv. blacklist EO 14032",
            "2025 tariffs (Apr)", "2025 tariffs (May)", "2025 tariffs (Jul)",
            "2025 tariffs (Aug)", "2025 tariffs (Nov)"),
  stringsAsFactors = FALSE
) %>% mutate(event_date = dmy(date_text), event_order = row_number())

ev_results <- data.frame()
for (i in seq_len(nrow(events))) {
  ed <- events$event_date[i]
  dfe <- df_vol %>%
    filter(month >= ed - months(6), month <= ed + months(6)) %>%
    mutate(post_ev = as.integer(month >= ed))
  if (n_distinct(dfe$company[dfe$treated == 1 & dfe$post_ev == 0]) < 5) next
  mod <- tryCatch(
    feols(log_vol ~ i(treated, post_ev, ref = 0) | company + month,
          data = dfe, cluster = ~company),
    error = function(e) NULL)
  if (is.null(mod)) next
  cn <- grep("treated::1", names(coef(mod)), value = TRUE)[1]
  if (is.na(cn)) next
  est <- coef(mod)[cn]; sse <- se(mod)[cn]
  pv <- 2 * (1 - pnorm(abs(est / sse)))
  ev_results <- rbind(ev_results, data.frame(
    order = events$event_order[i], label = events$label[i],
    did = est, se = sse, p = pv,
    sig = case_when(pv < 0.01 ~ "***", pv < 0.05 ~ "**",
                    pv < 0.10 ~ "*", TRUE ~ ""),
    stringsAsFactors = FALSE))
}

## H4 event table
cat("\n--- H4: Event-specific DiD estimates (Table) ---\n")
print(ev_results %>% mutate(across(c(did, se, p), ~round(., 4))))

# Trend test across events
trend_ev <- lm(did ~ order, data = ev_results, weights = 1 / se^2)
cat(sprintf("\n--- H4 trend: slope = %.4f, p = %.4f ---\n",
            coef(trend_ev)[2], summary(trend_ev)$coefficients[2, 4]))

# --- Figure 4: Event-specific DiD ---
fig4 <- ggplot(ev_results, aes(x = order, y = did)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_errorbar(aes(ymin = did - 1.96 * se, ymax = did + 1.96 * se),
                width = 0.25, color = "grey55") +
  geom_point(aes(shape = p < 0.10), size = 3, color = "#8B0000") +
  scale_shape_manual(values = c("TRUE" = 16, "FALSE" = 1),
                     labels = c("TRUE" = "p < 0.10", "FALSE" = "p ≥ 0.10"),
                     name = NULL) +
  scale_x_continuous(breaks = ev_results$order, labels = ev_results$label) +
  labs(x = NULL, y = "DiD estimate (log scale)") +
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1),
        panel.grid.minor = element_blank(),
        legend.position = c(0.85, 0.90),
        legend.background = element_rect(fill = "white", color = NA),
        plot.margin = margin(10, 15, 10, 10))
print(fig4)

# ============================================================================
# Robustness: Stock price
# ============================================================================
cat("\n--- ROBUSTNESS: Stock price as alternative outcome ---\n")

# Price pooled ATT
m_p_att <- feols(log_price ~ i(treated, post, ref = 0) | company + month_fct,
                 data = df_price, cluster = ~company)
p_att <- tidy(m_p_att) %>% filter(grepl("treated::1", term))

# Price H1
m_p_h1 <- feols(log_price ~ i(treated, post, ref = 0) +
                   i(treated, post, ref = 0):loc_China | company + month_fct,
                 data = df_price, cluster = ~company)
p_h1 <- tidy(m_p_h1) %>% filter(grepl(":loc_China", term))

# Price H2
m_p_h2 <- feols(log_price ~ i(treated, post, ref = 0) +
                   i(treated, post, ref = 0):is_fin | company + month_fct,
                 data = df_price, cluster = ~company)
p_h2 <- tidy(m_p_h2) %>% filter(grepl(":is_fin", term))

# Price H3 trend
m_p_yr <- feols(log_ann_price ~ i(yr_fct, treated, ref = "2018") | company + year,
                data = df_price_yr, cluster = ~company)
eff_p <- get_effects(m_p_yr)
td_p <- eff_p %>% filter(year >= 2019, year <= 2023, std.error > 0)
lm_p_esc <- lm(estimate ~ year, data = td_p, weights = 1 / td_p$std.error^2)
sp <- summary(lm_p_esc)$coefficients

# Price H4 events
ev_p <- data.frame()
for (i in seq_len(nrow(events))) {
  ed <- events$event_date[i]
  dfe <- df_price %>%
    filter(month >= ed - months(6), month <= ed + months(6)) %>%
    mutate(post_ev = as.integer(month >= ed))
  if (n_distinct(dfe$company[dfe$treated == 1 & dfe$post_ev == 0]) < 5) next
  mod <- tryCatch(
    feols(log_price ~ i(treated, post_ev, ref = 0) | company + month,
          data = dfe, cluster = ~company),
    error = function(e) NULL)
  if (is.null(mod)) next
  cn <- grep("treated::1", names(coef(mod)), value = TRUE)[1]
  if (is.na(cn)) next
  est <- coef(mod)[cn]; sse <- se(mod)[cn]
  pv <- 2 * (1 - pnorm(abs(est / sse)))
  ev_p <- rbind(ev_p, data.frame(order = i, label = events$label[i],
    did = est, se = sse, p = pv,
    sig = case_when(pv < 0.01 ~ "***", pv < 0.05 ~ "**",
                    pv < 0.10 ~ "*", TRUE ~ ""),
    stringsAsFactors = FALSE))
}
trend_p <- lm(did ~ order, data = ev_p, weights = 1 / se^2)

## FOR THE PAPER: Comparison table (Table X in Section 3.2)
cat("\n--- Comparison Table: Volume vs Price ---\n")
se_esc <- summary(lm_esc)$coefficients
cat(sprintf("                           VOLUME              PRICE\n"))
cat(sprintf("Pooled ATT:            %+.4f (p=%.3f)     %+.4f (p=%.3f)\n",
            att$estimate, att$p.value, p_att$estimate, p_att$p.value))
cat(sprintf("Escalation ATT:        %+.4f (p=%.3f)     %+.4f (p=%.3f)\n",
            att_esc$estimate, att_esc$p.value,
            tidy(feols(log_price ~ i(treated, post, ref=0) | company + month_fct,
                       data = filter(df_price, year <= 2023), cluster = ~company)) %>%
              filter(grepl("treated::1", term)) %>% pull(estimate),
            tidy(feols(log_price ~ i(treated, post, ref=0) | company + month_fct,
                       data = filter(df_price, year <= 2023), cluster = ~company)) %>%
              filter(grepl("treated::1", term)) %>% pull(p.value)))
cat(sprintf("H1 (loc_China):        %+.4f (p=%.3f)     %+.4f (p=%.3f)\n",
            h1_pool$estimate, h1_pool$p.value, p_h1$estimate, p_h1$p.value))
cat(sprintf("H2 (financial):        %+.4f (p=%.3f)     %+.4f (p=%.3f)\n",
            h2_pool$estimate, h2_pool$p.value, p_h2$estimate, p_h2$p.value))
cat(sprintf("H3 esc. slope:          %.4f (p=%.3f)      %.4f (p=%.3f)\n",
            se_esc["year", "Estimate"], se_esc["year", "Pr(>|t|)"],
            sp["year", "Estimate"], sp["year", "Pr(>|t|)"]))
cat(sprintf("H4 event trend:         %.4f (p=%.3f)      %.4f (p=%.3f)\n",
            coef(trend_ev)[2], summary(trend_ev)$coefficients[2, 4],
            coef(trend_p)[2], summary(trend_p)$coefficients[2, 4]))

# ============================================================================
# Additional robustness checks
# ============================================================================
cat("\n--- Additional robustness ---\n")

# Parallel trends
pt <- tidy(m_event) %>% filter(grepl("2017:treated", term))
cat(sprintf("Parallel trends (2017): coef = %.4f, p = %.3f\n", pt$estimate, pt$p.value))

# Placebo
m_plac <- feols(log_ann_vol ~ i(yr_fct, treated, ref = "2017") | company + year,
                data = df_yr, cluster = ~company)
pl <- tidy(m_plac) %>% filter(grepl("2018:treated", term))
cat(sprintf("Placebo (2018):         coef = %.4f, p = %.3f\n", pl$estimate, pl$p.value))

# Sector × year FE
m_sfe <- feols(log_ann_vol ~ i(yr_fct, treated, ref = "2018") | company + interaction(sector, year),
               data = df_yr, cluster = ~company)
sfe <- get_effects(m_sfe)

cat("\nSector × year FE specification:\n")
print(sfe %>% select(year, estimate, std.error, p.value) %>%
        mutate(across(where(is.numeric), ~round(., 4))))


# ============================================================================
# for Section 3.2
# ============================================================================

library(gridExtra)
library(grid)

tt <- ttheme_minimal(
  core = list(
    fg_params = list(fontsize = 9, fontface = "plain"),
    bg_params = list(fill = c("grey95", "white"), col = "grey80"),
    padding = unit(c(6, 4), "mm")
  ),
  colhead = list(
    fg_params = list(fontsize = 9, fontface = "bold"),
    bg_params = list(fill = "grey80", col = "grey60"),
    padding = unit(c(6, 4), "mm")
  )
)

make_table <- function(df, title, note) {
  title_g <- textGrob(title, gp = gpar(fontsize = 10, fontface = "bold"),
                      x = 0.05, hjust = 0)
  note_g  <- textGrob(note, gp = gpar(fontsize = 7, fontface = "italic", col = "grey40"),
                      x = 0.05, hjust = 0)
  fig <- arrangeGrob(title_g, tableGrob(df, rows = NULL, theme = tt), note_g,
                     ncol = 1, heights = c(0.07, 0.86, 0.05))
  grid.newpage()
  grid.draw(fig)
}

# ============================================================================
# Case Study — Year-by-year treatment effects
# ============================================================================
t1 <- effects %>%
  filter(year >= 2017) %>%
  transmute(
    Year      = as.character(year),
    Coef.     = sprintf("%.4f", estimate),
    SE        = sprintf("%.4f", std.error),
    `p-value` = ifelse(is.na(p.value), "ref.", sprintf("%.3f", p.value)),
    `% Chg.`  = sprintf("%.1f", pct),
    `95% CI`  = ifelse(year == 2018, "[ref.]",
                       sprintf("[%.3f, %.3f]", estimate - 1.96*std.error,
                               estimate + 1.96*std.error))
  )

make_table(t1,
           "Figure 3. Case Study: year-by-year treatment effects on log trading volume",
           "Two-way FE (firm + year). Clustered SE. Reference year: 2018. % Chg. = (exp(coef) - 1) x 100.")

# ============================================================================
# Case Study — Pooled ATT and sign test
# ============================================================================
t2 <- data.frame(
  Specification = c("Pooled ATT (2019–2025)",
                    "Sign test (7/7 negative years)"),
  Coef.     = c(sprintf("%.4f", att$estimate), "—"),
  SE        = c(sprintf("%.4f", att$std.error), "—"),
  `p-value` = c(sprintf("%.3f", att$p.value), "0.008"),
  `% Chg.`  = c(sprintf("%.1f", (exp(att$estimate)-1)*100), "—"),
  check.names = FALSE, stringsAsFactors = FALSE
)

make_table(t2,
           "Figure 4. Case Study: pooled treatment effect estimate",
           "Monthly panel. Firm + month FE. Clustered SE. Sign test: P(7 negative | H0) = 0.5^7 = 0.008.")

# ============================================================================
#  H1 — Triple interaction (yearly)
# ============================================================================
t3 <- h1_coefs %>%
  filter(year >= 2019) %>%
  transmute(
    Year      = as.character(year),
    Coef.     = sprintf("%.4f", estimate),
    SE        = sprintf("%.4f", std.error),
    `p-value` = sprintf("%.3f", p.value),
    Sig.      = sig
  )

make_table(t3,
           "Table 6. Hypothesis 1: triple interaction coefficients (year x treated x loc_China)",
           "Positive = China-located firms more resistant. * p<0.10, ** p<0.05, *** p<0.01.")

# ============================================================================
# H1 — Summary statistics
# ============================================================================
t4 <- data.frame(
  Measure = c("Pooled differential (monthly)",
              "China-located avg. effect",
              "Non-China avg. effect",
              "Difference"),
  Coef.     = c(sprintf("%.4f", h1_pool$estimate),
                sprintf("%.4f", avg_ch$m),
                sprintf("%.4f", avg_nc$m),
                sprintf("%.4f", avg_ch$m - avg_nc$m)),
  `p-value` = c(sprintf("%.3f", h1_pool$p.value), "—", "—", "—"),
  `% Chg.`  = c(sprintf("%.1f", (exp(h1_pool$estimate)-1)*100),
                sprintf("%.1f", (exp(avg_ch$m)-1)*100),
                sprintf("%.1f", (exp(avg_nc$m)-1)*100),
                "—"),
  check.names = FALSE, stringsAsFactors = FALSE
)

make_table(t4,
           "Table 7. Hypothesis 1: summary of location differential",
           "Pooled: monthly panel, firm + month FE. Averages: mean of annual event-study coefficients (2019-2025).")

# ============================================================================
# H2 — Triple interaction (yearly) + summary
# ============================================================================
h2_yearly <- h2_coefs %>%
  filter(year >= 2019) %>%
  transmute(
    Year      = as.character(year),
    Coef.     = sprintf("%.4f", estimate),
    SE        = sprintf("%.4f", std.error),
    `p-value` = sprintf("%.3f", p.value),
    Sig.      = sig
  )

h2_summary <- data.frame(
  Year      = c("Pooled", "Fin. avg.", "Non-fin. avg."),
  Coef.     = c(sprintf("%.4f", h2_pool$estimate),
                sprintf("%.4f", avg_f$m),
                sprintf("%.4f", avg_nf$m)),
  SE        = c(sprintf("%.4f", h2_pool$std.error), "—", "—"),
  `p-value` = c(sprintf("%.3f", h2_pool$p.value), "—", "—"),
  Sig.      = c("", "", ""),
  check.names = FALSE, stringsAsFactors = FALSE
)

t5 <- rbind(h2_yearly, h2_summary)

make_table(t5,
           "Figure 9. Hypothesis 2: triple interaction (year x treated x is_financial) and summary",
           "Negative = financial sector more sensitive. Pooled: monthly panel. Averages: mean of annual coefficients.")

# ============================================================================
# H4 — Event-specific DiD
# ============================================================================
t6_events <- ev_results %>%
  transmute(
    `#`       = as.character(order),
    Event     = label,
    Coef.     = sprintf("%.4f", did),
    SE        = sprintf("%.4f", se),
    `p-value` = sprintf("%.3f", p),
    Sig.      = sig,
    `% Chg.`  = sprintf("%.1f", (exp(did)-1)*100)
  )

t6_trend <- data.frame(
  `#`       = "",
  Event     = "Trend across events",
  Coef.     = sprintf("%.4f", coef(trend_ev)[2]),
  SE        = sprintf("%.4f", summary(trend_ev)$coefficients[2,2]),
  `p-value` = sprintf("%.3f", summary(trend_ev)$coefficients[2,4]),
  Sig.      = "",
  `% Chg.`  = "—",
  check.names = FALSE, stringsAsFactors = FALSE
)

t6 <- rbind(t6_events, t6_trend)

make_table(t6,
           "Figure 12. Hypothesis 4: event-specific DiD estimates and trend",
           "6-month window. Firm + month FE. Clustered SE. Trend: WLS regression of DiD on event order.")

# ============================================================================
# Robustness — Volume vs Price comparison
# ============================================================================
p_att_val <- tidy(feols(log_price ~ i(treated, post, ref = 0) | company + month_fct,
                        data = df_price, cluster = ~company)) %>%
  filter(grepl("treated::1", term))

se_esc_coefs <- summary(lm_esc)$coefficients

t7 <- data.frame(
  Test = c("Pooled ATT",
           "H1: China differential",
           "H2: Financial differential",
           "H3: Full-period trend slope",
           "H4: Event trend"),
  `Vol. Coef.` = sprintf("%+.4f", c(att$estimate,
                                    h1_pool$estimate, h2_pool$estimate,
                                    coef(lm_full)["year"], coef(trend_ev)[2])),
  `Vol. p`     = sprintf("%.3f", c(att$p.value,
                                   h1_pool$p.value, h2_pool$p.value,
                                   summary(lm_full)$coefficients["year","Pr(>|t|)"],
                                   summary(trend_ev)$coefficients[2,4])),
  `Price Coef.`= sprintf("%+.4f", c(p_att_val$estimate,
                                    p_h1$estimate, p_h2$estimate,
                                    sp["year","Estimate"], coef(trend_p)[2])),
  `Price p`    = sprintf("%.3f", c(p_att_val$p.value,
                                   p_h1$p.value, p_h2$p.value,
                                   sp["year","Pr(>|t|)"],
                                   summary(trend_p)$coefficients[2,4])),
  check.names = FALSE, stringsAsFactors = FALSE
)

make_table(t7,
           "Figure 13. Robustness: comparison of results across outcome variables",
           "Volume: log winsorized monthly volume. Price: log monthly avg. price. All with firm + month FE.")

# ============================================================================
# Robustness — Parallel trends and placebo
# ============================================================================
t8a <- data.frame(
  Test = c("Parallel trends (2017 coef., ref. = 2018)",
           "Placebo (2018 coef., ref. = 2017)"),
  Coef.     = c(sprintf("%.4f", pt$estimate), sprintf("%.4f", pl$estimate)),
  SE        = c(sprintf("%.4f", pt$std.error), sprintf("%.4f", pl$std.error)),
  `p-value` = c(sprintf("%.3f", pt$p.value), sprintf("%.3f", pl$p.value)),
  Result    = c(ifelse(pt$p.value > 0.10, "Pass", "Fail"),
                ifelse(pl$p.value > 0.10, "Pass", "Fail")),
  check.names = FALSE, stringsAsFactors = FALSE
)

make_table(t8a,
           "Figure 14. Robustness: parallel trends and placebo tests",
           "Parallel trends: 2017 coef. should be ~ 0. Placebo: false treatment in 2018 should show no effect.")

# ============================================================================
# Robustness — Sector x year FE
# ============================================================================
t8b <- sfe %>%
  filter(year >= 2017) %>%
  transmute(
    Year      = as.character(year),
    Coef.     = sprintf("%.4f", estimate),
    SE        = sprintf("%.4f", std.error),
    `p-value` = ifelse(is.na(p.value), "ref.", sprintf("%.3f", p.value)),
    `% Chg.`  = sprintf("%.1f", (exp(estimate)-1)*100)
  )

make_table(t8b,
           "Figure 15. Robustness: sector x year fixed effects specification",
           "Firm + sector x year FE. Clustered SE. Reference year: 2018.")

# ============================================================================
# Robustness — Price event-specific DiD
# ============================================================================
t9 <- ev_p %>%
  transmute(
    `#`       = as.character(order),
    Event     = label,
    Coef.     = sprintf("%.4f", did),
    SE        = sprintf("%.4f", se),
    `p-value` = sprintf("%.3f", p),
    Sig.      = sig,
    `% Chg.`  = sprintf("%.1f", (exp(did)-1)*100)
  )

t9_trend <- data.frame(
  `#`       = "",
  Event     = "Trend across events",
  Coef.     = sprintf("%.4f", coef(trend_p)[2]),
  SE        = sprintf("%.4f", summary(trend_p)$coefficients[2,2]),
  `p-value` = sprintf("%.3f", summary(trend_p)$coefficients[2,4]),
  Sig.      = "",
  `% Chg.`  = "—",
  check.names = FALSE, stringsAsFactors = FALSE
)

t9 <- rbind(t9, t9_trend)

make_table(t9,
           "Figure 16. Robustness: event-specific DiD on stock price",
           "6-month window. Firm + month FE. Clustered SE. * p<0.10, ** p<0.05, *** p<0.01.")


# ============================================================================
# H3-Trend test summary
# ============================================================================
t6_h3 <- data.frame(
  Test = c("Full-period trend (2019–2025)",
           "Sign test (7/7 negative years)"),
  Slope     = c(sprintf("%.4f", coef(lm_full)["year"]), "—"),
  SE        = c(sprintf("%.4f", summary(lm_full)$coefficients["year", "Std. Error"]), "—"),
  `p-value` = c(sprintf("%.3f", summary(lm_full)$coefficients["year", "Pr(>|t|)"]), "0.008"),
  check.names = FALSE, stringsAsFactors = FALSE
)

make_table(t6_h3,
           "Figure 10. Hypothesis 3: trend test for cumulative policy impact",
           "WLS trend of year-by-year coefficients on year. Sign test: P(7 negative | H0) = 0.5^7 = 0.008.")


# ============================================================================
# ECONOMETRIC TABLES
# ============================================================================

# ============================================================================
# figure 17. Pooled DiD Estimates (Main Results)
# ============================================================================

# Sector × Year FE pooled on monthly data
m_att_sfe <- feols(log_vol ~ i(treated, post, ref = 0) |
                     company + interaction(sector, month_fct),
                   data = df_vol, cluster = ~company)
att_sfe <- tidy(m_att_sfe) %>% filter(grepl("treated::1", term))

t4 <- data.frame(
  ` ` = c("Treated × Post", "", "",
          "Treated × Post × loc_China", "", "",
          "Treated × Post × is_financial", "", "",
          "", "Firm FE", "Month FE", "Sector × Month FE",
          "Observations", "Treated firms", "Control firms"),
  `(1) Baseline` = c(
    sprintf("%.4f", att$estimate),
    sprintf("(%.4f)", att$std.error),
    sprintf("[p=%.3f]", att$p.value),
    "", "", "", "", "", "",
    "", "Yes", "Yes", "No",
    as.character(nrow(df_vol)), "46", "74"),
  `(2) H1 Location` = c(
    sprintf("%.4f", att$estimate),
    sprintf("(%.4f)", att$std.error),
    sprintf("[p=%.3f]", att$p.value),
    sprintf("%.4f**", h1_pool$estimate),
    sprintf("(%.4f)", h1_pool$std.error),
    sprintf("[p=%.3f]", h1_pool$p.value),
    "", "", "",
    "", "Yes", "Yes", "No",
    as.character(nrow(df_vol)), "46", "74"),
  `(3) H2 Financial` = c(
    sprintf("%.4f", att$estimate),
    sprintf("(%.4f)", att$std.error),
    sprintf("[p=%.3f]", att$p.value),
    "", "", "",
    sprintf("%.4f", h2_pool$estimate),
    sprintf("(%.4f)", h2_pool$std.error),
    sprintf("[p=%.3f]", h2_pool$p.value),
    "", "Yes", "Yes", "No",
    as.character(nrow(df_vol)), "46", "74"),
  `(4) Sector×Year FE` = c(
    sprintf("%.4f", att_sfe$estimate),
    sprintf("(%.4f)", att_sfe$std.error),
    sprintf("[p=%.3f]", att_sfe$p.value),
    "", "", "", "", "", "",
    "", "Yes", "No", "Yes",
    as.character(nrow(df_vol)), "46", "74"),
  check.names = FALSE, stringsAsFactors = FALSE
)

make_table(t4,
           "Figure 17. Difference-in-Differences Estimates: Pooled Treatment Effects",
           "Dep. var.: log winsorized monthly trading volume. SE clustered at firm level in parentheses. ** p<0.05, * p<0.1.")

# ============================================================================
# Figure 18. Year-by-Year Treatment Effects
# ============================================================================

t5_base <- get_effects(m_event) %>% filter(year >= 2017)
t5_sfe_d <- get_effects(m_sfe) %>% filter(year >= 2017)

# Build formatted table
t5_rows <- lapply(2017:2025, function(y) {
  b <- t5_base %>% filter(year == y)
  s <- t5_sfe_d %>% filter(year == y)
  h <- h1_coefs %>% filter(year == y)
  
  b_sig <- ifelse(y == 2018, "",
                  ifelse(is.na(b$p.value), "",
                         case_when(b$p.value < 0.01 ~ "***",
                                   b$p.value < 0.05 ~ "**",
                                   b$p.value < 0.10 ~ "*", TRUE ~ "")))
  s_sig <- ifelse(y == 2018, "",
                  ifelse(is.na(s$p.value), "",
                         case_when(s$p.value < 0.01 ~ "***",
                                   s$p.value < 0.05 ~ "**",
                                   s$p.value < 0.10 ~ "*", TRUE ~ "")))
  
  data.frame(
    Year = as.character(y),
    `(1) Coef.` = ifelse(y == 2018, "ref.", sprintf("%.4f%s", b$estimate, b_sig)),
    `(1) SE` = ifelse(y == 2018, "", sprintf("(%.4f)", b$std.error)),
    `(2) Coef.` = ifelse(y == 2018, "ref.", sprintf("%.4f%s", s$estimate, s_sig)),
    `(2) SE` = ifelse(y == 2018, "", sprintf("(%.4f)", s$std.error)),
    `(3) H1 Int.` = ifelse(nrow(h) == 0, "",
                           sprintf("%.4f%s", h$estimate, h$sig)),
    `(3) SE` = ifelse(nrow(h) == 0, "",
                      sprintf("(%.4f)", h$std.error)),
    check.names = FALSE, stringsAsFactors = FALSE
  )
})

t5_body <- do.call(rbind, t5_rows)

# Add summary rows
t5_summary <- data.frame(
  Year = c("", "Firm FE", "Year FE", "Sector×Year FE",
           "Observations", "Firms", "",
           "H3 trend slope",
           "Sign test (7/7 neg.)"),
  `(1) Coef.` = c("", "Yes", "Yes", "No",
                  as.character(nrow(df_yr)), "120", "",
                  sprintf("%.4f (p=%.3f)", coef(lm_full)["year"],
                          summary(lm_full)$coefficients["year","Pr(>|t|)"]),
                  "p = 0.008"),
  `(1) SE` = rep("", 9),
  `(2) Coef.` = c("", "Yes", "No", "Yes",
                  as.character(nrow(df_yr)), "120",
                  "", "", ""),
  `(2) SE` = rep("", 9),
  `(3) H1 Int.` = c("", "Yes", "Yes", "No",
                    as.character(nrow(df_yr)), "120",
                    "", "", ""),
  `(3) SE` = rep("", 9),
  check.names = FALSE, stringsAsFactors = FALSE
)

t5_final <- rbind(t5_body, t5_summary)
t5_final[t5_final == ""] <- " "

make_table(t5_final,
           "Figure 18. Year-by-Year Treatment Effects on Log Annual Trading Volume",
           "(1) Firm+year FE. (2) Firm+sector×year FE. (3) Triple interaction: year×treated×loc_China. Ref: 2018. *** p<0.01, ** p<0.05, * p<0.1.")

# ============================================================================
# Figure 19. Event-Specific DiD Estimates (H4) — Volume and Price
# ============================================================================

t6_data <- ev_results %>%
  left_join(
    ev_p %>% select(order, did_p = did, se_p = se, p_p = p, sig_p = sig),
    by = "order"
  ) %>%
  transmute(
    `#` = as.character(order),
    Event = label,
    `Vol. Coef.` = sprintf("%.4f%s", did, sig),
    `Vol. SE` = sprintf("(%.4f)", se),
    `Vol. p` = sprintf("%.3f", p),
    `Price Coef.` = ifelse(is.na(did_p), "", sprintf("%.4f%s", did_p, sig_p)),
    `Price SE` = ifelse(is.na(se_p), "", sprintf("(%.4f)", se_p)),
    `Price p` = ifelse(is.na(p_p), "", sprintf("%.3f", p_p))
  )

# Add trend rows
t6_trend <- data.frame(
  `#` = "",
  Event = "Weighted trend (delta)",
  `Vol. Coef.` = sprintf("%.4f", coef(trend_ev)[2]),
  `Vol. SE` = sprintf("(%.4f)", summary(trend_ev)$coefficients[2,2]),
  `Vol. p` = sprintf("%.3f", summary(trend_ev)$coefficients[2,4]),
  `Price Coef.` = sprintf("%.4f", coef(trend_p)[2]),
  `Price SE` = sprintf("(%.4f)", summary(trend_p)$coefficients[2,2]),
  `Price p` = sprintf("%.3f", summary(trend_p)$coefficients[2,4]),
  check.names = FALSE, stringsAsFactors = FALSE
)

t6_final <- rbind(t6_data, t6_trend)

make_table(t6_final,
           "Figure 19. Event-Specific Difference-in-Differences Estimates",
           "±6 month window. Firm+month FE. Clustered SE. WLS trend: coefficients regressed on event order. *** p<0.01, ** p<0.05, * p<0.1.")

# ============================================================================
# Figure 20. Robustness Summary — Volume vs Price
# ============================================================================

t7 <- data.frame(
  Hypothesis = c("Overall ATT",
                 "H1: loc_China",
                 "H2: is_financial",
                 "H3: Year trend",
                 "H4: Event trend"),
  `Vol. Coef.` = sprintf("%+.4f", c(att$estimate, h1_pool$estimate,
                                    h2_pool$estimate,
                                    coef(lm_full)["year"],
                                    coef(trend_ev)[2])),
  `Vol. SE` = sprintf("(%.4f)", c(att$std.error, h1_pool$std.error,
                                  h2_pool$std.error,
                                  summary(lm_full)$coefficients["year","Std. Error"],
                                  summary(trend_ev)$coefficients[2,2])),
  `Vol. p` = sprintf("%.3f", c(att$p.value, h1_pool$p.value,
                               h2_pool$p.value,
                               summary(lm_full)$coefficients["year","Pr(>|t|)"],
                               summary(trend_ev)$coefficients[2,4])),
  `Price Coef.` = sprintf("%+.4f", c(p_att$estimate, p_h1$estimate,
                                     p_h2$estimate,
                                     sp["year","Estimate"],
                                     coef(trend_p)[2])),
  `Price SE` = sprintf("(%.4f)", c(p_att$std.error, p_h1$std.error,
                                   p_h2$std.error,
                                   sp["year","Std. Error"],
                                   summary(trend_p)$coefficients[2,2])),
  `Price p` = sprintf("%.3f", c(p_att$p.value, p_h1$p.value,
                                p_h2$p.value,
                                sp["year","Pr(>|t|)"],
                                summary(trend_p)$coefficients[2,4])),
  Result = c("Not sig.",
             "** (vol.), * (price)",
             "Not sig.",
             "Not sig.",
             "Not sig."),
  check.names = FALSE, stringsAsFactors = FALSE
)

make_table(t7,
           "Figure 20. Robustness: Comparison of Results Across Outcome Variables",
           "Vol.=log winsorized monthly volume. Price=log monthly avg. price. Firm+month FE. Clustered SE. *** p<0.01, ** p<0.05, * p<0.1.")


