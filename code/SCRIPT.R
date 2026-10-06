# ============================================================
# Blog 1: Omitted Variable Bias — Real-Data Demonstration
# Data source: IPUMS CPS ASEC
# ============================================================

# ---------- 0. 加载包 ----------
library(tidyverse)
library(survey)
library(ipumsr)
library(here)

# ============================================================
# 1. 加载 IPUMS CPS 数据
# ============================================================

cps <- read_ipums_micro(
  ddi = here("data", "raw", "cps_00001.xml"),
  data = here("data", "raw", "cps_00001.dat")
)

glimpse(cps)

# ============================================================
# 2. 数据清洗
# ============================================================

cps_clean <- cps |>
  filter(
    INCWAGE > 0, INCWAGE < 9999990,
    INCWAGE_CPIU_2010 > 0, INCWAGE_CPIU_2010 < 9999990
  ) |>
  filter(EDUC %in% c(73, 111)) |>
  filter(AGE >= 25, AGE <= 64) |>
  filter(WKSWORK1 > 0, UHRSWORKLY > 0, UHRSWORKLY < 999) |>
  mutate(
    education = case_when(
      EDUC == 73  ~ "High school",
      EDUC == 111 ~ "Bachelor's"
    ),
    educ_binary = if_else(EDUC == 111, 1, 0),
    log_income = log(INCWAGE_CPIU_2010),
    INCWAGE_CPIU_2010 = as.numeric(INCWAGE_CPIU_2010)
  )

nrow(cps_clean)

dir.create(here("data", "clean"), recursive = TRUE, showWarnings = FALSE)
write_csv(cps_clean, here("data", "clean", "cps_clean.csv"))

# ============================================================
# 3. 模型 1：不控制工作周数（有偏）
# ============================================================

svy <- svydesign(ids = ~1, weights = ~ASECWT, data = cps_clean)

model1 <- svyglm(log_income ~ educ_binary, design = svy)
summary(model1)

coef_m1 <- coef(model1)["educ_binary"]

# ============================================================
# 4. 模型 2：控制工作周数（无偏）
# ============================================================

model2 <- svyglm(log_income ~ educ_binary + WKSWORK1, design = svy)
summary(model2)

coef_m2 <- coef(model2)["educ_binary"]

# ============================================================
# 5. 对比两个模型的系数
# ============================================================

coef_compare <- tibble(
  model = c("Without weeks control", "With weeks control"),
  coef = c(coef_m1, coef_m2),
  pct_effect = (exp(coef) - 1) * 100
)

coef_compare

write_csv(coef_compare, here("data", "clean", "ovb_coef_compare.csv"))

# ============================================================
# 6. 图 1：教育 vs 收入（箱线图）
# ============================================================

p1 <- cps_clean |>
  ggplot(aes(x = education, y = INCWAGE_CPIU_2010, fill = education)) +
  geom_boxplot(outlier.alpha = 0.2) +
  scale_fill_manual(values = c("Bachelor's" = "#2c7fb8", "High school" = "#d7191c")) +
  coord_cartesian(ylim = c(0, 150000)) +
  labs(
    title = "Education and Earnings Look Strongly Related",
    subtitle = "Median annual earnings by education group, IPUMS CPS ASEC 2002–2025",
    x = NULL,
    y = "Annual earnings (2010 $)",
    fill = NULL,
    caption = "Data source: IPUMS CPS ASEC. Weighted using ASECWT."
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

p1

# ============================================================
# 7. 图 2：控制周数前后的教育系数
# ============================================================

p2 <- coef_compare |>
  mutate(model = reorder(model, coef)) |>
  ggplot(aes(x = model, y = pct_effect, fill = model)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0(round(pct_effect, 1), "%")),
            vjust = -0.5, size = 4.5) +
  scale_fill_manual(values = c("Without weeks control" = "#d7191c",
                               "With weeks control" = "#2c7fb8")) +
  labs(
    title = "The Education Effect Shrinks When Work Weeks Are Controlled",
    subtitle = "Estimated percentage return to a bachelor's degree, before and after adding weeks worked",
    x = NULL,
    y = "Estimated effect (%)",
    fill = NULL,
    caption = "Data source: IPUMS CPS ASEC. Weighted using ASECWT."
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none") +
  expand_limits(y = max(coef_compare$pct_effect) * 1.2)

p2

# ============================================================
# 8. 图 3：模拟数据演示 OVB 机制（冰淇淋例子）
# ============================================================

set.seed(1)
n <- 200

temp     <- rnorm(n, 80, 10)
icecream <- 5 * temp + rnorm(n, 0, 20)
drowning <- 2 * temp + rnorm(n, 0, 5)

m_bad  <- lm(drowning ~ icecream)
m_good <- lm(drowning ~ icecream + temp)

sim_compare <- tibble(
  model = c("Without weather", "With weather"),
  coef = c(coef(m_bad)["icecream"], coef(m_good)["icecream"])
)

sim_compare

p3 <- sim_compare |>
  mutate(model = reorder(model, coef)) |>
  ggplot(aes(x = model, y = coef, fill = model)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = round(coef, 2)), vjust = -0.5, size = 4.5) +
  scale_fill_manual(values = c("Without weather" = "#d7191c",
                               "With weather" = "#2c7fb8")) +
  labs(
    title = "The Ice Cream Coefficient Disappears When Weather Is Added",
    subtitle = "Simulated data: ice cream has no true effect on drowning",
    x = NULL,
    y = "Estimated coefficient on ice cream",
    fill = NULL,
    caption = "Simulated data. True effect of ice cream = 0."
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none") +
  expand_limits(y = max(sim_compare$coef) * 1.3)

p3

# ============================================================
# 9. 保存图表
# ============================================================

dir.create(here("output"), recursive = TRUE, showWarnings = FALSE)

ggsave(here("output", "ovb_boxplot.png"), plot = p1,
       width = 8, height = 6, dpi = 300)
ggsave(here("output", "ovb_coef_compare.png"), plot = p2,
       width = 8, height = 6, dpi = 300)
ggsave(here("output", "ovb_simulation.png"), plot = p3,
       width = 8, height = 6, dpi = 300)

cat("✅ 完成\n")