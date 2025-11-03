library(ggplot2)
library(dplyr)
library(readr)

getwd()
# setwd() # if needed

foreign <- read_csv("foreign_threat.csv")
domestic <- read_csv("domestic_threat.csv")
promotion <- read_csv("promotion.csv")

## find year of promotion for each officer

year_candidates <- c("shangjiang_year", "zhongjiang_year", "cmc_first")
year_candidates <- year_candidates[year_candidates %in% names(promotion)]

if (length(year_candidates) == 0) {
  stop("No promotion year columns found (e.g. shangjiang_year, zhongjiang_year, cmc_first).")
}

promotion <- promotion %>%
  mutate(prom_year = pmin(shangjiang_year, zhongjiang_year, cmc_first, na.rm = TRUE)) %>%
  filter(!is.infinite(prom_year)) %>%
  mutate(prom_year = as.integer(prom_year)) %>%
  filter(prom_year >= 1949)   #only after 1949

## aggregate number of experienced promotions each year
combat_year <- promotion %>%
  group_by(prom_year) %>%
  summarise(num_combat = sum(combat_post_1949 == 1, na.rm = TRUE)) %>%
  left_join(foreign %>% select(year, fti), by = c("prom_year" = "year"))

## find officers with ties to leaders
net_cols <- c("deng.network", "jiang.network", "hu.network", "xi.network",
              "jiang.network.alt", "hu.network.alt", "xi.network.alt")
net_cols <- net_cols[net_cols %in% names(promotion)]
if (length(net_cols) == 0)
  stop("No *.network columns found. Add your leader network columns to 'net_cols'.")

promotion$loyal <- as.integer(
  rowSums(sapply(net_cols, function(c) as.integer(promotion[[c]] == 1)), na.rm = TRUE) > 0
)

## aggregate number of loyalty promotions each year
loyal_year <- promotion %>%
  group_by(prom_year) %>%
  summarise(num_loyal = sum(loyal == 1, na.rm = TRUE)) %>%
  left_join(domestic %>% select(year, dti), by = c("prom_year" = "year"))

## make my foreign threat vs. experienced promotion plot

scale_factor <- max(combat_year$num_combat, na.rm = TRUE) / max(combat_year$fti, na.rm = TRUE) # scale so lines align visually

png("foriegn.png", width = 1200, height = 800, res = 150)

ggplot(combat_year, aes(x = prom_year)) +
  geom_line(aes(y = num_combat, color = "Number of Combat Promotions"), linewidth = 1) +
  geom_line(aes(y = fti * scale_factor, color = "Foreign Threat Index"), linewidth = 1) +  # ✅ no dashed line
  scale_y_continuous(
    name = "Number of Combat Promotions",
    sec.axis = sec_axis(~ ./scale_factor, name = "Foreign Threat Index")
  ) +
  labs(
    title = "Foreign Threat Index vs. Combat-Experienced Promotions",
    x = "Year"
  ) +
  scale_color_manual(values = c("Number of Combat Promotions" = "steelblue",
                                "Foreign Threat Index" = "firebrick")) +
  theme_minimal(base_size = 13) +
  theme(
    legend.title = element_blank(),
    legend.position = "top"
  )

dev.off()

## make my domestic threat vs. loyalty promotion plot

scale_factor <- max(loyal_year$num_loyal, na.rm = TRUE) / max(loyal_year$dti, na.rm = TRUE) # scale

png("domestic.png", width = 1200, height = 800, res = 150)

ggplot(loyal_year, aes(x = prom_year)) +
  geom_line(aes(y = num_loyal, color = "Number of Loyal Promotions"), linewidth = 1) +
  geom_line(aes(y = dti * scale_factor, color = "Domestic Threat Index"), linewidth = 1) +
  scale_y_continuous(
    name = "Number of Loyal Promotions",
    sec.axis = sec_axis(~ ./scale_factor, name = "Domestic Threat Index")
  ) +
  labs(
    title = "Domestic Threat Index vs. Loyal Promotions",
    x = "Year"
  ) +
  scale_color_manual(values = c("Number of Loyal Promotions" = "darkgreen",
                                "Domestic Threat Index" = "firebrick")) +
  theme_minimal(base_size = 13) +
  theme(
    legend.title = element_blank(),
    legend.position = "top"
  )

dev.off()

## find periods of high threat with low promotions and compare them to those with high promotions
## for foreign threats

## use quartiles for classification (high or low threat)
q_threat <- quantile(combat_year$fti, probs = c(0.25, 0.75), na.rm = TRUE)
q_promo  <- quantile(combat_year$num_combat, probs = c(0.25, 0.75), na.rm = TRUE)

# classify periods
combat_year <- combat_year %>%
  mutate(
    threat_level = case_when(
      fti >= q_threat[2] ~ "High Threat",
      fti <= q_threat[1] ~ "Low Threat",
      TRUE ~ "Moderate Threat"
    ),
    promo_level = case_when(
      num_combat >= q_promo[2] ~ "High Promotion",
      num_combat <= q_promo[1] ~ "Low Promotion",
      TRUE ~ "Moderate Promotion"
    ),
    category = paste(threat_level, "/", promo_level)
  )

# interested in high threat periods
combat_summary <- combat_year %>%
  filter(threat_level == "High Threat") %>%
  select(prom_year, fti, num_combat, category) %>%
  arrange(prom_year)

print(combat_summary)

## now the same for periods of domestic threat

q_dti  <- quantile(loyal_year$dti, probs = c(0.25, 0.75), na.rm = TRUE)
q_loyal <- quantile(loyal_year$num_loyal, probs = c(0.25, 0.75), na.rm = TRUE)

loyal_year <- loyal_year %>%
  mutate(
    threat_level = case_when(
      dti >= q_dti[2] ~ "High Threat",
      dti <= q_dti[1] ~ "Low Threat",
      TRUE ~ "Moderate Threat"
    ),
    promo_level = case_when(
      num_loyal >= q_loyal[2] ~ "High Promotion",
      num_loyal <= q_loyal[1] ~ "Low Promotion",
      TRUE ~ "Moderate Promotion"
    ),
    category = paste(threat_level, "/", promo_level)
  )

loyal_summary <- loyal_year %>%
  filter(threat_level == "High Threat") %>%
  select(prom_year, dti, num_loyal, category) %>%
  arrange(prom_year)

print(loyal_summary)

write.csv(combat_summary, "Foreign x Promotion.csv", row.names = FALSE)
write.csv(loyal_summary, "Domestic x Promotion.csv", row.names = FALSE)

## now we have an idea of comparison between high threat / low vs. high promotion periods
## this helps us answer the question of how the government perceives threats


## make new plot with both num combat and num loyal promotions against FTI

combined_year <- combat_year %>%
  select(prom_year, num_combat, fti) %>%
  full_join(loyal_year %>% select(prom_year, num_loyal, dti),
            by = "prom_year")

scale_factor <- max(combined_year$num_combat, na.rm = TRUE) / max(combined_year$fti, na.rm = TRUE)

png("foreignExtra.png", width = 1200, height = 800, res = 150)

ggplot(combined_year, aes(x = prom_year)) +
  geom_line(aes(y = num_combat, color = "Combat Promotions"), linewidth = 1) +
  geom_line(aes(y = num_loyal, color = "Loyal Promotions"), linewidth = 1) +
  geom_line(aes(y = fti * scale_factor, color = "Foreign Threat Index"), linewidth = 1) +
  scale_y_continuous(
    name = "Number of Promotions",
    sec.axis = sec_axis(~ ./scale_factor, name = "Foreign Threat Index")
  ) +
  labs(
    title = "Foreign Threat Index vs. Combat and Loyal Promotions",
    x = "Year"
  ) +
  scale_color_manual(values = c("Combat Promotions" = "steelblue",
                                "Loyal Promotions" = "darkgreen",
                                "Foreign Threat Index" = "firebrick")) +
  theme_minimal(base_size = 13) +
  theme(
    legend.title = element_blank(),
    legend.position = "top"
  )

dev.off()

## make new plot with both num combat and num loyal promotions against DTI using the joined table above

scale_factor <- max(combined_year$num_combat, na.rm = TRUE) / max(combined_year$dti, na.rm = TRUE)

png("domesticExtra.png", width = 1200, height = 800, res = 150)

ggplot(combined_year, aes(x = prom_year)) +
  geom_line(aes(y = num_combat, color = "Combat Promotions"), linewidth = 1) +
  geom_line(aes(y = num_loyal, color = "Loyal Promotions"), linewidth = 1) +
  geom_line(aes(y = dti * scale_factor, color = "Domestic Threat Index"), linewidth = 1) +
  scale_y_continuous(
    name = "Number of Promotions",
    sec.axis = sec_axis(~ ./scale_factor, name = "Domestic Threat Index")
  ) +
  labs(
    title = "Domestic Threat Index vs. Combat and Loyal Promotions",
    x = "Year"
  ) +
  scale_color_manual(values = c("Combat Promotions" = "steelblue",
                                "Loyal Promotions" = "darkgreen",
                                "Domestic Threat Index" = "firebrick")) +
  theme_minimal(base_size = 13) +
  theme(
    legend.title = element_blank(),
    legend.position = "top"
  )

dev.off()

# Look at 5-year rolled-up correlations: FTI vs combat promotions

library(dplyr)
library(tidyr)
library(zoo)

combat_year_full <- combat_year %>%
  arrange(prom_year) %>%
  complete(prom_year = seq(min(prom_year), max(prom_year), by = 1))  # adds missing years

png("foreign5YearRolledNew.png", width = 1200, height = 800, res = 150)

combat_year_rolled <- combat_year_full %>%
  mutate(
    roll_corr = rollapply(
      data = cbind(fti, num_combat),
      width = 5, align = "right", by.column = FALSE, fill = NA_real_,
      FUN = function(m) {
        if (sum(complete.cases(m)) < 3) return(NA_real_)
        cor(m[,1], m[,2], use = "complete.obs")
      }
    )
  )

ggplot(combat_year_rolled, aes(x = prom_year, y = roll_corr)) +
  geom_line(color = "purple", linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "5-Year Rolling Correlation between Foreign Threat and Combat Promotions",
    y = "Correlation",
    x = "Year"
  ) +
  theme_minimal()

dev.off()


# 5-year rolling correlation: Domestic Threat (DTI) vs Loyal Promotions

# regularize to one row per calendar year
loyal_year_full <- loyal_year %>%
  arrange(prom_year) %>%
  complete(prom_year = seq(min(prom_year), max(prom_year), by = 1))

# compute rolling (calendar) 5-year correlations
loyal_year_rolled <- loyal_year_full %>%
  mutate(
    roll_corr = rollapply(
      data = cbind(dti, num_loyal),
      width = 5,
      align = "right",
      by.column = FALSE,
      fill = NA_real_,
      FUN = function(m) {
        # require at least 3 non-missing pairs inside the window
        if (sum(complete.cases(m)) < 3) return(NA_real_)
        cor(m[, 1], m[, 2], use = "complete.obs")
      }
    )
  )

png("domestic5YearRolledNew.png", width = 1200, height = 800, res = 150)

ggplot(loyal_year_rolled, aes(x = prom_year, y = roll_corr)) +
  geom_line(color = "darkgreen", linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "5-Year Rolling Correlation between Domestic Threat and Loyal Promotions",
    y = "Correlation",
    x = "Year"
  ) +
  theme_minimal()

dev.off()



