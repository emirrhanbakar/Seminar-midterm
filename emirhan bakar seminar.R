
if (!require(readxl)) install.packages("readxl")
if (!require(ggplot2)) install.packages("ggplot2")
library(readxl)
library(ggplot2)

raw_df <- read_excel(file.choose())

country_name <- as.character(raw_df$`Country Name`[1])
gdp_col <- grep("GDP", names(raw_df), value = TRUE)[1]
co2_col <- grep("CO2", names(raw_df), value = TRUE)[1]

df <- raw_df[!is.na(raw_df$Time), ]
df$gdp <- suppressWarnings(as.numeric(df[[gdp_col]]))
df$co2 <- suppressWarnings(as.numeric(df[[co2_col]]))
df <- na.omit(df[, c("Time", "gdp", "co2")])

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
df$ln_gdp <- log(df$gdp)
df$ln_co2 <- log(df$co2)

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
ekc_scatter <- ggplot(df, aes(x = ln_gdp, y = ln_co2)) +
  geom_point(color = "steelblue4", size = 2.5) +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), color = "firebrick", se = FALSE, linewidth = 1) +
  labs(
    title = paste("Environmental Kuznets Curve (EKC) -", country_name),
    subtitle = "Scatter Plot with Regression Line (1970-2024)",
    x = "Log GDP per capita",
    y = "Log CO2 emissions per capita"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray30")
  )

print(ekc_scatter)

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
df$ln_gdp_sq <- (df$ln_gdp)^2

ols_model <- lm(ln_co2 ~ ln_gdp + ln_gdp_sq, data = df)
summary(ols_model)