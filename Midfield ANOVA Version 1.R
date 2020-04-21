packages <- c("tidyverse", "hasseDiagram","knitr",
              "kableExtra", "car", "parameters",
              "DescTools", "ggplot2", "lattice",
              "psych", "lme4", "car", "emmeans", "viridis")
lapply(packages, library, character.only = TRUE)
source("https://raw.github.com/neilhatfield/STAT461/master/ANOVATools.R")

# Data Exploration

ggplot2::ggplot(data = midfieldLong,
                                mapping = aes(x = prop_fem)) +
  ggplot2::geom_histogram(col = "white", fill = "#001E70",
                          closed="left",
                          binwidth = 0.05) +
  ggplot2::theme_bw() +
  xlab("Proportion of Women Graduating in STEM") +
  ylab("Count")


# Data required log-transformation

midfieldLong$transformed <- log(midfieldLong$prop_fem)

# Histogram of log-transformed data

ggplot2::ggplot(data = midfieldLong,
                mapping = aes(x = transformed)) +
  ggplot2::geom_histogram(col = "white", fill = "#001E70",
                          closed="left",
                          binwidth = 0.1) +
  ggplot2::theme_bw() +
  xlab("Proportion of Women Graduating in STEM") +
  ylab("Count")

# One-Way (Repeated Measures) Within-Subjects ANOVA Model
options("contrasts" = c("contr.sum","contr.poly"))
midfieldM1 <- lme4::lmer(transformed ~ term_degree + (1| institution), data = midfieldLong)
midfieldModel <- aov(transformed ~ term_degree + Error(institution/term_degree), data = midfieldLong)


# Check Assumptions

### Normality of residuals
a <- car::qqPlot(
  x = residuals(midfieldM1),
  distribution = "norm",
  envelope = 0.9,
  xlab = "Normal Quantiles",
  ylab = "Log-Transformed Proportion of Woman Graduating (STEM)",
  pch = 19
)
## Very slights deviances on normality (1 value at top and 1 value at bottom).
## Test is robust enough to handle slight violations.


### Normality of random effects
b <- car::qqPlot(
  x = lme4::ranef(midfieldM1)$institution[, "(Intercept)"],
  distribution = "norm",
  envelope = 0.9,
  xlab = "Normal Quantiles",
  ylab = "Log-Transformed Proportion of Woman Graduating (STEM)",
  pch = 19
)
## No apparent issues with normality.
## Appears to be normally distributed.


## Homoscedasticity on Residuals
plot(midfieldModel, which = 1, pch = 19, xlab = "Fitted Values", ylab = "Residuals")
## No apparent fanning effect.
## Hard to interpret. Does not look heteroscedastic.

## Interaction
ggplot2::ggplot(data = midfieldLong,
                mapping = aes(x = term_degree,
                              y = transformed,
                              color = institution,
                              group = institution)) +
  ggplot2::geom_point(size=2) +
  ggplot2::geom_line(size=1) +
  ggplot2::theme_bw() +
  xlab("Term Year") +
  ylab("Log-Transformed Proportion of Woman Graduating (STEM)") +
  labs(color = "Institution")
## Very messy, seems to be apparent interactions within the model.

## ANOVA Table Results
options(knitr.kable.NA= "")
knitr::kable(
  parameters::model_parameters(
    midfieldModel),
  digits = 3,
  col.names = c("Term Group", "Source", "SS", "df", "MS", "F", "p-value"),
  caption = "ANOVA Table Within-Subjects: Proportion of Woman Graduating (STEM)"
) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "condensed"),
    font_size = 12, latex_options = "HOLD_position")

## Effect Sizes and Post Hoc Pending