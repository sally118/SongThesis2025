# install.packages("ggplot2")
# install.packages("tidyverse")
# install.packages("readxl")
# install.packages("dplyr")
# install.packages("drc")


library(readxl)
library(tidyr)
library(dplyr)
library(drc)
library(ggplot2)

# List all sheet names in Excel file
excel_sheets("/Users/sallysong/Downloads/LDOPA.xlsx")

# Sample measurements
samples <- read_excel("/Users/sallysong/Downloads/LDOPA.xlsx", sheet = "Samples")

print(samples)

HEPES <- data.frame(
  Expected = c(samples$`L-DOPA (µM)`),
  Absorbance = c(samples$HEPES_Rep1, samples$HEPES_Rep2)
)

Water <- data.frame(
  Expected = c(samples$`L-DOPA (µM)`),
  Absorbance = c(samples$Water_Rep1, samples$Water_Rep2)
)

FM <- data.frame(
  Expected = c(samples$`L-DOPA (µM)`),
  Absorbance = c(samples$FM_Rep1, samples$FM_Rep2)
)

# Standard measurements
std_data <- read_excel("/Users/sallysong/Downloads/LDOPA.xlsx", sheet = "Standards")

std <- data.frame(
  STD = c(std_data$`L-DOPA (µM)`),
  Absorbance = c(std_data$`Absorbance (475 nm)`)
)

# Create and fit the model
model <- drc::drm(Absorbance ~ STD, fct = drc::LL.5(), data = std)
std$Predicted <- rev(backfit(model)[,"Estimate"])
std <- rename(std, c("Expected" = "STD"))
std$datasource = "Standards"

# a <- ED(object = model, std$Absorbance, type = "absolute", display = FALSE, multcomp = FALSE)

# Create initial plot of model
plot(model)

## Function ED with this combination of arguments is the "inverse prediction" of the model!

# HEPES
HEPES$Predicted <- ED(object = model, HEPES$Absorbance, type = "absolute", display = FALSE, multcomp = FALSE)[, 1]
HEPES$datasource = "HEPES"

plot_df_HEPES = rbind(std, HEPES)

plot_df_HEPES$error <- plot_df_HEPES$Predicted - plot_df_HEPES$Expected
plot_df_HEPES$abs_error <- abs(plot_df_HEPES$error)
plot_df_HEPES$MAPE <- plot_df_HEPES$abs_error / plot_df_HEPES$Expected
plot_df_HEPES = subset(plot_df_HEPES, MAPE != Inf)

plot_df_HEPES$datasource <- factor(plot_df_HEPES$datasource)

# Fit regression models for Predicted ~ Expected
lm_std <- lm(Predicted ~ Expected, data = std)
lm_HEPES <- lm(Predicted ~ Expected, data = HEPES)

R2_STD <- summary(lm_std)$r.squared
R2_HEPES <- summary(lm_HEPES)$r.squared

ggplot(plot_df_HEPES, aes(x = Expected, y = Predicted, color = datasource)) +
  geom_point(size = 2) + geom_abline(slope = 1, intercept = 0) +
  annotate("text", x = 250, y = max(HEPES$Predicted) * 0.9, # Add R²
           label = paste0("R² (STD) = ", round(R2_STD, 3), 
                          "\nR² (HEPES) = ", round(R2_HEPES, 3)
                          ), 
           color = "black", size = 3, hjust = 0) + 
  ylab("Predicted L-DOPA Conc (µM)") +
  xlab("Expected L-DOPA Conc (µM)") +
  labs(color=NULL) +
  theme_classic() +
  scale_color_manual(values = c("HEPES" = "forestgreen", "Standards" = "goldenrod1"), 
                     limits = c("Standards", "HEPES"))



# FM
FM$Predicted <- ED(object = model, FM$Absorbance, type = "absolute", display = FALSE, multcomp = FALSE)[, 1]
FM$datasource = "FM"

plot_df_FM = rbind(std, FM)

plot_df_FM$error <- plot_df_FM$Predicted - plot_df_FM$Expected
plot_df_FM$abs_error <- abs(plot_df_FM$error)
plot_df_FM$MAPE <- plot_df_FM$abs_error / plot_df_FM$Expected
plot_df_FM = subset(plot_df_FM, MAPE != Inf)

plot_df_FM$datasource <- factor(plot_df_FM$datasource)

# Fit regression models for Predicted ~ Expected
lm_FM <- lm(Predicted ~ Expected, data = FM)

R2_FM <- summary(lm_FM)$r.squared

ggplot(plot_df_FM, aes(x = Expected, y = Predicted, color = datasource)) +
  geom_point(size = 2) + geom_abline(slope = 1, intercept = 0) +
  annotate("text", x = 250, y = 4300,
           label = paste0("R² (STD) = ", round(R2_STD, 3), 
                          "\nR² (FM) = ", round(R2_FM, 3)
           ), 
           color = "black", size = 3, hjust = 0) + 
  ylab("Predicted L-DOPA Conc (µM)") +
  xlab("Expected L-DOPA Conc (µM)") +
  labs(color=NULL) +
  theme_classic() +
  scale_color_manual(values = c("FM" = "brown1", "Standards" = "goldenrod1"),
                     limits = c("Standards", "FM"))



# Water
Water$Predicted <- ED(object = model, Water$Absorbance, type = "absolute", display = FALSE, multcomp = FALSE)[, 1]
Water$datasource = "Water"

plot_df_Water = rbind(std, Water)

plot_df_Water$error <- plot_df_Water$Predicted - plot_df_Water$Expected
plot_df_Water$abs_error <- abs(plot_df_Water$error)
plot_df_Water$MAPE <- plot_df_Water$abs_error / plot_df_Water$Expected
plot_df_Water = subset(plot_df_Water, MAPE != Inf)

plot_df_Water$datasource <- factor(plot_df_Water$datasource)

# Fit regression models for Predicted ~ Expected
lm_Water <- lm(Predicted ~ Expected, data = Water)

R2_Water <- summary(lm_Water)$r.squared

ggplot(plot_df_Water, aes(x = Expected, y = Predicted, color = datasource)) +
  geom_point(size = 2) + geom_abline(slope = 1, intercept = 0) +
  annotate("text", x = 250, y = 4300,
           label = paste0("R² (STD) = ", round(R2_STD, 3), 
                          "\nR² (Water) = ", round(R2_Water, 3)
           ), 
           color = "black", size = 3, hjust = 0) + 
  ylab("Predicted L-DOPA Conc (µM)") +
  xlab("Expected L-DOPA Conc (µM)") +
  labs(color=NULL) +
  theme_classic() +
  scale_color_manual(values = c("Water" = "dodgerblue2", "Standards" = "goldenrod1"),
                     limits = c("Standards", "Water"))




