library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(drc)
library(stringr)

# Supernatant sample measurements
super <- read_excel("/Users/sallysong/Downloads/LDOPA.xlsx", sheet = "0319_Supernatants")

## Rows 1,3 are replicates of each other.
## Rows 2,4 are replicates of each other (0.5 dilution).

super <- super[-c(2, 4), ] # Remove 0.5 dilution values, messes with LDOPA conc later

## Subtract the background (FM) mean abs from the samples

super$Non <- super$Non - mean(super$FM)
super$`Non (IPTG)` <- super$`Non (IPTG)` - mean(super$FM)
super$`Non (IPTG+tyr)` <- super$`Non (IPTG+tyr)` - mean(super$FM)
super$LD <- super$LD - mean(super$FM)
super$`LD (IPTG)` <- super$`LD (IPTG)` - mean(super$FM)
super$`LD (IPTG+tyr)` <- super$`LD (IPTG+tyr)` - mean(super$FM)

super_long <- super %>%
  pivot_longer(cols = matches("^(Non|LD|FM)"), 
               names_to = "Treatment", 
               values_to = "Absorbance")

super_long <- super_long[super_long$Treatment != "FM", ]

# Standard measurements
std_data <- read_excel("/Users/sallysong/Downloads/LDOPA.xlsx", sheet = "0319_Standards")

std <- data.frame(
  STD = c(std_data$`L-DOPA (µM)`),
  Absorbance = c(std_data$`Absorbance (475 nm)`))

std$Absorbance <- std$Absorbance - std$Absorbance[7] # subtract absorbance of 0

# Create and fit the model
model <- drc::drm(Absorbance ~ STD, fct = drc::LL.5(), data = std)

plot(model)

### Create standard curve plot with ggplot
# Create a data.frame that will be used to draw the standard curve line
line <- data.frame(
  x = seq(min(std$STD * 0.9), max(std$STD) * 1.1, length.out = 100)
)

# Fill it with 'predictions' that will be the Y points along the line
line$y <- stats::predict(model, line)

# Create ggplot of standard curve
ggplot(std, aes(STD, Absorbance)) + 
  # add the points of original data
  geom_point(aes(color = "Standard"), size = 1.5) + 
  # plot the standard curve
  geom_line(data = line, aes(x = x, y = y, color = "5PL"), linewidth = 0.5) + 
  # adjust x axis scale
  scale_x_continuous(breaks = seq(0, 4000, 1000)) + 
  # transform the y axis to log10
  scale_x_log10() + 
  # add a legend
  theme(legend.position = "right") + 
  scale_color_manual(name = NULL, values = c("Standard" = "gray30", "5PL" = "gray30")) + 
  # change the ggplot theme
  theme_classic() + 
  labs(y= "Absorbance (475 nm)", x = "L-DOPA (µM)")


### Predict LDOPA conc from sample supernatant

super_long <- super_long %>%
  #mutate(pred_LDOPA = predict(model, newdata = data.frame(Absorbance)))
  #changing predicting function
  mutate(pred_LDOPA = ED(object = model, super_long$Absorbance, type = "absolute", display = FALSE, multcomp = FALSE)[, 1])

super_long$pred_LDOPA[is.nan(super_long$pred_LDOPA)]<-0

summ_super_long <- super_long %>%
  group_by(Treatment) %>%
  summarize(
    mn = mean(pred_LDOPA),
    std = sd(pred_LDOPA),
    n = n()
  ) %>%
  mutate(conf_interval = 1.96*std/sqrt(n)) %>%
  mutate(ymin = mn-conf_interval) %>%
  mutate(ymax = mn+conf_interval)

summ_super_long <- summ_super_long %>%
  mutate(
    Strain = factor(case_when(
      str_starts(Treatment, "Non") ~ "Non",
      str_starts(Treatment, "LD") ~ "LD",
      TRUE ~ Treatment 
    ), levels = c("Non", "LD"))
  ) %>%
  mutate(
    Treatment = factor(Treatment, 
                       levels = c("Non", "Non (IPTG)", "Non (IPTG+tyr)", 
                                  "LD", "LD (IPTG)", "LD (IPTG+tyr)"))
  ) %>%
  mutate(
    fill_color = case_when(
      Treatment == "Non" ~ "#F8B0B6",
      Treatment == "Non (IPTG)" ~ "hotpink",
      Treatment == "Non (IPTG+tyr)" ~ "hotpink4",
      Treatment == "LD" ~ "palegreen1",
      Treatment == "LD (IPTG)" ~ "seagreen3",
      Treatment == "LD (IPTG+tyr)" ~ "seagreen4"
    )
  )

strain_labels <- c(
  "Non" = "italic('E. coli')~'Non'",
  "LD" = "italic('E. coli')~'LD'"
)

ggplot(summ_super_long, aes(x = Treatment, y = mn, fill=fill_color)) + 
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) + 
  geom_errorbar(aes(ymin = pmax(ymin,0), ymax = pmax(ymax,0)), 
                width = 0.2, position = position_dodge(width = 0.7)) +
  theme_bw() + 
  labs(y= "L-DOPA (µM)", x = "Treatment") +
  scale_fill_identity() +
  facet_wrap(~ Strain, scales = "free_x", labeller = labeller(Strain = strain_labels, .default = label_parsed)) +
  theme(strip.text = element_text(size = 12),
        axis.text.x = element_blank(),
        axis.title.x = element_text(margin = margin(t = 35)))