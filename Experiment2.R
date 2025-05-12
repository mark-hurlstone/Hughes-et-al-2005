# Load dependencies
library("tidyverse")  
library("tidyr")      
library("rstatix") 
source("simple.R")

# Import the data
E2Data <- read_csv("Experiment2.csv")

# Convert variables into factors
E2Data$Participant <- factor(E2Data$Participant)
E2Data$State <- factor(E2Data$State, levels = c("SS","CS"))
E2Data$Deviant <- factor(E2Data$Deviant, levels = c("No_Deviant","With_Deviant"))
E2Data$Concurrent_Articulation <- factor(E2Data$Concurrent_Articulation, levels = c("No_Concurrent_Articulation","With_Concurrent_Articulation"))

# Run the ANOVA
aovModel <- anova_test(data = E2Data, dv = pCorrect, wid = Participant, within = c(State,Deviant,Concurrent_Articulation), detailed = TRUE, effect.size = "pes")
aovModel$p <- round(aovModel$p, 3)
(aovModel)

# Break down the deviant x concurrent articulation interaction
#==================================================================

# Get ANOVA table
AnovaTable <- get_anova_table(aovModel)

# Calculate cell totals and counts
cellTotals <- E2Data %>%
  group_by(Deviant,Concurrent_Articulation) %>%
  summarise(sum = sum(pCorrect),n = n())
(cellTotals)

# Create "fixed" and "across" factors
fixed  <- "Deviant"
across <- "Concurrent_Articulation"

# Simple main effects of Deviant at Concurrent Articulation
smeDeviant <- simple(cellTotals,AnovaTable,fixed,across)
(smeDeviant)

# Create "fixed" and "across" factors
fixed  <- "Concurrent_Articulation"
across <- "Deviant"

# Simple main effects of Concurrent Articulation at Deviant 
smeCA <- simple(cellTotals,AnovaTable,fixed,across)
(smeCA)


# Generate a plot of the data
#============================

# Get descriptive statistics
descriptives <- E2Data %>%
  group_by(State,Deviant,Concurrent_Articulation) %>%
  get_summary_stats(pCorrect, show = c("mean","se"))
(descriptives)

# Change labels for concurrent articulation manipulation levels
levels(descriptives$Concurrent_Articulation) <- c("No-Concurrent Articulation","With-Concurrent Articulation")

# Plot the data
ggplot(data = descriptives, mapping = aes(x = State, y = mean, fill = Deviant)) +
  geom_col(width = 0.45, position = position_dodge(0.55)) +
  geom_errorbar(mapping = aes(ymin = mean - se, ymax = mean + se),
                size = .5, width = .2, position = position_dodge(.55)) +
  scale_x_discrete(labels = c(expression(paste("Steady-State")), 
                              expression(paste("Changing-State")))) +
  scale_fill_manual(labels = c("No-Deviant", "With-Deviant"), values = c("#355C7D", "#F67280")) +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  facet_wrap(~Concurrent_Articulation, nrow = 1) +
  labs(x = "State", y = "Proportion Correct", color = "Deviant") +
  theme_bw() +
  theme(strip.text            = element_text(size = 14, face = "bold"),
        panel.grid.major      = element_blank(),
        panel.grid.minor      = element_blank(),
        panel.background      = element_blank(),
        panel.border          = element_rect(colour = "black", fill=NA),
        axis.line             = element_blank(),
        axis.title.x          = element_text(size = 14, face = "bold"),
        axis.text.x           = element_text(size = 12, color = "black"),
        axis.text.y           = element_text(size = 12, color = "black"),
        axis.title.y          = element_text(size = 14, face = "bold"),
        legend.title          = element_text(size = 14, face = "bold"),
        legend.background     = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        legend.text           = element_text(size = 12),
        legend.position       = "bottom",
        legend.direction      = "vertical"
  ) 
ggsave("Experiment2.png", dpi=300, width = 8, height = 6)
