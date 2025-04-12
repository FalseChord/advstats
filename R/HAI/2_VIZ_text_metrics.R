library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)

# Read the metrics data
metrics_df <- read.csv("data/HAI_metrics_text.csv")
diversity_df <- read.csv("data/HAI_metrics_diversity.csv")
readability_df <- read.csv("data/HAI_metrics_readability.csv")

# Create distribution plots for each metric
plot_metric_dist <- function(data, metric_col, title) {
  ggplot(data, aes(x = !!sym(metric_col), fill = Text_Type)) +
    geom_density(alpha = 0.5) +
    labs(title = title,
         x = gsub("_", " ", metric_col),
         y = "Density") +
    theme_minimal() +
    theme(legend.position = "bottom")
}

# Basic text metrics plots
p1 <- plot_metric_dist(metrics_df, "Word_Count", "Distribution of Word Count")
p2 <- plot_metric_dist(metrics_df, "Sentence_Count", "Distribution of Sentence Count")
p3 <- plot_metric_dist(metrics_df, "Avg_Sentence_Length", "Distribution of Average Sentence Length")
p4 <- plot_metric_dist(metrics_df, "Avg_Word_Length", "Distribution of Average Word Length")

# Diversity metrics plots
p5 <- plot_metric_dist(diversity_df, "TTR", "Distribution of Type-Token Ratio")
p6 <- plot_metric_dist(diversity_df, "Yule_K", "Distribution of Yule K")
p7 <- plot_metric_dist(diversity_df, "Simpson_D", "Distribution of Simpson D")
p8 <- plot_metric_dist(diversity_df, "Herdan_C", "Distribution of Herdan C")
p9 <- plot_metric_dist(diversity_df, "Brunet_W", "Distribution of Brunet W")
p10 <- plot_metric_dist(diversity_df, "Honore_R", "Distribution of Honore R")

# Readability metrics plots
p11 <- plot_metric_dist(readability_df, "Flesch_Reading_Ease", "Distribution of Flesch Reading Ease")
p12 <- plot_metric_dist(readability_df, "Flesch_Kincaid_Grade", "Distribution of Flesch-Kincaid Grade Level")
p13 <- plot_metric_dist(readability_df, "Automated_Readability_Index", "Distribution of Automated Readability Index")
p14 <- plot_metric_dist(readability_df, "Coleman_Liau_Index", "Distribution of Coleman-Liau Index")
p15 <- plot_metric_dist(readability_df, "Gunning_Fog_Index", "Distribution of Gunning Fog Index")
p16 <- plot_metric_dist(readability_df, "SMOG_Index", "Distribution of SMOG Index")
p17 <- plot_metric_dist(readability_df, "LIX_Score", "Distribution of LIX Score")
p18 <- plot_metric_dist(readability_df, "RIX_Score", "Distribution of RIX Score")

# Create summary statistics for each dataset separately
text_stats <- metrics_df %>%
  group_by(Text_Type) %>%
  summarise(
    Mean_Word_Count = mean(Word_Count),
    Mean_Sentence_Count = mean(Sentence_Count),
    Mean_Avg_Sentence_Length = mean(Avg_Sentence_Length),
    Mean_Avg_Word_Length = mean(Avg_Word_Length)
  )

diversity_stats <- diversity_df %>%
  group_by(Text_Type) %>%
  summarise(
    Mean_TTR = mean(TTR),
    Mean_Yule_K = mean(Yule_K),
    Mean_Simpson_D = mean(Simpson_D),
    Mean_Herdan_C = mean(Herdan_C),
    Mean_Brunet_W = mean(Brunet_W),
    Mean_Honore_R = mean(Honore_R)
  )

readability_stats <- readability_df %>%
  group_by(Text_Type) %>%
  summarise(
    Mean_Flesch_Reading_Ease = mean(Flesch_Reading_Ease),
    Mean_Flesch_Kincaid_Grade = mean(Flesch_Kincaid_Grade),
    Mean_Automated_Readability_Index = mean(Automated_Readability_Index),
    Mean_Gunning_Fog_Index = mean(Gunning_Fog_Index),
    Mean_SMOG_Index = mean(SMOG_Index),
    Mean_LIX_Score = mean(LIX_Score),
    Mean_RIX_Score = mean(RIX_Score)
  )

# Print summary statistics
print("Basic Text Metrics Summary:")
print(text_stats)
print("\nDiversity Metrics Summary:")
print(diversity_stats)
print("\nReadability Metrics Summary:")
print(readability_stats)

# Save basic metrics plot
ggsave("output/plots/basic_metrics_distribution.png", 
       arrangeGrob(p1, p2, p3, p4, ncol = 2),
       width = 12, height = 8)

# Save diversity metrics plot
ggsave("output/plots/diversity_metrics_distribution.png", 
       arrangeGrob(p5, p6, p7, p8, p9, p10, ncol = 2),
       width = 12, height = 8)

# Save readability metrics plot
ggsave("output/plots/readability_metrics_distribution.png", 
       arrangeGrob(p11, p12, p13, p14, p15, p16, p17, p18, ncol = 2),
       width = 12, height = 12)

# Display all plots in a grid (optional, for interactive viewing)
grid.arrange(
  p1, p2, p3, p4,  # Basic metrics
  p5, p6, p7, p8, p9, p10, # Diversity metrics
  p11, p12, p13, p14, p15, p16, p17, p18, # Readability metrics
  ncol = 3
) 