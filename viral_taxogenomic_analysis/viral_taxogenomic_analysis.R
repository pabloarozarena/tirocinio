library(dplyr)
library(ggplot2)

# Read the data
data <- read.csv("sequences.csv")

# Replace blank cells with "Unspecified"
data$Family <- ifelse(data$Family == "", "Unspecified", data$Family)

# Group by family and count sequences
family_counts <- data %>%
  group_by(Family) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

# Bar plot for all families
ggplot(family_counts, aes(x = reorder(Family, -Count), y = Count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(title = "Number of Sequences per Viral Family",
       x = "Viral Family",
       y = "Number of Sequences") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Mean number of sequences excluding Coronaviridae
filtered_data <- data %>%
  filter(!Family %in% c("Coronaviridae", "Unspecified"))

family_counts_filtered <- filtered_data %>%
  group_by(Family) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

mean_count <- mean(family_counts_filtered$Count)

ggplot(family_counts_filtered, aes(x = reorder(Family, -Count), y = Count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  labs(title = "Bar Plot of Viral Families", x = "Viral Family", y = "Count") +
  geom_hline(yintercept = mean_count, color = "red") +
  annotate("text", x = 1, y = mean_count + 1, label = paste("Mean =", round(mean_count, 2)), color = "red")

# Inspect Unspecified Family
unspecified_data <- data %>%
  filter(Family == "Unspecified") %>%
  select(Family, Genus, Species)

# Replace blank cells with "Unspecified"
unspecified_data$Genus <- ifelse(unspecified_data$Genus == "", "Unspecified", unspecified_data$Genus)

genus_counts <- unspecified_data %>%
  group_by(Genus) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

genus_counts_check <- genus_counts %>% 
  mutate(is_present_in_family_counts = ifelse(Genus %in% family_counts$Family, TRUE, FALSE))

#Genus Counts
all_genus <- data %>%
  group_by(Genus) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))

all_genus$Genus <- ifelse(all_genus$Genus == "", "Unspecified", all_genus$Genus)

#Filter out Betacoronavirus and Unspecified Genera
filtered_genus <- all_genus %>%
  filter(!Genus %in% c("Betacoronavirus", "Unspecified"))

# Mean number of sequences per genus
mean_count_genus <- mean(filtered_genus$Count)

ggplot(filtered_genus, aes(x = reorder(Genus, -Count), y = Count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme(axis.text.x = element_blank()) + 
  labs(title = "Bar Plot of Viral Genus", x = "Viral Genus", y = "Count") +
  geom_hline(yintercept = mean_count, color = "red") +
  annotate("text", x = 1, y = mean_count + 1, label = paste("Mean =", round(mean_count_genus, 2)), color = "red")

filtered_genus_100 <- filtered_genus %>%
  filter(Count >=100)

ggplot(filtered_genus_100, aes(x = reorder(Genus, -Count), y = Count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme(axis.text.x = element_blank()) + 
  labs(title = "Bar Plot of Viral Genus", x = "Viral Genus", y = "Count") +
  geom_hline(yintercept = mean_count, color = "red") +
  annotate("text", x = 1, y = mean_count + 1, label = paste("Mean =", round(mean_count_genus, 2)), color = "red")

genus_filtered <- unspecified_data %>% 
  filter(Genus != "Unspecified")

genus_filtered_count <- genus_filtered %>%
  group_by(Genus) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))
