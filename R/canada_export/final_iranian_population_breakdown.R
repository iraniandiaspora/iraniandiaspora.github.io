# Final Iranian Population Breakdown
# Created: June 28, 2025
# Purpose: Create comprehensive CSV and updated visualization

library(tidyverse)
library(ggplot2)
library(scales)
library(here)

# Create final comprehensive breakdown
create_final_breakdown <- function() {
  
  cat("=== FINAL IRANIAN-CANADIAN POPULATION BREAKDOWN ===\n\n")
  
  # All population components
  breakdown_data <- tibble(
    Category = c(
      # Main categories
      "Born in Iran + Iranian Ethnic + Persian",
      "Born in Iran + Iranian Ethnic + No Persian",
      "Born in Iran + No Iranian Ethnic + Persian",
      "Born in Iran + No Iranian Ethnic + No Persian",
      "Not Born + Iranian Ethnic + Persian",
      "Not Born + Iranian Ethnic + No Persian",
      "Not Born + No Iranian Ethnic + Persian (Included)",
      "Not Born + No Iranian Ethnic + Persian (Excluded)",
      
      # Subtotals
      "Total Born in Iran",
      "Total Iranian Ethnic", 
      "Total Persian Speakers (Included)",
      "Total Excluded Persian Speakers",
      
      # Final total
      "TOTAL IRANIAN-CANADIAN POPULATION"
    ),
    
    Population = c(
      # Main cells
      105558,  # Born Iran + Iranian + Persian
      13556,   # Born Iran + Iranian + No Persian
      53779,   # Born Iran + No Iranian + Persian
      20000,   # Born Iran + No Iranian + No Persian
      13074,   # Not Born + Iranian + Persian
      11371,   # Not Born + Iranian + No Persian
      6630,    # Not Born + No Iranian + Persian (strict criteria)
      16222,   # Not Born + No Iranian + Persian (excluded)
      
      # Subtotals
      192893,  # Total born in Iran (105558 + 13556 + 53779 + 20000)
      143559,  # Total Iranian ethnic (105558 + 13556 + 13074 + 11371)
      179041,  # Total Persian included (105558 + 53779 + 13074 + 6630)
      16222,   # Excluded Persian speakers
      
      # Final
      223968   # Grand total
    ),
    
    Percentage_of_Total = c(
      # Calculate percentages
      round(105558/223968*100, 1),
      round(13556/223968*100, 1),
      round(53779/223968*100, 1),
      round(20000/223968*100, 1),
      round(13074/223968*100, 1),
      round(11371/223968*100, 1),
      round(6630/223968*100, 1),
      NA,  # Excluded group
      
      round(192893/223968*100, 1),
      round(143559/223968*100, 1),
      round(179041/223968*100, 1),
      NA,  # Excluded
      
      100.0
    ),
    
    Included_in_Total = c(
      rep("Yes", 7),
      "No",
      rep("Summary", 4),
      "Total"
    ),
    
    Notes = c(
      "Core Iranian population - born in Iran with Iranian identity and Persian language",
      "Iranians born in Iran who don't speak Persian (likely arrived as adults)",
      "Ethnic minorities from Iran who speak Persian (Kurdish, Azeri, etc.)",
      "Non-Persian ethnic minorities from Iran",
      "Second+ generation Iranians maintaining Persian",
      "Second+ generation Iranians without Persian",
      "Strict criteria: Second gen with Iranian parents, West Asian VM, strong Persian",
      "Likely Afghan, Tajik, other Central Asian Persian speakers",
      
      "All people born in Iran regardless of ethnicity or language",
      "All people identifying as Iranian ethnic regardless of birthplace",
      "All Persian speakers included in Iranian count",
      "Persian speakers excluded as likely non-Iranian",
      
      "Sum of all included categories"
    )
  )
  
  # Write main breakdown
  write_csv(breakdown_data, 
            here("output_population_page/iranian_population_final_breakdown_complete.csv"))
  
  # Create detailed breakdown of the 6,630
  refined_breakdown <- tibble(
    Rule = c(
      "Rule 1: Parent from region + Strong Persian + 2nd gen + Not Afghan",
      "Rule 2: West Asian VM + Strong Persian + Not Afghan",
      "Rule 3: Multiple ethnic + Persian MT + 2nd gen + Parent + Not Afghan",
      "Rules 1 & 2 overlap",
      "Rules 1 & 3 overlap", 
      "Rules 2 & 3 overlap",
      "All three rules overlap",
      "TOTAL UNIQUE INDIVIDUALS"
    ),
    
    Population = c(
      4630,  # Rule 1 total
      4074,  # Rule 2 total
      3074,  # Rule 3 total
      -2333, # Subtract overlap
      -593,  # Subtract overlap
      0,     # No overlap
      -1111, # Subtract overlap
      6630   # Final total
    ),
    
    Notes = c(
      "Second generation (genstat=2 or 3) with parent from West Asia/Africa",
      "Self-identified West Asian visible minority with strong Persian",
      "Multiple ethnic responses indicating mixed heritage",
      "Individuals meeting both Rules 1 and 2",
      "Individuals meeting both Rules 1 and 3",
      "No individuals meet only Rules 2 and 3",
      "Individuals meeting all three rules",
      "Total after removing overlaps"
    )
  )
  
  write_csv(refined_breakdown,
            here("output_population_page/persian_speakers_6630_detailed_rules.csv"))
  
  # Create visualization
  create_updated_visualization()
  
  cat("Files created:\n")
  cat("1. iranian_population_final_breakdown_complete.csv\n")
  cat("2. persian_speakers_6630_detailed_rules.csv\n")
  cat("3. iranian_population_final_visualization.png\n")
  
  return(breakdown_data)
}

# Create the updated visualization
create_updated_visualization <- function() {
  
  # Data for visualization
  viz_data <- tibble(
    x = c(1, 2, 3, 1, 2, 3),
    y = c(2, 2, 2, 1, 1, 1),
    population = c(105558, 53779, 13074, 13556, 20000, 11371),
    label = comma(population),
    cell_type = "standard"
  )
  
  # Split cell data  
  split_data <- tibble(
    x = c(3.75, 4.25),
    y = c(2, 2),
    width = c(0.4, 0.6),  # Proportional to 6630 vs 16222
    population = c(6630, 16222),
    label = comma(population),
    cell_type = c("included", "excluded"),
    explanation = c("Iranian\nheritage", "Non-Iranian\n(Afghan, etc.)")
  )
  
  # Create plot
  p <- ggplot() +
    # Main cells
    geom_tile(data = viz_data, 
              aes(x = x, y = y, fill = population),
              width = 0.95, height = 0.95, 
              color = "white", linewidth = 2) +
    
    # Split cell - included
    geom_tile(data = split_data[1,], 
              aes(x = x, y = y, width = width, height = 0.95),
              fill = "#6BAED6", color = "white", linewidth = 2) +
    
    # Split cell - excluded  
    geom_tile(data = split_data[2,], 
              aes(x = x, y = y, width = width, height = 0.95),
              fill = "#FB6A4A", color = "white", linewidth = 2) +
    
    # Labels
    geom_text(data = viz_data, 
              aes(x = x, y = y, label = label),
              size = 8, fontface = "bold") +
    
    geom_text(data = split_data,
              aes(x = x, y = y, label = paste0(label, "\n", explanation)),
              size = 5, fontface = "bold",
              color = c("black", "white")) +
    
    # Text above split cell
    annotate("text", x = 4, y = 2.55, 
             label = "Split by heritage",
             size = 5, fontface = "italic") +
    
    # Scale
    scale_fill_gradient(low = "#E6F2FF", high = "#004C99",
                       labels = comma,
                       guide = "none") +
    
    # Axes
    scale_x_continuous(
      breaks = 1:4,
      labels = c("Born Iran +\nIranian Ethnic", 
                 "Born Iran +\nNo Iranian Ethnic",
                 "Not Born Iran +\nIranian Ethnic",
                 "Not Born Iran +\nNo Iranian Ethnic"),
      expand = c(0.02, 0.02)
    ) +
    scale_y_continuous(
      breaks = 1:2,
      labels = c("No Persian Mother Tongue\nor Home Language",
                 "Persian Mother Tongue\nOR Home Language"),
      expand = c(0.02, 0.02),
      limits = c(0.7, 2.7)
    ) +
    
    # Labels and theme
    labs(title = "Iranian-Canadian Population by Identification Criteria",
         subtitle = "Total: 223,968 people",
         caption = paste0(
           "Persian speakers in 'Not Born Iran + No Iranian Ethnic' category are evaluated using strict criteria:\n",
           "• Included (6,630): Parent from Iran/region + (Strong Persian & 2nd gen) OR (West Asian VM & Strong Persian) OR (Multiple ethnic & Persian MT & 2nd gen)\n",
           "• Excluded (16,222): Likely Afghan, Tajik, or other Central Asian Persian speakers without clear Iranian heritage markers"
         )) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 16, hjust = 0.5),
      plot.caption = element_text(size = 10, hjust = 0, face = "italic"),
      axis.text.x = element_text(size = 11, hjust = 0.5),
      axis.text.y = element_text(size = 11, angle = 0, hjust = 1),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      plot.margin = margin(20, 20, 20, 20)
    ) +
    coord_fixed(ratio = 0.8)
  
  # Save
  ggsave(filename = here("output_population_page/iranian_population_final_visualization.png"),
         plot = p,
         width = 14,
         height = 8,
         dpi = 300)
  
  return(p)
}

# Execute
results <- create_final_breakdown()