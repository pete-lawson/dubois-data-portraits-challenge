library(ggplot2)
library(tidyr)
library(dplyr)
library(stringr)

data <- readr::read_csv("data.csv") %>%
  pivot_longer(cols = 3:7,
               names_to = "attributes",
               values_to = "amount") %>%
  mutate(attributes = factor(attributes,
                             levels = c("Other",
                                        "Tax",
                                        "Clothes",
                                        "Food",
                                        "Rent"))) %>%
  mutate(Class = forcats::fct_rev(Class)) %>% 
  mutate(textcolor = ifelse(str_equal(attributes, "Rent"), "white", "black"))

ggplot(data, aes(
  x = amount,
  y = Class,
  fill = attributes
)) +
  geom_bar(
    stat = "identity",
    position = "stack",
    width = 0.5,
    alpha = 0.9,
    color = "#333333",
    size = .2
  ) +
  geom_text(
   aes(label = ifelse(amount != 0, paste0(amount, "%"), ""),
       color = textcolor),
    position = position_stack(vjust = 0.5),
    size = 3.5,
    fontface = 'bold'
  ) +
  scale_color_manual(values = c("white" = "#d3baa1", 
                                "black" = "#161613")) + 
  
theme_minimal() +
  scale_fill_manual(
    values = c(
      "Other" = "#c6b4a1",
      "Tax" = "#ab9b95",
      "Clothes" = "#c17e6c",
      "Food" = "#8b6a8c",
      "Rent" = "#161613"
    )
  )  +
  theme(
    plot.background = element_rect(fill = '#d3baa1'),
    panel.grid = element_blank(),
    legend.position = "none",
    axis.title = element_blank(),
    axis.text.x = element_blank()
  )
