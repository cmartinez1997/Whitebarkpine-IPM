
# exlporing survival dataframe with whitebark pine codes 


whitebarkpine_test <- read_csv("data_processed/0814_whitebark_pine.csv")



whitebarkpine_test$PLT_CN <- as.character(whitebarkpine_test$PLT_CN)
whitebarkpine_test$TREE_CN <- as.character(whitebarkpine_test$TREE_CN)
whitebarkpine_test$COND_CN <- as.character(whitebarkpine_test$COND_CN)


length(unique(whitebarkpine_test$TREE_COMPOSITE_ID)) # 20038 wbp trees 

# filter out only remeasured trees 
whitebark_pine_measures <- whitebarkpine_test |> 
  group_by(STATECD, TREE_COMPOSITE_ID) |>
  summarize(n = length(unique(INVYR))) 

whitebark_pine_repeats <- filter(whitebark_pine_measures, n > 1) |>
  left_join(whitebarkpine_all)
whitebark_pine_repeats <- whitebark_pine_repeats %>% 
  relocate(STATECD, TPA_UNADJ, TREE_COMPOSITE_ID, INVYR, CYCLE, MEASYEAR, STATUSCD, DIA)

length(unique(whitebark_pine_repeats$TREE_COMPOSITE_ID)) # 8147 remeasured trees (no wyoming)

#TPA adjustment for microplot individuals
whitebark_pine_microplot <- whitebark_pine_repeats %>% 
  filter((TPA_UNADJ == 74.965282) | 
           (is.na(TPA_UNADJ) & (lag(TPA_UNADJ) == 74.965282 | lead(TPA_UNADJ) == 74.965282))
  ) 

#okay now add the expansion factor to scale microplot trees up to the subplot level so we can compare total number of tree mortality
whitebark_pine_microplot <- whitebark_pine_microplot %>%
  mutate(microplot_corrected = 1 * 12.4567479)

whitebark_pine_repeats <- whitebark_pine_repeats %>%
  left_join(select(whitebark_pine_microplot, TREE_COMPOSITE_ID, microplot_corrected), by = "TREE_COMPOSITE_ID")

str(whitebark_pine_repeats)

# do same steps as previous to plot the corrected mortality
mort_trees <- whitebark_pine_repeats %>%
  filter(STATUSCD == 2 & PREV_STATUS_CD == 1) %>% 
  relocate(STATUSCD, PREV_STATUS_CD, MEASYEAR, .after = 3) %>% 
  distinct() #1869 dead trees

surv_trees <- whitebark_pine_repeats %>%
  filter(STATUSCD == 1  & PREV_STATUS_CD == 1) %>% 
  select(TREE_COMPOSITE_ID) %>% 
  distinct()

length(unique(surv_corrected$TREE_COMPOSITE_ID)) #2761
length(unique(mort_trees$TREE_COMPOSITE_ID)) #1869
  
mort_dia <- whitebark_pine_repeats %>%
  filter(TREE_COMPOSITE_ID %in% mort_trees$TREE_COMPOSITE_ID & STATUSCD == 1) %>% 
  select(TREE_COMPOSITE_ID, DIA, microplot_corrected, AGENTCD) %>% 
  mutate(weight = if_else(is.na(microplot_corrected), 1, microplot_corrected)) %>% 
  distinct(TREE_COMPOSITE_ID, .keep_all = TRUE) 
sum(mort_dia$weight) #2656.253 trees died with the expansion factor

surv_corrected <- whitebark_pine_repeats %>%
  filter(STATUSCD == 1 & PREV_STATUS_CD == 1) %>%
  select(TREE_COMPOSITE_ID, DIA, microplot_corrected) %>%
  mutate(weight = if_else(is.na(microplot_corrected), 1, microplot_corrected)) %>% 
  select(-microplot_corrected) %>% 
  distinct(TREE_COMPOSITE_ID, .keep_all = TRUE) 
sum(surv_corrected$weight) #7870.72 trees with expansion factor survived, 46.45 percent of trees died from 2003-2019 with the expansion factor


length(unique(surv_corrected$TREE_COMPOSITE_ID)) #2761
length(unique(mort_trees$TREE_COMPOSITE_ID)) #1869

#67.69% without scaling of trees on microplot died from 2003 - 2019


#get some means
# Calculate mean DIA
mean_dia_alive <- mean(surv_corrected$DIA, na.rm = TRUE)
mean_dia_dead <- mean(mort_dia$DIA, na.rm = TRUE)

#plotting mortality 

# Plot histogram 
mort_hist <- ggplot() +
  geom_histogram(data = surv_corrected, aes(x = DIA, weight = weight, fill = "Survived"), binwidth = 1, color = "white", alpha = 0.75, position = "identity") + 
  geom_histogram(data = mort_dia, aes(x = DIA, weight = weight, fill = "Died"), binwidth = 1, color = "white", alpha = 0.4, position = "identity") +
  labs(title = "Whitebark Pine Mortality and Survival of remeasured trees from 2003-2019 in the interior west",
       x = "Diameter (in.)",
       y = "Number of Trees", 
       fill = "Tree Status") +
  geom_vline(aes(xintercept = mean_dia_dead), color = "#006699", linetype = "dashed") + 
  geom_vline(aes(xintercept = mean_dia_alive), color = "#9999FF", linetype = "dashed") + 
  theme_bw() + 
  theme(
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 17), 
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 17),       
    legend.key.size = unit(1.5, "lines")   
  ) +
  scale_fill_manual(values = c("Died" = "#006699", "Survived" = "#9999FF")) + 
  xlim(0, 20)

