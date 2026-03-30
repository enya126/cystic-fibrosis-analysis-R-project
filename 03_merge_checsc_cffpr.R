# library
library(dplyr)
library(ggplot2)

# read in dataset
chec <- read.csv("chec_mod.csv")


# remove duplicated start date & NA stop date rows, keep unique ones
chec_cleaned <- chec %>%
  group_by(newID) %>%
  filter({
    dup_start_date <- duplicated(chec_start_date) | duplicated(chec_start_date, fromLast = TRUE)
    !(dup_start_date & is.na(chec_stop_date))
  }) %>%
  ungroup()

chec_unique <- chec_cleaned %>%
  group_by(newID, cftr_mod) %>%
  summarise(chec_min_start = min(chec_start_date), chec_max_start = max(chec_start_date),
            chec_min_stop = if (all(is.na(chec_stop_date))) NA else min(chec_stop_date, na.rm = TRUE), 
            chec_max_stop = if (all(is.na(chec_stop_date))) NA else max(chec_stop_date, na.rm = TRUE)
            ) # if all NA return NA, else return min/max date

# save the result
write.csv(chec_unique, "chec_mod_short.csv")

# table of summary statistics
# a. # participants with more than one start date per modulator
summary_a <- chec %>%
  group_by(cftr_mod, newID) %>%
  summarise(n_start_dates = n(), .groups = "drop") %>% # .groups = "drop" removes all grouping after the summarise step
  filter(n_start_dates > 1) %>%
  group_by(cftr_mod) %>%
  summarise('Number of participants with >1 start date collected' = n_distinct(newID))

# b. # participants with discrepant start dates
summary_b <- chec_unique %>%
  group_by(newID, cftr_mod) %>%
  summarise(discrepant_start = sum(chec_min_start != chec_max_start, na.rm = TRUE), .groups = "drop") %>%
  filter(discrepant_start > 0) %>%
  group_by(cftr_mod) %>%
  summarise('Number of participants with discrepant start dates' = n_distinct(newID))

# c.
summary_c <- chec_unique %>%
  mutate(cftr_mod = na_if(cftr_mod, "")) %>%         # convert "" to NA in cftr_mod col
  filter(!is.na(cftr_mod))  %>%
  mutate(diff = abs(chec_min_start-chec_max_start)) %>%
  group_by(cftr_mod) %>%
  summarise('Absolute median (range) of difference in start dates' = median(diff, na.rm=T))

# d. # participants with differences in start dates 
summary_d <- chec_unique %>%
  mutate(cftr_mod = na_if(cftr_mod, "")) %>%         # convert "" to NA in cftr_mod col
  filter(!is.na(cftr_mod))  %>%
  mutate(diff = abs(chec_min_start-chec_max_start), diff_weeks = diff >= 1/84, diff_months = diff >= 1/12, diff_6months = diff >= 1/2) %>%
  group_by(cftr_mod) %>%
  summarise('≥ 1 week' = sum(diff_weeks == TRUE, na.rm = TRUE), '≥ 1 month' = sum(diff_months == TRUE, na.rm = TRUE), '≥ 6 months' = sum(diff_6months == TRUE, na.rm = TRUE))

# e. - i. repeat the summary statistics for stop dates
summary_e <- na.omit(chec_unique) %>%
  group_by(cftr_mod) %>%
  summarise('Number of participants with a stop date collected' = n_distinct(newID))

# f.
summary_f <- chec %>%
  filter(!is.na(chec_stop_date)) %>%
  group_by(cftr_mod, newID) %>%
  summarise(n_stop_dates = n(), .groups = "drop") %>% # .groups = "drop" removes all grouping after the summarise step
  filter(n_stop_dates > 1) %>%
  group_by(cftr_mod) %>%
  summarise('Number of participants with >1 stop date collected' = n_distinct(newID))

# g.
summary_g <- chec_unique %>%
  group_by(newID, cftr_mod) %>%
  summarise(discrepant_stop = sum(chec_min_stop != chec_max_stop, na.rm = TRUE), .groups = "drop") %>%
  filter(discrepant_stop > 0) %>%
  group_by(cftr_mod) %>%
  summarise('Number of participants with discrepant stop dates' = n_distinct(newID))

# h.
summary_h <- chec_unique %>%
  mutate(cftr_mod = na_if(cftr_mod, "")) %>%         # convert "" to NA in cftr_mod col
  filter(!is.na(cftr_mod))  %>%
  mutate(diff = abs(chec_min_stop-chec_max_stop)) %>%
  group_by(cftr_mod) %>%
  summarise('Absolute median (range) of difference in stop dates' = median(diff, na.rm=T))

# i.
summary_i <- chec_unique %>%
  mutate(cftr_mod = na_if(cftr_mod, "")) %>%         # convert "" to NA in cftr_mod col
  filter(!is.na(cftr_mod))  %>%
  mutate(diff = abs(chec_min_stop-chec_max_stop), diff_weeks = diff >= 1/84, diff_months = diff >= 1/12, diff_6months = diff >= 1/2) %>%
  group_by(cftr_mod) %>%
  summarise('difference ≥ 1 week' = sum(diff_weeks == TRUE, na.rm = TRUE), 'difference ≥ 1 month' = sum(diff_months == TRUE, na.rm = TRUE), 'difference ≥ 6 months' = sum(diff_6months == TRUE, na.rm = TRUE))

# Combine both into one table
library(tidyr)
combined_summary <- full_join(summary_a, summary_b, by = "cftr_mod") %>%
  full_join(summary_c, by = "cftr_mod") %>%
  full_join(summary_d, by = "cftr_mod") %>%
  full_join(summary_e, by = "cftr_mod") %>%
  full_join(summary_f, by = "cftr_mod") %>%
  full_join(summary_g, by = "cftr_mod") %>%
  full_join(summary_h, by = "cftr_mod") %>%
  full_join(summary_i, by = "cftr_mod") %>%
  pivot_longer(-cftr_mod, names_to = "Metric", values_to = "Value")%>%
  pivot_wider(names_from = cftr_mod, values_from = Value)

# CFFPR vs CHEC-SC
chec_short <- read.csv("chec_mod_short.csv")
cffpr <- read.csv("Registry_tx_dates.csv")

cffpr$cftr_mod[cffpr$cftr_mod == 'Vx770'] <- 'Ivacaftor'
cffpr$cftr_mod[cffpr$cftr_mod == 'vx661comb'] <- 'Tezacaftor / Ivacaftor'
cffpr$cftr_mod[cffpr$cftr_mod == 'vx445comb'] <- 'Elexacaftor / Tezacaftor / Ivacaftor'
cffpr$cftr_mod[cffpr$cftr_mod == 'Vx809comb'] <- 'Lumacaftor / Ivacaftor'

# merge two datasets
merged_dataset <- left_join(chec_short[,-1], cffpr[,-1], by = c("newID", "cftr_mod"))

merged_dataset <- merged_dataset %>%
  mutate(start_date_diff = (chec_min_start-start_date_1), stop_date_diff = (chec_max_stop - stop_date_1),
         tx_dur_diff = (chec_max_stop - chec_min_start) - (stop_date_1 - start_date_1),
         ) %>%
  rowwise() %>%
  mutate(total_gap_dur = sum(c_across(starts_with("gap_dur_")), na.rm = TRUE)) %>% # total gap dur
  ungroup() %>%
  rowwise() %>%
  mutate(total_gap_dur_no_mod = sum(
    c_across(gap_dur_1:gap_dur_12)[c_across(gap_mod_1:gap_mod_12) == 0],na.rm = TRUE)) %>% # total_gap_dur_no_mod
  ungroup() %>%
  rowwise() %>%
  mutate(total_gap_num = sum(c_across(gap_dur_1:gap_dur_12) > 0, na.rm = TRUE)) %>% # total gap num
  ungroup() %>%
  rowwise() %>%
  mutate(total_gap_num_no_mod = sum(c_across(gap_mod_1:gap_mod_12) == 0, na.rm = TRUE)) %>% # total gap num no mode
  ungroup()
  
# table of summary statistics for merged dataset
# a. Number of participants with any gaps with duration > 0
sum_a <- merged_dataset %>%
  filter(total_gap_num>0) %>%
  group_by(cftr_mod) %>%
  summarise('Number of participants with any gaps with duration > 0' = n_distinct(newID))

# b.
sum_b <- merged_dataset %>%
  filter(total_gap_num_no_mod>0&total_gap_num>0) %>%
  group_by(cftr_mod) %>%
  summarise('Number of participants with any gaps during which no modulator was prescribed with duration > 0' = n_distinct(newID))

# c. 
sum_c <- merged_dataset %>%
  mutate(cftr_mod = na_if(cftr_mod, "")) %>%         # convert "" to NA in cftr_mod col
  filter(!is.na(cftr_mod)&total_gap_dur>0)  %>%
  group_by(cftr_mod) %>%
  summarise('Median total gap duration' = median(total_gap_dur, na.rm=T))

# d. 
sum_d <- merged_dataset %>%
  mutate(cftr_mod = na_if(cftr_mod, "")) %>%         # convert "" to NA in cftr_mod col
  filter(!is.na(cftr_mod)&total_gap_dur_no_mod>0) %>%
  group_by(cftr_mod) %>%
  summarise('Median total gap duration with no modulator prescription' = median(total_gap_dur_no_mod, na.rm=T))

# e. 
sum_e <- merged_dataset %>%
  mutate(cftr_mod = na_if(cftr_mod, "")) %>%         # convert "" to NA in cftr_mod col
  filter(!is.na(cftr_mod)&total_gap_num>0) %>%
  group_by(cftr_mod) %>%
  summarise('Median number of gaps' = median(total_gap_num, na.rm=T))

# f.
sum_f <- merged_dataset %>%
  mutate(cftr_mod = na_if(cftr_mod, "")) %>%         # convert "" to NA in cftr_mod col
  filter(!is.na(cftr_mod)&total_gap_num_no_mod>0) %>%
  group_by(cftr_mod) %>%
  summarise('Median number of gaps during which no modulator was prescribed' = median(total_gap_num_no_mod, na.rm=T))

# g.
sum_g <- merged_dataset %>%
  mutate(cftr_mod = na_if(cftr_mod, "")) %>%         # convert "" to NA in cftr_mod col
  filter(!is.na(cftr_mod)) %>%
  group_by(cftr_mod) %>%
  summarise('Median of start_date_diff' = median(start_date_diff, na.rm=T))

# h. 
sum_h <- merged_dataset %>%
  mutate(cftr_mod = na_if(cftr_mod, "")) %>%         # convert "" to NA in cftr_mod col
  filter(!is.na(cftr_mod))  %>%
  mutate(diff = abs(start_date_diff), diff_weeks = diff >= 1/84, diff_months = diff >= 1/12, diff_6months = diff >= 1/2) %>%
  group_by(cftr_mod) %>%
  summarise('difference ≥ 1 week' = sum(diff_weeks == TRUE, na.rm = TRUE), 'difference ≥ 1 month' = sum(diff_months == TRUE, na.rm = TRUE), 'difference ≥ 6 months' = sum(diff_6months == TRUE, na.rm = TRUE))

# i.
sum_i <- merged_dataset %>%
  mutate(cftr_mod = na_if(cftr_mod, "")) %>%         # convert "" to NA in cftr_mod col
  filter(!is.na(cftr_mod)) %>%
  group_by(cftr_mod) %>%
  summarise('Median of stop_date_diff' = median(stop_date_diff, na.rm=T))

# j. 
sum_j <- merged_dataset %>%
  mutate(cftr_mod = na_if(cftr_mod, "")) %>%         # convert "" to NA in cftr_mod col
  filter(!is.na(cftr_mod))  %>%
  mutate(diff = abs(stop_date_diff), diff_weeks = diff >= 1/84, diff_months = diff >= 1/12, diff_6months = diff >= 1/2) %>%
  group_by(cftr_mod) %>%
  summarise('difference ≥ 1 week' = sum(diff_weeks == TRUE, na.rm = TRUE), 'difference ≥ 1 month' = sum(diff_months == TRUE, na.rm = TRUE), 'difference ≥ 6 months' = sum(diff_6months == TRUE, na.rm = TRUE))

# k.
sum_k <- merged_dataset %>%
  mutate(cftr_mod = na_if(cftr_mod, "")) %>%         # convert "" to NA in cftr_mod col
  filter(!is.na(cftr_mod)) %>%
  group_by(cftr_mod) %>%
  summarise('Median of tx_dur_diff' = median(tx_dur_diff, na.rm=T))

# l. 
sum_l <- merged_dataset %>%
  mutate(cftr_mod = na_if(cftr_mod, "")) %>%         # convert "" to NA in cftr_mod col
  filter(!is.na(cftr_mod))  %>%
  mutate(diff = abs(tx_dur_diff), diff_weeks = diff >= 1/84, diff_months = diff >= 1/12, diff_6months = diff >= 1/2) %>%
  group_by(cftr_mod) %>%
  summarise('difference ≥ 1 week' = sum(diff_weeks == TRUE, na.rm = TRUE), 'difference ≥ 1 month' = sum(diff_months == TRUE, na.rm = TRUE), 'difference ≥ 6 months' = sum(diff_6months == TRUE, na.rm = TRUE))


# Combine both into one table
library(tidyr)
summary2 <- full_join(sum_a, sum_b, by = "cftr_mod") %>%
  full_join(sum_c, by = "cftr_mod") %>%
  full_join(sum_d, by = "cftr_mod") %>%
  full_join(sum_e, by = "cftr_mod") %>%
  full_join(sum_f, by = "cftr_mod") %>%
  full_join(sum_g, by = "cftr_mod") %>%
  full_join(sum_h, by = "cftr_mod") %>%
  full_join(sum_i, by = "cftr_mod") %>%
  full_join(sum_j, by = "cftr_mod") %>%
  full_join(sum_k, by = "cftr_mod") %>%
  full_join(sum_l, by = "cftr_mod") %>%
  pivot_longer(-cftr_mod, names_to = "Metric", values_to = "Value")%>%
  pivot_wider(names_from = cftr_mod, values_from = Value)%>%
  mutate(across(where(is.numeric), ~ round(.))) # reduce decimal places

write.csv(merged_dataset, "tx_hx.csv")

# 7. create a figure depicting the treatment duration as specified in CHEC-SC (chec_min_start_date to chec_max_stop_date) and CFFPR (start_date_1 and stop_date_1) as separate lines. 
tx_diff_1yr <- na.omit(merged_dataset[merged_dataset$tx_dur_diff >= 1, c(1,3,6,7,8)])
tx_diff_1yr <- tx_diff_1yr %>% rename(CFFPR_start = start_date_1,
       CFFPR_stop = stop_date_1,
       CHEC_start = chec_min_start,
       CHEC_stop = chec_max_stop)
tx_diff_long <- tx_diff_1yr %>%
  pivot_longer(
    cols = c(2,3,4,5),
    names_to = c("source", ".value"),
    names_sep = "_"
  )

# make 2 lines slightly separate from each other
tx_diff_long <- tx_diff_long %>%
  mutate(y_pos = as.numeric(factor(newID)) + 
           ifelse(source == "CHEC", 0.1, -0.1))  # adjust spacing

# plot
ggplot(tx_diff_long, aes(
  x = start,
  xend = stop,
  y = y_pos,
  yend = y_pos,
  color = source
)) +
  geom_segment(linewidth = 1) +
  labs(
    x = "Time",
    y = "Participant ID",
    color = "Data source"
  ) + 
  geom_vline(xintercept = 0,
             linetype = "dashed", linewidth = 0.8, color = "purple") +
  theme_bw()

ggplot(tx_diff_long[c(1:40),], aes(
  x = start,
  xend = stop,
  y = y_pos,
  yend = y_pos,
  color = source
)) +
  geom_segment(linewidth = 1) +
  labs(
    x = "Time",
    y = "Participant ID",
    color = "Data source"
  ) + 
  geom_vline(xintercept = 0,
             linetype = "dashed", linewidth = 0.8, color = "purple") +
  theme_bw()


