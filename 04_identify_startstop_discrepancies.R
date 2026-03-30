library(dplyr)
library(tidyr)
# readin datasets
swdt <- read.csv("chec_swdt.csv")
tx_hx <- read.csv("tx_hx.csv")
registry <- read.csv("registry_deID.csv")

# merge 2 datasets
merged_swdt <- swdt %>%
  left_join(tx_hx, by = c("newid" = "newID", "cftr_mod" = "cftr_mod"))
merged_swdt <- merged_swdt[,c(-1,-5)]
merged_swdt <- merged_swdt %>%
  rename(
    "chec_start_date_diff" = "start_date_diff",
    "chec_stop_date_diff" = "stop_date_diff"
  )
swdt_out <- merged_swdt %>%
  rowwise() %>%
  mutate(
    idx = {
      starts <- c_across(starts_with("start_date_"))
      stops  <- c_across(starts_with("stop_date_"))
      
      which(starts <= swdt & swdt <= stops)[1]
    },
    reg_start_date = ifelse(is.na(idx), NA, c_across(starts_with("start_date_"))[idx]),
    reg_stop_date  = ifelse(is.na(idx), NA, c_across(starts_with("stop_date_"))[idx])
  ) %>%
  ungroup() %>%
  select(-idx)


write.csv(swdt_out, "chec_mod_merged_0.csv")

# allowing a certain number of encounters
# function if we are expanding only 1 interval
expand_interval <- function(swdt, starts, stops, gap, tolerate = 2) {
  idx <- which(starts < swdt & swdt < stops)[1]
  if (is.na(idx)) return(c(exp_start = NA, exp_stop = NA))
  
  s <- starts[idx]
  e <- stops[idx]
  
  g_prev <- if (idx > 1) gap[idx - 1] else NA_real_
  g_next <- if (idx < length(starts)) gap[idx + 1] else NA_real_
  
  # expand to previous if condition holds
  if (!is.na(g_prev) && g_prev <= tolerate) {
    s <- starts[idx - 1]
  }
  
  # expand to next if condition holds
  if (!is.na(g_next) && g_next <= tolerate) {
    e <- stops[idx + 1]
  }
  
  c(exp_start = s, exp_stop = e)
}

# if we are keeping expanding as long as there is gap_enc < tolerate
# function
expand_interval <- function(swdt, starts, stops, gap, tolerate = 0) {
  idx <- which(starts < swdt & swdt <= stops)[1]
  if (is.na(idx)) return(c(exp_start = NA, exp_stop = NA))
  
  left <- right <- idx
  s <- starts[idx]
  e <- stops[idx]
  
  # expand backward while condition holds
  while (left > 1) {
    g_prev <- gap[left - 1]
    if (is.na(g_prev) || g_prev > tolerate) break
    
    left <- left - 1
    s <- starts[left]
  }
  
  # expand forward while condition holds
  while (right < length(starts)) {
    g_next <- gap[right + 1]
    if (is.na(g_next) || g_next > tolerate) break
    
    right <- right + 1
    e <- stops[right]
  }
  
  c(exp_start = s, exp_stop = e)
}

# apply function to our dataset
df_out <- swdt_out %>%
  rowwise() %>%
  mutate(
    .res = list(
      expand_interval(
        swdt   = swdt,
        starts = c_across(starts_with("start_date_")),
        stops  = c_across(starts_with("stop_date_")),
        gap    = c_across(starts_with("gap_enc_")),
        tolerate = 2
      )
    ),
    expanded_start = .res[[1]],
    expanded_stop  = .res[[2]]
  ) %>%
  ungroup() %>%
  select(-.res)
check <- df_out[df_out$newid==1016,]
write.csv(df_out, "chec_mod_merged_0.csv")
write.csv(df_out, "chec_mod_merged_1.csv")
write.csv(df_out, "chec_mod_merged_2.csv")

### start date discrepancies ###
median(df_out$chec_min_start - df_out$expanded_start, na.rm = T)
IQR(df_out$chec_min_start - df_out$expanded_start, na.rm = T)

# # incorrectly included encounters
# df_out$incor <- ifelse(df_out$chec_min_start > df_out$expanded_start, 1, 0)
# incorr_enc <- df_out[df_out$incor == 1,] %>%
#   rowwise() %>%
#   mutate(
#     gap_sum = {
#       sd  <- c_across(starts_with("start_date_"))
#       gap <- c_across(starts_with("gap_enc_"))
#       idx <- which(sd >= expanded_start & sd < chec_min_start)
#       if (length(idx) == 0) 0 else sum(gap[idx], na.rm = TRUE)
#     }
#   ) %>%
#   ungroup()
# 
# sum(incorr_enc$gap_sum)
# 
# # incorrectly excluded encounters
# df_out$incor <- ifelse(df_out$chec_min_start < df_out$expanded_start, 1, 0)
# incorr_enc <- df_out[df_out$incor == 1,] %>%
#   rowwise() %>%
#   mutate(
#     gap_sum = {
#       sd  <- c_across(starts_with("start_date_"))
#       gap <- c_across(starts_with("gap_enc_"))
#       idx <- which(sd < expanded_start & sd >= chec_min_start)
#       if (length(idx) == 0) 0 else sum(gap[idx], na.rm = TRUE)
#     }
#   ) %>%
#   ungroup()
# 
# sum(incorr_enc$gap_sum)

### stop date discrepancies ###
median(df_out$chec_max_stop - df_out$expanded_stop, na.rm = T)
IQR(df_out$chec_max_stop - df_out$expanded_stop, na.rm = T)

# # incorrectly included encounters
# df_out$incor <- ifelse(df_out$chec_max_stop < df_out$expanded_stop, 1, 0)
# incorr_enc <- df_out[df_out$incor == 1,] %>%
#   rowwise() %>%
#   mutate(
#     gap_sum = {
#       sd  <- c_across(starts_with("stop_date_"))
#       gap <- c_across(starts_with("gap_enc_"))
#       idx <- which(sd >= chec_max_stop & sd < expanded_stop)
#       if (length(idx) == 0) 0 else sum(gap[idx], na.rm = TRUE)
#     }
#   ) %>%
#   ungroup()
# 
# sum(incorr_enc$gap_sum)
# 
# # incorrectly excluded encounters
# df_out$incor <- ifelse(df_out$chec_max_stop > df_out$expanded_stop, 1, 0)
# incorr_enc <- df_out[df_out$incor == 1,] %>%
#   rowwise() %>%
#   mutate(
#     gap_sum = {
#       sd  <- c_across(starts_with("stop_date_"))
#       gap <- c_across(starts_with("gap_enc_"))
#       idx <- which(sd >= expanded_stop & sd < chec_max_stop)
#       if (length(idx) == 0) 0 else sum(gap[idx], na.rm = TRUE)
#     }
#   ) %>%
#   ungroup()
# 
# sum(incorr_enc$gap_sum)
# 
# # Number of Correctly Included Encounters
# incorr_enc <- df_out %>%
#   rowwise() %>%
#   mutate(
#     gap_sum = {
#       std <- c_across(starts_with("start_date_"))
#       sd  <- c_across(starts_with("stop_date_"))
#       gap <- c_across(starts_with("gap_enc_"))
#       idx <- which(std >= max(chec_min_start, expanded_start) & sd < min(chec_max_stop, expanded_stop))
#       if (length(idx) == 0) 0 else sum(gap[idx], na.rm = TRUE)
#     }
#   ) %>%
#   ungroup()
# sum(incorr_enc$gap_sum)


####################### Updated Version #######################
df_time <- df_out[,c(1,2,4,7,77,78)]


count_encounters <- function(df_time, registry) {
  
  dat <- df_time %>%
    left_join(registry, by = c("newid" = "newID"))
  
  list(
    incor_inclu_start = dat %>%
      filter(!is.na(newid) & stdyrs >= expanded_start & stdyrs < chec_min_start) %>%
      summarise(n_events = n(), .by = newid),
    
    incor_exclu_stop = dat %>%
      filter(!is.na(newid) & stdyrs > expanded_stop & stdyrs <= chec_max_stop) %>%
      summarise(n_events = n(), .by = newid),
    
    incor_exclu_start = dat %>%
      filter(!is.na(newid) & stdyrs < expanded_start & stdyrs >= chec_min_start) %>%
      summarise(n_events = n(), .by = newid),
    
    incor_inclu_stop = dat %>%
      filter(!is.na(newid) & stdyrs <= expanded_stop & stdyrs > chec_max_stop) %>%
      summarise(n_events = n(), .by = newid),
    
    cor_inclu = dat %>%
      filter(
        stdyrs >= expanded_start &
          stdyrs >= chec_min_start &
          stdyrs <= expanded_stop &
          stdyrs <= chec_max_stop
      ) %>%
      summarise(n_events = n(), .by = newid)
  )
}

res <- count_encounters(df_time, registry)
sum(res$incor_inclu_start$n_events)
sum(res$incor_exclu_stop$n_events)
sum(res$incor_exclu_start$n_events)
sum(res$incor_inclu_stop$n_events)
sum(res$cor_inclu$n_events)


