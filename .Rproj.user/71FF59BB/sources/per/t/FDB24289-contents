
# load the raw data -------------------------------------------------------

# the current code directly call Google Sheets API and local the data from my Google drive.
# a sample dataset can be found from folder .\\data\\dat_mpr_league_1819.xlsx
# for example, one can read the data use the following code:
# dat_standing <- read_excel(".\\data\\dat_mpr_league_1819.xlsx", sheet = "standings")
# dat_detail <- read_excel(".\\data\\dat_mpr_league_1819.xlsx", sheet = "detail")

# return spreadsheets feed informion of my Google drive through Google Sheets API
gs_ls()

# get the fantasy bball googlesheets
mpr_fantasy <- gs_title("MPR_league_1819")


# read the standing data
dat_standing <- gs_read(ss = mpr_fantasy, ws = "standings")

# create long table form of the dat_standing
dat_rank <- dat_standing %>%
  gather(key = team,
         value = rank,
         `House of Guards`:`Pippen Ain't Easy`) %>%
  dplyr::filter(!is.na(rank)) %>%
  set_names(tolower(names(.))) %>%
  mutate(week = fct_relevel(week, paste("Week", 1:(n()/12), sep = " ")))

# read the transation data
dat_trans <- gs_read(ss = mpr_fantasy, ws = "transaction") %>%
  arrange(moves) %>%
  mutate(team = factor(team, levels = team))

# # read and prepare the weekly result data
dat_result <- gs_read(ss = mpr_fantasy, ws = "detail") %>%
  select(team, week, win, loss, tie) %>%
  mutate(team = factor(team), # levels = team),
         margin = win - loss,
         final = ifelse(margin > 0, "1", "0"))

# read the weekly performance data, by category
dat_detail <- gs_read(ss = mpr_fantasy, ws = "detail")


# Current week performance ------------------------------------------------

var_list <- c("fg_pct", "ft_pct", "3ptm", "pts", "reb", "ast", "stl", "blk", "to")

# rank the stat for each category - high = bad, low = good
dat_this_week <- dat_detail %>%
  dplyr::filter(week == max(dat_detail$week[!is.na(dat_detail$win)])) %>%
  mutate_at(var_list, funs(rank = 13 - rank(.))) %>%
  mutate(to_rank = 13-to_rank) %>%
  select(team, fg_pct_rank, ft_pct_rank, `3ptm_rank`, pts_rank,
         reb_rank, ast_rank, stl_rank, blk_rank, to_rank)

# the weekly rank = sum of ranks for each category 
rank_this_week <- dat_this_week %>%
  gather(category, wk_rank, fg_pct_rank:to_rank) %>%
  group_by(team) %>%
  summarise(wk_rank = sum(wk_rank)) %>%
  ungroup %>%
  mutate(wk_rank = rank(wk_rank))



# Score to other teams ----------------------------------------------------

scoreCompr <- function(team_name){
  
  # the weekly performance of the reference team
  team_score <- dat_this_week %>% dplyr::filter(team == team_name) %>% 
    select(-team) %>% as.data.frame()
  
  dat_this_week_score <- dat_this_week %>% select(-team)
  
  dat_this_week_team <- dat_this_week %>% select(team)
  
  dat_this_week_score <- as.matrix(dat_this_week_score) - 
    matrix(rep(as.matrix(team_score), 12), nrow = 12, byrow = T)
  
  tmp <- dat_this_week_score

  tmp[dat_this_week_score > 0] <- "win"
  tmp[dat_this_week_score < 0] <- "lose"
  tmp[dat_this_week_score == 0] <- "tie"
  
  dat_this_week_score <- tmp %>% as.data.frame()

  dat_this_week_result <- bind_cols(dat_this_week_team, dat_this_week_score) %>%
    dplyr::filter(team != team_name) %>%
    mutate_at(vars(contains("rank")), funs(as.character)) %>%
    tidyr::gather(category, result, fg_pct_rank:to_rank) %>%
    group_by(team, result) %>%
    summarise(score = n()) %>%
    ungroup %>%
    tidyr::spread(result, score)

  dat_this_week_result[is.na(dat_this_week_result)] <- 0

  dat_this_week_result <- dat_this_week_result %>%
    mutate(result = case_when(
      lose - win > 0 ~ "loss",
      lose - win == 0 ~ "tie",
      lose - win < 0 ~ "win"))

  return(match_up_win = sum(dat_this_week_result$result == "win"))
}

# the trade network -------------------------------------------------------

team <- c("House of Guards",	"Flash",	"WizKids",	"Shaqtin' A Fool",	
          "The Butler Did It",	"Heinsohn and Cooz",	"Lauren's Legit Team",	
          "TRUST THE PROCESS",	"YouCantStopThaD",	"Damion's Team",
          "Derrick's Team",	"Pippen Ain't Easy")

gm <- c("Fei", "Sheng", "Matt", "Bryan",  "Beau", "Josh", "Lauren", 
        "Jeremy", "Devin", "Damion", "Derrick", "Scott")

dat_player_trade <- read_csv("..\\data\\dat_trade_after_process.csv") %>%
  group_by(id) %>%
  mutate(title = str_c("<p>", note[1], "<br>", note[2], "</p>", sep = ""),
         value = sum(n_players)/4)

edges_from <- dat_player_trade %>%
  dplyr::filter(offer == "from") %>%
  select(id, from = gm)

edges_to <- dat_player_trade %>%
  dplyr::filter(offer == "to") %>%
  select(id, to = gm, title, value)

edges <- full_join(edges_from, edges_to, by = "id") %>%
  ungroup() %>%
  select(-id)

nodes <- data.frame(id = gm, label = team,
                    shape = "circularImage",
                    image = str_c(gm, ".jpg", sep = ""))
