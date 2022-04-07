library(tidyverse)
library(crayon)


bachelor <- read_csv('Bachelor1.csv')
bach_error <- read_csv('BachelorError.csv')
hs <- read_csv('HighSchool2.csv')
hs_error <- read_csv('HighSchoolError.csv')

df_bachelor <- data.frame(t(bachelor[2:ncol(bachelor)]))
colnames(df_bachelor) <- bachelor$Race
df_bachelor$State <- rownames(df_bachelor)
df_bachelor <- remove_rownames(df_bachelor)

df_bach_adj <- df_bachelor %>% transmute(state_name = State,
                                         Total = Total - Total[1],
                                         White = White - White[1],
                                         Black = Black - Black[1],
                                         Hispanic = Hispanic - Hispanic[1],
                                         Asian = Asian - Asian[1],
                                         `Two or More Races` = `Two or More Races` - `Two or More Races`[1])


df_hs <- data.frame(t(hs[2:ncol(hs)]))
colnames(df_hs) <- hs$Race
df_hs$State <- rownames(df_hs)
df_hs <- remove_rownames(df_hs)

df_hs_adj <- df_hs %>% transmute(state_name = State,
                                         Total = Total - Total[1],
                                         White = White - White[1],
                                         Black = Black - Black[1],
                                         Hispanic = Hispanic - Hispanic[1],
                                         Asian = Asian - Asian[1],
                                         `Two or More Races` = `Two or More Races` - `Two or More Races`[1])


us <- USAboundaries::us_states()

write.csv(df_bach_adj,"bach_adj.csv")
write.csv(df_hs_adj,"hs_adj.csv")

