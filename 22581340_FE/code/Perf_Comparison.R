Perf_comparisons <- function(Idxs, Ys, Alias){

    Unconditional_SD <-

        Idxs %>%

        group_by(Index) %>%

        mutate(Full_SD = sd(Return) * sqrt(12)) %>%

        filter(Year %in% Ys) %>%

        summarise(SD = sd(Return) * sqrt(12), across(.cols = starts_with("Full"), .fns = max)) %>%

        arrange(desc(SD)) %>% mutate(Period = Alias) %>%

        group_by(Index) %>%

        mutate(Ratio = SD / Full_SD)

    Unconditional_SD

}