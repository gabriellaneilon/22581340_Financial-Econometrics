Roll_optimizer <- function(return_mat, Rebalance_Quarterly, LookBackSel = 36){

    return_df_used <- return_mat %>% filter(date >= Rebalance_Quarterly%m-% months(LookBackSel))

    if(return_df_used %>% nrow() < LookBackSel) return(NULL) # PRO TIP - return NULL effectively skips the iteration when binding....

    return_mat_Nodate <- data.matrix(return_df_used[, -1])
    # Simple Sample covariance and mean for the lookback period:
    Sigma <- RiskPortfolios::covEstimation(return_mat_Nodate)
    Mu <- return_mat %>% summarise(across(-date, ~prod(1+.)^(1/n())-1)) %>% purrr::as_vector()


    My_Weights <-
        left_join(
            optim_foo(Type = "mv", mu, Sigma, LB, UB, printmsg = F),
            optim_foo(Type = "minvol", mu, Sigma, LB, UB, printmsg = F),
            by = c("Tickers")) %>%
        left_join(.,optim_foo(Type = "erc", mu, Sigma, LB, UB, printmsg = F),by = c("Tickers")) %>%
        left_join(.,optim_foo(Type = "riskeff", mu, Sigma, LB, UB, printmsg = F),by = c("Tickers")) %>%

        mutate(date = Rebalance_Quarterly , Look_Back_Period = LookBackSel)

}