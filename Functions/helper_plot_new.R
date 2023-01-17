plot_function_new <- function(data,conf = "Include", slope = "Include", covid = "Include",season_start=1,season_end=12){
  
  data %>%
    ggplot(aes(x=season, y=mean, group=dv, colour=dv)) +
    geom_line() +
    geom_point(size=1,aes(colour=dv)) +
    geom_hline(yintercept=0, linetype="dashed", color = "black") +
    scale_x_continuous(name = "Seasons from 2010/2011 (1) to 2021/2022 (12)", breaks=c(seq(as.numeric(season_start):(as.numeric(season_end)+2))),limits = c(as.numeric(season_start),as.numeric(season_end)))+
    labs(y = "Dependent Var.") +
    theme_minimal() + 
    theme(legend.title=element_blank()) +
    {if(covid == "Include")geom_vline(xintercept = 10.25, linetype=6, color = "red")} +
    {if(covid == "Include")geom_vline(xintercept = 11.75, linetype=6,color = "red")} +
    {if(covid == "Include")annotate("text", x = 11, y = 2.5, label = "Covid-19", color="red",parse = TRUE)} +
    {if(conf == "Include")geom_ribbon(aes(ymin=CI_low, ymax=CI_hi,colour = dv), fill = "grey20", alpha = 0.1)} +
    {if(slope == "Include")geom_smooth(method = "lm", se = FALSE)}
}

