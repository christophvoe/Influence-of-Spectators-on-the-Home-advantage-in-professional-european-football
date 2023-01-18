plot_function_manus <- function(data,conf = "Include", slope = "Include", covid = "Include",season_start=1,season_end=12, title="Dependent Variable", type ="ext",legend = "Exclude"){
  
  data %>%
    ggplot(aes(x=season, y=mean, group=dv, colour=dv)) +
    geom_line(color="black") +
    geom_point(size=2,color="red") +
    geom_hline(yintercept=0, linetype="dashed", color = "black") +
    labs(y = as.character(title)) +
    theme_minimal() + 
    theme(legend.title=element_blank()) +
    {if(type == "rep")scale_x_continuous(name = "2010/2011 (1) to 2019/2020 (10)", breaks=c(seq(as.numeric(season_start):(as.numeric(season_end)+2))),limits = c(as.numeric(season_start),as.numeric(season_end)))} +
    {if(type == "ext")scale_x_continuous(name = "2010/2011 (1) to 2021/2022 (12)", breaks=c(seq(as.numeric(season_start):(as.numeric(season_end)+2))),limits = c(as.numeric(season_start),as.numeric(season_end)))} +
    {if(type == "explor")scale_x_continuous(name = "2010/2011 (1) to 2020/2021 (11)", breaks=c(seq(as.numeric(season_start):(as.numeric(season_end)+2))),limits = c(as.numeric(season_start),as.numeric(season_end)))} +
    {if(legend == "Exclude")theme(legend.position = "none")} +
    {if(covid == "Include")geom_vline(xintercept = 10.25, linetype=6, color = "red")} +
    {if(covid == "Include")geom_vline(xintercept = 11.75, linetype=6,color = "red")} +
    {if(covid == "Include")annotate("text", x = 11, y = 2.5, label = "Covid-19", color="red",parse = TRUE)} +
    {if(conf == "Include")geom_ribbon(aes(ymin=CI_low, ymax=CI_hi,colour = dv), fill = "grey20", alpha = 0.1)} +
    {if(slope == "Include")geom_smooth(method = "lm", se = FALSE)}
}