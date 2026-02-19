
make_table <-function(x_var2, y_var2){
  full_model <- lm(get(y_var2) ~ get(x_var2), data = iris)
  full_corr <- cor(iris[[x_var2]], iris[[y_var2]])

  # table for all the species.
  t1<-tidy(full_model) %>%
    filter(term!="(Intercept)") %>%
    select(estimate, p.value) %>%
    mutate(Species = "All Species",
           corrs = full_corr) %>%
    select(Species, estimate, p.value, corrs) %>%
    as.data.frame()
  
  # table for individual species.
  t2 <- iris %>%
    group_by(Species) %>%
    nest() %>%
    mutate(
      models = map(data, ~lm(get(y_var2) ~ get(x_var2), data = .x)),
      tidy_info = map(models, tidy),
      corrs = map(data, ~cor(.x[[x_var2]], .x[[y_var2]]))
    ) %>%
    unnest(c(corrs, tidy_info)) %>%
    filter(term!="(Intercept)") %>%
    select(Species, estimate, p.value, corrs) %>%
    as.data.frame()
  
  rbind(t1,t2) %>%
    gt() %>%
    tab_header(
      title = "Exploring Simpson's Paradox",
      subtitle = paste0(
        sub("\\."," ",y_var2),
        " vs. ",
        sub("\\."," ", x_var2),
        ", Regression Statistics"
      )
    ) %>%
    fmt_number(columns = c("estimate","corrs"),
               decimals = 3) %>%
    fmt_scientific(column = "p.value",
                   decimals = 3) %>%
    opt_horizontal_padding(scale = 2) %>%
    tab_style(
      style = cell_fill(color='red',alpha = .3),
      locations = cells_body(
        columns = estimate,
        rows = estimate < 0
      )
    ) %>%
    tab_style(
      style = cell_fill(color='green',alpha = .3),
      locations = cells_body(
        columns = estimate,
        rows = estimate > 0
      )
    ) %>%
    cols_label(
      estimate = "Regression Slope",
      p.value = "P-value",
      corrs = "Correlation Coefficient"
    ) %>%
    cols_align(align = "center")
  
}


create_plot <- function(df, show_color){
  if (show_color){
    ggplot(data = df,
           aes(x = Sepal.Width, y = Petal.Width,
               color = Species))+
      geom_point() + 
      scale_color_brewer(palette = "Dark2")+
      labs(x = "Sepal Width (in)",
           y = "Petal Width (in)")+
      scale_y_continuous(limits = c(0,2.6))+
      scale_x_continuous(limits = c(2, 4.5))+
      theme_bw()
  } else {
    ggplot(data = df,
           aes(x = Sepal.Width, y = Petal.Width))+
      geom_point() + 
      labs(x = "Sepal Width (in)",
           y = "Petal Width (in)")+
      scale_y_continuous(limits = c(0,2.6))+
      scale_x_continuous(limits = c(2, 4.5))+
      theme_bw()
  }
  
}

create_table <- function(df){
  df %>%
    group_by(Species) %>%
    summarise(mean_petal = mean(Petal.Width),
              mean_sepal = mean(Sepal.Width)) %>%
    knitr::kable(
      "html",
      col.names = c("Species", "Mean Petal Width", "Mean Sepal Width"),
      align = c("r","c","c")
    ) %>%
    kable_styling(full_width = F,
                  bootstrap_options = c("hover","bordered"))
}


