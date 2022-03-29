library(tidyverse)

df <- poa %>% mutate(inc_neg = inc_neg == 1)
dems <- new_variables
var <- c("emp_change", "inc_neg")
item <- "inc_be_med_b"

make_plots <- function(df, dems, var, min = 5, title = "Percent Newly Unemployed") {
  #sym_var <- sym(var)
  out <- lapply(setNames(dems, dems), function(item){
    sym_item <- sym(item)
    reshaped <- df %>%
      group_by_at(vars(!!sym_item, var)) %>%
      count() %>%
      group_by(!!sym_item) %>% mutate(prop = round(n/sum(n), digits = 4), denom = sum(n)) %>%
      filter_if(is.logical, ~TRUE == .) %>%
      filter(!str_detect(!!sym_item, "prefer|;")) %>%
      ungroup %>% mutate(width = denom/sum(denom)) %>%
      mutate_if(labelled::is.labelled, labelled::to_character)
    
    filtered <- reshaped %>% filter(n >= min) #%>% select(-!!sym_var, -width)
    
    p.values <- c()
    cats <- filtered[[item]]
    i <- 1
    for(cat in cats) {
      for(cat2 in cats) {
        if(cat == cat2) {
          next
        }
        temp <- filtered[cats == cat,][c("n", "denom")] %>%
          rbind(filtered[cats == cat2,][c("n", "denom")])
        
        p.value <- prop.test(temp$n, temp$denom)$p.value
        
        names(p.value) <- glue::glue("{cat} & {cat2} p-value:")
        if(any(
          glue::glue("{cat2} & {cat} p-value:") %in% names(p.values),
          p.value > 0.1,
          is.na(p.value))) {
          next
        }
        p.values <- c(p.values, p.value)
        #print(glue::glue("{i}: {cat}, {cat2}"))
        #i <- i+1
      }
    }
    
    # return plots that have at least on statistically significant value
    if(is.null(p.values)) {
      return(NULL)
    } else {
      plot <- reshaped %>% filter(n >= min) %>%
        # plot
        ggplot(aes(x = prop, y = #stringr::str_to_title(labelled::to_character(!!sym_item))
                     reorder(stringr::str_to_title(labelled::to_character(!!sym_item)), prop))) +
        geom_col(fill = project_pal[4]) +
        
        # colors
        scale_x_continuous(labels = scales::percent) +
        scale_color_discrete(guide = "legend", name = item) + 
        project_theme + # question: add a variable for this?
        
        # labels
        xlab(NULL) + ylab(NULL) +
        ggtitle(glue::glue("{stringr::str_to_title(title)}\nby {stringr::str_to_title(item)}")) +
        labs(subtitle = paste(names(p.values), signif(p.values, 4), collapse = "\n")) +
        geom_text(aes(label = glue::glue("{scales::percent(prop)}\n{n}/{denom}")),
                  color = project_pal[1], hjust = 1.2) 
    }
    
    
  })
  return(out)
}
