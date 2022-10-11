
rm(list=ls())

library(tidyverse)
library(tidygraph)
library(igraph)
library(ggraph)
library(patchwork)

gtheme = theme(panel.background = element_rect(fill='white', color='white'), 
               plot.background = element_blank(), 
               title = element_text(hjust = 10))

ptheme = theme_classic() + theme(panel.background = element_rect(fill='white', color='white'), 
                                 plot.background = element_blank(), 
                                 title = element_text(hjust = 10))

source("~/Shane's Projects/visualization tools/color palettes.R")

inv_logit = function(x) exp(x)/(1+exp(x))

#N = 40
#p_basal = 0.6
#e = expand.grid(1:N, 1:N)
#e$basal = e$Var1 <= (N * p_basal)
#e$did = 1:nrow(e)
#
#
## create vertex attributes 
#set.seed(777)
#
#reps = 9
#V = list()
#G = list()
#
#for(i in 1:reps) {
#    v = tibble(
#        name = 1:N, 
#        mass = rnorm(N, mean = 10, sd = i)
#    ) 
#    V[[i]] <- v
#}
#
#ggplot(V[[5]]) + geom_density(aes(mass))
#
#for(n in 1:reps) {
#    
#    # create edgelist with body mass values
#    e0 = merge(e, V[[n]], by.x = 'Var1', by.y = 'name')
#    e1 = merge(e0, V[[n]], by.x = 'Var2', by.y = 'name')
#    
#    # calculate 
#    edgelist = suppressMessages(left_join(e0, e1)) %>% 
#        select(Var1, Var2, did, basal, mass.x, mass.y) %>%
#        arrange(did) %>%
#        mutate(diff = mass.x - mass.y, 
#               diff_prob = inv_logit(diff), 
#               ij = rbinom(nrow(e), size=1, prob = diff_prob)) %>%
#        filter(ij == 1)
#    
#    # make graphs 
#    G[[n]] = graph.data.frame(edgelist, vertices = V[[n]])
#    
#}
#
#lapply(G, average.path.length)

# Writing the function


fwfun = function(N = 40, 
                 p_basal = 0.6, 
                 mean = 0, 
                 sd_seq = 1:3,
                 seed = 777 ) {
    
    # setup
    #set.seed(seed)
    V = list()
    G = list()
    
    # create grid and assign basal species
    e0 = expand.grid(1:N, 1:N)
    e0$basal = e0$Var1 <= (N * p_basal)
    e0$did = 1:nrow(e0)
    
    # create vertex attributes
    # WARNINGS 
    if(any(sd_seq <= 0) == T) { 
        print('Standard Deviations cannot be negative or zero.') } 
    else if(length(sd_seq) < 1) { 
        print("Vector of standard deviations (i.e., sd_seq) cannot be zero.") } 
    
    else { 
        for(i in seq_along(sd_seq)) {
            v = tibble(
                name = 1:N, 
                mass = rnorm(N, mean = mean, sd = i), 
                basal = name <= (N * p_basal)
            ) 
            V[[i]] <- v
        }
    }
    
    for(k in seq_along(sd_seq)) {
        
        # create edgelist with body mass values
        e1 = merge(e0, V[[k]][,1:2], by.x = 'Var1', by.y = 'name')
        e2 = merge(e1, V[[k]][,1:2], by.x = 'Var2', by.y = 'name')
        
        # calculate mass 
        edgelist = suppressMessages(left_join(e1, e2)) %>% 
            select(Var1, Var2, did, basal, mass.x, mass.y) %>%
            arrange(did) %>%
            mutate(diff = mass.x - mass.y, 
                   diff_prob = inv_logit(diff), 
                   ij = rbinom(n(), size=1, prob = diff_prob), 
                   ij = ifelse(basal == T, 0, ij)) %>%
            filter(ij == 1)
        
        # make graphs 
        G[[k]] = graph.data.frame( edgelist, vertices = V[[k]] ) 
        
    }
    return(G)
}



#seq_along(rep(1,100))



el = fwfun(sd_seq = c(0.01,0.1,0.5,1,1.5), mean=0, N=100)

library(sna)
library(intergraph)
lapply( lapply(el, asNetwork), hierarchy, measure = 'krackhardt')
lapply(el, average.path.length)


cols = carnival(4)

el[[1]] %>% as_tbl_graph() %>%
    activate(edges) %>%
    data.frame() %>% 
    ggplot(aes(diff_prob)) + 
    ptheme + 
    geom_density(fill=cols[1], color=cols[1], alpha=0.3, lwd=1)

el[[2]] %>% as_tbl_graph() %>%
    activate(edges) %>%
    data.frame() %>% 
    ggplot(aes(diff_prob)) + 
    ptheme + 
    geom_density(fill=cols[2], color=cols[2], alpha=0.3, lwd=1)

el[[3]] %>% as_tbl_graph() %>%
    activate(edges) %>%
    data.frame() %>% 
    ggplot(aes(diff_prob)) + 
    ptheme + 
    geom_density(fill=cols[3], color=cols[3], alpha=0.3, lwd=1)

el[[4]] %>% as_tbl_graph() %>%
    activate(edges) %>%
    data.frame() %>% 
    ggplot(aes(diff_prob)) + 
    ptheme + 
    geom_density(fill=cols[4], color=cols[4], alpha=0.3, lwd=1)


el[[1]] %>%
    as_tbl_graph() %>%
    ggraph('sugiyama') + 
    gtheme + 
    geom_edge_link() + 
    geom_node_point(aes(color=basal))





el[[1]]


plot(graph.strength(el[[1]], mode = 'in'), V(el[[1]])$mass, col=factor(V(el[[1]])$basal))
plot(graph.strength(el[[1]], mode = 'out'), V(el[[1]])$mass, col=factor(V(el[[1]])$basal))



matrix(unlist(lapply(el, graph.strength, mode = 'in')), ncol=length(el))
matrix(unlist(lapply(el, graph.strength, mode = 'out')), ncol=length(el))


e = e1 %>% 
    select(Var1, Var2, did, basal, mass.x, mass.y) %>% 
    arrange(did) %>% 
    mutate(diff = mass.x - mass.y, 
           diff_prob = inv_logit(diff))

e$ij = rbinom(nrow(e), size=1, prob = e$diff_prob)
e[ e$basal == T, 'ij' ] <- 0


g = graph.data.frame(e[ e$ij == 1, ], vertices = v)
V(g)$basal = ifelse(as.numeric(V(g)$name) <= N*p_basal, 'basal','consumer')

# graph ----
#as_tbl_graph(g) %>%
#    activate(nodes) %>%
#    mutate(degree = centrality_degree(mode = 'in')) %>%
#    ggraph('sugiyama') + gtheme + 
#    geom_edge_link(arrow = arrow(length = unit(1.5, 'mm'), type='closed'), 
#                   start_cap = circle(3, 'mm'),
#                   end_cap = circle(3, 'mm'), 
#                   width=0.2, 
#                   alpha=0.4) +
#    geom_edge_loop(arrow = arrow(length = unit(1.5, 'mm'), type='closed'), 
#                   start_cap = circle(3, 'mm'),
#                   end_cap = circle(3, 'mm'), 
#                   width=1, 
#                   alpha=1,
#                   aes(strength=1.4)) + 
#    geom_node_point(aes(color = basal), size=3) + 
#    scale_color_manual(values = carnival(2)[2:1]) + 
#    labs(color=NULL)

# path length ----

d = as_tbl_graph(g) %>%
    activate(nodes) %>%
    mutate(vulnerability = centrality_degree(mode = 'in'), 
           generality    = centrality_degree(mode = 'out')) %>%
    data.frame()

sd_vulnerability = sd(d$vulnerability)
sd_generality = sd(d[ !d$basal == 'basal', 'generality'])

d %>%
    select(vulnerability, generality) %>%
    gather(key=key, value=value) %>%
    ggplot() + 
    theme_classic() + 
    geom_density(aes(value, color=key, fill=key), alpha=0.3, lwd=2) + 
    scale_color_manual(values = wildberry(2)) + 
    scale_fill_manual(values = wildberry(2)) + 
    geom_text(aes(17,0.2, label=paste0('sd vulnerability = ', round(sd_vulnerability, 2))), color=wildberry(10)[9]) + 
    geom_text(aes(17,0.225, label=paste0('sd generality = ', round(sd_generality, 2))), color=wildberry(10)[2]) + 
    #scale_x_continuous(limits = c(0,20)) + 
    labs(color="", fill="", x='', y='density') + 
    theme(legend.position = c(0.82,0.8))

average.path.length(g)



# simulate multiple networks 
N = 40
p_basal = 0.6
e = expand.grid(1:N, 1:N)
e$basal = e$Var1 <= (N * p_basal)
e$did = 1:nrow(e)

v = tibble(
    name = 1:N, 
    mass = rnorm(N, mean = 0, sd = 5)
) 



ggplot(v) + geom_density(aes(mass))


e0 = merge(e, v, by.x = 'Var1', by.y = 'name')
e1 = merge(e0, v, by.x = 'Var2', by.y = 'name')
e = e1 %>% 
    select(Var1, Var2, did, basal, mass.x, mass.y) %>% 
    arrange(did) %>% 
    mutate(diff = mass.x - mass.y, 
           diff_prob = inv_logit(diff))

e$ij = rbinom(nrow(e), size=1, prob = e$diff_prob)
e[ e$basal == T, 'ij' ] <- 0