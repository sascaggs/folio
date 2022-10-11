#install.packages('blockmodeling', dependencies = T)
#library(blockmodeling)
library(igraph)
library(statnet)
library(tidygraph)
library(ggraph)
library(intergraph)

gtheme = theme(panel.background = element_rect(fill='white', color='white'), 
               plot.background = element_blank(), 
               title = element_text(hjust = 10))

ptheme = theme_classic() + theme(panel.background = element_rect(fill='white', color='white'), 
                                 plot.background = element_blank(), 
                                 title = element_text(hjust = 10))

source("~/Shane's Projects/visualization tools/color palettes.R")

N = 100
groups = 4

g = network(N, density = 0, directed = T)
g %v% 'group.no' = sample( 1:groups, size = N, replace = T)

s1 = simulate( g ~ edges + nodematch('group.no'), 
               coef = c(-7,5), 
               seed = 777)

as_tbl_graph(s1) %>%
    activate(nodes) %>%
    mutate(eigen = centrality_eigen()) %>%
    ggraph('fr') + 
    gtheme + 
    geom_edge_arc(strength = 0.05, alpha=0.5) + 
    geom_node_point(aes(fill = factor(group.no), size=eigen), 
                    shape = 21, show.legend = F) + 
    scale_fill_manual(values = distinct(6)[1:groups])


m1 = as.matrix(get.adjacency(asIgraph(s1)))
e1 = equiv.clust(s1, method = 'euclidean', mode = 'digraph')
b1 = blockmodel(s1, e1, h=4, mode = 'digraph')
plot(b1)
#c1 = optRandomParC(M = m1, k = 3, rep = 20, 
#                      blocks = c('com','rdo'), approaches = 'bin')
#s1 %v% 'cluster' = c1$best$best1$cl
s1 %v% 'block' = b1$block.membership 
as_tbl_graph(s1) %>%
    activate(nodes) %>%
    mutate(eigen = centrality_eigen()) %>%
    ggraph('fr') + 
    gtheme + 
    geom_edge_arc(strength = 0.05, alpha=0.5) + 
    geom_node_point(aes(fill = factor(block), size=eigen), 
                    shape = 21, show.legend = F) + 
    scale_fill_manual(values = distinct(10))
