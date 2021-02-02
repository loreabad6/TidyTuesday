Tidy Tuesday 2021
================
Lorena Abad

# Tidy Tuesday week 5

For this week, I present an step by step of my submission, going through
data preparation and visualization. In particular, I used a custom
`ggraph` layout based on geographic coordinate reference systems to
combine `sfnetwork` objects with spatially implicit edges (without
spatial geometries) from the
[`sfnetworks`](https://luukvdmeer.github.io/sfnetworks/) package with
other `ggplot2` functions.

## Libraries

Let’s start with loading the libraries we will use:

``` r
library(tidytuesdayR)
library(dplyr, quietly = T)
library(tidygeocoder)
library(sf, quietly = T)
library(rnaturalearth)
library(tidygraph, quietly = T)
# remotes::install_github("luukvdmeer/sfnetworks")
library(sfnetworks)
library(ggplot2)
library(ggraph)
library(stringi)
library(ggtext)
```

## Data

The data for [this
week](https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-01-26/readme.md)
is from the [“Break Free from
Plastic”](https://www.breakfreefromplastic.org/) initiative.

In the data, information regarding plastic recollection campaigns in
several countries around the world from 2019 and 2020 is categorized by
plastic type and by parent company which counts how many times a plastic
item coming from that particular company was collected. We can take a
look at the data structure and main variables here:

    ## 
    ##  Downloading file 1 of 1: `plastics.csv`

``` r
tuesdata$plastics %>% glimpse()
```

    ## Rows: 13,380
    ## Columns: 14
    ## $ country        <chr> "Argentina", "Argentina", "Argentina", "Argentina", ...
    ## $ year           <dbl> 2019, 2019, 2019, 2019, 2019, 2019, 2019, 2019, 2019...
    ## $ parent_company <chr> "Grand Total", "Unbranded", "The Coca-Cola Company",...
    ## $ empty          <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0...
    ## $ hdpe           <dbl> 215, 155, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
    ## $ ldpe           <dbl> 55, 50, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,...
    ## $ o              <dbl> 607, 532, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 13, 0, 0,...
    ## $ pet            <dbl> 1376, 848, 222, 39, 38, 22, 21, 26, 19, 14, 14, 14, ...
    ## $ pp             <dbl> 281, 122, 35, 4, 0, 7, 6, 0, 1, 4, 3, 1, 0, 0, 3, 0,...
    ## $ ps             <dbl> 116, 114, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
    ## $ pvc            <dbl> 18, 17, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,...
    ## $ grand_total    <dbl> 2668, 1838, 257, 43, 38, 29, 27, 26, 20, 18, 17, 15,...
    ## $ num_events     <dbl> 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4...
    ## $ volunteers     <dbl> 243, 243, 243, 243, 243, 243, 243, 243, 243, 243, 24...

## Idea and data wrangling

An interesting opportunity to create a spatial graph structure from this
data is to generate a connected network between those countries where
plastics have been collected and the parent companies headquarters. To
avoid making a cluttered graph, we can analyze the **Top 5** polluting
parent companies.

### Preparing the data

First, we tidy the entries for parent companies, removing `Unbranded`,
`null`s and the `Grand Total`s per year. Also for countries, we remove
the `EMPTY` fields and harmonize country names with capitalized letters.

``` r
plastics = tuesdata$plastics %>% 
  filter(
    !(parent_company %in% c("Grand Total", "null", "Null", "Unbranded")), 
    country != "EMPTY"
  ) %>% 
  mutate(
    parent_company = parent_company %>% 
      tolower() %>% 
      stri_trans_general("Latin-ASCII") %>% 
      stri_trans_totitle(),
    country = stri_trans_totitle(country)
  ) 
```

Now we can query which were the top five polluters for the 2019 and 2020
campaigns.

``` r
top_Co = plastics %>% 
  group_by(parent_company) %>% 
  summarise(
    country_count = n_distinct(country), 
    grand_total_sum = sum(grand_total, na.rm = T)
  ) %>% 
  arrange(desc(country_count, grand_total_sum)) %>% 
  head(5)

knitr::kable(top_Co)
```

| parent\_company       | country\_count | grand\_total\_sum |
|:----------------------|---------------:|------------------:|
| The Coca-Cola Company |             58 |             23823 |
| Pepsico               |             49 |              7457 |
| Nestle                |             44 |             12622 |
| Mars, Incorporated    |             42 |              1022 |
| Unilever              |             41 |              8401 |

These five companies will become our `from` nodes in the network
structure we will create later.

To create our `to` nodes, we will look at those countries that were
affected with the plastic pollution originally coming from these
companies.

``` r
countries = plastics %>% 
  filter(parent_company %in% top_Co$parent_company) %>% 
  group_by(country) %>% 
  summarise(no_of_brands = n()) %>% 
  arrange(desc(no_of_brands))

## Let's have a glimpse at it!
knitr::kable(head(countries, 5))
```

| country     | no\_of\_brands |
|:------------|---------------:|
| Philippines |             11 |
| Argentina   |             10 |
| India       |             10 |
| Mexico      |             10 |
| Switzerland |             10 |

**BUT**, we want to create a spatial network, so we need to locate these
nodes in space!

### Making it spatial!

#### From nodes

Let’s start with our `from` nodes. I could not really find a good way to
query automatically the headquarter locations of these companies
(suggestions welcome!), so this was a manual process. I looked into this
[crunchbase](https://www.crunchbase.com/lists/companies-search-with-headquarters)
and searched for each parent company in my `top_Co` list. I copied the
addresses below:

``` r
hq = c(
  "Atlanta, Georgia, United States", #Coca-Cola
  "New York, New York, United States", #PepsiCo
  "Vevey, Vaud, Switzerland", #Nestle
  "Mclean, Virginia, United States", #Mars
  "London, England, United Kingdom" #Unilever
)
```

Now, we can make use of some nice geocoding packages to get a point
location for these addresses. I have chosen to use `tidygeocoder` for
this case. I query from OSM and get a nice `data.frame` with the
address, the latitude and the longitude values.

``` r
coords = geo(hq, method = "osm")
```

``` r
knitr::kable(coords)
```

| address                           |      lat |        long |
|:----------------------------------|---------:|------------:|
| Atlanta, Georgia, United States   | 33.74899 | -84.3902644 |
| New York, New York, United States | 40.71273 | -74.0060152 |
| Vevey, Vaud, Switzerland          | 46.46030 |   6.8418655 |
| Mclean, Virginia, United States   | 38.93429 | -77.1776327 |
| London, England, United Kingdom   | 51.50732 |  -0.1276474 |

I can now combine this `data.frame` with the `top_Co` object I created
before. I will add the longitude and latitude columns to my data, and
convert into an `sf` object with these coordinates. Note that the
results from the geocoding process are in `CRS = 4326`. We will add a
type column to clearly identify this as “Parent Company” nodes.

``` r
top_parent_companies = top_Co %>% 
  mutate(hq = hq, lat = coords$lat, long = coords$long) %>% 
  st_as_sf(crs = 4326, coords = c("long", "lat")) %>% 
  select(name = parent_company) %>% 
  mutate(type = "Parent Company")

top_parent_companies
```

    ## Simple feature collection with 5 features and 2 fields
    ## geometry type:  POINT
    ## dimension:      XY
    ## bbox:           xmin: -84.39026 ymin: 33.74899 xmax: 6.841866 ymax: 51.50732
    ## geographic CRS: WGS 84
    ## # A tibble: 5 x 3
    ##   name                               geometry type          
    ## * <chr>                           <POINT [°]> <chr>         
    ## 1 The Coca-Cola Company  (-84.39026 33.74899) Parent Company
    ## 2 Pepsico                (-74.00602 40.71273) Parent Company
    ## 3 Nestle                   (6.841865 46.4603) Parent Company
    ## 4 Mars, Incorporated     (-77.17763 38.93429) Parent Company
    ## 5 Unilever              (-0.1276474 51.50732) Parent Company

#### To nodes

Now let’s take a look at our `to` nodes. This will be made out of those
countries where the plastics were found. We can find their coordinates
(probably their centroids, depends on OSM) using the
`tidygeocoder::geo()` function.

``` r
coords_countries = geo(countries$country, method = "osm")
# Get Taiwan coordinates, which was not recognized
coords_taiwan = geo("Taiwan", method = "osm")
coords_country = coords_countries %>% 
  mutate(
    lat = ifelse(address == "Taiwan_ Republic Of China (Roc)", coords_taiwan$lat, lat),
    long = ifelse(address == "Taiwan_ Republic Of China (Roc)", coords_taiwan$long, long)
) 
```

And last but not least, we convert it to an `sf` object, giving it a
type “Affected Country” for differentiation.

``` r
affected_countries = countries %>% 
  left_join(coords_country, by = c("country" = "address")) %>% 
  st_as_sf(crs = 4326, coords = c("long", "lat")) %>% 
  select(name = country) %>% 
  mutate(type = "Affected Country")

affected_countries
```

    ## Simple feature collection with 59 features and 2 fields
    ## geometry type:  POINT
    ## dimension:      XY
    ## bbox:           xmin: -107.9917 ymin: -34.9965 xmax: 139.2394 ymax: 61.06669
    ## geographic CRS: WGS 84
    ## # A tibble: 59 x 3
    ##    name                                 geometry type            
    ##  * <chr>                             <POINT [°]> <chr>           
    ##  1 Philippines               (122.7312 12.75035) Affected Country
    ##  2 Argentina                (-64.96728 -34.9965) Affected Country
    ##  3 India                     (78.66774 22.35111) Affected Country
    ##  4 Mexico                        (-100 22.50005) Affected Country
    ##  5 Switzerland               (8.231974 46.79856) Affected Country
    ##  6 United States Of America (-100.4459 39.78373) Affected Country
    ##  7 Brazil                      (-53.2 -10.33333) Affected Country
    ##  8 Ecuador                  (-79.3667 -1.339767) Affected Country
    ##  9 Indonesia                (117.8903 -2.483383) Affected Country
    ## 10 Latvia                    (24.75376 56.84065) Affected Country
    ## # ... with 49 more rows

### Preparing the graph structure

So maybe you already picked it up but to make it clear: a graph
structure requires two elements “nodes” and “edges”. Nodes are the
interacting elements and the edges are how these elements are connected
to each other.

We can now create our node list with the countries and parent companies
coordinates. For this we will bind both datasets together, since they
have the same column names!

``` r
nodes = rbind(top_parent_companies, affected_countries)
```

To create our edges, we will go back to our original plastics data. We
filter the parent companies and the countries according to our previous
analyses. To make a proper edge table, we will rename the
`parent_company` column as `from` and the `country` column as `to`.

``` r
edges = plastics %>% 
  filter(
    parent_company %in% top_Co$parent_company,
    country %in% countries$country
  ) %>% 
  select(from = parent_company, to = country, everything()) 
```

And finally, we can build our spatial network using the sfnetworks
package. We pass the nodes and the edges to the building function
`sfnetwork()`. You will see now that our nodes and edges are combined
into one single `sfnetwork` object.

``` r
net = sfnetwork(nodes, edges)
net
```

    ## # A sfnetwork with 64 nodes and 342 edges
    ## #
    ## # CRS:  EPSG:4326 
    ## #
    ## # A directed acyclic multigraph with 1 component with spatially implicit edges
    ## #
    ## # Node Data:     64 x 3 (active)
    ## # Geometry type: POINT
    ## # Dimension:     XY
    ## # Bounding box:  xmin: -107.9917 ymin: -34.9965 xmax: 139.2394 ymax: 61.06669
    ##   name                               geometry type            
    ##   <chr>                           <POINT [°]> <chr>           
    ## 1 The Coca-Cola Company  (-84.39026 33.74899) Parent Company  
    ## 2 Pepsico                (-74.00602 40.71273) Parent Company  
    ## 3 Nestle                   (6.841865 46.4603) Parent Company  
    ## 4 Mars, Incorporated     (-77.17763 38.93429) Parent Company  
    ## 5 Unilever              (-0.1276474 51.50732) Parent Company  
    ## 6 Philippines             (122.7312 12.75035) Affected Country
    ## # ... with 58 more rows
    ## #
    ## # Edge Data: 342 x 14
    ##    from    to  year empty  hdpe  ldpe     o   pet    pp    ps   pvc grand_total
    ##   <int> <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>       <dbl>
    ## 1     1     7  2019     0     0     0     0   222    35     0     0         257
    ## 2     2     7  2019     0     0     0     0    21     6     0     0          27
    ## 3     3     7  2019     0     1     0     0     6     2     0     0           9
    ## # ... with 339 more rows, and 2 more variables: num_events <dbl>,
    ## #   volunteers <dbl>

### Background data and final wrangling

We will now get some background data, which in this case will be the
world countries, obtained with the package `rnaturalearth`. We will
transform the projection to one that does not distort so much the
country sizes. I have chosen Winkel Tripel.

``` r
world = ne_countries(scale = "medium", returnclass = "sf") %>% 
  st_transform(crs = "+proj=wintri +datum=WGS84 +no_defs +over")
```

We will need to project our network as well, which can be done with the
same function as above.

``` r
net = net %>% 
  st_transform(crs = "+proj=wintri +datum=WGS84 +no_defs +over")
```

#### Network simplification

One interesting thing to note is that we have repeated edges going from
the same parent company to the same country. Here we can filter the
network to show this. From our process to construct the network, we know
that the first 5 nodes are for the parent companies, and the rest for
affected countries, so we can filter as follows:

``` r
net %>% 
  activate("edges") %>% 
  filter(edge_is_between(1, 6))
```

    ## # A sfnetwork with 64 nodes and 2 edges
    ## #
    ## # CRS:  +proj=wintri +datum=WGS84 +no_defs +over 
    ## #
    ## # A directed acyclic multigraph with 63 components with spatially implicit edges
    ## #
    ## # Edge Data: 2 x 14 (active)
    ##    from    to  year empty  hdpe  ldpe     o   pet    pp    ps   pvc grand_total
    ##   <int> <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>       <dbl>
    ## 1     1     6  2019     0   710    34   130  1793    94    43     0        2804
    ## 2     1     6  2020     0     0     0    26   545    43     0     0         614
    ## # ... with 2 more variables: num_events <dbl>, volunteers <dbl>
    ## #
    ## # Node Data:     64 x 3
    ## # Geometry type: POINT
    ## # Dimension:     XY
    ## # Bounding box:  xmin: -10811490 ymin: -3992144 xmax: 14426080 ymax: 7132385
    ##   name                            geometry type          
    ##   <chr>                        <POINT [m]> <chr>         
    ## 1 The Coca-Cola Company (-8798994 3918686) Parent Company
    ## 2 Pepsico               (-7484496 4671906) Parent Company
    ## 3 Nestle                (674228.5 5173176) Parent Company
    ## # ... with 61 more rows

This repetition is because we have data for two years. We can summarize
the data to get a grand total using a [“spatial
morpher”](https://luukvdmeer.github.io/sfnetworks/articles/morphers.html)
that will simplify these redundant nodes.

``` r
net = net %>% 
  convert(
    to_spatial_simple, .clean = T, 
    ## This will summarize our attributes by summing them up.
    ## It will ignore the year column from our final output.
    summarise_attributes = list(
      function(x) sum(x, na.rm = T), 
      year = "ignore"
    )
  )

net %>% 
  activate("edges") %>% 
  filter(edge_is_between(1, 6))
```

    ## # A sfnetwork with 64 nodes and 1 edges
    ## #
    ## # CRS:  +proj=wintri +datum=WGS84 +no_defs +over 
    ## #
    ## # A rooted forest with 63 trees with spatially implicit edges
    ## #
    ## # Edge Data: 1 x 13 (active)
    ##    from    to empty  hdpe  ldpe     o   pet    pp    ps   pvc grand_total
    ##   <int> <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>       <dbl>
    ## 1     1     6     0   710    34   156  2338   137    43     0        3418
    ## # ... with 2 more variables: num_events <dbl>, volunteers <dbl>
    ## #
    ## # Node Data:     64 x 3
    ## # Geometry type: POINT
    ## # Dimension:     XY
    ## # Bounding box:  xmin: -10811490 ymin: -3992144 xmax: 14426080 ymax: 7132385
    ##   name                            geometry type          
    ##   <chr>                        <POINT [m]> <chr>         
    ## 1 The Coca-Cola Company (-8798994 3918686) Parent Company
    ## 2 Pepsico               (-7484496 4671906) Parent Company
    ## 3 Nestle                (674228.5 5173176) Parent Company
    ## # ... with 61 more rows

And now we are ready to create our data visualization!

## Visualizing!

First things first, let’s load some fonts (yes, I am a windows user! 😐).

``` r
extrafont::loadfonts(device = 'win')
```

Now we can prepare the title, subtitle, caption and a short description
to be included in the plot. Colors in the description correspond to the
points used in the final map. This assignment was done manually. The
text is wrapped around `<span>` tags to pass them to `ggtext` functions
later.

``` r
## Check the .Rmd file for a correct way to prepare this description.
title = "From **multinationals** to **wasteland countries**"
subtitle = "Insights from the 
            <span style='color:#3a8c9e'>
            #break<span style='color:#85cbda'>
            free</span>fromplastic
            </span> initiative" 
caption = "Data: *Break Free from Plastic* courtesy of Sarah Sauve. 
          Visualization: @loreabad6"
description = paste(
  "Plastics from the **top 5** polluting companies: 
  <span style='color:#1B9E77'>Coca-Cola</span>, 
  <span style='color:#E7298A'>Pepsico</span>, 
  <span style='color:#7570B3'>Nestlé</span>, 
  <span style='color:#D95F02'>Mars, Inc.</span> 
  and <span style='color:#66A61E'>Unilever</span>, 
  have been found in", nrow(affected_countries), 
  "different countries between 2019 and 2020. 
  <br>The lines connect the parent companies' 
  headquarters to the countries where their 
  plastics were found. Thicker lines represent 
  higher plastic counts."
  )
```

### Integrating sfnetworks with ggraph

Since sfnetworks subclasses `tbl_graph` objects, we can easily pass them
to a ggraph structure. But, to let ggraph know where to place the nodes,
we can create a layout function that will extract the graph coordinates.

``` r
layout_sf = function(graph){
  # Extract X and Y coordinates from the nodes
  graph = activate(graph, "nodes")
  x = sf::st_coordinates(graph)[,"X"]
  y = sf::st_coordinates(graph)[,"Y"]
  data.frame(x, y)
}
```

### Let’s plot already!

And now we can finally work on our visualization! See the comments
between each line for explanations of what is going on…

``` r
# Start our ggraph, with a layout that calls our layout_sf() function
g = ggraph(net, layout = layout_sf) +
  # Plot background with geom_sf
  geom_sf(data = world, fill = "grey30", color = NA) +
  # Create arcs between edges using ggraph
  geom_edge_arc(
    # Alpha and width change according to the plastic grand total count
    aes(alpha = grand_total, width = grand_total),
    # The strength corresponds to how curved the arc is
    color = "white", strength = 0.7, show.legend = F
  ) +
  # Create node geoms using ggraph, in this case points
  geom_node_point(
    # Remember the types we assigned during creation,
    # They will help us filter which nodes we want to 
    # represent with a point and which not.
    aes(
      fill = ifelse(type == "Parent Company", name,  NA_character_),
      shape = type,
    ),
    # I will use pch 21 below, so the stroke determines 
    # the bound to use
    size = 2.5, color = "white", stroke = 1, show.legend = F
  ) +
  # Assign pch 21 only to parent companies
  scale_shape_manual(values = c(NA, 21)) +
  # Give colors to each Parent Company
  scale_fill_manual(values = RColorBrewer::brewer.pal(5, "Dark2")) +
  # Get the labels created above. They have markdown syntax.
  labs(
    title = title,
    subtitle = subtitle, 
    caption = caption
  ) +
  # Add our description from above
  # Note that X and Y are a trial and error result
  geom_textbox(
    aes(label = description, x = -16011994, y = -4018694), 
    color = "white", size = 2.75, width = grid::unit(0.25, "npc"),
    # remove label background and outline
    fill = NA, box.color = NA, family = "Tahoma", 
    # remove box padding, since we have removed the box outline
    box.padding = grid::unit(rep(0.1, 4), "pt") 
  ) +
  # Adjust the width and alpha limits
  scale_edge_width(range = c(0.25, 0.75)) +
  scale_edge_alpha(range = c(0.15, 0.5)) +
  # This is needed to correctly plot Winkel Tripel!
  coord_sf(datum = NULL) +
  # And final touches
  theme(
    text = element_text(color = "white"),
    plot.background = element_rect(fill = "grey10"),
    panel.background = element_rect(fill = "grey10"),
    # Pass theme options to ggtext to understand markdown syntax
    plot.title = element_markdown(family = 'Tahoma', size = 14), 
    plot.subtitle = element_markdown(family = 'Tahoma', size = 10), 
    plot.caption = element_markdown(family = 'Tahoma', size = 8)
  )
```

### Save plot and crop white space

This final step for me is always trial and error until getting the best
width and height. I use the `here::here()` function to handle paths. The
`knitr::plot_crop()` is a really handy function to give the last
post-processing to our result.

``` r
ggsave(g, filename = here::here("plot", "2021_week_05.png"), 
       device = "png", width = 20, height = 14, units = "cm")
knitr::plot_crop(here::here("plot", "2021_week_05.png"))
```

## Check out the result!

And voilá!

``` r
knitr::include_graphics(here::here("plot", "2021_week_05.png"))
```

<img src="E:/Personal_projects/TidyTuesday/plot/2021_week_05.png" width="2358" />

Hopefully you did not quit half way through this step by step guide and
will be now encouraged to test the `sfnetworks` and `ggraph`
integration! Leave me any suggestions or comments on the issues of this
repo!
