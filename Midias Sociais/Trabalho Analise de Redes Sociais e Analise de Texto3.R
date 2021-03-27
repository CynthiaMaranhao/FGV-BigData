Sys.setlocale("LC_ALL", "pt_BR.UTF-8")
#Instala os pacotes necess√°rios
list.of.packages <- c('rvest',
                      'stringr',
                      'tidyverse',
                      'tm',
                      'igraph',
                      'wordcloud',
                      'urltools',
                      'spacyr',
                      # 'textreadr', # Melhor rvest
                      'rvest',
                      'magrittr',
                      'urltools',
                      'gtools')
new.packages <-
  list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages))
  install.packages(new.packages, dependencies = TRUE)
for (package in list.of.packages){
  library(package, character.only = TRUE)
}


#Fun√ß√£o para acessar os links
scrape_post_links <- function(site) {
  print(paste("Baixando ", site))
  # scrape HTML from input site
  source_html <- read_html(site)
  print(source_html)
  # grab the title attributes from link (anchor)
  # tags within H2 header tags
  links <- source_html %>%
    html_nodes("div.widget--info__text-container") %>%
    html_nodes("a") %>%
    html_attr("href")
  # filter out any titles that are NA (where no title was found)
  links <- links[!is.na(links)]
  # return vector of titles
  return(links)
}

#FunÁ„o para extrair os links
extract_urls <- function(raw_url) {
  params <- urltools::param_get(raw_url)
  scraped_url <- params$u
  return (url_decode(scraped_url))
}

cleaned_links <- lapply(all_links, extract_urls)

# Not interested in Videos from globoplay app
cleaned_links <- Filter(function(x) !any(grepl("globoplay", x)),
                        cleaned_links)


#Acessando cada link
scrape_post_body <- function(site) {
  # Escape 404 Not Found Errors
  try(
    text <- site %>%
      read_html %>%
      html_nodes("article") %>%
      html_nodes("p.content-text__container") %>%
      html_text
  )
}

#Itera√ß√£o em 20 p√°ginas
root <- "https://g1.globo.com/busca/?q=rachadinhas"
# get each webpage URL we need
all_pages <- c(root, paste0(root, "&page=", 1:20))
# use our function to scrape the title of each post
all_links <- lapply(all_pages, scrape_post_links)
# collapse the titles into a vector
all_links <- unlist(all_links)



data <- lapply(cleaned_links, scrape_post_body)
data <- lapply(data,
               function(item) paste(unlist(item),
                                    collapse = ''))

##PR√â-PROCESSAMENTO DO TEXTO
# convert all titles to lowercase
cleaned <- tolower(data)
# remove any numbers from the titles
cleaned <- removeNumbers(cleaned)
# remove English stopwords
cleaned <- removeWords(cleaned, c(stopwords("pt"), "rachadinha","sobre", "parte", "ser", "janeiro","segundo", "disse","nesta", "primeira", "nessa", "ainda", "rachadinhas"))
# remove punctuation
cleaned <- removePunctuation(cleaned)
# remove spaces at the beginning and end of each title
cleaned <- str_trim(cleaned)
# convert vector of titles to a corpus
cleaned_corpus <- Corpus(VectorSource(cleaned))
# steam each word in each title
# cleaned_corpus <- tm_map(cleaned_corpus, stemDocument)
doc_object <- TermDocumentMatrix(cleaned_corpus)
doc_matrix <- as.matrix(doc_object)
# get counts of each word
counts <- sort(rowSums(doc_matrix),decreasing=TRUE)
# filter out any words that contain non-letters
counts <- counts[grepl("^[a-z]+$", names(counts))]
# create data frame from word frequency info
frame_counts <- data.frame(word = names(counts), freq = counts)

#Cria a nuvem de palavras
wordcloud(words = frame_counts$word,
          freq = frame_counts$freq,
          min.freq = 1,
          max.words=200, random.order=FALSE,
          rot.per=0,
          scale=c(3.5,0.25),
          colors=brewer.pal(8, "Dark2"))

# Mostra os top10 Palavras com maior frequ√™ncia
head(frame_counts, 20)

# Plotagem dos Top 10
barplot(frame_counts[1:20,]$freq, las = 2, 
        names.arg = frame_counts[1:20,]$word,
        col ="lightblue", main ="Palavras Mais Frequentes",
        ylab = "Frequ√™ncias de Palavras")


#Instala e roda spaCy
spacy_install()
spacy_download_langmodel('pt')

#Analise de Entidades - filtra por tipo de Entidades
spacy_initialize(model="pt_core_news_sm")
entities <- spacy_extract_entity(unlist(data))
head(entities)

## lista de adjacÍncia onde cada aresta define a concorrÍncia de duas entidades em um texto
# group entities by document
filtered_entities <- subset(entities, entities["ent_type"] == "ORG" |
                                      entities["ent_type"] == "PER" )

#Agrupa as Entidades por Identidades (concatena)
edges <- filtered_entities %>%
  group_by(doc_id) %>%
  summarise(entities = paste(text, collapse = ","))

#Remove duplicatas do mesmo documento
edges <- lapply(str_split(edges$entities, ","),
                function(t){unique(unlist(t))})

# FunÁ„o Auxiliar para criar adjancentes
get_adjacent_list <- function(edge_list) {
  if(length(edge_list)>2)
    adjacent_matrix <- combinations(length(edge_list),
                                  2, edge_list)
}

adjacent_matrix <- edges %>%
  lapply(get_adjacent_list) %>%
  reduce(rbind)

#Objeto Grafo que ser· exportado por Gephi
df <- as_tibble(adjacent_matrix, colnames=c('source', 'target'))
weighted_edgelist <- df %>%
  group_by(V1, V2) %>%
  summarise(weight=n())
news_graph <- weighted_edgelist %>% graph_from_data_frame(directed=F)

#CriaÁ„o do Arquivo para exportar pro Gephi
write_graph(news_graph, 'news_graph3.graphml', 'graphml')
