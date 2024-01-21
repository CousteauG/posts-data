require(readxl)
require(dplyr)

email_gen <- function(nom, ape, nac){
  paste0(substr(x = nom, start = 1, stop = 2),
         gsub(x = ape, pattern = " |[a-z]+$", replacement = ""),
         format(x = nac, format = "_%y@"),
         sample(x = c("gmail", "outlook", "yahoo", "msn"), 
                size = length(nom), replace = TRUE),
         ".com") %>% 
    
    tolower %>% 
    
    chartr(old = "\u00e1\u00e9\u00ed\u00f3\u00fa\u00fc\u00f1", 
           new = "aeiouunn")
}

n <- 5000

inputData <- mapply(sheet = c(nombres = 1, apellidos = 2, ciudades = 3), read_excel, 
                    MoreArgs = list(path = "nombres-apellidos-ciudades.xlsx"))

set.seed(1234)

inputData[1:2] %>% 
  
  lapply(pull, 1) %>% 
  
  lapply(combn, m = 2) %>% 
  
  lapply(\(x) x[,sample(x = seq(ncol(x)), size = n)] %>% apply(2, paste, collapse = " ")) %>% 
  
  as_tibble %>% 
  
  mutate(dni = sample(x = seq(from = 11111111, to = 99999999), size = n),
         nacimiento = sample(x = seq(from = as.Date("1970-1-1"), 
                                     to = as.Date("2000-1-1"), 
                                     by = "day"),
                             size = n, replace = TRUE),
         email = email_gen(nom = nombres, ape = apellidos, nac = nacimiento),
         categoria = sample(x = LETTERS[1:4], size = n, 
                            replace = TRUE, prob = c(1, 3, 4, 2))) %>% 
  
  bind_cols(inputData$ciudades %>% 
              
              slice(sample(x = seq(nrow(.)), size = n, replace = TRUE))) %>% 
  
  rename(ciudad = capital) 
