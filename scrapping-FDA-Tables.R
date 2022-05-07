#install and import packages needed for scrapping, cleaning column names, and for NLP
library(rvest)
library(tidyverse)
library(dplyr)
library(janitor)
library(udpipe)
library(lattice)

# scrapping html tables of novel drug approvals from FDA website
x = seq(2015,2022)
tables <- list()
index <- 1
for (i in x){
  Sys.sleep(5)
 try({ 
  url = paste0('https://www.fda.gov/drugs/new-drugs-fda-cders-new-molecular-entities-and-new-therapeutic-biological-products/novel-drug-approvals-',i)
  
  table <- url %>% read_html() %>% html_table(fill = TRUE)
  
  tables[index] <- table
  
  Sys.sleep(5) 
  index <- index + 1 
  
  })
}

## cleaning tables in order to have same number of columns and same column names 
tables[[6]] = tables[[6]][,-6]

tables[[1]] = tables[[1]] %>% rename('Approval Date' = Date)

y = seq(1,8)

for(i in y){
tables[[i]] = tables[[i]] %>% clean_names()
}


# concatenate tables in one large table  
df <- do.call("rbind", tables)


# as the approval use is very long, an attempt of finding keywords was performed using NPL package

ud_model <- udpipe_download_model(language = "english")

udmodel_english <- udpipe_load_model(file = 'english-ewt-ud-2.5-191206.udpipe')

keywords_sub <- subset(df)

x <- udpipe_annotate(udmodel_english , x = keywords_sub$fda_approved_use_on_approval_date)

x = as.data.frame(x)

stats <- subset(x, upos %in% "NOUN")
stats <- txt_freq(x = stats$lemma)

stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 100), col = "blue", main = "Most occurring nouns", xlab = "Freq")


## conditions was created for manual validation 
all_conditions = df[,5]




#words were collected from stats table which represent the freq of words and a manual validation took place
# to generate a list of keywords
words = c('cancer','heart', 'leukemia', 'lymphoma', 'infection' , 'cardio', 'urinary tract infection', 'gout', 'hypertension', 'neuromuscular',
          'acid lipase (LAL) deficiency', 'myeloma', 'melanoma', 'HIV', 'asthma', 'hypophosphatasia', 
          'sarcoma', 'hyperkalemia','blood-thinning', 'schizophrenia', 'diabetes', 'aciduria', 'chemotherapy-induced nausea',
          'high cholesterol', 'cystic fibrosis', 'blood clots' , 'irritable bowel syndrome',
          'submental fat', 'bile acid synthesis disorders', 'neuroblastoma', 'hypoparathyroidism', 'psoriasis',
          'eczema', 'muscular dystrophy', 'glycemic control', 'dry eye', 'hepatitis C', 'tumor', 'chronic liver disease', 'multiple sclerosis', 
          'hallucinations', 'hepatic veno-occlusive', 'anthrax', 'seizures', 'increase blood pressure', 'growth hormone',
          'impetigo', 'ocular hypertension', 'hemophilia A ',
          'mucopolysaccharidosis', 'bacterial vaginosis', 'Chagas disease','thromboembolism', 'arthritis', 'amyotrophic lateral sclerosis', 'carcinoma', 'osteoporosis',
          'Batten disease', 'dyskinesia', 'chorea', 'opioid-induced constipation', 'Parkinson', 'carcinoid syndrome diarrhea',
          'hyperparathyroidism', 'Chronic Idiopathic Constipation', 'paroxysmal nocturnal hemoglobinuria', 'neoplasm' , 'Lambert-Eaton myasthenic syndrome',
          'hemophagocytic lymphohistiocytosis', 'travelers’ diarrhea', 'COPD', 'influenza', 'polyneuropathy' , 'Immunodeficiency',
          'acne', 'migraine' , 'angioedema' , 'neurotrophic keratitis', 'Fabry disease', 'vaginal ring', 'cholestasis', 'smallpox', 'malaria', 'endometriosis',
          'epilepsy', 'onchocerciasis', 'phenylketonuria', 'insomnia',  'opioid withdrawal symptoms', 'ebola', 'COVID', 'graft-versus-host', 'cholestatic pruritus', 'muscular atrophy',
          'deficit hyperactivity', 'Alzheimer', 'Cushing', 'hypophosphatemia', 'Parkinson',  'myelodysplastic',  'thrombotic thrombocytopenic purpura' , 'Adenosine Deaminase-Severe Combined Immunodeficiency',
          'hepatic porphyria', 'Keratosis', 'hypoactive sexual desire disorder ', 'molybdenum' , 'candidiasis',  'pregnancy' , 'acute pain' , 'phototoxic reactions', 'septic', 'achondroplasia' , 'agglutinin disease',
          'tuberculosis', 'anemia' ,  'glabellar lines', 'sedation' , 'eye surgery', ' infertility' , 'Parkinsonian syndromes', 'atopic dermatitis', 'thrombocytopenia', 'overactive bladder' , 'sickle cell',
          'postpartum depression', 'fascioliasis', 'obesity', 'hyperoxaluria', 'premature aging', 'Growth hormone', 'head lice', 'pruritus', 'von Hippel-Lindau', 'trypanosomiasis', 'hypoglycemia', 'hypercholesterolemia',
          'chronic idiopathic constipation', 'vasculitis', 'lysosomal acid lipase', 'macular degeneration', 'bacterial pneumonia', 'myelofibrosis', 'sleepiness' , 'neuromyelitis', 'long-chain fatty acid oxidation', 'lupus erythematousus',
          'upus nephritis', 'myelofibrosis', 'Pompe disease', 'diabetic macular edema', 'myloma', 'nausea and vomiting after surgery', 'Thyroid eye disease','myasthenia gravis', 'polycythemia vera') 


# an empty column was created for keywords
df$indication = NA

# detecting keywords in approved-use column and placing the word in indication
for (word in words){
  df[grep(word, df$fda_approved_use_on_approval_date),"indication"] = word
}


## filtering empty rows and manually fixing a glitch causing one deviated row in 2020 approvals

df = df %>%  mutate (approval_date = case_when(drug_name == 'Ebanga'~'12/21/2020',  T ~ approval_date ) , fda_approved_use_on_approval_date = case_when(drug_name == 'Ebanga' ~ 'To treat ebola', T ~ fda_approved_use_on_approval_date), indication = case_when(drug_name == 'Ebanga' ~ 'Ebola', T ~ indication ))


df = df %>% filter(!is.na(indication)) 




 
test = df %>%  mutate(condition = case_when(
      str_detect(indication, 'oma|cancer|tumor|chemotherapy-induced nausea|leukemia|carcinoid syndrome diarrhea|neoplasm|myelofibrosis|myelodysplastic',  negate = F) ~ 'Cancers',
      str_detect(fda_approved_use_on_approval_date, 'graft-versus-host|hemophilia A|paroxysmal nocturnal hemoglobinuria|hypophosphatasia|angioedema|Pompe disease|rare|neuromyelitis|von Hippel-Lindau|agglutinin disease|molybdenum|polycythemia vera') ~ 'rare', str_detect(indication, 'blood-thinning|vasculitis|heart|high cholesterol|cardio|hypercholesterolemia|hyperkalemia|hypertension|thromboembolism|blood clots', negate = F) ~ 'Cardiovascular diseases',
      str_detect(indication, 'anthrax|infection|bacterial vaginosis|septic|Chagas disease|impetigo|candidiasis|trypanosomiasis', negate = F) ~ 'Infectious diseases', str_detect(indication, 'chronic idiopathic constipation|Chronic Idiopathic Constipation|irritable bowel syndrome|opioid-induced constipation|travelers’ diarrhea') ~ 'Digestive diseases', str_detect(indication, 'asthma|COPD|cystic fibrosis', negate = F) ~ 'Respiratory diseases',
      str_detect(indication, 'HIV') ~ 'HIV/AIDS', str_detect(indication, 'malaria') ~ 'Malaria',   str_detect(indication, 'Growth hormone|hyperparathyroidism|growth hormone|hypoparathyroidism|endometriosis|gout|Cushing') ~ 'Endocrine diseases',
      str_detect(indication, 'tuberculosis' ) ~ 'Tuberculosis',  str_detect(indication, 'hepatitis C' ) ~ 'Hepatitis',  str_detect(indication, 'neuromuscular|muscular atrophy|schizophrenia|multiple sclerosis|amyotrophic lateral sclerosis|dyskinesia|muscular dystrophy|epilepsy|seizures|sleepiness|insomnia|deficit hyperactivity|myasthenia gravis' , negate = F) ~ 'Neurological diseases', 
      str_detect(indication, 'acne|eczema|psoriasis|phototoxic reactions|atopic dermatitis|smallpox|lupus erythematousus|pruritus' , negate = F ) ~ 'Skin diseases',  str_detect(indication, 'bacterial pneumonia' ) ~ 'Lower respiratory infections', str_detect(indication, 'diabetes|hypoglycemia|diabetic macular edema|glycemic control|polyneuropathy', negate = F) ~ 'Diabetes', 
      str_detect(indication, 'chronic liver disease|hepatic veno-occlusive|cholestasis|bile acid synthesis disorders' , negate = F) ~ 'Liver diseases', str_detect(indication, 'Parkinson' ) ~ "Parkinson's disease",  str_detect(indication, 'osteoporosis|arthritis' , negate = F) ~ 'Skeletal diseases', str_detect(indication, 'migraine' ) ~ 'Migraine',
      str_detect(indication, 'influenza' ) ~ 'Flu', str_detect(indication, 'neurotrophic keratitis|dry eye|Keratosis|macular degeneration|eye surgery|Thyroid eye disease',  negate = F ) ~ 'Eye diseases', str_detect(indication, 'anemia|sickle cell|thrombocytopenia',  negate = F ) ~ 'Blood diseases', str_detect(indication, 'Alzheimer',  negate = F ) ~ "Alzheimer's disease and other dementias",
      str_detect(indication, 'nephritis' ) ~ 'Kidney disease', str_detect(indication, 'mucopolysaccharidosis|Batten disease|achondroplasia|chorea', negate = F ) ~ 'Genetic diseases', str_detect(indication, 'hypoactive sexual desire disorder| infertility', negate = F ) ~ 'Sexual dysfunction',
      F ~ indication
))


ts = test %>% filter(is.na(condition)) 
     