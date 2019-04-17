
removeWords <- function(str, stopwords) {
  x <- unlist(strsplit(str, " "))
  paste(x[!x %in% stopwords], collapse = " ")
}


stopwords <- c('a', 'ahogy', 'ahol', 'aki', 'akik', 'akkor', 'alatt', 'által', 'általában', 
               'amely', 'amelyek', 'amelyekben', 'amelyeket', 'amelyet', 'amelynek', 'ami', 
               'amit', 'amolyan', 'amíg', 'amikor', 'át', 'abban', 'ahhoz', 'annak', 'arra', 
               'arról', 'az', 'azok', 'azon', 'azt', 'azzal', 'azért', 'aztán', 'azután', 
               'bár', 'be', 'belül', 'benne', 'cikk', 'cikkek', 'cikkeket', 'csak', 
               'e', 'eddig', 'egész', 'egy', 'egyes', 'egyetlen', 'egyéb', 'egyik', 
               'egyre', 'ekkor', 'el', 'elég', 'ellen', 'elő', 'először', 'előtt', 'első', 
               'én', 'éppen', 'ebben', 'ehhez', 'emilyen', 'ennek', 'erre', 'ez', 'ezt', 
               'ezek', 'ezen', 'ezzel', 'ezért', 'és', 'fel', 'felé', 'hanem', 'hiszen', 
               'hogy', 'hogyan', 'igen', 'így', 'illetve', 'ill.', 'ill', 'ilyen', 'ilyenkor', 
               'is','ison', 'ismét', 'itt', 'kell', 'kellett', 'keresztül', 
               'keressünk', 'ki', 'kívül', 'között', 'közül', 'legalább', 'lehet', 'lehetett', 
               'legyen', 'lenne', 'lenni', 'lesz', 'lett', 'maga', 'magát', 'majd', 'majd', 
               'már', 'más', 'másik', 'meg', 'még', 'mellett', 'mert', 'mely', 'melyek', 'mi', 
               'mit', 'míg', 'miért', 'milyen', 'mikor', 'minden', 'mindent', 'mindenki', 
               'mindig', 'mint', 'mintha', 'mivel', 'most', 'nagy', 'nagyobb', 'nagyon', 'ne', 
               'néha', 'nekem', 'neki', 'néhány', 'olyan', 'ott', 
               'össze', 'ő', 'ők', 'őket', 'pedig', 'persze', 'rá', 's', 'saját', 
               'sok', 'sokat', 'sokkal', 'számára', 'szemben', 'szerint', 'szinte', 'talán', 
               'tehát', 'teljes', 'tovább', 'továbbá', 'több', 'úgy', 'ugyanis', 'új', 'újabb', 
               'újra', 'után', 'utána', 'utolsó', 'vagy', 'vagyis', 'valaki', 'valami', 'valamint', 
               'való', 'vagyok', 'van', 'vannak', 'volt', 'voltam', 'voltak', 'voltunk', 'vissza', 
               'vele', 'volna', 'úgyhogy','azaz', 'ha', 'esetén','során', 'óta')
