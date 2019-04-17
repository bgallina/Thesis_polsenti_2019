# Docker futtatása cmdben:
# docker run -it -p 9090:9090 oroszgy/hunlp

######Python######
#Kód lefuttatása shellben:
import hunlp
from hunlp import HuNlp
import pandas as pd
import csv
from tqdm import tqdm

nlp = HuNlp()

#Beolvasom a korpuszt:
mylist = []

for chunk in  pd.read_csv('~/Egyetemi/survey/Szakdolgozat/thesis_polsenti/src/corpus.csv', sep=';' , chunksize=20000):
    mylist.append(chunk)

big_data = pd.concat(mylist, axis= 0)
del mylist


#Csak az output oszlopot kiválasztom:
string = big_data .loc[: , "text"]
del big_data
doc = []

#Lemmatizálás
for i in tqdm(range(len(string))):
    if isinstance(string[i], float):
        pass
    elif len(string[i]) < 1500:
            for sent in nlp(string[i]):
                for tok in sent:
                    writer.writerows([[tok.lemma,str(i)]])

