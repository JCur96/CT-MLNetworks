# fuzzyDist.py
## edge list creation script
## author: J Curry
## date: 09/03/2022

import csv
#from fuzzywuzzy import fuzz
#from fuzzywuzzy import process
import pandas as pd
from tqdm.notebook import tqdm
from alive_progress import alive_bar
import time

from thefuzz import fuzz
from thefuzz import process

import numpy as np

## might not need these??
import random
import gc 
import timeit
import sys
from datetime import datetime as dt
import nltk
nltk.download('stopwords')
from nltk.corpus import stopwords

from sklearn.feature_extraction.text import TfidfVectorizer
import re

from scipy.sparse import csr_matrix
#%pip install sparse_dot_topn 
import sparse_dot_topn.sparse_dot_topn as ct

import time


refsDf = pd.read_csv('citationNetworks/Data/SplitRefs.csv')
#refsDf['ref_title'] = str(refsDf['ref_title'])
refsDf['ref_title'] = refsDf['ref_title'].astype(str)

STOPWORDS = stopwords.words('english')
STOPWORDS = set(STOPWORDS)
    
def text_prepare(text, STOPWORDS):
    """
        text: a string
        
        return: a clean string
    """
    REPLACE_BY_SPACE_RE = re.compile('[\n\"\'/(){}\[\]\|@,;#]')
    text = re.sub(REPLACE_BY_SPACE_RE, ' ', text)
    text = re.sub(' +', ' ', text)
    text = text.lower()

    # delete stopwords from text
    text = ' '.join([word for word in text.split() if word not in STOPWORDS]) 
    text = text.strip()
    return text

text_prepare(" 'Which camera trap type and how many do I need?'' A review of camera features and study designs for a range of wildlife research applications (2013) Hystrix, 24, pp. 148-156", STOPWORDS)
print(type(refsDf['ref_title']))
refsDf['ProcessedRef'] = refsDf['ref_title'].apply(lambda x: text_prepare(x, STOPWORDS)) # not working

tfidf_vectorizer = TfidfVectorizer(ngram_range=(1,2), max_df=0.9, min_df=5, token_pattern='(\S+)')
tf_idf_matrix = tfidf_vectorizer.fit_transform(refsDf['ProcessedRef'])

print(tf_idf_matrix[0])

refsDf['ProcessedRef'].iloc[0]

def awesome_cossim_top(A, B, ntop, lower_bound=0):
    # force A and B as a CSR matrix.
    # If they have already been CSR, there is no overhead
    A = A.tocsr()
    B = B.tocsr()
    M, _ = A.shape
    _, N = B.shape
 
    idx_dtype = np.int32
 
    nnz_max = M*ntop
 
    indptr = np.zeros(M+1, dtype=idx_dtype)
    indices = np.zeros(nnz_max, dtype=idx_dtype)
    data = np.zeros(nnz_max, dtype=A.dtype)
    ct.sparse_dot_topn(
            M, N, np.asarray(A.indptr, dtype=idx_dtype),
            np.asarray(A.indices, dtype=idx_dtype),
            A.data,
            np.asarray(B.indptr, dtype=idx_dtype),
            np.asarray(B.indices, dtype=idx_dtype),
            B.data,
            ntop,
            lower_bound,
            indptr, indices, data)
    return csr_matrix((data,indices,indptr),shape=(M,N))

    t1 = time.time()

# adjust lower bound: 0.8
# keep top 10 similar results
matches = awesome_cossim_top(tf_idf_matrix, tf_idf_matrix.transpose(), 10, 0.8)

t = time.time()-t1
print("finished in:", t)

refsDf = pd.read_csv('citationNetworks/Data/SplitRefs.csv')
#print(refsDf.head())
#fuzz.ratio(refsDf["Title"], refsDf["Reference"])

# reduce to unique 
# uniqueRefs = refsDf.drop_duplicates(subset=['ref_title'])

# for i in range(len(uniqueRefs)):
#     title = uniqueRefs.iloc[i,6] # 6 or title
#     refTitle = uniqueRefs.iloc[i,7] # 7 or ref title
#     ID = uniqueRefs.iloc[i,4] # 4 or ID
#     ratio = fuzz.ratio(title, refTitle)
#     if ratio > 50:
#         #append to a df with title, re_title, ID, ratio
#         tmpDf = pd.DataFrame([[ID, title, refTitle, ratio]], columns=list('ID', 'Title', 'RefTitle', 'PercentMatch'))


# for index, row in uniqueRefs.iterrows():
#     title = row['Title'] # 6 or title
#     refTitle = row['ref_title'] # 7 or ref title
#     ID = row['ID'] # 4 or ID
#     ratio = fuzz.ratio(title, refTitle)
#     # print(ratio)
#     # if ratio > 50:
#     #     #append to a df with title, re_title, ID, ratio
#     #     tmpDf = pd.DataFrame([[ID, title, refTitle, ratio]], columns=list('ID', 'Title', 'RefTitle', 'PercentMatch'))
    
nan_value = float("NaN")
uniqueRefs = uniqueRefs.replace("", nan_value)
uniqueRefs = uniqueRefs.dropna(subset=["ref_title"])
# !jupyter nbextension enable --py widgetsnbextension

# miniUniqueRefs = uniqueRefs.iloc[0:10]
# titleDf = uniqueRefs = refsDf.drop_duplicates(subset=['Title'])#
# #tmpDf = pd.DataFrame([[ID, title, refTitle, ratio, bestMatch, bestMatchID]]) # add bits needed
# tmpDf = pd.DataFrame(columns=('ID', 'Title', 'RefTitle', 'PercentMatch', 'BestMatchTitle', 'BestMatchID'))
# columns = list(tmpDf)
# newDF = []

# for index, row in tqdm(miniUniqueRefs.iterrows()):
#     title = row['Title'] # 6 or title
#     refTitle = str(row['ref_title']) # 7 or ref title
#     ID = row['ID'] # 4 or ID
#     output = process.extractOne(refTitle, titleDf['Title'], scorer=fuzz.token_sort_ratio)
#     # print(output)
#     ratio = output[1]
#     bestMatch = output[0]
#     bestMatchID = output[2]
#     values = [ID, title, refTitle, ratio, bestMatch, bestMatchID]
#     zipped = zip(columns, values)
#     dictionary = dict(zipped)
#     newDF.append(dictionary)
#     # tmpDf = pd.DataFrame([[ID, title, refTitle, ratio, bestMatch, bestMatchID]]) # add bits needed
#     # tmpDf.columns=('ID', 'Title', 'RefTitle', 'PercentMatch', 'BestMatchTitle', 'BestMatchID')
#     # tmpDf.append(tmpDf)
# print(tmpDf)
titleDf = uniqueRefs = refsDf.drop_duplicates(subset=['Title'])#
#tmpDf = pd.DataFrame([[ID, title, refTitle, ratio, bestMatch, bestMatchID]]) # add bits needed
tmpDf = pd.DataFrame(columns=('ID', 'Title', 'RefTitle', 'PercentMatch', 'BestMatchTitle', 'BestMatchID'))
columns = list(tmpDf)
newDF = []

#with alive_bar(238148, force_tty=True) as bar:
for index, row in tqdm(uniqueRefs.iterrows()):
    title = row['Title'] # 6 or title
    refTitle = str(row['ref_title']) # 7 or ref title
    ID = row['ID'] # 4 or ID
    output = process.extractOne(refTitle, titleDf['Title'], scorer=fuzz.token_sort_ratio)
    # print(output)
    ratio = output[1]
    bestMatch = output[0]
    bestMatchID = output[2]
    values = [ID, title, refTitle, ratio, bestMatch, bestMatchID]
    zipped = zip(columns, values)
    dictionary = dict(zipped)
    newDF.append(dictionary)
    #tmpDf = pd.DataFrame([[ID, title, refTitle, ratio, bestMatch, bestMatchID]]) # add bits needed
    #tmpDf.columns=('ID', 'Title', 'RefTitle', 'PercentMatch', 'BestMatchTitle', 'BestMatchID')
    #tmpDf.append(tmpDf)
#bar()
miniUniqueRefs = uniqueRefs.iloc[0:10]
df0_names = list(miniUniqueRefs.ref_title.unique())
df1_names = list(uniqueRefs.ref_title.unique())
df2_names = list(uniqueRefs.Title.unique())
print(process.extractOne(str(df0_names), df2_names, scorer=fuzz.token_sort_ratio))
#print(process.extractOne(str(df1_names), df2_names, scorer=fuzz.token_sort_ratio))
def match_names(name, list_names, min_score=0):
    max_score = -1
    max_name = ''
    for x in list_names:
        score = fuzz.ratio(name, x)
        if (score > min_score) & (score > max_score):
            max_name = x
            max_score = score
    return (max_name, max_score)

df1_names = list(uniqueRefs.ref_title.unique())
titlesList = list(uniqueRefs.Title.unique())
refsAsArray = uniqueRefs.to_numpy()
miniUniqueRefs = uniqueRefs.iloc[0:10]
miniArray = miniUniqueRefs.to_numpy()

print(process.extractOne(str(miniArray[:,5]), titlesList, scorer=fuzz.token_sort_ratio))
