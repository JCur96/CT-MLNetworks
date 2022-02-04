## network.py
## citation network creation script
## author: J Curry
## date: 24/01/2022
##

## imports ##
from unicodedata import name
import metaknowledge as mk
import networkx as nx
import matplotlib.pyplot as plt 
#%matplotlib inline
import metaknowledge.contour.plotting as mkv



## convert Web of Science plain text / tab delimited to a record collection 
#help(mk.RecordCollection)
#RC = mk.RecordCollection('savedrecs.txt', name='savedrecs', extension='txt')
RC = mk.RecordCollection(inCollection='docNonNetwork\\records\\', name='savedrecs', extension='.txt')
CoCitation = RC.networkCoCitation()
print(mk.graphStats(CoCitation, makeString = True))


CoCitation.nodes()
CoCitation.edges()
coCiteJournals = RC.networkCoCitation(nodeType = 'journal', dropNonJournals = True)
print(mk.graphStats(coCiteJournals))

nx.draw_spring(coCiteJournals)
citationsA = RC.networkCitation(nodeType = 'year', keyWords = ['A'])
print(mk.graphStats(citationsA))
nx.draw_spring(citationsA, with_labels = True)


coAuths = RC.networkCoAuthor()
print(mk.graphStats(coAuths))

wcCoOccurs = RC.oneModeNetwork('WC')
print(mk.graphStats(wcCoOccurs))

nx.draw_spring(wcCoOccurs, with_labels = True)

