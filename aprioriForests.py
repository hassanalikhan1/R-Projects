#--
import pandas as pd

from apyori import apriori  

df = pd.read_csv("forests.txt",sep=" ",header=None,names = list(range(0,140)))

records = []  
for i in range(0, len(df)):  
    records.append([df.values[i,j] for j in range(0, 140)])



##all patterns with support threshold of 50%

association_rules = apriori(records, min_support=0.5)  

association_results = list(association_rules)  

print association_results[0][0]
print("Patterns of plant species:")
text_file = open("results1.txt", "w")
text_file.write("Patterns of plant species:\n\n\n\n")

for rule in association_results:

	item=rule[0]
	items=[x for x in item]

	for a in items:
		text_file.write(str(int(a))+ " ")
	text_file.write("\n")
	print items

text_file.close()

##part C

association_rules = apriori(records, min_support=0.5,min_confidence=0.7)  

association_results = list(association_rules)
print association_results

text_file = open("results2.txt", "w")
text_file.write("Rules with support threshold:40 and confidence:70%:\n\n\n\n")

for rule in association_results:
	
	text_file.write("Support: " + str(rule[1])+"\n")

	for order in rule[2]:
		
		item=order[0]
		items=[x for x in item]
		text_file.write("Rule: ")
		for a in items:
			text_file.write(str(int(a))+" ")
		text_file.write(" -> ")

		item1=order[1]
		items1=[y for y in item1]
		for b in items1:
			text_file.write(str(int(b))+" ")

		text_file.write("\n")

		#second index of the inner list
		#third index of the list located at 0t
		#of the third index of the inner list

		text_file.write("Confidence: " + str(order[2])+"\n")
		text_file.write("Lift: " + str(order[3])+"\n")
		text_file.write("=====================================\n")

text_file.close()




# PART B


association_rules = apriori(records, min_support=0.5)  
association_results = list(association_rules)

text_file = open("results3.txt", "w")
text_file.write("Maximun frequent patterns :\n\n\n\n")

item_Sets=[]
for rule in association_results:

 	item=rule[0]
 	items=[int(x) for x in item]

 	item_Sets.append(items)

sets={frozenset(e) for e in item_Sets}  
us=[]
while sets:
	e=sets.pop()
	if any(e.issubset(s) for s in sets) or any(e.issubset(s) for s in us):
		continue
	else:
		us.append(list(e))
		for bla in e:
			text_file.write(str(int(bla))+" ")
	text_file.write("\n")

print us


