import numpy as np

dataFilePath = ".\\bank-additional-full.txt"

### age + marital + education + housing + duration +
### emp.var.rate + cons.price.idx + cons.conf.idx + euribor3m,
### The numeric variables are [age ,duration ,emp.var.rate, cons.price.idx,cons.conf.idx, euribor3m]
variablesList = [0,2,3,5,10,15,16,17,18,20]
data = []
with open(dataFilePath,"r",encoding="UTF-8") as rh:
    k = 0
    for line in rh:
        if k != 0:
            oneLine = line.strip()
            rowData = oneLine.split(";")
            newData = []
            for index in variablesList:
                try:
                    newData.append(float(rowData[index]))
                except Exception:
                    newData.append(rowData[index])
            #print(newData)
            data.append(newData)
        k += 1

numberOfSamples = len(data)
numberOfVariables = len(data[0])

variablesMap = {}
for i in range(numberOfVariables):
    variablesMap[i] = list()
for i in range(numberOfSamples):
    for j in range(numberOfVariables):
        variablesMap[j].append(data[i][j])
newDataSet = []
print(variablesMap[4][0:3])
for i in range(numberOfVariables):
    if isinstance(variablesMap[i][0],str):
        currentUniqueStringList = list(set(variablesMap[i]))
        numberOfIdenticalVariables = len(currentUniqueStringList) - 1
        string2number = {}
        for j in range(numberOfIdenticalVariables):
            string2number[currentUniqueStringList[j]] = j
        currentData = [[] for _ in range(numberOfIdenticalVariables)]
        for value in variablesMap[i]:
            try:
                corNumber = string2number[value]
                for l in range(numberOfIdenticalVariables):
                    if l != corNumber:
                        currentData[l].append(0.)
                    else:
                        currentData[l].append(1.)
            except Exception:
                for l in range(numberOfIdenticalVariables):
                    currentData[l].append(0.)
        for l in range(numberOfIdenticalVariables):
            newDataSet.append(currentData[l])
    else:
        newDataSet.append((np.array(variablesMap[i]) - np.mean(variablesMap[i])) / np.std(variablesMap[i]) + 0.0)
for varibale in newDataSet:
    print(varibale[0:5])
newDataSet = np.array(newDataSet,dtype=np.float32).transpose()
newData = newDataSet[:,:-1]
newLabel = newDataSet[:,-1]
print(newDataSet)
print(newDataSet.shape)
np.save(".\\processed_select_data",newData)
np.save(".\\labels_select",newLabel)



