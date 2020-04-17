from sklearn.linear_model import LogisticRegression
import numpy as np
import sklearn.metrics as metrics
import matplotlib.pyplot as plt

trainingDataRatio = 0.8
enUpR = 0

### Data pre-processing
data = np.load("processed_select_data.npy")
# print(data[0])
labels = np.load("labels_select.npy")
numberOfSamples = data.shape[0]
oneLabelIndexAll = np.where(labels == 1)[0]
zeroLabelIndexAll = np.where(labels == 0)[0]
print(oneLabelIndexAll)
numberOfAllOneLabel = oneLabelIndexAll.shape[0]
numberOfAllZeroLabel = zeroLabelIndexAll.shape[0]
trainingData = []
trainingLabels = []
testingData = []
testingLabels = []
for i in range(numberOfAllOneLabel):
    if i <= int(numberOfAllOneLabel * trainingDataRatio):
        trainingData.append(data[oneLabelIndexAll[i]])
        trainingLabels.append(labels[oneLabelIndexAll[i]])
    else:
        testingData.append(data[oneLabelIndexAll[i]])
        testingLabels.append(labels[oneLabelIndexAll[i]])
for i in range(numberOfAllZeroLabel):
    if i <= int(numberOfAllZeroLabel * trainingDataRatio):
        trainingData.append(data[zeroLabelIndexAll[i]])
        trainingLabels.append(labels[zeroLabelIndexAll[i]])
    else:
        testingData.append(data[zeroLabelIndexAll[i]])
        testingLabels.append(labels[zeroLabelIndexAll[i]])
### calculate the weight of labels
trainingSamples = len(trainingData)
trainingData = np.array(trainingData)
trainingLabels = np.array(trainingLabels)
testingData = np.array(testingData)
testingLabels = np.array(testingLabels)
oneSamples = sum(trainingLabels)
print("The one samples for training ", oneSamples)
zeroSamples = trainingSamples - oneSamples
print("The zero samples for training ", zeroSamples)
oneLabelIndex = np.where(trainingLabels == 1)
for index in oneLabelIndex:
    for t in range(enUpR):
        trainingData = np.append(trainingData, trainingData[index], axis=0)
        trainingLabels = np.append(trainingLabels, trainingLabels[index], axis=0)
print(trainingData)
print(trainingLabels)
print(trainingData.shape)
print(trainingLabels.shape)
print(testingData.shape)
print(testingLabels.shape)
print("True positive testing samples :",sum(testingLabels))
lrg = LogisticRegression(n_jobs=2,C = 0.5)
lrg.fit(X =trainingData,y = trainingLabels)
result = lrg.predict_proba(testingData)
print(result)
trainingResults = lrg.predict_proba(trainingData)
predictList = []
scores = []
for oneResult in result:
    position = np.argmax(oneResult)
    predictList.append(position)
    scores.append(oneResult[1])
acc = metrics.accuracy_score(y_true=testingLabels, y_pred=predictList)
F1 = metrics.f1_score(y_true=testingLabels, y_pred=predictList)
confusionMatrix = metrics.confusion_matrix(y_true=testingLabels, y_pred=predictList)
fpr, tpr, thresholds = metrics.roc_curve(testingLabels, scores, pos_label=1)
auc = metrics.auc(fpr, tpr)
print("ACC : ",acc)
print("AUC : ",auc)
print("F1 : ",F1)
print(confusionMatrix)

plt.figure()
lw = 2
plt.plot(fpr, tpr, color='darkorange',
         lw=lw, label='ROC curve (area = %0.2f)' % auc)
plt.plot([0, 1], [0, 1], color='navy', lw=lw, linestyle='--')
plt.xlim([0.0, 1.0])
plt.ylim([0.0, 1.05])
plt.xlabel('False Positive Rate')
plt.ylabel('True Positive Rate')
plt.title('ROC of Logistic regression')
plt.legend(loc="lower right")
plt.show()

### Model diagnostic

### There is no influential values (extreme values or outliers) in the continuous predictors
### Because i did Z-Score transformation for all numerica variables

### Multicolinearity
### [age, emp.var.rate, cons.price.idx,cons.conf.idx, euribor3m]
dataTranspose = list(trainingData.transpose())
xVariables = []
for variables in dataTranspose:
    thisSet = set(variables)
    print(thisSet)
    if len(thisSet) > 2:
        xVariables.append(variables)
correlationMatrix = np.corrcoef(xVariables)
print("VIF")
print(np.linalg.inv(correlationMatrix))
### all VIF smaller than 10, there is no multicolinearity between its.

### it requires that the independent variables are linearly related to the log odds
### because the Pearson correlation can express the linearly between two variables.
logit = np.log(np.array(trainingResults[:,1]) / (1. - trainingResults[:,1]))
correlateX_Logit = []
for variables in xVariables:
    correlateX_Logit.append(np.corrcoef(logit,variables)[0,1])
print("correlate : X - Logit")
print(correlateX_Logit)

with open("trainingResultP.txt","w") as wh:
    for i in range(len(trainingLabels)):
        wh.write(str(float(trainingResults[i,1])) + "\t"
                 + str(trainingLabels[i]) + "\n")
