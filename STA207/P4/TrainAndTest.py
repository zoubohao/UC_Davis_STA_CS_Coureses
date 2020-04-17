import torch
import torchvision as tv
from torch.utils import data as d
import numpy as np
import Model
import torch.nn as nn
import torch.optim as optim
import math
from sklearn import metrics
from torch.utils.data import Dataset

class MyDataSet(Dataset):

    def __init__(self,myNpData:np.array,targets:np.array,shape:list,transform=None):
        """
        :param myNpData: [numberOfSample,numberOfVariables]
        :param targets: [numberOfSample,numberOfClasses]
        :param transform: transforms in torch-vision
        """
        self.data = myNpData
        self.targets = targets
        self.transform = transform
        self.numberOfPadding = shape[0] * shape[1] - self.data.shape[1]
        self.shape = shape
        if self.numberOfPadding < 0 : raise Exception("number of padding can not lower than zero.")


    def __len__(self):
        return len(self.data)

    def __getitem__(self, item):
        currentData = np.array(list(self.data[item] + 0.01) + [0. for _ in range(self.numberOfPadding)]).reshape(self.shape)
        currentTarget = self.targets[item]
        if self.transform is not None:
            currentData = self.transform(currentData)
        return currentData,currentTarget


if __name__ == "__main__":
    ### config
    batchSize = 32
    growthRate = 32
    blocks = [6,12,24]
    learning_rate = 1e-4
    labelsNumber = 2
    ifUseBn = True
    ifTrain = True
    epoch = 30
    displayTimes = 100
    modelSavePath = ".//"
    loadWeight = False
    trainModelLoad = 3
    testModelLoad = 29
    decayRate = 0.98
    decayStep = 2500
    trainingDataRatio = 0.8
    weight_decay = 5e-4

    ### Data pre-processing
    data = np.load("processed_data.npy")
    #print(data[0])
    labels = np.load("labels.npy")
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
    print("The one samples for training ",oneSamples)
    zeroSamples = trainingSamples - oneSamples
    print("The zero samples for training ",zeroSamples)
    enUpR = int(zeroSamples // oneSamples - 1)
    oneLabelIndex = np.where(trainingLabels == 1)
    for index in oneLabelIndex:
        for t in range(enUpR):
            trainingData = np.append(trainingData,trainingData[index],axis=0)
            trainingLabels = np.append(trainingLabels,trainingLabels[index],axis=0)
    print(trainingData)
    print(trainingLabels)
    transformationTrain = tv.transforms.Compose([
        tv.transforms.ToTensor()
    ])

    transformationTest = tv.transforms.Compose([
        tv.transforms.ToTensor(),
    ])
    trainingSet = MyDataSet(trainingData,trainingLabels,shape=[8,7],transform=transformationTrain)
    testingSet = MyDataSet(testingData,testingLabels,shape=[8,7],transform=transformationTest)
    dataLoader = d.DataLoader(trainingSet,batch_size=batchSize,shuffle=True,num_workers=4,pin_memory=True)
    device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
    ### model construct
    model = Model.MyModel(1,labelsNumber,blocks,growthRate,ifUseBn).to(device)
    #print(model)
    #lossCri = Model.LabelsSmoothingCrossLoss(labelsNumber,0.09).to(device)
    lossCri = nn.CrossEntropyLoss(reduction="sum")
    optimizer = optim.SGD(model.parameters(),lr=learning_rate,momentum=0.9,nesterov=True,weight_decay=weight_decay)
    if loadWeight :
       # print(torch.load(modelSavePath + "Model_BN_" + str(trainModelLoad) + ".pth"))
        model.load_state_dict(torch.load(modelSavePath + "Model_" + str(trainModelLoad) + ".pth"))
    else:
        for m in model.modules():
            if isinstance(m, nn.Conv2d):
                torch.nn.init.xavier_normal_(m.weight)
                torch.nn.init.constant_(m.bias, 0.)
            if isinstance(m, nn.Linear):
                torch.nn.init.xavier_normal_(m.weight)
                torch.nn.init.constant_(m.bias, 0.)
    ### Train or Test
    if ifTrain:
        model.train()
        trainingTimes = 0
        for e in range(epoch):
            for times , (images, labels) in enumerate(dataLoader,start=1):
                #print(images[0])
                imagesCuda = images.float().to(device,non_blocking = True)
                labelsCuda = labels.long().to(device,non_blocking = True)
                predict = model(imagesCuda)
                loss = lossCri(predict,target = labelsCuda)
                optimizer.zero_grad()
                loss.backward()
                optimizer.step()
                if trainingTimes % displayTimes == 0:
                    print("#########")
                    print("Predict is : ",predict[0:3])
                    print("Labels are : ",labelsCuda[0:3])
                    print("Learning rate is ", optimizer.state_dict()['param_groups'][0]["lr"])
                    print("Loss is ", loss)
                    print("Epoch : ", e)
                    print("Training time is ", trainingTimes)
                trainingTimes += 1
            learning_rate = learning_rate * math.pow(decayRate, trainingTimes / decayStep + 0.)
            state_dic = optimizer.state_dict()
            state_dic["param_groups"][0]["lr"] = learning_rate
            optimizer.load_state_dict(state_dic)
            torch.save(model.state_dict(), modelSavePath + "Model_" + str(e) + ".pth")
    else:
        model.eval()
        model.load_state_dict(torch.load(modelSavePath + "Model_" + str(testModelLoad) + ".pth"))
        predictList = []
        truthList = []
        k = 0
        for testImage, testTarget in testingSet:
            predictTensor = model(testImage.unsqueeze(0).float().to(device)).cpu().detach().numpy()
            position = np.argmax(np.squeeze(predictTensor))
            if k % displayTimes == 0:
                print("##############" + str(k))
                print(position)
                print(testTarget)
                predictList.append(position)
                truthList.append(testTarget)
            k += 1
        acc = metrics.accuracy_score(y_true=truthList,y_pred=predictList)
        classifiedInfor = metrics.classification_report(y_true=truthList,y_pred=predictList)
        macroF1 = metrics.f1_score(y_true=truthList,y_pred=predictList,average="macro")
        microF1 = metrics.f1_score(y_true=truthList,y_pred=predictList,average="micro")
        F1 = metrics.f1_score(y_true=truthList,y_pred=predictList)
        confusionMatrix = metrics.confusion_matrix(y_true=truthList,y_pred=predictList)
        print("The accuracy is : ",acc)
        print("The classified result is : ")
        print(classifiedInfor)
        print("The macro F1 is : ",macroF1)
        print("The micro F1 is : ",microF1)
        print(confusionMatrix)
        print(F1)