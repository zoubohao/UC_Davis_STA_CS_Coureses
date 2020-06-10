import pyreadr
import torch
from torch.utils.data import Dataset , DataLoader
from sklearn.model_selection import train_test_split
import torch.nn as nn
import numpy as np
import torch.nn.functional as F

class HW3_Data_set(Dataset):

    def __init__(self,dataset,dataset_labels):
        super().__init__()
        self.set = torch.from_numpy(dataset)
        self.set_labels = torch.from_numpy(dataset_labels)

    def __len__(self):
        return len(self.set_labels)

    def __getitem__(self, item):
        return self.set[item] , self.set_labels[item]


def rbf(dataset, vector,sigma):
    """
    :param dataset: [n,d]
    :param vector: [b,d]
    :param sigma : rbf sigma, must be positive
    :return: [b,n]
    """
    b = vector.shape[0]
    d = vector.shape[1]
    vectors = torch.chunk(vector,dim=0,chunks=b)
    batchDis = []
    for oneVec in vectors:
        # print("dataset",dataset)
        # print("x",oneVec)
        # print(dataset - oneVec.view(1,d))
        dis = torch.sum(torch.pow(dataset - oneVec.view(1,d), 2.), dim=1, keepdim=False)
        batchDis.append(dis)
    #print("before div sigma",-torch.stack(batchDis,dim=0))
    #print("After div sigma", -torch.stack(batchDis,dim=0) / (2. * sigma))
    #print("Final result " , torch.exp(-torch.stack(batchDis,dim=0) / (2. * sigma)))
    return torch.exp(-torch.stack(batchDis,dim=0) / (2. * sigma))


class SVM(nn.Module):

    def __init__(self,data_set, data_labels,sigma):
        """
        :param data_set: [n,d]
        :param data_labels: 1,-1 set [n]
        """
        super().__init__()
        self.data_set = torch.from_numpy(data_set).float()
        self.data_labels = torch.from_numpy(data_labels).float()
        self.n = self.data_set.size()[0]
        self.lambdas = nn.Parameter(torch.ones(size=[self.n],requires_grad=True).float(),requires_grad=True)
        self.b = nn.Parameter(torch.ones(1,requires_grad=True).float(),requires_grad=True)
        self.sigma2 = np.square(sigma)

    def forward(self,x):
        ## return [b]
        lambdas = F.relu(self.lambdas)
        result = self.data_labels.view(1,self.n) * lambdas.view(1,self.n) * rbf(self.data_set,x,self.sigma2)
        #print("Lambda ",lambdas)
        #print("Before sign ",torch.sum(result,dim=1) + self.b)
        return torch.tanh(torch.sum(result,dim=1) + self.b)



class LogisticRegression(nn.Module):

    def __init__(self, input_dim, output_dim):

        super().__init__()
        self.linear = nn.Linear(input_dim, output_dim)

    def forward(self, x):
        out = self.linear(x)
        return out


class NeuralNet(nn.Module):
    def __init__(self, input_size, hidden_size, num_classes):
        super(NeuralNet, self).__init__()
        self.fc1 = nn.Linear(input_size, hidden_size)
        self.relu = nn.ReLU()
        self.fc2 = nn.Linear(hidden_size, hidden_size)
        self.relu = nn.ReLU()
        self.fc3 = nn.Linear(hidden_size, num_classes)

    def forward(self, x):
        out = self.fc1(x)
        out = self.relu(out)
        out = self.fc2(out)
        out = self.relu(out)
        out = self.fc3(out)
        return out

def train(model,lossCri,dataLoader,testDataLoader,epoch,weight_decay,lr,device = "cuda" if torch.cuda.is_available() else "cpu"):
    model = model.to(device)
    optimizer = torch.optim.SGD(model.parameters(),lr=lr,weight_decay=weight_decay,nesterov=True,momentum=0.9)
    lossCri = lossCri.to(device)
    for e in range(epoch):
        finalLoss = 0
        for times, (trainRows, trainLabels) in enumerate(dataLoader):
            trainRows = trainRows.float().to(device)
            trainLabels = trainLabels.long().to(device)
            optimizer.zero_grad()
            trainOutput = model(trainRows)
            loss = lossCri(trainOutput,trainLabels)
            loss.backward()
            optimizer.step()
            finalLoss = loss
        model = model.eval()
        # Calculate Accuracy
        correct = 0
        total = 0
        # Iterate through test dataset
        for testRows, testLabels in testDataLoader:
            testLabels = testLabels.to(device)
            # Forward pass only to get logits/output
            testOutputs = model(testRows.float().to(device))

            # Get predictions from the maximum value
            _, predicted = testOutputs.max(1)

            # Total number of labels
            total += testLabels.size(0)

            # Total correct predictions
            correct += predicted.eq(testLabels).sum().item()

        accuracy = 100 * correct / total

        # Print Loss
        print('Epoch: {}. Loss : {}.  Accuracy In Test Data set: {}'.format(e,finalLoss, accuracy))
        model = model.train(True)
    return model

def train_svm(model,dataLoader,testDataLoader,epoch,lr,weight_decay,device = "cuda" if torch.cuda.is_available() else "cpu"):
    model = model.to(device)
    optimizer = torch.optim.SGD(model.parameters(),lr=lr,weight_decay=weight_decay,nesterov=True,momentum=0.9)
    lossCri = nn.MSELoss().to(device)
    for e in range(epoch):
        for times, (trainRows, trainLabels) in enumerate(dataLoader):
            #print(times)
            trainRows = trainRows.float().to(device)
            trainLabels = trainLabels.float().to(device)
            optimizer.zero_grad()
            trainOutput = model(trainRows)
            loss = lossCri(trainOutput,trainLabels)
            # print("##########")
            # print(trainOutput)
            # print(trainLabels)
            # print(loss)
            loss.backward()
            optimizer.step()
        model = model.eval()
        # Calculate Accuracy
        correct = 0
        total = 0
        # Iterate through test dataset
        for testRows, testLabels in testDataLoader:
            testLabels = testLabels.to(device)
            # Forward pass only to get logits/output
            testOutputs = model(testRows.float().to(device))

            # Get predictions from the maximum value
            b = testOutputs.size(0)
            predicted = torch.where(testOutputs >= 0.,torch.ones(size=[b]),torch.zeros(size=[b])-1)
            total += testLabels.size(0)

            # Total correct predictions
            correct += predicted.eq(testLabels).sum().item()

        accuracy = 100 * correct / total

        # Print Result
        print('Epoch: {}.  Accuracy In Test Data set: {}%'.format(e, accuracy))
        model = model.train(True)
    return model

if __name__ == "__main__":
    data = pyreadr.read_r("./DeepFeature.RData")


    deep_features = np.transpose(np.array(data["deep.feature"]).reshape([-1,2000]))
    image_array = np.transpose(np.array(data["image.array"]).reshape([-1,2000]))
    labelsChar = np.array(data["label"]).reshape([2000])
    labels01 = np.array(labelsChar == "cat",dtype=np.float32)
    labels1_1 = np.where(labelsChar == "cat",-1,1)

    # print(deep_features)
    # print(image_array)

    dp_train, dp_test, dp_label_train, dp_label_test = \
        train_test_split(deep_features, labels01, test_size=0.30)

    ia_train, ia_test, ia_label_train, ia_label_test = \
        train_test_split(image_array, labels01, test_size=0.30)

    dp_D = dp_train.shape[1]
    ia_D = ia_train.shape[1]

    # print(labels01)
    # print(labels1_1)

    #
    # ## train dataset
    # dp_train_set = HW3_Data_set(dataset=dp_train,dataset_labels=dp_label_train)
    # ia_train_set = HW3_Data_set(dataset=ia_train / 255.,dataset_labels=ia_label_train)
    #
    # ## test dataset
    # dp_test_set = HW3_Data_set(dataset=dp_test,dataset_labels=dp_label_test)
    # ia_test_set = HW3_Data_set(dataset=ia_test / 255.,dataset_labels=ia_label_test)
    #
    # ## data loader
    # dp_train_loader = DataLoader(dp_train_set,batch_size=25,shuffle=True,num_workers=2)
    # ia_train_loader = DataLoader(ia_train_set,batch_size=25,shuffle=True,num_workers=2)
    #
    # dp_test_loader = DataLoader(dp_test_set,batch_size=25,shuffle=True,num_workers=2)
    # ia_test_loader = DataLoader(ia_test_set,batch_size=25,shuffle=True,num_workers=2)
    #
    # ### logistic regression deep.feature
    # print("Logistic regression for deep.feature")
    # logisticR = LogisticRegression(dp_D,2)
    # lossCriL = nn.CrossEntropyLoss()
    # train(logisticR,lossCriL,dp_train_loader,dp_test_loader,epoch=15,weight_decay=5e-4,lr= 0.0001)
    #
    # ### logistic regression image.array
    # print("Logistric regression for image.array")
    # logisticR = LogisticRegression(ia_D,2)
    # lossCriL = nn.CrossEntropyLoss()
    # train(logisticR,lossCriL,ia_train_loader,ia_test_loader,epoch=15,weight_decay=5e-4,lr= 0.0001)
    #
    # ### Neural net for deep.feature
    # print("Neural net for deep.feature")
    # NNt = NeuralNet(dp_D,hidden_size=2000,num_classes=2)
    # lossCriL = nn.CrossEntropyLoss()
    # train(NNt,lossCriL,dp_train_loader,dp_test_loader,epoch=25,weight_decay=5e-4,lr= 0.0001)


    ### Neural net for image.array
    # print("Neural net for image.array")
    # NNt = NeuralNet(ia_D,hidden_size=2000,num_classes=2)
    # lossCriL = nn.CrossEntropyLoss()
    # train(NNt,lossCriL,ia_train_loader,ia_test_loader,epoch=25,weight_decay=5e-4,lr= 0.0001)

    ### svm data loader construct
    dp_train_svm, dp_test_svm, dp_label_train_svm, dp_label_test_svm = \
        train_test_split(deep_features, labels1_1, test_size=0.30)

    ia_train_svm, ia_test_svm, ia_label_train_svm, ia_label_test_svm = \
        train_test_split(image_array, labels1_1, test_size=0.30)

    ## train dataset
    dp_train_set_svm = HW3_Data_set(dataset=dp_train_svm,dataset_labels=dp_label_train_svm)
    ia_train_set_svm = HW3_Data_set(dataset=ia_train_svm / 255.,dataset_labels=ia_label_train_svm)

    ## test dataset
    dp_test_set_svm = HW3_Data_set(dataset=dp_test_svm,dataset_labels=dp_label_test_svm)
    ia_test_set_svm = HW3_Data_set(dataset=ia_test_svm / 255.,dataset_labels=ia_label_test_svm)

    ## data loader
    dp_train_loader_svm = DataLoader(dp_train_set_svm,batch_size=1,shuffle=True)
    ia_train_loader_svm = DataLoader(ia_train_set_svm,batch_size=1,shuffle=True)

    dp_test_loader_svm = DataLoader(dp_test_set_svm,batch_size=1,shuffle=False)
    ia_test_loader_svm = DataLoader(ia_test_set_svm,batch_size=1,shuffle=False)

    ## SVM for deep.feature
    # print("SVM for deep.feature.")
    # svm = SVM(dp_train_svm,dp_label_train_svm,sigma=36)
    # train_svm(svm,dp_train_loader_svm,dp_test_loader_svm,epoch=25,lr = 0.0001,weight_decay=0)

    # ### SVM for image.array
    print("SVM for image.array.")
    svm = SVM(ia_train_svm / 255.,ia_label_train_svm,sigma=18)
    train_svm(svm,ia_train_loader_svm,ia_test_loader_svm,epoch=25,lr = 0.0001,weight_decay=0)

