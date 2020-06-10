import pyreadr
import torch
from torch.utils.data import Dataset , DataLoader
from sklearn.model_selection import train_test_split
import torch.nn as nn
import numpy as np


testA = torch.randn(size=[5000,10]).float()
testB = torch.randn(size=[25,10]).float()

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
    print(len(vectors))
    batchDis = []
    for oneVec in vectors:
        dis = torch.sum(torch.pow(dataset - oneVec.view(1,d), 2), dim=1, keepdim=False)
        batchDis.append(dis)
    return torch.exp(-torch.stack(batchDis,dim=0) / (2. * sigma))

print(rbf(testA,testB,1.).shape)

class SVM(nn.Module):

    def __init__(self,data_set, data_labels,sigma):
        """
        :param data_set: [n,d]
        :param data_labels: 1,-1 set [d]
        """
        super().__init__()
        self.data_set = torch.from_numpy(data_set).float()
        self.data_labels = torch.from_numpy(data_labels).float()
        self.n = self.data_set.size()[0]
        self.lambdas = nn.Parameter(torch.ones(self.n).float(),requires_grad=True)
        self.b = nn.Parameter(torch.zeros(1).float(),requires_grad=True)
        self.sigma2 = np.square(sigma)

    def forward(self,x):
        ## return [b]
        result = self.data_labels.view(1,self.n) * self.lambdas.view(1,self.n) * rbf(self.data_set,x,self.sigma2)
        return torch.tanh(torch.sum(result,dim=1) + self.b)

testRandom = torch.randn(size=[4]).float()
testRes = torch.where(testRandom >= 0.,torch.ones(size=[4]),torch.zeros(size=[4])-1)
print(testRandom)
print(testRes)

print(torch.ones(size=[4,3]) * torch.from_numpy(np.array([1,0,1]).reshape([1,3])))
