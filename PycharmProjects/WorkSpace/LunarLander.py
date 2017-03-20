import numpy as np
from keras.models import Sequential
from keras.layers import Dense
from keras.optimizers import *
import random
import math
import gym
from gym import wrappers

capacity = 100000
lr = 0.00025
batchSize = 64
gamma = 0.99
epsMax = 1
epsMin = 0.01
epsDecay = 0.00001


class RLearner:
    steps = 0
    epsilon = epsMax

    def __init__(self, numStates, numActions):
        self.numStates = numStates
        self.numActions = numActions

        self.NN = NN(numStates, numActions)
        self.memory = Memory(capacity)

    def expReplay(self):
        batch = self.memory.sample(batchSize)
        batchLen = len(batch)
        no_state = np.zeros(self.numStates)
        states = np.array([obs[0] for obs in batch])
        next_states = np.array([(no_state if obs[3] is None else obs[3]) for obs in batch])
        predStates = rlearner.NN.predict(states)
        predNextStates = rlearner.NN.predict(next_states)
        x = np.zeros((batchLen, self.numStates))
        y = np.zeros((batchLen, self.numActions))
        for i in range(batchLen):
            obs = batch[i]
            s = obs[0]; a = obs[1]; r = obs[2]; sNext = obs[3]
            transition = predStates[i]
            if sNext is None:
                transition[a] = r
            else:
                transition[a] = r + gamma * np.amax(predNextStates[i])
            x[i] = s; y[i] = transition
        self.NN.train(x, y)

    def act(self, s):
        if random.random() < self.epsilon:
            return random.randint(0, self.numActions - 1)
        else:
            return np.argmax(self.NN.predictAction(s))

    def observe(self, sample):
        self.memory.add(sample)
        self.steps += 1
        self.epsilon = epsMin + (epsMax - epsMin) * math.exp(-epsDecay * self.steps)
        # print(self.epsilon)

    def expReplay(self):
        batch = self.memory.sample(batchSize)
        batchLen = len(batch)
        no_state = np.zeros(self.numStates)
        states = np.array([obs[0] for obs in batch])
        next_states = np.array([(no_state if obs[3] is None else obs[3]) for obs in batch])
        predStates = rlearner.NN.predict(states)
        predNextStates = rlearner.NN.predict(next_states)
        x = np.zeros((batchLen, self.numStates))
        y = np.zeros((batchLen, self.numActions))
        for i in range(batchLen):
            obs = batch[i]
            s = obs[0]; a = obs[1]; r = obs[2]; sNext = obs[3]
            transition = predStates[i]
            if sNext is None:
                transition[a] = r
            else:
                transition[a] = r + gamma * np.amax(predNextStates[i])
            x[i] = s; y[i] = transition
        self.NN.train(x, y)


class NN:
    def __init__(self, numStates, numActions):
        self.numStates = numStates
        self.numActions = numActions
        self.model = self._makeModel()
        self.model.load_weights("model.h5")

    def train(self, x, y, epoch=1, verbose=0):
        self.model.fit(x, y, batch_size=64, epochs=epoch, verbose=verbose)

    def predict(self, s):
        return self.model.predict(s)

    def predictAction(self, s):
        return self.predict(s.reshape(1, self.numStates)).flatten()

    def _makeModel(self):
        model = Sequential()
        model.add(Dense(output_dim=(self.numStates ** 3), activation='relu', input_dim=numStates))
        model.add(Dense(output_dim=numActions, activation='linear'))
        opt = RMSprop(lr=lr)
        model.compile(loss='mse', optimizer=opt)
        return model


class Memory:
    samples = []

    def __init__(self, capacity):
        self.capacity = capacity

    def sample(self, n):
        n = min(n, len(self.samples))
        return random.sample(self.samples, n)

    def add(self, sample):
        self.samples.append(sample)
        if len(self.samples) > self.capacity:
            self.samples.pop(0)


class Environment:
    def __init__(self, test):
        self.test = test
        self.env = gym.make(test)
        #self.env = wrappers.Monitor(gym.make(test), '/tmp/env_loc', force=True)

    def DQN(self, rlearner):
        s = self.env.reset()
        totalReward = 0
        while True:
            # self.env.render()
            a = rlearner.act(s)
            sNext, r, done, info = self.env.step(a)
            if done:
                sNext = None
            rlearner.observe((s, a, r, sNext))
            rlearner.expReplay()
            s = sNext; totalReward += r
            if done:
                break
        print("Total Reward = {}".format(totalReward))

# test = 'CartPole-v0'
test = 'LunarLander-v2'
env = Environment(test)
numStates = env.env.observation_space.shape[0]
numActions = env.env.action_space.n
rlearner = RLearner(numStates, numActions)
try:
    while True:
        env.DQN(rlearner)
finally:
    # rlearner.NN.model.save("model.h5")
    print("Done")