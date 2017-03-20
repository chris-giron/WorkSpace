import gym
import math
import argparse
import numpy as np
from gym import wrappers
from gym.spaces import Box, Discrete
import random
from keras.models import Sequential, Model
from keras.layers import Dense, Activation, Flatten, Input, Lambda
from keras.optimizers import *
from keras.layers.normalization import BatchNormalization
from keras import backend as K

MAX_EPSILON = 1
MIN_EPSILON = 0.01
LAMBDA = 0.001
GAMMA = 0.99
capacity = 100000
BATCH_SIZE = 64

def eGreedy(s, e):
    if random.random() < e:
        return random.randint(0, numActions - 1)
    else:
        return np.argmax(model.predict(s.reshape(1, stateCnt)).flatten())

class Memory:
    samples = []

    def __init__(self, capacity):
        self.capacity = capacity

    def add(self, sample):
        self.samples.append(sample)

        if len(self.samples) > self.capacity:
            self.samples.pop(0)

    def sample(self, n):
        n = min(n, len(self.samples))
        return random.sample(self.samples, n)

memory = Memory(capacity)

ENV_NAME = 'CartPole-v0';ENV_LOC = '/tmp/cartpole'
#ENV_NAME = 'LunarLander-v2';ENV_LOC = '/tmp/lunar-lander'
env = gym.make(ENV_NAME)
# env = wrappers.Monitor(env, ENV_LOC, force=True)
numActions = env.action_space.n
maxSteps = env.spec.tags.get('wrapper_config.TimeLimit.max_episode_steps')
stateCnt = env.observation_space.shape[0]
model = Sequential()
model.add(Dense(output_dim=64, activation='relu', input_dim=stateCnt))
# model.add(Dense(20, activation='tanh', init='uniform'))
model.add(Dense(output_dim=numActions, activation='linear'))
opt = RMSprop(lr=0.001)
model.compile(loss='mse', optimizer=opt)
# model.load_weights("model.h5")
Episode = 0
steps = 0
e = MAX_EPSILON

try:
    while True:
        Episode += 1
        s = env.reset()
        totalReward = 0

        while True:
            print(s)
            a = eGreedy(s, e)
            s_, r, done, info = env.step(a)
            print("{} {} {} {} {}".format(s, a, r, s_, done))
            if done:
                s_ = None
            memory.add((s, a, r, s_))
            steps += 1
            e = MIN_EPSILON + (MAX_EPSILON - MIN_EPSILON) * math.exp(-LAMBDA * steps)
            batch = memory.sample(BATCH_SIZE)
            batchLen = len(batch)
            no_state = np.zeros(stateCnt)
            states = np.array([o[0] for o in batch])
            print(states)
            print(batchLen)
            states_ = np.array([(no_state if o[3] is None else o[3]) for o in batch])
            p = model.predict(states)
            p_ = model.predict(states_)
            x = np.zeros((batchLen, stateCnt))
            y = np.zeros((batchLen, numActions))
            for i in range(batchLen):
                o = batch[i]
                s = o[0]; a = o[1]; r = o[2]; s_ = o[3]
                t = p[i]
                if s_ is None:
                    t[a] = r
                else:
                    t[a] = r + GAMMA * np.amax(p_[i])
                x[i] = s
                y[i] = t
            model.fit(x, y, batch_size=64, epochs=1, verbose=0)
            print(steps)
            print(s)
            s = s_
            print(s)
            totalReward += r
            if done:
                break
        print("Episode: {} --- Reward: {} --- Epsilon: {}".format(Episode, totalReward, e))
finally:
        model.save("model.h5")



