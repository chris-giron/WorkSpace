import gym
import numpy as np
from gym import wrappers
import random


def e_greedy(Q=np.zeros((500, 6)), state=0, epsilon=0.2):
    xi = random.random()
    if xi < epsilon:
        a = random.randint(0, Q.shape[1]-1)
    else:
        a = Q.argmax(axis=1)[state]
    return a


def moving100avg(all_rewards, episode):
    if episode <= 100:
        return all_rewards[:episode].mean()
    else:
        return all_rewards[episode-100:episode].mean()


def qlearn():
    env = gym.make('Taxi-v1')
    max_steps = env.spec.tags.get('wrapper_config.TimeLimit.max_episode_steps')
    numActions = env.action_space.n
    numStates = env.observation_space.n
    env = wrappers.Monitor(env, '/tmp/taxi-experiment-1', force=True)
    alpha_start = 0.7
    gamma = 0.9
    epsilon_start = 0.8
    epsilon_iter_end = 10
    episode = 1
    dim = (numStates, numActions)
    Q = np.zeros(dim)
    all_rewards = np.zeros(1000000)
    while moving100avg(all_rewards, episode) < 9.7 and episode < 100000:
        print("Episode: {} --- moving100avg = {}".format(episode, moving100avg(all_rewards, episode)))
        state = env.reset()
        total_rewards = 0
        done = False
        step = 1
        if episode < epsilon_iter_end:
            # epsilon = (epsilon_start / epsilon_iter_end ** 2) * (episode - epsilon_iter_end) ** 2
            # epsilon = epsilon ** 2
            epsilon = epsilon_start
            alpha = alpha_start
        elif episode < epsilon_iter_end * 10:
            epsilon = epsilon_start / 2
        elif episode < epsilon_iter_end * 15:
            epsilon = epsilon_start / 4
            alpha = alpha_start
        else:
            epsilon = 0.0
        while step <= max_steps and done is False:
            action = e_greedy(Q, state, epsilon)
            next_state, reward, done, info = env.step(action)
            if done is True:
                Q_prime = 0
            else:
                Q_prime = Q.max(axis=1)[next_state]
            total_rewards += reward
            Q[state, action] += alpha * (reward + gamma * Q_prime - Q[state, action])
            state = next_state
            step += 1
        all_rewards[episode] = total_rewards
        episode += 1
    return Q


def sarsa():
    env = gym.make('Taxi-v1')
    max_steps = env.spec.tags.get('wrapper_config.TimeLimit.max_episode_steps')
    numActions = env.action_space.n
    numStates = env.observation_space.n
    env = wrappers.Monitor(env, '/tmp/taxi-experiment-1', force=True)
    alpha_start = 0.1
    gamma = 0.9
    epsilon_start = 0.8
    epsilon_iter_end = 20000
    episode = 1
    dim = (numStates, numActions)
    Q = np.zeros(dim)
    all_rewards = np.zeros(1000000)
    while moving100avg(all_rewards, episode) < 9.7 and episode < 100000:
        print("Episode: {} --- moving100avg = {}".format(episode, moving100avg(all_rewards, episode)))
        state = env.reset()
        total_rewards = 0
        done = False
        step = 1
        if episode < epsilon_iter_end:
            # epsilon = (epsilon_start / epsilon_iter_end ** 2) * (episode - epsilon_iter_end) ** 2
            # epsilon = epsilon ** 2
            epsilon = epsilon_start
            alpha = alpha_start
        elif episode < epsilon_iter_end + 10000:
            epsilon = 0.0
        elif episode < epsilon_iter_end + 15000:
            epsilon = 0.0
        else:
            epsilon = 0.0
        action = e_greedy(Q, state, epsilon)
        while step <= max_steps and done is False:
            next_state, reward, done, info = env.step(action)
            next_action = e_greedy(Q, next_state, epsilon)
            if done is True:
                Q_prime = 0
            else:
                Q_prime = Q[next_state, next_action]
            total_rewards += reward
            Q[state, action] += alpha * (reward + gamma * Q_prime - Q[state, action])
            state = next_state
            action = next_action
            step += 1
        all_rewards[episode] = total_rewards
        episode += 1
    return Q


# Q = qlearn()
Q = sarsa()
ans1 = -11.3744; ans2 = 4.3489; ans3 = -0.5857; ans4 = 9.683; ans5 = -12.8233
Q1 = round(Q[462, 4], 4); Q2 = round(Q[398, 3], 4); Q3 = round(Q[253, 0], 4); Q4 = round(Q[377, 1], 4); Q5 = round(Q[83, 5], 4)

print('{} --> {}'.format(ans1, Q1))
print('{} --> {}'.format(ans2, Q2))
print('{} --> {}'.format(ans3, Q3))
print('{} --> {}'.format(ans4, Q4))
print('{} --> {}'.format(ans5, Q5))

if Q1 == ans1 and Q2 == ans2 and Q3 == ans3 and Q4 == ans4 and Q5 == ans5:
    print('YOU FUCKING DID IT CHRIS!!')
    print('YOU ARE A GENIUS AND YOU ALWAYS WILL BE')
    print('A MODERN DAY EINSTEIN AND EVERYBODY LOVES YOU!')
    print('GRAB YOURSELF A BEER! YOU EARNED IT!')
else:
    print('YOU ARE A FUCKING IDIOT...')
    print('YOU ARE A DISGRACE TO YOUR FAMILY NAME...')
    print('NOBODY LOVES YOU...')
    print('SERIOUSLY JUST GIVE UP ON LIFE')
