# install.packages("Rglpk", repos="http://cran.us.r-project.org")
# library(Rglpk)
# library(ggplot2)

# new.game returns initialized state (list: $B.pos, $A.pos, $A.ball (boolean)) from Figure 4 in the Greenwald-Hall paper
new.game <- function(){
  return(list(B.pos=3,A.pos=5,A.ball=FALSE))
}

# show.state returns grid world matrix of inputed soccer field state
# 1 -> A without the ball; 2 -> B without the ball
# 11 -> A with the ball; 22 -> B with the ball
show.state <- function(state){
  if(state$A.ball){
    A = 11; B = 2
  } else {
    A = 1; B = 22
  }
  mat = c(0,0,0,0,0,0,0,0)
  mat[state$A.pos] = A
  mat[state$B.pos] = B
  return(matrix(mat,nrow=2))
}

# act inputs the player committing the action, the current state, and the action the player will commit
# act outputs the new.state and the rewards for player A and B ($new.state, $A.reward, $B.reward)
# Actions: 1 (stay), 2 (north), 3 (east), 4 (south), and 5 (west)
# Example:
# result = act("A",state,4)
# new.state = result$new.state
# A.reward = result$A.reward
# B.reward = result$B.reward
act <- function(player,state,action){
  if(player == "A"){self.pos = state$A.pos; opp.pos = state$B.pos; has.ball = state$A.ball
  } else {self.pos = state$B.pos; opp.pos = state$A.pos; has.ball = !(state$A.ball)}
  # print("#####")
  # print(paste("player =",player))
  # print(paste("self.pos =",self.pos))
  # print(paste("opp.pos =",opp.pos))
  # print(paste("has.ball =",has.ball))
  A.reward = 0
  B.reward = 0
  if(action == 3 && self.pos <= 6){
    self.new.pos = self.pos + 2
  } else if(action == 5 && self.pos >= 3){
    self.new.pos = self.pos -2
  } else if(action == 2 && (self.pos %% 2) == 0){
    self.new.pos = self.pos - 1
  } else if(action == 4 && (self.pos %% 2) == 1){
    self.new.pos = self.pos + 1
  } else {self.new.pos = self.pos}
  if(self.new.pos == opp.pos){
    self.new.pos = self.pos
    if(has.ball){has.ball = FALSE}
  }
  if(player == "A"){new.state = list(B.pos = opp.pos, A.pos = self.new.pos, A.ball = has.ball)
  } else {new.state = list(B.pos = self.new.pos, A.pos = opp.pos, A.ball = !has.ball)}
  if(player == "A" && ((self.new.pos <= 2 && has.ball) || (opp.pos <= 2 && !has.ball))){
    A.reward = 100
    B.reward = -100
  }
  if(player == "B" && ((self.new.pos >= 7 && has.ball) || (opp.pos >= 7 && !has.ball))){
    A.reward = -100
    B.reward = 100
  }
  if(player == "A" && self.new.pos >= 7 && has.ball){
    A.reward = -100
    B.reward = 100
  }
  if(player == "B" && self.new.pos <= 2 && has.ball){
    A.reward = 100
    B.reward = -100
  }
  # print(paste("A.reward =",A.reward))
  # print("#####")
  return(list(new.state=new.state, A.reward=A.reward, B.reward=B.reward))
}

e_greedy <- function(Q.A, Q.B, eps, state){
  xi = runif(1)
  if(xi < eps){
    action.A = sample(dim(Q.A)[4],1)
  } else {
    action.A = which.max(row_means(Q.A[state$A.pos, state$B.pos, as.numeric(state$A.ball)+1, , ]))
  }
  xi = runif(1)
  if(xi < eps){
    action.B = sample(dim(Q.B)[5],1)
  } else {
    action.B = which.max(col_means(Q.B[state$A.pos, state$B.pos, as.numeric(state$A.ball)+1, , ]))
  }
  return(list(action.A=action.A, action.B=action.B))
}

e_greedy_Q <- function(Q, eps, state){
  xi = runif(1)
  if(xi < eps){
    action = sample(dim(Q)[4],1)
  } else {
    action = which.max(Q[state$A.pos, state$B.pos, as.numeric(state$A.ball)+1, ])
  }
  return(action)
}

Q.learn <- function(state = new.state, player.Q, opp.Q){
  A.pos = state$A.pos
  B.pos = state$B.pos
  A.ball = state$A.ball
  V = max(player.Q[A.pos, B.pos, as.numeric(A.ball)+1, , ])
  return(V)
}

qlearn <- function(state = new.state, Q){
  A.pos = state$A.pos
  B.pos = state$B.pos
  A.ball = state$A.ball
  V = max(Q[A.pos, B.pos, as.numeric(A.ball)+1, ])
  return(V)
}

friend.val <- function(state = new.state, player.Q, opp.Q, player){
  A.pos = state$A.pos
  B.pos = state$B.pos
  A.ball = state$A.ball
  V = max(player.Q[A.pos, B.pos, as.numeric(A.ball)+1, , ])
  return(V)
}

foe.val <- function(state = new.state, player.Q, opp.Q, player){
  A.pos = state$A.pos
  B.pos = state$B.pos
  A.ball = state$A.ball
  obj <- c(0,0,0,0,0,1)
  if(player == "A"){
    mat <- rbind(cbind(player.Q[A.pos, B.pos, as.numeric(A.ball)+1, , ],rep(-1,times=5)),c(1,1,1,1,1,0))
  } else{
    mat <- rbind(cbind(t(player.Q[A.pos, B.pos, as.numeric(A.ball)+1, , ]),rep(-1,times=5)),c(1,1,1,1,1,0))
  }
  dir <- c(">=",">=",">=",">=",">=","==")
  rhs <- c(0,0,0,0,0,1)
  max <- TRUE
  # print(mat)
  lp = Rglpk_solve_LP(obj,mat,dir,rhs,max = max)
  V = lp$optimum
  # print(V)
  return(V)
}

ce.val <- function(state = new.state, player.Q, opp.Q, player){
  A.pos = state$A.pos
  B.pos = state$B.pos
  A.ball = state$A.ball
  obj <- c(0,0,0,0,0,1)
  if(player == "A"){
    mat <- rbind(cbind((player.Q[A.pos, B.pos, as.numeric(A.ball)+1, , ]+
                          opp.Q[A.pos, B.pos, as.numeric(A.ball)+1, , ]),rep(-1,times=5)),c(1,1,1,1,1,0))
  } else{
    mat <- rbind(cbind((t(player.Q[A.pos, B.pos, as.numeric(A.ball)+1, , ])+
                            t(opp.Q[A.pos, B.pos, as.numeric(A.ball)+1, , ])),rep(-1,times=5)),c(1,1,1,1,1,0))
  }
  dir <- c(">=",">=",">=",">=",">=","==")
  rhs <- c(0,0,0,0,0,1)
  max <- TRUE
  # print(mat)
  lp = Rglpk_solve_LP(obj,mat,dir,rhs,max = max)
  V = lp$optimum
  # print(V)
  return(V)
}

# val.fun(Q, s,)
# multi.Q inputs Q.learn value function with hyperparameters and updated Q.A and Q.B matrices
# multi.Q outputs newly updated Q.A and Q.B matrics
multi.Q <- function(val.fun = Q.learn, gamma = 0.9, alpha = 1, eps = 0.2, S = 0.99999, iter = 1e6){
  Q.A = array(100, dim = c(8,8,2,5,5))
  Q.B = array(100, dim = c(8,8,2,5,5))
  ERR = rep(0,times = 0)
  state = new.game()
  A.pos = state$A.pos; B.pos = state$B.pos; A.ball = state$A.ball
  actions = e_greedy(Q.A, Q.B, eps, state)
  action.A = actions$action.A
  action.B = actions$action.B
  for(t in seq(1:iter)){
    # print("========================================")
    print(paste("t =",t))
    # Randomly Decide who goes first
    if(runif(1) <= 0.5){
      first.player = "A" ; last.player = "B"; first.action = action.A; last.action = action.B
    } else {
      first.player = "B"; last.player = "A"; first.action = action.B; last.action = action.A
    }
    new.Q.A = Q.A
    new.Q.B = Q.B
    # print(show.state(state))
    # print(paste("first.player =",first.player))
    # print(paste("first.action =",first.action))
    result.1 = act(first.player, state, first.action)
    new.state = result.1$new.state
    # print(paste("result.1$A.reward =",result.1$A.reward))
    A.reward = result.1$A.reward
    B.reward = result.1$B.reward
    # print(paste("A.reward =",A.reward))
    # print(paste("A.ball =",new.state$A.ball))
    # print(show.state(new.state))
    if(A.reward != 0){
      V = 0
      new.Q.A[A.pos, B.pos, as.numeric(A.ball)+1, action.A, action.B] = 
        (1 - alpha) * Q.A[A.pos, B.pos, as.numeric(A.ball)+1, action.A, action.B] + alpha * ((1 - gamma) * A.reward + gamma * V)
      new.Q.B[A.pos, B.pos, as.numeric(A.ball)+1, action.A, action.B] = 
        (1 - alpha) * Q.B[A.pos, B.pos, as.numeric(A.ball)+1, action.A, action.B] + alpha * ((1 - gamma) * B.reward + gamma * V)
      ERR[t] = abs(new.Q.A[5,3,1,4,1] - Q.A[5,3,1,4,1])
      Q.A = new.Q.A
      Q.B = new.Q.B
      state = new.game()
      A.pos = state$A.pos; B.pos = state$B.pos; A.ball = state$A.ball
      actions = e_greedy(Q.A, Q.B, eps, state)
      action.A = actions$action.A
      action.B = actions$action.B
      alpha = alpha * S
      next
    }
    # print(paste("last.player =",last.player))
    # print(paste("last.action =",last.action))
    result.2 = act(last.player, new.state, last.action)
    new.state = result.2$new.state
    # print(paste("result.2$A.reward =",result.2$A.reward))
    A.reward = result.2$A.reward
    B.reward = result.2$B.reward
    # print(paste("A.reward =",A.reward))
    # print(paste("A.ball =",new.state$A.ball))
    # print(show.state(new.state))
    if(A.reward != 0){
      V = 0
      new.Q.A[A.pos, B.pos, as.numeric(A.ball)+1, action.A, action.B] = 
        (1 - alpha) * Q.A[A.pos, B.pos, as.numeric(A.ball)+1, action.A, action.B] + alpha * ((1 - gamma) * A.reward + gamma * V)
      new.Q.B[A.pos, B.pos, as.numeric(A.ball)+1, action.A, action.B] = 
        (1 - alpha) * Q.B[A.pos, B.pos, as.numeric(A.ball)+1, action.A, action.B] + alpha * ((1 - gamma) * B.reward + gamma * V)
      ERR[t] = abs(new.Q.A[5,3,1,4,1] - Q.A[5,3,1,4,1])
      Q.A = new.Q.A
      Q.B = new.Q.B
      state = new.game()
      A.pos = state$A.pos; B.pos = state$B.pos; A.ball = state$A.ball
      actions = e_greedy(Q.A, Q.B, eps, state)
      action.A = actions$action.A
      action.B = actions$action.B
      alpha = alpha * S
      next
    }
    V.A = val.fun(new.state, Q.A, Q.B, "A")
    V.B = val.fun(new.state, Q.B, Q.A, "B")
    new.Q.A[A.pos, B.pos, as.numeric(A.ball)+1, action.A, action.B] = 
      (1 - alpha) * Q.A[A.pos, B.pos, as.numeric(A.ball)+1, action.A, action.B] + alpha * ((1 - gamma) * A.reward + gamma * V.A)
    new.Q.B[A.pos, B.pos, as.numeric(A.ball)+1, action.A, action.B] = 
      (1 - alpha) * Q.B[A.pos, B.pos, as.numeric(A.ball)+1, action.A, action.B] + alpha * ((1 - gamma) * B.reward + gamma * V.B)
    ERR[t] = abs(new.Q.A[5,3,1,4,1] - Q.A[5,3,1,4,1])
    Q.A = new.Q.A
    Q.B = new.Q.B
    actions = e_greedy(Q.A, Q.B, eps, new.state)
    action.A = actions$action.A
    action.B = actions$action.B
    state = new.state
    A.pos = state$A.pos; B.pos = state$B.pos; A.ball = state$A.ball
    alpha = alpha * S
  }
  # return(list(Q.A = Q.A, Q.B = Q.B, ERR = ERR))
  return(as.data.frame(list(iteration = seq(1,iter), ERR = ERR)))
}

regular.Q <- function(gamma = 0.9, alpha = 1, eps = 0.2, S = 0.99999, iter = 1e6){
  Q.A = array(0, dim = c(8,8,2,5))
  Q.B = array(0, dim = c(8,8,2,5))
  ERR = rep(0,times = 0)
  state = new.game()
  A.pos = state$A.pos; B.pos = state$B.pos; A.ball = state$A.ball
  action.A = e_greedy_Q(Q.A, eps, state)
  action.B = e_greedy_Q(Q.B, eps, state)
  for(t in seq(1:iter)){
    # print("========================================")
    print(paste("t =",t))
    # Randomly Decide who goes first
    if(runif(1) <= 0.5){
      first.player = "A" ; last.player = "B"; first.action = action.A; last.action = action.B
    } else {
      first.player = "B"; last.player = "A"; first.action = action.B; last.action = action.A
    }
    new.Q.A = Q.A
    new.Q.B = Q.B
    # print(show.state(state))
    # print(paste("first.player =",first.player))
    # print(paste("first.action =",first.action))
    result.1 = act(first.player, state, first.action)
    new.state = result.1$new.state
    # print(paste("result.1$A.reward =",result.1$A.reward))
    A.reward = result.1$A.reward
    B.reward = result.1$B.reward
    # print(paste("A.reward =",A.reward))
    # print(paste("A.ball =",new.state$A.ball))
    # print(show.state(new.state))
    if(A.reward != 0){
      V = 0
      new.Q.A[A.pos, B.pos, as.numeric(A.ball)+1, action.A] = 
        (1 - alpha) * Q.A[A.pos, B.pos, as.numeric(A.ball)+1, action.A] + alpha * ((1 - gamma) * A.reward + gamma * V)
      new.Q.B[A.pos, B.pos, as.numeric(A.ball)+1, action.B] = 
        (1 - alpha) * Q.B[A.pos, B.pos, as.numeric(A.ball)+1, action.B] + alpha * ((1 - gamma) * B.reward + gamma * V)
      ERR[t] = abs(new.Q.A[5,3,1,4] - Q.A[5,3,1,4])
      Q.A = new.Q.A
      Q.B = new.Q.B
      state = new.game()
      A.pos = state$A.pos; B.pos = state$B.pos; A.ball = state$A.ball
      action.A = e_greedy_Q(Q.A, eps, state)
      action.B = e_greedy_Q(Q.B, eps, state)
      alpha = alpha * S
      next
    }
    # print(paste("last.player =",last.player))
    # print(paste("last.action =",last.action))
    result.2 = act(last.player, new.state, last.action)
    new.state = result.2$new.state
    # print(paste("result.2$A.reward =",result.2$A.reward))
    A.reward = result.2$A.reward
    B.reward = result.2$B.reward
    # print(paste("A.reward =",A.reward))
    # print(paste("A.ball =",new.state$A.ball))
    # print(show.state(new.state))
    if(A.reward != 0){
      V = 0
      new.Q.A[A.pos, B.pos, as.numeric(A.ball)+1, action.A] = 
        (1 - alpha) * Q.A[A.pos, B.pos, as.numeric(A.ball)+1, action.A] + alpha * ((1 - gamma) * A.reward + gamma * V)
      new.Q.B[A.pos, B.pos, as.numeric(A.ball)+1, action.B] = 
        (1 - alpha) * Q.B[A.pos, B.pos, as.numeric(A.ball)+1, action.B] + alpha * ((1 - gamma) * B.reward + gamma * V)
      ERR[t] = abs(new.Q.A[5,3,1,4] - Q.A[5,3,1,4])
      Q.A = new.Q.A
      Q.B = new.Q.B
      state = new.game()
      A.pos = state$A.pos; B.pos = state$B.pos; A.ball = state$A.ball
      action.A = e_greedy_Q(Q.A, eps, state)
      action.B = e_greedy_Q(Q.B, eps, state)
      alpha = alpha * S
      next
    }
    V.A = qlearn(new.state, Q.A)
    V.B = qlearn(new.state, Q.B)
    new.Q.A[A.pos, B.pos, as.numeric(A.ball)+1, action.A] = 
      (1 - alpha) * Q.A[A.pos, B.pos, as.numeric(A.ball)+1, action.A] + alpha * ((1 - gamma) * A.reward + gamma * V.A)
    new.Q.B[A.pos, B.pos, as.numeric(A.ball)+1, action.B] = 
      (1 - alpha) * Q.B[A.pos, B.pos, as.numeric(A.ball)+1, action.B] + alpha * ((1 - gamma) * B.reward + gamma * V.B)
    ERR[t] = abs(new.Q.A[5,3,1,4] - Q.A[5,3,1,4])
    Q.A = new.Q.A
    Q.B = new.Q.B
    action.A = e_greedy_Q(Q.A, eps, state)
    action.B = e_greedy_Q(Q.B, eps, state)
    state = new.state
    A.pos = state$A.pos; B.pos = state$B.pos; A.ball = state$A.ball
    alpha = alpha * S
  }
  # return(list(Q.A = Q.A, Q.B = Q.B, ERR = ERR))
  return(as.data.frame(list(iteration = seq(1,iter), ERR = ERR)))
}

reg.q = regular.Q(gamma = 0.9, alpha = 1, eps = 0.2, S = 0.99999, iter = 1e6)
friend.Q = multi.Q(val.fun = Q.learn, gamma = 0.9, alpha = 1, eps = 0.2, S = 0.99999, iter = 1e6)
foe.Q = multi.Q(val.fun = foe.val, gamma = 0.9, alpha = 0.8, eps = 1, S = 0.999999, iter = 1e5)
ce.Q = multi.Q(val.fun = ce.val, gamma = 0.9, alpha = 1, eps = 0.9, S = 0.99999, iter = 1e5)

plot.reg <- function(data = reg.q){
  ggplot(data, aes(iteration, ERR)) + geom_line() + coord_cartesian(ylim = c(0,0.5)) +
    labs(title = 'Q-learning', x = 'Simulation Iteration', y = 'Q-Value Difference')
}

plot.friend <- function(data = friend.Q){
  ggplot(data, aes(iteration, ERR)) + geom_line() + coord_cartesian(ylim = c(0,0.5)) +
    labs(title = 'Friend-Q', x = 'Simulation Iteration', y = 'Q-Value Difference')
}

plot.foe <- function(data = foe.Q){
  ggplot(data, aes(iteration, ERR)) + geom_line() + coord_cartesian(ylim = c(0,0.5)) +
    labs(title = 'Foe-Q', x = 'Simulation Iteration', y = 'Q-Value Difference')
}

plot.ce <- function(data = ce.Q){
  ggplot(data, aes(iteration, ERR)) + geom_line() + coord_cartesian(ylim = c(0,0.5)) +
    labs(title = 'CE-Q', x = 'Simulation Iteration', y = 'Q-Value Difference')
}

plot.reg()
plot.friend()
plot.foe()
plot.ce()
