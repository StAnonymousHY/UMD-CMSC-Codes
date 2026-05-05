import numpy as np

class NN:
    def __init__(self, activation_function, loss_function, hidden_layers=[1024], input_d=784, output_d=10, momentum=0.9):
        self.weights = []
        self.biases = []
        self.activation_function = activation_function
        self.loss_function = loss_function
        d1 = input_d
        hidden_layers.append(output_d)
        # decides how much you want the past gradients to influence the update
        self.momentum = momentum
        for d2 in hidden_layers:
            self.weights.append(np.random.randn(d2, d1)*np.sqrt(2.0/d1))
            self.biases.append(np.zeros((d2,1)))
            d1 = d2
        # memory of past weight / bias updates
        self.v_Ws = [np.zeros_like(w) for w in self.weights]
        self.v_bs = [np.zeros_like(b) for b in self.biases]

    def print_model(self):
        """
        This function prints the shapes of weights and biases for each layer.
        """
        print("activation:{}".format(self.activation_function.__class__.__name__))
        print("loss function:{}".format(self.loss_function.__class__.__name__))
        for idx,(w,b) in enumerate(zip(self.weights, self.biases),1):
            print("Layer {}\tw:{}\tb:{}".format(idx, w.shape, b.shape))

    def predict(self, X):
        D = X
        ws = self.weights
        bs = self.biases
        for w,b in zip(ws[:-1], bs[:-1]):
            D = self.activation_function.activate(np.matmul(w,D)+b) 
        Yhat = np.matmul(ws[-1], D)+bs[-1]
        return np.argmax(Yhat, axis=0)

    def compute_gradients(self, X, Y):
        ws = self.weights
        bs = self.biases
        D_stack = []

        D = X
        D_stack.append(D)
        num_layers = len(ws)
        for idx in range(num_layers-1):
            A = np.matmul(ws[idx], D) + bs[idx]
            D = self.activation_function.activate(A)
            D_stack.append(D)

        Yhat = np.matmul(ws[-1], D) + bs[-1]
        training_loss = self.loss_function.loss(Y, Yhat)
        '''
        '''
        grad_bs = []
        grad_Ws = []

        grad = self.loss_function.lossGradient(Y,Yhat)
        grad_b = np.sum(grad, axis=1, keepdims=1)
        grad_W = np.matmul(grad, D_stack[num_layers-1].transpose())
        grad_bs.append(grad_b)
        grad_Ws.append(grad_W)
        for idx in range(num_layers-2, -1, -1):
            grad = np.matmul(ws[idx + 1].transpose(), grad) * self.activation_function.backprop_grad(D_stack[idx + 1])
            grad_b = np.zeros((grad.shape[0], 1))
            for i in range(grad.shape[0]):
                grad_b[i, 0] = np.sum(grad[i])
            grad_W = np.matmul(grad, D_stack[idx].transpose())
            grad_bs.append(grad_b)
            grad_Ws.append(grad_W)

        grad_bs, grad_Ws = grad_bs[::-1], grad_Ws[::-1]
        return training_loss, grad_Ws, grad_bs

    def update(self, grad_Ws, grad_bs, learning_rate):
        num_layers = len(grad_Ws)
        for idx in range(num_layers):
            # update using previous updates and current gradient
            # v = momentum * v - learning rate * gradient
            self.v_Ws[idx] = self.momentum * self.v_Ws[idx] - learning_rate * grad_Ws[idx]
            self.v_bs[idx] = self.momentum * self.v_bs[idx] - learning_rate * grad_bs[idx]
            # apply the updates
            # parameter = parameter + v
            self.weights[idx] += self.v_Ws[idx]
            self.biases[idx] += self.v_bs[idx]
        return 

class activationFunction:
    def activate(self,X):
        """
        The output of activate should have the same shape as X
        """
        raise NotImplementedError("Abstract class.")

    def backprop_grad(self, grad):
        """
        The output of backprop_grad should have the same shape as X
        """
        raise NotImplementedError("Abstract class.")

class Relu(activationFunction):
    def activate(self,X):
        """
        The output of activate should have the same shape as X
        """
        return X*(X>0)

    def backprop_grad(self, X):
        """
        The output of backprop_grad should have the same shape as X
        """
        return (X>0).astype(np.float64)

class Linear(activationFunction):
    def activate(self,X):
        """
        The output of activate should have the same shape as X
        """
        return X
    def backprop_grad(self,X):
        """
        The output of backprop_grad should have the same shape as X
        """
        return np.ones(X.shape, dtype=np.float64)

class LossFunction:
    def loss(self, Y, Yhat):
        """
        The true values are in the vector Y; the predicted values are
        in Yhat; compute the loss associated with these predictions.
        """
        raise NotImplementedError("Abstract class.")

    def lossGradient(self, Y, Yhat):
        """
        The true values are in the vector Y; the predicted values are in 
        Yhat; compute the gradient of the loss with respect to Yhat
        """
        raise NotImplementedError("Abstract class.")

class SquaredLoss(LossFunction):
    def loss(self, Y, Yhat):
        """
        The true values are in the vector Y; the predicted values are
        in Yhat; compute the loss associated with these predictions.
        """
        return np.sum((Yhat - Y) ** 2) / (2 * len(Y[0]))
        raise NotImplementedError("Implement SquaredLoss.")

    def lossGradient(self, Y, Yhat):
        """
        The true values are in the vector Y; the predicted values are in 
        Yhat; compute the gradient of the loss with respect to Yhat
        """
        return (Yhat - Y) / len(Y[0])
        raise NotImplementedError("Implement SquaredLoss.")


class CELoss(LossFunction):
    def loss(self, Y, Yhat):
        """
        The true values are in the vector Y; the predicted values are
        in Yhat; compute the loss associated with these predictions.
        """
        raise NotImplementedError("Implement CELoss.")

    def lossGradient(self, Y, Yhat):
        """
        The true values are in the vector Y; the predicted values are in 
        Yhat; compute the gradient of the loss with respect to Yhat, which
        has the same shape of Yhat and Y.
        """
        raise NotImplementedError("Implement CELoss")

