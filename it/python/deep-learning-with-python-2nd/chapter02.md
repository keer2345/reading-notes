**[The mathematical building blocks of neural networks](https://livebook.manning.com/book/deep-learning-with-python-second-edition/chapter-2)**

> dlwp2nd02 <> sharklasers.com 12345

本章涵盖：
- 神经网络的第一个例子
- 张量（Tensor）和张量操作
- 经网络如何通过反向传播（backpropagation）和梯度下降（gradient descent）学习

理解深度学习需要熟悉许多简单的数学概念：张量，张量运算，微分，梯度下降等。 我们在本章中的目标是在不过度了解技术的情况下建立您对这些概念的直觉。特别是，我们将避免使用数学符号，这对于那些没有数学背景的人可能会引起争议，并且不需要很好地解释。 数学运算最精确，最明确的描述是其可执行代码。

为了给张量和梯度下降添加一些背景，我们将以神经网络的实际示例开始本章。 然后，我们将逐点介绍引入的每个新概念。 请记住，这些概念对于您理解以下章节中的实际示例至关重要！

阅读完本章后，您将对深度学习背后的数学理论有一个直观的了解，并准备在第3章中开始研究 Keras 和 TensorFlow。

# 第一次了解神经网络

*A first look at a neural network*

让我们看一个使用Python库Keras来学习对手写数字进行分类的神经网络的具体示例。 除非您已经拥有Keras或类似库的经验，否则您将不会立即了解有关第一个示例的所有内容。 您可能甚至还没有安装Keras； 没关系。 在下一章中，我们将回顾示例中的每个元素并进行详细说明。 因此，不必担心某些步骤似乎是武断的或对您来说像魔术一样！ 我们必须从某个地方开始。

我们此处要解决的问题是将手写数字的灰度图像（28×28像素）分类为10类（0至9）。 我们将使用MNIST数据集，该数据集是机器学习社区中的经典之作，其存在时间与该领域本身差不多，并且经过了深入的研究。 它是由国家标准技术研究所（MNIST的NIST）在1980年代组装而成的，包含60,000张训练图像和10,000张测试图像。

您可以将MNIST“解决”为深度学习的“ Hello World”，这是您验证算法是否按预期工作的方法。 当您成为机器学习的从业人员时，您会在科学论文，博客文章等中一遍又一遍地看到MNIST。 您可以在下图中看到一些MNIST示例。

> **关于类别和标签的注意事项**
> 
> 在机器学习中，分类问题中的类别称为类 *class*。 数据点称为样本 *sample*。 与特定样本关联的类称为标签。与特定样本关联的类称为标签 *label*。

手写数字的样本数字：

![](https://drek4537l1klr.cloudfront.net/chollet2/v-4/Figures/MNIST-sample-digits.png)

您现在不需要尝试在您的机器上重现这个示例。如果你愿意，你首先需要建立一个深度学习工作区，这在第3章中介绍。

在Keras中，MNIST数据集以一组四个NumPy数组的形式预先加载。

```python
> ipython
In [1]: from keras.datasets import mnist

In [2]: (train_images, train_labels), (test_images, test_labels) = mnist.load_data()
Downloading data from https://storage.googleapis.com/tensorflow/tf-keras-datasets/mnist.npz
11493376/11490434 [==============================] - 3s 0us/step
```

`train_images` 和 `train_labels` 来自于训练集，是模型学习的数据。模型将在测试集中测试，`test_images` 和 `test_labels`。图像被编码成 Numpy 数组，标签是范围为 0 到 9 的数字数组，图像和标签是一对一的关系。

看一下训练数据：
```python
In [3]: train_images.shape
Out[3]: (60000, 28, 28)

In [4]: len(train_labels)
Out[4]: 60000

In [5]: train_labels
Out[5]: array([5, 0, 4, ..., 5, 6, 8], dtype=uint8)
```

这是测试数据：
```python
In [6]: test_images.shape
Out[6]: (10000, 28, 28)

In [7]: len(test_labels)
Out[7]: 10000

In [8]: test_labels
Out[8]: array([7, 2, 1, ..., 4, 5, 6], dtype=uint8)
```

工作流是这样：首先，我们将训练数据提供给神经网络，`train_images` 和 `train_labels`。网络将学习图像和标签的关联 。最后，我们要求网络为 `test_images` 生成预测，我们将验证这些预测是否与 `test_labes` 匹配。

让我们再次构建这个网络，请记住，您还不需要理解关于这个示例的所有内容。
```python
In [1]: from tensorflow.keras import models

In [2]: from tensorflow.keras import layers

In [3]: model = models.Sequential([
   ...:   layers.Dense(512, activation='relu'),
   ...:   layers.Dense(10,  activation='softmax')
   ...: ])
2021-02-05 22:54:18.902193: I tensorflow/compiler/jit/xla_cpu_device.cc:41] Not creating XLA devices, tf_xla_enable_xla_devices not set
```

构建神经网络块的核心是层（layer），可以把数据处理模块当做数据过滤器。一些数据输入后，就会以有用的形式输出。体来说，层从提供给它们的数据中提取表示形式——希望这些表示形式对当前的问题更有意义。大多数深度学习都由将简单层链接在一起构成，这些简单层将实现渐进式数据提炼的形式。 深度学习模型就像数据处理的筛子一样，由一系列不断完善的数据过滤器（各层）组成。

这里，我们的模型由两个序列的 `Dense` 层组成，密集地连接（也成为完全连接 *fully connected*）神经层。第二层（也是最后一层）是 10 路的 *softmax classification* 层，这意味着它将返回 10 个概率分数（总和为 1）的数组。每个分数将是当前数字图像属于我们10个数字类之一的概率。

为了让模型准备好训练，我们需要再做三件事，作为编译的步骤：
- 优化器——一种机制，模型将根据模型所看到的训练数据来更新自身，从而提高其性能。
- 损失函数——模型将如何在训练数据上衡量其性能，从而如何将自己引导到正确的方向。
- 训练和测试期间要监控的指标——在这里，我们只关心准确性（正确分类的图像的比例）

在接下来的两章中，将明确说明损失函数和优化器的确切目的。

编译步骤:
```python
In [5]: model.compile(optimizer='rmsprop',
   ...:               loss='sparse_categorical_crossentropy',
   ...:               metrics=['accuracy'])
```
在训练之前，我们将数据重新整形为模型期望的形状并对其进行缩放，以使所有值都在 `[0，1]` 区间内，从而对数据进行预处理。 以前，例如，我们的训练图像存储在 `uint8` 类型的形状 `(60000、28、28)` 形状的数组中，其值在 `[0，255]` 区间内。 我们将其转换为形状为 `(60000，28 * 28)` 的 `float32` 数组，其值介于 `0` 和 `1` 之间。

```python
In [9]: train_images = train_images.reshape((60000, 28 * 28))

In [10]: train_images = train_images.astype('float32') / 255

In [11]: test_images = test_images.reshape((10000, 28 * 28))

In [12]: test_images = test_images.astype('float32') / 255
```

我们现在准备训练模型，在 Keras 中是通过调用模型的 `fit` 方法——我们使模型适合（`fit`）其训练数据：

```python
In [13]: model.fit(train_images, train_labels, epochs=5, batch_size=128)
2021-02-06 10:18:35.409251: I tensorflow/compiler/mlir/mlir_graph_optimization_pass.cc:116] None of the MLIR optimization passes are enabled (registered 2)
Epoch 1/5
469/469 [==============================] - 6s 11ms/step - loss: 0.4254 - accuracy: 0.8771
Epoch 2/5
469/469 [==============================] - 5s 10ms/step - loss: 0.1110 - accuracy: 0.9666
Epoch 3/5
469/469 [==============================] - 5s 10ms/step - loss: 0.0691 - accuracy: 0.9792
Epoch 4/5
469/469 [==============================] - 5s 10ms/step - loss: 0.0479 - accuracy: 0.9852
Epoch 5/5
469/469 [==============================] - 5s 10ms/step - loss: 0.0373 - accuracy: 0.9896
Out[13]: <tensorflow.python.keras.callbacks.History at 0x1448a2730>
```
在训练过程中显示两个量：根据训练数据的模型损失和根据训练数据的模型准确性。 我们很快就在训练数据上达到了0.989（98.9％）的准确性。

现在我们有了训练有素的模型，您可以使用它来预测新数字的类别概率，这些数字不是训练数据的一部分，例如来自测试集的图像：
```python
In [14]: test_digits = test_images[0:10]

In [15]: predictions = model.predict(test_digits)

In [16]: predictions[0]
Out[16]:
array([1.3754735e-08, 1.6160874e-10, 6.5974518e-06, 9.8701028e-05,
       2.6899620e-11, 5.4845696e-08, 1.9237899e-15, 9.9989343e-01,
       2.3429433e-08, 1.3337665e-06], dtype=float32)
```

数组中每一个索引 `i` 的数目对应于数字图像 `test_digits[0]` 所属的第 `i` 类的概率。

第一个测试数字在索引7处具有最高的概率得分（0.99989343，几乎为1），因此根据我们的模型，该数字必须为7：
```python
In [17]: predictions[0].argmax()
Out[17]: 7

In [18]: predictions[0][7]
Out[18]: 0.9998934
```

平均而言，我们的模型对这些从未见过的数字进行分类的效果如何？ 让我们通过计算整个测试集的平均准确性来进行检查。

```python
In [19]: test_loss, test_acc = model.evaluate(test_images, test_labels)
313/313 [==============================] - 1s 2ms/step - loss: 0.0679 - accuracy: 0.9784

In [20]: print('test_acc:', test_acc)
test_acc: 0.9783999919891357
```

测试集的准确率为97.8%，远低于训练集的准确率(98.9%)。训练精度和测试精度之间的差距是过拟合的一个例子:事实上，机器学习模型在新数据上的表现往往比在训练数据上差。过度拟合是第三章的中心主题。

这是我们的第一个示例的总结，您刚刚看到了如何在不到15行Python代码中构建和训练一个神经网络来分类手写数字。在这一章和下一章，我们将详细介绍我们刚刚预览过的每一个移动部件，并阐明在幕后发生的事情。你会学到张量，模型中的数据存储对象;张量运算，由哪些层构成;以及梯度下降(`gradient descent`)，它允许您的模型从训练示例中学习。

# 神经网络的数据表示

*Data representations for neural networks*

在前面的示例中，我们从存储在多维NumPy数组（也称为张量 *tensors*）中的数据开始。通常，所有当前的机器学习系统都使用张量作为其基本数据结构。 张量是该领域的基础-TensorFlow以其命名为基础。 那么什么是张量？

张量的核心是数据的容器，通常是数字数据。 因此，它是数字的容器。 您可能已经熟悉矩阵，它们是2级张量：张量是矩阵到任意数量维的一般化（请注意，在张量的上下文中，维通常称为轴）。

## 标量（0阶张量）

*Scalars (rank-0 tensors)*

仅包含一个数字的张量称为标量（或标量张量，或 `rank-0` 张量，或 `0D` 张量）。 在 NumPy 中，`float32` 或 `float64` 数字是标量张量（或标量数组）。 您可以通过 `ndim` 属性显示 NumPy 张量的轴数。 标量张量具有 `0` 个轴（`ndim == 0`）。 张量的轴数也称为其等级。 这是NumPy标量：

```python
In [21]: import numpy as np

In [22]: x = np.array(12)

In [23]: x
Out[23]: array(12)

In [24]: x.ndim
Out[24]: 0
```

## 矢量（1阶张量）

*Vectors (rank-1 tensors)*


数字数组称为向量，或1级张量，或 `1D` 张量。 1级张量据说恰好具有一个轴。 以下是NumPy向量：
```python
In [25]: x = np.array([12, 3, 6, 14, 7])

In [26]: x
Out[26]: array([12,  3,  6, 14,  7])

In [27]: x.ndim
Out[27]: 1
```

该向量有五个条目，因此称为5维向量。 不要将5D向量与5D张量混淆！ 5D向量仅具有一个轴，并且沿其轴具有五个维度，而5D张量具有五个轴（并且沿每个轴可以具有任意数量的维度）。维度可以表示沿特定轴的条目数（例如5D矢量），也可以表示张量中的轴数（例如5D张量），这有时会造成混淆。 在后一种情况下，谈论等级5的张量（张量的等级是轴数）在技术上更正确，但无论如何，模糊的符号5D张量都是常见的。

## 矩阵（2阶张量）

*Matrices (rank-2 tensors)*

```python
In [28]: x = np.array([[5, 78, 2, 34, 0],
    ...:               [6, 79, 3, 35, 1],
    ...:               [7, 80, 4, 36, 2]])

In [29]: x.ndim
```

## 三阶及更高阶的张量

*Rank-3 tensors and higher-rank tensors*

```python
In [31]: x = np.array([[[5, 78, 2, 34, 0],
    ...:                [6, 79, 3, 35, 1],
    ...:                [7, 80, 4, 36, 2]],
    ...:               [[5, 78, 2, 34, 0],
    ...:                [6, 79, 3, 35, 1],
    ...:                [7, 80, 4, 36, 2]],
    ...:               [[5, 78, 2, 34, 0],
    ...:                [6, 79, 3, 35, 1],
    ...:                [7, 80, 4, 36, 2]]])

In [32]: x.ndim
Out[32]: 3
```

通过将 3 阶张量打包在一个数组中，可以创建 4 阶张量，依此类推。 在深度学习中，通常会处理等级 0 到 4 的阶张量，但如果处理视频数据则可能会增加到 5 阶。

## 关键属性

*Key attributes*

张量由三个关键属性定义：
- 轴的数目（等级）——例如，一个3阶张量有三个轴，一个矩阵有两个轴。这在Python库中也被称为张量的ndim，如NumPy或TensorFlow。
- 形状——这是一个整数元组，描述张量沿着每个轴的尺寸。 例如，前一个矩阵示例的形状为 `(3，5)`，3阶张量示例的形状为 `(3，3，5)`。 向量的形状只有一个元素，例如 `(5，)`，而标量的形状是空的 `()`。
- 据类型（在Python库中通常称为 `dtype`）——这是张量中包含的数据的类型； 例如，张量的类型可以是 `float16`，`float32`，`float64`，`uint8` 等。 在 TensorFlow 中，您也可能会遇到字符串张量。

为了更具体一点，让我们回顾一下在MNIST示例中处理的数据。 首先，我们加载MNIST数据集：

```python
In [33]: from keras.datasets import mnist

In [34]: (train_images, train_labels), (test_images, test_labels) = mnist.load_data()
```

接下来，我们显示张量 `train_images` 的轴数，即 `ndim` 属性：

```python
In [35]: print(train_images.ndim)
3
```

它的形状：
```python
In [36]: print(train_images.shape)
(60000, 28, 28)
```

数据类型，也就是 `dtype` 属性：
```python
In [37]: print(train_images.dtype)
uint8
```

所以我们这里是8位整数的3级张量。 更精确地说，它是由60,000个28×28整数矩阵组成的数组。 每个此类矩阵都是灰度图像，系数在0到255之间。

让我们使用Matplotlib库（标准科学Python套件的一部分）在此3级张量中显示第四个数字：
```python
In [3]: digit = train_images[4]

In [4]: import matplotlib.pyplot as plt

In [5]: plt.imshow(digit, cmap=plt.cm.binary)
Out[5]: <matplotlib.image.AxesImage at 0x1426dc970>

In [6]: plt.show()
```

![](https://drek4537l1klr.cloudfront.net/chollet2/v-4/Figures/The-fourth-sample-in-our-dataset.png)

自然地，相应的标签只是整数9：

```python
In [7]: train_labels[4]
Out[7]: 9
```

## 在NumPy中操纵张量

*Manipulating tensors in NumPy*