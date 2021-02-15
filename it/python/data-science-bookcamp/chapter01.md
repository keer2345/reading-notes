**[Computing Probabilities Using Python](https://livebook.manning.com/book/data-science-bookcamp/chapter-1/v-5/)**

> datasbook01 <> sharklasers.com 123456

您想赢点钱吗？ 让我们在纸牌游戏中下小额赌注。 在您的前面是一副洗牌。 所有52张卡面朝下。 一半的牌是红色的，一半是黑色的。 我将继续一张一张地翻转卡片。 如果我翻过的最后一张纸牌是红色，您将赢得一美元。 否则，您将损失一美元。

这是转折点； 您可以随时要求我暂停游戏。 当您说“暂停”时，我将翻过下一张纸牌并结束游戏。 下一张卡将用作最终卡。 如果是红色，您将赢得一美元。

我们可以根据您的喜好多次玩游戏。 甲板每次都会被改组。 每轮结束后，我们都会进行兑换。 您赢得这场比赛的最佳方法是什么？

![](https://drek4537l1klr.cloudfront.net/apeltsin/v-5/Figures/fig_p1-1.png)

**This section covers:**
- What are the basics of probability theory?
- Computing probabilities of a single observation.
- Computing probabilities across a range of observation.

**Probability theory** is an inherently complex branch of math. In fact, difficult probability problems can be solved in Python without needing to know a single math equation. Such an equation-free approach to probability requires a baseline understanding of what mathematicians call a **sample space**.

# Sample Space Analysis: An Equation-Free Approach for Measuring Uncertainty in Outcomes

A sample space is the set of all the possible outcomes that an action could produce. Let's take the simple action of flipping a coin. The coin will land on either heads or tails. 

```python
sample_space = {'Heads', 'Tails'}
```
All outcomes within `sample_space` share an identical probability, which is equal to `1 / len(sample_space)`.

Computing the probability of heads:
```python
probability_heads = 1 / len(sample_space)
print(f'Probability of choosing heads is {probability_heads}')
```
Result:
```
Probability of choosing heads is 0.5
```
We’ll assume the coin is unbiased, which means the coin is equally likely to fall on either heads or tails. Thus, a coin-flip is conceptually equivalent to choosing a random element from `sample_space`. 

Let's define two event conditions: one where the coin lands on either heads or tails, and another where the coin lands on neither heads nor tails.

Defining event conditions:
```python
def is_heads_or_tails(outcome): return outcome in {'Heads', 'Tails'}
def is_neither(outcome): return not is_heads_or_tails(outcome)
```
Defining additioning event conditions:
```python
def is_heads(outcome): return outcome == 'Heads'
def is_tails(outcome): return outcome == 'Tails'
```

We can pass event conditions into a generalized get_events `function`. It inputs are an event condition, and also a generic sample space:
```python
def get_event(event_condition, sample_space):
  return set([outcome for outcome in sample_space
              if event_condition(outcome)])
```
Detecting events using event conditions:
```python
for event_condition in event_conditions:
    print(f"Event Condition: {event_condition.__name__}")
    event = get_event(event_condition, sample_space)
    print(f'Event: {event}\n')
```
Result:
```
Event Condition: is_heads_or_tails
Event: {'Tails', 'Heads'}

Event Condition: is_heads
Event: {'Heads'}

Event Condition: is_tails
Event: {'Tails'}

Event Condition: is_neither
Event: set()
```
This property can be generalized to include multi-element events. The probability of an event is equal to `len(event) / len(sample_space)`, but only if all outcomes are known to occur with equal likelihood. In other words, the probability of a multi-element event for a fair coin is equal to event size divided by the sample space size. We’ll now leverage event size to compute the 4 event probabilities.

Computing event probabilities:
```python
def compute_probability(event_condition, generic_sample_space):
    event = get_event(event_condition, generic_sample_space)
    return len(event) / len(generic_sample_space)


for event_condition in event_conditions:
    prob = compute_probability(event_condition, sample_space)
    name = event_condition.__name__
    print(f"Probability of event arising from '{name}' is {prob}")
```
Result:
```
Probability of event arising from 'is_heads_or_tails' is 1.0
Probability of event arising from 'is_heads' is 0.5
Probability of event arising from 'is_tails' is 0.5
Probability of event arising from 'is_neither' is 0.0
```
The executed code outputs a diverse range of event probabilities, the lowest of which is 0.0 and the largest of which is 1.0. These values represent the lower and upper bounds of probability; no probability can ever fall below 0.0 or rise above 1.0.

## Analyzing a Biased Coin
We computed probabilities for an unbiased coin. What would happen if that coin was biased? Simple! We’ll construct a weighed sample space, represented by a Python dictionary. Each outcome will be treated as a key whose value maps to the associated weight. In our example, heads is weighted *4* times as heavily as tails. Therefore, we’ll map `Tails` to *1* and `Heads` to *4*.

Representing a weighted sample space:
```python
weighted_sample_space = {'Heads': 4, 'Tails': 1}
```
Our new sample space is stored within a dictionary. This allows us to redefine sample-space size as the sum of all dictionary weights. Within `weighted_sample_space`, that sum will equal *5*.
```python
sample_space_size = sum(weighted_sample_space.values())
assert sample_space_size == 5
```

We can redefine event size in similar manner.

Checking the weighted event size:
```python
event = get_event(is_heads_or_tails, weighted_sample_space)
event_size = sum(weighted_sample_space[outcome] for outcome in event)
assert event_size == 5
```
Our generalized definitions of sample-space size and event size permit us to create a `compute_event_probability` function. 

Defining a generalized event probability function:
```python
def compute_event_probability(event_condition, generic_sample_space):
    event = get_event(event_condition, generic_sample_space)
    if type(generic_sample_space) == type(set()):
        return len(event) / len(generic_sample_space)

    event_size = sum(generic_sample_space[outcome]
                     for outcome in event)
    return event_size / sum(generic_sample_space.values())
```
```python
for event_condition in event_conditions:
    prob = compute_event_probability(event_condition, weighted_sample_space)
    name = event_condition.__name__
    print(f"Probability of event arising from '{name}' is {prob}")
```
Result:
```
Probability of event arising from 'is_heads_or_tails' is 1.0
Probability of event arising from 'is_heads' is 0.8
Probability of event arising from 'is_tails' is 0.2
Probability of event arising from 'is_neither' is 0.0
```

# Computing Non-Trivial Probabilities
We will now proceed to solve several example problems using `compute_event_probability`.

## Problem 1: Analyzing a Family with 4 Children

Suppose a family has 4 children. What is the probability that exactly 2 of the children are boys? We’ll assume that each child is equally likely to be either a boy or a girl. This allows us to construct an unweighted sample space where each outcome is a 4-element tuple representing one possible sequence of 4 children.

The sample space for 4 successive children. Each row in the sample space contains one of 16 possible outcomes. Every outcome represents a unique combination of 4 children. The sex of each child is signaled by a letter; B for Boy and G for Girl. Outcomes with 2 boys are marked by an arrow. There are 6 such arrows present. Thus, the probability of 2 boys equals 6/16:

![](https://drek4537l1klr.cloudfront.net/apeltsin/v-5/Figures/fig1-2.png)

Computing the sample space of children:
```python
possible_children = ['Boy', 'Girl']
sample_space = set()
for child1 in possible_children:
    for child2 in possible_children:
        for child3 in possible_children:
            for child4 in possible_children:
                outcome = (child1, child2, child3, child4)
                sample_space.add(outcome)
```
```
{('Body', 'Body', 'Body', 'Body'),
 ('Body', 'Body', 'Body', 'Girl'),
 ('Body', 'Body', 'Girl', 'Body'),
 ('Body', 'Body', 'Girl', 'Girl'),
 ('Body', 'Girl', 'Body', 'Body'),
 ('Body', 'Girl', 'Body', 'Girl'),
 ('Body', 'Girl', 'Girl', 'Body'),
 ('Body', 'Girl', 'Girl', 'Girl'),
 ('Girl', 'Body', 'Body', 'Body'),
 ('Girl', 'Body', 'Body', 'Girl'),
 ('Girl', 'Body', 'Girl', 'Body'),
 ('Girl', 'Body', 'Girl', 'Girl'),
 ('Girl', 'Girl', 'Body', 'Body'),
 ('Girl', 'Girl', 'Body', 'Girl'),
 ('Girl', 'Girl', 'Girl', 'Body'),
 ('Girl', 'Girl', 'Girl', 'Girl')}
```

We use 4 *for-loops* to explore the sequence of 4 births. This is not an efficient use of code. We can more easily generate our sample space using Python's built-in `itertools.product` function.

Computing the sample space using product:
```python
from itertools import product
all_combinations = product(* (4 * [possible_children]))
assert set(all_combinations) == sample_space
```

We can make our code even more efficient by executing `set(product(possible_children, repeat=4))`. In general, running `product(possible_children, repeat=n)` will return an iterable over all possible combinations of `n` children.

```python
sample_space_efficient = set(product(possible_children, repeat=4))
assert sample_space == sample_space_efficient
```
Let's calculate the fraction of `sample_space` that is composed of families with 2 boys.

Computing the probability of 2 boys:
```python
def has_two_boys(outcome):
    return len([child for child in outcome if child == 'Boy']) == 2

prob = compute_event_probability(has_two_boys, sample_space)
print(f"Probability of 2 boys is {prob}")
```
Result:
```
Probability of 2 boys is 0.375
```

## Problem 2: Analyzing Multiple Dice Rolls

Suppose we’re shown a fair six-sided die whose faces are numbered from 1 to 6. The die is rolled 6 times. What is the probability that these 6 dice-rolls add up to 21?

We’ll begin by defining the possible values of any single roll. These are integers that range from 1 to 6.

```python
possible_rolls = list(range(1, 7))
print(possible_rolls)
```
Result:
```
[1, 2, 3, 4, 5, 6]
```
The sample space for 6 consecutive dice rolls:
```python
sample_space = set(product(possible_rolls, repeat=6))
```

Finally, we'll define a `has_sum_of_21` event condition that we'll subsequently pass into `compute_event_probability`.

```python
def get_event(event_condition, sample_space):
    return set([outcome for outcome in sample_space if event_condition(outcome)])


def compute_event_probability(event_condition, generic_sample_space):
    event = get_event(event_condition, generic_sample_space)
    if type(generic_sample_space)==type(set()):
        return len(event) / len(generic_sample_space)
    event_size = sum(generic_sample_space[outcome] for outcome in event)
    return event_size / sum(generic_sample_space.values())
```
```python
def has_sum_of_21(outcome): return sum(outcome) == 21

prob = compute_event_probability(has_sum_of_21, sample_space)

print(f"Probability of dice summing to 21 is {prob}")
```
Reuslt:
```
Probability of dice summing to 21 is 0.09284979423868313
```


Computing the probability using a lambda expression:

```python
prob = compute_event_probability(lambda x: sum(x) == 21, sample_space)
assert prob == compute_event_probability(has_sum_of_21, sample_space)
```

## Problem 3: Computing Dice-Roll Probabilities using Weighted Sample Spaces