**[Computing Probabilities Using Python](https://livebook.manning.com/book/data-science-bookcamp/chapter-1/v-5/)**

> datasbook01 <> sharklasers.com 123456

您想赢点钱吗？ 让我们在纸牌游戏中下小额赌注。 在您的前面是一副洗牌。 所有52张卡面朝下。 一半的牌是红色的，一半是黑色的。 我将继续一张一张地翻转卡片。 如果我翻过的最后一张纸牌是红色，您将赢得一美元。 否则，您将损失一美元。

这是转折点； 您可以随时要求我暂停游戏。 当您说“暂停”时，我将翻过下一张纸牌并结束游戏。 下一张卡将用作最终卡。 如果是红色，您将赢得一美元。

我们可以根据您的喜好多次玩游戏。 甲板每次都会被改组。 每轮结束后，我们都会进行兑换。 您赢得这场比赛的最佳方法是什么？

![](https://drek4537l1klr.cloudfront.net/apeltsin/v-5/Figures/fig_p1-1.png)

- [Sample Space Analysis: An Equation-Free Approach for Measuring Uncertainty in Outcomes](#sample-space-analysis-an-equation-free-approach-for-measuring-uncertainty-in-outcomes)
  - [Analyzing a Biased Coin](#analyzing-a-biased-coin)
- [Computing Non-Trivial Probabilities](#computing-non-trivial-probabilities)
  - [Problem 1: Analyzing a Family with 4 Children](#problem-1-analyzing-a-family-with-4-children)
  - [Problem 2: Analyzing Multiple Dice Rolls](#problem-2-analyzing-multiple-dice-rolls)
  - [Problem 3: Computing Dice-Roll Probabilities using Weighted Sample Spaces](#problem-3-computing-dice-roll-probabilities-using-weighted-sample-spaces)
- [Computing Probabilities Over Interval Ranges](#computing-probabilities-over-interval-ranges)
  - [Evaluating Extremes Using Interval Analysis](#evaluating-extremes-using-interval-analysis)
- [Summary](#summary)

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

Let's recompute that probability using a weighted sample space. How do we convert our unweighted sample-space set into a weighted sample-space dictionary? The solution is simple. We must first identify all possible sums of 6 dice-rolls. Then, we must count the number of times each sum appears across all possible 6-dice-rolls-combinations. These combinations are already stored in our computed `sample_space` set. By mapping the dice-roll sums to their occurence counts, we will produce a `weighted_sample_space` result.

Mapping dice-roll sums to ocurrence counts:
```python
from collections import defaultdict
weighted_sample_space = defaultdict(int)
for outcome in sample_space:
    total = sum(outcome)
    weighted_sample_space[total] += 1
```
```python
assert weighted_sample_space[6] == 1
assert weighted_sample_space[36] == 1
```

Checking a more common dice-roll combination:

```python
num_combinations = weighted_sample_space[21]
print(f"There are {num_combinations } ways for six rolled dice to sum to 21")
```
Result:
```
There are 4332 ways for six rolled dice to sum to 21
```

Exploring different ways of summing to 21:
```python
assert sum([4, 4, 4, 4, 3, 2]) == 21
assert sum([4, 4, 4, 5, 3, 1]) == 21

event = get_event(lambda x: sum(x) == 21, sample_space)
assert weighted_sample_space[21] == len(event)
assert sum(weighted_sample_space.values()) == len(sample_space)

assert sum(weighted_sample_space.values()) == 6 * 6 * 6 * 6 * 6 * 6
```

Let's now recompute the probability using the `weighted_sample_space` dictionary. The final probability of rolling a 21 should remain unchanged.


Computing the weighted event probability of dice rolls:
```python
prob = compute_event_probability(lambda x: x == 21, weighted_sample_space)
assert prob == compute_event_probability(has_sum_of_21, sample_space)
print(f"Probability of dice summing to 21 is {prob}")
```
Result:
```
Probability of dice summing to 21 is 0.09284979423868313
```

What is the benefit of using a weighted sample space over an unweighted one? Less memory usage! As we see below, the unweighted sample_space set has on the order of *1500x* more elements than the weighted sample space dictionary.

Comparing weighted to unweighted event space size:
```python
print('Number of Elements in Unweighted Sample Space:')
print(len(sample_space))
print('Number of Elements in Weighted Sample Space:')
print(len(weighted_sample_space))
```
Result:
```
Number of Elements in Unweighted Sample Space:
46656
Number of Elements in Weighted Sample Space:
31
```
# Computing Probabilities Over Interval Ranges
So far, we’ve only analyzed event conditions that satisfy some single value. Now, we’ll analyze event conditions that span across intervals of values. An **interval** is the set of all the numbers that are sandwiched between two boundary cutoffs. Let's define an `is_in_interval` function that checks whether a number falls within a specified interval. We’ll control the interval boundaries by passing a `minimum` and a `maximum` parameter.
```python
def is_in_interval(number, minimum, maximum):
    return minimum <= number <= maximum
```
Given the `is_in_interval` function, we can compute the probability that an event’s associated value falls within some numeric range. For instance, let’s compute the likelihood that our 6 consecutive dice-rolls sum up to a value between 10 and 21.

Computing the probability over an interval:
```python
prob = compute_event_probability(lambda x: is_in_interval(x, 10, 21),
                                 weighted_sample_space)
print(f"Probability of interval is {prob}")
```
Result:
```
Probability of interval is 0.5446244855967078
```

## Evaluating Extremes Using Interval Analysis

Interval analysis（区间分析） is critical to solving a whole class of very important problems in probability and statistics. One such problem involves the evaluation of extremes. The problem boils down to whether observed data is too extreme to be believable.

Data seems extreme when it is too unusual to have occurred by random chance. 

We’ll find our answer by computing an interval probability. However, first we need the sample space for every possible sequence of 10 flipped coins. Let's generate a weighted sample space. As previously discussed, this is more efficient than using a non-weighted representation.

Computing the sample space for 10 coin-flips:
```python
def generate_coin_sample_space(num_flips=10):
    weighted_sample_space = defaultdict(int)
    for coin_flips in product(['Heads', 'Tails'], repeat=num_flips):
        heads_count = len([outcome for outcome in coin_flips
                          if outcome == 'Heads'])
        weighted_sample_space[heads_count] += 1

    return weighted_sample_space

weighted_sample_space = generate_coin_sample_space()
assert weighted_sample_space[10] == 1
assert weighted_sample_space[9] == 10
```
```python
prob = compute_event_probability(lambda x: is_in_interval(x, 8, 10),
                                 weighted_sample_space)
print(f"Probability of observing more than 7 heads is {prob}")
```
Result:
```
Probability of observing more than 7 heads is 0.0546875
```

Let's formulate the problem as follows; what is the probability that 10 fair coin-flips produce either 0 to 2 heads or 8 to 10 heads? Or, stated more concisely, what is the probability that the coin-flips do NOT produce between 3 and 7 heads? That probability is computed below.

Computing an extreme interval probability:

```python
prob = compute_event_probability(lambda x: not is_in_interval(x, 3, 7),
                                 weighted_sample_space)
print(f"Probability of observing more than 7 heads or 7 tails is {prob}")
```
Result:
```
Probability of observing more than 7 heads or 7 tails is 0.109375
```

Analyzing extreme head-counts for 20 fair coin-flips:
```python
weighted_sample_space_20_flips = generate_coin_sample_space(num_flips=20)
prob = compute_event_probability(lambda x: not is_in_interval(x, 5, 15),
                                 weighted_sample_space_20_flips)
print(f"Probability of observing more than 15 heads or 15 tails is {prob}")
```
Result:
```
Probability of observing more than 15 heads or 15 tails is 0.01181793212890625
```

The updated probability has dropped from approximately .1 to approximately .01. Thus, the added evidence has caused a 10-fold decrease in our confidence of fairness. Despite this probability drop, the ratio of heads to tails has remained constant at 4-to-1. Both our original and updated experiments produced 80% heads and 20% tails. This leads to an interesting an question: why does the probability of observing 80% or more heads decrease as the supposedly fair coin gets flipped more times? We can find out through detailed mathematical analysis. However, a much more intuitive solution is to just visualize the distribution of head-counts across our 2 sample space dictionaries. The visualization would effectively be a plot of keys (head-counts) vs values (combination counts) present in each dictionary. We can carry out this plot using Matplotlib; Python’s most popular visualization library. In the subsequent section, we will discuss Matplotlib usage, and its application to probability theory.

# Summary
- A sample space is the set of all the possible outcomes that an action can produce.
- An event is a subset of the sample space containing just those outcomes that satisfy some event condition. An event condition is a Boolean function that takes as input an outcome and returns either True or False.
- The probability of an event equals the fraction of event outcomes that cover the entire sample space.
- Probabilities can be computed over numeric intervals. An interval is defined as the set of all the numbers that are sandwiched between two boundary values.
- Interval probabilities are useful for determining whether an observation appears extreme.