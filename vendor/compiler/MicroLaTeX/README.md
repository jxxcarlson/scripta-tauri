# MicroLaTeX

## TransformLaTeX

L153, modified also re $$

L243, L270: I've removed the error handling for
missing end-of-block tags.  This is so that
constructs such as 

```
\begin{theorem}
There are infinitely many primes.

```

```
\begin{equation}
a^2 + b^2 = c^2

```

are legal as they stand. Note that they are translated
to 

```
| theorem
There are infinitely many primes.

```

and 

```
|| equation
a^2 + b^2 = c^2

```

so that the closing tag is irrelevant.  This change
makes it easier to construct complex expressions, e.g.,


```
\begin{theorem}
Pythagoras said that 

  \begin{equation}
  a^2 + b^2 = c^2
  
  where $a$ and $b$ are the legs of
  right triangle and $c$ is the hypotenuse.
```

This text is transformed to

```
| theorem
Pythagoras said that 

  | equation
  a^2 + b^2 = c^2
  
  where $a$ and $b$ are the legs of
  right triangle and $c$ is the hypotenuse.
```

This last text is also valid microLaTeX text.

**Note.**  In the example below, the trailing $$
is needed:

```
| theorem
Pythagoras said that 

  $$
  a^2 + b^2 = c^2
  $$
  
  where $a$ and $b$ are the legs of
  right triangle and $c$ is the hypotenuse.
```

This restriction should be eliminated (6/21/2022)

