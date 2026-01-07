# Quantum haskell

Realization of the Grover's algorithm in Haskell on 4 qubits, with no external dependencies

## What is it

There is a lot of questions which are easy to check but hard to answer. They are called NP-hard.

If we can't figure out any structure inside the search space, then we are left with just going over through it.

But it is slow. Search space may be vast. SHA256, for example, allows 2^256 different values - that is more than the amount of atoms in the observable universe.

You simply can't try them all.

But quantum computers work differently - they allow you not to search all the space, but get to the answer in just √N steps - much faster.

Sure, you can't simulate a quantum computer with the same performance, but however inefficient it is still possible - that's what I do in this program.

## Resources

- <https://en.wikipedia.org/wiki/Grover%27s_algorithm>
- <https://quantum.country/search>
- <https://www.youtube.com/watch?v=RQWpF2Gb-gU&t=1524s&pp=ygUWMyBibHVlIDEgYnJvd24gcXVhbnR1bQ%3D%3D>
