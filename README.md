[![CircleCI](https://circleci.com/gh/MasWag/visualize-monaa-dot.svg?style=svg)](https://circleci.com/gh/MasWag/visualize-monaa-dot)

visualize-monaa-dot
===================

Translate [MONAA](https://github.com/MasWag/monaa)'s dot language to the dot languages recognizable by GraphViz.

Requirement
===========

- stack

Example
=======

```bash
stack run < ./example/small.dot | dot -T png -o ./small.png
```
