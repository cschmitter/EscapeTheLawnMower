# EscapeTheLawnMower

A basic platformer written in Haskell with gloss

### Clone project with git
*Note that currently the master branch is feature lacking, while the develop branch is up to date*
```console
$ git clone https://github.com/cschmitter/EscapeTheLawnMower.git
$ cd EscapeTheLawnMower
$ git checkout develop
```

### Prerequisites
You will need stack, a haskell build tool. You will also need libGL.so and libGLU.so
  
On Debian and Ubuntu these are provided by the following packages:
 - haskell-stack
 - libgl1-mesa-dev
 - libglu1-mesa-dev
  
You may also need to install haskell-platform, which provides a precompiled version of the GLUT bindings.

If you are on macOS you should only need to install stack.

[If you are on Windows](https://itsfoss.com/linux-better-than-windows/).

### To build project

```console
$ stack build
$ stack exec EscapeTheLawnMower
```
