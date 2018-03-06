# ALaDyn

[![Build Status Master](https://travis-ci.org/ALaDyn/ALaDyn.png?branch=master)](https://travis-ci.org/ALaDyn/ALaDyn "master")
[![Build status](https://ci.appveyor.com/api/projects/status/evol3yvpqqfyxi7p?svg=true)](https://ci.appveyor.com/project/cenit/aladyn-kul79)

![ALaDyn Logo](https://raw.githubusercontent.com/ALaDyn/ALaDyn/master/logo.png)

`ALaDyn` (**A**cceleration by **La**ser and **Dyn**amics of charged particles) is a PIC code firstly described in *ALaDyn: A High-Accuracy PIC code for the Maxwell-Vlasov Equations* by C. Benedetti et al., published on IEEE Transactions on Plasma Science, **36** 4, 1790-1798 (2008) and then again in the update *Charge preserving high order PIC schemes* by P. Londrillo et al., published on Nucl. Instrum. Meth. A, **620** 1, 28-35 (2010). PWFA modules have been presented in *Numerical investigation of beam-driven PWFA in quasi-nonlinear regime* by P. Londrillo et al., published on Nucl. Instrum. Meth. A, **740** (2014).

This newer version, in part rewritten from scratch, is released as is, without any warranty, and will be maintained here on GitHub. A new publication is currently underway to describe progresses on this new code. If you use `ALaDyn`, you're kindly required to cite the Zenodo DOI of the latest release: [![DOI](https://zenodo.org/badge/doi/10.5281/zenodo.592388.svg)](http://dx.doi.org/10.5281/zenodo.592388).

[Papers published using ALaDyn](doc/PAPERS.md)
[Code description](doc/DESCRIPTION.md)
[Input guide](doc/NAMELIST_GUIDE.md)

Copyright on the code is by the ALaDyn Collaboration.

# How to build

`ALaDyn` is built using CMake.

We support building ALaDyn on almost all sane systems, a guide will be published soon, in the meantime the appveyor and travis recipes should be clear enough on how to build the code on Windows (cygwin or WSL, only gcc toolchain), Mac (only gcc toolchain) and Linux (only gcc toolchain).

# Support channels

We have a Telegram channel to promote latest news: [join here](https://t.me/ALaDyn_Collaboration)
We also have a group on Telegram ([join here](https://t.me/ALaDyn_Chat)) (for quick questions and unofficial discussions), an [official slack channel](http://aladyn.slack.com) (for more technical discussions) and another slack channel for the [italian plasma community](http://plasmaitaly.slack.com).

# Releases

latest release:  [![DOI](https://zenodo.org/badge/doi/10.5281/zenodo.592388.svg)](http://dx.doi.org/10.5281/zenodo.592388)

OLD releases:

v1.0.0 (v2017.1): [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1065413.svg)](https://doi.org/10.5281/zenodo.1065413)   
v1.0.0-beta: [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.49553.svg)](https://doi.org/10.5281/zenodo.49553)   
v1.0.0-alpha2: [![DOI](https://zenodo.org/badge/doi/10.5281/zenodo.48933.svg)](http://dx.doi.org/10.5281/zenodo.48933)   
v1.0.0-alpha: [![DOI](https://zenodo.org/badge/doi/10.5281/zenodo.47467.svg)](http://dx.doi.org/10.5281/zenodo.47467)
